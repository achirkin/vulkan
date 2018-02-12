{- |

In this example, I follow vulkan-tutorial.com > Graphics pipeline basics

-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           Control.Exception
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           Graphics.Vulkan

import           Lib.GLFW
import           Lib.Utils
import           Lib.Utils.TH
import           Lib.Vulkan
import           Lib.Vulkan.Presentation


main :: IO ()
main = withGLFWWindow 800 600 "05-GraphicsPipeline-Window" $ \window ->
       withGLFWVulkanInstance "05-GraphicsPipeline" $ \vulkanInstance ->
       withSurface vulkanInstance window $ \vulkanSurface -> do
        (Just scsd, pdev)
          <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
        withGraphicsDevice pdev vulkanSurface $ \dev queues ->
          withSwapChain dev scsd queues vulkanSurface $ \swInfo ->
          withImageViews dev swInfo $ \imgViews ->
          withVkShaderStageCI dev
              $(compileGLSL "shaders/triangle.vert")
              VK_SHADER_STAGE_VERTEX_BIT
              $ \shaderVert ->
          withVkShaderStageCI dev
              $(compileGLSL "shaders/triangle.frag")
              VK_SHADER_STAGE_FRAGMENT_BIT
              $ \shaderFrag -> do
            putStrLn $ "Selected physical device: " ++ show pdev
            putStrLn $ "Createad surface: " ++ show vulkanSurface
            putStrLn $ "Createad device: " ++ show dev
            putStrLn $ "Createad queues: " ++ show queues
            putStrLn $ "Createad swapchain: " ++ show swInfo
            putStrLn $ "Createad image views: " ++ show imgViews
            putStrLn $ "Createad vertex shader module: " ++ show shaderVert
            putStrLn $ "Createad fragment shader module: " ++ show shaderFrag
            glfwMainLoop window (return ())




withVkShaderStageCI :: VkDevice
                   -> (Word64, Ptr Word32)
                   -> VkShaderStageFlagBits
                   -> (VkPipelineShaderStageCreateInfo -> IO a)
                   -> IO a
withVkShaderStageCI dev (codeSize, codePtr) stageBit action =
  withCString "main" $ \entryNamePtr -> do

    smCreateInfo <- newVkData @VkShaderModuleCreateInfo $ \smciPtr -> do

      writeField @"sType"    smciPtr VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
      writeField @"pNext"    smciPtr VK_NULL_HANDLE
      writeField @"codeSize" smciPtr codeSize
      writeField @"pCode"    smciPtr codePtr
      writeField @"flags"    smciPtr 0

    shaderModule <- alloca $ \smPtr -> do
      throwingVK "vkCreateShaderModule failed!"
        $ vkCreateShaderModule dev (unsafePtr smCreateInfo) VK_NULL_HANDLE smPtr
      peek smPtr

    ssci <- newVkData $ \psscPtr -> do
      clearStorable psscPtr
      writeField @"sType"  psscPtr
        VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
      writeField @"stage"  psscPtr stageBit
      writeField @"module" psscPtr shaderModule
      writeField @"pName"  psscPtr entryNamePtr

    finally (action ssci) $ do
      touchVkData ssci
      vkDestroyShaderModule dev shaderModule VK_NULL_HANDLE
      touchVkData smCreateInfo
