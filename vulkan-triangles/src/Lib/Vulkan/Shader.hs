{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.Shader
  ( createVkShaderStageCI
  , createVulkanShaderModule
  ) where

import           Graphics.Vulkan
import           Graphics.Vulkan.Marshal.Create

import           Lib.Program



createVkShaderStageCI :: VkDevice
                      -> (CSize, Ptr Word32)
                      -> VkShaderStageFlagBits
                      -> Program r VkPipelineShaderStageCreateInfo
createVkShaderStageCI dev shaderCode stageBit = do
    shaderModule <- createVulkanShaderModule dev shaderCode
    return $ createVk @VkPipelineShaderStageCreateInfo
          $  set @"sType"  VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
          &* set @"pNext"  VK_NULL
          &* set @"stage"  stageBit
          &* set @"module" shaderModule
          &* setStrRef @"pName" "main"


createVulkanShaderModule :: VkDevice
                         -> (CSize, Ptr Word32)
                         -> Program r VkShaderModule
createVulkanShaderModule dev (codeSize, codePtr) =
    allocResource
      (\sm -> liftIO $ vkDestroyShaderModule dev sm VK_NULL) $
      withVkPtr smCreateInfo $ \smciPtr -> allocaPeek $
        runVk . vkCreateShaderModule dev smciPtr VK_NULL
  where
    smCreateInfo = createVk @VkShaderModuleCreateInfo
      $  set @"sType"    VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
      &* set @"pNext"    VK_NULL
      &* set @"codeSize" codeSize
      &* set @"pCode"    codePtr
      &* set @"flags"    0
