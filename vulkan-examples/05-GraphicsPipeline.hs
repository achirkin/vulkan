{- |

In this example, I follow vulkan-tutorial.com > Graphics pipeline basics

-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           Control.Exception
import           Data.Bits
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0

import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Lib.GLFW
import           Lib.Utils
import           Lib.Utils.TH
import           Lib.Vulkan
import           Lib.Vulkan.Presentation

windowWidth, windowHeight :: Num a => a
windowWidth = 800
windowHeight = 600

main :: IO ()
main = withGLFWWindow windowWidth windowHeight "05-GraphicsPipeline-Window"
          $ \window ->
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
              $ \shaderFrag ->
          withGraphicsPipeline dev swInfo [shaderVert, shaderFrag]
              $ \graphicsPipeline -> do
            putStrLn $ "Selected physical device: " ++ show pdev
            putStrLn $ "Createad surface: " ++ show vulkanSurface
            putStrLn $ "Createad device: " ++ show dev
            putStrLn $ "Createad queues: " ++ show queues
            putStrLn $ "Createad swapchain: " ++ show swInfo
            putStrLn $ "Createad image views: " ++ show imgViews
            putStrLn $ "Createad vertex shader module: " ++ show shaderVert
            putStrLn $ "Createad fragment shader module: " ++ show shaderFrag
            putStrLn $ "Createad pipeline: " ++ show graphicsPipeline
            glfwMainLoop window (return ())




withVkShaderStageCI :: VkDevice
                   -> (CSize, Ptr Word32)
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


withGraphicsPipeline :: VkDevice
                     -> SwapChainImgInfo
                     -> [VkPipelineShaderStageCreateInfo]
                     -> (VkPipeline -> IO ())
                     -> IO ()
withGraphicsPipeline dev scii@SwapChainImgInfo{..} shaderDescs action = do

  -- vertex input
  vertexInputInfo <- newVkData @VkPipelineVertexInputStateCreateInfo
                               $ \viPtr -> do
    writeField @"sType" viPtr
      VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
    writeField @"pNext" viPtr VK_NULL_HANDLE
    writeField @"flags" viPtr 0
    writeField @"vertexBindingDescriptionCount" viPtr 0
    writeField @"pVertexBindingDescriptions"    viPtr VK_NULL_HANDLE
    writeField @"vertexBindingDescriptionCount" viPtr 0
    writeField @"pVertexAttributeDescriptions"  viPtr VK_NULL_HANDLE


  -- input assembly
  inputAssembly <- newVkData @VkPipelineInputAssemblyStateCreateInfo
                               $ \iaPtr -> do
    writeField @"sType" iaPtr
      VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
    writeField @"pNext" iaPtr VK_NULL_HANDLE
    writeField @"flags" iaPtr 0
    writeField @"topology" iaPtr VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
    writeField @"primitiveRestartEnable" iaPtr VK_FALSE


  -- viewports and scissors
  viewPort <- newVkData @VkViewport $ \vpPtr -> do
    writeField @"x"        vpPtr 0
    writeField @"y"        vpPtr 0
    writeField @"width"    vpPtr (fromIntegral $ getField @"width" swExtent)
    writeField @"height"   vpPtr (fromIntegral $ getField @"height" swExtent)
    writeField @"minDepth" vpPtr 0
    writeField @"maxDepth" vpPtr 1

  scissor <- newVkData @VkRect2D $ \scPtr -> do
    clearStorable scPtr
    writeField @"extent" scPtr swExtent

  viewPortState <- newVkData @VkPipelineViewportStateCreateInfo
                             $ \vpsPtr -> do
    writeField @"sType" vpsPtr
      VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
    writeField @"pNext" vpsPtr VK_NULL_HANDLE
    writeField @"flags" vpsPtr 0
    writeField @"viewportCount" vpsPtr 1
    writeField @"pViewports"    vpsPtr (unsafePtr viewPort)
    writeField @"scissorCount"  vpsPtr 1
    writeField @"pScissors"     vpsPtr (unsafePtr scissor)


  -- rasterizer
  rasterizer <- newVkData @VkPipelineRasterizationStateCreateInfo
                          $ \rPtr -> do
    writeField @"sType" rPtr
      VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
    writeField @"pNext" rPtr VK_NULL_HANDLE
    writeField @"flags" rPtr 0
    writeField @"depthClampEnable"
      rPtr VK_FALSE
    writeField @"rasterizerDiscardEnable"
      rPtr VK_FALSE
    writeField @"polygonMode"
      rPtr VK_POLYGON_MODE_FILL
    writeField @"cullMode"
      rPtr VK_CULL_MODE_BACK_BIT
    writeField @"frontFace"
      rPtr VK_FRONT_FACE_CLOCKWISE
    writeField @"depthBiasEnable"
      rPtr VK_FALSE
    writeField @"depthBiasConstantFactor"
      rPtr 0
    writeField @"depthBiasClamp"
      rPtr 0
    writeField @"depthBiasSlopeFactor"
      rPtr 0
    writeField @"lineWidth"
      rPtr 1.0


  -- multisampling
  multisampling <- newVkData @VkPipelineMultisampleStateCreateInfo
                             $ \msPtr -> do
    writeField @"sType" msPtr
      VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
    writeField @"pNext" msPtr VK_NULL_HANDLE
    writeField @"flags" msPtr 0
    writeField @"sampleShadingEnable"   msPtr VK_FALSE
    writeField @"rasterizationSamples"  msPtr VK_SAMPLE_COUNT_1_BIT
    writeField @"minSampleShading"      msPtr 1.0 -- Optional
    writeField @"pSampleMask"           msPtr VK_NULL_HANDLE -- Optional
    writeField @"alphaToCoverageEnable" msPtr VK_FALSE -- Optional
    writeField @"alphaToOneEnable"      msPtr VK_FALSE -- Optional


  -- Depth and stencil testing
  -- we will pass null pointer in a corresponding place


  -- color blending
  colorBlendAttachment <- newVkData @VkPipelineColorBlendAttachmentState
                                    $ \cbaPtr -> do
    writeField @"colorWriteMask"
      cbaPtr $  VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
            .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT
    writeField @"blendEnable"
      cbaPtr VK_FALSE
    writeField @"srcColorBlendFactor"
      cbaPtr VK_BLEND_FACTOR_ONE -- Optional
    writeField @"dstColorBlendFactor"
      cbaPtr VK_BLEND_FACTOR_ZERO -- Optional
    writeField @"colorBlendOp"
      cbaPtr VK_BLEND_OP_ADD -- Optional
    writeField @"srcAlphaBlendFactor"
      cbaPtr VK_BLEND_FACTOR_ONE -- Optional
    writeField @"dstAlphaBlendFactor"
      cbaPtr VK_BLEND_FACTOR_ZERO -- Optional
    writeField @"alphaBlendOp"
      cbaPtr VK_BLEND_OP_ADD -- Optional

  colorBlending <- newVkData @VkPipelineColorBlendStateCreateInfo
                             $ \cbPtr -> do
    writeField @"sType" cbPtr
      VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
    writeField @"pNext" cbPtr VK_NULL_HANDLE
    writeField @"flags" cbPtr 0
    writeField @"logicOpEnable"
      cbPtr VK_FALSE
    writeField @"logicOp"
      cbPtr VK_LOGIC_OP_COPY -- Optional
    writeField @"attachmentCount"
      cbPtr 1
    writeField @"pAttachments"
      cbPtr (unsafePtr colorBlendAttachment)
    writeFieldArray @"blendConstants" @0 cbPtr 0.0 -- Optional
    writeFieldArray @"blendConstants" @1 cbPtr 0.0 -- Optional
    writeFieldArray @"blendConstants" @2 cbPtr 0.0 -- Optional
    writeFieldArray @"blendConstants" @3 cbPtr 0.0 -- Optional


  -- finally, create pipeline!
  withPipelineLayout dev $ \pipelineLayout ->
    withRenderPass dev scii $ \renderPass ->
    withArrayLen shaderDescs $ \stageCount stagesPtr -> do

      pipelineInfo <- newVkData @VkGraphicsPipelineCreateInfo
                                $ \piPtr -> do
        writeField @"sType"
          piPtr VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
        writeField @"pNext"
          piPtr VK_NULL_HANDLE
        writeField @"flags"
          piPtr 0
        writeField @"stageCount"
          piPtr (fromIntegral stageCount)
        writeField @"pStages"
          piPtr stagesPtr
        writeField @"pVertexInputState"
          piPtr (unsafePtr vertexInputInfo)
        writeField @"pInputAssemblyState"
          piPtr (unsafePtr inputAssembly)
        writeField @"pTessellationState"
          piPtr VK_NULL_HANDLE
        writeField @"pViewportState"
          piPtr (unsafePtr viewPortState)
        writeField @"pRasterizationState"
          piPtr (unsafePtr rasterizer)
        writeField @"pMultisampleState"
          piPtr (unsafePtr multisampling)
        writeField @"pDepthStencilState"
          piPtr VK_NULL_HANDLE
        writeField @"pColorBlendState"
          piPtr (unsafePtr colorBlending)
        writeField @"pDynamicState"
          piPtr VK_NULL_HANDLE
        writeField @"layout"
          piPtr pipelineLayout
        writeField @"renderPass"
          piPtr renderPass
        writeField @"subpass"
          piPtr 0
        writeField @"basePipelineHandle"
          piPtr VK_NULL_HANDLE
        writeField @"basePipelineIndex"
          piPtr (-1)

      graphicsPipeline <- alloca $ \gpPtr -> do
        throwingVK "vkCreateGraphicsPipelines failed!"
          $ vkCreateGraphicsPipelines
              dev VK_NULL_HANDLE 1 (unsafePtr pipelineInfo) VK_NULL_HANDLE gpPtr
        peek gpPtr


      -- again, run an action and touch all allocated objects to make sure
      -- they are alive at the moment of vulkan object destruction.
      finally (action graphicsPipeline) $ do
        vkDestroyPipeline dev graphicsPipeline VK_NULL_HANDLE
        touchVkData pipelineInfo
        touchVkData vertexInputInfo
        touchVkData inputAssembly
        touchVkData viewPort
        touchVkData scissor
        touchVkData viewPortState
        touchVkData rasterizer
        touchVkData multisampling
        touchVkData colorBlending
        touchVkData colorBlendAttachment


withPipelineLayout :: VkDevice -> (VkPipelineLayout -> IO a) -> IO a
withPipelineLayout dev action = do

  -- pipeline layout
  pipelineLayoutInfo <- newVkData @VkPipelineLayoutCreateInfo
                                  $ \pliPtr -> do
    writeField @"sType" pliPtr
      VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
    writeField @"pNext" pliPtr VK_NULL_HANDLE
    writeField @"flags" pliPtr 0
    writeField @"setLayoutCount"         pliPtr 0 -- Optional
    writeField @"pSetLayouts"            pliPtr VK_NULL_HANDLE -- Optional
    writeField @"pushConstantRangeCount" pliPtr 0 -- Optional
    writeField @"pPushConstantRanges"    pliPtr VK_NULL_HANDLE -- Optional

  pipelineLayout <- alloca $ \plPtr -> do
    throwingVK "vkCreatePipelineLayout failed!"
      $ vkCreatePipelineLayout dev (unsafePtr pipelineLayoutInfo)
                               VK_NULL_HANDLE plPtr
    peek plPtr

  -- again, run an action and touch all allocated objects to make sure they are
  -- alive at the moment of vulkan object destruction.
  finally (action pipelineLayout) $ do
    vkDestroyPipelineLayout dev pipelineLayout VK_NULL_HANDLE
    touchVkData pipelineLayoutInfo


withRenderPass :: VkDevice -> SwapChainImgInfo
               -> (VkRenderPass -> IO a) -> IO a
withRenderPass dev SwapChainImgInfo{..} action = do

  -- attachment description
  colorAttachment <- newVkData @VkAttachmentDescription
                               $ \caPtr -> do
    writeField @"flags"   caPtr 0
    writeField @"format"  caPtr swImgFormat
    writeField @"samples" caPtr VK_SAMPLE_COUNT_1_BIT
    writeField @"loadOp"  caPtr VK_ATTACHMENT_LOAD_OP_CLEAR
    writeField @"storeOp" caPtr VK_ATTACHMENT_STORE_OP_STORE
    writeField @"stencilLoadOp"  caPtr VK_ATTACHMENT_LOAD_OP_DONT_CARE
    writeField @"stencilStoreOp" caPtr VK_ATTACHMENT_STORE_OP_DONT_CARE
    writeField @"initialLayout"  caPtr VK_IMAGE_LAYOUT_UNDEFINED
    writeField @"finalLayout"    caPtr VK_IMAGE_LAYOUT_PRESENT_SRC_KHR


  -- subpasses and attachment references
  colorAttachmentRef <- newVkData @VkAttachmentReference
                                  $ \carPtr -> do
    writeField @"attachment" carPtr 0
    writeField @"layout"     carPtr VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

  subpass <- newVkData @VkSubpassDescription
                           $ \sdPtr -> do
    clearStorable sdPtr
    writeField @"pipelineBindPoint" sdPtr VK_PIPELINE_BIND_POINT_GRAPHICS
    writeField @"colorAttachmentCount" sdPtr 1
    writeField @"pColorAttachments" sdPtr (unsafePtr colorAttachmentRef)


  -- render pass
  renderPassInfo <- newVkData @VkRenderPassCreateInfo
                              $ \rpiPtr -> do
    clearStorable rpiPtr
    writeField @"sType" rpiPtr VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
    writeField @"attachmentCount" rpiPtr 1
    writeField @"pAttachments" rpiPtr (unsafePtr colorAttachment)
    writeField @"subpassCount" rpiPtr 1
    writeField @"pSubpasses" rpiPtr (unsafePtr subpass)

  renderPass <- alloca $ \rpPtr -> do
    throwingVK "vkCreatePipelineLayout failed!"
      $ vkCreateRenderPass dev (unsafePtr renderPassInfo)
                               VK_NULL_HANDLE rpPtr
    peek rpPtr


  finally (action renderPass) $ do
    vkDestroyRenderPass dev renderPass VK_NULL_HANDLE
    touchVkData renderPassInfo
    touchVkData subpass
    touchVkData colorAttachment
    touchVkData colorAttachmentRef
