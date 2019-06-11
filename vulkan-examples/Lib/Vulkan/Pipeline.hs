{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.Pipeline
  ( withVkShaderStageCI
  , withGraphicsPipeline
  , withRenderPass
  ) where

import           Control.Exception
import           Data.Bits
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create

import           Lib.Utils
import           Lib.Vulkan.Presentation



withVkShaderStageCI :: VkDevice
                    -> (CSize, Ptr Word32)
                    -> VkShaderStageFlagBits
                    -> (VkPipelineShaderStageCreateInfo -> IO a)
                    -> IO a
withVkShaderStageCI dev shaderCode stageBit action = do
    shaderModule <- createVulkanShaderModule dev shaderCode
    let pssCreateInfo = createVk @VkPipelineShaderStageCreateInfo
          $  set @"sType"  VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
          &* set @"pNext"  VK_NULL
          &* set @"stage"  stageBit
          &* set @"module" shaderModule
          &* setStrRef @"pName" "main"
    finally (action pssCreateInfo) $ do
      destroyVulkanShaderModule dev shaderModule
      touchVkData pssCreateInfo


createVulkanShaderModule :: VkDevice -> (CSize, Ptr Word32) -> IO VkShaderModule
createVulkanShaderModule dev (codeSize, codePtr) =
    withPtr smCreateInfo $ \smciPtr -> alloca $ \smPtr -> do
      throwingVK "vkCreateShaderModule failed!"
        $ vkCreateShaderModule dev smciPtr VK_NULL smPtr
      peek smPtr
  where
    smCreateInfo = createVk @VkShaderModuleCreateInfo
      $  set @"sType"    VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
      &* set @"pNext"    VK_NULL
      &* set @"codeSize" codeSize
      &* set @"pCode"    codePtr
      &* set @"flags"    VK_ZERO_FLAGS

destroyVulkanShaderModule :: VkDevice -> VkShaderModule -> IO ()
destroyVulkanShaderModule dev = flip (vkDestroyShaderModule dev) VK_NULL



withGraphicsPipeline :: VkDevice
                     -> SwapChainImgInfo
                     -> [VkPipelineShaderStageCreateInfo]
                     -> VkRenderPass
                     -> (VkPipeline -> IO ())
                     -> IO ()
withGraphicsPipeline
    dev SwapChainImgInfo{..} shaderDescs renderPass action =
  let -- vertex input
      vertexInputInfo = createVk @VkPipelineVertexInputStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"vertexBindingDescriptionCount" 0
        &* set @"pVertexBindingDescriptions" VK_NULL
        &* set @"vertexAttributeDescriptionCount" 0
        &* set @"pVertexAttributeDescriptions" VK_NULL

      -- input assembly
      inputAssembly = createVk @VkPipelineInputAssemblyStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        &* set @"primitiveRestartEnable" VK_FALSE

      -- viewports and scissors
      viewPort = createVk @VkViewport
        $  set @"x" 0
        &* set @"y" 0
        &* set @"width" (fromIntegral $ getField @"width" swExtent)
        &* set @"height" (fromIntegral $ getField @"height" swExtent)
        &* set @"minDepth" 0
        &* set @"maxDepth" 1

      scissor = createVk @VkRect2D
        $  set   @"extent" swExtent
        &* setVk @"offset" ( set @"x" 0 &* set @"y" 0 )

      viewPortState = createVk @VkPipelineViewportStateCreateInfo
        $ set @"sType"
          VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"viewportCount" 1
        &* setVkRef @"pViewports" viewPort
        &* set @"scissorCount" 1
        &* setVkRef @"pScissors" scissor

      -- rasterizer
      rasterizer = createVk @VkPipelineRasterizationStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"depthClampEnable" VK_FALSE
        &* set @"rasterizerDiscardEnable" VK_FALSE
        &* set @"polygonMode" VK_POLYGON_MODE_FILL
        &* set @"cullMode" VK_CULL_MODE_BACK_BIT
        &* set @"frontFace" VK_FRONT_FACE_CLOCKWISE
        &* set @"depthBiasEnable" VK_FALSE
        &* set @"depthBiasConstantFactor" 0
        &* set @"depthBiasClamp" 0
        &* set @"depthBiasSlopeFactor" 0
        &* set @"lineWidth" 1.0

      -- multisampling
      multisampling = createVk @VkPipelineMultisampleStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"sampleShadingEnable" VK_FALSE
        &* set @"rasterizationSamples" VK_SAMPLE_COUNT_1_BIT
        &* set @"minSampleShading" 1.0 -- Optional
        &* set @"pSampleMask" VK_NULL -- Optional
        &* set @"alphaToCoverageEnable" VK_FALSE -- Optional
        &* set @"alphaToOneEnable" VK_FALSE -- Optional

      -- Depth and stencil testing
      -- we will pass null pointer in a corresponding place

      -- color blending
      colorBlendAttachment = createVk @VkPipelineColorBlendAttachmentState
        $  set @"colorWriteMask"
            (   VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
            .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT )
        &* set @"blendEnable" VK_FALSE
        &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE -- Optional
        &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO -- Optional
        &* set @"colorBlendOp" VK_BLEND_OP_ADD -- Optional
        &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE -- Optional
        &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO -- Optional
        &* set @"alphaBlendOp" VK_BLEND_OP_ADD -- Optional

      colorBlending = createVk @VkPipelineColorBlendStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"logicOpEnable" VK_FALSE
        &* set @"logicOp" VK_LOGIC_OP_COPY -- Optional
        &* set @"attachmentCount" 1
        &* setVkRef @"pAttachments" colorBlendAttachment
        &* setAt @"blendConstants" @0 0.0 -- Optional
        &* setAt @"blendConstants" @1 0.0 -- Optional
        &* setAt @"blendConstants" @2 0.0 -- Optional
        &* setAt @"blendConstants" @3 0.0 -- Optional


    -- finally, create pipeline!
  in withPipelineLayout dev $ \pipelineLayout ->
      withArrayLen shaderDescs $ \stageCount stagesPtr ->
        let gpCreateInfo = createVk @VkGraphicsPipelineCreateInfo
              $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
              &* set @"pNext" VK_NULL
              &* set @"flags" VK_ZERO_FLAGS
              &* set @"stageCount" (fromIntegral stageCount)
              &* set @"pStages" stagesPtr
              &* setVkRef @"pVertexInputState" vertexInputInfo
              &* setVkRef @"pInputAssemblyState" inputAssembly
              &* set @"pTessellationState" VK_NULL
              &* setVkRef @"pViewportState" viewPortState
              &* setVkRef @"pRasterizationState" rasterizer
              &* setVkRef @"pMultisampleState" multisampling
              &* set @"pDepthStencilState" VK_NULL
              &* setVkRef @"pColorBlendState" colorBlending
              &* set @"pDynamicState" VK_NULL
              &* set @"layout" pipelineLayout
              &* set @"renderPass" renderPass
              &* set @"subpass" 0
              &* set @"basePipelineHandle" VK_NULL_HANDLE
              &* set @"basePipelineIndex" (-1)

        in do
          createGPfun <- vkGetDeviceProc @VkCreateGraphicsPipelines dev
          graphicsPipeline <- withPtr gpCreateInfo
                $ \gpciPtr -> alloca $ \gpPtr -> do
            throwingVK "vkCreateGraphicsPipelines failed!"
              $ createGPfun dev VK_NULL 1 gpciPtr VK_NULL gpPtr
            peek gpPtr


          -- again, run an action and touch all allocated objects to make sure
          -- they are alive at the moment of vulkan object destruction.
          finally (action graphicsPipeline) $
            vkDestroyPipeline dev graphicsPipeline VK_NULL


withPipelineLayout :: VkDevice -> (VkPipelineLayout -> IO a) -> IO a
withPipelineLayout dev action = do
  pipelineLayout <- withPtr plCreateInfo $ \plciPtr -> alloca $ \plPtr -> do
    throwingVK "vkCreatePipelineLayout failed!"
      $ vkCreatePipelineLayout dev plciPtr VK_NULL plPtr
    peek plPtr
  finally (action pipelineLayout) $
    vkDestroyPipelineLayout dev pipelineLayout VK_NULL
  where
    plCreateInfo = createVk @VkPipelineLayoutCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"setLayoutCount"         0       -- Optional
      &* set @"pSetLayouts"            VK_NULL -- Optional
      &* set @"pushConstantRangeCount" 0       -- Optional
      &* set @"pPushConstantRanges"    VK_NULL -- Optional


withRenderPass :: VkDevice -> SwapChainImgInfo
               -> (VkRenderPass -> IO a) -> IO a
withRenderPass dev SwapChainImgInfo{..} action =
  let -- attachment description
      colorAttachment = createVk @VkAttachmentDescription
        $  set @"flags" VK_ZERO_FLAGS
        &* set @"format" swImgFormat
        &* set @"samples" VK_SAMPLE_COUNT_1_BIT
        &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
        &* set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
        &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
        &* set @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

      -- subpasses and attachment references
      colorAttachmentRef = createVk @VkAttachmentReference
        $  set @"attachment" 0
        &* set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

      subpass = createVk @VkSubpassDescription
        $  set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
        &* set @"colorAttachmentCount" 1
        &* setVkRef @"pColorAttachments" colorAttachmentRef
        &* set @"pPreserveAttachments" VK_NULL
        &* set @"pInputAttachments" VK_NULL

      -- subpass dependencies
      dependency = createVk @VkSubpassDependency
        $  set @"srcSubpass" VK_SUBPASS_EXTERNAL
        &* set @"dstSubpass" 0
        &* set @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* set @"srcAccessMask" VK_ZERO_FLAGS
        &* set @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* set @"dstAccessMask"
            (   VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
            .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT )

      -- render pass
      rpCreateInfo = createVk @VkRenderPassCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"attachmentCount" 1
        &* setVkRef @"pAttachments" colorAttachment
        &* set @"subpassCount" 1
        &* setVkRef @"pSubpasses" subpass
        &* set @"dependencyCount" 1
        &* setVkRef @"pDependencies" dependency

  in do
    renderPass <- withPtr rpCreateInfo $ \rpciPtr -> alloca $ \rpPtr -> do
      throwingVK "vkCreatePipelineLayout failed!"
        $ vkCreateRenderPass dev rpciPtr VK_NULL rpPtr
      peek rpPtr
    finally (action renderPass) $
      vkDestroyRenderPass dev renderPass VK_NULL
