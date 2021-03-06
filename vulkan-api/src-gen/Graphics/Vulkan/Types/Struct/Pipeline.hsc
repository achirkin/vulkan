#include "vulkan/vulkan.h"

{-# LANGUAGE CPP             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Pipeline
       (VkGraphicsPipelineCreateInfo, VkPipelineCacheCreateInfo,
        VkPipelineColorBlendAdvancedStateCreateInfoEXT,
        VkPipelineColorBlendAttachmentState,
        VkPipelineColorBlendStateCreateInfo,
        VkPipelineCompilerControlCreateInfoAMD,
        VkPipelineCoverageModulationStateCreateInfoNV,
        VkPipelineCoverageReductionStateCreateInfoNV,
        VkPipelineCoverageToColorStateCreateInfoNV,
        VkPipelineCreationFeedbackCreateInfoEXT,
        VkPipelineCreationFeedbackEXT,
        VkPipelineDepthStencilStateCreateInfo,
        VkPipelineDiscardRectangleStateCreateInfoEXT,
        VkPipelineDynamicStateCreateInfo, VkPipelineExecutableInfoKHR,
        VkPipelineExecutableInternalRepresentationKHR,
        VkPipelineExecutablePropertiesKHR,
        VkPipelineExecutableStatisticKHR,
        VkPipelineExecutableStatisticValueKHR, VkPipelineInfoKHR,
        VkPipelineInputAssemblyStateCreateInfo, VkPipelineLayoutCreateInfo,
        VkPipelineMultisampleStateCreateInfo,
        VkPipelineRasterizationConservativeStateCreateInfoEXT,
        VkPipelineRasterizationDepthClipStateCreateInfoEXT,
        VkPipelineRasterizationLineStateCreateInfoEXT,
        VkPipelineRasterizationStateCreateInfo,
        VkPipelineRasterizationStateRasterizationOrderAMD,
        VkPipelineRasterizationStateStreamCreateInfoEXT,
        VkPipelineRepresentativeFragmentTestStateCreateInfoNV,
        VkPipelineSampleLocationsStateCreateInfoEXT,
        VkPipelineShaderStageCreateInfo,
        VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT,
        VkPipelineTessellationDomainOriginStateCreateInfo,
        VkPipelineTessellationDomainOriginStateCreateInfoKHR,
        VkPipelineTessellationStateCreateInfo,
        VkPipelineVertexInputDivisorStateCreateInfoEXT,
        VkPipelineVertexInputStateCreateInfo,
        VkPipelineViewportCoarseSampleOrderStateCreateInfoNV,
        VkPipelineViewportExclusiveScissorStateCreateInfoNV,
        VkPipelineViewportShadingRateImageStateCreateInfoNV,
        VkPipelineViewportStateCreateInfo,
        VkPipelineViewportSwizzleStateCreateInfoNV,
        VkPipelineViewportWScalingStateCreateInfoNV)
       where
import Graphics.Vulkan.Constants                                   (VK_MAX_DESCRIPTION_SIZE)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes                             (VkBool32,
                                                                    VkSampleMask)
import Graphics.Vulkan.Types.Bitmasks                              (VkPipelineColorBlendStateCreateFlags,
                                                                    VkPipelineCoverageModulationStateCreateFlagsNV,
                                                                    VkPipelineCoverageReductionStateCreateFlagsNV,
                                                                    VkPipelineCoverageToColorStateCreateFlagsNV,
                                                                    VkPipelineDepthStencilStateCreateFlags,
                                                                    VkPipelineDiscardRectangleStateCreateFlagsEXT,
                                                                    VkPipelineDynamicStateCreateFlags,
                                                                    VkPipelineInputAssemblyStateCreateFlags,
                                                                    VkPipelineLayoutCreateFlags,
                                                                    VkPipelineMultisampleStateCreateFlags,
                                                                    VkPipelineRasterizationConservativeStateCreateFlagsEXT,
                                                                    VkPipelineRasterizationDepthClipStateCreateFlagsEXT,
                                                                    VkPipelineRasterizationStateCreateFlags,
                                                                    VkPipelineRasterizationStateStreamCreateFlagsEXT,
                                                                    VkPipelineTessellationStateCreateFlags,
                                                                    VkPipelineVertexInputStateCreateFlags,
                                                                    VkPipelineViewportStateCreateFlags,
                                                                    VkPipelineViewportSwizzleStateCreateFlagsNV)
import Graphics.Vulkan.Types.Enum.Blend                            (VkBlendFactor,
                                                                    VkBlendOp,
                                                                    VkBlendOverlapEXT)
import Graphics.Vulkan.Types.Enum.CoarseSampleOrderTypeNV          (VkCoarseSampleOrderTypeNV)
import Graphics.Vulkan.Types.Enum.Color                            (VkColorComponentFlags)
import Graphics.Vulkan.Types.Enum.CompareOp                        (VkCompareOp)
import Graphics.Vulkan.Types.Enum.ConservativeRasterizationModeEXT (VkConservativeRasterizationModeEXT)
import Graphics.Vulkan.Types.Enum.Coverage                         (VkCoverageModulationModeNV,
                                                                    VkCoverageReductionModeNV)
import Graphics.Vulkan.Types.Enum.CullModeFlags                    (VkCullModeFlags)
import Graphics.Vulkan.Types.Enum.DiscardRectangleModeEXT          (VkDiscardRectangleModeEXT)
import Graphics.Vulkan.Types.Enum.DynamicState                     (VkDynamicState)
import Graphics.Vulkan.Types.Enum.FrontFace                        (VkFrontFace)
import Graphics.Vulkan.Types.Enum.LineRasterizationModeEXT         (VkLineRasterizationModeEXT)
import Graphics.Vulkan.Types.Enum.LogicOp                          (VkLogicOp)
import Graphics.Vulkan.Types.Enum.Pipeline                         (VkPipelineCacheCreateFlags,
                                                                    VkPipelineCompilerControlFlagsAMD,
                                                                    VkPipelineCreateFlags,
                                                                    VkPipelineCreationFeedbackFlagsEXT,
                                                                    VkPipelineExecutableStatisticFormatKHR,
                                                                    VkPipelineShaderStageCreateFlags)
import Graphics.Vulkan.Types.Enum.PolygonMode                      (VkPolygonMode)
import Graphics.Vulkan.Types.Enum.PrimitiveTopology                (VkPrimitiveTopology)
import Graphics.Vulkan.Types.Enum.RasterizationOrderAMD            (VkRasterizationOrderAMD)
import Graphics.Vulkan.Types.Enum.SampleCountFlags                 (VkSampleCountFlagBits)
import Graphics.Vulkan.Types.Enum.Shader                           (VkShaderStageFlagBits,
                                                                    VkShaderStageFlags)
import Graphics.Vulkan.Types.Enum.StructureType                    (VkStructureType)
import Graphics.Vulkan.Types.Enum.TessellationDomainOrigin         (VkTessellationDomainOrigin)
import Graphics.Vulkan.Types.Handles                               (VkDescriptorSetLayout,
                                                                    VkPipeline,
                                                                    VkPipelineLayout,
                                                                    VkRenderPass,
                                                                    VkShaderModule)
import Graphics.Vulkan.Types.Struct.CoarseSample                   (VkCoarseSampleOrderCustomNV)
import Graphics.Vulkan.Types.Struct.ComputePipelineCreateInfo      (VkComputePipelineCreateInfo)
#ifdef VK_ENABLE_BETA_EXTENSIONS
import Graphics.Vulkan.Types.Struct.EnableBetaExtensions           (VkRayTracingPipelineCreateInfoKHR)
import Graphics.Vulkan.Types.Struct.RayTracing                     (VkRayTracingPipelineCreateInfoNV)
#endif
import Graphics.Vulkan.Types.Struct.PushConstantRange              (VkPushConstantRange)
import Graphics.Vulkan.Types.Struct.Rect                           (VkRect2D)
import Graphics.Vulkan.Types.Struct.SampleLocation                 (VkSampleLocationsInfoEXT)
import Graphics.Vulkan.Types.Struct.ShadingRatePaletteNV           (VkShadingRatePaletteNV)
import Graphics.Vulkan.Types.Struct.Specialization                 (VkSpecializationInfo)
import Graphics.Vulkan.Types.Struct.StencilOpState                 (VkStencilOpState)
import Graphics.Vulkan.Types.Struct.VertexInput                    (VkVertexInputAttributeDescription,
                                                                    VkVertexInputBindingDescription,
                                                                    VkVertexInputBindingDivisorDescriptionEXT)
import Graphics.Vulkan.Types.Struct.Viewport                       (VkViewport, VkViewportSwizzleNV,
                                                                    VkViewportWScalingNV)

-- | > typedef struct VkGraphicsPipelineCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineCreateFlags  flags;
--   >     uint32_t               stageCount;
--   >     const VkPipelineShaderStageCreateInfo* pStages;
--   >     const VkPipelineVertexInputStateCreateInfo* pVertexInputState;
--   >     const VkPipelineInputAssemblyStateCreateInfo* pInputAssemblyState;
--   >     const VkPipelineTessellationStateCreateInfo* pTessellationState;
--   >     const VkPipelineViewportStateCreateInfo* pViewportState;
--   >     const VkPipelineRasterizationStateCreateInfo* pRasterizationState;
--   >     const VkPipelineMultisampleStateCreateInfo* pMultisampleState;
--   >     const VkPipelineDepthStencilStateCreateInfo* pDepthStencilState;
--   >     const VkPipelineColorBlendStateCreateInfo* pColorBlendState;
--   >     const VkPipelineDynamicStateCreateInfo* pDynamicState;
--   >     VkPipelineLayout       layout;
--   >     VkRenderPass           renderPass;
--   >     uint32_t               subpass;
--   >     VkPipeline      basePipelineHandle;
--   >     int32_t                basePipelineIndex;
--   > } VkGraphicsPipelineCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkGraphicsPipelineCreateInfo VkGraphicsPipelineCreateInfo registry at www.khronos.org>
type VkGraphicsPipelineCreateInfo =
     VkStruct VkGraphicsPipelineCreateInfo' -- ' closing tick for hsc2hs

data VkGraphicsPipelineCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkGraphicsPipelineCreateInfo where
    type StructRep VkGraphicsPipelineCreateInfo =
         'StructMeta "VkGraphicsPipelineCreateInfo" -- ' closing tick for hsc2hs
           VkGraphicsPipelineCreateInfo
           #{size VkGraphicsPipelineCreateInfo}
           #{alignment VkGraphicsPipelineCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkGraphicsPipelineCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkGraphicsPipelineCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineCreateFlags 'True
                                                             #{offset VkGraphicsPipelineCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stageCount" Word32 'False
                                                    #{offset VkGraphicsPipelineCreateInfo, stageCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pStages" (Ptr VkPipelineShaderStageCreateInfo) 'False
                #{offset VkGraphicsPipelineCreateInfo, pStages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pVertexInputState" -- ' closing tick for hsc2hs
                (Ptr VkPipelineVertexInputStateCreateInfo)
                'True -- ' closing tick for hsc2hs
                #{offset VkGraphicsPipelineCreateInfo, pVertexInputState}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pInputAssemblyState" -- ' closing tick for hsc2hs
                (Ptr VkPipelineInputAssemblyStateCreateInfo)
                'True -- ' closing tick for hsc2hs
                #{offset VkGraphicsPipelineCreateInfo, pInputAssemblyState}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pTessellationState" -- ' closing tick for hsc2hs
                (Ptr VkPipelineTessellationStateCreateInfo)
                'True -- ' closing tick for hsc2hs
                #{offset VkGraphicsPipelineCreateInfo, pTessellationState}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pViewportState" -- ' closing tick for hsc2hs
                (Ptr VkPipelineViewportStateCreateInfo)
                'True -- ' closing tick for hsc2hs
                #{offset VkGraphicsPipelineCreateInfo, pViewportState}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pRasterizationState" -- ' closing tick for hsc2hs
                (Ptr VkPipelineRasterizationStateCreateInfo)
                'False -- ' closing tick for hsc2hs
                #{offset VkGraphicsPipelineCreateInfo, pRasterizationState}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pMultisampleState" -- ' closing tick for hsc2hs
                (Ptr VkPipelineMultisampleStateCreateInfo)
                'True -- ' closing tick for hsc2hs
                #{offset VkGraphicsPipelineCreateInfo, pMultisampleState}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDepthStencilState" -- ' closing tick for hsc2hs
                (Ptr VkPipelineDepthStencilStateCreateInfo)
                'True -- ' closing tick for hsc2hs
                #{offset VkGraphicsPipelineCreateInfo, pDepthStencilState}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pColorBlendState" -- ' closing tick for hsc2hs
                (Ptr VkPipelineColorBlendStateCreateInfo)
                'True -- ' closing tick for hsc2hs
                #{offset VkGraphicsPipelineCreateInfo, pColorBlendState}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDynamicState" (Ptr VkPipelineDynamicStateCreateInfo) -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkGraphicsPipelineCreateInfo, pDynamicState}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layout" VkPipelineLayout 'False
                                                          #{offset VkGraphicsPipelineCreateInfo, layout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "renderPass" VkRenderPass 'False
                                                          #{offset VkGraphicsPipelineCreateInfo, renderPass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subpass" Word32 'False
                                                 #{offset VkGraphicsPipelineCreateInfo, subpass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "basePipelineHandle" VkPipeline 'True
                                                               #{offset VkGraphicsPipelineCreateInfo, basePipelineHandle}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "basePipelineIndex" Int32 'False
                                                          #{offset VkGraphicsPipelineCreateInfo, basePipelineIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineCacheCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineCacheCreateFlags    flags;
--   >     size_t                 initialDataSize;
--   >     const void*            pInitialData;
--   > } VkPipelineCacheCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineCacheCreateInfo VkPipelineCacheCreateInfo registry at www.khronos.org>
type VkPipelineCacheCreateInfo =
     VkStruct VkPipelineCacheCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineCacheCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineCacheCreateInfo where
    type StructRep VkPipelineCacheCreateInfo =
         'StructMeta "VkPipelineCacheCreateInfo" VkPipelineCacheCreateInfo -- ' closing tick for hsc2hs
           #{size VkPipelineCacheCreateInfo}
           #{alignment VkPipelineCacheCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineCacheCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineCacheCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineCacheCreateFlags 'True
                                                                  #{offset VkPipelineCacheCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "initialDataSize" CSize 'True
                                                       #{offset VkPipelineCacheCreateInfo, initialDataSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pInitialData" (Ptr Void) 'False
                                                          #{offset VkPipelineCacheCreateInfo, pInitialData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineColorBlendAdvancedStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBool32               srcPremultiplied;
--   >     VkBool32               dstPremultiplied;
--   >     VkBlendOverlapEXT      blendOverlap;
--   > } VkPipelineColorBlendAdvancedStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineColorBlendAdvancedStateCreateInfoEXT VkPipelineColorBlendAdvancedStateCreateInfoEXT registry at www.khronos.org>
type VkPipelineColorBlendAdvancedStateCreateInfoEXT =
     VkStruct VkPipelineColorBlendAdvancedStateCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPipelineColorBlendAdvancedStateCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
    type StructRep VkPipelineColorBlendAdvancedStateCreateInfoEXT =
         'StructMeta "VkPipelineColorBlendAdvancedStateCreateInfoEXT" -- ' closing tick for hsc2hs
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
           #{size VkPipelineColorBlendAdvancedStateCreateInfoEXT}
           #{alignment VkPipelineColorBlendAdvancedStateCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcPremultiplied" VkBool32 'False
                                                            #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstPremultiplied" VkBool32 'False
                                                            #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "blendOverlap" VkBlendOverlapEXT 'False
                                                                 #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineColorBlendStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineColorBlendAttachmentState {
--   >     VkBool32               blendEnable;
--   >     VkBlendFactor          srcColorBlendFactor;
--   >     VkBlendFactor          dstColorBlendFactor;
--   >     VkBlendOp              colorBlendOp;
--   >     VkBlendFactor          srcAlphaBlendFactor;
--   >     VkBlendFactor          dstAlphaBlendFactor;
--   >     VkBlendOp              alphaBlendOp;
--   >     VkColorComponentFlags  colorWriteMask;
--   > } VkPipelineColorBlendAttachmentState;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineColorBlendAttachmentState VkPipelineColorBlendAttachmentState registry at www.khronos.org>
type VkPipelineColorBlendAttachmentState =
     VkStruct VkPipelineColorBlendAttachmentState' -- ' closing tick for hsc2hs

data VkPipelineColorBlendAttachmentState' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineColorBlendAttachmentState where
    type StructRep VkPipelineColorBlendAttachmentState =
         'StructMeta "VkPipelineColorBlendAttachmentState" -- ' closing tick for hsc2hs
           VkPipelineColorBlendAttachmentState
           #{size VkPipelineColorBlendAttachmentState}
           #{alignment VkPipelineColorBlendAttachmentState}
           '[('FieldMeta "blendEnable" VkBool32 'False  -- ' closing tick for hsc2hs
                                                       #{offset VkPipelineColorBlendAttachmentState, blendEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcColorBlendFactor" VkBlendFactor 'False
                                                                    #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstColorBlendFactor" VkBlendFactor 'False
                                                                    #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "colorBlendOp" VkBlendOp 'False
                                                         #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcAlphaBlendFactor" VkBlendFactor 'False
                                                                    #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstAlphaBlendFactor" VkBlendFactor 'False
                                                                    #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "alphaBlendOp" VkBlendOp 'False
                                                         #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "colorWriteMask" VkColorComponentFlags 'True
                                                                      #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineColorBlendStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineColorBlendStateCreateFlags    flags;
--   >     VkBool32               logicOpEnable;
--   >     VkLogicOp              logicOp;
--   >     uint32_t               attachmentCount;
--   >     const VkPipelineColorBlendAttachmentState* pAttachments;
--   >     float                  blendConstants[4];
--   > } VkPipelineColorBlendStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineColorBlendStateCreateInfo VkPipelineColorBlendStateCreateInfo registry at www.khronos.org>
type VkPipelineColorBlendStateCreateInfo =
     VkStruct VkPipelineColorBlendStateCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineColorBlendStateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineColorBlendStateCreateInfo where
    type StructRep VkPipelineColorBlendStateCreateInfo =
         'StructMeta "VkPipelineColorBlendStateCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineColorBlendStateCreateInfo
           #{size VkPipelineColorBlendStateCreateInfo}
           #{alignment VkPipelineColorBlendStateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineColorBlendStateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineColorBlendStateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineColorBlendStateCreateFlags 'True
                #{offset VkPipelineColorBlendStateCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "logicOpEnable" VkBool32 'False
                                                         #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "logicOp" VkLogicOp 'False
                                                    #{offset VkPipelineColorBlendStateCreateInfo, logicOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "attachmentCount" Word32 'True
                                                        #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttachments" -- ' closing tick for hsc2hs
                (Ptr VkPipelineColorBlendAttachmentState)
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "blendConstants" ( -- ' closing tick for hsc2hs
                                           #{type float}
                                           ) 'False -- ' closing tick for hsc2hs
                #{offset VkPipelineColorBlendStateCreateInfo, blendConstants}
                4
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineCompilerControlCreateInfoAMD {
--   >     VkStructureType   sType;
--   >     const void*                                                                            pNext;
--   >     VkPipelineCompilerControlFlagsAMD                                      compilerControlFlags;
--   > } VkPipelineCompilerControlCreateInfoAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineCompilerControlCreateInfoAMD VkPipelineCompilerControlCreateInfoAMD registry at www.khronos.org>
type VkPipelineCompilerControlCreateInfoAMD =
     VkStruct VkPipelineCompilerControlCreateInfoAMD' -- ' closing tick for hsc2hs

data VkPipelineCompilerControlCreateInfoAMD' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineCompilerControlCreateInfoAMD where
    type StructRep VkPipelineCompilerControlCreateInfoAMD =
         'StructMeta "VkPipelineCompilerControlCreateInfoAMD" -- ' closing tick for hsc2hs
           VkPipelineCompilerControlCreateInfoAMD
           #{size VkPipelineCompilerControlCreateInfoAMD}
           #{alignment VkPipelineCompilerControlCreateInfoAMD}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineCompilerControlCreateInfoAMD, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineCompilerControlCreateInfoAMD, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compilerControlFlags" -- ' closing tick for hsc2hs
                VkPipelineCompilerControlFlagsAMD
                'True -- ' closing tick for hsc2hs
                #{offset VkPipelineCompilerControlCreateInfoAMD, compilerControlFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkGraphicsPipelineCreateInfo, VkComputePipelineCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineCoverageModulationStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineCoverageModulationStateCreateFlagsNV                   flags;
--   >     VkCoverageModulationModeNV                                                       coverageModulationMode;
--   >     VkBool32                                                                         coverageModulationTableEnable;
--   >     uint32_t                                                         coverageModulationTableCount;
--   >     const float* pCoverageModulationTable;
--   > } VkPipelineCoverageModulationStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineCoverageModulationStateCreateInfoNV VkPipelineCoverageModulationStateCreateInfoNV registry at www.khronos.org>
type VkPipelineCoverageModulationStateCreateInfoNV =
     VkStruct VkPipelineCoverageModulationStateCreateInfoNV' -- ' closing tick for hsc2hs

data VkPipelineCoverageModulationStateCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineCoverageModulationStateCreateInfoNV
         where
    type StructRep VkPipelineCoverageModulationStateCreateInfoNV =
         'StructMeta "VkPipelineCoverageModulationStateCreateInfoNV" -- ' closing tick for hsc2hs
           VkPipelineCoverageModulationStateCreateInfoNV
           #{size VkPipelineCoverageModulationStateCreateInfoNV}
           #{alignment VkPipelineCoverageModulationStateCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineCoverageModulationStateCreateFlagsNV -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "coverageModulationMode" VkCoverageModulationModeNV -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "coverageModulationTableEnable" VkBool32 'False
                #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "coverageModulationTableCount" Word32 'True
                                                                     #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pCoverageModulationTable" -- ' closing tick for hsc2hs
                (Ptr #{type float})
                'True -- ' closing tick for hsc2hs
                #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineCoverageReductionStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                        pNext;
--   >     VkPipelineCoverageReductionStateCreateFlagsNV      flags;
--   >     VkCoverageReductionModeNV                                          coverageReductionMode;
--   > } VkPipelineCoverageReductionStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineCoverageReductionStateCreateInfoNV VkPipelineCoverageReductionStateCreateInfoNV registry at www.khronos.org>
type VkPipelineCoverageReductionStateCreateInfoNV =
     VkStruct VkPipelineCoverageReductionStateCreateInfoNV' -- ' closing tick for hsc2hs

data VkPipelineCoverageReductionStateCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineCoverageReductionStateCreateInfoNV
         where
    type StructRep VkPipelineCoverageReductionStateCreateInfoNV =
         'StructMeta "VkPipelineCoverageReductionStateCreateInfoNV" -- ' closing tick for hsc2hs
           VkPipelineCoverageReductionStateCreateInfoNV
           #{size VkPipelineCoverageReductionStateCreateInfoNV}
           #{alignment VkPipelineCoverageReductionStateCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineCoverageReductionStateCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineCoverageReductionStateCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineCoverageReductionStateCreateFlagsNV -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkPipelineCoverageReductionStateCreateInfoNV, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "coverageReductionMode" VkCoverageReductionModeNV -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineCoverageReductionStateCreateInfoNV, coverageReductionMode}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineCoverageToColorStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineCoverageToColorStateCreateFlagsNV                    flags;
--   >     VkBool32                         coverageToColorEnable;
--   >     uint32_t         coverageToColorLocation;
--   > } VkPipelineCoverageToColorStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineCoverageToColorStateCreateInfoNV VkPipelineCoverageToColorStateCreateInfoNV registry at www.khronos.org>
type VkPipelineCoverageToColorStateCreateInfoNV =
     VkStruct VkPipelineCoverageToColorStateCreateInfoNV' -- ' closing tick for hsc2hs

data VkPipelineCoverageToColorStateCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineCoverageToColorStateCreateInfoNV
         where
    type StructRep VkPipelineCoverageToColorStateCreateInfoNV =
         'StructMeta "VkPipelineCoverageToColorStateCreateInfoNV" -- ' closing tick for hsc2hs
           VkPipelineCoverageToColorStateCreateInfoNV
           #{size VkPipelineCoverageToColorStateCreateInfoNV}
           #{alignment VkPipelineCoverageToColorStateCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineCoverageToColorStateCreateFlagsNV -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "coverageToColorEnable" VkBool32 'False
                                                                 #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "coverageToColorLocation" Word32 'True
                                                                #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineCreationFeedbackCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     VkPipelineCreationFeedbackEXT*      pPipelineCreationFeedback;
--   >     uint32_t                            pipelineStageCreationFeedbackCount;
--   >     VkPipelineCreationFeedbackEXT* pPipelineStageCreationFeedbacks;
--   > } VkPipelineCreationFeedbackCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineCreationFeedbackCreateInfoEXT VkPipelineCreationFeedbackCreateInfoEXT registry at www.khronos.org>
type VkPipelineCreationFeedbackCreateInfoEXT =
     VkStruct VkPipelineCreationFeedbackCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPipelineCreationFeedbackCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineCreationFeedbackCreateInfoEXT
         where
    type StructRep VkPipelineCreationFeedbackCreateInfoEXT =
         'StructMeta "VkPipelineCreationFeedbackCreateInfoEXT" -- ' closing tick for hsc2hs
           VkPipelineCreationFeedbackCreateInfoEXT
           #{size VkPipelineCreationFeedbackCreateInfoEXT}
           #{alignment VkPipelineCreationFeedbackCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineCreationFeedbackCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineCreationFeedbackCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pPipelineCreationFeedback" -- ' closing tick for hsc2hs
                (Ptr VkPipelineCreationFeedbackEXT)
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineCreationFeedbackCreateInfoEXT, pPipelineCreationFeedback}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineStageCreationFeedbackCount" Word32 'False
                #{offset VkPipelineCreationFeedbackCreateInfoEXT, pipelineStageCreationFeedbackCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pPipelineStageCreationFeedbacks" -- ' closing tick for hsc2hs
                (Ptr VkPipelineCreationFeedbackEXT)
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineCreationFeedbackCreateInfoEXT, pPipelineStageCreationFeedbacks}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkGraphicsPipelineCreateInfo, VkComputePipelineCreateInfo -- ' closing tick for hsc2hs
#ifdef VK_ENABLE_BETA_EXTENSIONS
            , VkRayTracingPipelineCreateInfoNV
            , VkRayTracingPipelineCreateInfoKHR
#endif
            ]

-- | > typedef struct VkPipelineCreationFeedbackEXT {
--   >     VkPipelineCreationFeedbackFlagsEXT  flags;
--   >     uint64_t                            duration;
--   > } VkPipelineCreationFeedbackEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineCreationFeedbackEXT VkPipelineCreationFeedbackEXT registry at www.khronos.org>
type VkPipelineCreationFeedbackEXT =
     VkStruct VkPipelineCreationFeedbackEXT' -- ' closing tick for hsc2hs

data VkPipelineCreationFeedbackEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineCreationFeedbackEXT where
    type StructRep VkPipelineCreationFeedbackEXT =
         'StructMeta "VkPipelineCreationFeedbackEXT" -- ' closing tick for hsc2hs
           VkPipelineCreationFeedbackEXT
           #{size VkPipelineCreationFeedbackEXT}
           #{alignment VkPipelineCreationFeedbackEXT}
           '[('FieldMeta "flags" VkPipelineCreationFeedbackFlagsEXT 'False -- ' closing tick for hsc2hs
                #{offset VkPipelineCreationFeedbackEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "duration" Word64 'False
                                                  #{offset VkPipelineCreationFeedbackEXT, duration}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineDepthStencilStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineDepthStencilStateCreateFlags    flags;
--   >     VkBool32               depthTestEnable;
--   >     VkBool32               depthWriteEnable;
--   >     VkCompareOp            depthCompareOp;
--   >     VkBool32               depthBoundsTestEnable;
--   >     VkBool32               stencilTestEnable;
--   >     VkStencilOpState       front;
--   >     VkStencilOpState       back;
--   >     float                  minDepthBounds;
--   >     float                  maxDepthBounds;
--   > } VkPipelineDepthStencilStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineDepthStencilStateCreateInfo VkPipelineDepthStencilStateCreateInfo registry at www.khronos.org>
type VkPipelineDepthStencilStateCreateInfo =
     VkStruct VkPipelineDepthStencilStateCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineDepthStencilStateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineDepthStencilStateCreateInfo where
    type StructRep VkPipelineDepthStencilStateCreateInfo =
         'StructMeta "VkPipelineDepthStencilStateCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineDepthStencilStateCreateInfo
           #{size VkPipelineDepthStencilStateCreateInfo}
           #{alignment VkPipelineDepthStencilStateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineDepthStencilStateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineDepthStencilStateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineDepthStencilStateCreateFlags 'True
                #{offset VkPipelineDepthStencilStateCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthTestEnable" VkBool32 'False
                                                           #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthWriteEnable" VkBool32 'False
                                                            #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthCompareOp" VkCompareOp 'False
                                                             #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthBoundsTestEnable" VkBool32 'False
                                                                 #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencilTestEnable" VkBool32 'False
                                                             #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "front" VkStencilOpState 'False
                                                         #{offset VkPipelineDepthStencilStateCreateInfo, front}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "back" VkStencilOpState 'False
                                                        #{offset VkPipelineDepthStencilStateCreateInfo, back}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minDepthBounds" ( -- ' closing tick for hsc2hs
                                           #{type float}
                                           ) 'False -- ' closing tick for hsc2hs
                #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDepthBounds" ( -- ' closing tick for hsc2hs
                                           #{type float}
                                           ) 'False -- ' closing tick for hsc2hs
                #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineDiscardRectangleStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                       pNext;
--   >     VkPipelineDiscardRectangleStateCreateFlagsEXT     flags;
--   >     VkDiscardRectangleModeEXT                                         discardRectangleMode;
--   >     uint32_t                                          discardRectangleCount;
--   >     const VkRect2D* pDiscardRectangles;
--   > } VkPipelineDiscardRectangleStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineDiscardRectangleStateCreateInfoEXT VkPipelineDiscardRectangleStateCreateInfoEXT registry at www.khronos.org>
type VkPipelineDiscardRectangleStateCreateInfoEXT =
     VkStruct VkPipelineDiscardRectangleStateCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPipelineDiscardRectangleStateCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineDiscardRectangleStateCreateInfoEXT
         where
    type StructRep VkPipelineDiscardRectangleStateCreateInfoEXT =
         'StructMeta "VkPipelineDiscardRectangleStateCreateInfoEXT" -- ' closing tick for hsc2hs
           VkPipelineDiscardRectangleStateCreateInfoEXT
           #{size VkPipelineDiscardRectangleStateCreateInfoEXT}
           #{alignment VkPipelineDiscardRectangleStateCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineDiscardRectangleStateCreateFlagsEXT -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "discardRectangleMode" VkDiscardRectangleModeEXT 'False
                #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "discardRectangleCount" Word32 'True
                                                              #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDiscardRectangles" (Ptr VkRect2D) 'False
                                                                    #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkGraphicsPipelineCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineDynamicStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineDynamicStateCreateFlags    flags;
--   >     uint32_t               dynamicStateCount;
--   >     const VkDynamicState*  pDynamicStates;
--   > } VkPipelineDynamicStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineDynamicStateCreateInfo VkPipelineDynamicStateCreateInfo registry at www.khronos.org>
type VkPipelineDynamicStateCreateInfo =
     VkStruct VkPipelineDynamicStateCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineDynamicStateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineDynamicStateCreateInfo where
    type StructRep VkPipelineDynamicStateCreateInfo =
         'StructMeta "VkPipelineDynamicStateCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineDynamicStateCreateInfo
           #{size VkPipelineDynamicStateCreateInfo}
           #{alignment VkPipelineDynamicStateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineDynamicStateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineDynamicStateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineDynamicStateCreateFlags 'True
                #{offset VkPipelineDynamicStateCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dynamicStateCount" Word32 'True
                                                          #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDynamicStates" (Ptr VkDynamicState) 'False
                                                                      #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineExecutableInfoKHR {
--   >     VkStructureType sType;
--   >     const void*        pNext;
--   >     VkPipeline         pipeline;
--   >     uint32_t           executableIndex;
--   > } VkPipelineExecutableInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineExecutableInfoKHR VkPipelineExecutableInfoKHR registry at www.khronos.org>
type VkPipelineExecutableInfoKHR =
     VkStruct VkPipelineExecutableInfoKHR' -- ' closing tick for hsc2hs

data VkPipelineExecutableInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineExecutableInfoKHR where
    type StructRep VkPipelineExecutableInfoKHR =
         'StructMeta "VkPipelineExecutableInfoKHR" -- ' closing tick for hsc2hs
           VkPipelineExecutableInfoKHR
           #{size VkPipelineExecutableInfoKHR}
           #{alignment VkPipelineExecutableInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineExecutableInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineExecutableInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipeline" VkPipeline 'False
                                                      #{offset VkPipelineExecutableInfoKHR, pipeline}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "executableIndex" Word32 'False
                                                         #{offset VkPipelineExecutableInfoKHR, executableIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineExecutableInternalRepresentationKHR {
--   >     VkStructureType sType;
--   >     void*              pNext;
--   >     char               name[VK_MAX_DESCRIPTION_SIZE];
--   >     char               description[VK_MAX_DESCRIPTION_SIZE];
--   >     VkBool32           isText;
--   >     size_t               dataSize;
--   >     void* pData;
--   > } VkPipelineExecutableInternalRepresentationKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineExecutableInternalRepresentationKHR VkPipelineExecutableInternalRepresentationKHR registry at www.khronos.org>
type VkPipelineExecutableInternalRepresentationKHR =
     VkStruct VkPipelineExecutableInternalRepresentationKHR' -- ' closing tick for hsc2hs

data VkPipelineExecutableInternalRepresentationKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineExecutableInternalRepresentationKHR
         where
    type StructRep VkPipelineExecutableInternalRepresentationKHR =
         'StructMeta "VkPipelineExecutableInternalRepresentationKHR" -- ' closing tick for hsc2hs
           VkPipelineExecutableInternalRepresentationKHR
           #{size VkPipelineExecutableInternalRepresentationKHR}
           #{alignment VkPipelineExecutableInternalRepresentationKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineExecutableInternalRepresentationKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineExecutableInternalRepresentationKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" CChar 'False
                                             #{offset VkPipelineExecutableInternalRepresentationKHR, name}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "description" CChar 'False
                                                    #{offset VkPipelineExecutableInternalRepresentationKHR, description}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "isText" VkBool32 'False
                                                  #{offset VkPipelineExecutableInternalRepresentationKHR, isText}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dataSize" CSize 'True
                                                #{offset VkPipelineExecutableInternalRepresentationKHR, dataSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pData" (Ptr Void) 'True
                                                  #{offset VkPipelineExecutableInternalRepresentationKHR, pData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineExecutablePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*              pNext;
--   >     VkShaderStageFlags stages;
--   >     char               name[VK_MAX_DESCRIPTION_SIZE];
--   >     char               description[VK_MAX_DESCRIPTION_SIZE];
--   >     uint32_t           subgroupSize;
--   > } VkPipelineExecutablePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineExecutablePropertiesKHR VkPipelineExecutablePropertiesKHR registry at www.khronos.org>
type VkPipelineExecutablePropertiesKHR =
     VkStruct VkPipelineExecutablePropertiesKHR' -- ' closing tick for hsc2hs

data VkPipelineExecutablePropertiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineExecutablePropertiesKHR where
    type StructRep VkPipelineExecutablePropertiesKHR =
         'StructMeta "VkPipelineExecutablePropertiesKHR" -- ' closing tick for hsc2hs
           VkPipelineExecutablePropertiesKHR
           #{size VkPipelineExecutablePropertiesKHR}
           #{alignment VkPipelineExecutablePropertiesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineExecutablePropertiesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineExecutablePropertiesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stages" VkShaderStageFlags 'False
                                                            #{offset VkPipelineExecutablePropertiesKHR, stages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" CChar 'False
                                             #{offset VkPipelineExecutablePropertiesKHR, name}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "description" CChar 'False
                                                    #{offset VkPipelineExecutablePropertiesKHR, description}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subgroupSize" Word32 'False
                                                      #{offset VkPipelineExecutablePropertiesKHR, subgroupSize}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineExecutableStatisticKHR {
--   >     VkStructureType sType;
--   >     void*              pNext;
--   >     char               name[VK_MAX_DESCRIPTION_SIZE];
--   >     char               description[VK_MAX_DESCRIPTION_SIZE];
--   >     VkPipelineExecutableStatisticFormatKHR format;
--   >     VkPipelineExecutableStatisticValueKHR  value;
--   > } VkPipelineExecutableStatisticKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineExecutableStatisticKHR VkPipelineExecutableStatisticKHR registry at www.khronos.org>
type VkPipelineExecutableStatisticKHR =
     VkStruct VkPipelineExecutableStatisticKHR' -- ' closing tick for hsc2hs

data VkPipelineExecutableStatisticKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineExecutableStatisticKHR where
    type StructRep VkPipelineExecutableStatisticKHR =
         'StructMeta "VkPipelineExecutableStatisticKHR" -- ' closing tick for hsc2hs
           VkPipelineExecutableStatisticKHR
           #{size VkPipelineExecutableStatisticKHR}
           #{alignment VkPipelineExecutableStatisticKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineExecutableStatisticKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineExecutableStatisticKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" CChar 'False
                                             #{offset VkPipelineExecutableStatisticKHR, name}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "description" CChar 'False
                                                    #{offset VkPipelineExecutableStatisticKHR, description}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkPipelineExecutableStatisticFormatKHR 'False
                #{offset VkPipelineExecutableStatisticKHR, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "value" VkPipelineExecutableStatisticValueKHR 'False
                #{offset VkPipelineExecutableStatisticKHR, value}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef union VkPipelineExecutableStatisticValueKHR {
--   >     VkBool32           b32;
--   >     int64_t            i64;
--   >     uint64_t           u64;
--   >     double             f64;
--   > } VkPipelineExecutableStatisticValueKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineExecutableStatisticValueKHR VkPipelineExecutableStatisticValueKHR registry at www.khronos.org>
type VkPipelineExecutableStatisticValueKHR =
     VkStruct VkPipelineExecutableStatisticValueKHR' -- ' closing tick for hsc2hs

data VkPipelineExecutableStatisticValueKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineExecutableStatisticValueKHR where
    type StructRep VkPipelineExecutableStatisticValueKHR =
         'StructMeta "VkPipelineExecutableStatisticValueKHR" -- ' closing tick for hsc2hs
           VkPipelineExecutableStatisticValueKHR
           #{size VkPipelineExecutableStatisticValueKHR}
           #{alignment VkPipelineExecutableStatisticValueKHR}
           '[('FieldMeta "b32" VkBool32 'False  -- ' closing tick for hsc2hs
                                               #{offset VkPipelineExecutableStatisticValueKHR, b32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "i64" Int64 'False
                                            #{offset VkPipelineExecutableStatisticValueKHR, i64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "u64" Word64 'False
                                             #{offset VkPipelineExecutableStatisticValueKHR, u64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "f64" ( -- ' closing tick for hsc2hs
                                #{type double}
                                ) 'False  -- ' closing tick for hsc2hs
                                         #{offset VkPipelineExecutableStatisticValueKHR, f64}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineInfoKHR {
--   >     VkStructureType sType;
--   >     const void*        pNext;
--   >     VkPipeline         pipeline;
--   > } VkPipelineInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineInfoKHR VkPipelineInfoKHR registry at www.khronos.org>
type VkPipelineInfoKHR = VkStruct VkPipelineInfoKHR' -- ' closing tick for hsc2hs

data VkPipelineInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineInfoKHR where
    type StructRep VkPipelineInfoKHR =
         'StructMeta "VkPipelineInfoKHR" VkPipelineInfoKHR  -- ' closing tick for hsc2hs
                                                           #{size VkPipelineInfoKHR}
           #{alignment VkPipelineInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipeline" VkPipeline 'False
                                                      #{offset VkPipelineInfoKHR, pipeline}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineInputAssemblyStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineInputAssemblyStateCreateFlags    flags;
--   >     VkPrimitiveTopology    topology;
--   >     VkBool32               primitiveRestartEnable;
--   > } VkPipelineInputAssemblyStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineInputAssemblyStateCreateInfo VkPipelineInputAssemblyStateCreateInfo registry at www.khronos.org>
type VkPipelineInputAssemblyStateCreateInfo =
     VkStruct VkPipelineInputAssemblyStateCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineInputAssemblyStateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineInputAssemblyStateCreateInfo where
    type StructRep VkPipelineInputAssemblyStateCreateInfo =
         'StructMeta "VkPipelineInputAssemblyStateCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineInputAssemblyStateCreateInfo
           #{size VkPipelineInputAssemblyStateCreateInfo}
           #{alignment VkPipelineInputAssemblyStateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineInputAssemblyStateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineInputAssemblyStateCreateFlags 'True
                #{offset VkPipelineInputAssemblyStateCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "topology" VkPrimitiveTopology 'False
                                                               #{offset VkPipelineInputAssemblyStateCreateInfo, topology}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "primitiveRestartEnable" VkBool32 'False
                                                                  #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineLayoutCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineLayoutCreateFlags    flags;
--   >     uint32_t               setLayoutCount;
--   >     const VkDescriptorSetLayout* pSetLayouts;
--   >     uint32_t               pushConstantRangeCount;
--   >     const VkPushConstantRange* pPushConstantRanges;
--   > } VkPipelineLayoutCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineLayoutCreateInfo VkPipelineLayoutCreateInfo registry at www.khronos.org>
type VkPipelineLayoutCreateInfo =
     VkStruct VkPipelineLayoutCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineLayoutCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineLayoutCreateInfo where
    type StructRep VkPipelineLayoutCreateInfo =
         'StructMeta "VkPipelineLayoutCreateInfo" VkPipelineLayoutCreateInfo -- ' closing tick for hsc2hs
           #{size VkPipelineLayoutCreateInfo}
           #{alignment VkPipelineLayoutCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineLayoutCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineLayoutCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineLayoutCreateFlags 'True
                                                                   #{offset VkPipelineLayoutCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "setLayoutCount" Word32 'True
                                                       #{offset VkPipelineLayoutCreateInfo, setLayoutCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSetLayouts" (Ptr VkDescriptorSetLayout) 'False
                #{offset VkPipelineLayoutCreateInfo, pSetLayouts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pushConstantRangeCount" Word32 'True
                                                               #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pPushConstantRanges" (Ptr VkPushConstantRange) 'False
                #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineMultisampleStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineMultisampleStateCreateFlags    flags;
--   >     VkSampleCountFlagBits  rasterizationSamples;
--   >     VkBool32               sampleShadingEnable;
--   >     float                  minSampleShading;
--   >     const VkSampleMask*    pSampleMask;
--   >     VkBool32               alphaToCoverageEnable;
--   >     VkBool32               alphaToOneEnable;
--   > } VkPipelineMultisampleStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineMultisampleStateCreateInfo VkPipelineMultisampleStateCreateInfo registry at www.khronos.org>
type VkPipelineMultisampleStateCreateInfo =
     VkStruct VkPipelineMultisampleStateCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineMultisampleStateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineMultisampleStateCreateInfo where
    type StructRep VkPipelineMultisampleStateCreateInfo =
         'StructMeta "VkPipelineMultisampleStateCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineMultisampleStateCreateInfo
           #{size VkPipelineMultisampleStateCreateInfo}
           #{alignment VkPipelineMultisampleStateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineMultisampleStateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineMultisampleStateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineMultisampleStateCreateFlags 'True
                #{offset VkPipelineMultisampleStateCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rasterizationSamples" VkSampleCountFlagBits 'False
                #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleShadingEnable" VkBool32 'False
                                                               #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minSampleShading" ( -- ' closing tick for hsc2hs
                                             #{type float}
                                             ) 'False -- ' closing tick for hsc2hs
                #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSampleMask" (Ptr VkSampleMask) 'True
                                                                #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "alphaToCoverageEnable" VkBool32 'False
                                                                 #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "alphaToOneEnable" VkBool32 'False
                                                            #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineRasterizationConservativeStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineRasterizationConservativeStateCreateFlagsEXT           flags;
--   >     VkConservativeRasterizationModeEXT                                               conservativeRasterizationMode;
--   >     float                                                                            extraPrimitiveOverestimationSize;
--   > } VkPipelineRasterizationConservativeStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineRasterizationConservativeStateCreateInfoEXT VkPipelineRasterizationConservativeStateCreateInfoEXT registry at www.khronos.org>
type VkPipelineRasterizationConservativeStateCreateInfoEXT =
     VkStruct VkPipelineRasterizationConservativeStateCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPipelineRasterizationConservativeStateCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
    type StructRep
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         =
         'StructMeta "VkPipelineRasterizationConservativeStateCreateInfoEXT" -- ' closing tick for hsc2hs
           VkPipelineRasterizationConservativeStateCreateInfoEXT
           #{size VkPipelineRasterizationConservativeStateCreateInfoEXT}
           #{alignment VkPipelineRasterizationConservativeStateCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" -- ' closing tick for hsc2hs
                VkPipelineRasterizationConservativeStateCreateFlagsEXT
                'True -- ' closing tick for hsc2hs
                #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "conservativeRasterizationMode" -- ' closing tick for hsc2hs
                VkConservativeRasterizationModeEXT
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "extraPrimitiveOverestimationSize" -- ' closing tick for hsc2hs
                (#{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineRasterizationStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineRasterizationDepthClipStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                                 pNext;
--   >     VkPipelineRasterizationDepthClipStateCreateFlagsEXT         flags;
--   >     VkBool32                                                                    depthClipEnable;
--   > } VkPipelineRasterizationDepthClipStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineRasterizationDepthClipStateCreateInfoEXT VkPipelineRasterizationDepthClipStateCreateInfoEXT registry at www.khronos.org>
type VkPipelineRasterizationDepthClipStateCreateInfoEXT =
     VkStruct VkPipelineRasterizationDepthClipStateCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPipelineRasterizationDepthClipStateCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineRasterizationDepthClipStateCreateInfoEXT
         where
    type StructRep VkPipelineRasterizationDepthClipStateCreateInfoEXT =
         'StructMeta "VkPipelineRasterizationDepthClipStateCreateInfoEXT" -- ' closing tick for hsc2hs
           VkPipelineRasterizationDepthClipStateCreateInfoEXT
           #{size VkPipelineRasterizationDepthClipStateCreateInfoEXT}
           #{alignment VkPipelineRasterizationDepthClipStateCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineRasterizationDepthClipStateCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineRasterizationDepthClipStateCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" -- ' closing tick for hsc2hs
                VkPipelineRasterizationDepthClipStateCreateFlagsEXT
                'True -- ' closing tick for hsc2hs
                #{offset VkPipelineRasterizationDepthClipStateCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthClipEnable" VkBool32 'False
                                                           #{offset VkPipelineRasterizationDepthClipStateCreateInfoEXT, depthClipEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineRasterizationStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineRasterizationLineStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                      pNext;
--   >     VkLineRasterizationModeEXT                                       lineRasterizationMode;
--   >     VkBool32                                                         stippledLineEnable;
--   >     uint32_t                                         lineStippleFactor;
--   >     uint16_t                                         lineStipplePattern;
--   > } VkPipelineRasterizationLineStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineRasterizationLineStateCreateInfoEXT VkPipelineRasterizationLineStateCreateInfoEXT registry at www.khronos.org>
type VkPipelineRasterizationLineStateCreateInfoEXT =
     VkStruct VkPipelineRasterizationLineStateCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPipelineRasterizationLineStateCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineRasterizationLineStateCreateInfoEXT
         where
    type StructRep VkPipelineRasterizationLineStateCreateInfoEXT =
         'StructMeta "VkPipelineRasterizationLineStateCreateInfoEXT" -- ' closing tick for hsc2hs
           VkPipelineRasterizationLineStateCreateInfoEXT
           #{size VkPipelineRasterizationLineStateCreateInfoEXT}
           #{alignment VkPipelineRasterizationLineStateCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineRasterizationLineStateCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineRasterizationLineStateCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "lineRasterizationMode" VkLineRasterizationModeEXT -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineRasterizationLineStateCreateInfoEXT, lineRasterizationMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stippledLineEnable" VkBool32 'False
                                                              #{offset VkPipelineRasterizationLineStateCreateInfoEXT, stippledLineEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "lineStippleFactor" Word32 'True
                                                          #{offset VkPipelineRasterizationLineStateCreateInfoEXT, lineStippleFactor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "lineStipplePattern" Word16 'True
                                                           #{offset VkPipelineRasterizationLineStateCreateInfoEXT, lineStipplePattern}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineRasterizationStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineRasterizationStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkPipelineRasterizationStateCreateFlags    flags;
--   >     VkBool32               depthClampEnable;
--   >     VkBool32               rasterizerDiscardEnable;
--   >     VkPolygonMode          polygonMode;
--   >     VkCullModeFlags        cullMode;
--   >     VkFrontFace            frontFace;
--   >     VkBool32               depthBiasEnable;
--   >     float                  depthBiasConstantFactor;
--   >     float                  depthBiasClamp;
--   >     float                  depthBiasSlopeFactor;
--   >     float                  lineWidth;
--   > } VkPipelineRasterizationStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineRasterizationStateCreateInfo VkPipelineRasterizationStateCreateInfo registry at www.khronos.org>
type VkPipelineRasterizationStateCreateInfo =
     VkStruct VkPipelineRasterizationStateCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineRasterizationStateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineRasterizationStateCreateInfo where
    type StructRep VkPipelineRasterizationStateCreateInfo =
         'StructMeta "VkPipelineRasterizationStateCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineRasterizationStateCreateInfo
           #{size VkPipelineRasterizationStateCreateInfo}
           #{alignment VkPipelineRasterizationStateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineRasterizationStateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineRasterizationStateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineRasterizationStateCreateFlags 'True
                #{offset VkPipelineRasterizationStateCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthClampEnable" VkBool32 'False
                                                            #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rasterizerDiscardEnable" VkBool32 'False
                                                                   #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "polygonMode" VkPolygonMode 'False
                                                            #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "cullMode" VkCullModeFlags 'True
                                                          #{offset VkPipelineRasterizationStateCreateInfo, cullMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "frontFace" VkFrontFace 'False
                                                        #{offset VkPipelineRasterizationStateCreateInfo, frontFace}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthBiasEnable" VkBool32 'False
                                                           #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthBiasConstantFactor" ( -- ' closing tick for hsc2hs
                                                    #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthBiasClamp" ( -- ' closing tick for hsc2hs
                                           #{type float}
                                           ) 'False -- ' closing tick for hsc2hs
                #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthBiasSlopeFactor" ( -- ' closing tick for hsc2hs
                                                 #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "lineWidth" ( -- ' closing tick for hsc2hs
                                      #{type float}
                                      ) 'False -- ' closing tick for hsc2hs
                #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineRasterizationStateRasterizationOrderAMD {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkRasterizationOrderAMD          rasterizationOrder;
--   > } VkPipelineRasterizationStateRasterizationOrderAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineRasterizationStateRasterizationOrderAMD VkPipelineRasterizationStateRasterizationOrderAMD registry at www.khronos.org>
type VkPipelineRasterizationStateRasterizationOrderAMD =
     VkStruct VkPipelineRasterizationStateRasterizationOrderAMD' -- ' closing tick for hsc2hs

data VkPipelineRasterizationStateRasterizationOrderAMD' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
    type StructRep VkPipelineRasterizationStateRasterizationOrderAMD =
         'StructMeta "VkPipelineRasterizationStateRasterizationOrderAMD" -- ' closing tick for hsc2hs
           VkPipelineRasterizationStateRasterizationOrderAMD
           #{size VkPipelineRasterizationStateRasterizationOrderAMD}
           #{alignment VkPipelineRasterizationStateRasterizationOrderAMD}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rasterizationOrder" VkRasterizationOrderAMD 'False
                #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineRasterizationStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineRasterizationStateStreamCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineRasterizationStateStreamCreateFlagsEXT                 flags;
--   >     uint32_t                                                                         rasterizationStream;
--   > } VkPipelineRasterizationStateStreamCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineRasterizationStateStreamCreateInfoEXT VkPipelineRasterizationStateStreamCreateInfoEXT registry at www.khronos.org>
type VkPipelineRasterizationStateStreamCreateInfoEXT =
     VkStruct VkPipelineRasterizationStateStreamCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPipelineRasterizationStateStreamCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineRasterizationStateStreamCreateInfoEXT
         where
    type StructRep VkPipelineRasterizationStateStreamCreateInfoEXT =
         'StructMeta "VkPipelineRasterizationStateStreamCreateInfoEXT" -- ' closing tick for hsc2hs
           VkPipelineRasterizationStateStreamCreateInfoEXT
           #{size VkPipelineRasterizationStateStreamCreateInfoEXT}
           #{alignment VkPipelineRasterizationStateStreamCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineRasterizationStateStreamCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineRasterizationStateStreamCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" -- ' closing tick for hsc2hs
                VkPipelineRasterizationStateStreamCreateFlagsEXT
                'True -- ' closing tick for hsc2hs
                #{offset VkPipelineRasterizationStateStreamCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rasterizationStream" Word32 'False
                                                             #{offset VkPipelineRasterizationStateStreamCreateInfoEXT, rasterizationStream}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineRasterizationStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineRepresentativeFragmentTestStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*    pNext;
--   >     VkBool32       representativeFragmentTestEnable;
--   > } VkPipelineRepresentativeFragmentTestStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineRepresentativeFragmentTestStateCreateInfoNV VkPipelineRepresentativeFragmentTestStateCreateInfoNV registry at www.khronos.org>
type VkPipelineRepresentativeFragmentTestStateCreateInfoNV =
     VkStruct VkPipelineRepresentativeFragmentTestStateCreateInfoNV' -- ' closing tick for hsc2hs

data VkPipelineRepresentativeFragmentTestStateCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineRepresentativeFragmentTestStateCreateInfoNV
         where
    type StructRep
           VkPipelineRepresentativeFragmentTestStateCreateInfoNV
         =
         'StructMeta "VkPipelineRepresentativeFragmentTestStateCreateInfoNV" -- ' closing tick for hsc2hs
           VkPipelineRepresentativeFragmentTestStateCreateInfoNV
           #{size VkPipelineRepresentativeFragmentTestStateCreateInfoNV}
           #{alignment VkPipelineRepresentativeFragmentTestStateCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineRepresentativeFragmentTestStateCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineRepresentativeFragmentTestStateCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "representativeFragmentTestEnable" VkBool32 'False
                #{offset VkPipelineRepresentativeFragmentTestStateCreateInfoNV, representativeFragmentTestEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkGraphicsPipelineCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineSampleLocationsStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         sampleLocationsEnable;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkPipelineSampleLocationsStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineSampleLocationsStateCreateInfoEXT VkPipelineSampleLocationsStateCreateInfoEXT registry at www.khronos.org>
type VkPipelineSampleLocationsStateCreateInfoEXT =
     VkStruct VkPipelineSampleLocationsStateCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPipelineSampleLocationsStateCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineSampleLocationsStateCreateInfoEXT
         where
    type StructRep VkPipelineSampleLocationsStateCreateInfoEXT =
         'StructMeta "VkPipelineSampleLocationsStateCreateInfoEXT" -- ' closing tick for hsc2hs
           VkPipelineSampleLocationsStateCreateInfoEXT
           #{size VkPipelineSampleLocationsStateCreateInfoEXT}
           #{alignment VkPipelineSampleLocationsStateCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationsEnable" VkBool32 'False
                                                                 #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationsInfo" VkSampleLocationsInfoEXT 'False
                #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineShaderStageCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineShaderStageCreateFlags    flags;
--   >     VkShaderStageFlagBits  stage;
--   >     VkShaderModule         module;
--   >     const char*            pName;
--   >     const VkSpecializationInfo* pSpecializationInfo;
--   > } VkPipelineShaderStageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineShaderStageCreateInfo VkPipelineShaderStageCreateInfo registry at www.khronos.org>
type VkPipelineShaderStageCreateInfo =
     VkStruct VkPipelineShaderStageCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineShaderStageCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineShaderStageCreateInfo where
    type StructRep VkPipelineShaderStageCreateInfo =
         'StructMeta "VkPipelineShaderStageCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineShaderStageCreateInfo
           #{size VkPipelineShaderStageCreateInfo}
           #{alignment VkPipelineShaderStageCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineShaderStageCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineShaderStageCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineShaderStageCreateFlags 'True
                #{offset VkPipelineShaderStageCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stage" VkShaderStageFlagBits 'False
                                                              #{offset VkPipelineShaderStageCreateInfo, stage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "module" VkShaderModule 'False
                                                        #{offset VkPipelineShaderStageCreateInfo, module}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pName" CString 'False
                                                #{offset VkPipelineShaderStageCreateInfo, pName}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSpecializationInfo" (Ptr VkSpecializationInfo) 'True
                #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               requiredSubgroupSize;
--   > } VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT registry at www.khronos.org>
type VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT =
     VkStruct VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
         where
    type StructRep
           VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
         =
         'StructMeta -- ' closing tick for hsc2hs
           "VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT"
           VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
           #{size VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT}
           #{alignment VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "requiredSubgroupSize" Word32 'False
                                                              #{offset VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT, requiredSubgroupSize}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPipelineShaderStageCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineTessellationDomainOriginStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkTessellationDomainOrigin    domainOrigin;
--   > } VkPipelineTessellationDomainOriginStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineTessellationDomainOriginStateCreateInfo VkPipelineTessellationDomainOriginStateCreateInfo registry at www.khronos.org>
type VkPipelineTessellationDomainOriginStateCreateInfo =
     VkStruct VkPipelineTessellationDomainOriginStateCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineTessellationDomainOriginStateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
    type StructRep VkPipelineTessellationDomainOriginStateCreateInfo =
         'StructMeta "VkPipelineTessellationDomainOriginStateCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineTessellationDomainOriginStateCreateInfo
           #{size VkPipelineTessellationDomainOriginStateCreateInfo}
           #{alignment VkPipelineTessellationDomainOriginStateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "domainOrigin" VkTessellationDomainOrigin 'False
                #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineTessellationStateCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPipelineTessellationDomainOriginStateCreateInfo`
type VkPipelineTessellationDomainOriginStateCreateInfoKHR =
     VkPipelineTessellationDomainOriginStateCreateInfo

-- | > typedef struct VkPipelineTessellationStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineTessellationStateCreateFlags    flags;
--   >     uint32_t               patchControlPoints;
--   > } VkPipelineTessellationStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineTessellationStateCreateInfo VkPipelineTessellationStateCreateInfo registry at www.khronos.org>
type VkPipelineTessellationStateCreateInfo =
     VkStruct VkPipelineTessellationStateCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineTessellationStateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineTessellationStateCreateInfo where
    type StructRep VkPipelineTessellationStateCreateInfo =
         'StructMeta "VkPipelineTessellationStateCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineTessellationStateCreateInfo
           #{size VkPipelineTessellationStateCreateInfo}
           #{alignment VkPipelineTessellationStateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineTessellationStateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineTessellationStateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineTessellationStateCreateFlags 'True
                #{offset VkPipelineTessellationStateCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "patchControlPoints" Word32 'False
                                                            #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineVertexInputDivisorStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     uint32_t                            vertexBindingDivisorCount;
--   >     const VkVertexInputBindingDivisorDescriptionEXT*      pVertexBindingDivisors;
--   > } VkPipelineVertexInputDivisorStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineVertexInputDivisorStateCreateInfoEXT VkPipelineVertexInputDivisorStateCreateInfoEXT registry at www.khronos.org>
type VkPipelineVertexInputDivisorStateCreateInfoEXT =
     VkStruct VkPipelineVertexInputDivisorStateCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPipelineVertexInputDivisorStateCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
    type StructRep VkPipelineVertexInputDivisorStateCreateInfoEXT =
         'StructMeta "VkPipelineVertexInputDivisorStateCreateInfoEXT" -- ' closing tick for hsc2hs
           VkPipelineVertexInputDivisorStateCreateInfoEXT
           #{size VkPipelineVertexInputDivisorStateCreateInfoEXT}
           #{alignment VkPipelineVertexInputDivisorStateCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexBindingDivisorCount" Word32 'False
                                                                   #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pVertexBindingDivisors" -- ' closing tick for hsc2hs
                (Ptr VkVertexInputBindingDivisorDescriptionEXT)
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineVertexInputStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineVertexInputStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineVertexInputStateCreateFlags    flags;
--   >     uint32_t               vertexBindingDescriptionCount;
--   >     const VkVertexInputBindingDescription* pVertexBindingDescriptions;
--   >     uint32_t               vertexAttributeDescriptionCount;
--   >     const VkVertexInputAttributeDescription* pVertexAttributeDescriptions;
--   > } VkPipelineVertexInputStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineVertexInputStateCreateInfo VkPipelineVertexInputStateCreateInfo registry at www.khronos.org>
type VkPipelineVertexInputStateCreateInfo =
     VkStruct VkPipelineVertexInputStateCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineVertexInputStateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineVertexInputStateCreateInfo where
    type StructRep VkPipelineVertexInputStateCreateInfo =
         'StructMeta "VkPipelineVertexInputStateCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineVertexInputStateCreateInfo
           #{size VkPipelineVertexInputStateCreateInfo}
           #{alignment VkPipelineVertexInputStateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineVertexInputStateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineVertexInputStateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineVertexInputStateCreateFlags 'True
                #{offset VkPipelineVertexInputStateCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexBindingDescriptionCount" Word32 'True
                                                                      #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pVertexBindingDescriptions" -- ' closing tick for hsc2hs
                (Ptr VkVertexInputBindingDescription)
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexAttributeDescriptionCount" Word32 'True
                #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pVertexAttributeDescriptions" -- ' closing tick for hsc2hs
                (Ptr VkVertexInputAttributeDescription)
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineViewportCoarseSampleOrderStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                            pNext;
--   >     VkCoarseSampleOrderTypeNV                                              sampleOrderType;
--   >     uint32_t                                               customSampleOrderCount;
--   >     const VkCoarseSampleOrderCustomNV*        pCustomSampleOrders;
--   > } VkPipelineViewportCoarseSampleOrderStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineViewportCoarseSampleOrderStateCreateInfoNV VkPipelineViewportCoarseSampleOrderStateCreateInfoNV registry at www.khronos.org>
type VkPipelineViewportCoarseSampleOrderStateCreateInfoNV =
     VkStruct VkPipelineViewportCoarseSampleOrderStateCreateInfoNV' -- ' closing tick for hsc2hs

data VkPipelineViewportCoarseSampleOrderStateCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineViewportCoarseSampleOrderStateCreateInfoNV
         where
    type StructRep VkPipelineViewportCoarseSampleOrderStateCreateInfoNV
         =
         'StructMeta "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV" -- ' closing tick for hsc2hs
           VkPipelineViewportCoarseSampleOrderStateCreateInfoNV
           #{size VkPipelineViewportCoarseSampleOrderStateCreateInfoNV}
           #{alignment VkPipelineViewportCoarseSampleOrderStateCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineViewportCoarseSampleOrderStateCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineViewportCoarseSampleOrderStateCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleOrderType" VkCoarseSampleOrderTypeNV 'False
                #{offset VkPipelineViewportCoarseSampleOrderStateCreateInfoNV, sampleOrderType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "customSampleOrderCount" Word32 'True
                                                               #{offset VkPipelineViewportCoarseSampleOrderStateCreateInfoNV, customSampleOrderCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pCustomSampleOrders" (Ptr VkCoarseSampleOrderCustomNV) -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineViewportCoarseSampleOrderStateCreateInfoNV, pCustomSampleOrders}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineViewportStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineViewportExclusiveScissorStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                       pNext;
--   >     uint32_t                                          exclusiveScissorCount;
--   >     const VkRect2D* pExclusiveScissors;
--   > } VkPipelineViewportExclusiveScissorStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineViewportExclusiveScissorStateCreateInfoNV VkPipelineViewportExclusiveScissorStateCreateInfoNV registry at www.khronos.org>
type VkPipelineViewportExclusiveScissorStateCreateInfoNV =
     VkStruct VkPipelineViewportExclusiveScissorStateCreateInfoNV' -- ' closing tick for hsc2hs

data VkPipelineViewportExclusiveScissorStateCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineViewportExclusiveScissorStateCreateInfoNV
         where
    type StructRep VkPipelineViewportExclusiveScissorStateCreateInfoNV
         =
         'StructMeta "VkPipelineViewportExclusiveScissorStateCreateInfoNV" -- ' closing tick for hsc2hs
           VkPipelineViewportExclusiveScissorStateCreateInfoNV
           #{size VkPipelineViewportExclusiveScissorStateCreateInfoNV}
           #{alignment VkPipelineViewportExclusiveScissorStateCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineViewportExclusiveScissorStateCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineViewportExclusiveScissorStateCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "exclusiveScissorCount" Word32 'True
                                                              #{offset VkPipelineViewportExclusiveScissorStateCreateInfoNV, exclusiveScissorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pExclusiveScissors" (Ptr VkRect2D) 'False
                                                                    #{offset VkPipelineViewportExclusiveScissorStateCreateInfoNV, pExclusiveScissors}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineViewportStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineViewportShadingRateImageStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                             pNext;
--   >     VkBool32                                                                shadingRateImageEnable;
--   >     uint32_t                                                viewportCount;
--   >     const VkShadingRatePaletteNV* pShadingRatePalettes;
--   > } VkPipelineViewportShadingRateImageStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineViewportShadingRateImageStateCreateInfoNV VkPipelineViewportShadingRateImageStateCreateInfoNV registry at www.khronos.org>
type VkPipelineViewportShadingRateImageStateCreateInfoNV =
     VkStruct VkPipelineViewportShadingRateImageStateCreateInfoNV' -- ' closing tick for hsc2hs

data VkPipelineViewportShadingRateImageStateCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPipelineViewportShadingRateImageStateCreateInfoNV
         where
    type StructRep VkPipelineViewportShadingRateImageStateCreateInfoNV
         =
         'StructMeta "VkPipelineViewportShadingRateImageStateCreateInfoNV" -- ' closing tick for hsc2hs
           VkPipelineViewportShadingRateImageStateCreateInfoNV
           #{size VkPipelineViewportShadingRateImageStateCreateInfoNV}
           #{alignment VkPipelineViewportShadingRateImageStateCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineViewportShadingRateImageStateCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineViewportShadingRateImageStateCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shadingRateImageEnable" VkBool32 'False
                                                                  #{offset VkPipelineViewportShadingRateImageStateCreateInfoNV, shadingRateImageEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewportCount" Word32 'False
                                                       #{offset VkPipelineViewportShadingRateImageStateCreateInfoNV, viewportCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pShadingRatePalettes" (Ptr VkShadingRatePaletteNV) -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPipelineViewportShadingRateImageStateCreateInfoNV, pShadingRatePalettes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineViewportStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineViewportStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineViewportStateCreateFlags    flags;
--   >     uint32_t               viewportCount;
--   >     const VkViewport*      pViewports;
--   >     uint32_t               scissorCount;
--   >     const VkRect2D*        pScissors;
--   > } VkPipelineViewportStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineViewportStateCreateInfo VkPipelineViewportStateCreateInfo registry at www.khronos.org>
type VkPipelineViewportStateCreateInfo =
     VkStruct VkPipelineViewportStateCreateInfo' -- ' closing tick for hsc2hs

data VkPipelineViewportStateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineViewportStateCreateInfo where
    type StructRep VkPipelineViewportStateCreateInfo =
         'StructMeta "VkPipelineViewportStateCreateInfo" -- ' closing tick for hsc2hs
           VkPipelineViewportStateCreateInfo
           #{size VkPipelineViewportStateCreateInfo}
           #{alignment VkPipelineViewportStateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineViewportStateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineViewportStateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineViewportStateCreateFlags 'True
                #{offset VkPipelineViewportStateCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewportCount" Word32 'True
                                                      #{offset VkPipelineViewportStateCreateInfo, viewportCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pViewports" (Ptr VkViewport) 'True
                                                             #{offset VkPipelineViewportStateCreateInfo, pViewports}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "scissorCount" Word32 'True
                                                     #{offset VkPipelineViewportStateCreateInfo, scissorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pScissors" (Ptr VkRect2D) 'True
                                                          #{offset VkPipelineViewportStateCreateInfo, pScissors}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineViewportSwizzleStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineViewportSwizzleStateCreateFlagsNV    flags;
--   >     uint32_t               viewportCount;
--   >     const VkViewportSwizzleNV*      pViewportSwizzles;
--   > } VkPipelineViewportSwizzleStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineViewportSwizzleStateCreateInfoNV VkPipelineViewportSwizzleStateCreateInfoNV registry at www.khronos.org>
type VkPipelineViewportSwizzleStateCreateInfoNV =
     VkStruct VkPipelineViewportSwizzleStateCreateInfoNV' -- ' closing tick for hsc2hs

data VkPipelineViewportSwizzleStateCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineViewportSwizzleStateCreateInfoNV
         where
    type StructRep VkPipelineViewportSwizzleStateCreateInfoNV =
         'StructMeta "VkPipelineViewportSwizzleStateCreateInfoNV" -- ' closing tick for hsc2hs
           VkPipelineViewportSwizzleStateCreateInfoNV
           #{size VkPipelineViewportSwizzleStateCreateInfoNV}
           #{alignment VkPipelineViewportSwizzleStateCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineViewportSwizzleStateCreateFlagsNV -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewportCount" Word32 'False
                                                       #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pViewportSwizzles" (Ptr VkViewportSwizzleNV) 'False
                #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineViewportStateCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineViewportWScalingStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32               viewportWScalingEnable;
--   >     uint32_t               viewportCount;
--   >     const VkViewportWScalingNV*      pViewportWScalings;
--   > } VkPipelineViewportWScalingStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineViewportWScalingStateCreateInfoNV VkPipelineViewportWScalingStateCreateInfoNV registry at www.khronos.org>
type VkPipelineViewportWScalingStateCreateInfoNV =
     VkStruct VkPipelineViewportWScalingStateCreateInfoNV' -- ' closing tick for hsc2hs

data VkPipelineViewportWScalingStateCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineViewportWScalingStateCreateInfoNV
         where
    type StructRep VkPipelineViewportWScalingStateCreateInfoNV =
         'StructMeta "VkPipelineViewportWScalingStateCreateInfoNV" -- ' closing tick for hsc2hs
           VkPipelineViewportWScalingStateCreateInfoNV
           #{size VkPipelineViewportWScalingStateCreateInfoNV}
           #{alignment VkPipelineViewportWScalingStateCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewportWScalingEnable" VkBool32 'False
                                                                  #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewportCount" Word32 'False
                                                       #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pViewportWScalings" (Ptr VkViewportWScalingNV) 'True
                #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPipelineViewportStateCreateInfo] -- ' closing tick for hsc2hs
