#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.Pipeline
       (VkGraphicsPipelineCreateInfo(..), VkPipelineCacheCreateInfo(..),
        VkPipelineColorBlendAdvancedStateCreateInfoEXT(..),
        VkPipelineColorBlendAttachmentState(..),
        VkPipelineColorBlendStateCreateInfo(..),
        VkPipelineCoverageModulationStateCreateInfoNV(..),
        VkPipelineCoverageToColorStateCreateInfoNV(..),
        VkPipelineDepthStencilStateCreateInfo(..),
        VkPipelineDiscardRectangleStateCreateInfoEXT(..),
        VkPipelineDynamicStateCreateInfo(..),
        VkPipelineInputAssemblyStateCreateInfo(..),
        VkPipelineLayoutCreateInfo(..),
        VkPipelineMultisampleStateCreateInfo(..),
        VkPipelineRasterizationConservativeStateCreateInfoEXT(..),
        VkPipelineRasterizationStateCreateInfo(..),
        VkPipelineRasterizationStateRasterizationOrderAMD(..),
        VkPipelineSampleLocationsStateCreateInfoEXT(..),
        VkPipelineShaderStageCreateInfo(..),
        VkPipelineTessellationDomainOriginStateCreateInfo(..),
        VkPipelineTessellationDomainOriginStateCreateInfoKHR,
        VkPipelineTessellationStateCreateInfo(..),
        VkPipelineVertexInputDivisorStateCreateInfoEXT(..),
        VkPipelineVertexInputStateCreateInfo(..),
        VkPipelineViewportStateCreateInfo(..),
        VkPipelineViewportSwizzleStateCreateInfoNV(..),
        VkPipelineViewportWScalingStateCreateInfoNV(..))
       where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Base                                                    (Addr##,
                                                                              ByteArray##,
                                                                              Proxy##,
                                                                              byteArrayContents##,
                                                                              plusAddr##,
                                                                              proxy##)
import           GHC.TypeLits                                                (KnownNat,
                                                                              natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                             (VkBool32,
                                                                              VkSampleMask)
import           Graphics.Vulkan.Types.Bitmasks                              (VkPipelineCacheCreateFlags,
                                                                              VkPipelineColorBlendStateCreateFlags,
                                                                              VkPipelineCoverageModulationStateCreateFlagsNV,
                                                                              VkPipelineCoverageToColorStateCreateFlagsNV,
                                                                              VkPipelineDepthStencilStateCreateFlags,
                                                                              VkPipelineDiscardRectangleStateCreateFlagsEXT,
                                                                              VkPipelineDynamicStateCreateFlags,
                                                                              VkPipelineInputAssemblyStateCreateFlags,
                                                                              VkPipelineLayoutCreateFlags,
                                                                              VkPipelineMultisampleStateCreateFlags,
                                                                              VkPipelineRasterizationConservativeStateCreateFlagsEXT,
                                                                              VkPipelineRasterizationStateCreateFlags,
                                                                              VkPipelineShaderStageCreateFlags,
                                                                              VkPipelineTessellationStateCreateFlags,
                                                                              VkPipelineVertexInputStateCreateFlags,
                                                                              VkPipelineViewportStateCreateFlags,
                                                                              VkPipelineViewportSwizzleStateCreateFlagsNV)
import           Graphics.Vulkan.Types.Enum.Blend                            (VkBlendFactor,
                                                                              VkBlendOp,
                                                                              VkBlendOverlapEXT)
import           Graphics.Vulkan.Types.Enum.Color                            (VkColorComponentFlags)
import           Graphics.Vulkan.Types.Enum.CompareOp                        (VkCompareOp)
import           Graphics.Vulkan.Types.Enum.ConservativeRasterizationModeEXT (VkConservativeRasterizationModeEXT)
import           Graphics.Vulkan.Types.Enum.CoverageModulationModeNV         (VkCoverageModulationModeNV)
import           Graphics.Vulkan.Types.Enum.CullModeFlags                    (VkCullModeFlags)
import           Graphics.Vulkan.Types.Enum.DiscardRectangleModeEXT          (VkDiscardRectangleModeEXT)
import           Graphics.Vulkan.Types.Enum.DynamicState                     (VkDynamicState)
import           Graphics.Vulkan.Types.Enum.FrontFace                        (VkFrontFace)
import           Graphics.Vulkan.Types.Enum.LogicOp                          (VkLogicOp)
import           Graphics.Vulkan.Types.Enum.Pipeline                         (VkPipelineCreateFlags)
import           Graphics.Vulkan.Types.Enum.PolygonMode                      (VkPolygonMode)
import           Graphics.Vulkan.Types.Enum.PrimitiveTopology                (VkPrimitiveTopology)
import           Graphics.Vulkan.Types.Enum.RasterizationOrderAMD            (VkRasterizationOrderAMD)
import           Graphics.Vulkan.Types.Enum.SampleCountFlags                 (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.Enum.Shader                           (VkShaderStageFlagBits)
import           Graphics.Vulkan.Types.Enum.StructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Enum.TessellationDomainOrigin         (VkTessellationDomainOrigin)
import           Graphics.Vulkan.Types.Handles                               (VkDescriptorSetLayout,
                                                                              VkPipeline,
                                                                              VkPipelineLayout,
                                                                              VkRenderPass,
                                                                              VkShaderModule)
import           Graphics.Vulkan.Types.Struct.PushConstantRange              (VkPushConstantRange)
import           Graphics.Vulkan.Types.Struct.Rect                           (VkRect2D)
import           Graphics.Vulkan.Types.Struct.SampleLocation                 (VkSampleLocationsInfoEXT)
import           Graphics.Vulkan.Types.Struct.Specialization                 (VkSpecializationInfo)
import           Graphics.Vulkan.Types.Struct.StencilOpState                 (VkStencilOpState)
import           Graphics.Vulkan.Types.Struct.VertexInput                    (VkVertexInputAttributeDescription,
                                                                              VkVertexInputBindingDescription,
                                                                              VkVertexInputBindingDivisorDescriptionEXT)
import           Graphics.Vulkan.Types.Struct.Viewport                       (VkViewport,
                                                                              VkViewportSwizzleNV,
                                                                              VkViewportWScalingNV)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkGraphicsPipelineCreateInfo VkGraphicsPipelineCreateInfo registry at www.khronos.org>
data VkGraphicsPipelineCreateInfo = VkGraphicsPipelineCreateInfo## Addr##
                                                                  ByteArray##

instance Eq VkGraphicsPipelineCreateInfo where
        (VkGraphicsPipelineCreateInfo## a _) ==
          x@(VkGraphicsPipelineCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkGraphicsPipelineCreateInfo where
        (VkGraphicsPipelineCreateInfo## a _) `compare`
          x@(VkGraphicsPipelineCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkGraphicsPipelineCreateInfo where
        sizeOf ~_ = #{size VkGraphicsPipelineCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkGraphicsPipelineCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkGraphicsPipelineCreateInfo where
        unsafeAddr (VkGraphicsPipelineCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkGraphicsPipelineCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkGraphicsPipelineCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkGraphicsPipelineCreateInfo where
        type StructFields VkGraphicsPipelineCreateInfo =
             '["sType", "pNext", "flags", "stageCount", "pStages", -- ' closing tick for hsc2hs
               "pVertexInputState", "pInputAssemblyState", "pTessellationState",
               "pViewportState", "pRasterizationState", "pMultisampleState",
               "pDepthStencilState", "pColorBlendState", "pDynamicState",
               "layout", "renderPass", "subpass", "basePipelineHandle",
               "basePipelineIndex"]
        type CUnionType VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkGraphicsPipelineCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkGraphicsPipelineCreateInfo where
        type FieldType "sType" VkGraphicsPipelineCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, sType}
        type FieldIsArray "sType" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkGraphicsPipelineCreateInfo where
        type FieldType "pNext" VkGraphicsPipelineCreateInfo = Ptr Void
        type FieldOptional "pNext" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, pNext}
        type FieldIsArray "pNext" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkGraphicsPipelineCreateInfo where
        type FieldType "flags" VkGraphicsPipelineCreateInfo =
             VkPipelineCreateFlags
        type FieldOptional "flags" VkGraphicsPipelineCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, flags}
        type FieldIsArray "flags" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "stageCount" VkGraphicsPipelineCreateInfo where
        type FieldType "stageCount" VkGraphicsPipelineCreateInfo = Word32
        type FieldOptional "stageCount" VkGraphicsPipelineCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stageCount" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, stageCount}
        type FieldIsArray "stageCount" VkGraphicsPipelineCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, stageCount}

instance {-# OVERLAPPING #-}
         CanReadField "stageCount" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, stageCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, stageCount}

instance {-# OVERLAPPING #-}
         CanWriteField "stageCount" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, stageCount}

instance {-# OVERLAPPING #-}
         HasField "pStages" VkGraphicsPipelineCreateInfo where
        type FieldType "pStages" VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineShaderStageCreateInfo
        type FieldOptional "pStages" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pStages" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, pStages}
        type FieldIsArray "pStages" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pStages}

instance {-# OVERLAPPING #-}
         CanReadField "pStages" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pStages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pStages}

instance {-# OVERLAPPING #-}
         CanWriteField "pStages" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pStages}

instance {-# OVERLAPPING #-}
         HasField "pVertexInputState" VkGraphicsPipelineCreateInfo where
        type FieldType "pVertexInputState" VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineVertexInputStateCreateInfo
        type FieldOptional "pVertexInputState" VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pVertexInputState" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, pVertexInputState}
        type FieldIsArray "pVertexInputState" VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pVertexInputState}

instance {-# OVERLAPPING #-}
         CanReadField "pVertexInputState" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pVertexInputState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pVertexInputState}

instance {-# OVERLAPPING #-}
         CanWriteField "pVertexInputState" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pVertexInputState}

instance {-# OVERLAPPING #-}
         HasField "pInputAssemblyState" VkGraphicsPipelineCreateInfo where
        type FieldType "pInputAssemblyState" VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineInputAssemblyStateCreateInfo
        type FieldOptional "pInputAssemblyState"
               VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pInputAssemblyState" VkGraphicsPipelineCreateInfo
             =
             #{offset VkGraphicsPipelineCreateInfo, pInputAssemblyState}
        type FieldIsArray "pInputAssemblyState"
               VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pInputAssemblyState}

instance {-# OVERLAPPING #-}
         CanReadField "pInputAssemblyState" VkGraphicsPipelineCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pInputAssemblyState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pInputAssemblyState}

instance {-# OVERLAPPING #-}
         CanWriteField "pInputAssemblyState" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pInputAssemblyState}

instance {-# OVERLAPPING #-}
         HasField "pTessellationState" VkGraphicsPipelineCreateInfo where
        type FieldType "pTessellationState" VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineTessellationStateCreateInfo
        type FieldOptional "pTessellationState"
               VkGraphicsPipelineCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pTessellationState" VkGraphicsPipelineCreateInfo
             =
             #{offset VkGraphicsPipelineCreateInfo, pTessellationState}
        type FieldIsArray "pTessellationState" VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pTessellationState}

instance {-# OVERLAPPING #-}
         CanReadField "pTessellationState" VkGraphicsPipelineCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pTessellationState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pTessellationState}

instance {-# OVERLAPPING #-}
         CanWriteField "pTessellationState" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pTessellationState}

instance {-# OVERLAPPING #-}
         HasField "pViewportState" VkGraphicsPipelineCreateInfo where
        type FieldType "pViewportState" VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineViewportStateCreateInfo
        type FieldOptional "pViewportState" VkGraphicsPipelineCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pViewportState" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, pViewportState}
        type FieldIsArray "pViewportState" VkGraphicsPipelineCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pViewportState}

instance {-# OVERLAPPING #-}
         CanReadField "pViewportState" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pViewportState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pViewportState}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewportState" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pViewportState}

instance {-# OVERLAPPING #-}
         HasField "pRasterizationState" VkGraphicsPipelineCreateInfo where
        type FieldType "pRasterizationState" VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineRasterizationStateCreateInfo
        type FieldOptional "pRasterizationState"
               VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pRasterizationState" VkGraphicsPipelineCreateInfo
             =
             #{offset VkGraphicsPipelineCreateInfo, pRasterizationState}
        type FieldIsArray "pRasterizationState"
               VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pRasterizationState}

instance {-# OVERLAPPING #-}
         CanReadField "pRasterizationState" VkGraphicsPipelineCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pRasterizationState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pRasterizationState}

instance {-# OVERLAPPING #-}
         CanWriteField "pRasterizationState" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pRasterizationState}

instance {-# OVERLAPPING #-}
         HasField "pMultisampleState" VkGraphicsPipelineCreateInfo where
        type FieldType "pMultisampleState" VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineMultisampleStateCreateInfo
        type FieldOptional "pMultisampleState" VkGraphicsPipelineCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pMultisampleState" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, pMultisampleState}
        type FieldIsArray "pMultisampleState" VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pMultisampleState}

instance {-# OVERLAPPING #-}
         CanReadField "pMultisampleState" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pMultisampleState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pMultisampleState}

instance {-# OVERLAPPING #-}
         CanWriteField "pMultisampleState" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pMultisampleState}

instance {-# OVERLAPPING #-}
         HasField "pDepthStencilState" VkGraphicsPipelineCreateInfo where
        type FieldType "pDepthStencilState" VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineDepthStencilStateCreateInfo
        type FieldOptional "pDepthStencilState"
               VkGraphicsPipelineCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pDepthStencilState" VkGraphicsPipelineCreateInfo
             =
             #{offset VkGraphicsPipelineCreateInfo, pDepthStencilState}
        type FieldIsArray "pDepthStencilState" VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pDepthStencilState}

instance {-# OVERLAPPING #-}
         CanReadField "pDepthStencilState" VkGraphicsPipelineCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pDepthStencilState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pDepthStencilState}

instance {-# OVERLAPPING #-}
         CanWriteField "pDepthStencilState" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pDepthStencilState}

instance {-# OVERLAPPING #-}
         HasField "pColorBlendState" VkGraphicsPipelineCreateInfo where
        type FieldType "pColorBlendState" VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineColorBlendStateCreateInfo
        type FieldOptional "pColorBlendState" VkGraphicsPipelineCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pColorBlendState" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, pColorBlendState}
        type FieldIsArray "pColorBlendState" VkGraphicsPipelineCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pColorBlendState}

instance {-# OVERLAPPING #-}
         CanReadField "pColorBlendState" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pColorBlendState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pColorBlendState}

instance {-# OVERLAPPING #-}
         CanWriteField "pColorBlendState" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pColorBlendState}

instance {-# OVERLAPPING #-}
         HasField "pDynamicState" VkGraphicsPipelineCreateInfo where
        type FieldType "pDynamicState" VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineDynamicStateCreateInfo
        type FieldOptional "pDynamicState" VkGraphicsPipelineCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pDynamicState" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, pDynamicState}
        type FieldIsArray "pDynamicState" VkGraphicsPipelineCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, pDynamicState}

instance {-# OVERLAPPING #-}
         CanReadField "pDynamicState" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pDynamicState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pDynamicState}

instance {-# OVERLAPPING #-}
         CanWriteField "pDynamicState" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pDynamicState}

instance {-# OVERLAPPING #-}
         HasField "layout" VkGraphicsPipelineCreateInfo where
        type FieldType "layout" VkGraphicsPipelineCreateInfo =
             VkPipelineLayout
        type FieldOptional "layout" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layout" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, layout}
        type FieldIsArray "layout" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, layout}

instance {-# OVERLAPPING #-}
         CanReadField "layout" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, layout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, layout}

instance {-# OVERLAPPING #-}
         CanWriteField "layout" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, layout}

instance {-# OVERLAPPING #-}
         HasField "renderPass" VkGraphicsPipelineCreateInfo where
        type FieldType "renderPass" VkGraphicsPipelineCreateInfo =
             VkRenderPass
        type FieldOptional "renderPass" VkGraphicsPipelineCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "renderPass" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, renderPass}
        type FieldIsArray "renderPass" VkGraphicsPipelineCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, renderPass}

instance {-# OVERLAPPING #-}
         CanReadField "renderPass" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, renderPass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, renderPass}

instance {-# OVERLAPPING #-}
         CanWriteField "renderPass" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, renderPass}

instance {-# OVERLAPPING #-}
         HasField "subpass" VkGraphicsPipelineCreateInfo where
        type FieldType "subpass" VkGraphicsPipelineCreateInfo = Word32
        type FieldOptional "subpass" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subpass" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, subpass}
        type FieldIsArray "subpass" VkGraphicsPipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, subpass}

instance {-# OVERLAPPING #-}
         CanReadField "subpass" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, subpass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, subpass}

instance {-# OVERLAPPING #-}
         CanWriteField "subpass" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, subpass}

instance {-# OVERLAPPING #-}
         HasField "basePipelineHandle" VkGraphicsPipelineCreateInfo where
        type FieldType "basePipelineHandle" VkGraphicsPipelineCreateInfo =
             VkPipeline
        type FieldOptional "basePipelineHandle"
               VkGraphicsPipelineCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "basePipelineHandle" VkGraphicsPipelineCreateInfo
             =
             #{offset VkGraphicsPipelineCreateInfo, basePipelineHandle}
        type FieldIsArray "basePipelineHandle" VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, basePipelineHandle}

instance {-# OVERLAPPING #-}
         CanReadField "basePipelineHandle" VkGraphicsPipelineCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, basePipelineHandle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, basePipelineHandle}

instance {-# OVERLAPPING #-}
         CanWriteField "basePipelineHandle" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, basePipelineHandle}

instance {-# OVERLAPPING #-}
         HasField "basePipelineIndex" VkGraphicsPipelineCreateInfo where
        type FieldType "basePipelineIndex" VkGraphicsPipelineCreateInfo =
             Int32
        type FieldOptional "basePipelineIndex" VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "basePipelineIndex" VkGraphicsPipelineCreateInfo =
             #{offset VkGraphicsPipelineCreateInfo, basePipelineIndex}
        type FieldIsArray "basePipelineIndex" VkGraphicsPipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkGraphicsPipelineCreateInfo, basePipelineIndex}

instance {-# OVERLAPPING #-}
         CanReadField "basePipelineIndex" VkGraphicsPipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, basePipelineIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, basePipelineIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "basePipelineIndex" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, basePipelineIndex}

instance Show VkGraphicsPipelineCreateInfo where
        showsPrec d x
          = showString "VkGraphicsPipelineCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "stageCount = " .
                                  showsPrec d (getField @"stageCount" x) .
                                    showString ", " .
                                      showString "pStages = " .
                                        showsPrec d (getField @"pStages" x) .
                                          showString ", " .
                                            showString "pVertexInputState = " .
                                              showsPrec d (getField @"pVertexInputState" x) .
                                                showString ", " .
                                                  showString "pInputAssemblyState = " .
                                                    showsPrec d (getField @"pInputAssemblyState" x)
                                                      .
                                                      showString ", " .
                                                        showString "pTessellationState = " .
                                                          showsPrec d
                                                            (getField @"pTessellationState" x)
                                                            .
                                                            showString ", " .
                                                              showString "pViewportState = " .
                                                                showsPrec d
                                                                  (getField @"pViewportState" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "pRasterizationState = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"pRasterizationState"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "pMultisampleState = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"pMultisampleState"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "pDepthStencilState = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"pDepthStencilState"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "pColorBlendState = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"pColorBlendState"
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "pDynamicState = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (getField
                                                                                                   @"pDynamicState"
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "layout = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (getField
                                                                                                         @"layout"
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "renderPass = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (getField
                                                                                                               @"renderPass"
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "subpass = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (getField
                                                                                                                     @"subpass"
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "basePipelineHandle = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (getField
                                                                                                                           @"basePipelineHandle"
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showString
                                                                                                                          ", "
                                                                                                                          .
                                                                                                                          showString
                                                                                                                            "basePipelineIndex = "
                                                                                                                            .
                                                                                                                            showsPrec
                                                                                                                              d
                                                                                                                              (getField
                                                                                                                                 @"basePipelineIndex"
                                                                                                                                 x)
                                                                                                                              .
                                                                                                                              showChar
                                                                                                                                '}'

-- | > typedef struct VkPipelineCacheCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineCacheCreateFlags    flags;
--   >     size_t                 initialDataSize;
--   >     const void*            pInitialData;
--   > } VkPipelineCacheCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineCacheCreateInfo VkPipelineCacheCreateInfo registry at www.khronos.org>
data VkPipelineCacheCreateInfo = VkPipelineCacheCreateInfo## Addr##
                                                            ByteArray##

instance Eq VkPipelineCacheCreateInfo where
        (VkPipelineCacheCreateInfo## a _) ==
          x@(VkPipelineCacheCreateInfo## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineCacheCreateInfo where
        (VkPipelineCacheCreateInfo## a _) `compare`
          x@(VkPipelineCacheCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineCacheCreateInfo where
        sizeOf ~_ = #{size VkPipelineCacheCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPipelineCacheCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineCacheCreateInfo where
        unsafeAddr (VkPipelineCacheCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineCacheCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineCacheCreateInfo## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineCacheCreateInfo where
        type StructFields VkPipelineCacheCreateInfo =
             '["sType", "pNext", "flags", "initialDataSize", "pInitialData"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineCacheCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineCacheCreateInfo where
        type FieldType "sType" VkPipelineCacheCreateInfo = VkStructureType
        type FieldOptional "sType" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineCacheCreateInfo =
             #{offset VkPipelineCacheCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCacheCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineCacheCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineCacheCreateInfo where
        type FieldType "pNext" VkPipelineCacheCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineCacheCreateInfo =
             #{offset VkPipelineCacheCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCacheCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineCacheCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineCacheCreateInfo where
        type FieldType "flags" VkPipelineCacheCreateInfo =
             VkPipelineCacheCreateFlags
        type FieldOptional "flags" VkPipelineCacheCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineCacheCreateInfo =
             #{offset VkPipelineCacheCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCacheCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineCacheCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "initialDataSize" VkPipelineCacheCreateInfo where
        type FieldType "initialDataSize" VkPipelineCacheCreateInfo = CSize
        type FieldOptional "initialDataSize" VkPipelineCacheCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "initialDataSize" VkPipelineCacheCreateInfo =
             #{offset VkPipelineCacheCreateInfo, initialDataSize}
        type FieldIsArray "initialDataSize" VkPipelineCacheCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCacheCreateInfo, initialDataSize}

instance {-# OVERLAPPING #-}
         CanReadField "initialDataSize" VkPipelineCacheCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, initialDataSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, initialDataSize}

instance {-# OVERLAPPING #-}
         CanWriteField "initialDataSize" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, initialDataSize}

instance {-# OVERLAPPING #-}
         HasField "pInitialData" VkPipelineCacheCreateInfo where
        type FieldType "pInitialData" VkPipelineCacheCreateInfo = Ptr Void
        type FieldOptional "pInitialData" VkPipelineCacheCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pInitialData" VkPipelineCacheCreateInfo =
             #{offset VkPipelineCacheCreateInfo, pInitialData}
        type FieldIsArray "pInitialData" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCacheCreateInfo, pInitialData}

instance {-# OVERLAPPING #-}
         CanReadField "pInitialData" VkPipelineCacheCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, pInitialData})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, pInitialData}

instance {-# OVERLAPPING #-}
         CanWriteField "pInitialData" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, pInitialData}

instance Show VkPipelineCacheCreateInfo where
        showsPrec d x
          = showString "VkPipelineCacheCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "initialDataSize = " .
                                  showsPrec d (getField @"initialDataSize" x) .
                                    showString ", " .
                                      showString "pInitialData = " .
                                        showsPrec d (getField @"pInitialData" x) . showChar '}'

-- | > typedef struct VkPipelineColorBlendAdvancedStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBool32               srcPremultiplied;
--   >     VkBool32               dstPremultiplied;
--   >     VkBlendOverlapEXT      blendOverlap;
--   > } VkPipelineColorBlendAdvancedStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineColorBlendAdvancedStateCreateInfoEXT VkPipelineColorBlendAdvancedStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineColorBlendAdvancedStateCreateInfoEXT = VkPipelineColorBlendAdvancedStateCreateInfoEXT## Addr##
                                                                                                      ByteArray##

instance Eq VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a _) ==
          x@(VkPipelineColorBlendAdvancedStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a _) `compare`
          x@(VkPipelineColorBlendAdvancedStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineColorBlendAdvancedStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineColorBlendAdvancedStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        unsafeAddr (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineColorBlendAdvancedStateCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineColorBlendAdvancedStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type StructFields VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             '["sType", "pNext", "srcPremultiplied", "dstPremultiplied", -- ' closing tick for hsc2hs
               "blendOverlap"]
        type CUnionType VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             '[VkPipelineColorBlendStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBool32
        type FieldOptional "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}
        type FieldIsArray "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

instance {-# OVERLAPPING #-}
         CanReadField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

instance {-# OVERLAPPING #-}
         CanWriteField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

instance {-# OVERLAPPING #-}
         HasField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBool32
        type FieldOptional "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}
        type FieldIsArray "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

instance {-# OVERLAPPING #-}
         CanReadField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

instance {-# OVERLAPPING #-}
         CanWriteField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

instance {-# OVERLAPPING #-}
         HasField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBlendOverlapEXT
        type FieldOptional "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}
        type FieldIsArray "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

instance {-# OVERLAPPING #-}
         CanReadField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

instance {-# OVERLAPPING #-}
         CanWriteField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

instance Show VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineColorBlendAdvancedStateCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "srcPremultiplied = " .
                            showsPrec d (getField @"srcPremultiplied" x) .
                              showString ", " .
                                showString "dstPremultiplied = " .
                                  showsPrec d (getField @"dstPremultiplied" x) .
                                    showString ", " .
                                      showString "blendOverlap = " .
                                        showsPrec d (getField @"blendOverlap" x) . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineColorBlendAttachmentState VkPipelineColorBlendAttachmentState registry at www.khronos.org>
data VkPipelineColorBlendAttachmentState = VkPipelineColorBlendAttachmentState## Addr##
                                                                                ByteArray##

instance Eq VkPipelineColorBlendAttachmentState where
        (VkPipelineColorBlendAttachmentState## a _) ==
          x@(VkPipelineColorBlendAttachmentState## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineColorBlendAttachmentState where
        (VkPipelineColorBlendAttachmentState## a _) `compare`
          x@(VkPipelineColorBlendAttachmentState## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineColorBlendAttachmentState where
        sizeOf ~_ = #{size VkPipelineColorBlendAttachmentState}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineColorBlendAttachmentState}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineColorBlendAttachmentState
         where
        unsafeAddr (VkPipelineColorBlendAttachmentState## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineColorBlendAttachmentState## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineColorBlendAttachmentState##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineColorBlendAttachmentState where
        type StructFields VkPipelineColorBlendAttachmentState =
             '["blendEnable", "srcColorBlendFactor", "dstColorBlendFactor", -- ' closing tick for hsc2hs
               "colorBlendOp", "srcAlphaBlendFactor", "dstAlphaBlendFactor",
               "alphaBlendOp", "colorWriteMask"]
        type CUnionType VkPipelineColorBlendAttachmentState = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineColorBlendAttachmentState = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineColorBlendAttachmentState = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "blendEnable" VkPipelineColorBlendAttachmentState where
        type FieldType "blendEnable" VkPipelineColorBlendAttachmentState =
             VkBool32
        type FieldOptional "blendEnable"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "blendEnable" VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, blendEnable}
        type FieldIsArray "blendEnable" VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, blendEnable}

instance {-# OVERLAPPING #-}
         CanReadField "blendEnable" VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, blendEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, blendEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "blendEnable" VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, blendEnable}

instance {-# OVERLAPPING #-}
         HasField "srcColorBlendFactor" VkPipelineColorBlendAttachmentState
         where
        type FieldType "srcColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = VkBlendFactor
        type FieldOptional "srcColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}
        type FieldIsArray "srcColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}

instance {-# OVERLAPPING #-}
         CanReadField "srcColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "srcColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}

instance {-# OVERLAPPING #-}
         HasField "dstColorBlendFactor" VkPipelineColorBlendAttachmentState
         where
        type FieldType "dstColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = VkBlendFactor
        type FieldOptional "dstColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}
        type FieldIsArray "dstColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}

instance {-# OVERLAPPING #-}
         CanReadField "dstColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "dstColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}

instance {-# OVERLAPPING #-}
         HasField "colorBlendOp" VkPipelineColorBlendAttachmentState where
        type FieldType "colorBlendOp" VkPipelineColorBlendAttachmentState =
             VkBlendOp
        type FieldOptional "colorBlendOp"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "colorBlendOp" VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}
        type FieldIsArray "colorBlendOp"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}

instance {-# OVERLAPPING #-}
         CanReadField "colorBlendOp" VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, colorBlendOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}

instance {-# OVERLAPPING #-}
         CanWriteField "colorBlendOp" VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}

instance {-# OVERLAPPING #-}
         HasField "srcAlphaBlendFactor" VkPipelineColorBlendAttachmentState
         where
        type FieldType "srcAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = VkBlendFactor
        type FieldOptional "srcAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}
        type FieldIsArray "srcAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}

instance {-# OVERLAPPING #-}
         CanReadField "srcAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "srcAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}

instance {-# OVERLAPPING #-}
         HasField "dstAlphaBlendFactor" VkPipelineColorBlendAttachmentState
         where
        type FieldType "dstAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = VkBlendFactor
        type FieldOptional "dstAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}
        type FieldIsArray "dstAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}

instance {-# OVERLAPPING #-}
         CanReadField "dstAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "dstAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}

instance {-# OVERLAPPING #-}
         HasField "alphaBlendOp" VkPipelineColorBlendAttachmentState where
        type FieldType "alphaBlendOp" VkPipelineColorBlendAttachmentState =
             VkBlendOp
        type FieldOptional "alphaBlendOp"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "alphaBlendOp" VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}
        type FieldIsArray "alphaBlendOp"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}

instance {-# OVERLAPPING #-}
         CanReadField "alphaBlendOp" VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}

instance {-# OVERLAPPING #-}
         CanWriteField "alphaBlendOp" VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}

instance {-# OVERLAPPING #-}
         HasField "colorWriteMask" VkPipelineColorBlendAttachmentState where
        type FieldType "colorWriteMask" VkPipelineColorBlendAttachmentState
             = VkColorComponentFlags
        type FieldOptional "colorWriteMask"
               VkPipelineColorBlendAttachmentState
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "colorWriteMask"
               VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}
        type FieldIsArray "colorWriteMask"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}

instance {-# OVERLAPPING #-}
         CanReadField "colorWriteMask" VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, colorWriteMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}

instance {-# OVERLAPPING #-}
         CanWriteField "colorWriteMask" VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}

instance Show VkPipelineColorBlendAttachmentState where
        showsPrec d x
          = showString "VkPipelineColorBlendAttachmentState {" .
              showString "blendEnable = " .
                showsPrec d (getField @"blendEnable" x) .
                  showString ", " .
                    showString "srcColorBlendFactor = " .
                      showsPrec d (getField @"srcColorBlendFactor" x) .
                        showString ", " .
                          showString "dstColorBlendFactor = " .
                            showsPrec d (getField @"dstColorBlendFactor" x) .
                              showString ", " .
                                showString "colorBlendOp = " .
                                  showsPrec d (getField @"colorBlendOp" x) .
                                    showString ", " .
                                      showString "srcAlphaBlendFactor = " .
                                        showsPrec d (getField @"srcAlphaBlendFactor" x) .
                                          showString ", " .
                                            showString "dstAlphaBlendFactor = " .
                                              showsPrec d (getField @"dstAlphaBlendFactor" x) .
                                                showString ", " .
                                                  showString "alphaBlendOp = " .
                                                    showsPrec d (getField @"alphaBlendOp" x) .
                                                      showString ", " .
                                                        showString "colorWriteMask = " .
                                                          showsPrec d (getField @"colorWriteMask" x)
                                                            . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineColorBlendStateCreateInfo VkPipelineColorBlendStateCreateInfo registry at www.khronos.org>
data VkPipelineColorBlendStateCreateInfo = VkPipelineColorBlendStateCreateInfo## Addr##
                                                                                ByteArray##

instance Eq VkPipelineColorBlendStateCreateInfo where
        (VkPipelineColorBlendStateCreateInfo## a _) ==
          x@(VkPipelineColorBlendStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineColorBlendStateCreateInfo where
        (VkPipelineColorBlendStateCreateInfo## a _) `compare`
          x@(VkPipelineColorBlendStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineColorBlendStateCreateInfo where
        sizeOf ~_ = #{size VkPipelineColorBlendStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineColorBlendStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineColorBlendStateCreateInfo
         where
        unsafeAddr (VkPipelineColorBlendStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineColorBlendStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineColorBlendStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineColorBlendStateCreateInfo where
        type StructFields VkPipelineColorBlendStateCreateInfo =
             '["sType", "pNext", "flags", "logicOpEnable", "logicOp", -- ' closing tick for hsc2hs
               "attachmentCount", "pAttachments", "blendConstants"]
        type CUnionType VkPipelineColorBlendStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineColorBlendStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineColorBlendStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineColorBlendStateCreateInfo where
        type FieldType "sType" VkPipelineColorBlendStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineColorBlendStateCreateInfo =
             #{offset VkPipelineColorBlendStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineColorBlendStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineColorBlendStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineColorBlendStateCreateInfo where
        type FieldType "pNext" VkPipelineColorBlendStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineColorBlendStateCreateInfo =
             #{offset VkPipelineColorBlendStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineColorBlendStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineColorBlendStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineColorBlendStateCreateInfo where
        type FieldType "flags" VkPipelineColorBlendStateCreateInfo =
             VkPipelineColorBlendStateCreateFlags
        type FieldOptional "flags" VkPipelineColorBlendStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineColorBlendStateCreateInfo =
             #{offset VkPipelineColorBlendStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineColorBlendStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineColorBlendStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "logicOpEnable" VkPipelineColorBlendStateCreateInfo where
        type FieldType "logicOpEnable" VkPipelineColorBlendStateCreateInfo
             = VkBool32
        type FieldOptional "logicOpEnable"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "logicOpEnable"
               VkPipelineColorBlendStateCreateInfo
             =
             #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}
        type FieldIsArray "logicOpEnable"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}

instance {-# OVERLAPPING #-}
         CanReadField "logicOpEnable" VkPipelineColorBlendStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "logicOpEnable" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}

instance {-# OVERLAPPING #-}
         HasField "logicOp" VkPipelineColorBlendStateCreateInfo where
        type FieldType "logicOp" VkPipelineColorBlendStateCreateInfo =
             VkLogicOp
        type FieldOptional "logicOp" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "logicOp" VkPipelineColorBlendStateCreateInfo =
             #{offset VkPipelineColorBlendStateCreateInfo, logicOp}
        type FieldIsArray "logicOp" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, logicOp}

instance {-# OVERLAPPING #-}
         CanReadField "logicOp" VkPipelineColorBlendStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, logicOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOp}

instance {-# OVERLAPPING #-}
         CanWriteField "logicOp" VkPipelineColorBlendStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOp}

instance {-# OVERLAPPING #-}
         HasField "attachmentCount" VkPipelineColorBlendStateCreateInfo
         where
        type FieldType "attachmentCount"
               VkPipelineColorBlendStateCreateInfo
             = Word32
        type FieldOptional "attachmentCount"
               VkPipelineColorBlendStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "attachmentCount"
               VkPipelineColorBlendStateCreateInfo
             =
             #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}
        type FieldIsArray "attachmentCount"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         CanReadField "attachmentCount" VkPipelineColorBlendStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         CanWriteField "attachmentCount" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         HasField "pAttachments" VkPipelineColorBlendStateCreateInfo where
        type FieldType "pAttachments" VkPipelineColorBlendStateCreateInfo =
             Ptr VkPipelineColorBlendAttachmentState
        type FieldOptional "pAttachments"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAttachments" VkPipelineColorBlendStateCreateInfo
             =
             #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}
        type FieldIsArray "pAttachments"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         CanReadField "pAttachments" VkPipelineColorBlendStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, pAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttachments" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         HasField "blendConstants" VkPipelineColorBlendStateCreateInfo where
        type FieldType "blendConstants" VkPipelineColorBlendStateCreateInfo
             = #{type float}
        type FieldOptional "blendConstants"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "blendConstants"
               VkPipelineColorBlendStateCreateInfo
             =
             #{offset VkPipelineColorBlendStateCreateInfo, blendConstants}
        type FieldIsArray "blendConstants"
               VkPipelineColorBlendStateCreateInfo
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, blendConstants}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "blendConstants" idx
            VkPipelineColorBlendStateCreateInfo) =>
         CanReadFieldArray "blendConstants" idx
           VkPipelineColorBlendStateCreateInfo
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "blendConstants" 0
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "blendConstants" 1
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "blendConstants" 2
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "blendConstants" 3
                         VkPipelineColorBlendStateCreateInfo
                       #-}
        type FieldArrayLength "blendConstants"
               VkPipelineColorBlendStateCreateInfo
             = 4

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 4

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPipelineColorBlendStateCreateInfo, blendConstants}
                      +
                      sizeOf (undefined :: #{type float}) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPipelineColorBlendStateCreateInfo, blendConstants}
                 +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "blendConstants" idx
            VkPipelineColorBlendStateCreateInfo) =>
         CanWriteFieldArray "blendConstants" idx
           VkPipelineColorBlendStateCreateInfo
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "blendConstants" 0
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "blendConstants" 1
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "blendConstants" 2
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "blendConstants" 3
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPipelineColorBlendStateCreateInfo, blendConstants}
                 +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance Show VkPipelineColorBlendStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineColorBlendStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "logicOpEnable = " .
                                  showsPrec d (getField @"logicOpEnable" x) .
                                    showString ", " .
                                      showString "logicOp = " .
                                        showsPrec d (getField @"logicOp" x) .
                                          showString ", " .
                                            showString "attachmentCount = " .
                                              showsPrec d (getField @"attachmentCount" x) .
                                                showString ", " .
                                                  showString "pAttachments = " .
                                                    showsPrec d (getField @"pAttachments" x) .
                                                      showString ", " .
                                                        (showString "blendConstants = [" .
                                                           showsPrec d
                                                             (let s = sizeOf
                                                                        (undefined ::
                                                                           FieldType
                                                                             "blendConstants"
                                                                             VkPipelineColorBlendStateCreateInfo)
                                                                  o = fieldOffset @"blendConstants"
                                                                        @VkPipelineColorBlendStateCreateInfo
                                                                  f i
                                                                    = peekByteOff (unsafePtr x) i ::
                                                                        IO
                                                                          (FieldType
                                                                             "blendConstants"
                                                                             VkPipelineColorBlendStateCreateInfo)
                                                                in
                                                                unsafeDupablePerformIO . mapM f $
                                                                  map (\ i -> o + i * s)
                                                                    [0 .. 4 - 1])
                                                             . showChar ']')
                                                          . showChar '}'

-- | > typedef struct VkPipelineCoverageModulationStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineCoverageModulationStateCreateFlagsNV                   flags;
--   >     VkCoverageModulationModeNV                                                       coverageModulationMode;
--   >     VkBool32                                                                         coverageModulationTableEnable;
--   >     uint32_t                                                                         coverageModulationTableCount;
--   >     const float* pCoverageModulationTable;
--   > } VkPipelineCoverageModulationStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineCoverageModulationStateCreateInfoNV VkPipelineCoverageModulationStateCreateInfoNV registry at www.khronos.org>
data VkPipelineCoverageModulationStateCreateInfoNV = VkPipelineCoverageModulationStateCreateInfoNV## Addr##
                                                                                                    ByteArray##

instance Eq VkPipelineCoverageModulationStateCreateInfoNV where
        (VkPipelineCoverageModulationStateCreateInfoNV## a _) ==
          x@(VkPipelineCoverageModulationStateCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineCoverageModulationStateCreateInfoNV where
        (VkPipelineCoverageModulationStateCreateInfoNV## a _) `compare`
          x@(VkPipelineCoverageModulationStateCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineCoverageModulationStateCreateInfoNV
         where
        sizeOf ~_
          = #{size VkPipelineCoverageModulationStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineCoverageModulationStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        unsafeAddr (VkPipelineCoverageModulationStateCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineCoverageModulationStateCreateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineCoverageModulationStateCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type StructFields VkPipelineCoverageModulationStateCreateInfoNV =
             '["sType", "pNext", "flags", "coverageModulationMode", -- ' closing tick for hsc2hs
               "coverageModulationTableEnable", "coverageModulationTableCount",
               "pCoverageModulationTable"]
        type CUnionType VkPipelineCoverageModulationStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineCoverageModulationStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineCoverageModulationStateCreateInfoNV =
             '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "sType"
               VkPipelineCoverageModulationStateCreateInfoNV
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}
        type FieldIsArray "sType"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "pNext"
               VkPipelineCoverageModulationStateCreateInfoNV
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}
        type FieldIsArray "pNext"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "flags"
               VkPipelineCoverageModulationStateCreateInfoNV
             = VkPipelineCoverageModulationStateCreateFlagsNV
        type FieldOptional "flags"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}
        type FieldIsArray "flags"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         HasField "coverageModulationMode"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "coverageModulationMode"
               VkPipelineCoverageModulationStateCreateInfoNV
             = VkCoverageModulationModeNV
        type FieldOptional "coverageModulationMode"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "coverageModulationMode"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}
        type FieldIsArray "coverageModulationMode"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}

instance {-# OVERLAPPING #-}
         CanReadField "coverageModulationMode"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}

instance {-# OVERLAPPING #-}
         CanWriteField "coverageModulationMode"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}

instance {-# OVERLAPPING #-}
         HasField "coverageModulationTableEnable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "coverageModulationTableEnable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = VkBool32
        type FieldOptional "coverageModulationTableEnable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "coverageModulationTableEnable"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}
        type FieldIsArray "coverageModulationTableEnable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}

instance {-# OVERLAPPING #-}
         CanReadField "coverageModulationTableEnable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "coverageModulationTableEnable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}

instance {-# OVERLAPPING #-}
         HasField "coverageModulationTableCount"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "coverageModulationTableCount"
               VkPipelineCoverageModulationStateCreateInfoNV
             = Word32
        type FieldOptional "coverageModulationTableCount"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "coverageModulationTableCount"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}
        type FieldIsArray "coverageModulationTableCount"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}

instance {-# OVERLAPPING #-}
         CanReadField "coverageModulationTableCount"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}

instance {-# OVERLAPPING #-}
         CanWriteField "coverageModulationTableCount"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}

instance {-# OVERLAPPING #-}
         HasField "pCoverageModulationTable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "pCoverageModulationTable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = Ptr #{type float}
        type FieldOptional "pCoverageModulationTable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pCoverageModulationTable"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}
        type FieldIsArray "pCoverageModulationTable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}

instance {-# OVERLAPPING #-}
         CanReadField "pCoverageModulationTable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}

instance {-# OVERLAPPING #-}
         CanWriteField "pCoverageModulationTable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}

instance Show VkPipelineCoverageModulationStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineCoverageModulationStateCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "coverageModulationMode = " .
                                  showsPrec d (getField @"coverageModulationMode" x) .
                                    showString ", " .
                                      showString "coverageModulationTableEnable = " .
                                        showsPrec d (getField @"coverageModulationTableEnable" x) .
                                          showString ", " .
                                            showString "coverageModulationTableCount = " .
                                              showsPrec d
                                                (getField @"coverageModulationTableCount" x)
                                                .
                                                showString ", " .
                                                  showString "pCoverageModulationTable = " .
                                                    showsPrec d
                                                      (getField @"pCoverageModulationTable" x)
                                                      . showChar '}'

-- | > typedef struct VkPipelineCoverageToColorStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineCoverageToColorStateCreateFlagsNV                    flags;
--   >     VkBool32                         coverageToColorEnable;
--   >     uint32_t         coverageToColorLocation;
--   > } VkPipelineCoverageToColorStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineCoverageToColorStateCreateInfoNV VkPipelineCoverageToColorStateCreateInfoNV registry at www.khronos.org>
data VkPipelineCoverageToColorStateCreateInfoNV = VkPipelineCoverageToColorStateCreateInfoNV## Addr##
                                                                                              ByteArray##

instance Eq VkPipelineCoverageToColorStateCreateInfoNV where
        (VkPipelineCoverageToColorStateCreateInfoNV## a _) ==
          x@(VkPipelineCoverageToColorStateCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineCoverageToColorStateCreateInfoNV where
        (VkPipelineCoverageToColorStateCreateInfoNV## a _) `compare`
          x@(VkPipelineCoverageToColorStateCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineCoverageToColorStateCreateInfoNV where
        sizeOf ~_
          = #{size VkPipelineCoverageToColorStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineCoverageToColorStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        unsafeAddr (VkPipelineCoverageToColorStateCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineCoverageToColorStateCreateInfoNV## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineCoverageToColorStateCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineCoverageToColorStateCreateInfoNV
         where
        type StructFields VkPipelineCoverageToColorStateCreateInfoNV =
             '["sType", "pNext", "flags", "coverageToColorEnable", -- ' closing tick for hsc2hs
               "coverageToColorLocation"]
        type CUnionType VkPipelineCoverageToColorStateCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineCoverageToColorStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineCoverageToColorStateCreateInfoNV =
             '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineCoverageToColorStateCreateInfoNV where
        type FieldType "sType" VkPipelineCoverageToColorStateCreateInfoNV =
             VkStructureType
        type FieldOptional "sType"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineCoverageToColorStateCreateInfoNV
             =
             #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}
        type FieldIsArray "sType"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineCoverageToColorStateCreateInfoNV where
        type FieldType "pNext" VkPipelineCoverageToColorStateCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineCoverageToColorStateCreateInfoNV
             =
             #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}
        type FieldIsArray "pNext"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineCoverageToColorStateCreateInfoNV where
        type FieldType "flags" VkPipelineCoverageToColorStateCreateInfoNV =
             VkPipelineCoverageToColorStateCreateFlagsNV
        type FieldOptional "flags"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineCoverageToColorStateCreateInfoNV
             =
             #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}
        type FieldIsArray "flags"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         HasField "coverageToColorEnable"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        type FieldType "coverageToColorEnable"
               VkPipelineCoverageToColorStateCreateInfoNV
             = VkBool32
        type FieldOptional "coverageToColorEnable"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "coverageToColorEnable"
               VkPipelineCoverageToColorStateCreateInfoNV
             =
             #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}
        type FieldIsArray "coverageToColorEnable"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}

instance {-# OVERLAPPING #-}
         CanReadField "coverageToColorEnable"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "coverageToColorEnable"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}

instance {-# OVERLAPPING #-}
         HasField "coverageToColorLocation"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        type FieldType "coverageToColorLocation"
               VkPipelineCoverageToColorStateCreateInfoNV
             = Word32
        type FieldOptional "coverageToColorLocation"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "coverageToColorLocation"
               VkPipelineCoverageToColorStateCreateInfoNV
             =
             #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}
        type FieldIsArray "coverageToColorLocation"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}

instance {-# OVERLAPPING #-}
         CanReadField "coverageToColorLocation"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}

instance {-# OVERLAPPING #-}
         CanWriteField "coverageToColorLocation"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}

instance Show VkPipelineCoverageToColorStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineCoverageToColorStateCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "coverageToColorEnable = " .
                                  showsPrec d (getField @"coverageToColorEnable" x) .
                                    showString ", " .
                                      showString "coverageToColorLocation = " .
                                        showsPrec d (getField @"coverageToColorLocation" x) .
                                          showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineDepthStencilStateCreateInfo VkPipelineDepthStencilStateCreateInfo registry at www.khronos.org>
data VkPipelineDepthStencilStateCreateInfo = VkPipelineDepthStencilStateCreateInfo## Addr##
                                                                                    ByteArray##

instance Eq VkPipelineDepthStencilStateCreateInfo where
        (VkPipelineDepthStencilStateCreateInfo## a _) ==
          x@(VkPipelineDepthStencilStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineDepthStencilStateCreateInfo where
        (VkPipelineDepthStencilStateCreateInfo## a _) `compare`
          x@(VkPipelineDepthStencilStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineDepthStencilStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineDepthStencilStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineDepthStencilStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineDepthStencilStateCreateInfo
         where
        unsafeAddr (VkPipelineDepthStencilStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineDepthStencilStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineDepthStencilStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineDepthStencilStateCreateInfo where
        type StructFields VkPipelineDepthStencilStateCreateInfo =
             '["sType", "pNext", "flags", "depthTestEnable", "depthWriteEnable", -- ' closing tick for hsc2hs
               "depthCompareOp", "depthBoundsTestEnable", "stencilTestEnable",
               "front", "back", "minDepthBounds", "maxDepthBounds"]
        type CUnionType VkPipelineDepthStencilStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineDepthStencilStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineDepthStencilStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "sType" VkPipelineDepthStencilStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineDepthStencilStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineDepthStencilStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "pNext" VkPipelineDepthStencilStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineDepthStencilStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineDepthStencilStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "flags" VkPipelineDepthStencilStateCreateInfo =
             VkPipelineDepthStencilStateCreateFlags
        type FieldOptional "flags" VkPipelineDepthStencilStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineDepthStencilStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineDepthStencilStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "depthTestEnable" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}
        type FieldIsArray "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}

instance {-# OVERLAPPING #-}
         CanReadField "depthTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "depthTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}

instance {-# OVERLAPPING #-}
         HasField "depthWriteEnable" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}
        type FieldIsArray "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}

instance {-# OVERLAPPING #-}
         CanReadField "depthWriteEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "depthWriteEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}

instance {-# OVERLAPPING #-}
         HasField "depthCompareOp" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             = VkCompareOp
        type FieldOptional "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}
        type FieldIsArray "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}

instance {-# OVERLAPPING #-}
         CanReadField "depthCompareOp" VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}

instance {-# OVERLAPPING #-}
         CanWriteField "depthCompareOp"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}

instance {-# OVERLAPPING #-}
         HasField "depthBoundsTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}
        type FieldIsArray "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}

instance {-# OVERLAPPING #-}
         CanReadField "depthBoundsTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBoundsTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}

instance {-# OVERLAPPING #-}
         HasField "stencilTestEnable" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}
        type FieldIsArray "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}

instance {-# OVERLAPPING #-}
         CanReadField "stencilTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "stencilTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}

instance {-# OVERLAPPING #-}
         HasField "front" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "front" VkPipelineDepthStencilStateCreateInfo =
             VkStencilOpState
        type FieldOptional "front" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "front" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, front}
        type FieldIsArray "front" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, front}

instance {-# OVERLAPPING #-}
         CanReadField "front" VkPipelineDepthStencilStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, front})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, front}

instance {-# OVERLAPPING #-}
         CanWriteField "front" VkPipelineDepthStencilStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, front}

instance {-# OVERLAPPING #-}
         HasField "back" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "back" VkPipelineDepthStencilStateCreateInfo =
             VkStencilOpState
        type FieldOptional "back" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "back" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, back}
        type FieldIsArray "back" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, back}

instance {-# OVERLAPPING #-}
         CanReadField "back" VkPipelineDepthStencilStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, back})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, back}

instance {-# OVERLAPPING #-}
         CanWriteField "back" VkPipelineDepthStencilStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, back}

instance {-# OVERLAPPING #-}
         HasField "minDepthBounds" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = #{type float}
        type FieldOptional "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}
        type FieldIsArray "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}

instance {-# OVERLAPPING #-}
         CanReadField "minDepthBounds" VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}

instance {-# OVERLAPPING #-}
         CanWriteField "minDepthBounds"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}

instance {-# OVERLAPPING #-}
         HasField "maxDepthBounds" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = #{type float}
        type FieldOptional "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}
        type FieldIsArray "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}

instance {-# OVERLAPPING #-}
         CanReadField "maxDepthBounds" VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDepthBounds"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}

instance Show VkPipelineDepthStencilStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineDepthStencilStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "depthTestEnable = " .
                                  showsPrec d (getField @"depthTestEnable" x) .
                                    showString ", " .
                                      showString "depthWriteEnable = " .
                                        showsPrec d (getField @"depthWriteEnable" x) .
                                          showString ", " .
                                            showString "depthCompareOp = " .
                                              showsPrec d (getField @"depthCompareOp" x) .
                                                showString ", " .
                                                  showString "depthBoundsTestEnable = " .
                                                    showsPrec d
                                                      (getField @"depthBoundsTestEnable" x)
                                                      .
                                                      showString ", " .
                                                        showString "stencilTestEnable = " .
                                                          showsPrec d
                                                            (getField @"stencilTestEnable" x)
                                                            .
                                                            showString ", " .
                                                              showString "front = " .
                                                                showsPrec d (getField @"front" x) .
                                                                  showString ", " .
                                                                    showString "back = " .
                                                                      showsPrec d
                                                                        (getField @"back" x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "minDepthBounds = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"minDepthBounds"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "maxDepthBounds = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"maxDepthBounds"
                                                                                       x)
                                                                                    . showChar '}'

-- | > typedef struct VkPipelineDiscardRectangleStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineDiscardRectangleStateCreateFlagsEXT                    flags;
--   >     VkDiscardRectangleModeEXT                                                        discardRectangleMode;
--   >     uint32_t                                                         discardRectangleCount;
--   >     const VkRect2D* pDiscardRectangles;
--   > } VkPipelineDiscardRectangleStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineDiscardRectangleStateCreateInfoEXT VkPipelineDiscardRectangleStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineDiscardRectangleStateCreateInfoEXT = VkPipelineDiscardRectangleStateCreateInfoEXT## Addr##
                                                                                                  ByteArray##

instance Eq VkPipelineDiscardRectangleStateCreateInfoEXT where
        (VkPipelineDiscardRectangleStateCreateInfoEXT## a _) ==
          x@(VkPipelineDiscardRectangleStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineDiscardRectangleStateCreateInfoEXT where
        (VkPipelineDiscardRectangleStateCreateInfoEXT## a _) `compare`
          x@(VkPipelineDiscardRectangleStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineDiscardRectangleStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineDiscardRectangleStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        unsafeAddr (VkPipelineDiscardRectangleStateCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineDiscardRectangleStateCreateInfoEXT## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineDiscardRectangleStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type StructFields VkPipelineDiscardRectangleStateCreateInfoEXT =
             '["sType", "pNext", "flags", "discardRectangleMode", -- ' closing tick for hsc2hs
               "discardRectangleCount", "pDiscardRectangles"]
        type CUnionType VkPipelineDiscardRectangleStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineDiscardRectangleStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineDiscardRectangleStateCreateInfoEXT =
             '[VkGraphicsPipelineCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineDiscardRectangleStateCreateInfoEXT where
        type FieldType "sType" VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineDiscardRectangleStateCreateInfoEXT where
        type FieldType "pNext" VkPipelineDiscardRectangleStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineDiscardRectangleStateCreateInfoEXT where
        type FieldType "flags" VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkPipelineDiscardRectangleStateCreateFlagsEXT
        type FieldOptional "flags"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}
        type FieldIsArray "flags"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type FieldType "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkDiscardRectangleModeEXT
        type FieldOptional "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}
        type FieldIsArray "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

instance {-# OVERLAPPING #-}
         CanReadField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

instance {-# OVERLAPPING #-}
         CanWriteField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

instance {-# OVERLAPPING #-}
         HasField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type FieldType "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = Word32
        type FieldOptional "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}
        type FieldIsArray "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

instance {-# OVERLAPPING #-}
         CanReadField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

instance {-# OVERLAPPING #-}
         CanWriteField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

instance {-# OVERLAPPING #-}
         HasField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type FieldType "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = Ptr VkRect2D
        type FieldOptional "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}
        type FieldIsArray "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

instance {-# OVERLAPPING #-}
         CanReadField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

instance {-# OVERLAPPING #-}
         CanWriteField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

instance Show VkPipelineDiscardRectangleStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineDiscardRectangleStateCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "discardRectangleMode = " .
                                  showsPrec d (getField @"discardRectangleMode" x) .
                                    showString ", " .
                                      showString "discardRectangleCount = " .
                                        showsPrec d (getField @"discardRectangleCount" x) .
                                          showString ", " .
                                            showString "pDiscardRectangles = " .
                                              showsPrec d (getField @"pDiscardRectangles" x) .
                                                showChar '}'

-- | > typedef struct VkPipelineDynamicStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineDynamicStateCreateFlags    flags;
--   >     uint32_t               dynamicStateCount;
--   >     const VkDynamicState*  pDynamicStates;
--   > } VkPipelineDynamicStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineDynamicStateCreateInfo VkPipelineDynamicStateCreateInfo registry at www.khronos.org>
data VkPipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo## Addr##
                                                                          ByteArray##

instance Eq VkPipelineDynamicStateCreateInfo where
        (VkPipelineDynamicStateCreateInfo## a _) ==
          x@(VkPipelineDynamicStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineDynamicStateCreateInfo where
        (VkPipelineDynamicStateCreateInfo## a _) `compare`
          x@(VkPipelineDynamicStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineDynamicStateCreateInfo where
        sizeOf ~_ = #{size VkPipelineDynamicStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineDynamicStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineDynamicStateCreateInfo where
        unsafeAddr (VkPipelineDynamicStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineDynamicStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineDynamicStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineDynamicStateCreateInfo where
        type StructFields VkPipelineDynamicStateCreateInfo =
             '["sType", "pNext", "flags", "dynamicStateCount", "pDynamicStates"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineDynamicStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineDynamicStateCreateInfo where
        type FieldType "sType" VkPipelineDynamicStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineDynamicStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineDynamicStateCreateInfo =
             #{offset VkPipelineDynamicStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineDynamicStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineDynamicStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineDynamicStateCreateInfo where
        type FieldType "pNext" VkPipelineDynamicStateCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineDynamicStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineDynamicStateCreateInfo =
             #{offset VkPipelineDynamicStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineDynamicStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineDynamicStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineDynamicStateCreateInfo where
        type FieldType "flags" VkPipelineDynamicStateCreateInfo =
             VkPipelineDynamicStateCreateFlags
        type FieldOptional "flags" VkPipelineDynamicStateCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineDynamicStateCreateInfo =
             #{offset VkPipelineDynamicStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineDynamicStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineDynamicStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "dynamicStateCount" VkPipelineDynamicStateCreateInfo where
        type FieldType "dynamicStateCount" VkPipelineDynamicStateCreateInfo
             = Word32
        type FieldOptional "dynamicStateCount"
               VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dynamicStateCount"
               VkPipelineDynamicStateCreateInfo
             =
             #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}
        type FieldIsArray "dynamicStateCount"
               VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}

instance {-# OVERLAPPING #-}
         CanReadField "dynamicStateCount" VkPipelineDynamicStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}

instance {-# OVERLAPPING #-}
         CanWriteField "dynamicStateCount" VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}

instance {-# OVERLAPPING #-}
         HasField "pDynamicStates" VkPipelineDynamicStateCreateInfo where
        type FieldType "pDynamicStates" VkPipelineDynamicStateCreateInfo =
             Ptr VkDynamicState
        type FieldOptional "pDynamicStates"
               VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDynamicStates" VkPipelineDynamicStateCreateInfo
             =
             #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}
        type FieldIsArray "pDynamicStates" VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}

instance {-# OVERLAPPING #-}
         CanReadField "pDynamicStates" VkPipelineDynamicStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}

instance {-# OVERLAPPING #-}
         CanWriteField "pDynamicStates" VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}

instance Show VkPipelineDynamicStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineDynamicStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "dynamicStateCount = " .
                                  showsPrec d (getField @"dynamicStateCount" x) .
                                    showString ", " .
                                      showString "pDynamicStates = " .
                                        showsPrec d (getField @"pDynamicStates" x) . showChar '}'

-- | > typedef struct VkPipelineInputAssemblyStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineInputAssemblyStateCreateFlags    flags;
--   >     VkPrimitiveTopology    topology;
--   >     VkBool32               primitiveRestartEnable;
--   > } VkPipelineInputAssemblyStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineInputAssemblyStateCreateInfo VkPipelineInputAssemblyStateCreateInfo registry at www.khronos.org>
data VkPipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo## Addr##
                                                                                      ByteArray##

instance Eq VkPipelineInputAssemblyStateCreateInfo where
        (VkPipelineInputAssemblyStateCreateInfo## a _) ==
          x@(VkPipelineInputAssemblyStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineInputAssemblyStateCreateInfo where
        (VkPipelineInputAssemblyStateCreateInfo## a _) `compare`
          x@(VkPipelineInputAssemblyStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineInputAssemblyStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineInputAssemblyStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineInputAssemblyStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineInputAssemblyStateCreateInfo
         where
        unsafeAddr (VkPipelineInputAssemblyStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineInputAssemblyStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineInputAssemblyStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineInputAssemblyStateCreateInfo where
        type StructFields VkPipelineInputAssemblyStateCreateInfo =
             '["sType", "pNext", "flags", "topology", "primitiveRestartEnable"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineInputAssemblyStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineInputAssemblyStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineInputAssemblyStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "sType" VkPipelineInputAssemblyStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineInputAssemblyStateCreateInfo =
             #{offset VkPipelineInputAssemblyStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineInputAssemblyStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineInputAssemblyStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "pNext" VkPipelineInputAssemblyStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineInputAssemblyStateCreateInfo =
             #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineInputAssemblyStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineInputAssemblyStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "flags" VkPipelineInputAssemblyStateCreateInfo =
             VkPipelineInputAssemblyStateCreateFlags
        type FieldOptional "flags" VkPipelineInputAssemblyStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineInputAssemblyStateCreateInfo =
             #{offset VkPipelineInputAssemblyStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineInputAssemblyStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineInputAssemblyStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "topology" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "topology" VkPipelineInputAssemblyStateCreateInfo =
             VkPrimitiveTopology
        type FieldOptional "topology"
               VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "topology" VkPipelineInputAssemblyStateCreateInfo
             =
             #{offset VkPipelineInputAssemblyStateCreateInfo, topology}
        type FieldIsArray "topology" VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, topology}

instance {-# OVERLAPPING #-}
         CanReadField "topology" VkPipelineInputAssemblyStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, topology})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, topology}

instance {-# OVERLAPPING #-}
         CanWriteField "topology" VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, topology}

instance {-# OVERLAPPING #-}
         HasField "primitiveRestartEnable"
           VkPipelineInputAssemblyStateCreateInfo
         where
        type FieldType "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             = VkBool32
        type FieldOptional "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             =
             #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}
        type FieldIsArray "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}

instance {-# OVERLAPPING #-}
         CanReadField "primitiveRestartEnable"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "primitiveRestartEnable"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}

instance Show VkPipelineInputAssemblyStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineInputAssemblyStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "topology = " .
                                  showsPrec d (getField @"topology" x) .
                                    showString ", " .
                                      showString "primitiveRestartEnable = " .
                                        showsPrec d (getField @"primitiveRestartEnable" x) .
                                          showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineLayoutCreateInfo VkPipelineLayoutCreateInfo registry at www.khronos.org>
data VkPipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo## Addr##
                                                              ByteArray##

instance Eq VkPipelineLayoutCreateInfo where
        (VkPipelineLayoutCreateInfo## a _) ==
          x@(VkPipelineLayoutCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineLayoutCreateInfo where
        (VkPipelineLayoutCreateInfo## a _) `compare`
          x@(VkPipelineLayoutCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineLayoutCreateInfo where
        sizeOf ~_ = #{size VkPipelineLayoutCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPipelineLayoutCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineLayoutCreateInfo where
        unsafeAddr (VkPipelineLayoutCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineLayoutCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineLayoutCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineLayoutCreateInfo where
        type StructFields VkPipelineLayoutCreateInfo =
             '["sType", "pNext", "flags", "setLayoutCount", "pSetLayouts", -- ' closing tick for hsc2hs
               "pushConstantRangeCount", "pPushConstantRanges"]
        type CUnionType VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineLayoutCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineLayoutCreateInfo where
        type FieldType "sType" VkPipelineLayoutCreateInfo = VkStructureType
        type FieldOptional "sType" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineLayoutCreateInfo where
        type FieldType "pNext" VkPipelineLayoutCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineLayoutCreateInfo where
        type FieldType "flags" VkPipelineLayoutCreateInfo =
             VkPipelineLayoutCreateFlags
        type FieldOptional "flags" VkPipelineLayoutCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "setLayoutCount" VkPipelineLayoutCreateInfo where
        type FieldType "setLayoutCount" VkPipelineLayoutCreateInfo = Word32
        type FieldOptional "setLayoutCount" VkPipelineLayoutCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "setLayoutCount" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, setLayoutCount}
        type FieldIsArray "setLayoutCount" VkPipelineLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, setLayoutCount}

instance {-# OVERLAPPING #-}
         CanReadField "setLayoutCount" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, setLayoutCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, setLayoutCount}

instance {-# OVERLAPPING #-}
         CanWriteField "setLayoutCount" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, setLayoutCount}

instance {-# OVERLAPPING #-}
         HasField "pSetLayouts" VkPipelineLayoutCreateInfo where
        type FieldType "pSetLayouts" VkPipelineLayoutCreateInfo =
             Ptr VkDescriptorSetLayout
        type FieldOptional "pSetLayouts" VkPipelineLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pSetLayouts" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, pSetLayouts}
        type FieldIsArray "pSetLayouts" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, pSetLayouts}

instance {-# OVERLAPPING #-}
         CanReadField "pSetLayouts" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pSetLayouts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pSetLayouts}

instance {-# OVERLAPPING #-}
         CanWriteField "pSetLayouts" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pSetLayouts}

instance {-# OVERLAPPING #-}
         HasField "pushConstantRangeCount" VkPipelineLayoutCreateInfo where
        type FieldType "pushConstantRangeCount" VkPipelineLayoutCreateInfo
             = Word32
        type FieldOptional "pushConstantRangeCount"
               VkPipelineLayoutCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pushConstantRangeCount"
               VkPipelineLayoutCreateInfo
             =
             #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}
        type FieldIsArray "pushConstantRangeCount"
               VkPipelineLayoutCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}

instance {-# OVERLAPPING #-}
         CanReadField "pushConstantRangeCount" VkPipelineLayoutCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}

instance {-# OVERLAPPING #-}
         CanWriteField "pushConstantRangeCount" VkPipelineLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}

instance {-# OVERLAPPING #-}
         HasField "pPushConstantRanges" VkPipelineLayoutCreateInfo where
        type FieldType "pPushConstantRanges" VkPipelineLayoutCreateInfo =
             Ptr VkPushConstantRange
        type FieldOptional "pPushConstantRanges" VkPipelineLayoutCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPushConstantRanges" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}
        type FieldIsArray "pPushConstantRanges" VkPipelineLayoutCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}

instance {-# OVERLAPPING #-}
         CanReadField "pPushConstantRanges" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}

instance {-# OVERLAPPING #-}
         CanWriteField "pPushConstantRanges" VkPipelineLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}

instance Show VkPipelineLayoutCreateInfo where
        showsPrec d x
          = showString "VkPipelineLayoutCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "setLayoutCount = " .
                                  showsPrec d (getField @"setLayoutCount" x) .
                                    showString ", " .
                                      showString "pSetLayouts = " .
                                        showsPrec d (getField @"pSetLayouts" x) .
                                          showString ", " .
                                            showString "pushConstantRangeCount = " .
                                              showsPrec d (getField @"pushConstantRangeCount" x) .
                                                showString ", " .
                                                  showString "pPushConstantRanges = " .
                                                    showsPrec d (getField @"pPushConstantRanges" x)
                                                      . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineMultisampleStateCreateInfo VkPipelineMultisampleStateCreateInfo registry at www.khronos.org>
data VkPipelineMultisampleStateCreateInfo = VkPipelineMultisampleStateCreateInfo## Addr##
                                                                                  ByteArray##

instance Eq VkPipelineMultisampleStateCreateInfo where
        (VkPipelineMultisampleStateCreateInfo## a _) ==
          x@(VkPipelineMultisampleStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineMultisampleStateCreateInfo where
        (VkPipelineMultisampleStateCreateInfo## a _) `compare`
          x@(VkPipelineMultisampleStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineMultisampleStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineMultisampleStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineMultisampleStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineMultisampleStateCreateInfo
         where
        unsafeAddr (VkPipelineMultisampleStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineMultisampleStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineMultisampleStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineMultisampleStateCreateInfo where
        type StructFields VkPipelineMultisampleStateCreateInfo =
             '["sType", "pNext", "flags", "rasterizationSamples", -- ' closing tick for hsc2hs
               "sampleShadingEnable", "minSampleShading", "pSampleMask",
               "alphaToCoverageEnable", "alphaToOneEnable"]
        type CUnionType VkPipelineMultisampleStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineMultisampleStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineMultisampleStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineMultisampleStateCreateInfo where
        type FieldType "sType" VkPipelineMultisampleStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineMultisampleStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineMultisampleStateCreateInfo =
             #{offset VkPipelineMultisampleStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineMultisampleStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineMultisampleStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineMultisampleStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineMultisampleStateCreateInfo where
        type FieldType "pNext" VkPipelineMultisampleStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineMultisampleStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineMultisampleStateCreateInfo =
             #{offset VkPipelineMultisampleStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineMultisampleStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineMultisampleStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineMultisampleStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineMultisampleStateCreateInfo where
        type FieldType "flags" VkPipelineMultisampleStateCreateInfo =
             VkPipelineMultisampleStateCreateFlags
        type FieldOptional "flags" VkPipelineMultisampleStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineMultisampleStateCreateInfo =
             #{offset VkPipelineMultisampleStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineMultisampleStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineMultisampleStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineMultisampleStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "rasterizationSamples"
           VkPipelineMultisampleStateCreateInfo
         where
        type FieldType "rasterizationSamples"
               VkPipelineMultisampleStateCreateInfo
             = VkSampleCountFlagBits
        type FieldOptional "rasterizationSamples"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "rasterizationSamples"
               VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}
        type FieldIsArray "rasterizationSamples"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}

instance {-# OVERLAPPING #-}
         CanReadField "rasterizationSamples"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}

instance {-# OVERLAPPING #-}
         CanWriteField "rasterizationSamples"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}

instance {-# OVERLAPPING #-}
         HasField "sampleShadingEnable" VkPipelineMultisampleStateCreateInfo
         where
        type FieldType "sampleShadingEnable"
               VkPipelineMultisampleStateCreateInfo
             = VkBool32
        type FieldOptional "sampleShadingEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleShadingEnable"
               VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}
        type FieldIsArray "sampleShadingEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}

instance {-# OVERLAPPING #-}
         CanReadField "sampleShadingEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleShadingEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}

instance {-# OVERLAPPING #-}
         HasField "minSampleShading" VkPipelineMultisampleStateCreateInfo
         where
        type FieldType "minSampleShading"
               VkPipelineMultisampleStateCreateInfo
             = #{type float}
        type FieldOptional "minSampleShading"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minSampleShading"
               VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}
        type FieldIsArray "minSampleShading"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}

instance {-# OVERLAPPING #-}
         CanReadField "minSampleShading"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}

instance {-# OVERLAPPING #-}
         CanWriteField "minSampleShading"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}

instance {-# OVERLAPPING #-}
         HasField "pSampleMask" VkPipelineMultisampleStateCreateInfo where
        type FieldType "pSampleMask" VkPipelineMultisampleStateCreateInfo =
             Ptr VkSampleMask
        type FieldOptional "pSampleMask"
               VkPipelineMultisampleStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pSampleMask" VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}
        type FieldIsArray "pSampleMask"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}

instance {-# OVERLAPPING #-}
         CanReadField "pSampleMask" VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}

instance {-# OVERLAPPING #-}
         CanWriteField "pSampleMask" VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}

instance {-# OVERLAPPING #-}
         HasField "alphaToCoverageEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        type FieldType "alphaToCoverageEnable"
               VkPipelineMultisampleStateCreateInfo
             = VkBool32
        type FieldOptional "alphaToCoverageEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "alphaToCoverageEnable"
               VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}
        type FieldIsArray "alphaToCoverageEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}

instance {-# OVERLAPPING #-}
         CanReadField "alphaToCoverageEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "alphaToCoverageEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}

instance {-# OVERLAPPING #-}
         HasField "alphaToOneEnable" VkPipelineMultisampleStateCreateInfo
         where
        type FieldType "alphaToOneEnable"
               VkPipelineMultisampleStateCreateInfo
             = VkBool32
        type FieldOptional "alphaToOneEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "alphaToOneEnable"
               VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}
        type FieldIsArray "alphaToOneEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}

instance {-# OVERLAPPING #-}
         CanReadField "alphaToOneEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "alphaToOneEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}

instance Show VkPipelineMultisampleStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineMultisampleStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "rasterizationSamples = " .
                                  showsPrec d (getField @"rasterizationSamples" x) .
                                    showString ", " .
                                      showString "sampleShadingEnable = " .
                                        showsPrec d (getField @"sampleShadingEnable" x) .
                                          showString ", " .
                                            showString "minSampleShading = " .
                                              showsPrec d (getField @"minSampleShading" x) .
                                                showString ", " .
                                                  showString "pSampleMask = " .
                                                    showsPrec d (getField @"pSampleMask" x) .
                                                      showString ", " .
                                                        showString "alphaToCoverageEnable = " .
                                                          showsPrec d
                                                            (getField @"alphaToCoverageEnable" x)
                                                            .
                                                            showString ", " .
                                                              showString "alphaToOneEnable = " .
                                                                showsPrec d
                                                                  (getField @"alphaToOneEnable" x)
                                                                  . showChar '}'

-- | > typedef struct VkPipelineRasterizationConservativeStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineRasterizationConservativeStateCreateFlagsEXT           flags;
--   >     VkConservativeRasterizationModeEXT                                               conservativeRasterizationMode;
--   >     float                                                                            extraPrimitiveOverestimationSize;
--   > } VkPipelineRasterizationConservativeStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineRasterizationConservativeStateCreateInfoEXT VkPipelineRasterizationConservativeStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineRasterizationConservativeStateCreateInfoEXT = VkPipelineRasterizationConservativeStateCreateInfoEXT## Addr##
                                                                                                                    ByteArray##

instance Eq VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        (VkPipelineRasterizationConservativeStateCreateInfoEXT## a _) ==
          x@(VkPipelineRasterizationConservativeStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        (VkPipelineRasterizationConservativeStateCreateInfoEXT## a _)
          `compare`
          x@(VkPipelineRasterizationConservativeStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineRasterizationConservativeStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineRasterizationConservativeStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        unsafeAddr
          (VkPipelineRasterizationConservativeStateCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineRasterizationConservativeStateCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineRasterizationConservativeStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type StructFields
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             '["sType", "pNext", "flags", "conservativeRasterizationMode", -- ' closing tick for hsc2hs
               "extraPrimitiveOverestimationSize"]
        type CUnionType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = '[VkPipelineRasterizationStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "sType"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "pNext"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "flags"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkPipelineRasterizationConservativeStateCreateFlagsEXT
        type FieldOptional "flags"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}
        type FieldIsArray "flags"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "conservativeRasterizationMode"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkConservativeRasterizationModeEXT
        type FieldOptional "conservativeRasterizationMode"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "conservativeRasterizationMode"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}
        type FieldIsArray "conservativeRasterizationMode"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

instance {-# OVERLAPPING #-}
         CanReadField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

instance {-# OVERLAPPING #-}
         CanWriteField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

instance {-# OVERLAPPING #-}
         HasField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "extraPrimitiveOverestimationSize"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = #{type float}
        type FieldOptional "extraPrimitiveOverestimationSize"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extraPrimitiveOverestimationSize"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}
        type FieldIsArray "extraPrimitiveOverestimationSize"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

instance {-# OVERLAPPING #-}
         CanReadField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

instance {-# OVERLAPPING #-}
         CanWriteField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

instance Show VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        showsPrec d x
          = showString
              "VkPipelineRasterizationConservativeStateCreateInfoEXT {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "conservativeRasterizationMode = " .
                                  showsPrec d (getField @"conservativeRasterizationMode" x) .
                                    showString ", " .
                                      showString "extraPrimitiveOverestimationSize = " .
                                        showsPrec d (getField @"extraPrimitiveOverestimationSize" x)
                                          . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineRasterizationStateCreateInfo VkPipelineRasterizationStateCreateInfo registry at www.khronos.org>
data VkPipelineRasterizationStateCreateInfo = VkPipelineRasterizationStateCreateInfo## Addr##
                                                                                      ByteArray##

instance Eq VkPipelineRasterizationStateCreateInfo where
        (VkPipelineRasterizationStateCreateInfo## a _) ==
          x@(VkPipelineRasterizationStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineRasterizationStateCreateInfo where
        (VkPipelineRasterizationStateCreateInfo## a _) `compare`
          x@(VkPipelineRasterizationStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineRasterizationStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineRasterizationStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineRasterizationStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineRasterizationStateCreateInfo
         where
        unsafeAddr (VkPipelineRasterizationStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineRasterizationStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineRasterizationStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineRasterizationStateCreateInfo where
        type StructFields VkPipelineRasterizationStateCreateInfo =
             '["sType", "pNext", "flags", "depthClampEnable", -- ' closing tick for hsc2hs
               "rasterizerDiscardEnable", "polygonMode", "cullMode", "frontFace",
               "depthBiasEnable", "depthBiasConstantFactor", "depthBiasClamp",
               "depthBiasSlopeFactor", "lineWidth"]
        type CUnionType VkPipelineRasterizationStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineRasterizationStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineRasterizationStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineRasterizationStateCreateInfo where
        type FieldType "sType" VkPipelineRasterizationStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineRasterizationStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineRasterizationStateCreateInfo =
             #{offset VkPipelineRasterizationStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineRasterizationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineRasterizationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineRasterizationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineRasterizationStateCreateInfo where
        type FieldType "pNext" VkPipelineRasterizationStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineRasterizationStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineRasterizationStateCreateInfo =
             #{offset VkPipelineRasterizationStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineRasterizationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineRasterizationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineRasterizationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineRasterizationStateCreateInfo where
        type FieldType "flags" VkPipelineRasterizationStateCreateInfo =
             VkPipelineRasterizationStateCreateFlags
        type FieldOptional "flags" VkPipelineRasterizationStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineRasterizationStateCreateInfo =
             #{offset VkPipelineRasterizationStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineRasterizationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineRasterizationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineRasterizationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "depthClampEnable" VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "depthClampEnable"
               VkPipelineRasterizationStateCreateInfo
             = VkBool32
        type FieldOptional "depthClampEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthClampEnable"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}
        type FieldIsArray "depthClampEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}

instance {-# OVERLAPPING #-}
         CanReadField "depthClampEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "depthClampEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}

instance {-# OVERLAPPING #-}
         HasField "rasterizerDiscardEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "rasterizerDiscardEnable"
               VkPipelineRasterizationStateCreateInfo
             = VkBool32
        type FieldOptional "rasterizerDiscardEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "rasterizerDiscardEnable"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}
        type FieldIsArray "rasterizerDiscardEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}

instance {-# OVERLAPPING #-}
         CanReadField "rasterizerDiscardEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "rasterizerDiscardEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}

instance {-# OVERLAPPING #-}
         HasField "polygonMode" VkPipelineRasterizationStateCreateInfo where
        type FieldType "polygonMode" VkPipelineRasterizationStateCreateInfo
             = VkPolygonMode
        type FieldOptional "polygonMode"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "polygonMode"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}
        type FieldIsArray "polygonMode"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}

instance {-# OVERLAPPING #-}
         CanReadField "polygonMode" VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, polygonMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}

instance {-# OVERLAPPING #-}
         CanWriteField "polygonMode" VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}

instance {-# OVERLAPPING #-}
         HasField "cullMode" VkPipelineRasterizationStateCreateInfo where
        type FieldType "cullMode" VkPipelineRasterizationStateCreateInfo =
             VkCullModeFlags
        type FieldOptional "cullMode"
               VkPipelineRasterizationStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "cullMode" VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, cullMode}
        type FieldIsArray "cullMode" VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, cullMode}

instance {-# OVERLAPPING #-}
         CanReadField "cullMode" VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, cullMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, cullMode}

instance {-# OVERLAPPING #-}
         CanWriteField "cullMode" VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, cullMode}

instance {-# OVERLAPPING #-}
         HasField "frontFace" VkPipelineRasterizationStateCreateInfo where
        type FieldType "frontFace" VkPipelineRasterizationStateCreateInfo =
             VkFrontFace
        type FieldOptional "frontFace"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "frontFace" VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, frontFace}
        type FieldIsArray "frontFace"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, frontFace}

instance {-# OVERLAPPING #-}
         CanReadField "frontFace" VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, frontFace})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, frontFace}

instance {-# OVERLAPPING #-}
         CanWriteField "frontFace" VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, frontFace}

instance {-# OVERLAPPING #-}
         HasField "depthBiasEnable" VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "depthBiasEnable"
               VkPipelineRasterizationStateCreateInfo
             = VkBool32
        type FieldOptional "depthBiasEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBiasEnable"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}
        type FieldIsArray "depthBiasEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}

instance {-# OVERLAPPING #-}
         CanReadField "depthBiasEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBiasEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}

instance {-# OVERLAPPING #-}
         HasField "depthBiasConstantFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "depthBiasConstantFactor"
               VkPipelineRasterizationStateCreateInfo
             = #{type float}
        type FieldOptional "depthBiasConstantFactor"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBiasConstantFactor"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}
        type FieldIsArray "depthBiasConstantFactor"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}

instance {-# OVERLAPPING #-}
         CanReadField "depthBiasConstantFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBiasConstantFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}

instance {-# OVERLAPPING #-}
         HasField "depthBiasClamp" VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "depthBiasClamp"
               VkPipelineRasterizationStateCreateInfo
             = #{type float}
        type FieldOptional "depthBiasClamp"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBiasClamp"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}
        type FieldIsArray "depthBiasClamp"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}

instance {-# OVERLAPPING #-}
         CanReadField "depthBiasClamp"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBiasClamp"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}

instance {-# OVERLAPPING #-}
         HasField "depthBiasSlopeFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "depthBiasSlopeFactor"
               VkPipelineRasterizationStateCreateInfo
             = #{type float}
        type FieldOptional "depthBiasSlopeFactor"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBiasSlopeFactor"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}
        type FieldIsArray "depthBiasSlopeFactor"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}

instance {-# OVERLAPPING #-}
         CanReadField "depthBiasSlopeFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBiasSlopeFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}

instance {-# OVERLAPPING #-}
         HasField "lineWidth" VkPipelineRasterizationStateCreateInfo where
        type FieldType "lineWidth" VkPipelineRasterizationStateCreateInfo =
             #{type float}
        type FieldOptional "lineWidth"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "lineWidth" VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}
        type FieldIsArray "lineWidth"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}

instance {-# OVERLAPPING #-}
         CanReadField "lineWidth" VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, lineWidth})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}

instance {-# OVERLAPPING #-}
         CanWriteField "lineWidth" VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}

instance Show VkPipelineRasterizationStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineRasterizationStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "depthClampEnable = " .
                                  showsPrec d (getField @"depthClampEnable" x) .
                                    showString ", " .
                                      showString "rasterizerDiscardEnable = " .
                                        showsPrec d (getField @"rasterizerDiscardEnable" x) .
                                          showString ", " .
                                            showString "polygonMode = " .
                                              showsPrec d (getField @"polygonMode" x) .
                                                showString ", " .
                                                  showString "cullMode = " .
                                                    showsPrec d (getField @"cullMode" x) .
                                                      showString ", " .
                                                        showString "frontFace = " .
                                                          showsPrec d (getField @"frontFace" x) .
                                                            showString ", " .
                                                              showString "depthBiasEnable = " .
                                                                showsPrec d
                                                                  (getField @"depthBiasEnable" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "depthBiasConstantFactor = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"depthBiasConstantFactor"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "depthBiasClamp = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"depthBiasClamp"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "depthBiasSlopeFactor = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"depthBiasSlopeFactor"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "lineWidth = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"lineWidth"
                                                                                             x)
                                                                                          .
                                                                                          showChar
                                                                                            '}'

-- | > typedef struct VkPipelineRasterizationStateRasterizationOrderAMD {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkRasterizationOrderAMD          rasterizationOrder;
--   > } VkPipelineRasterizationStateRasterizationOrderAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineRasterizationStateRasterizationOrderAMD VkPipelineRasterizationStateRasterizationOrderAMD registry at www.khronos.org>
data VkPipelineRasterizationStateRasterizationOrderAMD = VkPipelineRasterizationStateRasterizationOrderAMD## Addr##
                                                                                                            ByteArray##

instance Eq VkPipelineRasterizationStateRasterizationOrderAMD where
        (VkPipelineRasterizationStateRasterizationOrderAMD## a _) ==
          x@(VkPipelineRasterizationStateRasterizationOrderAMD## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineRasterizationStateRasterizationOrderAMD
         where
        (VkPipelineRasterizationStateRasterizationOrderAMD## a _) `compare`
          x@(VkPipelineRasterizationStateRasterizationOrderAMD## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineRasterizationStateRasterizationOrderAMD
         where
        sizeOf ~_
          = #{size VkPipelineRasterizationStateRasterizationOrderAMD}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineRasterizationStateRasterizationOrderAMD}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        unsafeAddr (VkPipelineRasterizationStateRasterizationOrderAMD## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineRasterizationStateRasterizationOrderAMD## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineRasterizationStateRasterizationOrderAMD##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type StructFields VkPipelineRasterizationStateRasterizationOrderAMD
             = '["sType", "pNext", "rasterizationOrder"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineRasterizationStateRasterizationOrderAMD =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPipelineRasterizationStateRasterizationOrderAMD
             = '[VkPipelineRasterizationStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type FieldType "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             =
             #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}
        type FieldIsArray "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type FieldType "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             =
             #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}
        type FieldIsArray "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

instance {-# OVERLAPPING #-}
         HasField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type FieldType "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = VkRasterizationOrderAMD
        type FieldOptional "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             =
             #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}
        type FieldIsArray "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

instance {-# OVERLAPPING #-}
         CanReadField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

instance {-# OVERLAPPING #-}
         CanWriteField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

instance Show VkPipelineRasterizationStateRasterizationOrderAMD
         where
        showsPrec d x
          = showString "VkPipelineRasterizationStateRasterizationOrderAMD {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "rasterizationOrder = " .
                            showsPrec d (getField @"rasterizationOrder" x) . showChar '}'

-- | > typedef struct VkPipelineSampleLocationsStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         sampleLocationsEnable;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkPipelineSampleLocationsStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineSampleLocationsStateCreateInfoEXT VkPipelineSampleLocationsStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineSampleLocationsStateCreateInfoEXT = VkPipelineSampleLocationsStateCreateInfoEXT## Addr##
                                                                                                ByteArray##

instance Eq VkPipelineSampleLocationsStateCreateInfoEXT where
        (VkPipelineSampleLocationsStateCreateInfoEXT## a _) ==
          x@(VkPipelineSampleLocationsStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineSampleLocationsStateCreateInfoEXT where
        (VkPipelineSampleLocationsStateCreateInfoEXT## a _) `compare`
          x@(VkPipelineSampleLocationsStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineSampleLocationsStateCreateInfoEXT where
        sizeOf ~_
          = #{size VkPipelineSampleLocationsStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineSampleLocationsStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        unsafeAddr (VkPipelineSampleLocationsStateCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineSampleLocationsStateCreateInfoEXT## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineSampleLocationsStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type StructFields VkPipelineSampleLocationsStateCreateInfoEXT =
             '["sType", "pNext", "sampleLocationsEnable", "sampleLocationsInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineSampleLocationsStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineSampleLocationsStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineSampleLocationsStateCreateInfoEXT =
             '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineSampleLocationsStateCreateInfoEXT where
        type FieldType "sType" VkPipelineSampleLocationsStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineSampleLocationsStateCreateInfoEXT where
        type FieldType "pNext" VkPipelineSampleLocationsStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type FieldType "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkBool32
        type FieldOptional "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}
        type FieldIsArray "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type FieldType "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkSampleLocationsInfoEXT
        type FieldOptional "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}
        type FieldIsArray "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

instance Show VkPipelineSampleLocationsStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineSampleLocationsStateCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "sampleLocationsEnable = " .
                            showsPrec d (getField @"sampleLocationsEnable" x) .
                              showString ", " .
                                showString "sampleLocationsInfo = " .
                                  showsPrec d (getField @"sampleLocationsInfo" x) . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineShaderStageCreateInfo VkPipelineShaderStageCreateInfo registry at www.khronos.org>
data VkPipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo## Addr##
                                                                        ByteArray##

instance Eq VkPipelineShaderStageCreateInfo where
        (VkPipelineShaderStageCreateInfo## a _) ==
          x@(VkPipelineShaderStageCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineShaderStageCreateInfo where
        (VkPipelineShaderStageCreateInfo## a _) `compare`
          x@(VkPipelineShaderStageCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineShaderStageCreateInfo where
        sizeOf ~_ = #{size VkPipelineShaderStageCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineShaderStageCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineShaderStageCreateInfo where
        unsafeAddr (VkPipelineShaderStageCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineShaderStageCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineShaderStageCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineShaderStageCreateInfo where
        type StructFields VkPipelineShaderStageCreateInfo =
             '["sType", "pNext", "flags", "stage", "module", "pName", -- ' closing tick for hsc2hs
               "pSpecializationInfo"]
        type CUnionType VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineShaderStageCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineShaderStageCreateInfo where
        type FieldType "sType" VkPipelineShaderStageCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineShaderStageCreateInfo where
        type FieldType "pNext" VkPipelineShaderStageCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineShaderStageCreateInfo where
        type FieldType "flags" VkPipelineShaderStageCreateInfo =
             VkPipelineShaderStageCreateFlags
        type FieldOptional "flags" VkPipelineShaderStageCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "stage" VkPipelineShaderStageCreateInfo where
        type FieldType "stage" VkPipelineShaderStageCreateInfo =
             VkShaderStageFlagBits
        type FieldOptional "stage" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stage" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, stage}
        type FieldIsArray "stage" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, stage}

instance {-# OVERLAPPING #-}
         CanReadField "stage" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, stage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, stage}

instance {-# OVERLAPPING #-}
         CanWriteField "stage" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, stage}

instance {-# OVERLAPPING #-}
         HasField "module" VkPipelineShaderStageCreateInfo where
        type FieldType "module" VkPipelineShaderStageCreateInfo =
             VkShaderModule
        type FieldOptional "module" VkPipelineShaderStageCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "module" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, module}
        type FieldIsArray "module" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, module}

instance {-# OVERLAPPING #-}
         CanReadField "module" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, module})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, module}

instance {-# OVERLAPPING #-}
         CanWriteField "module" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, module}

instance {-# OVERLAPPING #-}
         HasField "pName" VkPipelineShaderStageCreateInfo where
        type FieldType "pName" VkPipelineShaderStageCreateInfo = CString
        type FieldOptional "pName" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pName" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, pName}
        type FieldIsArray "pName" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, pName}

instance {-# OVERLAPPING #-}
         CanReadField "pName" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, pName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, pName}

instance {-# OVERLAPPING #-}
         CanWriteField "pName" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, pName}

instance {-# OVERLAPPING #-}
         HasField "pSpecializationInfo" VkPipelineShaderStageCreateInfo
         where
        type FieldType "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             = Ptr VkSpecializationInfo
        type FieldOptional "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             =
             #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}
        type FieldIsArray "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}

instance {-# OVERLAPPING #-}
         CanReadField "pSpecializationInfo" VkPipelineShaderStageCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "pSpecializationInfo" VkPipelineShaderStageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}

instance Show VkPipelineShaderStageCreateInfo where
        showsPrec d x
          = showString "VkPipelineShaderStageCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "stage = " .
                                  showsPrec d (getField @"stage" x) .
                                    showString ", " .
                                      showString "module = " .
                                        showsPrec d (getField @"module" x) .
                                          showString ", " .
                                            showString "pName = " .
                                              showsPrec d (getField @"pName" x) .
                                                showString ", " .
                                                  showString "pSpecializationInfo = " .
                                                    showsPrec d (getField @"pSpecializationInfo" x)
                                                      . showChar '}'

-- | > typedef struct VkPipelineTessellationDomainOriginStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkTessellationDomainOrigin    domainOrigin;
--   > } VkPipelineTessellationDomainOriginStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineTessellationDomainOriginStateCreateInfo VkPipelineTessellationDomainOriginStateCreateInfo registry at www.khronos.org>
data VkPipelineTessellationDomainOriginStateCreateInfo = VkPipelineTessellationDomainOriginStateCreateInfo## Addr##
                                                                                                            ByteArray##

instance Eq VkPipelineTessellationDomainOriginStateCreateInfo where
        (VkPipelineTessellationDomainOriginStateCreateInfo## a _) ==
          x@(VkPipelineTessellationDomainOriginStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineTessellationDomainOriginStateCreateInfo
         where
        (VkPipelineTessellationDomainOriginStateCreateInfo## a _) `compare`
          x@(VkPipelineTessellationDomainOriginStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineTessellationDomainOriginStateCreateInfo
         where
        sizeOf ~_
          = #{size VkPipelineTessellationDomainOriginStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineTessellationDomainOriginStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        unsafeAddr (VkPipelineTessellationDomainOriginStateCreateInfo## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineTessellationDomainOriginStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineTessellationDomainOriginStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        type StructFields VkPipelineTessellationDomainOriginStateCreateInfo
             = '["sType", "pNext", "domainOrigin"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineTessellationDomainOriginStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPipelineTessellationDomainOriginStateCreateInfo
             = '[VkPipelineTessellationStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineTessellationDomainOriginStateCreateInfo
         where
        type FieldType "sType"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineTessellationDomainOriginStateCreateInfo
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType}
        type FieldIsArray "sType"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineTessellationDomainOriginStateCreateInfo
         where
        type FieldType "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfo
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext}
        type FieldIsArray "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        type FieldType "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = VkTessellationDomainOrigin
        type FieldOptional "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfo
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin}
        type FieldIsArray "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin}

instance {-# OVERLAPPING #-}
         CanReadField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin}

instance {-# OVERLAPPING #-}
         CanWriteField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin}

instance Show VkPipelineTessellationDomainOriginStateCreateInfo
         where
        showsPrec d x
          = showString "VkPipelineTessellationDomainOriginStateCreateInfo {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "domainOrigin = " .
                            showsPrec d (getField @"domainOrigin" x) . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineTessellationStateCreateInfo VkPipelineTessellationStateCreateInfo registry at www.khronos.org>
data VkPipelineTessellationStateCreateInfo = VkPipelineTessellationStateCreateInfo## Addr##
                                                                                    ByteArray##

instance Eq VkPipelineTessellationStateCreateInfo where
        (VkPipelineTessellationStateCreateInfo## a _) ==
          x@(VkPipelineTessellationStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineTessellationStateCreateInfo where
        (VkPipelineTessellationStateCreateInfo## a _) `compare`
          x@(VkPipelineTessellationStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineTessellationStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineTessellationStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineTessellationStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineTessellationStateCreateInfo
         where
        unsafeAddr (VkPipelineTessellationStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineTessellationStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineTessellationStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineTessellationStateCreateInfo where
        type StructFields VkPipelineTessellationStateCreateInfo =
             '["sType", "pNext", "flags", "patchControlPoints"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineTessellationStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineTessellationStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineTessellationStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineTessellationStateCreateInfo where
        type FieldType "sType" VkPipelineTessellationStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineTessellationStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineTessellationStateCreateInfo =
             #{offset VkPipelineTessellationStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineTessellationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineTessellationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineTessellationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineTessellationStateCreateInfo where
        type FieldType "pNext" VkPipelineTessellationStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineTessellationStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineTessellationStateCreateInfo =
             #{offset VkPipelineTessellationStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineTessellationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineTessellationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineTessellationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineTessellationStateCreateInfo where
        type FieldType "flags" VkPipelineTessellationStateCreateInfo =
             VkPipelineTessellationStateCreateFlags
        type FieldOptional "flags" VkPipelineTessellationStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineTessellationStateCreateInfo =
             #{offset VkPipelineTessellationStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineTessellationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineTessellationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineTessellationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "patchControlPoints" VkPipelineTessellationStateCreateInfo
         where
        type FieldType "patchControlPoints"
               VkPipelineTessellationStateCreateInfo
             = Word32
        type FieldOptional "patchControlPoints"
               VkPipelineTessellationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "patchControlPoints"
               VkPipelineTessellationStateCreateInfo
             =
             #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}
        type FieldIsArray "patchControlPoints"
               VkPipelineTessellationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}

instance {-# OVERLAPPING #-}
         CanReadField "patchControlPoints"
           VkPipelineTessellationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}

instance {-# OVERLAPPING #-}
         CanWriteField "patchControlPoints"
           VkPipelineTessellationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}

instance Show VkPipelineTessellationStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineTessellationStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "patchControlPoints = " .
                                  showsPrec d (getField @"patchControlPoints" x) . showChar '}'

-- | > typedef struct VkPipelineVertexInputDivisorStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     uint32_t                            vertexBindingDivisorCount;
--   >     const VkVertexInputBindingDivisorDescriptionEXT*      pVertexBindingDivisors;
--   > } VkPipelineVertexInputDivisorStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineVertexInputDivisorStateCreateInfoEXT VkPipelineVertexInputDivisorStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineVertexInputDivisorStateCreateInfoEXT = VkPipelineVertexInputDivisorStateCreateInfoEXT## Addr##
                                                                                                      ByteArray##

instance Eq VkPipelineVertexInputDivisorStateCreateInfoEXT where
        (VkPipelineVertexInputDivisorStateCreateInfoEXT## a _) ==
          x@(VkPipelineVertexInputDivisorStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineVertexInputDivisorStateCreateInfoEXT where
        (VkPipelineVertexInputDivisorStateCreateInfoEXT## a _) `compare`
          x@(VkPipelineVertexInputDivisorStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineVertexInputDivisorStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineVertexInputDivisorStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        unsafeAddr (VkPipelineVertexInputDivisorStateCreateInfoEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineVertexInputDivisorStateCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineVertexInputDivisorStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        type StructFields VkPipelineVertexInputDivisorStateCreateInfoEXT =
             '["sType", "pNext", "vertexBindingDivisorCount", -- ' closing tick for hsc2hs
               "pVertexBindingDivisors"]
        type CUnionType VkPipelineVertexInputDivisorStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineVertexInputDivisorStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineVertexInputDivisorStateCreateInfoEXT =
             '[VkPipelineVertexInputStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        type FieldType "sType"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             =
             #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        type FieldType "pNext"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             =
             #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "vertexBindingDivisorCount"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        type FieldType "vertexBindingDivisorCount"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = Word32
        type FieldOptional "vertexBindingDivisorCount"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "vertexBindingDivisorCount"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             =
             #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount}
        type FieldIsArray "vertexBindingDivisorCount"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount}

instance {-# OVERLAPPING #-}
         CanReadField "vertexBindingDivisorCount"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "vertexBindingDivisorCount"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount}

instance {-# OVERLAPPING #-}
         HasField "pVertexBindingDivisors"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        type FieldType "pVertexBindingDivisors"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = Ptr VkVertexInputBindingDivisorDescriptionEXT
        type FieldOptional "pVertexBindingDivisors"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pVertexBindingDivisors"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             =
             #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors}
        type FieldIsArray "pVertexBindingDivisors"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors}

instance {-# OVERLAPPING #-}
         CanReadField "pVertexBindingDivisors"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors}

instance {-# OVERLAPPING #-}
         CanWriteField "pVertexBindingDivisors"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors}

instance Show VkPipelineVertexInputDivisorStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineVertexInputDivisorStateCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "vertexBindingDivisorCount = " .
                            showsPrec d (getField @"vertexBindingDivisorCount" x) .
                              showString ", " .
                                showString "pVertexBindingDivisors = " .
                                  showsPrec d (getField @"pVertexBindingDivisors" x) . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineVertexInputStateCreateInfo VkPipelineVertexInputStateCreateInfo registry at www.khronos.org>
data VkPipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo## Addr##
                                                                                  ByteArray##

instance Eq VkPipelineVertexInputStateCreateInfo where
        (VkPipelineVertexInputStateCreateInfo## a _) ==
          x@(VkPipelineVertexInputStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineVertexInputStateCreateInfo where
        (VkPipelineVertexInputStateCreateInfo## a _) `compare`
          x@(VkPipelineVertexInputStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineVertexInputStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineVertexInputStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineVertexInputStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineVertexInputStateCreateInfo
         where
        unsafeAddr (VkPipelineVertexInputStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineVertexInputStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineVertexInputStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineVertexInputStateCreateInfo where
        type StructFields VkPipelineVertexInputStateCreateInfo =
             '["sType", "pNext", "flags", "vertexBindingDescriptionCount", -- ' closing tick for hsc2hs
               "pVertexBindingDescriptions", "vertexAttributeDescriptionCount",
               "pVertexAttributeDescriptions"]
        type CUnionType VkPipelineVertexInputStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineVertexInputStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineVertexInputStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineVertexInputStateCreateInfo where
        type FieldType "sType" VkPipelineVertexInputStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineVertexInputStateCreateInfo =
             #{offset VkPipelineVertexInputStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineVertexInputStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineVertexInputStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineVertexInputStateCreateInfo where
        type FieldType "pNext" VkPipelineVertexInputStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineVertexInputStateCreateInfo =
             #{offset VkPipelineVertexInputStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineVertexInputStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineVertexInputStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineVertexInputStateCreateInfo where
        type FieldType "flags" VkPipelineVertexInputStateCreateInfo =
             VkPipelineVertexInputStateCreateFlags
        type FieldOptional "flags" VkPipelineVertexInputStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineVertexInputStateCreateInfo =
             #{offset VkPipelineVertexInputStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineVertexInputStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineVertexInputStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "vertexBindingDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = Word32
        type FieldOptional "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}
        type FieldIsArray "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}

instance {-# OVERLAPPING #-}
         CanReadField "vertexBindingDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}

instance {-# OVERLAPPING #-}
         CanWriteField "vertexBindingDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}

instance {-# OVERLAPPING #-}
         HasField "pVertexBindingDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = Ptr VkVertexInputBindingDescription
        type FieldOptional "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}
        type FieldIsArray "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}

instance {-# OVERLAPPING #-}
         CanReadField "pVertexBindingDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}

instance {-# OVERLAPPING #-}
         CanWriteField "pVertexBindingDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}

instance {-# OVERLAPPING #-}
         HasField "vertexAttributeDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = Word32
        type FieldOptional "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}
        type FieldIsArray "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}

instance {-# OVERLAPPING #-}
         CanReadField "vertexAttributeDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}

instance {-# OVERLAPPING #-}
         CanWriteField "vertexAttributeDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}

instance {-# OVERLAPPING #-}
         HasField "pVertexAttributeDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = Ptr VkVertexInputAttributeDescription
        type FieldOptional "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}
        type FieldIsArray "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}

instance {-# OVERLAPPING #-}
         CanReadField "pVertexAttributeDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}

instance {-# OVERLAPPING #-}
         CanWriteField "pVertexAttributeDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}

instance Show VkPipelineVertexInputStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineVertexInputStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "vertexBindingDescriptionCount = " .
                                  showsPrec d (getField @"vertexBindingDescriptionCount" x) .
                                    showString ", " .
                                      showString "pVertexBindingDescriptions = " .
                                        showsPrec d (getField @"pVertexBindingDescriptions" x) .
                                          showString ", " .
                                            showString "vertexAttributeDescriptionCount = " .
                                              showsPrec d
                                                (getField @"vertexAttributeDescriptionCount" x)
                                                .
                                                showString ", " .
                                                  showString "pVertexAttributeDescriptions = " .
                                                    showsPrec d
                                                      (getField @"pVertexAttributeDescriptions" x)
                                                      . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineViewportStateCreateInfo VkPipelineViewportStateCreateInfo registry at www.khronos.org>
data VkPipelineViewportStateCreateInfo = VkPipelineViewportStateCreateInfo## Addr##
                                                                            ByteArray##

instance Eq VkPipelineViewportStateCreateInfo where
        (VkPipelineViewportStateCreateInfo## a _) ==
          x@(VkPipelineViewportStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineViewportStateCreateInfo where
        (VkPipelineViewportStateCreateInfo## a _) `compare`
          x@(VkPipelineViewportStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineViewportStateCreateInfo where
        sizeOf ~_ = #{size VkPipelineViewportStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineViewportStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineViewportStateCreateInfo where
        unsafeAddr (VkPipelineViewportStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineViewportStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineViewportStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineViewportStateCreateInfo where
        type StructFields VkPipelineViewportStateCreateInfo =
             '["sType", "pNext", "flags", "viewportCount", "pViewports", -- ' closing tick for hsc2hs
               "scissorCount", "pScissors"]
        type CUnionType VkPipelineViewportStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineViewportStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineViewportStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineViewportStateCreateInfo where
        type FieldType "sType" VkPipelineViewportStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineViewportStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineViewportStateCreateInfo where
        type FieldType "pNext" VkPipelineViewportStateCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineViewportStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineViewportStateCreateInfo where
        type FieldType "flags" VkPipelineViewportStateCreateInfo =
             VkPipelineViewportStateCreateFlags
        type FieldOptional "flags" VkPipelineViewportStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineViewportStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "viewportCount" VkPipelineViewportStateCreateInfo where
        type FieldType "viewportCount" VkPipelineViewportStateCreateInfo =
             Word32
        type FieldOptional "viewportCount"
               VkPipelineViewportStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportCount" VkPipelineViewportStateCreateInfo
             =
             #{offset VkPipelineViewportStateCreateInfo, viewportCount}
        type FieldIsArray "viewportCount" VkPipelineViewportStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, viewportCount}

instance {-# OVERLAPPING #-}
         CanReadField "viewportCount" VkPipelineViewportStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, viewportCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, viewportCount}

instance {-# OVERLAPPING #-}
         CanWriteField "viewportCount" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, viewportCount}

instance {-# OVERLAPPING #-}
         HasField "pViewports" VkPipelineViewportStateCreateInfo where
        type FieldType "pViewports" VkPipelineViewportStateCreateInfo =
             Ptr VkViewport
        type FieldOptional "pViewports" VkPipelineViewportStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pViewports" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, pViewports}
        type FieldIsArray "pViewports" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, pViewports}

instance {-# OVERLAPPING #-}
         CanReadField "pViewports" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, pViewports})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, pViewports}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewports" VkPipelineViewportStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, pViewports}

instance {-# OVERLAPPING #-}
         HasField "scissorCount" VkPipelineViewportStateCreateInfo where
        type FieldType "scissorCount" VkPipelineViewportStateCreateInfo =
             Word32
        type FieldOptional "scissorCount" VkPipelineViewportStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "scissorCount" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, scissorCount}
        type FieldIsArray "scissorCount" VkPipelineViewportStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, scissorCount}

instance {-# OVERLAPPING #-}
         CanReadField "scissorCount" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, scissorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, scissorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "scissorCount" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, scissorCount}

instance {-# OVERLAPPING #-}
         HasField "pScissors" VkPipelineViewportStateCreateInfo where
        type FieldType "pScissors" VkPipelineViewportStateCreateInfo =
             Ptr VkRect2D
        type FieldOptional "pScissors" VkPipelineViewportStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pScissors" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, pScissors}
        type FieldIsArray "pScissors" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, pScissors}

instance {-# OVERLAPPING #-}
         CanReadField "pScissors" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, pScissors})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, pScissors}

instance {-# OVERLAPPING #-}
         CanWriteField "pScissors" VkPipelineViewportStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, pScissors}

instance Show VkPipelineViewportStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineViewportStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "viewportCount = " .
                                  showsPrec d (getField @"viewportCount" x) .
                                    showString ", " .
                                      showString "pViewports = " .
                                        showsPrec d (getField @"pViewports" x) .
                                          showString ", " .
                                            showString "scissorCount = " .
                                              showsPrec d (getField @"scissorCount" x) .
                                                showString ", " .
                                                  showString "pScissors = " .
                                                    showsPrec d (getField @"pScissors" x) .
                                                      showChar '}'

-- | > typedef struct VkPipelineViewportSwizzleStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineViewportSwizzleStateCreateFlagsNV    flags;
--   >     uint32_t               viewportCount;
--   >     const VkViewportSwizzleNV*      pViewportSwizzles;
--   > } VkPipelineViewportSwizzleStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineViewportSwizzleStateCreateInfoNV VkPipelineViewportSwizzleStateCreateInfoNV registry at www.khronos.org>
data VkPipelineViewportSwizzleStateCreateInfoNV = VkPipelineViewportSwizzleStateCreateInfoNV## Addr##
                                                                                              ByteArray##

instance Eq VkPipelineViewportSwizzleStateCreateInfoNV where
        (VkPipelineViewportSwizzleStateCreateInfoNV## a _) ==
          x@(VkPipelineViewportSwizzleStateCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineViewportSwizzleStateCreateInfoNV where
        (VkPipelineViewportSwizzleStateCreateInfoNV## a _) `compare`
          x@(VkPipelineViewportSwizzleStateCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineViewportSwizzleStateCreateInfoNV where
        sizeOf ~_
          = #{size VkPipelineViewportSwizzleStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineViewportSwizzleStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        unsafeAddr (VkPipelineViewportSwizzleStateCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineViewportSwizzleStateCreateInfoNV## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineViewportSwizzleStateCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineViewportSwizzleStateCreateInfoNV
         where
        type StructFields VkPipelineViewportSwizzleStateCreateInfoNV =
             '["sType", "pNext", "flags", "viewportCount", "pViewportSwizzles"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineViewportSwizzleStateCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineViewportSwizzleStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineViewportSwizzleStateCreateInfoNV =
             '[VkPipelineViewportStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineViewportSwizzleStateCreateInfoNV where
        type FieldType "sType" VkPipelineViewportSwizzleStateCreateInfoNV =
             VkStructureType
        type FieldOptional "sType"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineViewportSwizzleStateCreateInfoNV
             =
             #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}
        type FieldIsArray "sType"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineViewportSwizzleStateCreateInfoNV where
        type FieldType "pNext" VkPipelineViewportSwizzleStateCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineViewportSwizzleStateCreateInfoNV
             =
             #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}
        type FieldIsArray "pNext"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineViewportSwizzleStateCreateInfoNV where
        type FieldType "flags" VkPipelineViewportSwizzleStateCreateInfoNV =
             VkPipelineViewportSwizzleStateCreateFlagsNV
        type FieldOptional "flags"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineViewportSwizzleStateCreateInfoNV
             =
             #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}
        type FieldIsArray "flags"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         HasField "viewportCount" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        type FieldType "viewportCount"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = Word32
        type FieldOptional "viewportCount"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportCount"
               VkPipelineViewportSwizzleStateCreateInfoNV
             =
             #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}
        type FieldIsArray "viewportCount"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         CanReadField "viewportCount"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         CanWriteField "viewportCount"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         HasField "pViewportSwizzles"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        type FieldType "pViewportSwizzles"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = Ptr VkViewportSwizzleNV
        type FieldOptional "pViewportSwizzles"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pViewportSwizzles"
               VkPipelineViewportSwizzleStateCreateInfoNV
             =
             #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}
        type FieldIsArray "pViewportSwizzles"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

instance {-# OVERLAPPING #-}
         CanReadField "pViewportSwizzles"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewportSwizzles"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

instance Show VkPipelineViewportSwizzleStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineViewportSwizzleStateCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "viewportCount = " .
                                  showsPrec d (getField @"viewportCount" x) .
                                    showString ", " .
                                      showString "pViewportSwizzles = " .
                                        showsPrec d (getField @"pViewportSwizzles" x) . showChar '}'

-- | > typedef struct VkPipelineViewportWScalingStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32               viewportWScalingEnable;
--   >     uint32_t               viewportCount;
--   >     const VkViewportWScalingNV*      pViewportWScalings;
--   > } VkPipelineViewportWScalingStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineViewportWScalingStateCreateInfoNV VkPipelineViewportWScalingStateCreateInfoNV registry at www.khronos.org>
data VkPipelineViewportWScalingStateCreateInfoNV = VkPipelineViewportWScalingStateCreateInfoNV## Addr##
                                                                                                ByteArray##

instance Eq VkPipelineViewportWScalingStateCreateInfoNV where
        (VkPipelineViewportWScalingStateCreateInfoNV## a _) ==
          x@(VkPipelineViewportWScalingStateCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineViewportWScalingStateCreateInfoNV where
        (VkPipelineViewportWScalingStateCreateInfoNV## a _) `compare`
          x@(VkPipelineViewportWScalingStateCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineViewportWScalingStateCreateInfoNV where
        sizeOf ~_
          = #{size VkPipelineViewportWScalingStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineViewportWScalingStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        unsafeAddr (VkPipelineViewportWScalingStateCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineViewportWScalingStateCreateInfoNV## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineViewportWScalingStateCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineViewportWScalingStateCreateInfoNV
         where
        type StructFields VkPipelineViewportWScalingStateCreateInfoNV =
             '["sType", "pNext", "viewportWScalingEnable", "viewportCount", -- ' closing tick for hsc2hs
               "pViewportWScalings"]
        type CUnionType VkPipelineViewportWScalingStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineViewportWScalingStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineViewportWScalingStateCreateInfoNV =
             '[VkPipelineViewportStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineViewportWScalingStateCreateInfoNV where
        type FieldType "sType" VkPipelineViewportWScalingStateCreateInfoNV
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}
        type FieldIsArray "sType"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineViewportWScalingStateCreateInfoNV where
        type FieldType "pNext" VkPipelineViewportWScalingStateCreateInfoNV
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}
        type FieldIsArray "pNext"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "viewportWScalingEnable"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type FieldType "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             = VkBool32
        type FieldOptional "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}
        type FieldIsArray "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

instance {-# OVERLAPPING #-}
         CanReadField "viewportWScalingEnable"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "viewportWScalingEnable"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

instance {-# OVERLAPPING #-}
         HasField "viewportCount"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type FieldType "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             = Word32
        type FieldOptional "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}
        type FieldIsArray "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         CanReadField "viewportCount"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         CanWriteField "viewportCount"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         HasField "pViewportWScalings"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type FieldType "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             = Ptr VkViewportWScalingNV
        type FieldOptional "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}
        type FieldIsArray "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

instance {-# OVERLAPPING #-}
         CanReadField "pViewportWScalings"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewportWScalings"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

instance Show VkPipelineViewportWScalingStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineViewportWScalingStateCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "viewportWScalingEnable = " .
                            showsPrec d (getField @"viewportWScalingEnable" x) .
                              showString ", " .
                                showString "viewportCount = " .
                                  showsPrec d (getField @"viewportCount" x) .
                                    showString ", " .
                                      showString "pViewportWScalings = " .
                                        showsPrec d (getField @"pViewportWScalings" x) .
                                          showChar '}'
