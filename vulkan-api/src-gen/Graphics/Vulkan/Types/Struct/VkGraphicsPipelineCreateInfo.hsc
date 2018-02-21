#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkGraphicsPipelineCreateInfo
       (VkGraphicsPipelineCreateInfo(..)) where
import           Foreign.Storable
                                                                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkPipelineCreateFlags
                                                                                      (VkPipelineCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                      (VkStructureType)
import           Graphics.Vulkan.Types.Handles
                                                                                      (VkPipeline,
                                                                                      VkPipelineLayout,
                                                                                      VkRenderPass)
import           Graphics.Vulkan.Types.Struct.VkPipelineColorBlendStateCreateInfo
                                                                                      (VkPipelineColorBlendStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPipelineDepthStencilStateCreateInfo
                                                                                      (VkPipelineDepthStencilStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPipelineDynamicStateCreateInfo
                                                                                      (VkPipelineDynamicStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPipelineInputAssemblyStateCreateInfo
                                                                                      (VkPipelineInputAssemblyStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo
                                                                                      (VkPipelineMultisampleStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo
                                                                                      (VkPipelineRasterizationStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPipelineShaderStageCreateInfo
                                                                                      (VkPipelineShaderStageCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo
                                                                                      (VkPipelineTessellationStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPipelineVertexInputStateCreateInfo
                                                                                      (VkPipelineVertexInputStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo
                                                                                      (VkPipelineViewportStateCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                      (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkGraphicsPipelineCreateInfo.html VkGraphicsPipelineCreateInfo registry at www.khronos.org>
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
         HasVkSType VkGraphicsPipelineCreateInfo where
        type VkSTypeMType VkGraphicsPipelineCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, sType}

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

instance CanReadField "sType" VkGraphicsPipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkGraphicsPipelineCreateInfo where
        type VkPNextMType VkGraphicsPipelineCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pNext}

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

instance CanReadField "pNext" VkGraphicsPipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkGraphicsPipelineCreateInfo where
        type VkFlagsMType VkGraphicsPipelineCreateInfo =
             VkPipelineCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, flags}

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

instance CanReadField "flags" VkGraphicsPipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkStageCount VkGraphicsPipelineCreateInfo where
        type VkStageCountMType VkGraphicsPipelineCreateInfo = Word32

        {-# NOINLINE vkStageCount #-}
        vkStageCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, stageCount})

        {-# INLINE vkStageCountByteOffset #-}
        vkStageCountByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, stageCount}

        {-# INLINE readVkStageCount #-}
        readVkStageCount p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, stageCount}

        {-# INLINE writeVkStageCount #-}
        writeVkStageCount p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, stageCount}

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

instance CanReadField "stageCount" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkStageCount

        {-# INLINE readField #-}
        readField = readVkStageCount

instance CanWriteField "stageCount" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkStageCount

instance {-# OVERLAPPING #-}
         HasVkPStages VkGraphicsPipelineCreateInfo where
        type VkPStagesMType VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineShaderStageCreateInfo

        {-# NOINLINE vkPStages #-}
        vkPStages x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pStages})

        {-# INLINE vkPStagesByteOffset #-}
        vkPStagesByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pStages}

        {-# INLINE readVkPStages #-}
        readVkPStages p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pStages}

        {-# INLINE writeVkPStages #-}
        writeVkPStages p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pStages}

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

instance CanReadField "pStages" VkGraphicsPipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkPStages

        {-# INLINE readField #-}
        readField = readVkPStages

instance CanWriteField "pStages" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPStages

instance {-# OVERLAPPING #-}
         HasVkPVertexInputState VkGraphicsPipelineCreateInfo where
        type VkPVertexInputStateMType VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineVertexInputStateCreateInfo

        {-# NOINLINE vkPVertexInputState #-}
        vkPVertexInputState x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pVertexInputState})

        {-# INLINE vkPVertexInputStateByteOffset #-}
        vkPVertexInputStateByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pVertexInputState}

        {-# INLINE readVkPVertexInputState #-}
        readVkPVertexInputState p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pVertexInputState}

        {-# INLINE writeVkPVertexInputState #-}
        writeVkPVertexInputState p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pVertexInputState}

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

instance CanReadField "pVertexInputState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPVertexInputState

        {-# INLINE readField #-}
        readField = readVkPVertexInputState

instance CanWriteField "pVertexInputState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPVertexInputState

instance {-# OVERLAPPING #-}
         HasVkPInputAssemblyState VkGraphicsPipelineCreateInfo where
        type VkPInputAssemblyStateMType VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineInputAssemblyStateCreateInfo

        {-# NOINLINE vkPInputAssemblyState #-}
        vkPInputAssemblyState x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pInputAssemblyState})

        {-# INLINE vkPInputAssemblyStateByteOffset #-}
        vkPInputAssemblyStateByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pInputAssemblyState}

        {-# INLINE readVkPInputAssemblyState #-}
        readVkPInputAssemblyState p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pInputAssemblyState}

        {-# INLINE writeVkPInputAssemblyState #-}
        writeVkPInputAssemblyState p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pInputAssemblyState}

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

instance CanReadField "pInputAssemblyState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPInputAssemblyState

        {-# INLINE readField #-}
        readField = readVkPInputAssemblyState

instance CanWriteField "pInputAssemblyState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPInputAssemblyState

instance {-# OVERLAPPING #-}
         HasVkPTessellationState VkGraphicsPipelineCreateInfo where
        type VkPTessellationStateMType VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineTessellationStateCreateInfo

        {-# NOINLINE vkPTessellationState #-}
        vkPTessellationState x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pTessellationState})

        {-# INLINE vkPTessellationStateByteOffset #-}
        vkPTessellationStateByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pTessellationState}

        {-# INLINE readVkPTessellationState #-}
        readVkPTessellationState p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pTessellationState}

        {-# INLINE writeVkPTessellationState #-}
        writeVkPTessellationState p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pTessellationState}

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

instance CanReadField "pTessellationState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPTessellationState

        {-# INLINE readField #-}
        readField = readVkPTessellationState

instance CanWriteField "pTessellationState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPTessellationState

instance {-# OVERLAPPING #-}
         HasVkPViewportState VkGraphicsPipelineCreateInfo where
        type VkPViewportStateMType VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineViewportStateCreateInfo

        {-# NOINLINE vkPViewportState #-}
        vkPViewportState x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pViewportState})

        {-# INLINE vkPViewportStateByteOffset #-}
        vkPViewportStateByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pViewportState}

        {-# INLINE readVkPViewportState #-}
        readVkPViewportState p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pViewportState}

        {-# INLINE writeVkPViewportState #-}
        writeVkPViewportState p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pViewportState}

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

instance CanReadField "pViewportState" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPViewportState

        {-# INLINE readField #-}
        readField = readVkPViewportState

instance CanWriteField "pViewportState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPViewportState

instance {-# OVERLAPPING #-}
         HasVkPRasterizationState VkGraphicsPipelineCreateInfo where
        type VkPRasterizationStateMType VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineRasterizationStateCreateInfo

        {-# NOINLINE vkPRasterizationState #-}
        vkPRasterizationState x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pRasterizationState})

        {-# INLINE vkPRasterizationStateByteOffset #-}
        vkPRasterizationStateByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pRasterizationState}

        {-# INLINE readVkPRasterizationState #-}
        readVkPRasterizationState p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pRasterizationState}

        {-# INLINE writeVkPRasterizationState #-}
        writeVkPRasterizationState p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pRasterizationState}

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

instance CanReadField "pRasterizationState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPRasterizationState

        {-# INLINE readField #-}
        readField = readVkPRasterizationState

instance CanWriteField "pRasterizationState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPRasterizationState

instance {-# OVERLAPPING #-}
         HasVkPMultisampleState VkGraphicsPipelineCreateInfo where
        type VkPMultisampleStateMType VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineMultisampleStateCreateInfo

        {-# NOINLINE vkPMultisampleState #-}
        vkPMultisampleState x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pMultisampleState})

        {-# INLINE vkPMultisampleStateByteOffset #-}
        vkPMultisampleStateByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pMultisampleState}

        {-# INLINE readVkPMultisampleState #-}
        readVkPMultisampleState p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pMultisampleState}

        {-# INLINE writeVkPMultisampleState #-}
        writeVkPMultisampleState p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pMultisampleState}

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

instance CanReadField "pMultisampleState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPMultisampleState

        {-# INLINE readField #-}
        readField = readVkPMultisampleState

instance CanWriteField "pMultisampleState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPMultisampleState

instance {-# OVERLAPPING #-}
         HasVkPDepthStencilState VkGraphicsPipelineCreateInfo where
        type VkPDepthStencilStateMType VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineDepthStencilStateCreateInfo

        {-# NOINLINE vkPDepthStencilState #-}
        vkPDepthStencilState x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pDepthStencilState})

        {-# INLINE vkPDepthStencilStateByteOffset #-}
        vkPDepthStencilStateByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pDepthStencilState}

        {-# INLINE readVkPDepthStencilState #-}
        readVkPDepthStencilState p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pDepthStencilState}

        {-# INLINE writeVkPDepthStencilState #-}
        writeVkPDepthStencilState p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pDepthStencilState}

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

instance CanReadField "pDepthStencilState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPDepthStencilState

        {-# INLINE readField #-}
        readField = readVkPDepthStencilState

instance CanWriteField "pDepthStencilState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDepthStencilState

instance {-# OVERLAPPING #-}
         HasVkPColorBlendState VkGraphicsPipelineCreateInfo where
        type VkPColorBlendStateMType VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineColorBlendStateCreateInfo

        {-# NOINLINE vkPColorBlendState #-}
        vkPColorBlendState x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pColorBlendState})

        {-# INLINE vkPColorBlendStateByteOffset #-}
        vkPColorBlendStateByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pColorBlendState}

        {-# INLINE readVkPColorBlendState #-}
        readVkPColorBlendState p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pColorBlendState}

        {-# INLINE writeVkPColorBlendState #-}
        writeVkPColorBlendState p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pColorBlendState}

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

instance CanReadField "pColorBlendState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPColorBlendState

        {-# INLINE readField #-}
        readField = readVkPColorBlendState

instance CanWriteField "pColorBlendState"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPColorBlendState

instance {-# OVERLAPPING #-}
         HasVkPDynamicState VkGraphicsPipelineCreateInfo where
        type VkPDynamicStateMType VkGraphicsPipelineCreateInfo =
             Ptr VkPipelineDynamicStateCreateInfo

        {-# NOINLINE vkPDynamicState #-}
        vkPDynamicState x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, pDynamicState})

        {-# INLINE vkPDynamicStateByteOffset #-}
        vkPDynamicStateByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, pDynamicState}

        {-# INLINE readVkPDynamicState #-}
        readVkPDynamicState p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, pDynamicState}

        {-# INLINE writeVkPDynamicState #-}
        writeVkPDynamicState p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, pDynamicState}

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

instance CanReadField "pDynamicState" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPDynamicState

        {-# INLINE readField #-}
        readField = readVkPDynamicState

instance CanWriteField "pDynamicState" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDynamicState

instance {-# OVERLAPPING #-}
         HasVkLayout VkGraphicsPipelineCreateInfo where
        type VkLayoutMType VkGraphicsPipelineCreateInfo = VkPipelineLayout

        {-# NOINLINE vkLayout #-}
        vkLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, layout})

        {-# INLINE vkLayoutByteOffset #-}
        vkLayoutByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, layout}

        {-# INLINE readVkLayout #-}
        readVkLayout p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, layout}

        {-# INLINE writeVkLayout #-}
        writeVkLayout p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, layout}

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

instance CanReadField "layout" VkGraphicsPipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkLayout

        {-# INLINE readField #-}
        readField = readVkLayout

instance CanWriteField "layout" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkLayout

instance {-# OVERLAPPING #-}
         HasVkRenderPass VkGraphicsPipelineCreateInfo where
        type VkRenderPassMType VkGraphicsPipelineCreateInfo = VkRenderPass

        {-# NOINLINE vkRenderPass #-}
        vkRenderPass x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, renderPass})

        {-# INLINE vkRenderPassByteOffset #-}
        vkRenderPassByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, renderPass}

        {-# INLINE readVkRenderPass #-}
        readVkRenderPass p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, renderPass}

        {-# INLINE writeVkRenderPass #-}
        writeVkRenderPass p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, renderPass}

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

instance CanReadField "renderPass" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkRenderPass

        {-# INLINE readField #-}
        readField = readVkRenderPass

instance CanWriteField "renderPass" VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkRenderPass

instance {-# OVERLAPPING #-}
         HasVkSubpass VkGraphicsPipelineCreateInfo where
        type VkSubpassMType VkGraphicsPipelineCreateInfo = Word32

        {-# NOINLINE vkSubpass #-}
        vkSubpass x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, subpass})

        {-# INLINE vkSubpassByteOffset #-}
        vkSubpassByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, subpass}

        {-# INLINE readVkSubpass #-}
        readVkSubpass p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, subpass}

        {-# INLINE writeVkSubpass #-}
        writeVkSubpass p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, subpass}

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

instance CanReadField "subpass" VkGraphicsPipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkSubpass

        {-# INLINE readField #-}
        readField = readVkSubpass

instance CanWriteField "subpass" VkGraphicsPipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSubpass

instance {-# OVERLAPPING #-}
         HasVkBasePipelineHandle VkGraphicsPipelineCreateInfo where
        type VkBasePipelineHandleMType VkGraphicsPipelineCreateInfo =
             VkPipeline

        {-# NOINLINE vkBasePipelineHandle #-}
        vkBasePipelineHandle x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, basePipelineHandle})

        {-# INLINE vkBasePipelineHandleByteOffset #-}
        vkBasePipelineHandleByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, basePipelineHandle}

        {-# INLINE readVkBasePipelineHandle #-}
        readVkBasePipelineHandle p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, basePipelineHandle}

        {-# INLINE writeVkBasePipelineHandle #-}
        writeVkBasePipelineHandle p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, basePipelineHandle}

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

instance CanReadField "basePipelineHandle"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkBasePipelineHandle

        {-# INLINE readField #-}
        readField = readVkBasePipelineHandle

instance CanWriteField "basePipelineHandle"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkBasePipelineHandle

instance {-# OVERLAPPING #-}
         HasVkBasePipelineIndex VkGraphicsPipelineCreateInfo where
        type VkBasePipelineIndexMType VkGraphicsPipelineCreateInfo = Int32

        {-# NOINLINE vkBasePipelineIndex #-}
        vkBasePipelineIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkGraphicsPipelineCreateInfo, basePipelineIndex})

        {-# INLINE vkBasePipelineIndexByteOffset #-}
        vkBasePipelineIndexByteOffset ~_
          = #{offset VkGraphicsPipelineCreateInfo, basePipelineIndex}

        {-# INLINE readVkBasePipelineIndex #-}
        readVkBasePipelineIndex p
          = peekByteOff p #{offset VkGraphicsPipelineCreateInfo, basePipelineIndex}

        {-# INLINE writeVkBasePipelineIndex #-}
        writeVkBasePipelineIndex p
          = pokeByteOff p #{offset VkGraphicsPipelineCreateInfo, basePipelineIndex}

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

instance CanReadField "basePipelineIndex"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkBasePipelineIndex

        {-# INLINE readField #-}
        readField = readVkBasePipelineIndex

instance CanWriteField "basePipelineIndex"
           VkGraphicsPipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkBasePipelineIndex

instance Show VkGraphicsPipelineCreateInfo where
        showsPrec d x
          = showString "VkGraphicsPipelineCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkStageCount = " .
                                  showsPrec d (vkStageCount x) .
                                    showString ", " .
                                      showString "vkPStages = " .
                                        showsPrec d (vkPStages x) .
                                          showString ", " .
                                            showString "vkPVertexInputState = " .
                                              showsPrec d (vkPVertexInputState x) .
                                                showString ", " .
                                                  showString "vkPInputAssemblyState = " .
                                                    showsPrec d (vkPInputAssemblyState x) .
                                                      showString ", " .
                                                        showString "vkPTessellationState = " .
                                                          showsPrec d (vkPTessellationState x) .
                                                            showString ", " .
                                                              showString "vkPViewportState = " .
                                                                showsPrec d (vkPViewportState x) .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkPRasterizationState = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkPRasterizationState x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkPMultisampleState = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkPMultisampleState
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkPDepthStencilState = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkPDepthStencilState
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "vkPColorBlendState = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (vkPColorBlendState
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "vkPDynamicState = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (vkPDynamicState
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "vkLayout = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (vkLayout
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "vkRenderPass = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (vkRenderPass
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "vkSubpass = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (vkSubpass
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "vkBasePipelineHandle = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (vkBasePipelineHandle
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showString
                                                                                                                          ", "
                                                                                                                          .
                                                                                                                          showString
                                                                                                                            "vkBasePipelineIndex = "
                                                                                                                            .
                                                                                                                            showsPrec
                                                                                                                              d
                                                                                                                              (vkBasePipelineIndex
                                                                                                                                 x)
                                                                                                                              .
                                                                                                                              showChar
                                                                                                                                '}'
