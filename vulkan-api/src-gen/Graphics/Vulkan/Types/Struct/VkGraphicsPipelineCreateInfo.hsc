#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkGraphicsPipelineCreateInfo
       (VkGraphicsPipelineCreateInfo(..)) where
import           Foreign.Storable
                                                                                      (Storable (..))
import           GHC.Base
                                                                                      (Addr##,
                                                                                      ByteArray##,
                                                                                      byteArrayContents##,
                                                                                      plusAddr##)
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
