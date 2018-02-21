#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo
       (VkCommandBufferInheritanceInfo(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkQueryControlFlags           (VkQueryControlFlags)
import           Graphics.Vulkan.Types.Enum.VkQueryPipelineStatisticFlags (VkQueryPipelineStatisticFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Handles                            (VkFramebuffer,
                                                                           VkRenderPass)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkCommandBufferInheritanceInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRenderPass    renderPass;
--   >     uint32_t               subpass;
--   >     VkFramebuffer   framebuffer;
--   >     VkBool32               occlusionQueryEnable;
--   >     VkQueryControlFlags    queryFlags;
--   >     VkQueryPipelineStatisticFlags pipelineStatistics;
--   > } VkCommandBufferInheritanceInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkCommandBufferInheritanceInfo.html VkCommandBufferInheritanceInfo registry at www.khronos.org>
data VkCommandBufferInheritanceInfo = VkCommandBufferInheritanceInfo## Addr##
                                                                      ByteArray##

instance Eq VkCommandBufferInheritanceInfo where
        (VkCommandBufferInheritanceInfo## a _) ==
          x@(VkCommandBufferInheritanceInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandBufferInheritanceInfo where
        (VkCommandBufferInheritanceInfo## a _) `compare`
          x@(VkCommandBufferInheritanceInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandBufferInheritanceInfo where
        sizeOf ~_ = #{size VkCommandBufferInheritanceInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkCommandBufferInheritanceInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandBufferInheritanceInfo where
        unsafeAddr (VkCommandBufferInheritanceInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandBufferInheritanceInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandBufferInheritanceInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandBufferInheritanceInfo where
        type StructFields VkCommandBufferInheritanceInfo =
             '["sType", "pNext", "renderPass", "subpass", "framebuffer", -- ' closing tick for hsc2hs
               "occlusionQueryEnable", "queryFlags", "pipelineStatistics"]
        type CUnionType VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandBufferInheritanceInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkCommandBufferInheritanceInfo where
        type VkSTypeMType VkCommandBufferInheritanceInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkCommandBufferInheritanceInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandBufferInheritanceInfo where
        type FieldType "sType" VkCommandBufferInheritanceInfo =
             VkStructureType
        type FieldOptional "sType" VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, sType}
        type FieldIsArray "sType" VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, sType}

instance CanReadField "sType" VkCommandBufferInheritanceInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkCommandBufferInheritanceInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkCommandBufferInheritanceInfo where
        type VkPNextMType VkCommandBufferInheritanceInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkCommandBufferInheritanceInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandBufferInheritanceInfo where
        type FieldType "pNext" VkCommandBufferInheritanceInfo = Ptr Void
        type FieldOptional "pNext" VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, pNext}
        type FieldIsArray "pNext" VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, pNext}

instance CanReadField "pNext" VkCommandBufferInheritanceInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkCommandBufferInheritanceInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkRenderPass VkCommandBufferInheritanceInfo where
        type VkRenderPassMType VkCommandBufferInheritanceInfo =
             VkRenderPass

        {-# NOINLINE vkRenderPass #-}
        vkRenderPass x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, renderPass})

        {-# INLINE vkRenderPassByteOffset #-}
        vkRenderPassByteOffset ~_
          = #{offset VkCommandBufferInheritanceInfo, renderPass}

        {-# INLINE readVkRenderPass #-}
        readVkRenderPass p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, renderPass}

        {-# INLINE writeVkRenderPass #-}
        writeVkRenderPass p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, renderPass}

instance {-# OVERLAPPING #-}
         HasField "renderPass" VkCommandBufferInheritanceInfo where
        type FieldType "renderPass" VkCommandBufferInheritanceInfo =
             VkRenderPass
        type FieldOptional "renderPass" VkCommandBufferInheritanceInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "renderPass" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, renderPass}
        type FieldIsArray "renderPass" VkCommandBufferInheritanceInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, renderPass}

instance CanReadField "renderPass" VkCommandBufferInheritanceInfo
         where
        {-# INLINE getField #-}
        getField = vkRenderPass

        {-# INLINE readField #-}
        readField = readVkRenderPass

instance CanWriteField "renderPass" VkCommandBufferInheritanceInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkRenderPass

instance {-# OVERLAPPING #-}
         HasVkSubpass VkCommandBufferInheritanceInfo where
        type VkSubpassMType VkCommandBufferInheritanceInfo = Word32

        {-# NOINLINE vkSubpass #-}
        vkSubpass x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, subpass})

        {-# INLINE vkSubpassByteOffset #-}
        vkSubpassByteOffset ~_
          = #{offset VkCommandBufferInheritanceInfo, subpass}

        {-# INLINE readVkSubpass #-}
        readVkSubpass p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, subpass}

        {-# INLINE writeVkSubpass #-}
        writeVkSubpass p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, subpass}

instance {-# OVERLAPPING #-}
         HasField "subpass" VkCommandBufferInheritanceInfo where
        type FieldType "subpass" VkCommandBufferInheritanceInfo = Word32
        type FieldOptional "subpass" VkCommandBufferInheritanceInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subpass" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, subpass}
        type FieldIsArray "subpass" VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, subpass}

instance CanReadField "subpass" VkCommandBufferInheritanceInfo
         where
        {-# INLINE getField #-}
        getField = vkSubpass

        {-# INLINE readField #-}
        readField = readVkSubpass

instance CanWriteField "subpass" VkCommandBufferInheritanceInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubpass

instance {-# OVERLAPPING #-}
         HasVkFramebuffer VkCommandBufferInheritanceInfo where
        type VkFramebufferMType VkCommandBufferInheritanceInfo =
             VkFramebuffer

        {-# NOINLINE vkFramebuffer #-}
        vkFramebuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, framebuffer})

        {-# INLINE vkFramebufferByteOffset #-}
        vkFramebufferByteOffset ~_
          = #{offset VkCommandBufferInheritanceInfo, framebuffer}

        {-# INLINE readVkFramebuffer #-}
        readVkFramebuffer p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, framebuffer}

        {-# INLINE writeVkFramebuffer #-}
        writeVkFramebuffer p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, framebuffer}

instance {-# OVERLAPPING #-}
         HasField "framebuffer" VkCommandBufferInheritanceInfo where
        type FieldType "framebuffer" VkCommandBufferInheritanceInfo =
             VkFramebuffer
        type FieldOptional "framebuffer" VkCommandBufferInheritanceInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "framebuffer" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, framebuffer}
        type FieldIsArray "framebuffer" VkCommandBufferInheritanceInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, framebuffer}

instance CanReadField "framebuffer" VkCommandBufferInheritanceInfo
         where
        {-# INLINE getField #-}
        getField = vkFramebuffer

        {-# INLINE readField #-}
        readField = readVkFramebuffer

instance CanWriteField "framebuffer" VkCommandBufferInheritanceInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFramebuffer

instance {-# OVERLAPPING #-}
         HasVkOcclusionQueryEnable VkCommandBufferInheritanceInfo where
        type VkOcclusionQueryEnableMType VkCommandBufferInheritanceInfo =
             VkBool32

        {-# NOINLINE vkOcclusionQueryEnable #-}
        vkOcclusionQueryEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable})

        {-# INLINE vkOcclusionQueryEnableByteOffset #-}
        vkOcclusionQueryEnableByteOffset ~_
          = #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable}

        {-# INLINE readVkOcclusionQueryEnable #-}
        readVkOcclusionQueryEnable p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable}

        {-# INLINE writeVkOcclusionQueryEnable #-}
        writeVkOcclusionQueryEnable p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable}

instance {-# OVERLAPPING #-}
         HasField "occlusionQueryEnable" VkCommandBufferInheritanceInfo
         where
        type FieldType "occlusionQueryEnable"
               VkCommandBufferInheritanceInfo
             = VkBool32
        type FieldOptional "occlusionQueryEnable"
               VkCommandBufferInheritanceInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "occlusionQueryEnable"
               VkCommandBufferInheritanceInfo
             =
             #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable}
        type FieldIsArray "occlusionQueryEnable"
               VkCommandBufferInheritanceInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable}

instance CanReadField "occlusionQueryEnable"
           VkCommandBufferInheritanceInfo
         where
        {-# INLINE getField #-}
        getField = vkOcclusionQueryEnable

        {-# INLINE readField #-}
        readField = readVkOcclusionQueryEnable

instance CanWriteField "occlusionQueryEnable"
           VkCommandBufferInheritanceInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkOcclusionQueryEnable

instance {-# OVERLAPPING #-}
         HasVkQueryFlags VkCommandBufferInheritanceInfo where
        type VkQueryFlagsMType VkCommandBufferInheritanceInfo =
             VkQueryControlFlags

        {-# NOINLINE vkQueryFlags #-}
        vkQueryFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, queryFlags})

        {-# INLINE vkQueryFlagsByteOffset #-}
        vkQueryFlagsByteOffset ~_
          = #{offset VkCommandBufferInheritanceInfo, queryFlags}

        {-# INLINE readVkQueryFlags #-}
        readVkQueryFlags p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, queryFlags}

        {-# INLINE writeVkQueryFlags #-}
        writeVkQueryFlags p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, queryFlags}

instance {-# OVERLAPPING #-}
         HasField "queryFlags" VkCommandBufferInheritanceInfo where
        type FieldType "queryFlags" VkCommandBufferInheritanceInfo =
             VkQueryControlFlags
        type FieldOptional "queryFlags" VkCommandBufferInheritanceInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "queryFlags" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, queryFlags}
        type FieldIsArray "queryFlags" VkCommandBufferInheritanceInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, queryFlags}

instance CanReadField "queryFlags" VkCommandBufferInheritanceInfo
         where
        {-# INLINE getField #-}
        getField = vkQueryFlags

        {-# INLINE readField #-}
        readField = readVkQueryFlags

instance CanWriteField "queryFlags" VkCommandBufferInheritanceInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkQueryFlags

instance {-# OVERLAPPING #-}
         HasVkPipelineStatistics VkCommandBufferInheritanceInfo where
        type VkPipelineStatisticsMType VkCommandBufferInheritanceInfo =
             VkQueryPipelineStatisticFlags

        {-# NOINLINE vkPipelineStatistics #-}
        vkPipelineStatistics x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, pipelineStatistics})

        {-# INLINE vkPipelineStatisticsByteOffset #-}
        vkPipelineStatisticsByteOffset ~_
          = #{offset VkCommandBufferInheritanceInfo, pipelineStatistics}

        {-# INLINE readVkPipelineStatistics #-}
        readVkPipelineStatistics p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, pipelineStatistics}

        {-# INLINE writeVkPipelineStatistics #-}
        writeVkPipelineStatistics p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, pipelineStatistics}

instance {-# OVERLAPPING #-}
         HasField "pipelineStatistics" VkCommandBufferInheritanceInfo where
        type FieldType "pipelineStatistics" VkCommandBufferInheritanceInfo
             = VkQueryPipelineStatisticFlags
        type FieldOptional "pipelineStatistics"
               VkCommandBufferInheritanceInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pipelineStatistics"
               VkCommandBufferInheritanceInfo
             =
             #{offset VkCommandBufferInheritanceInfo, pipelineStatistics}
        type FieldIsArray "pipelineStatistics"
               VkCommandBufferInheritanceInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, pipelineStatistics}

instance CanReadField "pipelineStatistics"
           VkCommandBufferInheritanceInfo
         where
        {-# INLINE getField #-}
        getField = vkPipelineStatistics

        {-# INLINE readField #-}
        readField = readVkPipelineStatistics

instance CanWriteField "pipelineStatistics"
           VkCommandBufferInheritanceInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPipelineStatistics

instance Show VkCommandBufferInheritanceInfo where
        showsPrec d x
          = showString "VkCommandBufferInheritanceInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkRenderPass = " .
                            showsPrec d (vkRenderPass x) .
                              showString ", " .
                                showString "vkSubpass = " .
                                  showsPrec d (vkSubpass x) .
                                    showString ", " .
                                      showString "vkFramebuffer = " .
                                        showsPrec d (vkFramebuffer x) .
                                          showString ", " .
                                            showString "vkOcclusionQueryEnable = " .
                                              showsPrec d (vkOcclusionQueryEnable x) .
                                                showString ", " .
                                                  showString "vkQueryFlags = " .
                                                    showsPrec d (vkQueryFlags x) .
                                                      showString ", " .
                                                        showString "vkPipelineStatistics = " .
                                                          showsPrec d (vkPipelineStatistics x) .
                                                            showChar '}'
