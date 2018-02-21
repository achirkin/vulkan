#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkFramebufferCreateInfo
       (VkFramebufferCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkFramebufferCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkImageView,
                                                             VkRenderPass)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkFramebufferCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkFramebufferCreateFlags    flags;
--   >     VkRenderPass           renderPass;
--   >     uint32_t               attachmentCount;
--   >     const VkImageView*     pAttachments;
--   >     uint32_t               width;
--   >     uint32_t               height;
--   >     uint32_t               layers;
--   > } VkFramebufferCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkFramebufferCreateInfo.html VkFramebufferCreateInfo registry at www.khronos.org>
data VkFramebufferCreateInfo = VkFramebufferCreateInfo## Addr##
                                                        ByteArray##

instance Eq VkFramebufferCreateInfo where
        (VkFramebufferCreateInfo## a _) == x@(VkFramebufferCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFramebufferCreateInfo where
        (VkFramebufferCreateInfo## a _) `compare`
          x@(VkFramebufferCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFramebufferCreateInfo where
        sizeOf ~_ = #{size VkFramebufferCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFramebufferCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFramebufferCreateInfo where
        unsafeAddr (VkFramebufferCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFramebufferCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFramebufferCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFramebufferCreateInfo where
        type StructFields VkFramebufferCreateInfo =
             '["sType", "pNext", "flags", "renderPass", "attachmentCount", -- ' closing tick for hsc2hs
               "pAttachments", "width", "height", "layers"]
        type CUnionType VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkFramebufferCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkFramebufferCreateInfo
         where
        type VkSTypeMType VkFramebufferCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkFramebufferCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkFramebufferCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkFramebufferCreateInfo where
        type FieldType "sType" VkFramebufferCreateInfo = VkStructureType
        type FieldOptional "sType" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, sType}
        type FieldIsArray "sType" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, sType}

instance CanReadField "sType" VkFramebufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkFramebufferCreateInfo
         where
        type VkPNextMType VkFramebufferCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkFramebufferCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkFramebufferCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkFramebufferCreateInfo where
        type FieldType "pNext" VkFramebufferCreateInfo = Ptr Void
        type FieldOptional "pNext" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, pNext}
        type FieldIsArray "pNext" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, pNext}

instance CanReadField "pNext" VkFramebufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkFramebufferCreateInfo
         where
        type VkFlagsMType VkFramebufferCreateInfo =
             VkFramebufferCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkFramebufferCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkFramebufferCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkFramebufferCreateInfo where
        type FieldType "flags" VkFramebufferCreateInfo =
             VkFramebufferCreateFlags
        type FieldOptional "flags" VkFramebufferCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, flags}
        type FieldIsArray "flags" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, flags}

instance CanReadField "flags" VkFramebufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkRenderPass VkFramebufferCreateInfo where
        type VkRenderPassMType VkFramebufferCreateInfo = VkRenderPass

        {-# NOINLINE vkRenderPass #-}
        vkRenderPass x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, renderPass})

        {-# INLINE vkRenderPassByteOffset #-}
        vkRenderPassByteOffset ~_
          = #{offset VkFramebufferCreateInfo, renderPass}

        {-# INLINE readVkRenderPass #-}
        readVkRenderPass p
          = peekByteOff p #{offset VkFramebufferCreateInfo, renderPass}

        {-# INLINE writeVkRenderPass #-}
        writeVkRenderPass p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, renderPass}

instance {-# OVERLAPPING #-}
         HasField "renderPass" VkFramebufferCreateInfo where
        type FieldType "renderPass" VkFramebufferCreateInfo = VkRenderPass
        type FieldOptional "renderPass" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "renderPass" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, renderPass}
        type FieldIsArray "renderPass" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFramebufferCreateInfo, renderPass}

instance CanReadField "renderPass" VkFramebufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkRenderPass

        {-# INLINE readField #-}
        readField = readVkRenderPass

instance CanWriteField "renderPass" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkRenderPass

instance {-# OVERLAPPING #-}
         HasVkAttachmentCount VkFramebufferCreateInfo where
        type VkAttachmentCountMType VkFramebufferCreateInfo = Word32

        {-# NOINLINE vkAttachmentCount #-}
        vkAttachmentCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, attachmentCount})

        {-# INLINE vkAttachmentCountByteOffset #-}
        vkAttachmentCountByteOffset ~_
          = #{offset VkFramebufferCreateInfo, attachmentCount}

        {-# INLINE readVkAttachmentCount #-}
        readVkAttachmentCount p
          = peekByteOff p #{offset VkFramebufferCreateInfo, attachmentCount}

        {-# INLINE writeVkAttachmentCount #-}
        writeVkAttachmentCount p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         HasField "attachmentCount" VkFramebufferCreateInfo where
        type FieldType "attachmentCount" VkFramebufferCreateInfo = Word32
        type FieldOptional "attachmentCount" VkFramebufferCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "attachmentCount" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, attachmentCount}
        type FieldIsArray "attachmentCount" VkFramebufferCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFramebufferCreateInfo, attachmentCount}

instance CanReadField "attachmentCount" VkFramebufferCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkAttachmentCount

        {-# INLINE readField #-}
        readField = readVkAttachmentCount

instance CanWriteField "attachmentCount" VkFramebufferCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkAttachmentCount

instance {-# OVERLAPPING #-}
         HasVkPAttachments VkFramebufferCreateInfo where
        type VkPAttachmentsMType VkFramebufferCreateInfo = Ptr VkImageView

        {-# NOINLINE vkPAttachments #-}
        vkPAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, pAttachments})

        {-# INLINE vkPAttachmentsByteOffset #-}
        vkPAttachmentsByteOffset ~_
          = #{offset VkFramebufferCreateInfo, pAttachments}

        {-# INLINE readVkPAttachments #-}
        readVkPAttachments p
          = peekByteOff p #{offset VkFramebufferCreateInfo, pAttachments}

        {-# INLINE writeVkPAttachments #-}
        writeVkPAttachments p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         HasField "pAttachments" VkFramebufferCreateInfo where
        type FieldType "pAttachments" VkFramebufferCreateInfo =
             Ptr VkImageView
        type FieldOptional "pAttachments" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAttachments" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, pAttachments}
        type FieldIsArray "pAttachments" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFramebufferCreateInfo, pAttachments}

instance CanReadField "pAttachments" VkFramebufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkPAttachments

        {-# INLINE readField #-}
        readField = readVkPAttachments

instance CanWriteField "pAttachments" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPAttachments

instance {-# OVERLAPPING #-} HasVkWidth VkFramebufferCreateInfo
         where
        type VkWidthMType VkFramebufferCreateInfo = Word32

        {-# NOINLINE vkWidth #-}
        vkWidth x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, width})

        {-# INLINE vkWidthByteOffset #-}
        vkWidthByteOffset ~_
          = #{offset VkFramebufferCreateInfo, width}

        {-# INLINE readVkWidth #-}
        readVkWidth p
          = peekByteOff p #{offset VkFramebufferCreateInfo, width}

        {-# INLINE writeVkWidth #-}
        writeVkWidth p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, width}

instance {-# OVERLAPPING #-}
         HasField "width" VkFramebufferCreateInfo where
        type FieldType "width" VkFramebufferCreateInfo = Word32
        type FieldOptional "width" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "width" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, width}
        type FieldIsArray "width" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, width}

instance CanReadField "width" VkFramebufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkWidth

        {-# INLINE readField #-}
        readField = readVkWidth

instance CanWriteField "width" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkWidth

instance {-# OVERLAPPING #-} HasVkHeight VkFramebufferCreateInfo
         where
        type VkHeightMType VkFramebufferCreateInfo = Word32

        {-# NOINLINE vkHeight #-}
        vkHeight x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, height})

        {-# INLINE vkHeightByteOffset #-}
        vkHeightByteOffset ~_
          = #{offset VkFramebufferCreateInfo, height}

        {-# INLINE readVkHeight #-}
        readVkHeight p
          = peekByteOff p #{offset VkFramebufferCreateInfo, height}

        {-# INLINE writeVkHeight #-}
        writeVkHeight p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, height}

instance {-# OVERLAPPING #-}
         HasField "height" VkFramebufferCreateInfo where
        type FieldType "height" VkFramebufferCreateInfo = Word32
        type FieldOptional "height" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "height" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, height}
        type FieldIsArray "height" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, height}

instance CanReadField "height" VkFramebufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkHeight

        {-# INLINE readField #-}
        readField = readVkHeight

instance CanWriteField "height" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkHeight

instance {-# OVERLAPPING #-} HasVkLayers VkFramebufferCreateInfo
         where
        type VkLayersMType VkFramebufferCreateInfo = Word32

        {-# NOINLINE vkLayers #-}
        vkLayers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, layers})

        {-# INLINE vkLayersByteOffset #-}
        vkLayersByteOffset ~_
          = #{offset VkFramebufferCreateInfo, layers}

        {-# INLINE readVkLayers #-}
        readVkLayers p
          = peekByteOff p #{offset VkFramebufferCreateInfo, layers}

        {-# INLINE writeVkLayers #-}
        writeVkLayers p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, layers}

instance {-# OVERLAPPING #-}
         HasField "layers" VkFramebufferCreateInfo where
        type FieldType "layers" VkFramebufferCreateInfo = Word32
        type FieldOptional "layers" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layers" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, layers}
        type FieldIsArray "layers" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, layers}

instance CanReadField "layers" VkFramebufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkLayers

        {-# INLINE readField #-}
        readField = readVkLayers

instance CanWriteField "layers" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkLayers

instance Show VkFramebufferCreateInfo where
        showsPrec d x
          = showString "VkFramebufferCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkRenderPass = " .
                                  showsPrec d (vkRenderPass x) .
                                    showString ", " .
                                      showString "vkAttachmentCount = " .
                                        showsPrec d (vkAttachmentCount x) .
                                          showString ", " .
                                            showString "vkPAttachments = " .
                                              showsPrec d (vkPAttachments x) .
                                                showString ", " .
                                                  showString "vkWidth = " .
                                                    showsPrec d (vkWidth x) .
                                                      showString ", " .
                                                        showString "vkHeight = " .
                                                          showsPrec d (vkHeight x) .
                                                            showString ", " .
                                                              showString "vkLayers = " .
                                                                showsPrec d (vkLayers x) .
                                                                  showChar '}'
