#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAttachmentReference
       (VkAttachmentReference(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageLayout (VkImageLayout)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkAttachmentReference {
--   >     uint32_t               attachment;
--   >     VkImageLayout          layout;
--   > } VkAttachmentReference;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkAttachmentReference.html VkAttachmentReference registry at www.khronos.org>
data VkAttachmentReference = VkAttachmentReference## Addr##
                                                    ByteArray##

instance Eq VkAttachmentReference where
        (VkAttachmentReference## a _) == x@(VkAttachmentReference## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAttachmentReference where
        (VkAttachmentReference## a _) `compare`
          x@(VkAttachmentReference## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAttachmentReference where
        sizeOf ~_ = #{size VkAttachmentReference}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkAttachmentReference}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAttachmentReference where
        unsafeAddr (VkAttachmentReference## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAttachmentReference## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAttachmentReference## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAttachmentReference where
        type StructFields VkAttachmentReference = '["attachment", "layout"] -- ' closing tick for hsc2hs
        type CUnionType VkAttachmentReference = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAttachmentReference = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAttachmentReference = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkAttachment VkAttachmentReference
         where
        type VkAttachmentMType VkAttachmentReference = Word32

        {-# NOINLINE vkAttachment #-}
        vkAttachment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentReference, attachment})

        {-# INLINE vkAttachmentByteOffset #-}
        vkAttachmentByteOffset ~_
          = #{offset VkAttachmentReference, attachment}

        {-# INLINE readVkAttachment #-}
        readVkAttachment p
          = peekByteOff p #{offset VkAttachmentReference, attachment}

        {-# INLINE writeVkAttachment #-}
        writeVkAttachment p
          = pokeByteOff p #{offset VkAttachmentReference, attachment}

instance {-# OVERLAPPING #-}
         HasField "attachment" VkAttachmentReference where
        type FieldType "attachment" VkAttachmentReference = Word32
        type FieldOptional "attachment" VkAttachmentReference = 'False -- ' closing tick for hsc2hs
        type FieldOffset "attachment" VkAttachmentReference =
             #{offset VkAttachmentReference, attachment}
        type FieldIsArray "attachment" VkAttachmentReference = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentReference, attachment}

instance CanReadField "attachment" VkAttachmentReference where
        {-# INLINE getField #-}
        getField = vkAttachment

        {-# INLINE readField #-}
        readField = readVkAttachment

instance CanWriteField "attachment" VkAttachmentReference where
        {-# INLINE writeField #-}
        writeField = writeVkAttachment

instance {-# OVERLAPPING #-} HasVkLayout VkAttachmentReference
         where
        type VkLayoutMType VkAttachmentReference = VkImageLayout

        {-# NOINLINE vkLayout #-}
        vkLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentReference, layout})

        {-# INLINE vkLayoutByteOffset #-}
        vkLayoutByteOffset ~_
          = #{offset VkAttachmentReference, layout}

        {-# INLINE readVkLayout #-}
        readVkLayout p
          = peekByteOff p #{offset VkAttachmentReference, layout}

        {-# INLINE writeVkLayout #-}
        writeVkLayout p
          = pokeByteOff p #{offset VkAttachmentReference, layout}

instance {-# OVERLAPPING #-}
         HasField "layout" VkAttachmentReference where
        type FieldType "layout" VkAttachmentReference = VkImageLayout
        type FieldOptional "layout" VkAttachmentReference = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layout" VkAttachmentReference =
             #{offset VkAttachmentReference, layout}
        type FieldIsArray "layout" VkAttachmentReference = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkAttachmentReference, layout}

instance CanReadField "layout" VkAttachmentReference where
        {-# INLINE getField #-}
        getField = vkLayout

        {-# INLINE readField #-}
        readField = readVkLayout

instance CanWriteField "layout" VkAttachmentReference where
        {-# INLINE writeField #-}
        writeField = writeVkLayout

instance Show VkAttachmentReference where
        showsPrec d x
          = showString "VkAttachmentReference {" .
              showString "vkAttachment = " .
                showsPrec d (vkAttachment x) .
                  showString ", " .
                    showString "vkLayout = " . showsPrec d (vkLayout x) . showChar '}'
