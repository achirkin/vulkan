#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAttachmentReference
       (VkAttachmentReference(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageLayout (VkImageLayout)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkAttachmentReference {
--   >     uint32_t               attachment;
--   >     VkImageLayout          layout;
--   > } VkAttachmentReference;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkAttachmentReference VkAttachmentReference registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "attachment" VkAttachmentReference where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentReference, attachment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentReference, attachment}

instance {-# OVERLAPPING #-}
         CanWriteField "attachment" VkAttachmentReference where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentReference, attachment}

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

instance {-# OVERLAPPING #-}
         CanReadField "layout" VkAttachmentReference where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentReference, layout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentReference, layout}

instance {-# OVERLAPPING #-}
         CanWriteField "layout" VkAttachmentReference where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentReference, layout}

instance Show VkAttachmentReference where
        showsPrec d x
          = showString "VkAttachmentReference {" .
              showString "attachment = " .
                showsPrec d (getField @"attachment" x) .
                  showString ", " .
                    showString "layout = " .
                      showsPrec d (getField @"layout" x) . showChar '}'
