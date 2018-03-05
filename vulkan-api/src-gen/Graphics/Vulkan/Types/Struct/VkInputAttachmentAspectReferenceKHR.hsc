#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkInputAttachmentAspectReferenceKHR
       (VkInputAttachmentAspectReferenceKHR(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags (VkImageAspectFlags)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkInputAttachmentAspectReferenceKHR {
--   >     uint32_t                        subpass;
--   >     uint32_t                        inputAttachmentIndex;
--   >     VkImageAspectFlags              aspectMask;
--   > } VkInputAttachmentAspectReferenceKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkInputAttachmentAspectReferenceKHR.html VkInputAttachmentAspectReferenceKHR registry at www.khronos.org>
data VkInputAttachmentAspectReferenceKHR = VkInputAttachmentAspectReferenceKHR## Addr##
                                                                                ByteArray##

instance Eq VkInputAttachmentAspectReferenceKHR where
        (VkInputAttachmentAspectReferenceKHR## a _) ==
          x@(VkInputAttachmentAspectReferenceKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkInputAttachmentAspectReferenceKHR where
        (VkInputAttachmentAspectReferenceKHR## a _) `compare`
          x@(VkInputAttachmentAspectReferenceKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkInputAttachmentAspectReferenceKHR where
        sizeOf ~_ = #{size VkInputAttachmentAspectReferenceKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkInputAttachmentAspectReferenceKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkInputAttachmentAspectReferenceKHR
         where
        unsafeAddr (VkInputAttachmentAspectReferenceKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkInputAttachmentAspectReferenceKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkInputAttachmentAspectReferenceKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkInputAttachmentAspectReferenceKHR where
        type StructFields VkInputAttachmentAspectReferenceKHR =
             '["subpass", "inputAttachmentIndex", "aspectMask"] -- ' closing tick for hsc2hs
        type CUnionType VkInputAttachmentAspectReferenceKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkInputAttachmentAspectReferenceKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkInputAttachmentAspectReferenceKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "subpass" VkInputAttachmentAspectReferenceKHR where
        type FieldType "subpass" VkInputAttachmentAspectReferenceKHR =
             Word32
        type FieldOptional "subpass" VkInputAttachmentAspectReferenceKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subpass" VkInputAttachmentAspectReferenceKHR =
             #{offset VkInputAttachmentAspectReferenceKHR, subpass}
        type FieldIsArray "subpass" VkInputAttachmentAspectReferenceKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInputAttachmentAspectReferenceKHR, subpass}

instance {-# OVERLAPPING #-}
         CanReadField "subpass" VkInputAttachmentAspectReferenceKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReferenceKHR, subpass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInputAttachmentAspectReferenceKHR, subpass}

instance {-# OVERLAPPING #-}
         CanWriteField "subpass" VkInputAttachmentAspectReferenceKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInputAttachmentAspectReferenceKHR, subpass}

instance {-# OVERLAPPING #-}
         HasField "inputAttachmentIndex" VkInputAttachmentAspectReferenceKHR
         where
        type FieldType "inputAttachmentIndex"
               VkInputAttachmentAspectReferenceKHR
             = Word32
        type FieldOptional "inputAttachmentIndex"
               VkInputAttachmentAspectReferenceKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "inputAttachmentIndex"
               VkInputAttachmentAspectReferenceKHR
             =
             #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}
        type FieldIsArray "inputAttachmentIndex"
               VkInputAttachmentAspectReferenceKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}

instance {-# OVERLAPPING #-}
         CanReadField "inputAttachmentIndex"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "inputAttachmentIndex"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkInputAttachmentAspectReferenceKHR where
        type FieldType "aspectMask" VkInputAttachmentAspectReferenceKHR =
             VkImageAspectFlags
        type FieldOptional "aspectMask" VkInputAttachmentAspectReferenceKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkInputAttachmentAspectReferenceKHR =
             #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}
        type FieldIsArray "aspectMask" VkInputAttachmentAspectReferenceKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkInputAttachmentAspectReferenceKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReferenceKHR, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}

instance Show VkInputAttachmentAspectReferenceKHR where
        showsPrec d x
          = showString "VkInputAttachmentAspectReferenceKHR {" .
              showString "subpass = " .
                showsPrec d (getField @"subpass" x) .
                  showString ", " .
                    showString "inputAttachmentIndex = " .
                      showsPrec d (getField @"inputAttachmentIndex" x) .
                        showString ", " .
                          showString "aspectMask = " .
                            showsPrec d (getField @"aspectMask" x) . showChar '}'
