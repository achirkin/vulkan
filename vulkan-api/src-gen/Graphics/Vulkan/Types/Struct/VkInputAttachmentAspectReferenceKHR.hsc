#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkInputAttachmentAspectReferenceKHR
       (VkInputAttachmentAspectReferenceKHR(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags (VkImageAspectFlags)
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSubpass VkInputAttachmentAspectReferenceKHR where
        type VkSubpassMType VkInputAttachmentAspectReferenceKHR = Word32

        {-# NOINLINE vkSubpass #-}
        vkSubpass x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReferenceKHR, subpass})

        {-# INLINE vkSubpassByteOffset #-}
        vkSubpassByteOffset ~_
          = #{offset VkInputAttachmentAspectReferenceKHR, subpass}

        {-# INLINE readVkSubpass #-}
        readVkSubpass p
          = peekByteOff p #{offset VkInputAttachmentAspectReferenceKHR, subpass}

        {-# INLINE writeVkSubpass #-}
        writeVkSubpass p
          = pokeByteOff p #{offset VkInputAttachmentAspectReferenceKHR, subpass}

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

instance CanReadField "subpass" VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE getField #-}
        getField = vkSubpass

        {-# INLINE readField #-}
        readField = readVkSubpass

instance CanWriteField "subpass"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubpass

instance {-# OVERLAPPING #-}
         HasVkInputAttachmentIndex VkInputAttachmentAspectReferenceKHR where
        type VkInputAttachmentIndexMType
               VkInputAttachmentAspectReferenceKHR
             = Word32

        {-# NOINLINE vkInputAttachmentIndex #-}
        vkInputAttachmentIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex})

        {-# INLINE vkInputAttachmentIndexByteOffset #-}
        vkInputAttachmentIndexByteOffset ~_
          = #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}

        {-# INLINE readVkInputAttachmentIndex #-}
        readVkInputAttachmentIndex p
          = peekByteOff p #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}

        {-# INLINE writeVkInputAttachmentIndex #-}
        writeVkInputAttachmentIndex p
          = pokeByteOff p #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}

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

instance CanReadField "inputAttachmentIndex"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE getField #-}
        getField = vkInputAttachmentIndex

        {-# INLINE readField #-}
        readField = readVkInputAttachmentIndex

instance CanWriteField "inputAttachmentIndex"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkInputAttachmentIndex

instance {-# OVERLAPPING #-}
         HasVkAspectMask VkInputAttachmentAspectReferenceKHR where
        type VkAspectMaskMType VkInputAttachmentAspectReferenceKHR =
             VkImageAspectFlags

        {-# NOINLINE vkAspectMask #-}
        vkAspectMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReferenceKHR, aspectMask})

        {-# INLINE vkAspectMaskByteOffset #-}
        vkAspectMaskByteOffset ~_
          = #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}

        {-# INLINE readVkAspectMask #-}
        readVkAspectMask p
          = peekByteOff p #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}

        {-# INLINE writeVkAspectMask #-}
        writeVkAspectMask p
          = pokeByteOff p #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}

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

instance CanReadField "aspectMask"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE getField #-}
        getField = vkAspectMask

        {-# INLINE readField #-}
        readField = readVkAspectMask

instance CanWriteField "aspectMask"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkAspectMask

instance Show VkInputAttachmentAspectReferenceKHR where
        showsPrec d x
          = showString "VkInputAttachmentAspectReferenceKHR {" .
              showString "vkSubpass = " .
                showsPrec d (vkSubpass x) .
                  showString ", " .
                    showString "vkInputAttachmentIndex = " .
                      showsPrec d (vkInputAttachmentIndex x) .
                        showString ", " .
                          showString "vkAspectMask = " .
                            showsPrec d (vkAspectMask x) . showChar '}'
