#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.InputAttachmentAspectReference
       (VkInputAttachmentAspectReference,
        VkInputAttachmentAspectReference', -- ' closing tick for hsc2hs
        VkInputAttachmentAspectReferenceKHR)
       where
import Foreign.Storable                 (Storable (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.Image (VkImageAspectFlags)
import System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkInputAttachmentAspectReference {
--   >     uint32_t                        subpass;
--   >     uint32_t                        inputAttachmentIndex;
--   >     VkImageAspectFlags              aspectMask;
--   > } VkInputAttachmentAspectReference;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkInputAttachmentAspectReference VkInputAttachmentAspectReference registry at www.khronos.org>
type VkInputAttachmentAspectReference =
     VulkanStruct VkInputAttachmentAspectReference' -- ' closing tick for hsc2hs

data VkInputAttachmentAspectReference' -- ' closing tick for hsc2hs

instance Eq VkInputAttachmentAspectReference where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkInputAttachmentAspectReference where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkInputAttachmentAspectReference where
        sizeOf ~_ = #{size VkInputAttachmentAspectReference}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkInputAttachmentAspectReference}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkInputAttachmentAspectReference where
        type StructFields VkInputAttachmentAspectReference =
             '["subpass", "inputAttachmentIndex", "aspectMask"] -- ' closing tick for hsc2hs
        type CUnionType VkInputAttachmentAspectReference = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkInputAttachmentAspectReference = 'False -- ' closing tick for hsc2hs
        type StructExtends VkInputAttachmentAspectReference = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "subpass" VkInputAttachmentAspectReference where
        type FieldType "subpass" VkInputAttachmentAspectReference = Word32
        type FieldOptional "subpass" VkInputAttachmentAspectReference =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subpass" VkInputAttachmentAspectReference =
             #{offset VkInputAttachmentAspectReference, subpass}
        type FieldIsArray "subpass" VkInputAttachmentAspectReference =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInputAttachmentAspectReference, subpass}

instance {-# OVERLAPPING #-}
         CanReadField "subpass" VkInputAttachmentAspectReference where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReference, subpass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInputAttachmentAspectReference, subpass}

instance {-# OVERLAPPING #-}
         CanWriteField "subpass" VkInputAttachmentAspectReference where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInputAttachmentAspectReference, subpass}

instance {-# OVERLAPPING #-}
         HasField "inputAttachmentIndex" VkInputAttachmentAspectReference
         where
        type FieldType "inputAttachmentIndex"
               VkInputAttachmentAspectReference
             = Word32
        type FieldOptional "inputAttachmentIndex"
               VkInputAttachmentAspectReference
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "inputAttachmentIndex"
               VkInputAttachmentAspectReference
             =
             #{offset VkInputAttachmentAspectReference, inputAttachmentIndex}
        type FieldIsArray "inputAttachmentIndex"
               VkInputAttachmentAspectReference
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInputAttachmentAspectReference, inputAttachmentIndex}

instance {-# OVERLAPPING #-}
         CanReadField "inputAttachmentIndex"
           VkInputAttachmentAspectReference
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReference, inputAttachmentIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInputAttachmentAspectReference, inputAttachmentIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "inputAttachmentIndex"
           VkInputAttachmentAspectReference
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInputAttachmentAspectReference, inputAttachmentIndex}

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkInputAttachmentAspectReference where
        type FieldType "aspectMask" VkInputAttachmentAspectReference =
             VkImageAspectFlags
        type FieldOptional "aspectMask" VkInputAttachmentAspectReference =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkInputAttachmentAspectReference =
             #{offset VkInputAttachmentAspectReference, aspectMask}
        type FieldIsArray "aspectMask" VkInputAttachmentAspectReference =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInputAttachmentAspectReference, aspectMask}

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkInputAttachmentAspectReference where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReference, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInputAttachmentAspectReference, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkInputAttachmentAspectReference where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInputAttachmentAspectReference, aspectMask}

instance Show VkInputAttachmentAspectReference where
        showsPrec d x
          = showString "VkInputAttachmentAspectReference {" .
              showString "subpass = " .
                showsPrec d (getField @"subpass" x) .
                  showString ", " .
                    showString "inputAttachmentIndex = " .
                      showsPrec d (getField @"inputAttachmentIndex" x) .
                        showString ", " .
                          showString "aspectMask = " .
                            showsPrec d (getField @"aspectMask" x) . showChar '}'

-- | Alias for `VkInputAttachmentAspectReference`
type VkInputAttachmentAspectReferenceKHR =
     VkInputAttachmentAspectReference
