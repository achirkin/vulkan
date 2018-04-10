#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferImageCopy
       (VkBufferImageCopy(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                       (VkDeviceSize)
import           Graphics.Vulkan.Types.Struct.VkExtent3D               (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers (VkImageSubresourceLayers)
import           Graphics.Vulkan.Types.Struct.VkOffset3D               (VkOffset3D)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkBufferImageCopy {
--   >     VkDeviceSize           bufferOffset;
--   >     uint32_t               bufferRowLength;
--   >     uint32_t               bufferImageHeight;
--   >     VkImageSubresourceLayers imageSubresource;
--   >     VkOffset3D             imageOffset;
--   >     VkExtent3D             imageExtent;
--   > } VkBufferImageCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkBufferImageCopy VkBufferImageCopy registry at www.khronos.org>
data VkBufferImageCopy = VkBufferImageCopy## Addr## ByteArray##

instance Eq VkBufferImageCopy where
        (VkBufferImageCopy## a _) == x@(VkBufferImageCopy## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferImageCopy where
        (VkBufferImageCopy## a _) `compare` x@(VkBufferImageCopy## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferImageCopy where
        sizeOf ~_ = #{size VkBufferImageCopy}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBufferImageCopy}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferImageCopy where
        unsafeAddr (VkBufferImageCopy## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferImageCopy## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferImageCopy## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferImageCopy where
        type StructFields VkBufferImageCopy =
             '["bufferOffset", "bufferRowLength", "bufferImageHeight", -- ' closing tick for hsc2hs
               "imageSubresource", "imageOffset", "imageExtent"]
        type CUnionType VkBufferImageCopy = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferImageCopy = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferImageCopy = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "bufferOffset" VkBufferImageCopy where
        type FieldType "bufferOffset" VkBufferImageCopy = VkDeviceSize
        type FieldOptional "bufferOffset" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "bufferOffset" VkBufferImageCopy =
             #{offset VkBufferImageCopy, bufferOffset}
        type FieldIsArray "bufferOffset" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferImageCopy, bufferOffset}

instance {-# OVERLAPPING #-}
         CanReadField "bufferOffset" VkBufferImageCopy where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, bufferOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferImageCopy, bufferOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "bufferOffset" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferImageCopy, bufferOffset}

instance {-# OVERLAPPING #-}
         HasField "bufferRowLength" VkBufferImageCopy where
        type FieldType "bufferRowLength" VkBufferImageCopy = Word32
        type FieldOptional "bufferRowLength" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "bufferRowLength" VkBufferImageCopy =
             #{offset VkBufferImageCopy, bufferRowLength}
        type FieldIsArray "bufferRowLength" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferImageCopy, bufferRowLength}

instance {-# OVERLAPPING #-}
         CanReadField "bufferRowLength" VkBufferImageCopy where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, bufferRowLength})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferImageCopy, bufferRowLength}

instance {-# OVERLAPPING #-}
         CanWriteField "bufferRowLength" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferImageCopy, bufferRowLength}

instance {-# OVERLAPPING #-}
         HasField "bufferImageHeight" VkBufferImageCopy where
        type FieldType "bufferImageHeight" VkBufferImageCopy = Word32
        type FieldOptional "bufferImageHeight" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "bufferImageHeight" VkBufferImageCopy =
             #{offset VkBufferImageCopy, bufferImageHeight}
        type FieldIsArray "bufferImageHeight" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferImageCopy, bufferImageHeight}

instance {-# OVERLAPPING #-}
         CanReadField "bufferImageHeight" VkBufferImageCopy where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, bufferImageHeight})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferImageCopy, bufferImageHeight}

instance {-# OVERLAPPING #-}
         CanWriteField "bufferImageHeight" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferImageCopy, bufferImageHeight}

instance {-# OVERLAPPING #-}
         HasField "imageSubresource" VkBufferImageCopy where
        type FieldType "imageSubresource" VkBufferImageCopy =
             VkImageSubresourceLayers
        type FieldOptional "imageSubresource" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageSubresource" VkBufferImageCopy =
             #{offset VkBufferImageCopy, imageSubresource}
        type FieldIsArray "imageSubresource" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferImageCopy, imageSubresource}

instance {-# OVERLAPPING #-}
         CanReadField "imageSubresource" VkBufferImageCopy where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, imageSubresource})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferImageCopy, imageSubresource}

instance {-# OVERLAPPING #-}
         CanWriteField "imageSubresource" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferImageCopy, imageSubresource}

instance {-# OVERLAPPING #-}
         HasField "imageOffset" VkBufferImageCopy where
        type FieldType "imageOffset" VkBufferImageCopy = VkOffset3D
        type FieldOptional "imageOffset" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageOffset" VkBufferImageCopy =
             #{offset VkBufferImageCopy, imageOffset}
        type FieldIsArray "imageOffset" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferImageCopy, imageOffset}

instance {-# OVERLAPPING #-}
         CanReadField "imageOffset" VkBufferImageCopy where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, imageOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferImageCopy, imageOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "imageOffset" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferImageCopy, imageOffset}

instance {-# OVERLAPPING #-}
         HasField "imageExtent" VkBufferImageCopy where
        type FieldType "imageExtent" VkBufferImageCopy = VkExtent3D
        type FieldOptional "imageExtent" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageExtent" VkBufferImageCopy =
             #{offset VkBufferImageCopy, imageExtent}
        type FieldIsArray "imageExtent" VkBufferImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferImageCopy, imageExtent}

instance {-# OVERLAPPING #-}
         CanReadField "imageExtent" VkBufferImageCopy where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, imageExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferImageCopy, imageExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "imageExtent" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferImageCopy, imageExtent}

instance Show VkBufferImageCopy where
        showsPrec d x
          = showString "VkBufferImageCopy {" .
              showString "bufferOffset = " .
                showsPrec d (getField @"bufferOffset" x) .
                  showString ", " .
                    showString "bufferRowLength = " .
                      showsPrec d (getField @"bufferRowLength" x) .
                        showString ", " .
                          showString "bufferImageHeight = " .
                            showsPrec d (getField @"bufferImageHeight" x) .
                              showString ", " .
                                showString "imageSubresource = " .
                                  showsPrec d (getField @"imageSubresource" x) .
                                    showString ", " .
                                      showString "imageOffset = " .
                                        showsPrec d (getField @"imageOffset" x) .
                                          showString ", " .
                                            showString "imageExtent = " .
                                              showsPrec d (getField @"imageExtent" x) . showChar '}'
