#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferImageCopy
       (VkBufferImageCopy(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                       (VkDeviceSize)
import           Graphics.Vulkan.Types.Struct.VkExtent3D               (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers (VkImageSubresourceLayers)
import           Graphics.Vulkan.Types.Struct.VkOffset3D               (VkOffset3D)
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBufferImageCopy.html VkBufferImageCopy registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkBufferOffset VkBufferImageCopy
         where
        type VkBufferOffsetMType VkBufferImageCopy = VkDeviceSize

        {-# NOINLINE vkBufferOffset #-}
        vkBufferOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, bufferOffset})

        {-# INLINE vkBufferOffsetByteOffset #-}
        vkBufferOffsetByteOffset ~_
          = #{offset VkBufferImageCopy, bufferOffset}

        {-# INLINE readVkBufferOffset #-}
        readVkBufferOffset p
          = peekByteOff p #{offset VkBufferImageCopy, bufferOffset}

        {-# INLINE writeVkBufferOffset #-}
        writeVkBufferOffset p
          = pokeByteOff p #{offset VkBufferImageCopy, bufferOffset}

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

instance CanReadField "bufferOffset" VkBufferImageCopy where
        {-# INLINE getField #-}
        getField = vkBufferOffset

        {-# INLINE readField #-}
        readField = readVkBufferOffset

instance CanWriteField "bufferOffset" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkBufferOffset

instance {-# OVERLAPPING #-} HasVkBufferRowLength VkBufferImageCopy
         where
        type VkBufferRowLengthMType VkBufferImageCopy = Word32

        {-# NOINLINE vkBufferRowLength #-}
        vkBufferRowLength x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, bufferRowLength})

        {-# INLINE vkBufferRowLengthByteOffset #-}
        vkBufferRowLengthByteOffset ~_
          = #{offset VkBufferImageCopy, bufferRowLength}

        {-# INLINE readVkBufferRowLength #-}
        readVkBufferRowLength p
          = peekByteOff p #{offset VkBufferImageCopy, bufferRowLength}

        {-# INLINE writeVkBufferRowLength #-}
        writeVkBufferRowLength p
          = pokeByteOff p #{offset VkBufferImageCopy, bufferRowLength}

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

instance CanReadField "bufferRowLength" VkBufferImageCopy where
        {-# INLINE getField #-}
        getField = vkBufferRowLength

        {-# INLINE readField #-}
        readField = readVkBufferRowLength

instance CanWriteField "bufferRowLength" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkBufferRowLength

instance {-# OVERLAPPING #-}
         HasVkBufferImageHeight VkBufferImageCopy where
        type VkBufferImageHeightMType VkBufferImageCopy = Word32

        {-# NOINLINE vkBufferImageHeight #-}
        vkBufferImageHeight x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, bufferImageHeight})

        {-# INLINE vkBufferImageHeightByteOffset #-}
        vkBufferImageHeightByteOffset ~_
          = #{offset VkBufferImageCopy, bufferImageHeight}

        {-# INLINE readVkBufferImageHeight #-}
        readVkBufferImageHeight p
          = peekByteOff p #{offset VkBufferImageCopy, bufferImageHeight}

        {-# INLINE writeVkBufferImageHeight #-}
        writeVkBufferImageHeight p
          = pokeByteOff p #{offset VkBufferImageCopy, bufferImageHeight}

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

instance CanReadField "bufferImageHeight" VkBufferImageCopy where
        {-# INLINE getField #-}
        getField = vkBufferImageHeight

        {-# INLINE readField #-}
        readField = readVkBufferImageHeight

instance CanWriteField "bufferImageHeight" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkBufferImageHeight

instance {-# OVERLAPPING #-}
         HasVkImageSubresource VkBufferImageCopy where
        type VkImageSubresourceMType VkBufferImageCopy =
             VkImageSubresourceLayers

        {-# NOINLINE vkImageSubresource #-}
        vkImageSubresource x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, imageSubresource})

        {-# INLINE vkImageSubresourceByteOffset #-}
        vkImageSubresourceByteOffset ~_
          = #{offset VkBufferImageCopy, imageSubresource}

        {-# INLINE readVkImageSubresource #-}
        readVkImageSubresource p
          = peekByteOff p #{offset VkBufferImageCopy, imageSubresource}

        {-# INLINE writeVkImageSubresource #-}
        writeVkImageSubresource p
          = pokeByteOff p #{offset VkBufferImageCopy, imageSubresource}

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

instance CanReadField "imageSubresource" VkBufferImageCopy where
        {-# INLINE getField #-}
        getField = vkImageSubresource

        {-# INLINE readField #-}
        readField = readVkImageSubresource

instance CanWriteField "imageSubresource" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkImageSubresource

instance {-# OVERLAPPING #-} HasVkImageOffset VkBufferImageCopy
         where
        type VkImageOffsetMType VkBufferImageCopy = VkOffset3D

        {-# NOINLINE vkImageOffset #-}
        vkImageOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, imageOffset})

        {-# INLINE vkImageOffsetByteOffset #-}
        vkImageOffsetByteOffset ~_
          = #{offset VkBufferImageCopy, imageOffset}

        {-# INLINE readVkImageOffset #-}
        readVkImageOffset p
          = peekByteOff p #{offset VkBufferImageCopy, imageOffset}

        {-# INLINE writeVkImageOffset #-}
        writeVkImageOffset p
          = pokeByteOff p #{offset VkBufferImageCopy, imageOffset}

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

instance CanReadField "imageOffset" VkBufferImageCopy where
        {-# INLINE getField #-}
        getField = vkImageOffset

        {-# INLINE readField #-}
        readField = readVkImageOffset

instance CanWriteField "imageOffset" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkImageOffset

instance {-# OVERLAPPING #-} HasVkImageExtent VkBufferImageCopy
         where
        type VkImageExtentMType VkBufferImageCopy = VkExtent3D

        {-# NOINLINE vkImageExtent #-}
        vkImageExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferImageCopy, imageExtent})

        {-# INLINE vkImageExtentByteOffset #-}
        vkImageExtentByteOffset ~_
          = #{offset VkBufferImageCopy, imageExtent}

        {-# INLINE readVkImageExtent #-}
        readVkImageExtent p
          = peekByteOff p #{offset VkBufferImageCopy, imageExtent}

        {-# INLINE writeVkImageExtent #-}
        writeVkImageExtent p
          = pokeByteOff p #{offset VkBufferImageCopy, imageExtent}

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

instance CanReadField "imageExtent" VkBufferImageCopy where
        {-# INLINE getField #-}
        getField = vkImageExtent

        {-# INLINE readField #-}
        readField = readVkImageExtent

instance CanWriteField "imageExtent" VkBufferImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkImageExtent

instance Show VkBufferImageCopy where
        showsPrec d x
          = showString "VkBufferImageCopy {" .
              showString "vkBufferOffset = " .
                showsPrec d (vkBufferOffset x) .
                  showString ", " .
                    showString "vkBufferRowLength = " .
                      showsPrec d (vkBufferRowLength x) .
                        showString ", " .
                          showString "vkBufferImageHeight = " .
                            showsPrec d (vkBufferImageHeight x) .
                              showString ", " .
                                showString "vkImageSubresource = " .
                                  showsPrec d (vkImageSubresource x) .
                                    showString ", " .
                                      showString "vkImageOffset = " .
                                        showsPrec d (vkImageOffset x) .
                                          showString ", " .
                                            showString "vkImageExtent = " .
                                              showsPrec d (vkImageExtent x) . showChar '}'
