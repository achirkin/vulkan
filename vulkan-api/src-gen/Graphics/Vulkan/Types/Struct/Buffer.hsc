#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Buffer
       (VkBufferCopy(..), VkBufferCreateInfo(..), VkBufferImageCopy(..),
        VkBufferMemoryBarrier(..), VkBufferMemoryRequirementsInfo2(..),
        VkBufferMemoryRequirementsInfo2KHR, VkBufferViewCreateInfo(..))
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkDeviceSize)
import           Graphics.Vulkan.Types.Bitmasks           (VkBufferViewCreateFlags)
import           Graphics.Vulkan.Types.Enum.AccessFlags   (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.Buffer        (VkBufferCreateFlags,
                                                           VkBufferUsageFlags)
import           Graphics.Vulkan.Types.Enum.Format        (VkFormat)
import           Graphics.Vulkan.Types.Enum.SharingMode   (VkSharingMode)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer)
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.Image       (VkImageSubresourceLayers)
import           Graphics.Vulkan.Types.Struct.Offset      (VkOffset3D)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkBufferCopy {
--   >     VkDeviceSize           srcOffset;
--   >     VkDeviceSize           dstOffset;
--   >     VkDeviceSize           size;
--   > } VkBufferCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferCopy VkBufferCopy registry at www.khronos.org>
data VkBufferCopy = VkBufferCopy## Addr## ByteArray##

instance Eq VkBufferCopy where
        (VkBufferCopy## a _) == x@(VkBufferCopy## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferCopy where
        (VkBufferCopy## a _) `compare` x@(VkBufferCopy## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferCopy where
        sizeOf ~_ = #{size VkBufferCopy}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBufferCopy}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferCopy where
        unsafeAddr (VkBufferCopy## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferCopy## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferCopy## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferCopy where
        type StructFields VkBufferCopy =
             '["srcOffset", "dstOffset", "size"] -- ' closing tick for hsc2hs
        type CUnionType VkBufferCopy = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferCopy = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferCopy = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "srcOffset" VkBufferCopy
         where
        type FieldType "srcOffset" VkBufferCopy = VkDeviceSize
        type FieldOptional "srcOffset" VkBufferCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcOffset" VkBufferCopy =
             #{offset VkBufferCopy, srcOffset}
        type FieldIsArray "srcOffset" VkBufferCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCopy, srcOffset}

instance {-# OVERLAPPING #-} CanReadField "srcOffset" VkBufferCopy
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCopy, srcOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCopy, srcOffset}

instance {-# OVERLAPPING #-} CanWriteField "srcOffset" VkBufferCopy
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCopy, srcOffset}

instance {-# OVERLAPPING #-} HasField "dstOffset" VkBufferCopy
         where
        type FieldType "dstOffset" VkBufferCopy = VkDeviceSize
        type FieldOptional "dstOffset" VkBufferCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstOffset" VkBufferCopy =
             #{offset VkBufferCopy, dstOffset}
        type FieldIsArray "dstOffset" VkBufferCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCopy, dstOffset}

instance {-# OVERLAPPING #-} CanReadField "dstOffset" VkBufferCopy
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCopy, dstOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCopy, dstOffset}

instance {-# OVERLAPPING #-} CanWriteField "dstOffset" VkBufferCopy
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCopy, dstOffset}

instance {-# OVERLAPPING #-} HasField "size" VkBufferCopy where
        type FieldType "size" VkBufferCopy = VkDeviceSize
        type FieldOptional "size" VkBufferCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkBufferCopy =
             #{offset VkBufferCopy, size}
        type FieldIsArray "size" VkBufferCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCopy, size}

instance {-# OVERLAPPING #-} CanReadField "size" VkBufferCopy where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCopy, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCopy, size}

instance {-# OVERLAPPING #-} CanWriteField "size" VkBufferCopy
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCopy, size}

instance Show VkBufferCopy where
        showsPrec d x
          = showString "VkBufferCopy {" .
              showString "srcOffset = " .
                showsPrec d (getField @"srcOffset" x) .
                  showString ", " .
                    showString "dstOffset = " .
                      showsPrec d (getField @"dstOffset" x) .
                        showString ", " .
                          showString "size = " .
                            showsPrec d (getField @"size" x) . showChar '}'

-- | > typedef struct VkBufferCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBufferCreateFlags    flags;
--   >     VkDeviceSize           size;
--   >     VkBufferUsageFlags     usage;
--   >     VkSharingMode          sharingMode;
--   >     uint32_t               queueFamilyIndexCount;
--   >     const uint32_t*        pQueueFamilyIndices;
--   > } VkBufferCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferCreateInfo VkBufferCreateInfo registry at www.khronos.org>
data VkBufferCreateInfo = VkBufferCreateInfo## Addr## ByteArray##

instance Eq VkBufferCreateInfo where
        (VkBufferCreateInfo## a _) == x@(VkBufferCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferCreateInfo where
        (VkBufferCreateInfo## a _) `compare` x@(VkBufferCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferCreateInfo where
        sizeOf ~_ = #{size VkBufferCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBufferCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferCreateInfo where
        unsafeAddr (VkBufferCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferCreateInfo where
        type StructFields VkBufferCreateInfo =
             '["sType", "pNext", "flags", "size", "usage", "sharingMode", -- ' closing tick for hsc2hs
               "queueFamilyIndexCount", "pQueueFamilyIndices"]
        type CUnionType VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkBufferCreateInfo
         where
        type FieldType "sType" VkBufferCreateInfo = VkStructureType
        type FieldOptional "sType" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, sType}
        type FieldIsArray "sType" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkBufferCreateInfo
         where
        type FieldType "pNext" VkBufferCreateInfo = Ptr Void
        type FieldOptional "pNext" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, pNext}
        type FieldIsArray "pNext" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkBufferCreateInfo
         where
        type FieldType "flags" VkBufferCreateInfo = VkBufferCreateFlags
        type FieldOptional "flags" VkBufferCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, flags}
        type FieldIsArray "flags" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCreateInfo, flags}

instance {-# OVERLAPPING #-} HasField "size" VkBufferCreateInfo
         where
        type FieldType "size" VkBufferCreateInfo = VkDeviceSize
        type FieldOptional "size" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, size}
        type FieldIsArray "size" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, size}

instance {-# OVERLAPPING #-} CanReadField "size" VkBufferCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCreateInfo, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCreateInfo, size}

instance {-# OVERLAPPING #-} HasField "usage" VkBufferCreateInfo
         where
        type FieldType "usage" VkBufferCreateInfo = VkBufferUsageFlags
        type FieldOptional "usage" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, usage}
        type FieldIsArray "usage" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, usage}

instance {-# OVERLAPPING #-}
         CanReadField "usage" VkBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCreateInfo, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCreateInfo, usage}

instance {-# OVERLAPPING #-}
         HasField "sharingMode" VkBufferCreateInfo where
        type FieldType "sharingMode" VkBufferCreateInfo = VkSharingMode
        type FieldOptional "sharingMode" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sharingMode" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, sharingMode}
        type FieldIsArray "sharingMode" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, sharingMode}

instance {-# OVERLAPPING #-}
         CanReadField "sharingMode" VkBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, sharingMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCreateInfo, sharingMode}

instance {-# OVERLAPPING #-}
         CanWriteField "sharingMode" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCreateInfo, sharingMode}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndexCount" VkBufferCreateInfo where
        type FieldType "queueFamilyIndexCount" VkBufferCreateInfo = Word32
        type FieldOptional "queueFamilyIndexCount" VkBufferCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndexCount" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, queueFamilyIndexCount}
        type FieldIsArray "queueFamilyIndexCount" VkBufferCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferCreateInfo, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyIndexCount" VkBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, queueFamilyIndexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCreateInfo, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyIndexCount" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCreateInfo, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         HasField "pQueueFamilyIndices" VkBufferCreateInfo where
        type FieldType "pQueueFamilyIndices" VkBufferCreateInfo =
             Ptr Word32
        type FieldOptional "pQueueFamilyIndices" VkBufferCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pQueueFamilyIndices" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, pQueueFamilyIndices}
        type FieldIsArray "pQueueFamilyIndices" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferCreateInfo, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pQueueFamilyIndices" VkBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, pQueueFamilyIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCreateInfo, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pQueueFamilyIndices" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCreateInfo, pQueueFamilyIndices}

instance Show VkBufferCreateInfo where
        showsPrec d x
          = showString "VkBufferCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "size = " .
                                  showsPrec d (getField @"size" x) .
                                    showString ", " .
                                      showString "usage = " .
                                        showsPrec d (getField @"usage" x) .
                                          showString ", " .
                                            showString "sharingMode = " .
                                              showsPrec d (getField @"sharingMode" x) .
                                                showString ", " .
                                                  showString "queueFamilyIndexCount = " .
                                                    showsPrec d
                                                      (getField @"queueFamilyIndexCount" x)
                                                      .
                                                      showString ", " .
                                                        showString "pQueueFamilyIndices = " .
                                                          showsPrec d
                                                            (getField @"pQueueFamilyIndices" x)
                                                            . showChar '}'

-- | > typedef struct VkBufferImageCopy {
--   >     VkDeviceSize           bufferOffset;
--   >     uint32_t               bufferRowLength;
--   >     uint32_t               bufferImageHeight;
--   >     VkImageSubresourceLayers imageSubresource;
--   >     VkOffset3D             imageOffset;
--   >     VkExtent3D             imageExtent;
--   > } VkBufferImageCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferImageCopy VkBufferImageCopy registry at www.khronos.org>
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

-- | > typedef struct VkBufferMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   >     uint32_t               srcQueueFamilyIndex;
--   >     uint32_t               dstQueueFamilyIndex;
--   >     VkBuffer               buffer;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           size;
--   > } VkBufferMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferMemoryBarrier VkBufferMemoryBarrier registry at www.khronos.org>
data VkBufferMemoryBarrier = VkBufferMemoryBarrier## Addr##
                                                    ByteArray##

instance Eq VkBufferMemoryBarrier where
        (VkBufferMemoryBarrier## a _) == x@(VkBufferMemoryBarrier## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferMemoryBarrier where
        (VkBufferMemoryBarrier## a _) `compare`
          x@(VkBufferMemoryBarrier## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferMemoryBarrier where
        sizeOf ~_ = #{size VkBufferMemoryBarrier}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBufferMemoryBarrier}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferMemoryBarrier where
        unsafeAddr (VkBufferMemoryBarrier## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferMemoryBarrier## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferMemoryBarrier## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferMemoryBarrier where
        type StructFields VkBufferMemoryBarrier =
             '["sType", "pNext", "srcAccessMask", "dstAccessMask", -- ' closing tick for hsc2hs
               "srcQueueFamilyIndex", "dstQueueFamilyIndex", "buffer", "offset",
               "size"]
        type CUnionType VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferMemoryBarrier = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkBufferMemoryBarrier
         where
        type FieldType "sType" VkBufferMemoryBarrier = VkStructureType
        type FieldOptional "sType" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, sType}
        type FieldIsArray "sType" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkBufferMemoryBarrier
         where
        type FieldType "pNext" VkBufferMemoryBarrier = Ptr Void
        type FieldOptional "pNext" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, pNext}
        type FieldIsArray "pNext" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         HasField "srcAccessMask" VkBufferMemoryBarrier where
        type FieldType "srcAccessMask" VkBufferMemoryBarrier =
             VkAccessFlags
        type FieldOptional "srcAccessMask" VkBufferMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "srcAccessMask" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, srcAccessMask}
        type FieldIsArray "srcAccessMask" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "srcAccessMask" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, srcAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "srcAccessMask" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dstAccessMask" VkBufferMemoryBarrier where
        type FieldType "dstAccessMask" VkBufferMemoryBarrier =
             VkAccessFlags
        type FieldOptional "dstAccessMask" VkBufferMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dstAccessMask" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, dstAccessMask}
        type FieldIsArray "dstAccessMask" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "dstAccessMask" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, dstAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "dstAccessMask" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         HasField "srcQueueFamilyIndex" VkBufferMemoryBarrier where
        type FieldType "srcQueueFamilyIndex" VkBufferMemoryBarrier = Word32
        type FieldOptional "srcQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "srcQueueFamilyIndex" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}
        type FieldIsArray "srcQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "srcQueueFamilyIndex" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "srcQueueFamilyIndex" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "dstQueueFamilyIndex" VkBufferMemoryBarrier where
        type FieldType "dstQueueFamilyIndex" VkBufferMemoryBarrier = Word32
        type FieldOptional "dstQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dstQueueFamilyIndex" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}
        type FieldIsArray "dstQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "dstQueueFamilyIndex" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "dstQueueFamilyIndex" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBufferMemoryBarrier where
        type FieldType "buffer" VkBufferMemoryBarrier = VkBuffer
        type FieldOptional "buffer" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, buffer}
        type FieldIsArray "buffer" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, buffer}

instance {-# OVERLAPPING #-}
         HasField "offset" VkBufferMemoryBarrier where
        type FieldType "offset" VkBufferMemoryBarrier = VkDeviceSize
        type FieldOptional "offset" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, offset}
        type FieldIsArray "offset" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, offset}

instance {-# OVERLAPPING #-} HasField "size" VkBufferMemoryBarrier
         where
        type FieldType "size" VkBufferMemoryBarrier = VkDeviceSize
        type FieldOptional "size" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, size}
        type FieldIsArray "size" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, size}

instance {-# OVERLAPPING #-}
         CanReadField "size" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, size}

instance Show VkBufferMemoryBarrier where
        showsPrec d x
          = showString "VkBufferMemoryBarrier {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "srcAccessMask = " .
                            showsPrec d (getField @"srcAccessMask" x) .
                              showString ", " .
                                showString "dstAccessMask = " .
                                  showsPrec d (getField @"dstAccessMask" x) .
                                    showString ", " .
                                      showString "srcQueueFamilyIndex = " .
                                        showsPrec d (getField @"srcQueueFamilyIndex" x) .
                                          showString ", " .
                                            showString "dstQueueFamilyIndex = " .
                                              showsPrec d (getField @"dstQueueFamilyIndex" x) .
                                                showString ", " .
                                                  showString "buffer = " .
                                                    showsPrec d (getField @"buffer" x) .
                                                      showString ", " .
                                                        showString "offset = " .
                                                          showsPrec d (getField @"offset" x) .
                                                            showString ", " .
                                                              showString "size = " .
                                                                showsPrec d (getField @"size" x) .
                                                                  showChar '}'

-- | > typedef struct VkBufferMemoryRequirementsInfo2 {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkBuffer                                                             buffer;
--   > } VkBufferMemoryRequirementsInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferMemoryRequirementsInfo2 VkBufferMemoryRequirementsInfo2 registry at www.khronos.org>
data VkBufferMemoryRequirementsInfo2 = VkBufferMemoryRequirementsInfo2## Addr##
                                                                        ByteArray##

instance Eq VkBufferMemoryRequirementsInfo2 where
        (VkBufferMemoryRequirementsInfo2## a _) ==
          x@(VkBufferMemoryRequirementsInfo2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferMemoryRequirementsInfo2 where
        (VkBufferMemoryRequirementsInfo2## a _) `compare`
          x@(VkBufferMemoryRequirementsInfo2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferMemoryRequirementsInfo2 where
        sizeOf ~_ = #{size VkBufferMemoryRequirementsInfo2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBufferMemoryRequirementsInfo2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferMemoryRequirementsInfo2 where
        unsafeAddr (VkBufferMemoryRequirementsInfo2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferMemoryRequirementsInfo2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferMemoryRequirementsInfo2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferMemoryRequirementsInfo2 where
        type StructFields VkBufferMemoryRequirementsInfo2 =
             '["sType", "pNext", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferMemoryRequirementsInfo2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBufferMemoryRequirementsInfo2 where
        type FieldType "sType" VkBufferMemoryRequirementsInfo2 =
             VkStructureType
        type FieldOptional "sType" VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferMemoryRequirementsInfo2 =
             #{offset VkBufferMemoryRequirementsInfo2, sType}
        type FieldIsArray "sType" VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBufferMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBufferMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBufferMemoryRequirementsInfo2 where
        type FieldType "pNext" VkBufferMemoryRequirementsInfo2 = Ptr Void
        type FieldOptional "pNext" VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferMemoryRequirementsInfo2 =
             #{offset VkBufferMemoryRequirementsInfo2, pNext}
        type FieldIsArray "pNext" VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBufferMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBufferMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBufferMemoryRequirementsInfo2 where
        type FieldType "buffer" VkBufferMemoryRequirementsInfo2 = VkBuffer
        type FieldOptional "buffer" VkBufferMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBufferMemoryRequirementsInfo2 =
             #{offset VkBufferMemoryRequirementsInfo2, buffer}
        type FieldIsArray "buffer" VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkBufferMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkBufferMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2, buffer}

instance Show VkBufferMemoryRequirementsInfo2 where
        showsPrec d x
          = showString "VkBufferMemoryRequirementsInfo2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) . showChar '}'

-- | Alias for `VkBufferMemoryRequirementsInfo2`
type VkBufferMemoryRequirementsInfo2KHR =
     VkBufferMemoryRequirementsInfo2

-- | > typedef struct VkBufferViewCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBufferViewCreateFlagsflags;
--   >     VkBuffer               buffer;
--   >     VkFormat               format;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           range;
--   > } VkBufferViewCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferViewCreateInfo VkBufferViewCreateInfo registry at www.khronos.org>
data VkBufferViewCreateInfo = VkBufferViewCreateInfo## Addr##
                                                      ByteArray##

instance Eq VkBufferViewCreateInfo where
        (VkBufferViewCreateInfo## a _) == x@(VkBufferViewCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferViewCreateInfo where
        (VkBufferViewCreateInfo## a _) `compare`
          x@(VkBufferViewCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferViewCreateInfo where
        sizeOf ~_ = #{size VkBufferViewCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBufferViewCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferViewCreateInfo where
        unsafeAddr (VkBufferViewCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferViewCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferViewCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferViewCreateInfo where
        type StructFields VkBufferViewCreateInfo =
             '["sType", "pNext", "flags", "buffer", "format", "offset", "range"] -- ' closing tick for hsc2hs
        type CUnionType VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferViewCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBufferViewCreateInfo where
        type FieldType "sType" VkBufferViewCreateInfo = VkStructureType
        type FieldOptional "sType" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, sType}
        type FieldIsArray "sType" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBufferViewCreateInfo where
        type FieldType "pNext" VkBufferViewCreateInfo = Ptr Void
        type FieldOptional "pNext" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, pNext}
        type FieldIsArray "pNext" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkBufferViewCreateInfo where
        type FieldType "flags" VkBufferViewCreateInfo =
             VkBufferViewCreateFlags
        type FieldOptional "flags" VkBufferViewCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, flags}
        type FieldIsArray "flags" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBufferViewCreateInfo where
        type FieldType "buffer" VkBufferViewCreateInfo = VkBuffer
        type FieldOptional "buffer" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, buffer}
        type FieldIsArray "buffer" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, buffer}

instance {-# OVERLAPPING #-}
         HasField "format" VkBufferViewCreateInfo where
        type FieldType "format" VkBufferViewCreateInfo = VkFormat
        type FieldOptional "format" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, format}
        type FieldIsArray "format" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         HasField "offset" VkBufferViewCreateInfo where
        type FieldType "offset" VkBufferViewCreateInfo = VkDeviceSize
        type FieldOptional "offset" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, offset}
        type FieldIsArray "offset" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, offset}

instance {-# OVERLAPPING #-}
         HasField "range" VkBufferViewCreateInfo where
        type FieldType "range" VkBufferViewCreateInfo = VkDeviceSize
        type FieldOptional "range" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "range" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, range}
        type FieldIsArray "range" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, range}

instance {-# OVERLAPPING #-}
         CanReadField "range" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, range})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, range}

instance {-# OVERLAPPING #-}
         CanWriteField "range" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, range}

instance Show VkBufferViewCreateInfo where
        showsPrec d x
          = showString "VkBufferViewCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "buffer = " .
                                  showsPrec d (getField @"buffer" x) .
                                    showString ", " .
                                      showString "format = " .
                                        showsPrec d (getField @"format" x) .
                                          showString ", " .
                                            showString "offset = " .
                                              showsPrec d (getField @"offset" x) .
                                                showString ", " .
                                                  showString "range = " .
                                                    showsPrec d (getField @"range" x) . showChar '}'
