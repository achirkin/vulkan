#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferCreateInfo
       (VkBufferCreateInfo(..)) where
import           Foreign.Storable                               (Storable (..))
import           GHC.Base                                       (Addr##,
                                                                 ByteArray##,
                                                                 byteArrayContents##,
                                                                 plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkBufferCreateFlags (VkBufferCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkBufferUsageFlags  (VkBufferUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkSharingMode       (VkSharingMode)
import           Graphics.Vulkan.Types.Enum.VkStructureType     (VkStructureType)
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkBufferCreateInfoVkBufferCreateInfo registry at www.khronos.org>
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
