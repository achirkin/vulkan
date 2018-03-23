#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageCreateInfo
       (VkImageCreateInfo(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormat           (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags (VkImageCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkImageLayout      (VkImageLayout)
import           Graphics.Vulkan.Types.Enum.VkImageTiling      (VkImageTiling)
import           Graphics.Vulkan.Types.Enum.VkImageType        (VkImageType)
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags  (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.Enum.VkSharingMode      (VkSharingMode)
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkExtent3D       (VkExtent3D)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkImageCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkImageCreateFlags     flags;
--   >     VkImageType            imageType;
--   >     VkFormat               format;
--   >     VkExtent3D             extent;
--   >     uint32_t               mipLevels;
--   >     uint32_t               arrayLayers;
--   >     VkSampleCountFlagBits  samples;
--   >     VkImageTiling          tiling;
--   >     VkImageUsageFlags      usage;
--   >     VkSharingMode          sharingMode;
--   >     uint32_t               queueFamilyIndexCount;
--   >     const uint32_t*        pQueueFamilyIndices;
--   >     VkImageLayout          initialLayout;
--   > } VkImageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkImageCreateInfo.html VkImageCreateInfo registry at www.khronos.org>
data VkImageCreateInfo = VkImageCreateInfo## Addr## ByteArray##

instance Eq VkImageCreateInfo where
        (VkImageCreateInfo## a _) == x@(VkImageCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageCreateInfo where
        (VkImageCreateInfo## a _) `compare` x@(VkImageCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageCreateInfo where
        sizeOf ~_ = #{size VkImageCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageCreateInfo where
        unsafeAddr (VkImageCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageCreateInfo where
        type StructFields VkImageCreateInfo =
             '["sType", "pNext", "flags", "imageType", "format", "extent", -- ' closing tick for hsc2hs
               "mipLevels", "arrayLayers", "samples", "tiling", "usage",
               "sharingMode", "queueFamilyIndexCount", "pQueueFamilyIndices",
               "initialLayout"]
        type CUnionType VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkImageCreateInfo
         where
        type FieldType "sType" VkImageCreateInfo = VkStructureType
        type FieldOptional "sType" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageCreateInfo =
             #{offset VkImageCreateInfo, sType}
        type FieldIsArray "sType" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, sType}

instance {-# OVERLAPPING #-} CanReadField "sType" VkImageCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkImageCreateInfo
         where
        type FieldType "pNext" VkImageCreateInfo = Ptr Void
        type FieldOptional "pNext" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageCreateInfo =
             #{offset VkImageCreateInfo, pNext}
        type FieldIsArray "pNext" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, pNext}

instance {-# OVERLAPPING #-} CanReadField "pNext" VkImageCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkImageCreateInfo
         where
        type FieldType "flags" VkImageCreateInfo = VkImageCreateFlags
        type FieldOptional "flags" VkImageCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImageCreateInfo =
             #{offset VkImageCreateInfo, flags}
        type FieldIsArray "flags" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, flags}

instance {-# OVERLAPPING #-} CanReadField "flags" VkImageCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, flags}

instance {-# OVERLAPPING #-} HasField "imageType" VkImageCreateInfo
         where
        type FieldType "imageType" VkImageCreateInfo = VkImageType
        type FieldOptional "imageType" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageType" VkImageCreateInfo =
             #{offset VkImageCreateInfo, imageType}
        type FieldIsArray "imageType" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, imageType}

instance {-# OVERLAPPING #-}
         CanReadField "imageType" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, imageType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, imageType}

instance {-# OVERLAPPING #-}
         CanWriteField "imageType" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, imageType}

instance {-# OVERLAPPING #-} HasField "format" VkImageCreateInfo
         where
        type FieldType "format" VkImageCreateInfo = VkFormat
        type FieldOptional "format" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkImageCreateInfo =
             #{offset VkImageCreateInfo, format}
        type FieldIsArray "format" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, format}

instance {-# OVERLAPPING #-} HasField "extent" VkImageCreateInfo
         where
        type FieldType "extent" VkImageCreateInfo = VkExtent3D
        type FieldOptional "extent" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkImageCreateInfo =
             #{offset VkImageCreateInfo, extent}
        type FieldIsArray "extent" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, extent}

instance {-# OVERLAPPING #-}
         CanReadField "extent" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, extent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, extent}

instance {-# OVERLAPPING #-}
         CanWriteField "extent" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, extent}

instance {-# OVERLAPPING #-} HasField "mipLevels" VkImageCreateInfo
         where
        type FieldType "mipLevels" VkImageCreateInfo = Word32
        type FieldOptional "mipLevels" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mipLevels" VkImageCreateInfo =
             #{offset VkImageCreateInfo, mipLevels}
        type FieldIsArray "mipLevels" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, mipLevels}

instance {-# OVERLAPPING #-}
         CanReadField "mipLevels" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, mipLevels})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, mipLevels}

instance {-# OVERLAPPING #-}
         CanWriteField "mipLevels" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, mipLevels}

instance {-# OVERLAPPING #-}
         HasField "arrayLayers" VkImageCreateInfo where
        type FieldType "arrayLayers" VkImageCreateInfo = Word32
        type FieldOptional "arrayLayers" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "arrayLayers" VkImageCreateInfo =
             #{offset VkImageCreateInfo, arrayLayers}
        type FieldIsArray "arrayLayers" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, arrayLayers}

instance {-# OVERLAPPING #-}
         CanReadField "arrayLayers" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, arrayLayers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, arrayLayers}

instance {-# OVERLAPPING #-}
         CanWriteField "arrayLayers" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, arrayLayers}

instance {-# OVERLAPPING #-} HasField "samples" VkImageCreateInfo
         where
        type FieldType "samples" VkImageCreateInfo = VkSampleCountFlagBits
        type FieldOptional "samples" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "samples" VkImageCreateInfo =
             #{offset VkImageCreateInfo, samples}
        type FieldIsArray "samples" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, samples}

instance {-# OVERLAPPING #-}
         CanReadField "samples" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, samples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, samples}

instance {-# OVERLAPPING #-}
         CanWriteField "samples" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, samples}

instance {-# OVERLAPPING #-} HasField "tiling" VkImageCreateInfo
         where
        type FieldType "tiling" VkImageCreateInfo = VkImageTiling
        type FieldOptional "tiling" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tiling" VkImageCreateInfo =
             #{offset VkImageCreateInfo, tiling}
        type FieldIsArray "tiling" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, tiling}

instance {-# OVERLAPPING #-}
         CanReadField "tiling" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, tiling})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, tiling}

instance {-# OVERLAPPING #-}
         CanWriteField "tiling" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, tiling}

instance {-# OVERLAPPING #-} HasField "usage" VkImageCreateInfo
         where
        type FieldType "usage" VkImageCreateInfo = VkImageUsageFlags
        type FieldOptional "usage" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkImageCreateInfo =
             #{offset VkImageCreateInfo, usage}
        type FieldIsArray "usage" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, usage}

instance {-# OVERLAPPING #-} CanReadField "usage" VkImageCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, usage}

instance {-# OVERLAPPING #-}
         HasField "sharingMode" VkImageCreateInfo where
        type FieldType "sharingMode" VkImageCreateInfo = VkSharingMode
        type FieldOptional "sharingMode" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sharingMode" VkImageCreateInfo =
             #{offset VkImageCreateInfo, sharingMode}
        type FieldIsArray "sharingMode" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, sharingMode}

instance {-# OVERLAPPING #-}
         CanReadField "sharingMode" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, sharingMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, sharingMode}

instance {-# OVERLAPPING #-}
         CanWriteField "sharingMode" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, sharingMode}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndexCount" VkImageCreateInfo where
        type FieldType "queueFamilyIndexCount" VkImageCreateInfo = Word32
        type FieldOptional "queueFamilyIndexCount" VkImageCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndexCount" VkImageCreateInfo =
             #{offset VkImageCreateInfo, queueFamilyIndexCount}
        type FieldIsArray "queueFamilyIndexCount" VkImageCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageCreateInfo, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyIndexCount" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, queueFamilyIndexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyIndexCount" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         HasField "pQueueFamilyIndices" VkImageCreateInfo where
        type FieldType "pQueueFamilyIndices" VkImageCreateInfo = Ptr Word32
        type FieldOptional "pQueueFamilyIndices" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pQueueFamilyIndices" VkImageCreateInfo =
             #{offset VkImageCreateInfo, pQueueFamilyIndices}
        type FieldIsArray "pQueueFamilyIndices" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageCreateInfo, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pQueueFamilyIndices" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, pQueueFamilyIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pQueueFamilyIndices" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         HasField "initialLayout" VkImageCreateInfo where
        type FieldType "initialLayout" VkImageCreateInfo = VkImageLayout
        type FieldOptional "initialLayout" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "initialLayout" VkImageCreateInfo =
             #{offset VkImageCreateInfo, initialLayout}
        type FieldIsArray "initialLayout" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageCreateInfo, initialLayout}

instance {-# OVERLAPPING #-}
         CanReadField "initialLayout" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, initialLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, initialLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "initialLayout" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, initialLayout}

instance Show VkImageCreateInfo where
        showsPrec d x
          = showString "VkImageCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "imageType = " .
                                  showsPrec d (getField @"imageType" x) .
                                    showString ", " .
                                      showString "format = " .
                                        showsPrec d (getField @"format" x) .
                                          showString ", " .
                                            showString "extent = " .
                                              showsPrec d (getField @"extent" x) .
                                                showString ", " .
                                                  showString "mipLevels = " .
                                                    showsPrec d (getField @"mipLevels" x) .
                                                      showString ", " .
                                                        showString "arrayLayers = " .
                                                          showsPrec d (getField @"arrayLayers" x) .
                                                            showString ", " .
                                                              showString "samples = " .
                                                                showsPrec d (getField @"samples" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "tiling = " .
                                                                      showsPrec d
                                                                        (getField @"tiling" x)
                                                                        .
                                                                        showString ", " .
                                                                          showString "usage = " .
                                                                            showsPrec d
                                                                              (getField @"usage" x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "sharingMode = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"sharingMode"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "queueFamilyIndexCount = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"queueFamilyIndexCount"
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "pQueueFamilyIndices = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (getField
                                                                                                   @"pQueueFamilyIndices"
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "initialLayout = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (getField
                                                                                                         @"initialLayout"
                                                                                                         x)
                                                                                                      .
                                                                                                      showChar
                                                                                                        '}'
