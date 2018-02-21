#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageCreateInfo.html VkImageCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkImageCreateInfo where
        type VkSTypeMType VkImageCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageCreateInfo, sType}

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

instance CanReadField "sType" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkImageCreateInfo where
        type VkPNextMType VkImageCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageCreateInfo, pNext}

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

instance CanReadField "pNext" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkImageCreateInfo where
        type VkFlagsMType VkImageCreateInfo = VkImageCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkImageCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkImageCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkImageCreateInfo, flags}

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

instance CanReadField "flags" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkImageType VkImageCreateInfo where
        type VkImageTypeMType VkImageCreateInfo = VkImageType

        {-# NOINLINE vkImageType #-}
        vkImageType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, imageType})

        {-# INLINE vkImageTypeByteOffset #-}
        vkImageTypeByteOffset ~_
          = #{offset VkImageCreateInfo, imageType}

        {-# INLINE readVkImageType #-}
        readVkImageType p
          = peekByteOff p #{offset VkImageCreateInfo, imageType}

        {-# INLINE writeVkImageType #-}
        writeVkImageType p
          = pokeByteOff p #{offset VkImageCreateInfo, imageType}

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

instance CanReadField "imageType" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkImageType

        {-# INLINE readField #-}
        readField = readVkImageType

instance CanWriteField "imageType" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkImageType

instance {-# OVERLAPPING #-} HasVkFormat VkImageCreateInfo where
        type VkFormatMType VkImageCreateInfo = VkFormat

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkImageCreateInfo, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkImageCreateInfo, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkImageCreateInfo, format}

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

instance CanReadField "format" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-} HasVkExtent VkImageCreateInfo where
        type VkExtentMType VkImageCreateInfo = VkExtent3D

        {-# NOINLINE vkExtent #-}
        vkExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, extent})

        {-# INLINE vkExtentByteOffset #-}
        vkExtentByteOffset ~_
          = #{offset VkImageCreateInfo, extent}

        {-# INLINE readVkExtent #-}
        readVkExtent p
          = peekByteOff p #{offset VkImageCreateInfo, extent}

        {-# INLINE writeVkExtent #-}
        writeVkExtent p
          = pokeByteOff p #{offset VkImageCreateInfo, extent}

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

instance CanReadField "extent" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkExtent

        {-# INLINE readField #-}
        readField = readVkExtent

instance CanWriteField "extent" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkExtent

instance {-# OVERLAPPING #-} HasVkMipLevels VkImageCreateInfo where
        type VkMipLevelsMType VkImageCreateInfo = Word32

        {-# NOINLINE vkMipLevels #-}
        vkMipLevels x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, mipLevels})

        {-# INLINE vkMipLevelsByteOffset #-}
        vkMipLevelsByteOffset ~_
          = #{offset VkImageCreateInfo, mipLevels}

        {-# INLINE readVkMipLevels #-}
        readVkMipLevels p
          = peekByteOff p #{offset VkImageCreateInfo, mipLevels}

        {-# INLINE writeVkMipLevels #-}
        writeVkMipLevels p
          = pokeByteOff p #{offset VkImageCreateInfo, mipLevels}

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

instance CanReadField "mipLevels" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkMipLevels

        {-# INLINE readField #-}
        readField = readVkMipLevels

instance CanWriteField "mipLevels" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMipLevels

instance {-# OVERLAPPING #-} HasVkArrayLayers VkImageCreateInfo
         where
        type VkArrayLayersMType VkImageCreateInfo = Word32

        {-# NOINLINE vkArrayLayers #-}
        vkArrayLayers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, arrayLayers})

        {-# INLINE vkArrayLayersByteOffset #-}
        vkArrayLayersByteOffset ~_
          = #{offset VkImageCreateInfo, arrayLayers}

        {-# INLINE readVkArrayLayers #-}
        readVkArrayLayers p
          = peekByteOff p #{offset VkImageCreateInfo, arrayLayers}

        {-# INLINE writeVkArrayLayers #-}
        writeVkArrayLayers p
          = pokeByteOff p #{offset VkImageCreateInfo, arrayLayers}

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

instance CanReadField "arrayLayers" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkArrayLayers

        {-# INLINE readField #-}
        readField = readVkArrayLayers

instance CanWriteField "arrayLayers" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkArrayLayers

instance {-# OVERLAPPING #-} HasVkSamples VkImageCreateInfo where
        type VkSamplesMType VkImageCreateInfo = VkSampleCountFlagBits

        {-# NOINLINE vkSamples #-}
        vkSamples x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, samples})

        {-# INLINE vkSamplesByteOffset #-}
        vkSamplesByteOffset ~_
          = #{offset VkImageCreateInfo, samples}

        {-# INLINE readVkSamples #-}
        readVkSamples p
          = peekByteOff p #{offset VkImageCreateInfo, samples}

        {-# INLINE writeVkSamples #-}
        writeVkSamples p
          = pokeByteOff p #{offset VkImageCreateInfo, samples}

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

instance CanReadField "samples" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkSamples

        {-# INLINE readField #-}
        readField = readVkSamples

instance CanWriteField "samples" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSamples

instance {-# OVERLAPPING #-} HasVkTiling VkImageCreateInfo where
        type VkTilingMType VkImageCreateInfo = VkImageTiling

        {-# NOINLINE vkTiling #-}
        vkTiling x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, tiling})

        {-# INLINE vkTilingByteOffset #-}
        vkTilingByteOffset ~_
          = #{offset VkImageCreateInfo, tiling}

        {-# INLINE readVkTiling #-}
        readVkTiling p
          = peekByteOff p #{offset VkImageCreateInfo, tiling}

        {-# INLINE writeVkTiling #-}
        writeVkTiling p
          = pokeByteOff p #{offset VkImageCreateInfo, tiling}

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

instance CanReadField "tiling" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkTiling

        {-# INLINE readField #-}
        readField = readVkTiling

instance CanWriteField "tiling" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkTiling

instance {-# OVERLAPPING #-} HasVkUsage VkImageCreateInfo where
        type VkUsageMType VkImageCreateInfo = VkImageUsageFlags

        {-# NOINLINE vkUsage #-}
        vkUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, usage})

        {-# INLINE vkUsageByteOffset #-}
        vkUsageByteOffset ~_
          = #{offset VkImageCreateInfo, usage}

        {-# INLINE readVkUsage #-}
        readVkUsage p
          = peekByteOff p #{offset VkImageCreateInfo, usage}

        {-# INLINE writeVkUsage #-}
        writeVkUsage p
          = pokeByteOff p #{offset VkImageCreateInfo, usage}

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

instance CanReadField "usage" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkUsage

        {-# INLINE readField #-}
        readField = readVkUsage

instance CanWriteField "usage" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkUsage

instance {-# OVERLAPPING #-} HasVkSharingMode VkImageCreateInfo
         where
        type VkSharingModeMType VkImageCreateInfo = VkSharingMode

        {-# NOINLINE vkSharingMode #-}
        vkSharingMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, sharingMode})

        {-# INLINE vkSharingModeByteOffset #-}
        vkSharingModeByteOffset ~_
          = #{offset VkImageCreateInfo, sharingMode}

        {-# INLINE readVkSharingMode #-}
        readVkSharingMode p
          = peekByteOff p #{offset VkImageCreateInfo, sharingMode}

        {-# INLINE writeVkSharingMode #-}
        writeVkSharingMode p
          = pokeByteOff p #{offset VkImageCreateInfo, sharingMode}

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

instance CanReadField "sharingMode" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkSharingMode

        {-# INLINE readField #-}
        readField = readVkSharingMode

instance CanWriteField "sharingMode" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSharingMode

instance {-# OVERLAPPING #-}
         HasVkQueueFamilyIndexCount VkImageCreateInfo where
        type VkQueueFamilyIndexCountMType VkImageCreateInfo = Word32

        {-# NOINLINE vkQueueFamilyIndexCount #-}
        vkQueueFamilyIndexCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, queueFamilyIndexCount})

        {-# INLINE vkQueueFamilyIndexCountByteOffset #-}
        vkQueueFamilyIndexCountByteOffset ~_
          = #{offset VkImageCreateInfo, queueFamilyIndexCount}

        {-# INLINE readVkQueueFamilyIndexCount #-}
        readVkQueueFamilyIndexCount p
          = peekByteOff p #{offset VkImageCreateInfo, queueFamilyIndexCount}

        {-# INLINE writeVkQueueFamilyIndexCount #-}
        writeVkQueueFamilyIndexCount p
          = pokeByteOff p #{offset VkImageCreateInfo, queueFamilyIndexCount}

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

instance CanReadField "queueFamilyIndexCount" VkImageCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkQueueFamilyIndexCount

        {-# INLINE readField #-}
        readField = readVkQueueFamilyIndexCount

instance CanWriteField "queueFamilyIndexCount" VkImageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkQueueFamilyIndexCount

instance {-# OVERLAPPING #-}
         HasVkPQueueFamilyIndices VkImageCreateInfo where
        type VkPQueueFamilyIndicesMType VkImageCreateInfo = Ptr Word32

        {-# NOINLINE vkPQueueFamilyIndices #-}
        vkPQueueFamilyIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, pQueueFamilyIndices})

        {-# INLINE vkPQueueFamilyIndicesByteOffset #-}
        vkPQueueFamilyIndicesByteOffset ~_
          = #{offset VkImageCreateInfo, pQueueFamilyIndices}

        {-# INLINE readVkPQueueFamilyIndices #-}
        readVkPQueueFamilyIndices p
          = peekByteOff p #{offset VkImageCreateInfo, pQueueFamilyIndices}

        {-# INLINE writeVkPQueueFamilyIndices #-}
        writeVkPQueueFamilyIndices p
          = pokeByteOff p #{offset VkImageCreateInfo, pQueueFamilyIndices}

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

instance CanReadField "pQueueFamilyIndices" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkPQueueFamilyIndices

        {-# INLINE readField #-}
        readField = readVkPQueueFamilyIndices

instance CanWriteField "pQueueFamilyIndices" VkImageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPQueueFamilyIndices

instance {-# OVERLAPPING #-} HasVkInitialLayout VkImageCreateInfo
         where
        type VkInitialLayoutMType VkImageCreateInfo = VkImageLayout

        {-# NOINLINE vkInitialLayout #-}
        vkInitialLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, initialLayout})

        {-# INLINE vkInitialLayoutByteOffset #-}
        vkInitialLayoutByteOffset ~_
          = #{offset VkImageCreateInfo, initialLayout}

        {-# INLINE readVkInitialLayout #-}
        readVkInitialLayout p
          = peekByteOff p #{offset VkImageCreateInfo, initialLayout}

        {-# INLINE writeVkInitialLayout #-}
        writeVkInitialLayout p
          = pokeByteOff p #{offset VkImageCreateInfo, initialLayout}

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

instance CanReadField "initialLayout" VkImageCreateInfo where
        {-# INLINE getField #-}
        getField = vkInitialLayout

        {-# INLINE readField #-}
        readField = readVkInitialLayout

instance CanWriteField "initialLayout" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkInitialLayout

instance Show VkImageCreateInfo where
        showsPrec d x
          = showString "VkImageCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkImageType = " .
                                  showsPrec d (vkImageType x) .
                                    showString ", " .
                                      showString "vkFormat = " .
                                        showsPrec d (vkFormat x) .
                                          showString ", " .
                                            showString "vkExtent = " .
                                              showsPrec d (vkExtent x) .
                                                showString ", " .
                                                  showString "vkMipLevels = " .
                                                    showsPrec d (vkMipLevels x) .
                                                      showString ", " .
                                                        showString "vkArrayLayers = " .
                                                          showsPrec d (vkArrayLayers x) .
                                                            showString ", " .
                                                              showString "vkSamples = " .
                                                                showsPrec d (vkSamples x) .
                                                                  showString ", " .
                                                                    showString "vkTiling = " .
                                                                      showsPrec d (vkTiling x) .
                                                                        showString ", " .
                                                                          showString "vkUsage = " .
                                                                            showsPrec d (vkUsage x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkSharingMode = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkSharingMode
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "vkQueueFamilyIndexCount = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (vkQueueFamilyIndexCount
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "vkPQueueFamilyIndices = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (vkPQueueFamilyIndices
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "vkInitialLayout = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (vkInitialLayout
                                                                                                         x)
                                                                                                      .
                                                                                                      showChar
                                                                                                        '}'
