#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Sampler
       (VkSamplerCreateInfo(..), VkSamplerReductionModeCreateInfoEXT(..),
        VkSamplerYcbcrConversionCreateInfo(..),
        VkSamplerYcbcrConversionCreateInfoKHR,
        VkSamplerYcbcrConversionImageFormatProperties(..),
        VkSamplerYcbcrConversionImageFormatPropertiesKHR,
        VkSamplerYcbcrConversionInfo(..), VkSamplerYcbcrConversionInfoKHR)
       where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks                (VkSamplerCreateFlags)
import           Graphics.Vulkan.Types.Enum.BorderColor        (VkBorderColor)
import           Graphics.Vulkan.Types.Enum.ChromaLocation     (VkChromaLocation)
import           Graphics.Vulkan.Types.Enum.CompareOp          (VkCompareOp)
import           Graphics.Vulkan.Types.Enum.Filter             (VkFilter)
import           Graphics.Vulkan.Types.Enum.Format             (VkFormat)
import           Graphics.Vulkan.Types.Enum.Sampler            (VkSamplerAddressMode,
                                                                VkSamplerMipmapMode,
                                                                VkSamplerReductionModeEXT,
                                                                VkSamplerYcbcrModelConversion,
                                                                VkSamplerYcbcrRange)
import           Graphics.Vulkan.Types.Enum.StructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                 (VkSamplerYcbcrConversion)
import           Graphics.Vulkan.Types.Struct.ComponentMapping (VkComponentMapping)
import           Graphics.Vulkan.Types.Struct.Image            (VkImageFormatProperties2,
                                                                VkImageViewCreateInfo)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkSamplerCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSamplerCreateFlags   flags;
--   >     VkFilter               magFilter;
--   >     VkFilter               minFilter;
--   >     VkSamplerMipmapMode    mipmapMode;
--   >     VkSamplerAddressMode   addressModeU;
--   >     VkSamplerAddressMode   addressModeV;
--   >     VkSamplerAddressMode   addressModeW;
--   >     float                  mipLodBias;
--   >     VkBool32               anisotropyEnable;
--   >     float                  maxAnisotropy;
--   >     VkBool32               compareEnable;
--   >     VkCompareOp            compareOp;
--   >     float                  minLod;
--   >     float                  maxLod;
--   >     VkBorderColor          borderColor;
--   >     VkBool32               unnormalizedCoordinates;
--   > } VkSamplerCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerCreateInfo VkSamplerCreateInfo registry at www.khronos.org>
data VkSamplerCreateInfo = VkSamplerCreateInfo## Addr## ByteArray##

instance Eq VkSamplerCreateInfo where
        (VkSamplerCreateInfo## a _) == x@(VkSamplerCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerCreateInfo where
        (VkSamplerCreateInfo## a _) `compare` x@(VkSamplerCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerCreateInfo where
        sizeOf ~_ = #{size VkSamplerCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSamplerCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSamplerCreateInfo where
        unsafeAddr (VkSamplerCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSamplerCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSamplerCreateInfo where
        type StructFields VkSamplerCreateInfo =
             '["sType", "pNext", "flags", "magFilter", "minFilter", -- ' closing tick for hsc2hs
               "mipmapMode", "addressModeU", "addressModeV", "addressModeW",
               "mipLodBias", "anisotropyEnable", "maxAnisotropy", "compareEnable",
               "compareOp", "minLod", "maxLod", "borderColor",
               "unnormalizedCoordinates"]
        type CUnionType VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSamplerCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkSamplerCreateInfo
         where
        type FieldType "sType" VkSamplerCreateInfo = VkStructureType
        type FieldOptional "sType" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, sType}
        type FieldIsArray "sType" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSamplerCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkSamplerCreateInfo
         where
        type FieldType "pNext" VkSamplerCreateInfo = Ptr Void
        type FieldOptional "pNext" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, pNext}
        type FieldIsArray "pNext" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSamplerCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkSamplerCreateInfo
         where
        type FieldType "flags" VkSamplerCreateInfo = VkSamplerCreateFlags
        type FieldOptional "flags" VkSamplerCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, flags}
        type FieldIsArray "flags" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSamplerCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "magFilter" VkSamplerCreateInfo where
        type FieldType "magFilter" VkSamplerCreateInfo = VkFilter
        type FieldOptional "magFilter" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "magFilter" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, magFilter}
        type FieldIsArray "magFilter" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSamplerCreateInfo, magFilter}

instance {-# OVERLAPPING #-}
         CanReadField "magFilter" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, magFilter})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, magFilter}

instance {-# OVERLAPPING #-}
         CanWriteField "magFilter" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, magFilter}

instance {-# OVERLAPPING #-}
         HasField "minFilter" VkSamplerCreateInfo where
        type FieldType "minFilter" VkSamplerCreateInfo = VkFilter
        type FieldOptional "minFilter" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minFilter" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, minFilter}
        type FieldIsArray "minFilter" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSamplerCreateInfo, minFilter}

instance {-# OVERLAPPING #-}
         CanReadField "minFilter" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, minFilter})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, minFilter}

instance {-# OVERLAPPING #-}
         CanWriteField "minFilter" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, minFilter}

instance {-# OVERLAPPING #-}
         HasField "mipmapMode" VkSamplerCreateInfo where
        type FieldType "mipmapMode" VkSamplerCreateInfo =
             VkSamplerMipmapMode
        type FieldOptional "mipmapMode" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mipmapMode" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, mipmapMode}
        type FieldIsArray "mipmapMode" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSamplerCreateInfo, mipmapMode}

instance {-# OVERLAPPING #-}
         CanReadField "mipmapMode" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, mipmapMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, mipmapMode}

instance {-# OVERLAPPING #-}
         CanWriteField "mipmapMode" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, mipmapMode}

instance {-# OVERLAPPING #-}
         HasField "addressModeU" VkSamplerCreateInfo where
        type FieldType "addressModeU" VkSamplerCreateInfo =
             VkSamplerAddressMode
        type FieldOptional "addressModeU" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "addressModeU" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, addressModeU}
        type FieldIsArray "addressModeU" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerCreateInfo, addressModeU}

instance {-# OVERLAPPING #-}
         CanReadField "addressModeU" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, addressModeU})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, addressModeU}

instance {-# OVERLAPPING #-}
         CanWriteField "addressModeU" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, addressModeU}

instance {-# OVERLAPPING #-}
         HasField "addressModeV" VkSamplerCreateInfo where
        type FieldType "addressModeV" VkSamplerCreateInfo =
             VkSamplerAddressMode
        type FieldOptional "addressModeV" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "addressModeV" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, addressModeV}
        type FieldIsArray "addressModeV" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerCreateInfo, addressModeV}

instance {-# OVERLAPPING #-}
         CanReadField "addressModeV" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, addressModeV})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, addressModeV}

instance {-# OVERLAPPING #-}
         CanWriteField "addressModeV" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, addressModeV}

instance {-# OVERLAPPING #-}
         HasField "addressModeW" VkSamplerCreateInfo where
        type FieldType "addressModeW" VkSamplerCreateInfo =
             VkSamplerAddressMode
        type FieldOptional "addressModeW" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "addressModeW" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, addressModeW}
        type FieldIsArray "addressModeW" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerCreateInfo, addressModeW}

instance {-# OVERLAPPING #-}
         CanReadField "addressModeW" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, addressModeW})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, addressModeW}

instance {-# OVERLAPPING #-}
         CanWriteField "addressModeW" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, addressModeW}

instance {-# OVERLAPPING #-}
         HasField "mipLodBias" VkSamplerCreateInfo where
        type FieldType "mipLodBias" VkSamplerCreateInfo =
             #{type float}
        type FieldOptional "mipLodBias" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mipLodBias" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, mipLodBias}
        type FieldIsArray "mipLodBias" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSamplerCreateInfo, mipLodBias}

instance {-# OVERLAPPING #-}
         CanReadField "mipLodBias" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, mipLodBias})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, mipLodBias}

instance {-# OVERLAPPING #-}
         CanWriteField "mipLodBias" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, mipLodBias}

instance {-# OVERLAPPING #-}
         HasField "anisotropyEnable" VkSamplerCreateInfo where
        type FieldType "anisotropyEnable" VkSamplerCreateInfo = VkBool32
        type FieldOptional "anisotropyEnable" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "anisotropyEnable" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, anisotropyEnable}
        type FieldIsArray "anisotropyEnable" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerCreateInfo, anisotropyEnable}

instance {-# OVERLAPPING #-}
         CanReadField "anisotropyEnable" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, anisotropyEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, anisotropyEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "anisotropyEnable" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, anisotropyEnable}

instance {-# OVERLAPPING #-}
         HasField "maxAnisotropy" VkSamplerCreateInfo where
        type FieldType "maxAnisotropy" VkSamplerCreateInfo =
             #{type float}
        type FieldOptional "maxAnisotropy" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxAnisotropy" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, maxAnisotropy}
        type FieldIsArray "maxAnisotropy" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerCreateInfo, maxAnisotropy}

instance {-# OVERLAPPING #-}
         CanReadField "maxAnisotropy" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, maxAnisotropy})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, maxAnisotropy}

instance {-# OVERLAPPING #-}
         CanWriteField "maxAnisotropy" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, maxAnisotropy}

instance {-# OVERLAPPING #-}
         HasField "compareEnable" VkSamplerCreateInfo where
        type FieldType "compareEnable" VkSamplerCreateInfo = VkBool32
        type FieldOptional "compareEnable" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compareEnable" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, compareEnable}
        type FieldIsArray "compareEnable" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerCreateInfo, compareEnable}

instance {-# OVERLAPPING #-}
         CanReadField "compareEnable" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, compareEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, compareEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "compareEnable" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, compareEnable}

instance {-# OVERLAPPING #-}
         HasField "compareOp" VkSamplerCreateInfo where
        type FieldType "compareOp" VkSamplerCreateInfo = VkCompareOp
        type FieldOptional "compareOp" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compareOp" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, compareOp}
        type FieldIsArray "compareOp" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSamplerCreateInfo, compareOp}

instance {-# OVERLAPPING #-}
         CanReadField "compareOp" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, compareOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, compareOp}

instance {-# OVERLAPPING #-}
         CanWriteField "compareOp" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, compareOp}

instance {-# OVERLAPPING #-} HasField "minLod" VkSamplerCreateInfo
         where
        type FieldType "minLod" VkSamplerCreateInfo =
             #{type float}
        type FieldOptional "minLod" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minLod" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, minLod}
        type FieldIsArray "minLod" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSamplerCreateInfo, minLod}

instance {-# OVERLAPPING #-}
         CanReadField "minLod" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, minLod})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, minLod}

instance {-# OVERLAPPING #-}
         CanWriteField "minLod" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, minLod}

instance {-# OVERLAPPING #-} HasField "maxLod" VkSamplerCreateInfo
         where
        type FieldType "maxLod" VkSamplerCreateInfo =
             #{type float}
        type FieldOptional "maxLod" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxLod" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, maxLod}
        type FieldIsArray "maxLod" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSamplerCreateInfo, maxLod}

instance {-# OVERLAPPING #-}
         CanReadField "maxLod" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, maxLod})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, maxLod}

instance {-# OVERLAPPING #-}
         CanWriteField "maxLod" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, maxLod}

instance {-# OVERLAPPING #-}
         HasField "borderColor" VkSamplerCreateInfo where
        type FieldType "borderColor" VkSamplerCreateInfo = VkBorderColor
        type FieldOptional "borderColor" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "borderColor" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, borderColor}
        type FieldIsArray "borderColor" VkSamplerCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerCreateInfo, borderColor}

instance {-# OVERLAPPING #-}
         CanReadField "borderColor" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, borderColor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, borderColor}

instance {-# OVERLAPPING #-}
         CanWriteField "borderColor" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, borderColor}

instance {-# OVERLAPPING #-}
         HasField "unnormalizedCoordinates" VkSamplerCreateInfo where
        type FieldType "unnormalizedCoordinates" VkSamplerCreateInfo =
             VkBool32
        type FieldOptional "unnormalizedCoordinates" VkSamplerCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "unnormalizedCoordinates" VkSamplerCreateInfo =
             #{offset VkSamplerCreateInfo, unnormalizedCoordinates}
        type FieldIsArray "unnormalizedCoordinates" VkSamplerCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerCreateInfo, unnormalizedCoordinates}

instance {-# OVERLAPPING #-}
         CanReadField "unnormalizedCoordinates" VkSamplerCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, unnormalizedCoordinates})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerCreateInfo, unnormalizedCoordinates}

instance {-# OVERLAPPING #-}
         CanWriteField "unnormalizedCoordinates" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerCreateInfo, unnormalizedCoordinates}

instance Show VkSamplerCreateInfo where
        showsPrec d x
          = showString "VkSamplerCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "magFilter = " .
                                  showsPrec d (getField @"magFilter" x) .
                                    showString ", " .
                                      showString "minFilter = " .
                                        showsPrec d (getField @"minFilter" x) .
                                          showString ", " .
                                            showString "mipmapMode = " .
                                              showsPrec d (getField @"mipmapMode" x) .
                                                showString ", " .
                                                  showString "addressModeU = " .
                                                    showsPrec d (getField @"addressModeU" x) .
                                                      showString ", " .
                                                        showString "addressModeV = " .
                                                          showsPrec d (getField @"addressModeV" x) .
                                                            showString ", " .
                                                              showString "addressModeW = " .
                                                                showsPrec d
                                                                  (getField @"addressModeW" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "mipLodBias = " .
                                                                      showsPrec d
                                                                        (getField @"mipLodBias" x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "anisotropyEnable = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"anisotropyEnable"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "maxAnisotropy = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"maxAnisotropy"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "compareEnable = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"compareEnable"
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "compareOp = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (getField
                                                                                                   @"compareOp"
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "minLod = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (getField
                                                                                                         @"minLod"
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "maxLod = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (getField
                                                                                                               @"maxLod"
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "borderColor = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (getField
                                                                                                                     @"borderColor"
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "unnormalizedCoordinates = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (getField
                                                                                                                           @"unnormalizedCoordinates"
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showChar
                                                                                                                          '}'

-- | > typedef struct VkSamplerReductionModeCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSamplerReductionModeEXT reductionMode;
--   > } VkSamplerReductionModeCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerReductionModeCreateInfoEXT VkSamplerReductionModeCreateInfoEXT registry at www.khronos.org>
data VkSamplerReductionModeCreateInfoEXT = VkSamplerReductionModeCreateInfoEXT## Addr##
                                                                                ByteArray##

instance Eq VkSamplerReductionModeCreateInfoEXT where
        (VkSamplerReductionModeCreateInfoEXT## a _) ==
          x@(VkSamplerReductionModeCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerReductionModeCreateInfoEXT where
        (VkSamplerReductionModeCreateInfoEXT## a _) `compare`
          x@(VkSamplerReductionModeCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerReductionModeCreateInfoEXT where
        sizeOf ~_ = #{size VkSamplerReductionModeCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerReductionModeCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSamplerReductionModeCreateInfoEXT
         where
        unsafeAddr (VkSamplerReductionModeCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSamplerReductionModeCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerReductionModeCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSamplerReductionModeCreateInfoEXT where
        type StructFields VkSamplerReductionModeCreateInfoEXT =
             '["sType", "pNext", "reductionMode"] -- ' closing tick for hsc2hs
        type CUnionType VkSamplerReductionModeCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerReductionModeCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSamplerReductionModeCreateInfoEXT =
             '[VkSamplerCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerReductionModeCreateInfoEXT where
        type FieldType "sType" VkSamplerReductionModeCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkSamplerReductionModeCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSamplerReductionModeCreateInfoEXT =
             #{offset VkSamplerReductionModeCreateInfoEXT, sType}
        type FieldIsArray "sType" VkSamplerReductionModeCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerReductionModeCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSamplerReductionModeCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerReductionModeCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSamplerReductionModeCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerReductionModeCreateInfoEXT where
        type FieldType "pNext" VkSamplerReductionModeCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkSamplerReductionModeCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSamplerReductionModeCreateInfoEXT =
             #{offset VkSamplerReductionModeCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkSamplerReductionModeCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSamplerReductionModeCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerReductionModeCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSamplerReductionModeCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "reductionMode" VkSamplerReductionModeCreateInfoEXT where
        type FieldType "reductionMode" VkSamplerReductionModeCreateInfoEXT
             = VkSamplerReductionModeEXT
        type FieldOptional "reductionMode"
               VkSamplerReductionModeCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "reductionMode"
               VkSamplerReductionModeCreateInfoEXT
             =
             #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}
        type FieldIsArray "reductionMode"
               VkSamplerReductionModeCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

instance {-# OVERLAPPING #-}
         CanReadField "reductionMode" VkSamplerReductionModeCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

instance {-# OVERLAPPING #-}
         CanWriteField "reductionMode" VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

instance Show VkSamplerReductionModeCreateInfoEXT where
        showsPrec d x
          = showString "VkSamplerReductionModeCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "reductionMode = " .
                            showsPrec d (getField @"reductionMode" x) . showChar '}'

-- | > typedef struct VkSamplerYcbcrConversionCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkFormat                         format;
--   >     VkSamplerYcbcrModelConversion ycbcrModel;
--   >     VkSamplerYcbcrRange           ycbcrRange;
--   >     VkComponentMapping               components;
--   >     VkChromaLocation              xChromaOffset;
--   >     VkChromaLocation              yChromaOffset;
--   >     VkFilter                         chromaFilter;
--   >     VkBool32                         forceExplicitReconstruction;
--   > } VkSamplerYcbcrConversionCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerYcbcrConversionCreateInfo VkSamplerYcbcrConversionCreateInfo registry at www.khronos.org>
data VkSamplerYcbcrConversionCreateInfo = VkSamplerYcbcrConversionCreateInfo## Addr##
                                                                              ByteArray##

instance Eq VkSamplerYcbcrConversionCreateInfo where
        (VkSamplerYcbcrConversionCreateInfo## a _) ==
          x@(VkSamplerYcbcrConversionCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionCreateInfo where
        (VkSamplerYcbcrConversionCreateInfo## a _) `compare`
          x@(VkSamplerYcbcrConversionCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionCreateInfo where
        sizeOf ~_ = #{size VkSamplerYcbcrConversionCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSamplerYcbcrConversionCreateInfo where
        unsafeAddr (VkSamplerYcbcrConversionCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSamplerYcbcrConversionCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerYcbcrConversionCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSamplerYcbcrConversionCreateInfo where
        type StructFields VkSamplerYcbcrConversionCreateInfo =
             '["sType", "pNext", "format", "ycbcrModel", "ycbcrRange", -- ' closing tick for hsc2hs
               "components", "xChromaOffset", "yChromaOffset", "chromaFilter",
               "forceExplicitReconstruction"]
        type CUnionType VkSamplerYcbcrConversionCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerYcbcrConversionCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSamplerYcbcrConversionCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "sType" VkSamplerYcbcrConversionCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, sType}
        type FieldIsArray "sType" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "pNext" VkSamplerYcbcrConversionCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, pNext}
        type FieldIsArray "pNext" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "format" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "format" VkSamplerYcbcrConversionCreateInfo =
             VkFormat
        type FieldOptional "format" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, format}
        type FieldIsArray "format" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, format}

instance {-# OVERLAPPING #-}
         HasField "ycbcrModel" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "ycbcrModel" VkSamplerYcbcrConversionCreateInfo =
             VkSamplerYcbcrModelConversion
        type FieldOptional "ycbcrModel" VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ycbcrModel" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel}
        type FieldIsArray "ycbcrModel" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel}

instance {-# OVERLAPPING #-}
         CanReadField "ycbcrModel" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel}

instance {-# OVERLAPPING #-}
         CanWriteField "ycbcrModel" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel}

instance {-# OVERLAPPING #-}
         HasField "ycbcrRange" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "ycbcrRange" VkSamplerYcbcrConversionCreateInfo =
             VkSamplerYcbcrRange
        type FieldOptional "ycbcrRange" VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ycbcrRange" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange}
        type FieldIsArray "ycbcrRange" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange}

instance {-# OVERLAPPING #-}
         CanReadField "ycbcrRange" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange}

instance {-# OVERLAPPING #-}
         CanWriteField "ycbcrRange" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange}

instance {-# OVERLAPPING #-}
         HasField "components" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "components" VkSamplerYcbcrConversionCreateInfo =
             VkComponentMapping
        type FieldOptional "components" VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "components" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, components}
        type FieldIsArray "components" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, components}

instance {-# OVERLAPPING #-}
         CanReadField "components" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, components})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, components}

instance {-# OVERLAPPING #-}
         CanWriteField "components" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, components}

instance {-# OVERLAPPING #-}
         HasField "xChromaOffset" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "xChromaOffset" VkSamplerYcbcrConversionCreateInfo =
             VkChromaLocation
        type FieldOptional "xChromaOffset"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "xChromaOffset" VkSamplerYcbcrConversionCreateInfo
             =
             #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset}
        type FieldIsArray "xChromaOffset"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset}

instance {-# OVERLAPPING #-}
         CanReadField "xChromaOffset" VkSamplerYcbcrConversionCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "xChromaOffset" VkSamplerYcbcrConversionCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset}

instance {-# OVERLAPPING #-}
         HasField "yChromaOffset" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "yChromaOffset" VkSamplerYcbcrConversionCreateInfo =
             VkChromaLocation
        type FieldOptional "yChromaOffset"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "yChromaOffset" VkSamplerYcbcrConversionCreateInfo
             =
             #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset}
        type FieldIsArray "yChromaOffset"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset}

instance {-# OVERLAPPING #-}
         CanReadField "yChromaOffset" VkSamplerYcbcrConversionCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "yChromaOffset" VkSamplerYcbcrConversionCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset}

instance {-# OVERLAPPING #-}
         HasField "chromaFilter" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "chromaFilter" VkSamplerYcbcrConversionCreateInfo =
             VkFilter
        type FieldOptional "chromaFilter"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "chromaFilter" VkSamplerYcbcrConversionCreateInfo
             =
             #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter}
        type FieldIsArray "chromaFilter" VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter}

instance {-# OVERLAPPING #-}
         CanReadField "chromaFilter" VkSamplerYcbcrConversionCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter}

instance {-# OVERLAPPING #-}
         CanWriteField "chromaFilter" VkSamplerYcbcrConversionCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter}

instance {-# OVERLAPPING #-}
         HasField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfo
         where
        type FieldType "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfo
             = VkBool32
        type FieldOptional "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfo
             =
             #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction}
        type FieldIsArray "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction}

instance {-# OVERLAPPING #-}
         CanReadField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction}

instance {-# OVERLAPPING #-}
         CanWriteField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction}

instance Show VkSamplerYcbcrConversionCreateInfo where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "format = " .
                            showsPrec d (getField @"format" x) .
                              showString ", " .
                                showString "ycbcrModel = " .
                                  showsPrec d (getField @"ycbcrModel" x) .
                                    showString ", " .
                                      showString "ycbcrRange = " .
                                        showsPrec d (getField @"ycbcrRange" x) .
                                          showString ", " .
                                            showString "components = " .
                                              showsPrec d (getField @"components" x) .
                                                showString ", " .
                                                  showString "xChromaOffset = " .
                                                    showsPrec d (getField @"xChromaOffset" x) .
                                                      showString ", " .
                                                        showString "yChromaOffset = " .
                                                          showsPrec d (getField @"yChromaOffset" x)
                                                            .
                                                            showString ", " .
                                                              showString "chromaFilter = " .
                                                                showsPrec d
                                                                  (getField @"chromaFilter" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "forceExplicitReconstruction = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"forceExplicitReconstruction"
                                                                           x)
                                                                        . showChar '}'

-- | Alias for `VkSamplerYcbcrConversionCreateInfo`
type VkSamplerYcbcrConversionCreateInfoKHR =
     VkSamplerYcbcrConversionCreateInfo

-- | > typedef struct VkSamplerYcbcrConversionImageFormatProperties {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     uint32_t                         combinedImageSamplerDescriptorCount;
--   > } VkSamplerYcbcrConversionImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerYcbcrConversionImageFormatProperties VkSamplerYcbcrConversionImageFormatProperties registry at www.khronos.org>
data VkSamplerYcbcrConversionImageFormatProperties = VkSamplerYcbcrConversionImageFormatProperties## Addr##
                                                                                                    ByteArray##

instance Eq VkSamplerYcbcrConversionImageFormatProperties where
        (VkSamplerYcbcrConversionImageFormatProperties## a _) ==
          x@(VkSamplerYcbcrConversionImageFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionImageFormatProperties where
        (VkSamplerYcbcrConversionImageFormatProperties## a _) `compare`
          x@(VkSamplerYcbcrConversionImageFormatProperties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionImageFormatProperties
         where
        sizeOf ~_
          = #{size VkSamplerYcbcrConversionImageFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionImageFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkSamplerYcbcrConversionImageFormatProperties
         where
        unsafeAddr (VkSamplerYcbcrConversionImageFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkSamplerYcbcrConversionImageFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerYcbcrConversionImageFormatProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkSamplerYcbcrConversionImageFormatProperties
         where
        type StructFields VkSamplerYcbcrConversionImageFormatProperties =
             '["sType", "pNext", "combinedImageSamplerDescriptorCount"] -- ' closing tick for hsc2hs
        type CUnionType VkSamplerYcbcrConversionImageFormatProperties =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerYcbcrConversionImageFormatProperties =
             'True -- ' closing tick for hsc2hs
        type StructExtends VkSamplerYcbcrConversionImageFormatProperties =
             '[VkImageFormatProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerYcbcrConversionImageFormatProperties
         where
        type FieldType "sType"
               VkSamplerYcbcrConversionImageFormatProperties
             = VkStructureType
        type FieldOptional "sType"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkSamplerYcbcrConversionImageFormatProperties
             =
             #{offset VkSamplerYcbcrConversionImageFormatProperties, sType}
        type FieldIsArray "sType"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerYcbcrConversionImageFormatProperties
         where
        type FieldType "pNext"
               VkSamplerYcbcrConversionImageFormatProperties
             = Ptr Void
        type FieldOptional "pNext"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkSamplerYcbcrConversionImageFormatProperties
             =
             #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext}
        type FieldIsArray "pNext"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatProperties
         where
        type FieldType "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatProperties
             = Word32
        type FieldOptional "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatProperties
             =
             #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount}
        type FieldIsArray "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount}

instance Show VkSamplerYcbcrConversionImageFormatProperties where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionImageFormatProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "combinedImageSamplerDescriptorCount = " .
                            showsPrec d (getField @"combinedImageSamplerDescriptorCount" x) .
                              showChar '}'

-- | Alias for `VkSamplerYcbcrConversionImageFormatProperties`
type VkSamplerYcbcrConversionImageFormatPropertiesKHR =
     VkSamplerYcbcrConversionImageFormatProperties

-- | > typedef struct VkSamplerYcbcrConversionInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSamplerYcbcrConversion      conversion;
--   > } VkSamplerYcbcrConversionInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerYcbcrConversionInfo VkSamplerYcbcrConversionInfo registry at www.khronos.org>
data VkSamplerYcbcrConversionInfo = VkSamplerYcbcrConversionInfo## Addr##
                                                                  ByteArray##

instance Eq VkSamplerYcbcrConversionInfo where
        (VkSamplerYcbcrConversionInfo## a _) ==
          x@(VkSamplerYcbcrConversionInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionInfo where
        (VkSamplerYcbcrConversionInfo## a _) `compare`
          x@(VkSamplerYcbcrConversionInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionInfo where
        sizeOf ~_ = #{size VkSamplerYcbcrConversionInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSamplerYcbcrConversionInfo where
        unsafeAddr (VkSamplerYcbcrConversionInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSamplerYcbcrConversionInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerYcbcrConversionInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSamplerYcbcrConversionInfo where
        type StructFields VkSamplerYcbcrConversionInfo =
             '["sType", "pNext", "conversion"] -- ' closing tick for hsc2hs
        type CUnionType VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSamplerYcbcrConversionInfo =
             '[VkSamplerCreateInfo, VkImageViewCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerYcbcrConversionInfo where
        type FieldType "sType" VkSamplerYcbcrConversionInfo =
             VkStructureType
        type FieldOptional "sType" VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSamplerYcbcrConversionInfo =
             #{offset VkSamplerYcbcrConversionInfo, sType}
        type FieldIsArray "sType" VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSamplerYcbcrConversionInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSamplerYcbcrConversionInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerYcbcrConversionInfo where
        type FieldType "pNext" VkSamplerYcbcrConversionInfo = Ptr Void
        type FieldOptional "pNext" VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSamplerYcbcrConversionInfo =
             #{offset VkSamplerYcbcrConversionInfo, pNext}
        type FieldIsArray "pNext" VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSamplerYcbcrConversionInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSamplerYcbcrConversionInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "conversion" VkSamplerYcbcrConversionInfo where
        type FieldType "conversion" VkSamplerYcbcrConversionInfo =
             VkSamplerYcbcrConversion
        type FieldOptional "conversion" VkSamplerYcbcrConversionInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "conversion" VkSamplerYcbcrConversionInfo =
             #{offset VkSamplerYcbcrConversionInfo, conversion}
        type FieldIsArray "conversion" VkSamplerYcbcrConversionInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionInfo, conversion}

instance {-# OVERLAPPING #-}
         CanReadField "conversion" VkSamplerYcbcrConversionInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfo, conversion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfo, conversion}

instance {-# OVERLAPPING #-}
         CanWriteField "conversion" VkSamplerYcbcrConversionInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfo, conversion}

instance Show VkSamplerYcbcrConversionInfo where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "conversion = " .
                            showsPrec d (getField @"conversion" x) . showChar '}'

-- | Alias for `VkSamplerYcbcrConversionInfo`
type VkSamplerYcbcrConversionInfoKHR = VkSamplerYcbcrConversionInfo
