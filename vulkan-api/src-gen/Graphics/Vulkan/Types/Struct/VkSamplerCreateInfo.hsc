#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo
       (VkSamplerCreateInfo(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Base                                        (Addr##,
                                                                  ByteArray##,
                                                                  byteArrayContents##,
                                                                  plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                 (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks                  (VkSamplerCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkBorderColor        (VkBorderColor)
import           Graphics.Vulkan.Types.Enum.VkCompareOp          (VkCompareOp)
import           Graphics.Vulkan.Types.Enum.VkFilter             (VkFilter)
import           Graphics.Vulkan.Types.Enum.VkSamplerAddressMode (VkSamplerAddressMode)
import           Graphics.Vulkan.Types.Enum.VkSamplerMipmapMode  (VkSamplerMipmapMode)
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkSamplerCreateInfo VkSamplerCreateInfo registry at www.khronos.org>
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
