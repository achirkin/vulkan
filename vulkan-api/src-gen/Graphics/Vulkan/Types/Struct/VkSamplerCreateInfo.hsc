#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo
       (VkSamplerCreateInfo(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
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
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSamplerCreateInfo.html VkSamplerCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkSamplerCreateInfo where
        type VkSTypeMType VkSamplerCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSamplerCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSamplerCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSamplerCreateInfo, sType}

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

instance CanReadField "sType" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSamplerCreateInfo where
        type VkPNextMType VkSamplerCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSamplerCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSamplerCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSamplerCreateInfo, pNext}

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

instance CanReadField "pNext" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkSamplerCreateInfo where
        type VkFlagsMType VkSamplerCreateInfo = VkSamplerCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkSamplerCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkSamplerCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkSamplerCreateInfo, flags}

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

instance CanReadField "flags" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkMagFilter VkSamplerCreateInfo
         where
        type VkMagFilterMType VkSamplerCreateInfo = VkFilter

        {-# NOINLINE vkMagFilter #-}
        vkMagFilter x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, magFilter})

        {-# INLINE vkMagFilterByteOffset #-}
        vkMagFilterByteOffset ~_
          = #{offset VkSamplerCreateInfo, magFilter}

        {-# INLINE readVkMagFilter #-}
        readVkMagFilter p
          = peekByteOff p #{offset VkSamplerCreateInfo, magFilter}

        {-# INLINE writeVkMagFilter #-}
        writeVkMagFilter p
          = pokeByteOff p #{offset VkSamplerCreateInfo, magFilter}

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

instance CanReadField "magFilter" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkMagFilter

        {-# INLINE readField #-}
        readField = readVkMagFilter

instance CanWriteField "magFilter" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMagFilter

instance {-# OVERLAPPING #-} HasVkMinFilter VkSamplerCreateInfo
         where
        type VkMinFilterMType VkSamplerCreateInfo = VkFilter

        {-# NOINLINE vkMinFilter #-}
        vkMinFilter x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, minFilter})

        {-# INLINE vkMinFilterByteOffset #-}
        vkMinFilterByteOffset ~_
          = #{offset VkSamplerCreateInfo, minFilter}

        {-# INLINE readVkMinFilter #-}
        readVkMinFilter p
          = peekByteOff p #{offset VkSamplerCreateInfo, minFilter}

        {-# INLINE writeVkMinFilter #-}
        writeVkMinFilter p
          = pokeByteOff p #{offset VkSamplerCreateInfo, minFilter}

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

instance CanReadField "minFilter" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkMinFilter

        {-# INLINE readField #-}
        readField = readVkMinFilter

instance CanWriteField "minFilter" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMinFilter

instance {-# OVERLAPPING #-} HasVkMipmapMode VkSamplerCreateInfo
         where
        type VkMipmapModeMType VkSamplerCreateInfo = VkSamplerMipmapMode

        {-# NOINLINE vkMipmapMode #-}
        vkMipmapMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, mipmapMode})

        {-# INLINE vkMipmapModeByteOffset #-}
        vkMipmapModeByteOffset ~_
          = #{offset VkSamplerCreateInfo, mipmapMode}

        {-# INLINE readVkMipmapMode #-}
        readVkMipmapMode p
          = peekByteOff p #{offset VkSamplerCreateInfo, mipmapMode}

        {-# INLINE writeVkMipmapMode #-}
        writeVkMipmapMode p
          = pokeByteOff p #{offset VkSamplerCreateInfo, mipmapMode}

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

instance CanReadField "mipmapMode" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkMipmapMode

        {-# INLINE readField #-}
        readField = readVkMipmapMode

instance CanWriteField "mipmapMode" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMipmapMode

instance {-# OVERLAPPING #-} HasVkAddressModeU VkSamplerCreateInfo
         where
        type VkAddressModeUMType VkSamplerCreateInfo = VkSamplerAddressMode

        {-# NOINLINE vkAddressModeU #-}
        vkAddressModeU x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, addressModeU})

        {-# INLINE vkAddressModeUByteOffset #-}
        vkAddressModeUByteOffset ~_
          = #{offset VkSamplerCreateInfo, addressModeU}

        {-# INLINE readVkAddressModeU #-}
        readVkAddressModeU p
          = peekByteOff p #{offset VkSamplerCreateInfo, addressModeU}

        {-# INLINE writeVkAddressModeU #-}
        writeVkAddressModeU p
          = pokeByteOff p #{offset VkSamplerCreateInfo, addressModeU}

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

instance CanReadField "addressModeU" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkAddressModeU

        {-# INLINE readField #-}
        readField = readVkAddressModeU

instance CanWriteField "addressModeU" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkAddressModeU

instance {-# OVERLAPPING #-} HasVkAddressModeV VkSamplerCreateInfo
         where
        type VkAddressModeVMType VkSamplerCreateInfo = VkSamplerAddressMode

        {-# NOINLINE vkAddressModeV #-}
        vkAddressModeV x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, addressModeV})

        {-# INLINE vkAddressModeVByteOffset #-}
        vkAddressModeVByteOffset ~_
          = #{offset VkSamplerCreateInfo, addressModeV}

        {-# INLINE readVkAddressModeV #-}
        readVkAddressModeV p
          = peekByteOff p #{offset VkSamplerCreateInfo, addressModeV}

        {-# INLINE writeVkAddressModeV #-}
        writeVkAddressModeV p
          = pokeByteOff p #{offset VkSamplerCreateInfo, addressModeV}

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

instance CanReadField "addressModeV" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkAddressModeV

        {-# INLINE readField #-}
        readField = readVkAddressModeV

instance CanWriteField "addressModeV" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkAddressModeV

instance {-# OVERLAPPING #-} HasVkAddressModeW VkSamplerCreateInfo
         where
        type VkAddressModeWMType VkSamplerCreateInfo = VkSamplerAddressMode

        {-# NOINLINE vkAddressModeW #-}
        vkAddressModeW x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, addressModeW})

        {-# INLINE vkAddressModeWByteOffset #-}
        vkAddressModeWByteOffset ~_
          = #{offset VkSamplerCreateInfo, addressModeW}

        {-# INLINE readVkAddressModeW #-}
        readVkAddressModeW p
          = peekByteOff p #{offset VkSamplerCreateInfo, addressModeW}

        {-# INLINE writeVkAddressModeW #-}
        writeVkAddressModeW p
          = pokeByteOff p #{offset VkSamplerCreateInfo, addressModeW}

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

instance CanReadField "addressModeW" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkAddressModeW

        {-# INLINE readField #-}
        readField = readVkAddressModeW

instance CanWriteField "addressModeW" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkAddressModeW

instance {-# OVERLAPPING #-} HasVkMipLodBias VkSamplerCreateInfo
         where
        type VkMipLodBiasMType VkSamplerCreateInfo =
             #{type float}

        {-# NOINLINE vkMipLodBias #-}
        vkMipLodBias x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, mipLodBias})

        {-# INLINE vkMipLodBiasByteOffset #-}
        vkMipLodBiasByteOffset ~_
          = #{offset VkSamplerCreateInfo, mipLodBias}

        {-# INLINE readVkMipLodBias #-}
        readVkMipLodBias p
          = peekByteOff p #{offset VkSamplerCreateInfo, mipLodBias}

        {-# INLINE writeVkMipLodBias #-}
        writeVkMipLodBias p
          = pokeByteOff p #{offset VkSamplerCreateInfo, mipLodBias}

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

instance CanReadField "mipLodBias" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkMipLodBias

        {-# INLINE readField #-}
        readField = readVkMipLodBias

instance CanWriteField "mipLodBias" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMipLodBias

instance {-# OVERLAPPING #-}
         HasVkAnisotropyEnable VkSamplerCreateInfo where
        type VkAnisotropyEnableMType VkSamplerCreateInfo = VkBool32

        {-# NOINLINE vkAnisotropyEnable #-}
        vkAnisotropyEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, anisotropyEnable})

        {-# INLINE vkAnisotropyEnableByteOffset #-}
        vkAnisotropyEnableByteOffset ~_
          = #{offset VkSamplerCreateInfo, anisotropyEnable}

        {-# INLINE readVkAnisotropyEnable #-}
        readVkAnisotropyEnable p
          = peekByteOff p #{offset VkSamplerCreateInfo, anisotropyEnable}

        {-# INLINE writeVkAnisotropyEnable #-}
        writeVkAnisotropyEnable p
          = pokeByteOff p #{offset VkSamplerCreateInfo, anisotropyEnable}

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

instance CanReadField "anisotropyEnable" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkAnisotropyEnable

        {-# INLINE readField #-}
        readField = readVkAnisotropyEnable

instance CanWriteField "anisotropyEnable" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkAnisotropyEnable

instance {-# OVERLAPPING #-} HasVkMaxAnisotropy VkSamplerCreateInfo
         where
        type VkMaxAnisotropyMType VkSamplerCreateInfo =
             #{type float}

        {-# NOINLINE vkMaxAnisotropy #-}
        vkMaxAnisotropy x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, maxAnisotropy})

        {-# INLINE vkMaxAnisotropyByteOffset #-}
        vkMaxAnisotropyByteOffset ~_
          = #{offset VkSamplerCreateInfo, maxAnisotropy}

        {-# INLINE readVkMaxAnisotropy #-}
        readVkMaxAnisotropy p
          = peekByteOff p #{offset VkSamplerCreateInfo, maxAnisotropy}

        {-# INLINE writeVkMaxAnisotropy #-}
        writeVkMaxAnisotropy p
          = pokeByteOff p #{offset VkSamplerCreateInfo, maxAnisotropy}

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

instance CanReadField "maxAnisotropy" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkMaxAnisotropy

        {-# INLINE readField #-}
        readField = readVkMaxAnisotropy

instance CanWriteField "maxAnisotropy" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMaxAnisotropy

instance {-# OVERLAPPING #-} HasVkCompareEnable VkSamplerCreateInfo
         where
        type VkCompareEnableMType VkSamplerCreateInfo = VkBool32

        {-# NOINLINE vkCompareEnable #-}
        vkCompareEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, compareEnable})

        {-# INLINE vkCompareEnableByteOffset #-}
        vkCompareEnableByteOffset ~_
          = #{offset VkSamplerCreateInfo, compareEnable}

        {-# INLINE readVkCompareEnable #-}
        readVkCompareEnable p
          = peekByteOff p #{offset VkSamplerCreateInfo, compareEnable}

        {-# INLINE writeVkCompareEnable #-}
        writeVkCompareEnable p
          = pokeByteOff p #{offset VkSamplerCreateInfo, compareEnable}

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

instance CanReadField "compareEnable" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkCompareEnable

        {-# INLINE readField #-}
        readField = readVkCompareEnable

instance CanWriteField "compareEnable" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkCompareEnable

instance {-# OVERLAPPING #-} HasVkCompareOp VkSamplerCreateInfo
         where
        type VkCompareOpMType VkSamplerCreateInfo = VkCompareOp

        {-# NOINLINE vkCompareOp #-}
        vkCompareOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, compareOp})

        {-# INLINE vkCompareOpByteOffset #-}
        vkCompareOpByteOffset ~_
          = #{offset VkSamplerCreateInfo, compareOp}

        {-# INLINE readVkCompareOp #-}
        readVkCompareOp p
          = peekByteOff p #{offset VkSamplerCreateInfo, compareOp}

        {-# INLINE writeVkCompareOp #-}
        writeVkCompareOp p
          = pokeByteOff p #{offset VkSamplerCreateInfo, compareOp}

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

instance CanReadField "compareOp" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkCompareOp

        {-# INLINE readField #-}
        readField = readVkCompareOp

instance CanWriteField "compareOp" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkCompareOp

instance {-# OVERLAPPING #-} HasVkMinLod VkSamplerCreateInfo where
        type VkMinLodMType VkSamplerCreateInfo = #{type float}

        {-# NOINLINE vkMinLod #-}
        vkMinLod x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, minLod})

        {-# INLINE vkMinLodByteOffset #-}
        vkMinLodByteOffset ~_
          = #{offset VkSamplerCreateInfo, minLod}

        {-# INLINE readVkMinLod #-}
        readVkMinLod p
          = peekByteOff p #{offset VkSamplerCreateInfo, minLod}

        {-# INLINE writeVkMinLod #-}
        writeVkMinLod p
          = pokeByteOff p #{offset VkSamplerCreateInfo, minLod}

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

instance CanReadField "minLod" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkMinLod

        {-# INLINE readField #-}
        readField = readVkMinLod

instance CanWriteField "minLod" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMinLod

instance {-# OVERLAPPING #-} HasVkMaxLod VkSamplerCreateInfo where
        type VkMaxLodMType VkSamplerCreateInfo = #{type float}

        {-# NOINLINE vkMaxLod #-}
        vkMaxLod x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, maxLod})

        {-# INLINE vkMaxLodByteOffset #-}
        vkMaxLodByteOffset ~_
          = #{offset VkSamplerCreateInfo, maxLod}

        {-# INLINE readVkMaxLod #-}
        readVkMaxLod p
          = peekByteOff p #{offset VkSamplerCreateInfo, maxLod}

        {-# INLINE writeVkMaxLod #-}
        writeVkMaxLod p
          = pokeByteOff p #{offset VkSamplerCreateInfo, maxLod}

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

instance CanReadField "maxLod" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkMaxLod

        {-# INLINE readField #-}
        readField = readVkMaxLod

instance CanWriteField "maxLod" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMaxLod

instance {-# OVERLAPPING #-} HasVkBorderColor VkSamplerCreateInfo
         where
        type VkBorderColorMType VkSamplerCreateInfo = VkBorderColor

        {-# NOINLINE vkBorderColor #-}
        vkBorderColor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, borderColor})

        {-# INLINE vkBorderColorByteOffset #-}
        vkBorderColorByteOffset ~_
          = #{offset VkSamplerCreateInfo, borderColor}

        {-# INLINE readVkBorderColor #-}
        readVkBorderColor p
          = peekByteOff p #{offset VkSamplerCreateInfo, borderColor}

        {-# INLINE writeVkBorderColor #-}
        writeVkBorderColor p
          = pokeByteOff p #{offset VkSamplerCreateInfo, borderColor}

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

instance CanReadField "borderColor" VkSamplerCreateInfo where
        {-# INLINE getField #-}
        getField = vkBorderColor

        {-# INLINE readField #-}
        readField = readVkBorderColor

instance CanWriteField "borderColor" VkSamplerCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkBorderColor

instance {-# OVERLAPPING #-}
         HasVkUnnormalizedCoordinates VkSamplerCreateInfo where
        type VkUnnormalizedCoordinatesMType VkSamplerCreateInfo = VkBool32

        {-# NOINLINE vkUnnormalizedCoordinates #-}
        vkUnnormalizedCoordinates x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerCreateInfo, unnormalizedCoordinates})

        {-# INLINE vkUnnormalizedCoordinatesByteOffset #-}
        vkUnnormalizedCoordinatesByteOffset ~_
          = #{offset VkSamplerCreateInfo, unnormalizedCoordinates}

        {-# INLINE readVkUnnormalizedCoordinates #-}
        readVkUnnormalizedCoordinates p
          = peekByteOff p #{offset VkSamplerCreateInfo, unnormalizedCoordinates}

        {-# INLINE writeVkUnnormalizedCoordinates #-}
        writeVkUnnormalizedCoordinates p
          = pokeByteOff p #{offset VkSamplerCreateInfo, unnormalizedCoordinates}

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

instance CanReadField "unnormalizedCoordinates" VkSamplerCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkUnnormalizedCoordinates

        {-# INLINE readField #-}
        readField = readVkUnnormalizedCoordinates

instance CanWriteField "unnormalizedCoordinates"
           VkSamplerCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkUnnormalizedCoordinates

instance Show VkSamplerCreateInfo where
        showsPrec d x
          = showString "VkSamplerCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkMagFilter = " .
                                  showsPrec d (vkMagFilter x) .
                                    showString ", " .
                                      showString "vkMinFilter = " .
                                        showsPrec d (vkMinFilter x) .
                                          showString ", " .
                                            showString "vkMipmapMode = " .
                                              showsPrec d (vkMipmapMode x) .
                                                showString ", " .
                                                  showString "vkAddressModeU = " .
                                                    showsPrec d (vkAddressModeU x) .
                                                      showString ", " .
                                                        showString "vkAddressModeV = " .
                                                          showsPrec d (vkAddressModeV x) .
                                                            showString ", " .
                                                              showString "vkAddressModeW = " .
                                                                showsPrec d (vkAddressModeW x) .
                                                                  showString ", " .
                                                                    showString "vkMipLodBias = " .
                                                                      showsPrec d (vkMipLodBias x) .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkAnisotropyEnable = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkAnisotropyEnable x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkMaxAnisotropy = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkMaxAnisotropy
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "vkCompareEnable = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (vkCompareEnable
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "vkCompareOp = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (vkCompareOp
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "vkMinLod = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (vkMinLod
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "vkMaxLod = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (vkMaxLod
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "vkBorderColor = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (vkBorderColor
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "vkUnnormalizedCoordinates = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (vkUnnormalizedCoordinates
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showChar
                                                                                                                          '}'
