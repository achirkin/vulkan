#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties
       (VkSparseImageFormatProperties(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags       (VkImageAspectFlags)
import           Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags (VkSparseImageFormatFlags)
import           Graphics.Vulkan.Types.Struct.VkExtent3D             (VkExtent3D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageFormatProperties {
--   >     VkImageAspectFlags     aspectMask;
--   >     VkExtent3D             imageGranularity;
--   >     VkSparseImageFormatFlags flags;
--   > } VkSparseImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSparseImageFormatProperties.html VkSparseImageFormatProperties registry at www.khronos.org>
data VkSparseImageFormatProperties = VkSparseImageFormatProperties## Addr##
                                                                    ByteArray##

instance Eq VkSparseImageFormatProperties where
        (VkSparseImageFormatProperties## a _) ==
          x@(VkSparseImageFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageFormatProperties where
        (VkSparseImageFormatProperties## a _) `compare`
          x@(VkSparseImageFormatProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageFormatProperties where
        sizeOf ~_ = #{size VkSparseImageFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageFormatProperties where
        unsafeAddr (VkSparseImageFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageFormatProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageFormatProperties where
        type StructFields VkSparseImageFormatProperties =
             '["aspectMask", "imageGranularity", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageFormatProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkAspectMask VkSparseImageFormatProperties where
        type VkAspectMaskMType VkSparseImageFormatProperties =
             VkImageAspectFlags

        {-# NOINLINE vkAspectMask #-}
        vkAspectMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties, aspectMask})

        {-# INLINE vkAspectMaskByteOffset #-}
        vkAspectMaskByteOffset ~_
          = #{offset VkSparseImageFormatProperties, aspectMask}

        {-# INLINE readVkAspectMask #-}
        readVkAspectMask p
          = peekByteOff p #{offset VkSparseImageFormatProperties, aspectMask}

        {-# INLINE writeVkAspectMask #-}
        writeVkAspectMask p
          = pokeByteOff p #{offset VkSparseImageFormatProperties, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkSparseImageFormatProperties where
        type FieldType "aspectMask" VkSparseImageFormatProperties =
             VkImageAspectFlags
        type FieldOptional "aspectMask" VkSparseImageFormatProperties =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkSparseImageFormatProperties =
             #{offset VkSparseImageFormatProperties, aspectMask}
        type FieldIsArray "aspectMask" VkSparseImageFormatProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties, aspectMask}

instance CanReadField "aspectMask" VkSparseImageFormatProperties
         where
        {-# INLINE getField #-}
        getField = vkAspectMask

        {-# INLINE readField #-}
        readField = readVkAspectMask

instance {-# OVERLAPPING #-}
         HasVkImageGranularity VkSparseImageFormatProperties where
        type VkImageGranularityMType VkSparseImageFormatProperties =
             VkExtent3D

        {-# NOINLINE vkImageGranularity #-}
        vkImageGranularity x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties, imageGranularity})

        {-# INLINE vkImageGranularityByteOffset #-}
        vkImageGranularityByteOffset ~_
          = #{offset VkSparseImageFormatProperties, imageGranularity}

        {-# INLINE readVkImageGranularity #-}
        readVkImageGranularity p
          = peekByteOff p #{offset VkSparseImageFormatProperties, imageGranularity}

        {-# INLINE writeVkImageGranularity #-}
        writeVkImageGranularity p
          = pokeByteOff p #{offset VkSparseImageFormatProperties, imageGranularity}

instance {-# OVERLAPPING #-}
         HasField "imageGranularity" VkSparseImageFormatProperties where
        type FieldType "imageGranularity" VkSparseImageFormatProperties =
             VkExtent3D
        type FieldOptional "imageGranularity" VkSparseImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageGranularity" VkSparseImageFormatProperties =
             #{offset VkSparseImageFormatProperties, imageGranularity}
        type FieldIsArray "imageGranularity" VkSparseImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties, imageGranularity}

instance CanReadField "imageGranularity"
           VkSparseImageFormatProperties
         where
        {-# INLINE getField #-}
        getField = vkImageGranularity

        {-# INLINE readField #-}
        readField = readVkImageGranularity

instance {-# OVERLAPPING #-}
         HasVkFlags VkSparseImageFormatProperties where
        type VkFlagsMType VkSparseImageFormatProperties =
             VkSparseImageFormatFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkSparseImageFormatProperties, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkSparseImageFormatProperties, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkSparseImageFormatProperties, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkSparseImageFormatProperties where
        type FieldType "flags" VkSparseImageFormatProperties =
             VkSparseImageFormatFlags
        type FieldOptional "flags" VkSparseImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSparseImageFormatProperties =
             #{offset VkSparseImageFormatProperties, flags}
        type FieldIsArray "flags" VkSparseImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties, flags}

instance CanReadField "flags" VkSparseImageFormatProperties where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance Show VkSparseImageFormatProperties where
        showsPrec d x
          = showString "VkSparseImageFormatProperties {" .
              showString "vkAspectMask = " .
                showsPrec d (vkAspectMask x) .
                  showString ", " .
                    showString "vkImageGranularity = " .
                      showsPrec d (vkImageGranularity x) .
                        showString ", " .
                          showString "vkFlags = " . showsPrec d (vkFlags x) . showChar '}'
