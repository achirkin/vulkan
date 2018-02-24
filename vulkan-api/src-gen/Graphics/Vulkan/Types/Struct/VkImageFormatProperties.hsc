#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageFormatProperties
       (VkImageFormatProperties(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags (VkSampleCountFlags)
import           Graphics.Vulkan.Types.Struct.VkExtent3D       (VkExtent3D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkImageFormatProperties {
--   >     VkExtent3D             maxExtent;
--   >     uint32_t               maxMipLevels;
--   >     uint32_t               maxArrayLayers;
--   >     VkSampleCountFlags     sampleCounts;
--   >     VkDeviceSize           maxResourceSize;
--   > } VkImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageFormatProperties.html VkImageFormatProperties registry at www.khronos.org>
data VkImageFormatProperties = VkImageFormatProperties## Addr##
                                                        ByteArray##

instance Eq VkImageFormatProperties where
        (VkImageFormatProperties## a _) == x@(VkImageFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageFormatProperties where
        (VkImageFormatProperties## a _) `compare`
          x@(VkImageFormatProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageFormatProperties where
        sizeOf ~_ = #{size VkImageFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageFormatProperties where
        unsafeAddr (VkImageFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageFormatProperties## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageFormatProperties where
        type StructFields VkImageFormatProperties =
             '["maxExtent", "maxMipLevels", "maxArrayLayers", "sampleCounts", -- ' closing tick for hsc2hs
               "maxResourceSize"]
        type CUnionType VkImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkImageFormatProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkMaxExtent VkImageFormatProperties
         where
        type VkMaxExtentMType VkImageFormatProperties = VkExtent3D

        {-# NOINLINE vkMaxExtent #-}
        vkMaxExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxExtent})

        {-# INLINE vkMaxExtentByteOffset #-}
        vkMaxExtentByteOffset ~_
          = #{offset VkImageFormatProperties, maxExtent}

        {-# INLINE readVkMaxExtent #-}
        readVkMaxExtent p
          = peekByteOff p #{offset VkImageFormatProperties, maxExtent}

        {-# INLINE writeVkMaxExtent #-}
        writeVkMaxExtent p
          = pokeByteOff p #{offset VkImageFormatProperties, maxExtent}

instance {-# OVERLAPPING #-}
         HasField "maxExtent" VkImageFormatProperties where
        type FieldType "maxExtent" VkImageFormatProperties = VkExtent3D
        type FieldOptional "maxExtent" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxExtent" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxExtent}
        type FieldIsArray "maxExtent" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxExtent}

instance CanReadField "maxExtent" VkImageFormatProperties where
        {-# INLINE getField #-}
        getField = vkMaxExtent

        {-# INLINE readField #-}
        readField = readVkMaxExtent

instance CanWriteField "maxExtent" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField = writeVkMaxExtent

instance {-# OVERLAPPING #-}
         HasVkMaxMipLevels VkImageFormatProperties where
        type VkMaxMipLevelsMType VkImageFormatProperties = Word32

        {-# NOINLINE vkMaxMipLevels #-}
        vkMaxMipLevels x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxMipLevels})

        {-# INLINE vkMaxMipLevelsByteOffset #-}
        vkMaxMipLevelsByteOffset ~_
          = #{offset VkImageFormatProperties, maxMipLevels}

        {-# INLINE readVkMaxMipLevels #-}
        readVkMaxMipLevels p
          = peekByteOff p #{offset VkImageFormatProperties, maxMipLevels}

        {-# INLINE writeVkMaxMipLevels #-}
        writeVkMaxMipLevels p
          = pokeByteOff p #{offset VkImageFormatProperties, maxMipLevels}

instance {-# OVERLAPPING #-}
         HasField "maxMipLevels" VkImageFormatProperties where
        type FieldType "maxMipLevels" VkImageFormatProperties = Word32
        type FieldOptional "maxMipLevels" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMipLevels" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxMipLevels}
        type FieldIsArray "maxMipLevels" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxMipLevels}

instance CanReadField "maxMipLevels" VkImageFormatProperties where
        {-# INLINE getField #-}
        getField = vkMaxMipLevels

        {-# INLINE readField #-}
        readField = readVkMaxMipLevels

instance CanWriteField "maxMipLevels" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField = writeVkMaxMipLevels

instance {-# OVERLAPPING #-}
         HasVkMaxArrayLayers VkImageFormatProperties where
        type VkMaxArrayLayersMType VkImageFormatProperties = Word32

        {-# NOINLINE vkMaxArrayLayers #-}
        vkMaxArrayLayers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxArrayLayers})

        {-# INLINE vkMaxArrayLayersByteOffset #-}
        vkMaxArrayLayersByteOffset ~_
          = #{offset VkImageFormatProperties, maxArrayLayers}

        {-# INLINE readVkMaxArrayLayers #-}
        readVkMaxArrayLayers p
          = peekByteOff p #{offset VkImageFormatProperties, maxArrayLayers}

        {-# INLINE writeVkMaxArrayLayers #-}
        writeVkMaxArrayLayers p
          = pokeByteOff p #{offset VkImageFormatProperties, maxArrayLayers}

instance {-# OVERLAPPING #-}
         HasField "maxArrayLayers" VkImageFormatProperties where
        type FieldType "maxArrayLayers" VkImageFormatProperties = Word32
        type FieldOptional "maxArrayLayers" VkImageFormatProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxArrayLayers" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxArrayLayers}
        type FieldIsArray "maxArrayLayers" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxArrayLayers}

instance CanReadField "maxArrayLayers" VkImageFormatProperties
         where
        {-# INLINE getField #-}
        getField = vkMaxArrayLayers

        {-# INLINE readField #-}
        readField = readVkMaxArrayLayers

instance CanWriteField "maxArrayLayers" VkImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxArrayLayers

instance {-# OVERLAPPING #-}
         HasVkSampleCounts VkImageFormatProperties where
        type VkSampleCountsMType VkImageFormatProperties =
             VkSampleCountFlags

        {-# NOINLINE vkSampleCounts #-}
        vkSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, sampleCounts})

        {-# INLINE vkSampleCountsByteOffset #-}
        vkSampleCountsByteOffset ~_
          = #{offset VkImageFormatProperties, sampleCounts}

        {-# INLINE readVkSampleCounts #-}
        readVkSampleCounts p
          = peekByteOff p #{offset VkImageFormatProperties, sampleCounts}

        {-# INLINE writeVkSampleCounts #-}
        writeVkSampleCounts p
          = pokeByteOff p #{offset VkImageFormatProperties, sampleCounts}

instance {-# OVERLAPPING #-}
         HasField "sampleCounts" VkImageFormatProperties where
        type FieldType "sampleCounts" VkImageFormatProperties =
             VkSampleCountFlags
        type FieldOptional "sampleCounts" VkImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sampleCounts" VkImageFormatProperties =
             #{offset VkImageFormatProperties, sampleCounts}
        type FieldIsArray "sampleCounts" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, sampleCounts}

instance CanReadField "sampleCounts" VkImageFormatProperties where
        {-# INLINE getField #-}
        getField = vkSampleCounts

        {-# INLINE readField #-}
        readField = readVkSampleCounts

instance CanWriteField "sampleCounts" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField = writeVkSampleCounts

instance {-# OVERLAPPING #-}
         HasVkMaxResourceSize VkImageFormatProperties where
        type VkMaxResourceSizeMType VkImageFormatProperties = VkDeviceSize

        {-# NOINLINE vkMaxResourceSize #-}
        vkMaxResourceSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxResourceSize})

        {-# INLINE vkMaxResourceSizeByteOffset #-}
        vkMaxResourceSizeByteOffset ~_
          = #{offset VkImageFormatProperties, maxResourceSize}

        {-# INLINE readVkMaxResourceSize #-}
        readVkMaxResourceSize p
          = peekByteOff p #{offset VkImageFormatProperties, maxResourceSize}

        {-# INLINE writeVkMaxResourceSize #-}
        writeVkMaxResourceSize p
          = pokeByteOff p #{offset VkImageFormatProperties, maxResourceSize}

instance {-# OVERLAPPING #-}
         HasField "maxResourceSize" VkImageFormatProperties where
        type FieldType "maxResourceSize" VkImageFormatProperties =
             VkDeviceSize
        type FieldOptional "maxResourceSize" VkImageFormatProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxResourceSize" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxResourceSize}
        type FieldIsArray "maxResourceSize" VkImageFormatProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxResourceSize}

instance CanReadField "maxResourceSize" VkImageFormatProperties
         where
        {-# INLINE getField #-}
        getField = vkMaxResourceSize

        {-# INLINE readField #-}
        readField = readVkMaxResourceSize

instance CanWriteField "maxResourceSize" VkImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxResourceSize

instance Show VkImageFormatProperties where
        showsPrec d x
          = showString "VkImageFormatProperties {" .
              showString "vkMaxExtent = " .
                showsPrec d (vkMaxExtent x) .
                  showString ", " .
                    showString "vkMaxMipLevels = " .
                      showsPrec d (vkMaxMipLevels x) .
                        showString ", " .
                          showString "vkMaxArrayLayers = " .
                            showsPrec d (vkMaxArrayLayers x) .
                              showString ", " .
                                showString "vkSampleCounts = " .
                                  showsPrec d (vkSampleCounts x) .
                                    showString ", " .
                                      showString "vkMaxResourceSize = " .
                                        showsPrec d (vkMaxResourceSize x) . showChar '}'
