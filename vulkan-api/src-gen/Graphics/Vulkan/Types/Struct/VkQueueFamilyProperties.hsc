#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties
       (VkQueueFamilyProperties(..)) where
import           Foreign.Storable                        (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkQueueFlags (VkQueueFlags)
import           Graphics.Vulkan.Types.Struct.VkExtent3D (VkExtent3D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                        (unsafeDupablePerformIO)

-- | > typedef struct VkQueueFamilyProperties {
--   >     VkQueueFlags           queueFlags;
--   >     uint32_t               queueCount;
--   >     uint32_t               timestampValidBits;
--   >     VkExtent3D             minImageTransferGranularity;
--   > } VkQueueFamilyProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkQueueFamilyProperties.html VkQueueFamilyProperties registry at www.khronos.org>
data VkQueueFamilyProperties = VkQueueFamilyProperties## Addr##
                                                        ByteArray##

instance Eq VkQueueFamilyProperties where
        (VkQueueFamilyProperties## a _) == x@(VkQueueFamilyProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkQueueFamilyProperties where
        (VkQueueFamilyProperties## a _) `compare`
          x@(VkQueueFamilyProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkQueueFamilyProperties where
        sizeOf ~_ = #{size VkQueueFamilyProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkQueueFamilyProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkQueueFamilyProperties where
        unsafeAddr (VkQueueFamilyProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkQueueFamilyProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkQueueFamilyProperties## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkQueueFamilyProperties where
        type StructFields VkQueueFamilyProperties =
             '["queueFlags", "queueCount", "timestampValidBits", -- ' closing tick for hsc2hs
               "minImageTransferGranularity"]
        type CUnionType VkQueueFamilyProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkQueueFamilyProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkQueueFamilyProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkQueueFlags VkQueueFamilyProperties where
        type VkQueueFlagsMType VkQueueFamilyProperties = VkQueueFlags

        {-# NOINLINE vkQueueFlags #-}
        vkQueueFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, queueFlags})

        {-# INLINE vkQueueFlagsByteOffset #-}
        vkQueueFlagsByteOffset ~_
          = #{offset VkQueueFamilyProperties, queueFlags}

        {-# INLINE readVkQueueFlags #-}
        readVkQueueFlags p
          = peekByteOff p #{offset VkQueueFamilyProperties, queueFlags}

        {-# INLINE writeVkQueueFlags #-}
        writeVkQueueFlags p
          = pokeByteOff p #{offset VkQueueFamilyProperties, queueFlags}

instance {-# OVERLAPPING #-}
         HasField "queueFlags" VkQueueFamilyProperties where
        type FieldType "queueFlags" VkQueueFamilyProperties = VkQueueFlags
        type FieldOptional "queueFlags" VkQueueFamilyProperties = 'True -- ' closing tick for hsc2hs
        type FieldOffset "queueFlags" VkQueueFamilyProperties =
             #{offset VkQueueFamilyProperties, queueFlags}
        type FieldIsArray "queueFlags" VkQueueFamilyProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties, queueFlags}

instance CanReadField "queueFlags" VkQueueFamilyProperties where
        {-# INLINE getField #-}
        getField = vkQueueFlags

        {-# INLINE readField #-}
        readField = readVkQueueFlags

instance {-# OVERLAPPING #-}
         HasVkQueueCount VkQueueFamilyProperties where
        type VkQueueCountMType VkQueueFamilyProperties = Word32

        {-# NOINLINE vkQueueCount #-}
        vkQueueCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, queueCount})

        {-# INLINE vkQueueCountByteOffset #-}
        vkQueueCountByteOffset ~_
          = #{offset VkQueueFamilyProperties, queueCount}

        {-# INLINE readVkQueueCount #-}
        readVkQueueCount p
          = peekByteOff p #{offset VkQueueFamilyProperties, queueCount}

        {-# INLINE writeVkQueueCount #-}
        writeVkQueueCount p
          = pokeByteOff p #{offset VkQueueFamilyProperties, queueCount}

instance {-# OVERLAPPING #-}
         HasField "queueCount" VkQueueFamilyProperties where
        type FieldType "queueCount" VkQueueFamilyProperties = Word32
        type FieldOptional "queueCount" VkQueueFamilyProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueCount" VkQueueFamilyProperties =
             #{offset VkQueueFamilyProperties, queueCount}
        type FieldIsArray "queueCount" VkQueueFamilyProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties, queueCount}

instance CanReadField "queueCount" VkQueueFamilyProperties where
        {-# INLINE getField #-}
        getField = vkQueueCount

        {-# INLINE readField #-}
        readField = readVkQueueCount

instance {-# OVERLAPPING #-}
         HasVkTimestampValidBits VkQueueFamilyProperties where
        type VkTimestampValidBitsMType VkQueueFamilyProperties = Word32

        {-# NOINLINE vkTimestampValidBits #-}
        vkTimestampValidBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, timestampValidBits})

        {-# INLINE vkTimestampValidBitsByteOffset #-}
        vkTimestampValidBitsByteOffset ~_
          = #{offset VkQueueFamilyProperties, timestampValidBits}

        {-# INLINE readVkTimestampValidBits #-}
        readVkTimestampValidBits p
          = peekByteOff p #{offset VkQueueFamilyProperties, timestampValidBits}

        {-# INLINE writeVkTimestampValidBits #-}
        writeVkTimestampValidBits p
          = pokeByteOff p #{offset VkQueueFamilyProperties, timestampValidBits}

instance {-# OVERLAPPING #-}
         HasField "timestampValidBits" VkQueueFamilyProperties where
        type FieldType "timestampValidBits" VkQueueFamilyProperties =
             Word32
        type FieldOptional "timestampValidBits" VkQueueFamilyProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "timestampValidBits" VkQueueFamilyProperties =
             #{offset VkQueueFamilyProperties, timestampValidBits}
        type FieldIsArray "timestampValidBits" VkQueueFamilyProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties, timestampValidBits}

instance CanReadField "timestampValidBits" VkQueueFamilyProperties
         where
        {-# INLINE getField #-}
        getField = vkTimestampValidBits

        {-# INLINE readField #-}
        readField = readVkTimestampValidBits

instance {-# OVERLAPPING #-}
         HasVkMinImageTransferGranularity VkQueueFamilyProperties where
        type VkMinImageTransferGranularityMType VkQueueFamilyProperties =
             VkExtent3D

        {-# NOINLINE vkMinImageTransferGranularity #-}
        vkMinImageTransferGranularity x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, minImageTransferGranularity})

        {-# INLINE vkMinImageTransferGranularityByteOffset #-}
        vkMinImageTransferGranularityByteOffset ~_
          = #{offset VkQueueFamilyProperties, minImageTransferGranularity}

        {-# INLINE readVkMinImageTransferGranularity #-}
        readVkMinImageTransferGranularity p
          = peekByteOff p #{offset VkQueueFamilyProperties, minImageTransferGranularity}

        {-# INLINE writeVkMinImageTransferGranularity #-}
        writeVkMinImageTransferGranularity p
          = pokeByteOff p #{offset VkQueueFamilyProperties, minImageTransferGranularity}

instance {-# OVERLAPPING #-}
         HasField "minImageTransferGranularity" VkQueueFamilyProperties
         where
        type FieldType "minImageTransferGranularity"
               VkQueueFamilyProperties
             = VkExtent3D
        type FieldOptional "minImageTransferGranularity"
               VkQueueFamilyProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minImageTransferGranularity"
               VkQueueFamilyProperties
             =
             #{offset VkQueueFamilyProperties, minImageTransferGranularity}
        type FieldIsArray "minImageTransferGranularity"
               VkQueueFamilyProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties, minImageTransferGranularity}

instance CanReadField "minImageTransferGranularity"
           VkQueueFamilyProperties
         where
        {-# INLINE getField #-}
        getField = vkMinImageTransferGranularity

        {-# INLINE readField #-}
        readField = readVkMinImageTransferGranularity

instance Show VkQueueFamilyProperties where
        showsPrec d x
          = showString "VkQueueFamilyProperties {" .
              showString "vkQueueFlags = " .
                showsPrec d (vkQueueFlags x) .
                  showString ", " .
                    showString "vkQueueCount = " .
                      showsPrec d (vkQueueCount x) .
                        showString ", " .
                          showString "vkTimestampValidBits = " .
                            showsPrec d (vkTimestampValidBits x) .
                              showString ", " .
                                showString "vkMinImageTransferGranularity = " .
                                  showsPrec d (vkMinImageTransferGranularity x) . showChar '}'
