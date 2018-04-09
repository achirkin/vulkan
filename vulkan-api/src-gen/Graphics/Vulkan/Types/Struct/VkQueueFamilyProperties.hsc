#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties
       (VkQueueFamilyProperties(..)) where
import           Foreign.Storable                        (Storable (..))
import           GHC.Base                                (Addr##, ByteArray##,
                                                          byteArrayContents##,
                                                          plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkQueueFlags (VkQueueFlags)
import           Graphics.Vulkan.Types.Struct.VkExtent3D (VkExtent3D)
import           System.IO.Unsafe                        (unsafeDupablePerformIO)

-- | > typedef struct VkQueueFamilyProperties {
--   >     VkQueueFlags           queueFlags;
--   >     uint32_t               queueCount;
--   >     uint32_t               timestampValidBits;
--   >     VkExtent3D             minImageTransferGranularity;
--   > } VkQueueFamilyProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkQueueFamilyPropertiesVkQueueFamilyProperties registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "queueFlags" VkQueueFamilyProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, queueFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties, queueFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFlags" VkQueueFamilyProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties, queueFlags}

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

instance {-# OVERLAPPING #-}
         CanReadField "queueCount" VkQueueFamilyProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, queueCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties, queueCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queueCount" VkQueueFamilyProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties, queueCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "timestampValidBits" VkQueueFamilyProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, timestampValidBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties, timestampValidBits}

instance {-# OVERLAPPING #-}
         CanWriteField "timestampValidBits" VkQueueFamilyProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties, timestampValidBits}

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

instance {-# OVERLAPPING #-}
         CanReadField "minImageTransferGranularity" VkQueueFamilyProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, minImageTransferGranularity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties, minImageTransferGranularity}

instance {-# OVERLAPPING #-}
         CanWriteField "minImageTransferGranularity" VkQueueFamilyProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties, minImageTransferGranularity}

instance Show VkQueueFamilyProperties where
        showsPrec d x
          = showString "VkQueueFamilyProperties {" .
              showString "queueFlags = " .
                showsPrec d (getField @"queueFlags" x) .
                  showString ", " .
                    showString "queueCount = " .
                      showsPrec d (getField @"queueCount" x) .
                        showString ", " .
                          showString "timestampValidBits = " .
                            showsPrec d (getField @"timestampValidBits" x) .
                              showString ", " .
                                showString "minImageTransferGranularity = " .
                                  showsPrec d (getField @"minImageTransferGranularity" x) .
                                    showChar '}'
