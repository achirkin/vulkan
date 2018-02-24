#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
       (VkPhysicalDeviceProperties(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                                                  (KnownNat,
                                                                                natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                                     (VK_MAX_PHYSICAL_DEVICE_NAME_SIZE,
                                                                                pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE,
                                                                                VK_UUID_SIZE,
                                                                                pattern VK_UUID_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType               (VkPhysicalDeviceType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits           (VkPhysicalDeviceLimits)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties (VkPhysicalDeviceSparseProperties)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceProperties {
--   >     uint32_t       apiVersion;
--   >     uint32_t       driverVersion;
--   >     uint32_t       vendorID;
--   >     uint32_t       deviceID;
--   >     VkPhysicalDeviceType deviceType;
--   >     char           deviceName[VK_MAX_PHYSICAL_DEVICE_NAME_SIZE];
--   >     uint8_t        pipelineCacheUUID[VK_UUID_SIZE];
--   >     VkPhysicalDeviceLimits limits;
--   >     VkPhysicalDeviceSparseProperties sparseProperties;
--   > } VkPhysicalDeviceProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceProperties.html VkPhysicalDeviceProperties registry at www.khronos.org>
data VkPhysicalDeviceProperties = VkPhysicalDeviceProperties## Addr##
                                                              ByteArray##

instance Eq VkPhysicalDeviceProperties where
        (VkPhysicalDeviceProperties## a _) ==
          x@(VkPhysicalDeviceProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceProperties where
        (VkPhysicalDeviceProperties## a _) `compare`
          x@(VkPhysicalDeviceProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceProperties where
        sizeOf ~_ = #{size VkPhysicalDeviceProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPhysicalDeviceProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceProperties where
        unsafeAddr (VkPhysicalDeviceProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceProperties where
        type StructFields VkPhysicalDeviceProperties =
             '["apiVersion", "driverVersion", "vendorID", "deviceID", -- ' closing tick for hsc2hs
               "deviceType", "deviceName", "pipelineCacheUUID", "limits",
               "sparseProperties"]
        type CUnionType VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkApiVersion VkPhysicalDeviceProperties where
        type VkApiVersionMType VkPhysicalDeviceProperties = Word32

        {-# NOINLINE vkApiVersion #-}
        vkApiVersion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, apiVersion})

        {-# INLINE vkApiVersionByteOffset #-}
        vkApiVersionByteOffset ~_
          = #{offset VkPhysicalDeviceProperties, apiVersion}

        {-# INLINE readVkApiVersion #-}
        readVkApiVersion p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, apiVersion}

        {-# INLINE writeVkApiVersion #-}
        writeVkApiVersion p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, apiVersion}

instance {-# OVERLAPPING #-}
         HasField "apiVersion" VkPhysicalDeviceProperties where
        type FieldType "apiVersion" VkPhysicalDeviceProperties = Word32
        type FieldOptional "apiVersion" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "apiVersion" VkPhysicalDeviceProperties =
             #{offset VkPhysicalDeviceProperties, apiVersion}
        type FieldIsArray "apiVersion" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties, apiVersion}

instance CanReadField "apiVersion" VkPhysicalDeviceProperties where
        {-# INLINE getField #-}
        getField = vkApiVersion

        {-# INLINE readField #-}
        readField = readVkApiVersion

instance CanWriteField "apiVersion" VkPhysicalDeviceProperties
         where
        {-# INLINE writeField #-}
        writeField = writeVkApiVersion

instance {-# OVERLAPPING #-}
         HasVkDriverVersion VkPhysicalDeviceProperties where
        type VkDriverVersionMType VkPhysicalDeviceProperties = Word32

        {-# NOINLINE vkDriverVersion #-}
        vkDriverVersion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, driverVersion})

        {-# INLINE vkDriverVersionByteOffset #-}
        vkDriverVersionByteOffset ~_
          = #{offset VkPhysicalDeviceProperties, driverVersion}

        {-# INLINE readVkDriverVersion #-}
        readVkDriverVersion p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, driverVersion}

        {-# INLINE writeVkDriverVersion #-}
        writeVkDriverVersion p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, driverVersion}

instance {-# OVERLAPPING #-}
         HasField "driverVersion" VkPhysicalDeviceProperties where
        type FieldType "driverVersion" VkPhysicalDeviceProperties = Word32
        type FieldOptional "driverVersion" VkPhysicalDeviceProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "driverVersion" VkPhysicalDeviceProperties =
             #{offset VkPhysicalDeviceProperties, driverVersion}
        type FieldIsArray "driverVersion" VkPhysicalDeviceProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties, driverVersion}

instance CanReadField "driverVersion" VkPhysicalDeviceProperties
         where
        {-# INLINE getField #-}
        getField = vkDriverVersion

        {-# INLINE readField #-}
        readField = readVkDriverVersion

instance CanWriteField "driverVersion" VkPhysicalDeviceProperties
         where
        {-# INLINE writeField #-}
        writeField = writeVkDriverVersion

instance {-# OVERLAPPING #-}
         HasVkVendorID VkPhysicalDeviceProperties where
        type VkVendorIDMType VkPhysicalDeviceProperties = Word32

        {-# NOINLINE vkVendorID #-}
        vkVendorID x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, vendorID})

        {-# INLINE vkVendorIDByteOffset #-}
        vkVendorIDByteOffset ~_
          = #{offset VkPhysicalDeviceProperties, vendorID}

        {-# INLINE readVkVendorID #-}
        readVkVendorID p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, vendorID}

        {-# INLINE writeVkVendorID #-}
        writeVkVendorID p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, vendorID}

instance {-# OVERLAPPING #-}
         HasField "vendorID" VkPhysicalDeviceProperties where
        type FieldType "vendorID" VkPhysicalDeviceProperties = Word32
        type FieldOptional "vendorID" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "vendorID" VkPhysicalDeviceProperties =
             #{offset VkPhysicalDeviceProperties, vendorID}
        type FieldIsArray "vendorID" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties, vendorID}

instance CanReadField "vendorID" VkPhysicalDeviceProperties where
        {-# INLINE getField #-}
        getField = vkVendorID

        {-# INLINE readField #-}
        readField = readVkVendorID

instance CanWriteField "vendorID" VkPhysicalDeviceProperties where
        {-# INLINE writeField #-}
        writeField = writeVkVendorID

instance {-# OVERLAPPING #-}
         HasVkDeviceID VkPhysicalDeviceProperties where
        type VkDeviceIDMType VkPhysicalDeviceProperties = Word32

        {-# NOINLINE vkDeviceID #-}
        vkDeviceID x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, deviceID})

        {-# INLINE vkDeviceIDByteOffset #-}
        vkDeviceIDByteOffset ~_
          = #{offset VkPhysicalDeviceProperties, deviceID}

        {-# INLINE readVkDeviceID #-}
        readVkDeviceID p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, deviceID}

        {-# INLINE writeVkDeviceID #-}
        writeVkDeviceID p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, deviceID}

instance {-# OVERLAPPING #-}
         HasField "deviceID" VkPhysicalDeviceProperties where
        type FieldType "deviceID" VkPhysicalDeviceProperties = Word32
        type FieldOptional "deviceID" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceID" VkPhysicalDeviceProperties =
             #{offset VkPhysicalDeviceProperties, deviceID}
        type FieldIsArray "deviceID" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties, deviceID}

instance CanReadField "deviceID" VkPhysicalDeviceProperties where
        {-# INLINE getField #-}
        getField = vkDeviceID

        {-# INLINE readField #-}
        readField = readVkDeviceID

instance CanWriteField "deviceID" VkPhysicalDeviceProperties where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceID

instance {-# OVERLAPPING #-}
         HasVkDeviceType VkPhysicalDeviceProperties where
        type VkDeviceTypeMType VkPhysicalDeviceProperties =
             VkPhysicalDeviceType

        {-# NOINLINE vkDeviceType #-}
        vkDeviceType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, deviceType})

        {-# INLINE vkDeviceTypeByteOffset #-}
        vkDeviceTypeByteOffset ~_
          = #{offset VkPhysicalDeviceProperties, deviceType}

        {-# INLINE readVkDeviceType #-}
        readVkDeviceType p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, deviceType}

        {-# INLINE writeVkDeviceType #-}
        writeVkDeviceType p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, deviceType}

instance {-# OVERLAPPING #-}
         HasField "deviceType" VkPhysicalDeviceProperties where
        type FieldType "deviceType" VkPhysicalDeviceProperties =
             VkPhysicalDeviceType
        type FieldOptional "deviceType" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceType" VkPhysicalDeviceProperties =
             #{offset VkPhysicalDeviceProperties, deviceType}
        type FieldIsArray "deviceType" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties, deviceType}

instance CanReadField "deviceType" VkPhysicalDeviceProperties where
        {-# INLINE getField #-}
        getField = vkDeviceType

        {-# INLINE readField #-}
        readField = readVkDeviceType

instance CanWriteField "deviceType" VkPhysicalDeviceProperties
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceType

instance {-# OVERLAPPING #-}
         HasVkDeviceNameArray VkPhysicalDeviceProperties where
        type VkDeviceNameArrayMType VkPhysicalDeviceProperties = CChar

        {-# NOINLINE vkDeviceNameArray #-}
        vkDeviceNameArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: CChar) +
                    #{offset VkPhysicalDeviceProperties, deviceName}))

        {-# INLINE vkDeviceNameArrayByteOffset #-}
        vkDeviceNameArrayByteOffset ~_
          = #{offset VkPhysicalDeviceProperties, deviceName}

        {-# INLINE readVkDeviceNameArray #-}
        readVkDeviceNameArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: CChar) +
                 #{offset VkPhysicalDeviceProperties, deviceName})

        {-# INLINE writeVkDeviceNameArray #-}
        writeVkDeviceNameArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: CChar) +
                 #{offset VkPhysicalDeviceProperties, deviceName})

instance {-# OVERLAPPING #-}
         HasField "deviceName" VkPhysicalDeviceProperties where
        type FieldType "deviceName" VkPhysicalDeviceProperties = CChar
        type FieldOptional "deviceName" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceName" VkPhysicalDeviceProperties =
             #{offset VkPhysicalDeviceProperties, deviceName}
        type FieldIsArray "deviceName" VkPhysicalDeviceProperties = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties, deviceName}

instance (KnownNat idx,
          IndexInBounds "deviceName" idx VkPhysicalDeviceProperties) =>
         CanReadFieldArray "deviceName" idx VkPhysicalDeviceProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "deviceName" 0 VkPhysicalDeviceProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceName" 1 VkPhysicalDeviceProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceName" 2 VkPhysicalDeviceProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceName" 3 VkPhysicalDeviceProperties #-}
        type FieldArrayLength "deviceName" VkPhysicalDeviceProperties =
             VK_MAX_PHYSICAL_DEVICE_NAME_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_PHYSICAL_DEVICE_NAME_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkDeviceNameArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkDeviceNameArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "deviceName" idx VkPhysicalDeviceProperties) =>
         CanWriteFieldArray "deviceName" idx VkPhysicalDeviceProperties
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceName" 0 VkPhysicalDeviceProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceName" 1 VkPhysicalDeviceProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceName" 2 VkPhysicalDeviceProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceName" 3 VkPhysicalDeviceProperties #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkDeviceNameArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkPipelineCacheUUIDArray VkPhysicalDeviceProperties where
        type VkPipelineCacheUUIDArrayMType VkPhysicalDeviceProperties =
             Word8

        {-# NOINLINE vkPipelineCacheUUIDArray #-}
        vkPipelineCacheUUIDArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word8) +
                    #{offset VkPhysicalDeviceProperties, pipelineCacheUUID}))

        {-# INLINE vkPipelineCacheUUIDArrayByteOffset #-}
        vkPipelineCacheUUIDArrayByteOffset ~_
          = #{offset VkPhysicalDeviceProperties, pipelineCacheUUID}

        {-# INLINE readVkPipelineCacheUUIDArray #-}
        readVkPipelineCacheUUIDArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceProperties, pipelineCacheUUID})

        {-# INLINE writeVkPipelineCacheUUIDArray #-}
        writeVkPipelineCacheUUIDArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceProperties, pipelineCacheUUID})

instance {-# OVERLAPPING #-}
         HasField "pipelineCacheUUID" VkPhysicalDeviceProperties where
        type FieldType "pipelineCacheUUID" VkPhysicalDeviceProperties =
             Word8
        type FieldOptional "pipelineCacheUUID" VkPhysicalDeviceProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineCacheUUID" VkPhysicalDeviceProperties =
             #{offset VkPhysicalDeviceProperties, pipelineCacheUUID}
        type FieldIsArray "pipelineCacheUUID" VkPhysicalDeviceProperties =
             'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties, pipelineCacheUUID}

instance (KnownNat idx,
          IndexInBounds "pipelineCacheUUID" idx
            VkPhysicalDeviceProperties) =>
         CanReadFieldArray "pipelineCacheUUID" idx
           VkPhysicalDeviceProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "pipelineCacheUUID" 0 VkPhysicalDeviceProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "pipelineCacheUUID" 1 VkPhysicalDeviceProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "pipelineCacheUUID" 2 VkPhysicalDeviceProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "pipelineCacheUUID" 3 VkPhysicalDeviceProperties
                       #-}
        type FieldArrayLength "pipelineCacheUUID"
               VkPhysicalDeviceProperties
             = VK_UUID_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_UUID_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkPipelineCacheUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkPipelineCacheUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "pipelineCacheUUID" idx
            VkPhysicalDeviceProperties) =>
         CanWriteFieldArray "pipelineCacheUUID" idx
           VkPhysicalDeviceProperties
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "pipelineCacheUUID" 0 VkPhysicalDeviceProperties
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "pipelineCacheUUID" 1 VkPhysicalDeviceProperties
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "pipelineCacheUUID" 2 VkPhysicalDeviceProperties
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "pipelineCacheUUID" 3 VkPhysicalDeviceProperties
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkPipelineCacheUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkLimits VkPhysicalDeviceProperties
         where
        type VkLimitsMType VkPhysicalDeviceProperties =
             VkPhysicalDeviceLimits

        {-# NOINLINE vkLimits #-}
        vkLimits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, limits})

        {-# INLINE vkLimitsByteOffset #-}
        vkLimitsByteOffset ~_
          = #{offset VkPhysicalDeviceProperties, limits}

        {-# INLINE readVkLimits #-}
        readVkLimits p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, limits}

        {-# INLINE writeVkLimits #-}
        writeVkLimits p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, limits}

instance {-# OVERLAPPING #-}
         HasField "limits" VkPhysicalDeviceProperties where
        type FieldType "limits" VkPhysicalDeviceProperties =
             VkPhysicalDeviceLimits
        type FieldOptional "limits" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "limits" VkPhysicalDeviceProperties =
             #{offset VkPhysicalDeviceProperties, limits}
        type FieldIsArray "limits" VkPhysicalDeviceProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties, limits}

instance CanReadField "limits" VkPhysicalDeviceProperties where
        {-# INLINE getField #-}
        getField = vkLimits

        {-# INLINE readField #-}
        readField = readVkLimits

instance CanWriteField "limits" VkPhysicalDeviceProperties where
        {-# INLINE writeField #-}
        writeField = writeVkLimits

instance {-# OVERLAPPING #-}
         HasVkSparseProperties VkPhysicalDeviceProperties where
        type VkSparsePropertiesMType VkPhysicalDeviceProperties =
             VkPhysicalDeviceSparseProperties

        {-# NOINLINE vkSparseProperties #-}
        vkSparseProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, sparseProperties})

        {-# INLINE vkSparsePropertiesByteOffset #-}
        vkSparsePropertiesByteOffset ~_
          = #{offset VkPhysicalDeviceProperties, sparseProperties}

        {-# INLINE readVkSparseProperties #-}
        readVkSparseProperties p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, sparseProperties}

        {-# INLINE writeVkSparseProperties #-}
        writeVkSparseProperties p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, sparseProperties}

instance {-# OVERLAPPING #-}
         HasField "sparseProperties" VkPhysicalDeviceProperties where
        type FieldType "sparseProperties" VkPhysicalDeviceProperties =
             VkPhysicalDeviceSparseProperties
        type FieldOptional "sparseProperties" VkPhysicalDeviceProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseProperties" VkPhysicalDeviceProperties =
             #{offset VkPhysicalDeviceProperties, sparseProperties}
        type FieldIsArray "sparseProperties" VkPhysicalDeviceProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties, sparseProperties}

instance CanReadField "sparseProperties" VkPhysicalDeviceProperties
         where
        {-# INLINE getField #-}
        getField = vkSparseProperties

        {-# INLINE readField #-}
        readField = readVkSparseProperties

instance CanWriteField "sparseProperties"
           VkPhysicalDeviceProperties
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseProperties

instance Show VkPhysicalDeviceProperties where
        showsPrec d x
          = showString "VkPhysicalDeviceProperties {" .
              showString "vkApiVersion = " .
                showsPrec d (vkApiVersion x) .
                  showString ", " .
                    showString "vkDriverVersion = " .
                      showsPrec d (vkDriverVersion x) .
                        showString ", " .
                          showString "vkVendorID = " .
                            showsPrec d (vkVendorID x) .
                              showString ", " .
                                showString "vkDeviceID = " .
                                  showsPrec d (vkDeviceID x) .
                                    showString ", " .
                                      showString "vkDeviceType = " .
                                        showsPrec d (vkDeviceType x) .
                                          showString ", " .
                                            showString "vkDeviceNameArray = [" .
                                              showsPrec d
                                                (map (vkDeviceNameArray x)
                                                   [1 .. VK_MAX_PHYSICAL_DEVICE_NAME_SIZE])
                                                .
                                                showChar ']' .
                                                  showString ", " .
                                                    showString "vkPipelineCacheUUIDArray = [" .
                                                      showsPrec d
                                                        (map (vkPipelineCacheUUIDArray x)
                                                           [1 .. VK_UUID_SIZE])
                                                        .
                                                        showChar ']' .
                                                          showString ", " .
                                                            showString "vkLimits = " .
                                                              showsPrec d (vkLimits x) .
                                                                showString ", " .
                                                                  showString "vkSparseProperties = "
                                                                    .
                                                                    showsPrec d
                                                                      (vkSparseProperties x)
                                                                      . showChar '}'
