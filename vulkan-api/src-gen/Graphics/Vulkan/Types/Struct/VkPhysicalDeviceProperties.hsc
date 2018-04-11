#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
       (VkPhysicalDeviceProperties(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Base                                                      (Addr##,
                                                                                ByteArray##,
                                                                                Proxy##,
                                                                                byteArrayContents##,
                                                                                plusAddr##,
                                                                                proxy##)
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceProperties VkPhysicalDeviceProperties registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "apiVersion" VkPhysicalDeviceProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, apiVersion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, apiVersion}

instance {-# OVERLAPPING #-}
         CanWriteField "apiVersion" VkPhysicalDeviceProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, apiVersion}

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

instance {-# OVERLAPPING #-}
         CanReadField "driverVersion" VkPhysicalDeviceProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, driverVersion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, driverVersion}

instance {-# OVERLAPPING #-}
         CanWriteField "driverVersion" VkPhysicalDeviceProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, driverVersion}

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

instance {-# OVERLAPPING #-}
         CanReadField "vendorID" VkPhysicalDeviceProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, vendorID})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, vendorID}

instance {-# OVERLAPPING #-}
         CanWriteField "vendorID" VkPhysicalDeviceProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, vendorID}

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

instance {-# OVERLAPPING #-}
         CanReadField "deviceID" VkPhysicalDeviceProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, deviceID})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, deviceID}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceID" VkPhysicalDeviceProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, deviceID}

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

instance {-# OVERLAPPING #-}
         CanReadField "deviceType" VkPhysicalDeviceProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, deviceType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, deviceType}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceType" VkPhysicalDeviceProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, deviceType}

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceProperties, deviceName} +
                      sizeOf (undefined :: CChar) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceProperties, deviceName} +
                 sizeOf (undefined :: CChar) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceProperties, deviceName} +
                 sizeOf (undefined :: CChar) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceProperties, pipelineCacheUUID}
                      +
                      sizeOf (undefined :: Word8) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceProperties, pipelineCacheUUID}
                 +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceProperties, pipelineCacheUUID}
                 +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         CanReadField "limits" VkPhysicalDeviceProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, limits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, limits}

instance {-# OVERLAPPING #-}
         CanWriteField "limits" VkPhysicalDeviceProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, limits}

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

instance {-# OVERLAPPING #-}
         CanReadField "sparseProperties" VkPhysicalDeviceProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties, sparseProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties, sparseProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseProperties" VkPhysicalDeviceProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties, sparseProperties}

instance Show VkPhysicalDeviceProperties where
        showsPrec d x
          = showString "VkPhysicalDeviceProperties {" .
              showString "apiVersion = " .
                showsPrec d (getField @"apiVersion" x) .
                  showString ", " .
                    showString "driverVersion = " .
                      showsPrec d (getField @"driverVersion" x) .
                        showString ", " .
                          showString "vendorID = " .
                            showsPrec d (getField @"vendorID" x) .
                              showString ", " .
                                showString "deviceID = " .
                                  showsPrec d (getField @"deviceID" x) .
                                    showString ", " .
                                      showString "deviceType = " .
                                        showsPrec d (getField @"deviceType" x) .
                                          showString ", " .
                                            (showString "deviceName = [" .
                                               showsPrec d
                                                 (let s = sizeOf
                                                            (undefined ::
                                                               FieldType "deviceName"
                                                                 VkPhysicalDeviceProperties)
                                                      o = fieldOffset @"deviceName"
                                                            @VkPhysicalDeviceProperties
                                                      f i
                                                        = peekByteOff (unsafePtr x) i ::
                                                            IO
                                                              (FieldType "deviceName"
                                                                 VkPhysicalDeviceProperties)
                                                    in
                                                    unsafeDupablePerformIO . mapM f $
                                                      map (\ i -> o + i * s)
                                                        [0 .. VK_MAX_PHYSICAL_DEVICE_NAME_SIZE - 1])
                                                 . showChar ']')
                                              .
                                              showString ", " .
                                                (showString "pipelineCacheUUID = [" .
                                                   showsPrec d
                                                     (let s = sizeOf
                                                                (undefined ::
                                                                   FieldType "pipelineCacheUUID"
                                                                     VkPhysicalDeviceProperties)
                                                          o = fieldOffset @"pipelineCacheUUID"
                                                                @VkPhysicalDeviceProperties
                                                          f i
                                                            = peekByteOff (unsafePtr x) i ::
                                                                IO
                                                                  (FieldType "pipelineCacheUUID"
                                                                     VkPhysicalDeviceProperties)
                                                        in
                                                        unsafeDupablePerformIO . mapM f $
                                                          map (\ i -> o + i * s)
                                                            [0 .. VK_UUID_SIZE - 1])
                                                     . showChar ']')
                                                  .
                                                  showString ", " .
                                                    showString "limits = " .
                                                      showsPrec d (getField @"limits" x) .
                                                        showString ", " .
                                                          showString "sparseProperties = " .
                                                            showsPrec d
                                                              (getField @"sparseProperties" x)
                                                              . showChar '}'
