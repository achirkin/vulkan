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
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDProperties
       (VkPhysicalDeviceIDProperties(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                                             (KnownNat,
                                                                           natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                                (VK_LUID_SIZE,
                                                                           pattern VK_LUID_SIZE,
                                                                           VK_UUID_SIZE,
                                                                           pattern VK_UUID_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceIDProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint8_t                          deviceUUID[VK_UUID_SIZE];
--   >     uint8_t                          driverUUID[VK_UUID_SIZE];
--   >     uint8_t                          deviceLUID[VK_LUID_SIZE];
--   >     uint32_t                         deviceNodeMask;
--   >     VkBool32                         deviceLUIDValid;
--   > } VkPhysicalDeviceIDProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceIDProperties.html VkPhysicalDeviceIDProperties registry at www.khronos.org>
data VkPhysicalDeviceIDProperties = VkPhysicalDeviceIDProperties## Addr##
                                                                  ByteArray##

instance Eq VkPhysicalDeviceIDProperties where
        (VkPhysicalDeviceIDProperties## a _) ==
          x@(VkPhysicalDeviceIDProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceIDProperties where
        (VkPhysicalDeviceIDProperties## a _) `compare`
          x@(VkPhysicalDeviceIDProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceIDProperties where
        sizeOf ~_ = #{size VkPhysicalDeviceIDProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceIDProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceIDProperties where
        unsafeAddr (VkPhysicalDeviceIDProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceIDProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceIDProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceIDProperties where
        type StructFields VkPhysicalDeviceIDProperties =
             '["sType", "pNext", "deviceUUID", "driverUUID", "deviceLUID", -- ' closing tick for hsc2hs
               "deviceNodeMask", "deviceLUIDValid"]
        type CUnionType VkPhysicalDeviceIDProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceIDProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceIDProperties =
             '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceIDProperties where
        type FieldType "sType" VkPhysicalDeviceIDProperties =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceIDProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceIDProperties =
             #{offset VkPhysicalDeviceIDProperties, sType}
        type FieldIsArray "sType" VkPhysicalDeviceIDProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceIDProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceIDProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceIDProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceIDProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceIDProperties where
        type FieldType "pNext" VkPhysicalDeviceIDProperties = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceIDProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceIDProperties =
             #{offset VkPhysicalDeviceIDProperties, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceIDProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceIDProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceIDProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceIDProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceIDProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceUUID" VkPhysicalDeviceIDProperties where
        type FieldType "deviceUUID" VkPhysicalDeviceIDProperties = Word8
        type FieldOptional "deviceUUID" VkPhysicalDeviceIDProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceUUID" VkPhysicalDeviceIDProperties =
             #{offset VkPhysicalDeviceIDProperties, deviceUUID}
        type FieldIsArray "deviceUUID" VkPhysicalDeviceIDProperties = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDProperties, deviceUUID}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "deviceUUID" idx VkPhysicalDeviceIDProperties) =>
         CanReadFieldArray "deviceUUID" idx VkPhysicalDeviceIDProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 0 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 1 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 2 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 3 VkPhysicalDeviceIDProperties #-}
        type FieldArrayLength "deviceUUID" VkPhysicalDeviceIDProperties =
             VK_UUID_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_UUID_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceIDProperties, deviceUUID} +
                      sizeOf (undefined :: Word8) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceIDProperties, deviceUUID} +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "deviceUUID" idx VkPhysicalDeviceIDProperties) =>
         CanWriteFieldArray "deviceUUID" idx VkPhysicalDeviceIDProperties
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceUUID" 0 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceUUID" 1 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceUUID" 2 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceUUID" 3 VkPhysicalDeviceIDProperties #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceIDProperties, deviceUUID} +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "driverUUID" VkPhysicalDeviceIDProperties where
        type FieldType "driverUUID" VkPhysicalDeviceIDProperties = Word8
        type FieldOptional "driverUUID" VkPhysicalDeviceIDProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "driverUUID" VkPhysicalDeviceIDProperties =
             #{offset VkPhysicalDeviceIDProperties, driverUUID}
        type FieldIsArray "driverUUID" VkPhysicalDeviceIDProperties = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDProperties, driverUUID}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "driverUUID" idx VkPhysicalDeviceIDProperties) =>
         CanReadFieldArray "driverUUID" idx VkPhysicalDeviceIDProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 0 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 1 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 2 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 3 VkPhysicalDeviceIDProperties #-}
        type FieldArrayLength "driverUUID" VkPhysicalDeviceIDProperties =
             VK_UUID_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_UUID_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceIDProperties, driverUUID} +
                      sizeOf (undefined :: Word8) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceIDProperties, driverUUID} +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "driverUUID" idx VkPhysicalDeviceIDProperties) =>
         CanWriteFieldArray "driverUUID" idx VkPhysicalDeviceIDProperties
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "driverUUID" 0 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "driverUUID" 1 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "driverUUID" 2 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "driverUUID" 3 VkPhysicalDeviceIDProperties #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceIDProperties, driverUUID} +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "deviceLUID" VkPhysicalDeviceIDProperties where
        type FieldType "deviceLUID" VkPhysicalDeviceIDProperties = Word8
        type FieldOptional "deviceLUID" VkPhysicalDeviceIDProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceLUID" VkPhysicalDeviceIDProperties =
             #{offset VkPhysicalDeviceIDProperties, deviceLUID}
        type FieldIsArray "deviceLUID" VkPhysicalDeviceIDProperties = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDProperties, deviceLUID}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "deviceLUID" idx VkPhysicalDeviceIDProperties) =>
         CanReadFieldArray "deviceLUID" idx VkPhysicalDeviceIDProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 0 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 1 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 2 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 3 VkPhysicalDeviceIDProperties #-}
        type FieldArrayLength "deviceLUID" VkPhysicalDeviceIDProperties =
             VK_LUID_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_LUID_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceIDProperties, deviceLUID} +
                      sizeOf (undefined :: Word8) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceIDProperties, deviceLUID} +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "deviceLUID" idx VkPhysicalDeviceIDProperties) =>
         CanWriteFieldArray "deviceLUID" idx VkPhysicalDeviceIDProperties
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceLUID" 0 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceLUID" 1 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceLUID" 2 VkPhysicalDeviceIDProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceLUID" 3 VkPhysicalDeviceIDProperties #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceIDProperties, deviceLUID} +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "deviceNodeMask" VkPhysicalDeviceIDProperties where
        type FieldType "deviceNodeMask" VkPhysicalDeviceIDProperties =
             Word32
        type FieldOptional "deviceNodeMask" VkPhysicalDeviceIDProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceNodeMask" VkPhysicalDeviceIDProperties =
             #{offset VkPhysicalDeviceIDProperties, deviceNodeMask}
        type FieldIsArray "deviceNodeMask" VkPhysicalDeviceIDProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDProperties, deviceNodeMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceNodeMask" VkPhysicalDeviceIDProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDProperties, deviceNodeMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceIDProperties, deviceNodeMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceNodeMask" VkPhysicalDeviceIDProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceIDProperties, deviceNodeMask}

instance {-# OVERLAPPING #-}
         HasField "deviceLUIDValid" VkPhysicalDeviceIDProperties where
        type FieldType "deviceLUIDValid" VkPhysicalDeviceIDProperties =
             VkBool32
        type FieldOptional "deviceLUIDValid" VkPhysicalDeviceIDProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceLUIDValid" VkPhysicalDeviceIDProperties =
             #{offset VkPhysicalDeviceIDProperties, deviceLUIDValid}
        type FieldIsArray "deviceLUIDValid" VkPhysicalDeviceIDProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDProperties, deviceLUIDValid}

instance {-# OVERLAPPING #-}
         CanReadField "deviceLUIDValid" VkPhysicalDeviceIDProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDProperties, deviceLUIDValid})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceIDProperties, deviceLUIDValid}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceLUIDValid" VkPhysicalDeviceIDProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceIDProperties, deviceLUIDValid}

instance Show VkPhysicalDeviceIDProperties where
        showsPrec d x
          = showString "VkPhysicalDeviceIDProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          (showString "deviceUUID = [" .
                             showsPrec d
                               (let s = sizeOf
                                          (undefined ::
                                             FieldType "deviceUUID" VkPhysicalDeviceIDProperties)
                                    o = fieldOffset @"deviceUUID" @VkPhysicalDeviceIDProperties
                                    f i
                                      = peekByteOff (unsafePtr x) i ::
                                          IO (FieldType "deviceUUID" VkPhysicalDeviceIDProperties)
                                  in
                                  unsafeDupablePerformIO . mapM f $
                                    map (\ i -> o + i * s) [0 .. VK_UUID_SIZE - 1])
                               . showChar ']')
                            .
                            showString ", " .
                              (showString "driverUUID = [" .
                                 showsPrec d
                                   (let s = sizeOf
                                              (undefined ::
                                                 FieldType "driverUUID"
                                                   VkPhysicalDeviceIDProperties)
                                        o = fieldOffset @"driverUUID" @VkPhysicalDeviceIDProperties
                                        f i
                                          = peekByteOff (unsafePtr x) i ::
                                              IO
                                                (FieldType "driverUUID"
                                                   VkPhysicalDeviceIDProperties)
                                      in
                                      unsafeDupablePerformIO . mapM f $
                                        map (\ i -> o + i * s) [0 .. VK_UUID_SIZE - 1])
                                   . showChar ']')
                                .
                                showString ", " .
                                  (showString "deviceLUID = [" .
                                     showsPrec d
                                       (let s = sizeOf
                                                  (undefined ::
                                                     FieldType "deviceLUID"
                                                       VkPhysicalDeviceIDProperties)
                                            o = fieldOffset @"deviceLUID"
                                                  @VkPhysicalDeviceIDProperties
                                            f i
                                              = peekByteOff (unsafePtr x) i ::
                                                  IO
                                                    (FieldType "deviceLUID"
                                                       VkPhysicalDeviceIDProperties)
                                          in
                                          unsafeDupablePerformIO . mapM f $
                                            map (\ i -> o + i * s) [0 .. VK_LUID_SIZE - 1])
                                       . showChar ']')
                                    .
                                    showString ", " .
                                      showString "deviceNodeMask = " .
                                        showsPrec d (getField @"deviceNodeMask" x) .
                                          showString ", " .
                                            showString "deviceLUIDValid = " .
                                              showsPrec d (getField @"deviceLUIDValid" x) .
                                                showChar '}'
