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
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDPropertiesKHR
       (VkPhysicalDeviceIDPropertiesKHR(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                                                (KnownNat,
                                                                              natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                                   (VK_LUID_SIZE_KHR,
                                                                              pattern VK_LUID_SIZE_KHR,
                                                                              VK_UUID_SIZE,
                                                                              pattern VK_UUID_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                             (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceIDPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint8_t                          deviceUUID[VK_UUID_SIZE];
--   >     uint8_t                          driverUUID[VK_UUID_SIZE];
--   >     uint8_t                          deviceLUID[VK_LUID_SIZE_KHR];
--   >     uint32_t                         deviceNodeMask;
--   >     VkBool32                         deviceLUIDValid;
--   > } VkPhysicalDeviceIDPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceIDPropertiesKHR.html VkPhysicalDeviceIDPropertiesKHR registry at www.khronos.org>
data VkPhysicalDeviceIDPropertiesKHR = VkPhysicalDeviceIDPropertiesKHR## Addr##
                                                                        ByteArray##

instance Eq VkPhysicalDeviceIDPropertiesKHR where
        (VkPhysicalDeviceIDPropertiesKHR## a _) ==
          x@(VkPhysicalDeviceIDPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceIDPropertiesKHR where
        (VkPhysicalDeviceIDPropertiesKHR## a _) `compare`
          x@(VkPhysicalDeviceIDPropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceIDPropertiesKHR where
        sizeOf ~_ = #{size VkPhysicalDeviceIDPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceIDPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceIDPropertiesKHR where
        unsafeAddr (VkPhysicalDeviceIDPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceIDPropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceIDPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceIDPropertiesKHR where
        type StructFields VkPhysicalDeviceIDPropertiesKHR =
             '["sType", "pNext", "deviceUUID", "driverUUID", "deviceLUID", -- ' closing tick for hsc2hs
               "deviceNodeMask", "deviceLUIDValid"]
        type CUnionType VkPhysicalDeviceIDPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceIDPropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceIDPropertiesKHR =
             '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "sType" VkPhysicalDeviceIDPropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceIDPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceIDPropertiesKHR =
             #{offset VkPhysicalDeviceIDPropertiesKHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceIDPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceIDPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceIDPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "pNext" VkPhysicalDeviceIDPropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceIDPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceIDPropertiesKHR =
             #{offset VkPhysicalDeviceIDPropertiesKHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceIDPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceIDPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceIDPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceUUID" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "deviceUUID" VkPhysicalDeviceIDPropertiesKHR = Word8
        type FieldOptional "deviceUUID" VkPhysicalDeviceIDPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceUUID" VkPhysicalDeviceIDPropertiesKHR =
             #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID}
        type FieldIsArray "deviceUUID" VkPhysicalDeviceIDPropertiesKHR =
             'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "deviceUUID" idx VkPhysicalDeviceIDPropertiesKHR) =>
         CanReadFieldArray "deviceUUID" idx VkPhysicalDeviceIDPropertiesKHR
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 0 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 1 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 2 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 3 VkPhysicalDeviceIDPropertiesKHR
                       #-}
        type FieldArrayLength "deviceUUID" VkPhysicalDeviceIDPropertiesKHR
             = VK_UUID_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_UUID_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID}
                      +
                      sizeOf (undefined :: Word8) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID}
                 +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "deviceUUID" idx VkPhysicalDeviceIDPropertiesKHR) =>
         CanWriteFieldArray "deviceUUID" idx VkPhysicalDeviceIDPropertiesKHR
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceUUID" 0 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceUUID" 1 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceUUID" 2 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceUUID" 3 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID}
                 +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "driverUUID" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "driverUUID" VkPhysicalDeviceIDPropertiesKHR = Word8
        type FieldOptional "driverUUID" VkPhysicalDeviceIDPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "driverUUID" VkPhysicalDeviceIDPropertiesKHR =
             #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID}
        type FieldIsArray "driverUUID" VkPhysicalDeviceIDPropertiesKHR =
             'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "driverUUID" idx VkPhysicalDeviceIDPropertiesKHR) =>
         CanReadFieldArray "driverUUID" idx VkPhysicalDeviceIDPropertiesKHR
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 0 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 1 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 2 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 3 VkPhysicalDeviceIDPropertiesKHR
                       #-}
        type FieldArrayLength "driverUUID" VkPhysicalDeviceIDPropertiesKHR
             = VK_UUID_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_UUID_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID}
                      +
                      sizeOf (undefined :: Word8) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID}
                 +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "driverUUID" idx VkPhysicalDeviceIDPropertiesKHR) =>
         CanWriteFieldArray "driverUUID" idx VkPhysicalDeviceIDPropertiesKHR
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "driverUUID" 0 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "driverUUID" 1 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "driverUUID" 2 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "driverUUID" 3 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID}
                 +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "deviceLUID" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "deviceLUID" VkPhysicalDeviceIDPropertiesKHR = Word8
        type FieldOptional "deviceLUID" VkPhysicalDeviceIDPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceLUID" VkPhysicalDeviceIDPropertiesKHR =
             #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID}
        type FieldIsArray "deviceLUID" VkPhysicalDeviceIDPropertiesKHR =
             'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "deviceLUID" idx VkPhysicalDeviceIDPropertiesKHR) =>
         CanReadFieldArray "deviceLUID" idx VkPhysicalDeviceIDPropertiesKHR
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 0 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 1 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 2 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 3 VkPhysicalDeviceIDPropertiesKHR
                       #-}
        type FieldArrayLength "deviceLUID" VkPhysicalDeviceIDPropertiesKHR
             = VK_LUID_SIZE_KHR

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_LUID_SIZE_KHR

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID}
                      +
                      sizeOf (undefined :: Word8) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID}
                 +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "deviceLUID" idx VkPhysicalDeviceIDPropertiesKHR) =>
         CanWriteFieldArray "deviceLUID" idx VkPhysicalDeviceIDPropertiesKHR
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceLUID" 0 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceLUID" 1 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceLUID" 2 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "deviceLUID" 3 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID}
                 +
                 sizeOf (undefined :: Word8) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "deviceNodeMask" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "deviceNodeMask" VkPhysicalDeviceIDPropertiesKHR =
             Word32
        type FieldOptional "deviceNodeMask" VkPhysicalDeviceIDPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceNodeMask" VkPhysicalDeviceIDPropertiesKHR =
             #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask}
        type FieldIsArray "deviceNodeMask" VkPhysicalDeviceIDPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceNodeMask" VkPhysicalDeviceIDPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceNodeMask" VkPhysicalDeviceIDPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask}

instance {-# OVERLAPPING #-}
         HasField "deviceLUIDValid" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "deviceLUIDValid" VkPhysicalDeviceIDPropertiesKHR =
             VkBool32
        type FieldOptional "deviceLUIDValid"
               VkPhysicalDeviceIDPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceLUIDValid" VkPhysicalDeviceIDPropertiesKHR
             =
             #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid}
        type FieldIsArray "deviceLUIDValid" VkPhysicalDeviceIDPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid}

instance {-# OVERLAPPING #-}
         CanReadField "deviceLUIDValid" VkPhysicalDeviceIDPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceLUIDValid" VkPhysicalDeviceIDPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid}

instance Show VkPhysicalDeviceIDPropertiesKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceIDPropertiesKHR {" .
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
                                             FieldType "deviceUUID" VkPhysicalDeviceIDPropertiesKHR)
                                    o = fieldOffset @"deviceUUID" @VkPhysicalDeviceIDPropertiesKHR
                                    f i
                                      = peekByteOff (unsafePtr x) i ::
                                          IO
                                            (FieldType "deviceUUID" VkPhysicalDeviceIDPropertiesKHR)
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
                                                   VkPhysicalDeviceIDPropertiesKHR)
                                        o = fieldOffset @"driverUUID"
                                              @VkPhysicalDeviceIDPropertiesKHR
                                        f i
                                          = peekByteOff (unsafePtr x) i ::
                                              IO
                                                (FieldType "driverUUID"
                                                   VkPhysicalDeviceIDPropertiesKHR)
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
                                                       VkPhysicalDeviceIDPropertiesKHR)
                                            o = fieldOffset @"deviceLUID"
                                                  @VkPhysicalDeviceIDPropertiesKHR
                                            f i
                                              = peekByteOff (unsafePtr x) i ::
                                                  IO
                                                    (FieldType "deviceLUID"
                                                       VkPhysicalDeviceIDPropertiesKHR)
                                          in
                                          unsafeDupablePerformIO . mapM f $
                                            map (\ i -> o + i * s) [0 .. VK_LUID_SIZE_KHR - 1])
                                       . showChar ']')
                                    .
                                    showString ", " .
                                      showString "deviceNodeMask = " .
                                        showsPrec d (getField @"deviceNodeMask" x) .
                                          showString ", " .
                                            showString "deviceLUIDValid = " .
                                              showsPrec d (getField @"deviceLUIDValid" x) .
                                                showChar '}'
