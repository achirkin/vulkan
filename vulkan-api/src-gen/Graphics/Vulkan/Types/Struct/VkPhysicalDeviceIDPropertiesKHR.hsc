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
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkPhysicalDeviceIDPropertiesKHR where
        type VkSTypeMType VkPhysicalDeviceIDPropertiesKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, sType}

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

instance CanReadField "sType" VkPhysicalDeviceIDPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceIDPropertiesKHR where
        type VkPNextMType VkPhysicalDeviceIDPropertiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, pNext}

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

instance CanReadField "pNext" VkPhysicalDeviceIDPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkDeviceUUIDArray VkPhysicalDeviceIDPropertiesKHR where
        type VkDeviceUUIDArrayMType VkPhysicalDeviceIDPropertiesKHR = Word8

        {-# NOINLINE vkDeviceUUIDArray #-}
        vkDeviceUUIDArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word8) +
                    #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID}))

        {-# INLINE vkDeviceUUIDArrayByteOffset #-}
        vkDeviceUUIDArrayByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID}

        {-# INLINE readVkDeviceUUIDArray #-}
        readVkDeviceUUIDArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID})

        {-# INLINE writeVkDeviceUUIDArray #-}
        writeVkDeviceUUIDArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID})

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

instance (KnownNat idx,
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
        getFieldArray x
          = vkDeviceUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkDeviceUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkDriverUUIDArray VkPhysicalDeviceIDPropertiesKHR where
        type VkDriverUUIDArrayMType VkPhysicalDeviceIDPropertiesKHR = Word8

        {-# NOINLINE vkDriverUUIDArray #-}
        vkDriverUUIDArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word8) +
                    #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID}))

        {-# INLINE vkDriverUUIDArrayByteOffset #-}
        vkDriverUUIDArrayByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID}

        {-# INLINE readVkDriverUUIDArray #-}
        readVkDriverUUIDArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID})

        {-# INLINE writeVkDriverUUIDArray #-}
        writeVkDriverUUIDArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID})

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

instance (KnownNat idx,
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
        getFieldArray x
          = vkDriverUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkDriverUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkDeviceLUIDArray VkPhysicalDeviceIDPropertiesKHR where
        type VkDeviceLUIDArrayMType VkPhysicalDeviceIDPropertiesKHR = Word8

        {-# NOINLINE vkDeviceLUIDArray #-}
        vkDeviceLUIDArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word8) +
                    #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID}))

        {-# INLINE vkDeviceLUIDArrayByteOffset #-}
        vkDeviceLUIDArrayByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID}

        {-# INLINE readVkDeviceLUIDArray #-}
        readVkDeviceLUIDArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID})

        {-# INLINE writeVkDeviceLUIDArray #-}
        writeVkDeviceLUIDArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID})

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

instance (KnownNat idx,
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
        getFieldArray x
          = vkDeviceLUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkDeviceLUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkDeviceNodeMask VkPhysicalDeviceIDPropertiesKHR where
        type VkDeviceNodeMaskMType VkPhysicalDeviceIDPropertiesKHR = Word32

        {-# NOINLINE vkDeviceNodeMask #-}
        vkDeviceNodeMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask})

        {-# INLINE vkDeviceNodeMaskByteOffset #-}
        vkDeviceNodeMaskByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask}

        {-# INLINE readVkDeviceNodeMask #-}
        readVkDeviceNodeMask p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask}

        {-# INLINE writeVkDeviceNodeMask #-}
        writeVkDeviceNodeMask p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask}

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

instance CanReadField "deviceNodeMask"
           VkPhysicalDeviceIDPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkDeviceNodeMask

        {-# INLINE readField #-}
        readField = readVkDeviceNodeMask

instance {-# OVERLAPPING #-}
         HasVkDeviceLUIDValid VkPhysicalDeviceIDPropertiesKHR where
        type VkDeviceLUIDValidMType VkPhysicalDeviceIDPropertiesKHR =
             VkBool32

        {-# NOINLINE vkDeviceLUIDValid #-}
        vkDeviceLUIDValid x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid})

        {-# INLINE vkDeviceLUIDValidByteOffset #-}
        vkDeviceLUIDValidByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid}

        {-# INLINE readVkDeviceLUIDValid #-}
        readVkDeviceLUIDValid p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid}

        {-# INLINE writeVkDeviceLUIDValid #-}
        writeVkDeviceLUIDValid p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid}

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

instance CanReadField "deviceLUIDValid"
           VkPhysicalDeviceIDPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkDeviceLUIDValid

        {-# INLINE readField #-}
        readField = readVkDeviceLUIDValid

instance Show VkPhysicalDeviceIDPropertiesKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceIDPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceUUIDArray = [" .
                            showsPrec d (map (vkDeviceUUIDArray x) [1 .. VK_UUID_SIZE]) .
                              showChar ']' .
                                showString ", " .
                                  showString "vkDriverUUIDArray = [" .
                                    showsPrec d (map (vkDriverUUIDArray x) [1 .. VK_UUID_SIZE]) .
                                      showChar ']' .
                                        showString ", " .
                                          showString "vkDeviceLUIDArray = [" .
                                            showsPrec d
                                              (map (vkDeviceLUIDArray x) [1 .. VK_LUID_SIZE_KHR])
                                              .
                                              showChar ']' .
                                                showString ", " .
                                                  showString "vkDeviceNodeMask = " .
                                                    showsPrec d (vkDeviceNodeMask x) .
                                                      showString ", " .
                                                        showString "vkDeviceLUIDValid = " .
                                                          showsPrec d (vkDeviceLUIDValid x) .
                                                            showChar '}'
