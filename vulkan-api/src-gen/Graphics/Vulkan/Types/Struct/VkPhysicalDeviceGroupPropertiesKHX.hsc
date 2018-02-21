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
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupPropertiesKHX
       (VkPhysicalDeviceGroupPropertiesKHX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                               (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                  (VK_MAX_DEVICE_GROUP_SIZE_KHX,
                                                             pattern VK_MAX_DEVICE_GROUP_SIZE_KHX)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkPhysicalDevice)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceGroupPropertiesKHX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     VkPhysicalDevice                 physicalDevices[VK_MAX_DEVICE_GROUP_SIZE_KHX];
--   >     VkBool32                         subsetAllocation;
--   > } VkPhysicalDeviceGroupPropertiesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceGroupPropertiesKHX.html VkPhysicalDeviceGroupPropertiesKHX registry at www.khronos.org>
data VkPhysicalDeviceGroupPropertiesKHX = VkPhysicalDeviceGroupPropertiesKHX## Addr##
                                                                              ByteArray##

instance Eq VkPhysicalDeviceGroupPropertiesKHX where
        (VkPhysicalDeviceGroupPropertiesKHX## a _) ==
          x@(VkPhysicalDeviceGroupPropertiesKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceGroupPropertiesKHX where
        (VkPhysicalDeviceGroupPropertiesKHX## a _) `compare`
          x@(VkPhysicalDeviceGroupPropertiesKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceGroupPropertiesKHX where
        sizeOf ~_ = #{size VkPhysicalDeviceGroupPropertiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceGroupPropertiesKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceGroupPropertiesKHX where
        unsafeAddr (VkPhysicalDeviceGroupPropertiesKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceGroupPropertiesKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceGroupPropertiesKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceGroupPropertiesKHX where
        type StructFields VkPhysicalDeviceGroupPropertiesKHX =
             '["sType", "pNext", "physicalDeviceCount", "physicalDevices", -- ' closing tick for hsc2hs
               "subsetAllocation"]
        type CUnionType VkPhysicalDeviceGroupPropertiesKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceGroupPropertiesKHX = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceGroupPropertiesKHX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceGroupPropertiesKHX where
        type VkSTypeMType VkPhysicalDeviceGroupPropertiesKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceGroupPropertiesKHX where
        type FieldType "sType" VkPhysicalDeviceGroupPropertiesKHX =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceGroupPropertiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceGroupPropertiesKHX =
             #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}
        type FieldIsArray "sType" VkPhysicalDeviceGroupPropertiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}

instance CanReadField "sType" VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceGroupPropertiesKHX where
        type VkPNextMType VkPhysicalDeviceGroupPropertiesKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceGroupPropertiesKHX where
        type FieldType "pNext" VkPhysicalDeviceGroupPropertiesKHX =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceGroupPropertiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceGroupPropertiesKHX =
             #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceGroupPropertiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}

instance CanReadField "pNext" VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkPhysicalDeviceCount VkPhysicalDeviceGroupPropertiesKHX where
        type VkPhysicalDeviceCountMType VkPhysicalDeviceGroupPropertiesKHX
             = Word32

        {-# NOINLINE vkPhysicalDeviceCount #-}
        vkPhysicalDeviceCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount})

        {-# INLINE vkPhysicalDeviceCountByteOffset #-}
        vkPhysicalDeviceCountByteOffset ~_
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}

        {-# INLINE readVkPhysicalDeviceCount #-}
        readVkPhysicalDeviceCount p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}

        {-# INLINE writeVkPhysicalDeviceCount #-}
        writeVkPhysicalDeviceCount p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         HasField "physicalDeviceCount" VkPhysicalDeviceGroupPropertiesKHX
         where
        type FieldType "physicalDeviceCount"
               VkPhysicalDeviceGroupPropertiesKHX
             = Word32
        type FieldOptional "physicalDeviceCount"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "physicalDeviceCount"
               VkPhysicalDeviceGroupPropertiesKHX
             =
             #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}
        type FieldIsArray "physicalDeviceCount"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}

instance CanReadField "physicalDeviceCount"
           VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkPhysicalDeviceCount

        {-# INLINE readField #-}
        readField = readVkPhysicalDeviceCount

instance {-# OVERLAPPING #-}
         HasVkPhysicalDevicesArray VkPhysicalDeviceGroupPropertiesKHX where
        type VkPhysicalDevicesArrayMType VkPhysicalDeviceGroupPropertiesKHX
             = VkPhysicalDevice

        {-# NOINLINE vkPhysicalDevicesArray #-}
        vkPhysicalDevicesArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: VkPhysicalDevice) +
                    #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}))

        {-# INLINE vkPhysicalDevicesArrayByteOffset #-}
        vkPhysicalDevicesArrayByteOffset ~_
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}

        {-# INLINE readVkPhysicalDevicesArray #-}
        readVkPhysicalDevicesArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: VkPhysicalDevice) +
                 #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices})

        {-# INLINE writeVkPhysicalDevicesArray #-}
        writeVkPhysicalDevicesArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: VkPhysicalDevice) +
                 #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices})

instance {-# OVERLAPPING #-}
         HasField "physicalDevices" VkPhysicalDeviceGroupPropertiesKHX where
        type FieldType "physicalDevices" VkPhysicalDeviceGroupPropertiesKHX
             = VkPhysicalDevice
        type FieldOptional "physicalDevices"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "physicalDevices"
               VkPhysicalDeviceGroupPropertiesKHX
             =
             #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}
        type FieldIsArray "physicalDevices"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}

instance (KnownNat idx,
          IndexInBounds "physicalDevices" idx
            VkPhysicalDeviceGroupPropertiesKHX) =>
         CanReadFieldArray "physicalDevices" idx
           VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 0
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 1
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 2
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 3
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}
        type FieldArrayLength "physicalDevices"
               VkPhysicalDeviceGroupPropertiesKHX
             = VK_MAX_DEVICE_GROUP_SIZE_KHX

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_DEVICE_GROUP_SIZE_KHX

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkPhysicalDevicesArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkPhysicalDevicesArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSubsetAllocation VkPhysicalDeviceGroupPropertiesKHX where
        type VkSubsetAllocationMType VkPhysicalDeviceGroupPropertiesKHX =
             VkBool32

        {-# NOINLINE vkSubsetAllocation #-}
        vkSubsetAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation})

        {-# INLINE vkSubsetAllocationByteOffset #-}
        vkSubsetAllocationByteOffset ~_
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}

        {-# INLINE readVkSubsetAllocation #-}
        readVkSubsetAllocation p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}

        {-# INLINE writeVkSubsetAllocation #-}
        writeVkSubsetAllocation p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}

instance {-# OVERLAPPING #-}
         HasField "subsetAllocation" VkPhysicalDeviceGroupPropertiesKHX
         where
        type FieldType "subsetAllocation"
               VkPhysicalDeviceGroupPropertiesKHX
             = VkBool32
        type FieldOptional "subsetAllocation"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subsetAllocation"
               VkPhysicalDeviceGroupPropertiesKHX
             =
             #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}
        type FieldIsArray "subsetAllocation"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}

instance CanReadField "subsetAllocation"
           VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkSubsetAllocation

        {-# INLINE readField #-}
        readField = readVkSubsetAllocation

instance Show VkPhysicalDeviceGroupPropertiesKHX where
        showsPrec d x
          = showString "VkPhysicalDeviceGroupPropertiesKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPhysicalDeviceCount = " .
                            showsPrec d (vkPhysicalDeviceCount x) .
                              showString ", " .
                                showString "vkPhysicalDevicesArray = [" .
                                  showsPrec d
                                    (map (vkPhysicalDevicesArray x)
                                       [1 .. VK_MAX_DEVICE_GROUP_SIZE_KHX])
                                    .
                                    showChar ']' .
                                      showString ", " .
                                        showString "vkSubsetAllocation = " .
                                          showsPrec d (vkSubsetAllocation x) . showChar '}'
