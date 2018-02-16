#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHX_device_group_creation
       (-- * Vulkan extension: @VK_KHX_device_group_creation@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @KHX@
        --
        -- type: @instance@
        --
        -- Extension number: @71@
        VkPhysicalDeviceGroupPropertiesKHX(..),
        VkDeviceGroupDeviceCreateInfoKHX(..),
        vkEnumeratePhysicalDeviceGroupsKHX,
        VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION,
        pattern VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION,
        VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME,
        pattern VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX,
        pattern VK_MAX_DEVICE_GROUP_SIZE_KHX, VK_MAX_DEVICE_GROUP_SIZE_KHX,
        pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.TypeLits                     (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceGroupPropertiesKHX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     VkPhysicalDevice                 physicalDevices[VK_MAX_DEVICE_GROUP_SIZE_KHX];
--   >     VkBool32                         subsetAllocation;
--   > } VkPhysicalDeviceGroupPropertiesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceGroupPropertiesKHX.html VkPhysicalDeviceGroupPropertiesKHX registry at www.khronos.org>
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

-- | > typedef struct VkDeviceGroupDeviceCreateInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     const VkPhysicalDevice*  pPhysicalDevices;
--   > } VkDeviceGroupDeviceCreateInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGroupDeviceCreateInfoKHX.html VkDeviceGroupDeviceCreateInfoKHX registry at www.khronos.org>
data VkDeviceGroupDeviceCreateInfoKHX = VkDeviceGroupDeviceCreateInfoKHX## Addr##
                                                                          ByteArray##

instance Eq VkDeviceGroupDeviceCreateInfoKHX where
        (VkDeviceGroupDeviceCreateInfoKHX## a _) ==
          x@(VkDeviceGroupDeviceCreateInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupDeviceCreateInfoKHX where
        (VkDeviceGroupDeviceCreateInfoKHX## a _) `compare`
          x@(VkDeviceGroupDeviceCreateInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupDeviceCreateInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupDeviceCreateInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupDeviceCreateInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupDeviceCreateInfoKHX where
        unsafeAddr (VkDeviceGroupDeviceCreateInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupDeviceCreateInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupDeviceCreateInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupDeviceCreateInfoKHX where
        type StructFields VkDeviceGroupDeviceCreateInfoKHX =
             '["sType", "pNext", "physicalDeviceCount", "pPhysicalDevices"] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupDeviceCreateInfoKHX where
        type VkSTypeMType VkDeviceGroupDeviceCreateInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupDeviceCreateInfoKHX where
        type FieldType "sType" VkDeviceGroupDeviceCreateInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupDeviceCreateInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupDeviceCreateInfoKHX =
             #{offset VkDeviceGroupDeviceCreateInfoKHX, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, sType}

instance CanReadField "sType" VkDeviceGroupDeviceCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupDeviceCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupDeviceCreateInfoKHX where
        type VkPNextMType VkDeviceGroupDeviceCreateInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupDeviceCreateInfoKHX where
        type FieldType "pNext" VkDeviceGroupDeviceCreateInfoKHX = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupDeviceCreateInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupDeviceCreateInfoKHX =
             #{offset VkDeviceGroupDeviceCreateInfoKHX, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupDeviceCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupDeviceCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkPhysicalDeviceCount VkDeviceGroupDeviceCreateInfoKHX where
        type VkPhysicalDeviceCountMType VkDeviceGroupDeviceCreateInfoKHX =
             Word32

        {-# NOINLINE vkPhysicalDeviceCount #-}
        vkPhysicalDeviceCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfoKHX, physicalDeviceCount})

        {-# INLINE vkPhysicalDeviceCountByteOffset #-}
        vkPhysicalDeviceCountByteOffset ~_
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, physicalDeviceCount}

        {-# INLINE readVkPhysicalDeviceCount #-}
        readVkPhysicalDeviceCount p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, physicalDeviceCount}

        {-# INLINE writeVkPhysicalDeviceCount #-}
        writeVkPhysicalDeviceCount p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         HasField "physicalDeviceCount" VkDeviceGroupDeviceCreateInfoKHX
         where
        type FieldType "physicalDeviceCount"
               VkDeviceGroupDeviceCreateInfoKHX
             = Word32
        type FieldOptional "physicalDeviceCount"
               VkDeviceGroupDeviceCreateInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "physicalDeviceCount"
               VkDeviceGroupDeviceCreateInfoKHX
             =
             #{offset VkDeviceGroupDeviceCreateInfoKHX, physicalDeviceCount}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, physicalDeviceCount}

instance CanReadField "physicalDeviceCount"
           VkDeviceGroupDeviceCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPhysicalDeviceCount

        {-# INLINE readField #-}
        readField = readVkPhysicalDeviceCount

instance CanWriteField "physicalDeviceCount"
           VkDeviceGroupDeviceCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPhysicalDeviceCount

instance {-# OVERLAPPING #-}
         HasVkPPhysicalDevices VkDeviceGroupDeviceCreateInfoKHX where
        type VkPPhysicalDevicesMType VkDeviceGroupDeviceCreateInfoKHX =
             Ptr VkPhysicalDevice

        {-# NOINLINE vkPPhysicalDevices #-}
        vkPPhysicalDevices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfoKHX, pPhysicalDevices})

        {-# INLINE vkPPhysicalDevicesByteOffset #-}
        vkPPhysicalDevicesByteOffset ~_
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, pPhysicalDevices}

        {-# INLINE readVkPPhysicalDevices #-}
        readVkPPhysicalDevices p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, pPhysicalDevices}

        {-# INLINE writeVkPPhysicalDevices #-}
        writeVkPPhysicalDevices p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, pPhysicalDevices}

instance {-# OVERLAPPING #-}
         HasField "pPhysicalDevices" VkDeviceGroupDeviceCreateInfoKHX where
        type FieldType "pPhysicalDevices" VkDeviceGroupDeviceCreateInfoKHX
             = Ptr VkPhysicalDevice
        type FieldOptional "pPhysicalDevices"
               VkDeviceGroupDeviceCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPhysicalDevices"
               VkDeviceGroupDeviceCreateInfoKHX
             =
             #{offset VkDeviceGroupDeviceCreateInfoKHX, pPhysicalDevices}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, pPhysicalDevices}

instance CanReadField "pPhysicalDevices"
           VkDeviceGroupDeviceCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPPhysicalDevices

        {-# INLINE readField #-}
        readField = readVkPPhysicalDevices

instance CanWriteField "pPhysicalDevices"
           VkDeviceGroupDeviceCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPPhysicalDevices

instance Show VkDeviceGroupDeviceCreateInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupDeviceCreateInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPhysicalDeviceCount = " .
                            showsPrec d (vkPhysicalDeviceCount x) .
                              showString ", " .
                                showString "vkPPhysicalDevices = " .
                                  showsPrec d (vkPPhysicalDevices x) . showChar '}'

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDeviceGroupsKHX
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceGroupCount
--   >     , VkPhysicalDeviceGroupPropertiesKHX* pPhysicalDeviceGroupProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumeratePhysicalDeviceGroupsKHX.html vkEnumeratePhysicalDeviceGroupsKHX registry at www.khronos.org>
foreign import ccall unsafe "vkEnumeratePhysicalDeviceGroupsKHX"
               vkEnumeratePhysicalDeviceGroupsKHX ::
               VkInstance -- ^ instance
                          ->
                 Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                            -> Ptr VkPhysicalDeviceGroupPropertiesKHX -- ^ pPhysicalDeviceGroupProperties
                                                                      -> IO VkResult

pattern VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

type VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

pattern VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString

pattern VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME <-
        (is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME -> True)
  where VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME
          = _VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME

{-# INLINE _VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME #-}

_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString
_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME
  = Ptr "VK_KHX_device_group_creation\NUL"##

{-# INLINE is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME #-}

is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString -> Bool
is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME
  = eqCStrings _VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME

type VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME =
     "VK_KHX_device_group_creation"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX =
        VkStructureType 1000070000

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX =
        VkStructureType 1000070001

-- | If set, heap allocations allocate multiple instances by default
--
--   bitpos = @1@
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX ::
        VkMemoryHeapFlagBits

pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX =
        VkMemoryHeapFlagBits 2
