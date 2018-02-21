#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupDeviceCreateInfoKHX
       (VkDeviceGroupDeviceCreateInfoKHX(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                   (VkPhysicalDevice)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupDeviceCreateInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     const VkPhysicalDevice*  pPhysicalDevices;
--   > } VkDeviceGroupDeviceCreateInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupDeviceCreateInfoKHX.html VkDeviceGroupDeviceCreateInfoKHX registry at www.khronos.org>
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
        type CUnionType VkDeviceGroupDeviceCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupDeviceCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupDeviceCreateInfoKHX =
             '[VkDeviceCreateInfo] -- ' closing tick for hsc2hs

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
        type FieldIsArray "sType" VkDeviceGroupDeviceCreateInfoKHX = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pNext" VkDeviceGroupDeviceCreateInfoKHX = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "physicalDeviceCount"
               VkDeviceGroupDeviceCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pPhysicalDevices"
               VkDeviceGroupDeviceCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs

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
