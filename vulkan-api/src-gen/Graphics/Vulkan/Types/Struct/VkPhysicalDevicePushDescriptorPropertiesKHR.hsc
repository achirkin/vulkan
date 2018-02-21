#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDevicePushDescriptorPropertiesKHR
       (VkPhysicalDevicePushDescriptorPropertiesKHR(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDevicePushDescriptorPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxPushDescriptors;
--   > } VkPhysicalDevicePushDescriptorPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDevicePushDescriptorPropertiesKHR.html VkPhysicalDevicePushDescriptorPropertiesKHR registry at www.khronos.org>
data VkPhysicalDevicePushDescriptorPropertiesKHR = VkPhysicalDevicePushDescriptorPropertiesKHR## Addr##
                                                                                                ByteArray##

instance Eq VkPhysicalDevicePushDescriptorPropertiesKHR where
        (VkPhysicalDevicePushDescriptorPropertiesKHR## a _) ==
          x@(VkPhysicalDevicePushDescriptorPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDevicePushDescriptorPropertiesKHR where
        (VkPhysicalDevicePushDescriptorPropertiesKHR## a _) `compare`
          x@(VkPhysicalDevicePushDescriptorPropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDevicePushDescriptorPropertiesKHR where
        sizeOf ~_
          = #{size VkPhysicalDevicePushDescriptorPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDevicePushDescriptorPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        unsafeAddr (VkPhysicalDevicePushDescriptorPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDevicePushDescriptorPropertiesKHR## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDevicePushDescriptorPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        type StructFields VkPhysicalDevicePushDescriptorPropertiesKHR =
             '["sType", "pNext", "maxPushDescriptors"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDevicePushDescriptorPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDevicePushDescriptorPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDevicePushDescriptorPropertiesKHR =
             '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDevicePushDescriptorPropertiesKHR where
        type VkSTypeMType VkPhysicalDevicePushDescriptorPropertiesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDevicePushDescriptorPropertiesKHR where
        type FieldType "sType" VkPhysicalDevicePushDescriptorPropertiesKHR
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             =
             #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}
        type FieldIsArray "sType"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}

instance CanReadField "sType"
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDevicePushDescriptorPropertiesKHR where
        type VkPNextMType VkPhysicalDevicePushDescriptorPropertiesKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDevicePushDescriptorPropertiesKHR where
        type FieldType "pNext" VkPhysicalDevicePushDescriptorPropertiesKHR
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             =
             #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}

instance CanReadField "pNext"
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMaxPushDescriptors VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        type VkMaxPushDescriptorsMType
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = Word32

        {-# NOINLINE vkMaxPushDescriptors #-}
        vkMaxPushDescriptors x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors})

        {-# INLINE vkMaxPushDescriptorsByteOffset #-}
        vkMaxPushDescriptorsByteOffset ~_
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}

        {-# INLINE readVkMaxPushDescriptors #-}
        readVkMaxPushDescriptors p
          = peekByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}

        {-# INLINE writeVkMaxPushDescriptors #-}
        writeVkMaxPushDescriptors p
          = pokeByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}

instance {-# OVERLAPPING #-}
         HasField "maxPushDescriptors"
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        type FieldType "maxPushDescriptors"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = Word32
        type FieldOptional "maxPushDescriptors"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPushDescriptors"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             =
             #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}
        type FieldIsArray "maxPushDescriptors"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}

instance CanReadField "maxPushDescriptors"
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxPushDescriptors

        {-# INLINE readField #-}
        readField = readVkMaxPushDescriptors

instance CanWriteField "maxPushDescriptors"
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxPushDescriptors

instance Show VkPhysicalDevicePushDescriptorPropertiesKHR where
        showsPrec d x
          = showString "VkPhysicalDevicePushDescriptorPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMaxPushDescriptors = " .
                            showsPrec d (vkMaxPushDescriptors x) . showChar '}'
