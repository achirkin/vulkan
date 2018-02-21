#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceVariablePointerFeaturesKHR
       (VkPhysicalDeviceVariablePointerFeaturesKHR(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                           (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo           (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR (VkPhysicalDeviceFeatures2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceVariablePointerFeaturesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         variablePointersStorageBuffer;
--   >     VkBool32                         variablePointers;
--   > } VkPhysicalDeviceVariablePointerFeaturesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceVariablePointerFeaturesKHR.html VkPhysicalDeviceVariablePointerFeaturesKHR registry at www.khronos.org>
data VkPhysicalDeviceVariablePointerFeaturesKHR = VkPhysicalDeviceVariablePointerFeaturesKHR## Addr##
                                                                                              ByteArray##

instance Eq VkPhysicalDeviceVariablePointerFeaturesKHR where
        (VkPhysicalDeviceVariablePointerFeaturesKHR## a _) ==
          x@(VkPhysicalDeviceVariablePointerFeaturesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceVariablePointerFeaturesKHR where
        (VkPhysicalDeviceVariablePointerFeaturesKHR## a _) `compare`
          x@(VkPhysicalDeviceVariablePointerFeaturesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceVariablePointerFeaturesKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceVariablePointerFeaturesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceVariablePointerFeaturesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        unsafeAddr (VkPhysicalDeviceVariablePointerFeaturesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceVariablePointerFeaturesKHR## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceVariablePointerFeaturesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        type StructFields VkPhysicalDeviceVariablePointerFeaturesKHR =
             '["sType", "pNext", "variablePointersStorageBuffer", -- ' closing tick for hsc2hs
               "variablePointers"]
        type CUnionType VkPhysicalDeviceVariablePointerFeaturesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceVariablePointerFeaturesKHR =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceVariablePointerFeaturesKHR =
             '[VkPhysicalDeviceFeatures2KHR, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceVariablePointerFeaturesKHR where
        type VkSTypeMType VkPhysicalDeviceVariablePointerFeaturesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceVariablePointerFeaturesKHR where
        type FieldType "sType" VkPhysicalDeviceVariablePointerFeaturesKHR =
             VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceVariablePointerFeaturesKHR
             =
             #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType}

instance CanReadField "sType"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceVariablePointerFeaturesKHR where
        type VkPNextMType VkPhysicalDeviceVariablePointerFeaturesKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceVariablePointerFeaturesKHR where
        type FieldType "pNext" VkPhysicalDeviceVariablePointerFeaturesKHR =
             Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceVariablePointerFeaturesKHR
             =
             #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkVariablePointersStorageBuffer
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        type VkVariablePointersStorageBufferMType
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = VkBool32

        {-# NOINLINE vkVariablePointersStorageBuffer #-}
        vkVariablePointersStorageBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer})

        {-# INLINE vkVariablePointersStorageBufferByteOffset #-}
        vkVariablePointersStorageBufferByteOffset ~_
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer}

        {-# INLINE readVkVariablePointersStorageBuffer #-}
        readVkVariablePointersStorageBuffer p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer}

        {-# INLINE writeVkVariablePointersStorageBuffer #-}
        writeVkVariablePointersStorageBuffer p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer}

instance {-# OVERLAPPING #-}
         HasField "variablePointersStorageBuffer"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        type FieldType "variablePointersStorageBuffer"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = VkBool32
        type FieldOptional "variablePointersStorageBuffer"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "variablePointersStorageBuffer"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             =
             #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer}
        type FieldIsArray "variablePointersStorageBuffer"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer}

instance CanReadField "variablePointersStorageBuffer"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkVariablePointersStorageBuffer

        {-# INLINE readField #-}
        readField = readVkVariablePointersStorageBuffer

instance CanWriteField "variablePointersStorageBuffer"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkVariablePointersStorageBuffer

instance {-# OVERLAPPING #-}
         HasVkVariablePointers VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        type VkVariablePointersMType
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = VkBool32

        {-# NOINLINE vkVariablePointers #-}
        vkVariablePointers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers})

        {-# INLINE vkVariablePointersByteOffset #-}
        vkVariablePointersByteOffset ~_
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers}

        {-# INLINE readVkVariablePointers #-}
        readVkVariablePointers p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers}

        {-# INLINE writeVkVariablePointers #-}
        writeVkVariablePointers p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers}

instance {-# OVERLAPPING #-}
         HasField "variablePointers"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        type FieldType "variablePointers"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = VkBool32
        type FieldOptional "variablePointers"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "variablePointers"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             =
             #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers}
        type FieldIsArray "variablePointers"
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers}

instance CanReadField "variablePointers"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkVariablePointers

        {-# INLINE readField #-}
        readField = readVkVariablePointers

instance CanWriteField "variablePointers"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkVariablePointers

instance Show VkPhysicalDeviceVariablePointerFeaturesKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceVariablePointerFeaturesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkVariablePointersStorageBuffer = " .
                            showsPrec d (vkVariablePointersStorageBuffer x) .
                              showString ", " .
                                showString "vkVariablePointers = " .
                                  showsPrec d (vkVariablePointers x) . showChar '}'
