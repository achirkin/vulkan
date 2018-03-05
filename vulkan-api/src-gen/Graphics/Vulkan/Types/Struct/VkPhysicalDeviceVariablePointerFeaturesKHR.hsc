#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "variablePointersStorageBuffer"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer}

instance {-# OVERLAPPING #-}
         CanWriteField "variablePointersStorageBuffer"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer}

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

instance {-# OVERLAPPING #-}
         CanReadField "variablePointers"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers}

instance {-# OVERLAPPING #-}
         CanWriteField "variablePointers"
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers}

instance Show VkPhysicalDeviceVariablePointerFeaturesKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceVariablePointerFeaturesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "variablePointersStorageBuffer = " .
                            showsPrec d (getField @"variablePointersStorageBuffer" x) .
                              showString ", " .
                                showString "variablePointers = " .
                                  showsPrec d (getField @"variablePointers" x) . showChar '}'
