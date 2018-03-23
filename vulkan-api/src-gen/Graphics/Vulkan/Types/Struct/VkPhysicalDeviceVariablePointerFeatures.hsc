#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceVariablePointerFeatures
       (VkPhysicalDeviceVariablePointerFeatures(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                        (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo        (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2 (VkPhysicalDeviceFeatures2)
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceVariablePointerFeatures {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         variablePointersStorageBuffer;
--   >     VkBool32                         variablePointers;
--   > } VkPhysicalDeviceVariablePointerFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceVariablePointerFeatures.html VkPhysicalDeviceVariablePointerFeatures registry at www.khronos.org>
data VkPhysicalDeviceVariablePointerFeatures = VkPhysicalDeviceVariablePointerFeatures## Addr##
                                                                                        ByteArray##

instance Eq VkPhysicalDeviceVariablePointerFeatures where
        (VkPhysicalDeviceVariablePointerFeatures## a _) ==
          x@(VkPhysicalDeviceVariablePointerFeatures## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceVariablePointerFeatures where
        (VkPhysicalDeviceVariablePointerFeatures## a _) `compare`
          x@(VkPhysicalDeviceVariablePointerFeatures## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceVariablePointerFeatures where
        sizeOf ~_
          = #{size VkPhysicalDeviceVariablePointerFeatures}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceVariablePointerFeatures}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceVariablePointerFeatures
         where
        unsafeAddr (VkPhysicalDeviceVariablePointerFeatures## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceVariablePointerFeatures## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceVariablePointerFeatures##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceVariablePointerFeatures
         where
        type StructFields VkPhysicalDeviceVariablePointerFeatures =
             '["sType", "pNext", "variablePointersStorageBuffer", -- ' closing tick for hsc2hs
               "variablePointers"]
        type CUnionType VkPhysicalDeviceVariablePointerFeatures = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceVariablePointerFeatures = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceVariablePointerFeatures =
             '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceVariablePointerFeatures where
        type FieldType "sType" VkPhysicalDeviceVariablePointerFeatures =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceVariablePointerFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceVariablePointerFeatures =
             #{offset VkPhysicalDeviceVariablePointerFeatures, sType}
        type FieldIsArray "sType" VkPhysicalDeviceVariablePointerFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVariablePointerFeatures, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceVariablePointerFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeatures, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeatures, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceVariablePointerFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeatures, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceVariablePointerFeatures where
        type FieldType "pNext" VkPhysicalDeviceVariablePointerFeatures =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceVariablePointerFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceVariablePointerFeatures =
             #{offset VkPhysicalDeviceVariablePointerFeatures, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceVariablePointerFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVariablePointerFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceVariablePointerFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeatures, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceVariablePointerFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeatures, pNext}

instance {-# OVERLAPPING #-}
         HasField "variablePointersStorageBuffer"
           VkPhysicalDeviceVariablePointerFeatures
         where
        type FieldType "variablePointersStorageBuffer"
               VkPhysicalDeviceVariablePointerFeatures
             = VkBool32
        type FieldOptional "variablePointersStorageBuffer"
               VkPhysicalDeviceVariablePointerFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "variablePointersStorageBuffer"
               VkPhysicalDeviceVariablePointerFeatures
             =
             #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointersStorageBuffer}
        type FieldIsArray "variablePointersStorageBuffer"
               VkPhysicalDeviceVariablePointerFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointersStorageBuffer}

instance {-# OVERLAPPING #-}
         CanReadField "variablePointersStorageBuffer"
           VkPhysicalDeviceVariablePointerFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointersStorageBuffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointersStorageBuffer}

instance {-# OVERLAPPING #-}
         CanWriteField "variablePointersStorageBuffer"
           VkPhysicalDeviceVariablePointerFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointersStorageBuffer}

instance {-# OVERLAPPING #-}
         HasField "variablePointers" VkPhysicalDeviceVariablePointerFeatures
         where
        type FieldType "variablePointers"
               VkPhysicalDeviceVariablePointerFeatures
             = VkBool32
        type FieldOptional "variablePointers"
               VkPhysicalDeviceVariablePointerFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "variablePointers"
               VkPhysicalDeviceVariablePointerFeatures
             =
             #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointers}
        type FieldIsArray "variablePointers"
               VkPhysicalDeviceVariablePointerFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointers}

instance {-# OVERLAPPING #-}
         CanReadField "variablePointers"
           VkPhysicalDeviceVariablePointerFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointers}

instance {-# OVERLAPPING #-}
         CanWriteField "variablePointers"
           VkPhysicalDeviceVariablePointerFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointers}

instance Show VkPhysicalDeviceVariablePointerFeatures where
        showsPrec d x
          = showString "VkPhysicalDeviceVariablePointerFeatures {" .
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
