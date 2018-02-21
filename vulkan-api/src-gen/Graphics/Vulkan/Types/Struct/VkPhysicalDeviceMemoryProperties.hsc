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
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties
       (VkPhysicalDeviceMemoryProperties(..)) where
import           Foreign.Storable                          (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                              (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                 (VK_MAX_MEMORY_HEAPS, pattern VK_MAX_MEMORY_HEAPS,
                                                            VK_MAX_MEMORY_TYPES,
                                                            pattern VK_MAX_MEMORY_TYPES)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkMemoryHeap (VkMemoryHeap)
import           Graphics.Vulkan.Types.Struct.VkMemoryType (VkMemoryType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                          (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMemoryProperties {
--   >     uint32_t               memoryTypeCount;
--   >     VkMemoryType           memoryTypes[VK_MAX_MEMORY_TYPES];
--   >     uint32_t               memoryHeapCount;
--   >     VkMemoryHeap           memoryHeaps[VK_MAX_MEMORY_HEAPS];
--   > } VkPhysicalDeviceMemoryProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceMemoryProperties.html VkPhysicalDeviceMemoryProperties registry at www.khronos.org>
data VkPhysicalDeviceMemoryProperties = VkPhysicalDeviceMemoryProperties## Addr##
                                                                          ByteArray##

instance Eq VkPhysicalDeviceMemoryProperties where
        (VkPhysicalDeviceMemoryProperties## a _) ==
          x@(VkPhysicalDeviceMemoryProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMemoryProperties where
        (VkPhysicalDeviceMemoryProperties## a _) `compare`
          x@(VkPhysicalDeviceMemoryProperties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMemoryProperties where
        sizeOf ~_ = #{size VkPhysicalDeviceMemoryProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMemoryProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMemoryProperties where
        unsafeAddr (VkPhysicalDeviceMemoryProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMemoryProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMemoryProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMemoryProperties where
        type StructFields VkPhysicalDeviceMemoryProperties =
             '["memoryTypeCount", "memoryTypes", "memoryHeapCount", -- ' closing tick for hsc2hs
               "memoryHeaps"]
        type CUnionType VkPhysicalDeviceMemoryProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceMemoryProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceMemoryProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkMemoryTypeCount VkPhysicalDeviceMemoryProperties where
        type VkMemoryTypeCountMType VkPhysicalDeviceMemoryProperties =
             Word32

        {-# NOINLINE vkMemoryTypeCount #-}
        vkMemoryTypeCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties, memoryTypeCount})

        {-# INLINE vkMemoryTypeCountByteOffset #-}
        vkMemoryTypeCountByteOffset ~_
          = #{offset VkPhysicalDeviceMemoryProperties, memoryTypeCount}

        {-# INLINE readVkMemoryTypeCount #-}
        readVkMemoryTypeCount p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties, memoryTypeCount}

        {-# INLINE writeVkMemoryTypeCount #-}
        writeVkMemoryTypeCount p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties, memoryTypeCount}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeCount" VkPhysicalDeviceMemoryProperties where
        type FieldType "memoryTypeCount" VkPhysicalDeviceMemoryProperties =
             Word32
        type FieldOptional "memoryTypeCount"
               VkPhysicalDeviceMemoryProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeCount" VkPhysicalDeviceMemoryProperties
             =
             #{offset VkPhysicalDeviceMemoryProperties, memoryTypeCount}
        type FieldIsArray "memoryTypeCount"
               VkPhysicalDeviceMemoryProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMemoryProperties, memoryTypeCount}

instance CanReadField "memoryTypeCount"
           VkPhysicalDeviceMemoryProperties
         where
        {-# INLINE getField #-}
        getField = vkMemoryTypeCount

        {-# INLINE readField #-}
        readField = readVkMemoryTypeCount

instance {-# OVERLAPPING #-}
         HasVkMemoryTypesArray VkPhysicalDeviceMemoryProperties where
        type VkMemoryTypesArrayMType VkPhysicalDeviceMemoryProperties =
             VkMemoryType

        {-# NOINLINE vkMemoryTypesArray #-}
        vkMemoryTypesArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: VkMemoryType) +
                    #{offset VkPhysicalDeviceMemoryProperties, memoryTypes}))

        {-# INLINE vkMemoryTypesArrayByteOffset #-}
        vkMemoryTypesArrayByteOffset ~_
          = #{offset VkPhysicalDeviceMemoryProperties, memoryTypes}

        {-# INLINE readVkMemoryTypesArray #-}
        readVkMemoryTypesArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: VkMemoryType) +
                 #{offset VkPhysicalDeviceMemoryProperties, memoryTypes})

        {-# INLINE writeVkMemoryTypesArray #-}
        writeVkMemoryTypesArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: VkMemoryType) +
                 #{offset VkPhysicalDeviceMemoryProperties, memoryTypes})

instance {-# OVERLAPPING #-}
         HasField "memoryTypes" VkPhysicalDeviceMemoryProperties where
        type FieldType "memoryTypes" VkPhysicalDeviceMemoryProperties =
             VkMemoryType
        type FieldOptional "memoryTypes" VkPhysicalDeviceMemoryProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypes" VkPhysicalDeviceMemoryProperties =
             #{offset VkPhysicalDeviceMemoryProperties, memoryTypes}
        type FieldIsArray "memoryTypes" VkPhysicalDeviceMemoryProperties =
             'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMemoryProperties, memoryTypes}

instance (KnownNat idx,
          IndexInBounds "memoryTypes" idx
            VkPhysicalDeviceMemoryProperties) =>
         CanReadFieldArray "memoryTypes" idx
           VkPhysicalDeviceMemoryProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "memoryTypes" 0 VkPhysicalDeviceMemoryProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "memoryTypes" 1 VkPhysicalDeviceMemoryProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "memoryTypes" 2 VkPhysicalDeviceMemoryProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "memoryTypes" 3 VkPhysicalDeviceMemoryProperties
                       #-}
        type FieldArrayLength "memoryTypes"
               VkPhysicalDeviceMemoryProperties
             = VK_MAX_MEMORY_TYPES

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_MEMORY_TYPES

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkMemoryTypesArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkMemoryTypesArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkMemoryHeapCount VkPhysicalDeviceMemoryProperties where
        type VkMemoryHeapCountMType VkPhysicalDeviceMemoryProperties =
             Word32

        {-# NOINLINE vkMemoryHeapCount #-}
        vkMemoryHeapCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties, memoryHeapCount})

        {-# INLINE vkMemoryHeapCountByteOffset #-}
        vkMemoryHeapCountByteOffset ~_
          = #{offset VkPhysicalDeviceMemoryProperties, memoryHeapCount}

        {-# INLINE readVkMemoryHeapCount #-}
        readVkMemoryHeapCount p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties, memoryHeapCount}

        {-# INLINE writeVkMemoryHeapCount #-}
        writeVkMemoryHeapCount p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties, memoryHeapCount}

instance {-# OVERLAPPING #-}
         HasField "memoryHeapCount" VkPhysicalDeviceMemoryProperties where
        type FieldType "memoryHeapCount" VkPhysicalDeviceMemoryProperties =
             Word32
        type FieldOptional "memoryHeapCount"
               VkPhysicalDeviceMemoryProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryHeapCount" VkPhysicalDeviceMemoryProperties
             =
             #{offset VkPhysicalDeviceMemoryProperties, memoryHeapCount}
        type FieldIsArray "memoryHeapCount"
               VkPhysicalDeviceMemoryProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMemoryProperties, memoryHeapCount}

instance CanReadField "memoryHeapCount"
           VkPhysicalDeviceMemoryProperties
         where
        {-# INLINE getField #-}
        getField = vkMemoryHeapCount

        {-# INLINE readField #-}
        readField = readVkMemoryHeapCount

instance {-# OVERLAPPING #-}
         HasVkMemoryHeapsArray VkPhysicalDeviceMemoryProperties where
        type VkMemoryHeapsArrayMType VkPhysicalDeviceMemoryProperties =
             VkMemoryHeap

        {-# NOINLINE vkMemoryHeapsArray #-}
        vkMemoryHeapsArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: VkMemoryHeap) +
                    #{offset VkPhysicalDeviceMemoryProperties, memoryHeaps}))

        {-# INLINE vkMemoryHeapsArrayByteOffset #-}
        vkMemoryHeapsArrayByteOffset ~_
          = #{offset VkPhysicalDeviceMemoryProperties, memoryHeaps}

        {-# INLINE readVkMemoryHeapsArray #-}
        readVkMemoryHeapsArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: VkMemoryHeap) +
                 #{offset VkPhysicalDeviceMemoryProperties, memoryHeaps})

        {-# INLINE writeVkMemoryHeapsArray #-}
        writeVkMemoryHeapsArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: VkMemoryHeap) +
                 #{offset VkPhysicalDeviceMemoryProperties, memoryHeaps})

instance {-# OVERLAPPING #-}
         HasField "memoryHeaps" VkPhysicalDeviceMemoryProperties where
        type FieldType "memoryHeaps" VkPhysicalDeviceMemoryProperties =
             VkMemoryHeap
        type FieldOptional "memoryHeaps" VkPhysicalDeviceMemoryProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryHeaps" VkPhysicalDeviceMemoryProperties =
             #{offset VkPhysicalDeviceMemoryProperties, memoryHeaps}
        type FieldIsArray "memoryHeaps" VkPhysicalDeviceMemoryProperties =
             'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMemoryProperties, memoryHeaps}

instance (KnownNat idx,
          IndexInBounds "memoryHeaps" idx
            VkPhysicalDeviceMemoryProperties) =>
         CanReadFieldArray "memoryHeaps" idx
           VkPhysicalDeviceMemoryProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "memoryHeaps" 0 VkPhysicalDeviceMemoryProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "memoryHeaps" 1 VkPhysicalDeviceMemoryProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "memoryHeaps" 2 VkPhysicalDeviceMemoryProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "memoryHeaps" 3 VkPhysicalDeviceMemoryProperties
                       #-}
        type FieldArrayLength "memoryHeaps"
               VkPhysicalDeviceMemoryProperties
             = VK_MAX_MEMORY_HEAPS

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_MEMORY_HEAPS

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkMemoryHeapsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkMemoryHeapsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance Show VkPhysicalDeviceMemoryProperties where
        showsPrec d x
          = showString "VkPhysicalDeviceMemoryProperties {" .
              showString "vkMemoryTypeCount = " .
                showsPrec d (vkMemoryTypeCount x) .
                  showString ", " .
                    showString "vkMemoryTypesArray = [" .
                      showsPrec d (map (vkMemoryTypesArray x) [1 .. VK_MAX_MEMORY_TYPES])
                        .
                        showChar ']' .
                          showString ", " .
                            showString "vkMemoryHeapCount = " .
                              showsPrec d (vkMemoryHeapCount x) .
                                showString ", " .
                                  showString "vkMemoryHeapsArray = [" .
                                    showsPrec d
                                      (map (vkMemoryHeapsArray x) [1 .. VK_MAX_MEMORY_HEAPS])
                                      . showChar ']' . showChar '}'
