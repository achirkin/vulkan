#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties2KHR
       (VkPhysicalDeviceMemoryProperties2KHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties (VkPhysicalDeviceMemoryProperties)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMemoryProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceMemoryProperties memoryProperties;
--   > } VkPhysicalDeviceMemoryProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceMemoryProperties2KHR.html VkPhysicalDeviceMemoryProperties2KHR registry at www.khronos.org>
data VkPhysicalDeviceMemoryProperties2KHR = VkPhysicalDeviceMemoryProperties2KHR## Addr##
                                                                                  ByteArray##

instance Eq VkPhysicalDeviceMemoryProperties2KHR where
        (VkPhysicalDeviceMemoryProperties2KHR## a _) ==
          x@(VkPhysicalDeviceMemoryProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMemoryProperties2KHR where
        (VkPhysicalDeviceMemoryProperties2KHR## a _) `compare`
          x@(VkPhysicalDeviceMemoryProperties2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMemoryProperties2KHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceMemoryProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMemoryProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMemoryProperties2KHR
         where
        unsafeAddr (VkPhysicalDeviceMemoryProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMemoryProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMemoryProperties2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMemoryProperties2KHR where
        type StructFields VkPhysicalDeviceMemoryProperties2KHR =
             '["sType", "pNext", "memoryProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceMemoryProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceMemoryProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceMemoryProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMemoryProperties2KHR where
        type FieldType "sType" VkPhysicalDeviceMemoryProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMemoryProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceMemoryProperties2KHR =
             #{offset VkPhysicalDeviceMemoryProperties2KHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceMemoryProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMemoryProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceMemoryProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceMemoryProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMemoryProperties2KHR where
        type FieldType "pNext" VkPhysicalDeviceMemoryProperties2KHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMemoryProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceMemoryProperties2KHR =
             #{offset VkPhysicalDeviceMemoryProperties2KHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceMemoryProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMemoryProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceMemoryProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceMemoryProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryProperties" VkPhysicalDeviceMemoryProperties2KHR
         where
        type FieldType "memoryProperties"
               VkPhysicalDeviceMemoryProperties2KHR
             = VkPhysicalDeviceMemoryProperties
        type FieldOptional "memoryProperties"
               VkPhysicalDeviceMemoryProperties2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryProperties"
               VkPhysicalDeviceMemoryProperties2KHR
             =
             #{offset VkPhysicalDeviceMemoryProperties2KHR, memoryProperties}
        type FieldIsArray "memoryProperties"
               VkPhysicalDeviceMemoryProperties2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMemoryProperties2KHR, memoryProperties}

instance {-# OVERLAPPING #-}
         CanReadField "memoryProperties"
           VkPhysicalDeviceMemoryProperties2KHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties2KHR, memoryProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, memoryProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryProperties"
           VkPhysicalDeviceMemoryProperties2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, memoryProperties}

instance Show VkPhysicalDeviceMemoryProperties2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceMemoryProperties2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryProperties = " .
                            showsPrec d (getField @"memoryProperties" x) . showChar '}'
