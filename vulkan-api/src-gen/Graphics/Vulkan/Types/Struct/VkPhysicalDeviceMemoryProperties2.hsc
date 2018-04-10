#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties2
       (VkPhysicalDeviceMemoryProperties2(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Base                                                      (Addr##,
                                                                                ByteArray##,
                                                                                byteArrayContents##,
                                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties (VkPhysicalDeviceMemoryProperties)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMemoryProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceMemoryProperties memoryProperties;
--   > } VkPhysicalDeviceMemoryProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDeviceMemoryProperties2 VkPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
data VkPhysicalDeviceMemoryProperties2 = VkPhysicalDeviceMemoryProperties2## Addr##
                                                                            ByteArray##

instance Eq VkPhysicalDeviceMemoryProperties2 where
        (VkPhysicalDeviceMemoryProperties2## a _) ==
          x@(VkPhysicalDeviceMemoryProperties2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMemoryProperties2 where
        (VkPhysicalDeviceMemoryProperties2## a _) `compare`
          x@(VkPhysicalDeviceMemoryProperties2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMemoryProperties2 where
        sizeOf ~_ = #{size VkPhysicalDeviceMemoryProperties2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMemoryProperties2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMemoryProperties2 where
        unsafeAddr (VkPhysicalDeviceMemoryProperties2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMemoryProperties2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMemoryProperties2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMemoryProperties2 where
        type StructFields VkPhysicalDeviceMemoryProperties2 =
             '["sType", "pNext", "memoryProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceMemoryProperties2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceMemoryProperties2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceMemoryProperties2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMemoryProperties2 where
        type FieldType "sType" VkPhysicalDeviceMemoryProperties2 =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMemoryProperties2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceMemoryProperties2 =
             #{offset VkPhysicalDeviceMemoryProperties2, sType}
        type FieldIsArray "sType" VkPhysicalDeviceMemoryProperties2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMemoryProperties2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceMemoryProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceMemoryProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMemoryProperties2 where
        type FieldType "pNext" VkPhysicalDeviceMemoryProperties2 = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMemoryProperties2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceMemoryProperties2 =
             #{offset VkPhysicalDeviceMemoryProperties2, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceMemoryProperties2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMemoryProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceMemoryProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceMemoryProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties2, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryProperties" VkPhysicalDeviceMemoryProperties2 where
        type FieldType "memoryProperties" VkPhysicalDeviceMemoryProperties2
             = VkPhysicalDeviceMemoryProperties
        type FieldOptional "memoryProperties"
               VkPhysicalDeviceMemoryProperties2
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryProperties"
               VkPhysicalDeviceMemoryProperties2
             =
             #{offset VkPhysicalDeviceMemoryProperties2, memoryProperties}
        type FieldIsArray "memoryProperties"
               VkPhysicalDeviceMemoryProperties2
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMemoryProperties2, memoryProperties}

instance {-# OVERLAPPING #-}
         CanReadField "memoryProperties" VkPhysicalDeviceMemoryProperties2
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties2, memoryProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties2, memoryProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryProperties" VkPhysicalDeviceMemoryProperties2
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties2, memoryProperties}

instance Show VkPhysicalDeviceMemoryProperties2 where
        showsPrec d x
          = showString "VkPhysicalDeviceMemoryProperties2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryProperties = " .
                            showsPrec d (getField @"memoryProperties" x) . showChar '}'
