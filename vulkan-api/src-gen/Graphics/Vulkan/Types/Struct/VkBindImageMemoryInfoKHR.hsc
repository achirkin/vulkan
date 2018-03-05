#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR
       (VkBindImageMemoryInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkDeviceMemory,
                                                             VkImage)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBindImageMemoryInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage                          image;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindImageMemoryInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBindImageMemoryInfoKHR.html VkBindImageMemoryInfoKHR registry at www.khronos.org>
data VkBindImageMemoryInfoKHR = VkBindImageMemoryInfoKHR## Addr##
                                                          ByteArray##

instance Eq VkBindImageMemoryInfoKHR where
        (VkBindImageMemoryInfoKHR## a _) ==
          x@(VkBindImageMemoryInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemoryInfoKHR where
        (VkBindImageMemoryInfoKHR## a _) `compare`
          x@(VkBindImageMemoryInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemoryInfoKHR where
        sizeOf ~_ = #{size VkBindImageMemoryInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindImageMemoryInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImageMemoryInfoKHR where
        unsafeAddr (VkBindImageMemoryInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImageMemoryInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImageMemoryInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImageMemoryInfoKHR where
        type StructFields VkBindImageMemoryInfoKHR =
             '["sType", "pNext", "image", "memory", "memoryOffset"] -- ' closing tick for hsc2hs
        type CUnionType VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImageMemoryInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImageMemoryInfoKHR where
        type FieldType "sType" VkBindImageMemoryInfoKHR = VkStructureType
        type FieldOptional "sType" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImageMemoryInfoKHR =
             #{offset VkBindImageMemoryInfoKHR, sType}
        type FieldIsArray "sType" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImageMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImageMemoryInfoKHR where
        type FieldType "pNext" VkBindImageMemoryInfoKHR = Ptr Void
        type FieldOptional "pNext" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImageMemoryInfoKHR =
             #{offset VkBindImageMemoryInfoKHR, pNext}
        type FieldIsArray "pNext" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImageMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "image" VkBindImageMemoryInfoKHR where
        type FieldType "image" VkBindImageMemoryInfoKHR = VkImage
        type FieldOptional "image" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkBindImageMemoryInfoKHR =
             #{offset VkBindImageMemoryInfoKHR, image}
        type FieldIsArray "image" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfoKHR, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkBindImageMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, image}

instance {-# OVERLAPPING #-}
         HasField "memory" VkBindImageMemoryInfoKHR where
        type FieldType "memory" VkBindImageMemoryInfoKHR = VkDeviceMemory
        type FieldOptional "memory" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkBindImageMemoryInfoKHR =
             #{offset VkBindImageMemoryInfoKHR, memory}
        type FieldIsArray "memory" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkBindImageMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, memory}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkBindImageMemoryInfoKHR where
        type FieldType "memoryOffset" VkBindImageMemoryInfoKHR =
             VkDeviceSize
        type FieldOptional "memoryOffset" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkBindImageMemoryInfoKHR =
             #{offset VkBindImageMemoryInfoKHR, memoryOffset}
        type FieldIsArray "memoryOffset" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryInfoKHR, memoryOffset}

instance {-# OVERLAPPING #-}
         CanReadField "memoryOffset" VkBindImageMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, memoryOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, memoryOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryOffset" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, memoryOffset}

instance Show VkBindImageMemoryInfoKHR where
        showsPrec d x
          = showString "VkBindImageMemoryInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "image = " .
                            showsPrec d (getField @"image" x) .
                              showString ", " .
                                showString "memory = " .
                                  showsPrec d (getField @"memory" x) .
                                    showString ", " .
                                      showString "memoryOffset = " .
                                        showsPrec d (getField @"memoryOffset" x) . showChar '}'
