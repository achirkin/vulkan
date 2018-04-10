#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo
       (VkBindImageMemoryInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkDeviceMemory,
                                                             VkImage)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBindImageMemoryInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage                          image;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindImageMemoryInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkBindImageMemoryInfo VkBindImageMemoryInfo registry at www.khronos.org>
data VkBindImageMemoryInfo = VkBindImageMemoryInfo## Addr##
                                                    ByteArray##

instance Eq VkBindImageMemoryInfo where
        (VkBindImageMemoryInfo## a _) == x@(VkBindImageMemoryInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemoryInfo where
        (VkBindImageMemoryInfo## a _) `compare`
          x@(VkBindImageMemoryInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemoryInfo where
        sizeOf ~_ = #{size VkBindImageMemoryInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindImageMemoryInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImageMemoryInfo where
        unsafeAddr (VkBindImageMemoryInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImageMemoryInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImageMemoryInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImageMemoryInfo where
        type StructFields VkBindImageMemoryInfo =
             '["sType", "pNext", "image", "memory", "memoryOffset"] -- ' closing tick for hsc2hs
        type CUnionType VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImageMemoryInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkBindImageMemoryInfo
         where
        type FieldType "sType" VkBindImageMemoryInfo = VkStructureType
        type FieldOptional "sType" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImageMemoryInfo =
             #{offset VkBindImageMemoryInfo, sType}
        type FieldIsArray "sType" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImageMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImageMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkBindImageMemoryInfo
         where
        type FieldType "pNext" VkBindImageMemoryInfo = Ptr Void
        type FieldOptional "pNext" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImageMemoryInfo =
             #{offset VkBindImageMemoryInfo, pNext}
        type FieldIsArray "pNext" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImageMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImageMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfo, pNext}

instance {-# OVERLAPPING #-} HasField "image" VkBindImageMemoryInfo
         where
        type FieldType "image" VkBindImageMemoryInfo = VkImage
        type FieldOptional "image" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkBindImageMemoryInfo =
             #{offset VkBindImageMemoryInfo, image}
        type FieldIsArray "image" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfo, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkBindImageMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfo, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfo, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkBindImageMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfo, image}

instance {-# OVERLAPPING #-}
         HasField "memory" VkBindImageMemoryInfo where
        type FieldType "memory" VkBindImageMemoryInfo = VkDeviceMemory
        type FieldOptional "memory" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkBindImageMemoryInfo =
             #{offset VkBindImageMemoryInfo, memory}
        type FieldIsArray "memory" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkBindImageMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfo, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkBindImageMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkBindImageMemoryInfo where
        type FieldType "memoryOffset" VkBindImageMemoryInfo = VkDeviceSize
        type FieldOptional "memoryOffset" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkBindImageMemoryInfo =
             #{offset VkBindImageMemoryInfo, memoryOffset}
        type FieldIsArray "memoryOffset" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryInfo, memoryOffset}

instance {-# OVERLAPPING #-}
         CanReadField "memoryOffset" VkBindImageMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfo, memoryOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfo, memoryOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryOffset" VkBindImageMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfo, memoryOffset}

instance Show VkBindImageMemoryInfo where
        showsPrec d x
          = showString "VkBindImageMemoryInfo {" .
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
