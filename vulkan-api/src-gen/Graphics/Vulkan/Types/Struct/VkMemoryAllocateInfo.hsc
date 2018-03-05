#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo
       (VkMemoryAllocateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDeviceSize           allocationSize;
--   >     uint32_t               memoryTypeIndex;
--   > } VkMemoryAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryAllocateInfo.html VkMemoryAllocateInfo registry at www.khronos.org>
data VkMemoryAllocateInfo = VkMemoryAllocateInfo## Addr## ByteArray##

instance Eq VkMemoryAllocateInfo where
        (VkMemoryAllocateInfo## a _) == x@(VkMemoryAllocateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryAllocateInfo where
        (VkMemoryAllocateInfo## a _) `compare` x@(VkMemoryAllocateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryAllocateInfo where
        sizeOf ~_ = #{size VkMemoryAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryAllocateInfo where
        unsafeAddr (VkMemoryAllocateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryAllocateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryAllocateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryAllocateInfo where
        type StructFields VkMemoryAllocateInfo =
             '["sType", "pNext", "allocationSize", "memoryTypeIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryAllocateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkMemoryAllocateInfo
         where
        type FieldType "sType" VkMemoryAllocateInfo = VkStructureType
        type FieldOptional "sType" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, sType}
        type FieldIsArray "sType" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkMemoryAllocateInfo
         where
        type FieldType "pNext" VkMemoryAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, pNext}
        type FieldIsArray "pNext" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "allocationSize" VkMemoryAllocateInfo where
        type FieldType "allocationSize" VkMemoryAllocateInfo = VkDeviceSize
        type FieldOptional "allocationSize" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "allocationSize" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, allocationSize}
        type FieldIsArray "allocationSize" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateInfo, allocationSize}

instance {-# OVERLAPPING #-}
         CanReadField "allocationSize" VkMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, allocationSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateInfo, allocationSize}

instance {-# OVERLAPPING #-}
         CanWriteField "allocationSize" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, allocationSize}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeIndex" VkMemoryAllocateInfo where
        type FieldType "memoryTypeIndex" VkMemoryAllocateInfo = Word32
        type FieldOptional "memoryTypeIndex" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeIndex" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, memoryTypeIndex}
        type FieldIsArray "memoryTypeIndex" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateInfo, memoryTypeIndex}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeIndex" VkMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, memoryTypeIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateInfo, memoryTypeIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeIndex" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, memoryTypeIndex}

instance Show VkMemoryAllocateInfo where
        showsPrec d x
          = showString "VkMemoryAllocateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "allocationSize = " .
                            showsPrec d (getField @"allocationSize" x) .
                              showString ", " .
                                showString "memoryTypeIndex = " .
                                  showsPrec d (getField @"memoryTypeIndex" x) . showChar '}'
