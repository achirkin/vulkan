#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryType (VkMemoryType(..))
       where
import           Foreign.Storable                                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkMemoryPropertyFlags (VkMemoryPropertyFlags)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                 (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryType {
--   >     VkMemoryPropertyFlags  propertyFlags;
--   >     uint32_t               heapIndex;
--   > } VkMemoryType;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryType.html VkMemoryType registry at www.khronos.org>
data VkMemoryType = VkMemoryType## Addr## ByteArray##

instance Eq VkMemoryType where
        (VkMemoryType## a _) == x@(VkMemoryType## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryType where
        (VkMemoryType## a _) `compare` x@(VkMemoryType## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryType where
        sizeOf ~_ = #{size VkMemoryType}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryType}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryType where
        unsafeAddr (VkMemoryType## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryType## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryType## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryType where
        type StructFields VkMemoryType = '["propertyFlags", "heapIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryType = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryType = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryType = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkPropertyFlags VkMemoryType where
        type VkPropertyFlagsMType VkMemoryType = VkMemoryPropertyFlags

        {-# NOINLINE vkPropertyFlags #-}
        vkPropertyFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryType, propertyFlags})

        {-# INLINE vkPropertyFlagsByteOffset #-}
        vkPropertyFlagsByteOffset ~_
          = #{offset VkMemoryType, propertyFlags}

        {-# INLINE readVkPropertyFlags #-}
        readVkPropertyFlags p
          = peekByteOff p #{offset VkMemoryType, propertyFlags}

        {-# INLINE writeVkPropertyFlags #-}
        writeVkPropertyFlags p
          = pokeByteOff p #{offset VkMemoryType, propertyFlags}

instance {-# OVERLAPPING #-} HasField "propertyFlags" VkMemoryType
         where
        type FieldType "propertyFlags" VkMemoryType = VkMemoryPropertyFlags
        type FieldOptional "propertyFlags" VkMemoryType = 'True -- ' closing tick for hsc2hs
        type FieldOffset "propertyFlags" VkMemoryType =
             #{offset VkMemoryType, propertyFlags}
        type FieldIsArray "propertyFlags" VkMemoryType = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryType, propertyFlags}

instance CanReadField "propertyFlags" VkMemoryType where
        {-# INLINE getField #-}
        getField = vkPropertyFlags

        {-# INLINE readField #-}
        readField = readVkPropertyFlags

instance CanWriteField "propertyFlags" VkMemoryType where
        {-# INLINE writeField #-}
        writeField = writeVkPropertyFlags

instance {-# OVERLAPPING #-} HasVkHeapIndex VkMemoryType where
        type VkHeapIndexMType VkMemoryType = Word32

        {-# NOINLINE vkHeapIndex #-}
        vkHeapIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryType, heapIndex})

        {-# INLINE vkHeapIndexByteOffset #-}
        vkHeapIndexByteOffset ~_
          = #{offset VkMemoryType, heapIndex}

        {-# INLINE readVkHeapIndex #-}
        readVkHeapIndex p
          = peekByteOff p #{offset VkMemoryType, heapIndex}

        {-# INLINE writeVkHeapIndex #-}
        writeVkHeapIndex p
          = pokeByteOff p #{offset VkMemoryType, heapIndex}

instance {-# OVERLAPPING #-} HasField "heapIndex" VkMemoryType
         where
        type FieldType "heapIndex" VkMemoryType = Word32
        type FieldOptional "heapIndex" VkMemoryType = 'False -- ' closing tick for hsc2hs
        type FieldOffset "heapIndex" VkMemoryType =
             #{offset VkMemoryType, heapIndex}
        type FieldIsArray "heapIndex" VkMemoryType = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryType, heapIndex}

instance CanReadField "heapIndex" VkMemoryType where
        {-# INLINE getField #-}
        getField = vkHeapIndex

        {-# INLINE readField #-}
        readField = readVkHeapIndex

instance CanWriteField "heapIndex" VkMemoryType where
        {-# INLINE writeField #-}
        writeField = writeVkHeapIndex

instance Show VkMemoryType where
        showsPrec d x
          = showString "VkMemoryType {" .
              showString "vkPropertyFlags = " .
                showsPrec d (vkPropertyFlags x) .
                  showString ", " .
                    showString "vkHeapIndex = " .
                      showsPrec d (vkHeapIndex x) . showChar '}'
