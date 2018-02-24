#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryHeap (VkMemoryHeap(..))
       where
import           Foreign.Storable                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes              (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags (VkMemoryHeapFlags)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                             (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryHeap {
--   >     VkDeviceSize           size;
--   >     VkMemoryHeapFlags      flags;
--   > } VkMemoryHeap;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryHeap.html VkMemoryHeap registry at www.khronos.org>
data VkMemoryHeap = VkMemoryHeap## Addr## ByteArray##

instance Eq VkMemoryHeap where
        (VkMemoryHeap## a _) == x@(VkMemoryHeap## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryHeap where
        (VkMemoryHeap## a _) `compare` x@(VkMemoryHeap## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryHeap where
        sizeOf ~_ = #{size VkMemoryHeap}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryHeap}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryHeap where
        unsafeAddr (VkMemoryHeap## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryHeap## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryHeap## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryHeap where
        type StructFields VkMemoryHeap = '["size", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryHeap = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryHeap = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryHeap = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSize VkMemoryHeap where
        type VkSizeMType VkMemoryHeap = VkDeviceSize

        {-# NOINLINE vkSize #-}
        vkSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHeap, size})

        {-# INLINE vkSizeByteOffset #-}
        vkSizeByteOffset ~_ = #{offset VkMemoryHeap, size}

        {-# INLINE readVkSize #-}
        readVkSize p
          = peekByteOff p #{offset VkMemoryHeap, size}

        {-# INLINE writeVkSize #-}
        writeVkSize p
          = pokeByteOff p #{offset VkMemoryHeap, size}

instance {-# OVERLAPPING #-} HasField "size" VkMemoryHeap where
        type FieldType "size" VkMemoryHeap = VkDeviceSize
        type FieldOptional "size" VkMemoryHeap = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkMemoryHeap =
             #{offset VkMemoryHeap, size}
        type FieldIsArray "size" VkMemoryHeap = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryHeap, size}

instance CanReadField "size" VkMemoryHeap where
        {-# INLINE getField #-}
        getField = vkSize

        {-# INLINE readField #-}
        readField = readVkSize

instance CanWriteField "size" VkMemoryHeap where
        {-# INLINE writeField #-}
        writeField = writeVkSize

instance {-# OVERLAPPING #-} HasVkFlags VkMemoryHeap where
        type VkFlagsMType VkMemoryHeap = VkMemoryHeapFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHeap, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_ = #{offset VkMemoryHeap, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkMemoryHeap, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkMemoryHeap, flags}

instance {-# OVERLAPPING #-} HasField "flags" VkMemoryHeap where
        type FieldType "flags" VkMemoryHeap = VkMemoryHeapFlags
        type FieldOptional "flags" VkMemoryHeap = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMemoryHeap =
             #{offset VkMemoryHeap, flags}
        type FieldIsArray "flags" VkMemoryHeap = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryHeap, flags}

instance CanReadField "flags" VkMemoryHeap where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkMemoryHeap where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance Show VkMemoryHeap where
        showsPrec d x
          = showString "VkMemoryHeap {" .
              showString "vkSize = " .
                showsPrec d (vkSize x) .
                  showString ", " .
                    showString "vkFlags = " . showsPrec d (vkFlags x) . showChar '}'
