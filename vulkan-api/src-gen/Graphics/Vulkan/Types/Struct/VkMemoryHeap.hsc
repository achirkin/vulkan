#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryHeap (VkMemoryHeap(..))
       where
import           Foreign.Storable                             (Storable (..))
import           GHC.Base                                     (Addr##,
                                                               ByteArray##,
                                                               byteArrayContents##,
                                                               plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes              (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags (VkMemoryHeapFlags)
import           System.IO.Unsafe                             (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryHeap {
--   >     VkDeviceSize           size;
--   >     VkMemoryHeapFlags      flags;
--   > } VkMemoryHeap;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryHeap VkMemoryHeap registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} CanReadField "size" VkMemoryHeap where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHeap, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryHeap, size}

instance {-# OVERLAPPING #-} CanWriteField "size" VkMemoryHeap
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryHeap, size}

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

instance {-# OVERLAPPING #-} CanReadField "flags" VkMemoryHeap
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHeap, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryHeap, flags}

instance {-# OVERLAPPING #-} CanWriteField "flags" VkMemoryHeap
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryHeap, flags}

instance Show VkMemoryHeap where
        showsPrec d x
          = showString "VkMemoryHeap {" .
              showString "size = " .
                showsPrec d (getField @"size" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) . showChar '}'
