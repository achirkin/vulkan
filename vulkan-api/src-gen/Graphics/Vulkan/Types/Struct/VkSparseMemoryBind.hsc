#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseMemoryBind
       (VkSparseMemoryBind(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                    (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkSparseMemoryBindFlags (VkSparseMemoryBindFlags)
import           Graphics.Vulkan.Types.Handles                      (VkDeviceMemory)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkSparseMemoryBind {
--   >     VkDeviceSize           resourceOffset;
--   >     VkDeviceSize           size;
--   >     VkDeviceMemory         memory;
--   >     VkDeviceSize           memoryOffset;
--   >     VkSparseMemoryBindFlagsflags;
--   > } VkSparseMemoryBind;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSparseMemoryBind.html VkSparseMemoryBind registry at www.khronos.org>
data VkSparseMemoryBind = VkSparseMemoryBind## Addr## ByteArray##

instance Eq VkSparseMemoryBind where
        (VkSparseMemoryBind## a _) == x@(VkSparseMemoryBind## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseMemoryBind where
        (VkSparseMemoryBind## a _) `compare` x@(VkSparseMemoryBind## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseMemoryBind where
        sizeOf ~_ = #{size VkSparseMemoryBind}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSparseMemoryBind}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseMemoryBind where
        unsafeAddr (VkSparseMemoryBind## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseMemoryBind## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseMemoryBind## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseMemoryBind where
        type StructFields VkSparseMemoryBind =
             '["resourceOffset", "size", "memory", "memoryOffset", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSparseMemoryBind = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "resourceOffset" VkSparseMemoryBind where
        type FieldType "resourceOffset" VkSparseMemoryBind = VkDeviceSize
        type FieldOptional "resourceOffset" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "resourceOffset" VkSparseMemoryBind =
             #{offset VkSparseMemoryBind, resourceOffset}
        type FieldIsArray "resourceOffset" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseMemoryBind, resourceOffset}

instance {-# OVERLAPPING #-}
         CanReadField "resourceOffset" VkSparseMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, resourceOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseMemoryBind, resourceOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "resourceOffset" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseMemoryBind, resourceOffset}

instance {-# OVERLAPPING #-} HasField "size" VkSparseMemoryBind
         where
        type FieldType "size" VkSparseMemoryBind = VkDeviceSize
        type FieldOptional "size" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkSparseMemoryBind =
             #{offset VkSparseMemoryBind, size}
        type FieldIsArray "size" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseMemoryBind, size}

instance {-# OVERLAPPING #-} CanReadField "size" VkSparseMemoryBind
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseMemoryBind, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseMemoryBind, size}

instance {-# OVERLAPPING #-} HasField "memory" VkSparseMemoryBind
         where
        type FieldType "memory" VkSparseMemoryBind = VkDeviceMemory
        type FieldOptional "memory" VkSparseMemoryBind = 'True -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkSparseMemoryBind =
             #{offset VkSparseMemoryBind, memory}
        type FieldIsArray "memory" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseMemoryBind, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkSparseMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseMemoryBind, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseMemoryBind, memory}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkSparseMemoryBind where
        type FieldType "memoryOffset" VkSparseMemoryBind = VkDeviceSize
        type FieldOptional "memoryOffset" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkSparseMemoryBind =
             #{offset VkSparseMemoryBind, memoryOffset}
        type FieldIsArray "memoryOffset" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-}
         CanReadField "memoryOffset" VkSparseMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, memoryOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryOffset" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-} HasField "flags" VkSparseMemoryBind
         where
        type FieldType "flags" VkSparseMemoryBind = VkSparseMemoryBindFlags
        type FieldOptional "flags" VkSparseMemoryBind = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSparseMemoryBind =
             #{offset VkSparseMemoryBind, flags}
        type FieldIsArray "flags" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseMemoryBind, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkSparseMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseMemoryBind, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseMemoryBind, flags}

instance Show VkSparseMemoryBind where
        showsPrec d x
          = showString "VkSparseMemoryBind {" .
              showString "resourceOffset = " .
                showsPrec d (getField @"resourceOffset" x) .
                  showString ", " .
                    showString "size = " .
                      showsPrec d (getField @"size" x) .
                        showString ", " .
                          showString "memory = " .
                            showsPrec d (getField @"memory" x) .
                              showString ", " .
                                showString "memoryOffset = " .
                                  showsPrec d (getField @"memoryOffset" x) .
                                    showString ", " .
                                      showString "flags = " .
                                        showsPrec d (getField @"flags" x) . showChar '}'
