#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkSparseMemoryBind {
--   >     VkDeviceSize           resourceOffset;
--   >     VkDeviceSize           size;
--   >     VkDeviceMemory         memory;
--   >     VkDeviceSize           memoryOffset;
--   >     VkSparseMemoryBindFlagsflags;
--   > } VkSparseMemoryBind;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSparseMemoryBind.html VkSparseMemoryBind registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkResourceOffset VkSparseMemoryBind
         where
        type VkResourceOffsetMType VkSparseMemoryBind = VkDeviceSize

        {-# NOINLINE vkResourceOffset #-}
        vkResourceOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, resourceOffset})

        {-# INLINE vkResourceOffsetByteOffset #-}
        vkResourceOffsetByteOffset ~_
          = #{offset VkSparseMemoryBind, resourceOffset}

        {-# INLINE readVkResourceOffset #-}
        readVkResourceOffset p
          = peekByteOff p #{offset VkSparseMemoryBind, resourceOffset}

        {-# INLINE writeVkResourceOffset #-}
        writeVkResourceOffset p
          = pokeByteOff p #{offset VkSparseMemoryBind, resourceOffset}

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

instance CanReadField "resourceOffset" VkSparseMemoryBind where
        {-# INLINE getField #-}
        getField = vkResourceOffset

        {-# INLINE readField #-}
        readField = readVkResourceOffset

instance CanWriteField "resourceOffset" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkResourceOffset

instance {-# OVERLAPPING #-} HasVkSize VkSparseMemoryBind where
        type VkSizeMType VkSparseMemoryBind = VkDeviceSize

        {-# NOINLINE vkSize #-}
        vkSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, size})

        {-# INLINE vkSizeByteOffset #-}
        vkSizeByteOffset ~_
          = #{offset VkSparseMemoryBind, size}

        {-# INLINE readVkSize #-}
        readVkSize p
          = peekByteOff p #{offset VkSparseMemoryBind, size}

        {-# INLINE writeVkSize #-}
        writeVkSize p
          = pokeByteOff p #{offset VkSparseMemoryBind, size}

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

instance CanReadField "size" VkSparseMemoryBind where
        {-# INLINE getField #-}
        getField = vkSize

        {-# INLINE readField #-}
        readField = readVkSize

instance CanWriteField "size" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkSize

instance {-# OVERLAPPING #-} HasVkMemory VkSparseMemoryBind where
        type VkMemoryMType VkSparseMemoryBind = VkDeviceMemory

        {-# NOINLINE vkMemory #-}
        vkMemory x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, memory})

        {-# INLINE vkMemoryByteOffset #-}
        vkMemoryByteOffset ~_
          = #{offset VkSparseMemoryBind, memory}

        {-# INLINE readVkMemory #-}
        readVkMemory p
          = peekByteOff p #{offset VkSparseMemoryBind, memory}

        {-# INLINE writeVkMemory #-}
        writeVkMemory p
          = pokeByteOff p #{offset VkSparseMemoryBind, memory}

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

instance CanReadField "memory" VkSparseMemoryBind where
        {-# INLINE getField #-}
        getField = vkMemory

        {-# INLINE readField #-}
        readField = readVkMemory

instance CanWriteField "memory" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkMemory

instance {-# OVERLAPPING #-} HasVkMemoryOffset VkSparseMemoryBind
         where
        type VkMemoryOffsetMType VkSparseMemoryBind = VkDeviceSize

        {-# NOINLINE vkMemoryOffset #-}
        vkMemoryOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, memoryOffset})

        {-# INLINE vkMemoryOffsetByteOffset #-}
        vkMemoryOffsetByteOffset ~_
          = #{offset VkSparseMemoryBind, memoryOffset}

        {-# INLINE readVkMemoryOffset #-}
        readVkMemoryOffset p
          = peekByteOff p #{offset VkSparseMemoryBind, memoryOffset}

        {-# INLINE writeVkMemoryOffset #-}
        writeVkMemoryOffset p
          = pokeByteOff p #{offset VkSparseMemoryBind, memoryOffset}

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

instance CanReadField "memoryOffset" VkSparseMemoryBind where
        {-# INLINE getField #-}
        getField = vkMemoryOffset

        {-# INLINE readField #-}
        readField = readVkMemoryOffset

instance CanWriteField "memoryOffset" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkMemoryOffset

instance {-# OVERLAPPING #-} HasVkFlags VkSparseMemoryBind where
        type VkFlagsMType VkSparseMemoryBind = VkSparseMemoryBindFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkSparseMemoryBind, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkSparseMemoryBind, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkSparseMemoryBind, flags}

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

instance CanReadField "flags" VkSparseMemoryBind where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance Show VkSparseMemoryBind where
        showsPrec d x
          = showString "VkSparseMemoryBind {" .
              showString "vkResourceOffset = " .
                showsPrec d (vkResourceOffset x) .
                  showString ", " .
                    showString "vkSize = " .
                      showsPrec d (vkSize x) .
                        showString ", " .
                          showString "vkMemory = " .
                            showsPrec d (vkMemory x) .
                              showString ", " .
                                showString "vkMemoryOffset = " .
                                  showsPrec d (vkMemoryOffset x) .
                                    showString ", " .
                                      showString "vkFlags = " .
                                        showsPrec d (vkFlags x) . showChar '}'
