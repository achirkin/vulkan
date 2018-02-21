#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBind
       (VkSparseImageMemoryBind(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                    (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkSparseMemoryBindFlags (VkSparseMemoryBindFlags)
import           Graphics.Vulkan.Types.Handles                      (VkDeviceMemory)
import           Graphics.Vulkan.Types.Struct.VkExtent3D            (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.VkImageSubresource    (VkImageSubresource)
import           Graphics.Vulkan.Types.Struct.VkOffset3D            (VkOffset3D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageMemoryBind {
--   >     VkImageSubresource     subresource;
--   >     VkOffset3D             offset;
--   >     VkExtent3D             extent;
--   >     VkDeviceMemory         memory;
--   >     VkDeviceSize           memoryOffset;
--   >     VkSparseMemoryBindFlagsflags;
--   > } VkSparseImageMemoryBind;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSparseImageMemoryBind.html VkSparseImageMemoryBind registry at www.khronos.org>
data VkSparseImageMemoryBind = VkSparseImageMemoryBind## Addr##
                                                        ByteArray##

instance Eq VkSparseImageMemoryBind where
        (VkSparseImageMemoryBind## a _) == x@(VkSparseImageMemoryBind## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageMemoryBind where
        (VkSparseImageMemoryBind## a _) `compare`
          x@(VkSparseImageMemoryBind## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageMemoryBind where
        sizeOf ~_ = #{size VkSparseImageMemoryBind}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSparseImageMemoryBind}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageMemoryBind where
        unsafeAddr (VkSparseImageMemoryBind## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageMemoryBind## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageMemoryBind## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageMemoryBind where
        type StructFields VkSparseImageMemoryBind =
             '["subresource", "offset", "extent", "memory", "memoryOffset", -- ' closing tick for hsc2hs
               "flags"]
        type CUnionType VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageMemoryBind = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSubresource VkSparseImageMemoryBind where
        type VkSubresourceMType VkSparseImageMemoryBind =
             VkImageSubresource

        {-# NOINLINE vkSubresource #-}
        vkSubresource x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, subresource})

        {-# INLINE vkSubresourceByteOffset #-}
        vkSubresourceByteOffset ~_
          = #{offset VkSparseImageMemoryBind, subresource}

        {-# INLINE readVkSubresource #-}
        readVkSubresource p
          = peekByteOff p #{offset VkSparseImageMemoryBind, subresource}

        {-# INLINE writeVkSubresource #-}
        writeVkSubresource p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, subresource}

instance {-# OVERLAPPING #-}
         HasField "subresource" VkSparseImageMemoryBind where
        type FieldType "subresource" VkSparseImageMemoryBind =
             VkImageSubresource
        type FieldOptional "subresource" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subresource" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, subresource}
        type FieldIsArray "subresource" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryBind, subresource}

instance CanReadField "subresource" VkSparseImageMemoryBind where
        {-# INLINE getField #-}
        getField = vkSubresource

        {-# INLINE readField #-}
        readField = readVkSubresource

instance CanWriteField "subresource" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkSubresource

instance {-# OVERLAPPING #-} HasVkOffset VkSparseImageMemoryBind
         where
        type VkOffsetMType VkSparseImageMemoryBind = VkOffset3D

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkSparseImageMemoryBind, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkSparseImageMemoryBind, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, offset}

instance {-# OVERLAPPING #-}
         HasField "offset" VkSparseImageMemoryBind where
        type FieldType "offset" VkSparseImageMemoryBind = VkOffset3D
        type FieldOptional "offset" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, offset}
        type FieldIsArray "offset" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseImageMemoryBind, offset}

instance CanReadField "offset" VkSparseImageMemoryBind where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-} HasVkExtent VkSparseImageMemoryBind
         where
        type VkExtentMType VkSparseImageMemoryBind = VkExtent3D

        {-# NOINLINE vkExtent #-}
        vkExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, extent})

        {-# INLINE vkExtentByteOffset #-}
        vkExtentByteOffset ~_
          = #{offset VkSparseImageMemoryBind, extent}

        {-# INLINE readVkExtent #-}
        readVkExtent p
          = peekByteOff p #{offset VkSparseImageMemoryBind, extent}

        {-# INLINE writeVkExtent #-}
        writeVkExtent p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, extent}

instance {-# OVERLAPPING #-}
         HasField "extent" VkSparseImageMemoryBind where
        type FieldType "extent" VkSparseImageMemoryBind = VkExtent3D
        type FieldOptional "extent" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, extent}
        type FieldIsArray "extent" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseImageMemoryBind, extent}

instance CanReadField "extent" VkSparseImageMemoryBind where
        {-# INLINE getField #-}
        getField = vkExtent

        {-# INLINE readField #-}
        readField = readVkExtent

instance CanWriteField "extent" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkExtent

instance {-# OVERLAPPING #-} HasVkMemory VkSparseImageMemoryBind
         where
        type VkMemoryMType VkSparseImageMemoryBind = VkDeviceMemory

        {-# NOINLINE vkMemory #-}
        vkMemory x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, memory})

        {-# INLINE vkMemoryByteOffset #-}
        vkMemoryByteOffset ~_
          = #{offset VkSparseImageMemoryBind, memory}

        {-# INLINE readVkMemory #-}
        readVkMemory p
          = peekByteOff p #{offset VkSparseImageMemoryBind, memory}

        {-# INLINE writeVkMemory #-}
        writeVkMemory p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, memory}

instance {-# OVERLAPPING #-}
         HasField "memory" VkSparseImageMemoryBind where
        type FieldType "memory" VkSparseImageMemoryBind = VkDeviceMemory
        type FieldOptional "memory" VkSparseImageMemoryBind = 'True -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, memory}
        type FieldIsArray "memory" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseImageMemoryBind, memory}

instance CanReadField "memory" VkSparseImageMemoryBind where
        {-# INLINE getField #-}
        getField = vkMemory

        {-# INLINE readField #-}
        readField = readVkMemory

instance CanWriteField "memory" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkMemory

instance {-# OVERLAPPING #-}
         HasVkMemoryOffset VkSparseImageMemoryBind where
        type VkMemoryOffsetMType VkSparseImageMemoryBind = VkDeviceSize

        {-# NOINLINE vkMemoryOffset #-}
        vkMemoryOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, memoryOffset})

        {-# INLINE vkMemoryOffsetByteOffset #-}
        vkMemoryOffsetByteOffset ~_
          = #{offset VkSparseImageMemoryBind, memoryOffset}

        {-# INLINE readVkMemoryOffset #-}
        readVkMemoryOffset p
          = peekByteOff p #{offset VkSparseImageMemoryBind, memoryOffset}

        {-# INLINE writeVkMemoryOffset #-}
        writeVkMemoryOffset p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkSparseImageMemoryBind where
        type FieldType "memoryOffset" VkSparseImageMemoryBind =
             VkDeviceSize
        type FieldOptional "memoryOffset" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, memoryOffset}
        type FieldIsArray "memoryOffset" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryBind, memoryOffset}

instance CanReadField "memoryOffset" VkSparseImageMemoryBind where
        {-# INLINE getField #-}
        getField = vkMemoryOffset

        {-# INLINE readField #-}
        readField = readVkMemoryOffset

instance CanWriteField "memoryOffset" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkMemoryOffset

instance {-# OVERLAPPING #-} HasVkFlags VkSparseImageMemoryBind
         where
        type VkFlagsMType VkSparseImageMemoryBind = VkSparseMemoryBindFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkSparseImageMemoryBind, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkSparseImageMemoryBind, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkSparseImageMemoryBind where
        type FieldType "flags" VkSparseImageMemoryBind =
             VkSparseMemoryBindFlags
        type FieldOptional "flags" VkSparseImageMemoryBind = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, flags}
        type FieldIsArray "flags" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseImageMemoryBind, flags}

instance CanReadField "flags" VkSparseImageMemoryBind where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance Show VkSparseImageMemoryBind where
        showsPrec d x
          = showString "VkSparseImageMemoryBind {" .
              showString "vkSubresource = " .
                showsPrec d (vkSubresource x) .
                  showString ", " .
                    showString "vkOffset = " .
                      showsPrec d (vkOffset x) .
                        showString ", " .
                          showString "vkExtent = " .
                            showsPrec d (vkExtent x) .
                              showString ", " .
                                showString "vkMemory = " .
                                  showsPrec d (vkMemory x) .
                                    showString ", " .
                                      showString "vkMemoryOffset = " .
                                        showsPrec d (vkMemoryOffset x) .
                                          showString ", " .
                                            showString "vkFlags = " .
                                              showsPrec d (vkFlags x) . showChar '}'
