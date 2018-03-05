#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "subresource" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, subresource})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, subresource}

instance {-# OVERLAPPING #-}
         CanWriteField "subresource" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, subresource}

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

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, offset}

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

instance {-# OVERLAPPING #-}
         CanReadField "extent" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, extent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, extent}

instance {-# OVERLAPPING #-}
         CanWriteField "extent" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, extent}

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

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, memory}

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

instance {-# OVERLAPPING #-}
         CanReadField "memoryOffset" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, memoryOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryOffset" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, memoryOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, flags}

instance Show VkSparseImageMemoryBind where
        showsPrec d x
          = showString "VkSparseImageMemoryBind {" .
              showString "subresource = " .
                showsPrec d (getField @"subresource" x) .
                  showString ", " .
                    showString "offset = " .
                      showsPrec d (getField @"offset" x) .
                        showString ", " .
                          showString "extent = " .
                            showsPrec d (getField @"extent" x) .
                              showString ", " .
                                showString "memory = " .
                                  showsPrec d (getField @"memory" x) .
                                    showString ", " .
                                      showString "memoryOffset = " .
                                        showsPrec d (getField @"memoryOffset" x) .
                                          showString ", " .
                                            showString "flags = " .
                                              showsPrec d (getField @"flags" x) . showChar '}'
