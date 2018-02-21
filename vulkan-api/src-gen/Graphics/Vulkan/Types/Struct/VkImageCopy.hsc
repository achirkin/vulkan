#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageCopy (VkImageCopy(..))
       where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkExtent3D               (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers (VkImageSubresourceLayers)
import           Graphics.Vulkan.Types.Struct.VkOffset3D               (VkOffset3D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkImageCopy {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffset;
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffset;
--   >     VkExtent3D             extent;
--   > } VkImageCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageCopy.html VkImageCopy registry at www.khronos.org>
data VkImageCopy = VkImageCopy## Addr## ByteArray##

instance Eq VkImageCopy where
        (VkImageCopy## a _) == x@(VkImageCopy## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageCopy where
        (VkImageCopy## a _) `compare` x@(VkImageCopy## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageCopy where
        sizeOf ~_ = #{size VkImageCopy}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageCopy}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageCopy where
        unsafeAddr (VkImageCopy## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageCopy## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageCopy## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageCopy where
        type StructFields VkImageCopy =
             '["srcSubresource", "srcOffset", "dstSubresource", "dstOffset", -- ' closing tick for hsc2hs
               "extent"]
        type CUnionType VkImageCopy = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageCopy = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageCopy = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSrcSubresource VkImageCopy where
        type VkSrcSubresourceMType VkImageCopy = VkImageSubresourceLayers

        {-# NOINLINE vkSrcSubresource #-}
        vkSrcSubresource x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCopy, srcSubresource})

        {-# INLINE vkSrcSubresourceByteOffset #-}
        vkSrcSubresourceByteOffset ~_
          = #{offset VkImageCopy, srcSubresource}

        {-# INLINE readVkSrcSubresource #-}
        readVkSrcSubresource p
          = peekByteOff p #{offset VkImageCopy, srcSubresource}

        {-# INLINE writeVkSrcSubresource #-}
        writeVkSrcSubresource p
          = pokeByteOff p #{offset VkImageCopy, srcSubresource}

instance {-# OVERLAPPING #-} HasField "srcSubresource" VkImageCopy
         where
        type FieldType "srcSubresource" VkImageCopy =
             VkImageSubresourceLayers
        type FieldOptional "srcSubresource" VkImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcSubresource" VkImageCopy =
             #{offset VkImageCopy, srcSubresource}
        type FieldIsArray "srcSubresource" VkImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCopy, srcSubresource}

instance CanReadField "srcSubresource" VkImageCopy where
        {-# INLINE getField #-}
        getField = vkSrcSubresource

        {-# INLINE readField #-}
        readField = readVkSrcSubresource

instance CanWriteField "srcSubresource" VkImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkSrcSubresource

instance {-# OVERLAPPING #-} HasVkSrcOffset VkImageCopy where
        type VkSrcOffsetMType VkImageCopy = VkOffset3D

        {-# NOINLINE vkSrcOffset #-}
        vkSrcOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCopy, srcOffset})

        {-# INLINE vkSrcOffsetByteOffset #-}
        vkSrcOffsetByteOffset ~_
          = #{offset VkImageCopy, srcOffset}

        {-# INLINE readVkSrcOffset #-}
        readVkSrcOffset p
          = peekByteOff p #{offset VkImageCopy, srcOffset}

        {-# INLINE writeVkSrcOffset #-}
        writeVkSrcOffset p
          = pokeByteOff p #{offset VkImageCopy, srcOffset}

instance {-# OVERLAPPING #-} HasField "srcOffset" VkImageCopy where
        type FieldType "srcOffset" VkImageCopy = VkOffset3D
        type FieldOptional "srcOffset" VkImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcOffset" VkImageCopy =
             #{offset VkImageCopy, srcOffset}
        type FieldIsArray "srcOffset" VkImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCopy, srcOffset}

instance CanReadField "srcOffset" VkImageCopy where
        {-# INLINE getField #-}
        getField = vkSrcOffset

        {-# INLINE readField #-}
        readField = readVkSrcOffset

instance CanWriteField "srcOffset" VkImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkSrcOffset

instance {-# OVERLAPPING #-} HasVkDstSubresource VkImageCopy where
        type VkDstSubresourceMType VkImageCopy = VkImageSubresourceLayers

        {-# NOINLINE vkDstSubresource #-}
        vkDstSubresource x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCopy, dstSubresource})

        {-# INLINE vkDstSubresourceByteOffset #-}
        vkDstSubresourceByteOffset ~_
          = #{offset VkImageCopy, dstSubresource}

        {-# INLINE readVkDstSubresource #-}
        readVkDstSubresource p
          = peekByteOff p #{offset VkImageCopy, dstSubresource}

        {-# INLINE writeVkDstSubresource #-}
        writeVkDstSubresource p
          = pokeByteOff p #{offset VkImageCopy, dstSubresource}

instance {-# OVERLAPPING #-} HasField "dstSubresource" VkImageCopy
         where
        type FieldType "dstSubresource" VkImageCopy =
             VkImageSubresourceLayers
        type FieldOptional "dstSubresource" VkImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstSubresource" VkImageCopy =
             #{offset VkImageCopy, dstSubresource}
        type FieldIsArray "dstSubresource" VkImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCopy, dstSubresource}

instance CanReadField "dstSubresource" VkImageCopy where
        {-# INLINE getField #-}
        getField = vkDstSubresource

        {-# INLINE readField #-}
        readField = readVkDstSubresource

instance CanWriteField "dstSubresource" VkImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkDstSubresource

instance {-# OVERLAPPING #-} HasVkDstOffset VkImageCopy where
        type VkDstOffsetMType VkImageCopy = VkOffset3D

        {-# NOINLINE vkDstOffset #-}
        vkDstOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCopy, dstOffset})

        {-# INLINE vkDstOffsetByteOffset #-}
        vkDstOffsetByteOffset ~_
          = #{offset VkImageCopy, dstOffset}

        {-# INLINE readVkDstOffset #-}
        readVkDstOffset p
          = peekByteOff p #{offset VkImageCopy, dstOffset}

        {-# INLINE writeVkDstOffset #-}
        writeVkDstOffset p
          = pokeByteOff p #{offset VkImageCopy, dstOffset}

instance {-# OVERLAPPING #-} HasField "dstOffset" VkImageCopy where
        type FieldType "dstOffset" VkImageCopy = VkOffset3D
        type FieldOptional "dstOffset" VkImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstOffset" VkImageCopy =
             #{offset VkImageCopy, dstOffset}
        type FieldIsArray "dstOffset" VkImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCopy, dstOffset}

instance CanReadField "dstOffset" VkImageCopy where
        {-# INLINE getField #-}
        getField = vkDstOffset

        {-# INLINE readField #-}
        readField = readVkDstOffset

instance CanWriteField "dstOffset" VkImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkDstOffset

instance {-# OVERLAPPING #-} HasVkExtent VkImageCopy where
        type VkExtentMType VkImageCopy = VkExtent3D

        {-# NOINLINE vkExtent #-}
        vkExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCopy, extent})

        {-# INLINE vkExtentByteOffset #-}
        vkExtentByteOffset ~_ = #{offset VkImageCopy, extent}

        {-# INLINE readVkExtent #-}
        readVkExtent p
          = peekByteOff p #{offset VkImageCopy, extent}

        {-# INLINE writeVkExtent #-}
        writeVkExtent p
          = pokeByteOff p #{offset VkImageCopy, extent}

instance {-# OVERLAPPING #-} HasField "extent" VkImageCopy where
        type FieldType "extent" VkImageCopy = VkExtent3D
        type FieldOptional "extent" VkImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkImageCopy =
             #{offset VkImageCopy, extent}
        type FieldIsArray "extent" VkImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCopy, extent}

instance CanReadField "extent" VkImageCopy where
        {-# INLINE getField #-}
        getField = vkExtent

        {-# INLINE readField #-}
        readField = readVkExtent

instance CanWriteField "extent" VkImageCopy where
        {-# INLINE writeField #-}
        writeField = writeVkExtent

instance Show VkImageCopy where
        showsPrec d x
          = showString "VkImageCopy {" .
              showString "vkSrcSubresource = " .
                showsPrec d (vkSrcSubresource x) .
                  showString ", " .
                    showString "vkSrcOffset = " .
                      showsPrec d (vkSrcOffset x) .
                        showString ", " .
                          showString "vkDstSubresource = " .
                            showsPrec d (vkDstSubresource x) .
                              showString ", " .
                                showString "vkDstOffset = " .
                                  showsPrec d (vkDstOffset x) .
                                    showString ", " .
                                      showString "vkExtent = " .
                                        showsPrec d (vkExtent x) . showChar '}'
