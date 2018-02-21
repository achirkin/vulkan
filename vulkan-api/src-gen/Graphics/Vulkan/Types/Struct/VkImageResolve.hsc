#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageResolve
       (VkImageResolve(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkExtent3D               (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers (VkImageSubresourceLayers)
import           Graphics.Vulkan.Types.Struct.VkOffset3D               (VkOffset3D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkImageResolve {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffset;
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffset;
--   >     VkExtent3D             extent;
--   > } VkImageResolve;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageResolve.html VkImageResolve registry at www.khronos.org>
data VkImageResolve = VkImageResolve## Addr## ByteArray##

instance Eq VkImageResolve where
        (VkImageResolve## a _) == x@(VkImageResolve## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageResolve where
        (VkImageResolve## a _) `compare` x@(VkImageResolve## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageResolve where
        sizeOf ~_ = #{size VkImageResolve}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageResolve}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageResolve where
        unsafeAddr (VkImageResolve## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageResolve## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageResolve## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageResolve where
        type StructFields VkImageResolve =
             '["srcSubresource", "srcOffset", "dstSubresource", "dstOffset", -- ' closing tick for hsc2hs
               "extent"]
        type CUnionType VkImageResolve = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageResolve = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageResolve = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSrcSubresource VkImageResolve
         where
        type VkSrcSubresourceMType VkImageResolve =
             VkImageSubresourceLayers

        {-# NOINLINE vkSrcSubresource #-}
        vkSrcSubresource x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageResolve, srcSubresource})

        {-# INLINE vkSrcSubresourceByteOffset #-}
        vkSrcSubresourceByteOffset ~_
          = #{offset VkImageResolve, srcSubresource}

        {-# INLINE readVkSrcSubresource #-}
        readVkSrcSubresource p
          = peekByteOff p #{offset VkImageResolve, srcSubresource}

        {-# INLINE writeVkSrcSubresource #-}
        writeVkSrcSubresource p
          = pokeByteOff p #{offset VkImageResolve, srcSubresource}

instance {-# OVERLAPPING #-}
         HasField "srcSubresource" VkImageResolve where
        type FieldType "srcSubresource" VkImageResolve =
             VkImageSubresourceLayers
        type FieldOptional "srcSubresource" VkImageResolve = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcSubresource" VkImageResolve =
             #{offset VkImageResolve, srcSubresource}
        type FieldIsArray "srcSubresource" VkImageResolve = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageResolve, srcSubresource}

instance CanReadField "srcSubresource" VkImageResolve where
        {-# INLINE getField #-}
        getField = vkSrcSubresource

        {-# INLINE readField #-}
        readField = readVkSrcSubresource

instance CanWriteField "srcSubresource" VkImageResolve where
        {-# INLINE writeField #-}
        writeField = writeVkSrcSubresource

instance {-# OVERLAPPING #-} HasVkSrcOffset VkImageResolve where
        type VkSrcOffsetMType VkImageResolve = VkOffset3D

        {-# NOINLINE vkSrcOffset #-}
        vkSrcOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageResolve, srcOffset})

        {-# INLINE vkSrcOffsetByteOffset #-}
        vkSrcOffsetByteOffset ~_
          = #{offset VkImageResolve, srcOffset}

        {-# INLINE readVkSrcOffset #-}
        readVkSrcOffset p
          = peekByteOff p #{offset VkImageResolve, srcOffset}

        {-# INLINE writeVkSrcOffset #-}
        writeVkSrcOffset p
          = pokeByteOff p #{offset VkImageResolve, srcOffset}

instance {-# OVERLAPPING #-} HasField "srcOffset" VkImageResolve
         where
        type FieldType "srcOffset" VkImageResolve = VkOffset3D
        type FieldOptional "srcOffset" VkImageResolve = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcOffset" VkImageResolve =
             #{offset VkImageResolve, srcOffset}
        type FieldIsArray "srcOffset" VkImageResolve = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageResolve, srcOffset}

instance CanReadField "srcOffset" VkImageResolve where
        {-# INLINE getField #-}
        getField = vkSrcOffset

        {-# INLINE readField #-}
        readField = readVkSrcOffset

instance CanWriteField "srcOffset" VkImageResolve where
        {-# INLINE writeField #-}
        writeField = writeVkSrcOffset

instance {-# OVERLAPPING #-} HasVkDstSubresource VkImageResolve
         where
        type VkDstSubresourceMType VkImageResolve =
             VkImageSubresourceLayers

        {-# NOINLINE vkDstSubresource #-}
        vkDstSubresource x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageResolve, dstSubresource})

        {-# INLINE vkDstSubresourceByteOffset #-}
        vkDstSubresourceByteOffset ~_
          = #{offset VkImageResolve, dstSubresource}

        {-# INLINE readVkDstSubresource #-}
        readVkDstSubresource p
          = peekByteOff p #{offset VkImageResolve, dstSubresource}

        {-# INLINE writeVkDstSubresource #-}
        writeVkDstSubresource p
          = pokeByteOff p #{offset VkImageResolve, dstSubresource}

instance {-# OVERLAPPING #-}
         HasField "dstSubresource" VkImageResolve where
        type FieldType "dstSubresource" VkImageResolve =
             VkImageSubresourceLayers
        type FieldOptional "dstSubresource" VkImageResolve = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstSubresource" VkImageResolve =
             #{offset VkImageResolve, dstSubresource}
        type FieldIsArray "dstSubresource" VkImageResolve = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageResolve, dstSubresource}

instance CanReadField "dstSubresource" VkImageResolve where
        {-# INLINE getField #-}
        getField = vkDstSubresource

        {-# INLINE readField #-}
        readField = readVkDstSubresource

instance CanWriteField "dstSubresource" VkImageResolve where
        {-# INLINE writeField #-}
        writeField = writeVkDstSubresource

instance {-# OVERLAPPING #-} HasVkDstOffset VkImageResolve where
        type VkDstOffsetMType VkImageResolve = VkOffset3D

        {-# NOINLINE vkDstOffset #-}
        vkDstOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageResolve, dstOffset})

        {-# INLINE vkDstOffsetByteOffset #-}
        vkDstOffsetByteOffset ~_
          = #{offset VkImageResolve, dstOffset}

        {-# INLINE readVkDstOffset #-}
        readVkDstOffset p
          = peekByteOff p #{offset VkImageResolve, dstOffset}

        {-# INLINE writeVkDstOffset #-}
        writeVkDstOffset p
          = pokeByteOff p #{offset VkImageResolve, dstOffset}

instance {-# OVERLAPPING #-} HasField "dstOffset" VkImageResolve
         where
        type FieldType "dstOffset" VkImageResolve = VkOffset3D
        type FieldOptional "dstOffset" VkImageResolve = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstOffset" VkImageResolve =
             #{offset VkImageResolve, dstOffset}
        type FieldIsArray "dstOffset" VkImageResolve = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageResolve, dstOffset}

instance CanReadField "dstOffset" VkImageResolve where
        {-# INLINE getField #-}
        getField = vkDstOffset

        {-# INLINE readField #-}
        readField = readVkDstOffset

instance CanWriteField "dstOffset" VkImageResolve where
        {-# INLINE writeField #-}
        writeField = writeVkDstOffset

instance {-# OVERLAPPING #-} HasVkExtent VkImageResolve where
        type VkExtentMType VkImageResolve = VkExtent3D

        {-# NOINLINE vkExtent #-}
        vkExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageResolve, extent})

        {-# INLINE vkExtentByteOffset #-}
        vkExtentByteOffset ~_
          = #{offset VkImageResolve, extent}

        {-# INLINE readVkExtent #-}
        readVkExtent p
          = peekByteOff p #{offset VkImageResolve, extent}

        {-# INLINE writeVkExtent #-}
        writeVkExtent p
          = pokeByteOff p #{offset VkImageResolve, extent}

instance {-# OVERLAPPING #-} HasField "extent" VkImageResolve where
        type FieldType "extent" VkImageResolve = VkExtent3D
        type FieldOptional "extent" VkImageResolve = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkImageResolve =
             #{offset VkImageResolve, extent}
        type FieldIsArray "extent" VkImageResolve = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageResolve, extent}

instance CanReadField "extent" VkImageResolve where
        {-# INLINE getField #-}
        getField = vkExtent

        {-# INLINE readField #-}
        readField = readVkExtent

instance CanWriteField "extent" VkImageResolve where
        {-# INLINE writeField #-}
        writeField = writeVkExtent

instance Show VkImageResolve where
        showsPrec d x
          = showString "VkImageResolve {" .
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
