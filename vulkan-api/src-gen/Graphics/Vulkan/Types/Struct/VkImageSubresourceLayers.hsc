#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers
       (VkImageSubresourceLayers(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags (VkImageAspectFlags)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkImageSubresourceLayers {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               mipLevel;
--   >     uint32_t               baseArrayLayer;
--   >     uint32_t               layerCount;
--   > } VkImageSubresourceLayers;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageSubresourceLayers.html VkImageSubresourceLayers registry at www.khronos.org>
data VkImageSubresourceLayers = VkImageSubresourceLayers## Addr##
                                                          ByteArray##

instance Eq VkImageSubresourceLayers where
        (VkImageSubresourceLayers## a _) ==
          x@(VkImageSubresourceLayers## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSubresourceLayers where
        (VkImageSubresourceLayers## a _) `compare`
          x@(VkImageSubresourceLayers## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSubresourceLayers where
        sizeOf ~_ = #{size VkImageSubresourceLayers}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageSubresourceLayers}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSubresourceLayers where
        unsafeAddr (VkImageSubresourceLayers## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSubresourceLayers## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSubresourceLayers## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSubresourceLayers where
        type StructFields VkImageSubresourceLayers =
             '["aspectMask", "mipLevel", "baseArrayLayer", "layerCount"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSubresourceLayers = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkAspectMask VkImageSubresourceLayers where
        type VkAspectMaskMType VkImageSubresourceLayers =
             VkImageAspectFlags

        {-# NOINLINE vkAspectMask #-}
        vkAspectMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, aspectMask})

        {-# INLINE vkAspectMaskByteOffset #-}
        vkAspectMaskByteOffset ~_
          = #{offset VkImageSubresourceLayers, aspectMask}

        {-# INLINE readVkAspectMask #-}
        readVkAspectMask p
          = peekByteOff p #{offset VkImageSubresourceLayers, aspectMask}

        {-# INLINE writeVkAspectMask #-}
        writeVkAspectMask p
          = pokeByteOff p #{offset VkImageSubresourceLayers, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkImageSubresourceLayers where
        type FieldType "aspectMask" VkImageSubresourceLayers =
             VkImageAspectFlags
        type FieldOptional "aspectMask" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkImageSubresourceLayers =
             #{offset VkImageSubresourceLayers, aspectMask}
        type FieldIsArray "aspectMask" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceLayers, aspectMask}

instance CanReadField "aspectMask" VkImageSubresourceLayers where
        {-# INLINE getField #-}
        getField = vkAspectMask

        {-# INLINE readField #-}
        readField = readVkAspectMask

instance CanWriteField "aspectMask" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField = writeVkAspectMask

instance {-# OVERLAPPING #-} HasVkMipLevel VkImageSubresourceLayers
         where
        type VkMipLevelMType VkImageSubresourceLayers = Word32

        {-# NOINLINE vkMipLevel #-}
        vkMipLevel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, mipLevel})

        {-# INLINE vkMipLevelByteOffset #-}
        vkMipLevelByteOffset ~_
          = #{offset VkImageSubresourceLayers, mipLevel}

        {-# INLINE readVkMipLevel #-}
        readVkMipLevel p
          = peekByteOff p #{offset VkImageSubresourceLayers, mipLevel}

        {-# INLINE writeVkMipLevel #-}
        writeVkMipLevel p
          = pokeByteOff p #{offset VkImageSubresourceLayers, mipLevel}

instance {-# OVERLAPPING #-}
         HasField "mipLevel" VkImageSubresourceLayers where
        type FieldType "mipLevel" VkImageSubresourceLayers = Word32
        type FieldOptional "mipLevel" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mipLevel" VkImageSubresourceLayers =
             #{offset VkImageSubresourceLayers, mipLevel}
        type FieldIsArray "mipLevel" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceLayers, mipLevel}

instance CanReadField "mipLevel" VkImageSubresourceLayers where
        {-# INLINE getField #-}
        getField = vkMipLevel

        {-# INLINE readField #-}
        readField = readVkMipLevel

instance CanWriteField "mipLevel" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField = writeVkMipLevel

instance {-# OVERLAPPING #-}
         HasVkBaseArrayLayer VkImageSubresourceLayers where
        type VkBaseArrayLayerMType VkImageSubresourceLayers = Word32

        {-# NOINLINE vkBaseArrayLayer #-}
        vkBaseArrayLayer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, baseArrayLayer})

        {-# INLINE vkBaseArrayLayerByteOffset #-}
        vkBaseArrayLayerByteOffset ~_
          = #{offset VkImageSubresourceLayers, baseArrayLayer}

        {-# INLINE readVkBaseArrayLayer #-}
        readVkBaseArrayLayer p
          = peekByteOff p #{offset VkImageSubresourceLayers, baseArrayLayer}

        {-# INLINE writeVkBaseArrayLayer #-}
        writeVkBaseArrayLayer p
          = pokeByteOff p #{offset VkImageSubresourceLayers, baseArrayLayer}

instance {-# OVERLAPPING #-}
         HasField "baseArrayLayer" VkImageSubresourceLayers where
        type FieldType "baseArrayLayer" VkImageSubresourceLayers = Word32
        type FieldOptional "baseArrayLayer" VkImageSubresourceLayers =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "baseArrayLayer" VkImageSubresourceLayers =
             #{offset VkImageSubresourceLayers, baseArrayLayer}
        type FieldIsArray "baseArrayLayer" VkImageSubresourceLayers =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceLayers, baseArrayLayer}

instance CanReadField "baseArrayLayer" VkImageSubresourceLayers
         where
        {-# INLINE getField #-}
        getField = vkBaseArrayLayer

        {-# INLINE readField #-}
        readField = readVkBaseArrayLayer

instance CanWriteField "baseArrayLayer" VkImageSubresourceLayers
         where
        {-# INLINE writeField #-}
        writeField = writeVkBaseArrayLayer

instance {-# OVERLAPPING #-}
         HasVkLayerCount VkImageSubresourceLayers where
        type VkLayerCountMType VkImageSubresourceLayers = Word32

        {-# NOINLINE vkLayerCount #-}
        vkLayerCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, layerCount})

        {-# INLINE vkLayerCountByteOffset #-}
        vkLayerCountByteOffset ~_
          = #{offset VkImageSubresourceLayers, layerCount}

        {-# INLINE readVkLayerCount #-}
        readVkLayerCount p
          = peekByteOff p #{offset VkImageSubresourceLayers, layerCount}

        {-# INLINE writeVkLayerCount #-}
        writeVkLayerCount p
          = pokeByteOff p #{offset VkImageSubresourceLayers, layerCount}

instance {-# OVERLAPPING #-}
         HasField "layerCount" VkImageSubresourceLayers where
        type FieldType "layerCount" VkImageSubresourceLayers = Word32
        type FieldOptional "layerCount" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layerCount" VkImageSubresourceLayers =
             #{offset VkImageSubresourceLayers, layerCount}
        type FieldIsArray "layerCount" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceLayers, layerCount}

instance CanReadField "layerCount" VkImageSubresourceLayers where
        {-# INLINE getField #-}
        getField = vkLayerCount

        {-# INLINE readField #-}
        readField = readVkLayerCount

instance CanWriteField "layerCount" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField = writeVkLayerCount

instance Show VkImageSubresourceLayers where
        showsPrec d x
          = showString "VkImageSubresourceLayers {" .
              showString "vkAspectMask = " .
                showsPrec d (vkAspectMask x) .
                  showString ", " .
                    showString "vkMipLevel = " .
                      showsPrec d (vkMipLevel x) .
                        showString ", " .
                          showString "vkBaseArrayLayer = " .
                            showsPrec d (vkBaseArrayLayer x) .
                              showString ", " .
                                showString "vkLayerCount = " .
                                  showsPrec d (vkLayerCount x) . showChar '}'
