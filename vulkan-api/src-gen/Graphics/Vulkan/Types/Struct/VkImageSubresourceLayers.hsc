#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers
       (VkImageSubresourceLayers(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags (VkImageAspectFlags)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkImageSubresourceLayers {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               mipLevel;
--   >     uint32_t               baseArrayLayer;
--   >     uint32_t               layerCount;
--   > } VkImageSubresourceLayers;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageSubresourceLayers VkImageSubresourceLayers registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkImageSubresourceLayers where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceLayers, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceLayers, aspectMask}

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

instance {-# OVERLAPPING #-}
         CanReadField "mipLevel" VkImageSubresourceLayers where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, mipLevel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceLayers, mipLevel}

instance {-# OVERLAPPING #-}
         CanWriteField "mipLevel" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceLayers, mipLevel}

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

instance {-# OVERLAPPING #-}
         CanReadField "baseArrayLayer" VkImageSubresourceLayers where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, baseArrayLayer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceLayers, baseArrayLayer}

instance {-# OVERLAPPING #-}
         CanWriteField "baseArrayLayer" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceLayers, baseArrayLayer}

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

instance {-# OVERLAPPING #-}
         CanReadField "layerCount" VkImageSubresourceLayers where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, layerCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceLayers, layerCount}

instance {-# OVERLAPPING #-}
         CanWriteField "layerCount" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceLayers, layerCount}

instance Show VkImageSubresourceLayers where
        showsPrec d x
          = showString "VkImageSubresourceLayers {" .
              showString "aspectMask = " .
                showsPrec d (getField @"aspectMask" x) .
                  showString ", " .
                    showString "mipLevel = " .
                      showsPrec d (getField @"mipLevel" x) .
                        showString ", " .
                          showString "baseArrayLayer = " .
                            showsPrec d (getField @"baseArrayLayer" x) .
                              showString ", " .
                                showString "layerCount = " .
                                  showsPrec d (getField @"layerCount" x) . showChar '}'
