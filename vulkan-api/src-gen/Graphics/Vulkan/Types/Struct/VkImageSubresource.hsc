#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSubresource
       (VkImageSubresource(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags (VkImageAspectFlags)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkImageSubresource {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               mipLevel;
--   >     uint32_t               arrayLayer;
--   > } VkImageSubresource;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageSubresource.html VkImageSubresource registry at www.khronos.org>
data VkImageSubresource = VkImageSubresource## Addr## ByteArray##

instance Eq VkImageSubresource where
        (VkImageSubresource## a _) == x@(VkImageSubresource## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSubresource where
        (VkImageSubresource## a _) `compare` x@(VkImageSubresource## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSubresource where
        sizeOf ~_ = #{size VkImageSubresource}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageSubresource}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSubresource where
        unsafeAddr (VkImageSubresource## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSubresource## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSubresource## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSubresource where
        type StructFields VkImageSubresource =
             '["aspectMask", "mipLevel", "arrayLayer"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSubresource = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkAspectMask VkImageSubresource
         where
        type VkAspectMaskMType VkImageSubresource = VkImageAspectFlags

        {-# NOINLINE vkAspectMask #-}
        vkAspectMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresource, aspectMask})

        {-# INLINE vkAspectMaskByteOffset #-}
        vkAspectMaskByteOffset ~_
          = #{offset VkImageSubresource, aspectMask}

        {-# INLINE readVkAspectMask #-}
        readVkAspectMask p
          = peekByteOff p #{offset VkImageSubresource, aspectMask}

        {-# INLINE writeVkAspectMask #-}
        writeVkAspectMask p
          = pokeByteOff p #{offset VkImageSubresource, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkImageSubresource where
        type FieldType "aspectMask" VkImageSubresource = VkImageAspectFlags
        type FieldOptional "aspectMask" VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkImageSubresource =
             #{offset VkImageSubresource, aspectMask}
        type FieldIsArray "aspectMask" VkImageSubresource = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageSubresource, aspectMask}

instance CanReadField "aspectMask" VkImageSubresource where
        {-# INLINE getField #-}
        getField = vkAspectMask

        {-# INLINE readField #-}
        readField = readVkAspectMask

instance CanWriteField "aspectMask" VkImageSubresource where
        {-# INLINE writeField #-}
        writeField = writeVkAspectMask

instance {-# OVERLAPPING #-} HasVkMipLevel VkImageSubresource where
        type VkMipLevelMType VkImageSubresource = Word32

        {-# NOINLINE vkMipLevel #-}
        vkMipLevel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresource, mipLevel})

        {-# INLINE vkMipLevelByteOffset #-}
        vkMipLevelByteOffset ~_
          = #{offset VkImageSubresource, mipLevel}

        {-# INLINE readVkMipLevel #-}
        readVkMipLevel p
          = peekByteOff p #{offset VkImageSubresource, mipLevel}

        {-# INLINE writeVkMipLevel #-}
        writeVkMipLevel p
          = pokeByteOff p #{offset VkImageSubresource, mipLevel}

instance {-# OVERLAPPING #-} HasField "mipLevel" VkImageSubresource
         where
        type FieldType "mipLevel" VkImageSubresource = Word32
        type FieldOptional "mipLevel" VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mipLevel" VkImageSubresource =
             #{offset VkImageSubresource, mipLevel}
        type FieldIsArray "mipLevel" VkImageSubresource = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageSubresource, mipLevel}

instance CanReadField "mipLevel" VkImageSubresource where
        {-# INLINE getField #-}
        getField = vkMipLevel

        {-# INLINE readField #-}
        readField = readVkMipLevel

instance CanWriteField "mipLevel" VkImageSubresource where
        {-# INLINE writeField #-}
        writeField = writeVkMipLevel

instance {-# OVERLAPPING #-} HasVkArrayLayer VkImageSubresource
         where
        type VkArrayLayerMType VkImageSubresource = Word32

        {-# NOINLINE vkArrayLayer #-}
        vkArrayLayer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresource, arrayLayer})

        {-# INLINE vkArrayLayerByteOffset #-}
        vkArrayLayerByteOffset ~_
          = #{offset VkImageSubresource, arrayLayer}

        {-# INLINE readVkArrayLayer #-}
        readVkArrayLayer p
          = peekByteOff p #{offset VkImageSubresource, arrayLayer}

        {-# INLINE writeVkArrayLayer #-}
        writeVkArrayLayer p
          = pokeByteOff p #{offset VkImageSubresource, arrayLayer}

instance {-# OVERLAPPING #-}
         HasField "arrayLayer" VkImageSubresource where
        type FieldType "arrayLayer" VkImageSubresource = Word32
        type FieldOptional "arrayLayer" VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type FieldOffset "arrayLayer" VkImageSubresource =
             #{offset VkImageSubresource, arrayLayer}
        type FieldIsArray "arrayLayer" VkImageSubresource = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageSubresource, arrayLayer}

instance CanReadField "arrayLayer" VkImageSubresource where
        {-# INLINE getField #-}
        getField = vkArrayLayer

        {-# INLINE readField #-}
        readField = readVkArrayLayer

instance CanWriteField "arrayLayer" VkImageSubresource where
        {-# INLINE writeField #-}
        writeField = writeVkArrayLayer

instance Show VkImageSubresource where
        showsPrec d x
          = showString "VkImageSubresource {" .
              showString "vkAspectMask = " .
                showsPrec d (vkAspectMask x) .
                  showString ", " .
                    showString "vkMipLevel = " .
                      showsPrec d (vkMipLevel x) .
                        showString ", " .
                          showString "vkArrayLayer = " .
                            showsPrec d (vkArrayLayer x) . showChar '}'
