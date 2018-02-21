#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue
       (VkClearDepthStencilValue(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkClearDepthStencilValue {
--   >     float                  depth;
--   >     uint32_t               stencil;
--   > } VkClearDepthStencilValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkClearDepthStencilValue.html VkClearDepthStencilValue registry at www.khronos.org>
data VkClearDepthStencilValue = VkClearDepthStencilValue## Addr##
                                                          ByteArray##

instance Eq VkClearDepthStencilValue where
        (VkClearDepthStencilValue## a _) ==
          x@(VkClearDepthStencilValue## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkClearDepthStencilValue where
        (VkClearDepthStencilValue## a _) `compare`
          x@(VkClearDepthStencilValue## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkClearDepthStencilValue where
        sizeOf ~_ = #{size VkClearDepthStencilValue}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkClearDepthStencilValue}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkClearDepthStencilValue where
        unsafeAddr (VkClearDepthStencilValue## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkClearDepthStencilValue## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkClearDepthStencilValue## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkClearDepthStencilValue where
        type StructFields VkClearDepthStencilValue = '["depth", "stencil"] -- ' closing tick for hsc2hs
        type CUnionType VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs
        type StructExtends VkClearDepthStencilValue = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkDepth VkClearDepthStencilValue
         where
        type VkDepthMType VkClearDepthStencilValue =
             #{type float}

        {-# NOINLINE vkDepth #-}
        vkDepth x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearDepthStencilValue, depth})

        {-# INLINE vkDepthByteOffset #-}
        vkDepthByteOffset ~_
          = #{offset VkClearDepthStencilValue, depth}

        {-# INLINE readVkDepth #-}
        readVkDepth p
          = peekByteOff p #{offset VkClearDepthStencilValue, depth}

        {-# INLINE writeVkDepth #-}
        writeVkDepth p
          = pokeByteOff p #{offset VkClearDepthStencilValue, depth}

instance {-# OVERLAPPING #-}
         HasField "depth" VkClearDepthStencilValue where
        type FieldType "depth" VkClearDepthStencilValue =
             #{type float}
        type FieldOptional "depth" VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depth" VkClearDepthStencilValue =
             #{offset VkClearDepthStencilValue, depth}
        type FieldIsArray "depth" VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearDepthStencilValue, depth}

instance CanReadField "depth" VkClearDepthStencilValue where
        {-# INLINE getField #-}
        getField = vkDepth

        {-# INLINE readField #-}
        readField = readVkDepth

instance CanWriteField "depth" VkClearDepthStencilValue where
        {-# INLINE writeField #-}
        writeField = writeVkDepth

instance {-# OVERLAPPING #-} HasVkStencil VkClearDepthStencilValue
         where
        type VkStencilMType VkClearDepthStencilValue = Word32

        {-# NOINLINE vkStencil #-}
        vkStencil x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearDepthStencilValue, stencil})

        {-# INLINE vkStencilByteOffset #-}
        vkStencilByteOffset ~_
          = #{offset VkClearDepthStencilValue, stencil}

        {-# INLINE readVkStencil #-}
        readVkStencil p
          = peekByteOff p #{offset VkClearDepthStencilValue, stencil}

        {-# INLINE writeVkStencil #-}
        writeVkStencil p
          = pokeByteOff p #{offset VkClearDepthStencilValue, stencil}

instance {-# OVERLAPPING #-}
         HasField "stencil" VkClearDepthStencilValue where
        type FieldType "stencil" VkClearDepthStencilValue = Word32
        type FieldOptional "stencil" VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stencil" VkClearDepthStencilValue =
             #{offset VkClearDepthStencilValue, stencil}
        type FieldIsArray "stencil" VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkClearDepthStencilValue, stencil}

instance CanReadField "stencil" VkClearDepthStencilValue where
        {-# INLINE getField #-}
        getField = vkStencil

        {-# INLINE readField #-}
        readField = readVkStencil

instance CanWriteField "stencil" VkClearDepthStencilValue where
        {-# INLINE writeField #-}
        writeField = writeVkStencil

instance Show VkClearDepthStencilValue where
        showsPrec d x
          = showString "VkClearDepthStencilValue {" .
              showString "vkDepth = " .
                showsPrec d (vkDepth x) .
                  showString ", " .
                    showString "vkStencil = " .
                      showsPrec d (vkStencil x) . showChar '}'
