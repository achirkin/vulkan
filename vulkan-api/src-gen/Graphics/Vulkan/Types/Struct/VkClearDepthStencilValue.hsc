#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue
       (VkClearDepthStencilValue(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

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

instance {-# OVERLAPPING #-}
         CanReadField "depth" VkClearDepthStencilValue where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearDepthStencilValue, depth})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearDepthStencilValue, depth}

instance {-# OVERLAPPING #-}
         CanWriteField "depth" VkClearDepthStencilValue where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearDepthStencilValue, depth}

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

instance {-# OVERLAPPING #-}
         CanReadField "stencil" VkClearDepthStencilValue where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearDepthStencilValue, stencil})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearDepthStencilValue, stencil}

instance {-# OVERLAPPING #-}
         CanWriteField "stencil" VkClearDepthStencilValue where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearDepthStencilValue, stencil}

instance Show VkClearDepthStencilValue where
        showsPrec d x
          = showString "VkClearDepthStencilValue {" .
              showString "depth = " .
                showsPrec d (getField @"depth" x) .
                  showString ", " .
                    showString "stencil = " .
                      showsPrec d (getField @"stencil" x) . showChar '}'
