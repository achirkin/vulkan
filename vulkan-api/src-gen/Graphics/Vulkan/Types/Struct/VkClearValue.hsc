#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkClearValue (VkClearValue(..))
       where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkClearColorValue        (VkClearColorValue)
import           Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue (VkClearDepthStencilValue)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | // Union allowing specification of color or depth and stencil values. Actual value selected is based on attachment being cleared.
--
--   > typedef union VkClearValue {
--   >     VkClearColorValue      color;
--   >     VkClearDepthStencilValue depthStencil;
--   > } VkClearValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkClearValue.html VkClearValue registry at www.khronos.org>
data VkClearValue = VkClearValue## Addr## ByteArray##

instance Eq VkClearValue where
        (VkClearValue## a _) == x@(VkClearValue## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkClearValue where
        (VkClearValue## a _) `compare` x@(VkClearValue## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkClearValue where
        sizeOf ~_ = #{size VkClearValue}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkClearValue}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkClearValue where
        unsafeAddr (VkClearValue## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkClearValue## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkClearValue## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkClearValue where
        type StructFields VkClearValue = '["color", "depthStencil"] -- ' closing tick for hsc2hs
        type CUnionType VkClearValue = 'True -- ' closing tick for hsc2hs
        type ReturnedOnly VkClearValue = 'False -- ' closing tick for hsc2hs
        type StructExtends VkClearValue = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkColor VkClearValue where
        type VkColorMType VkClearValue = VkClearColorValue

        {-# NOINLINE vkColor #-}
        vkColor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearValue, color})

        {-# INLINE vkColorByteOffset #-}
        vkColorByteOffset ~_ = #{offset VkClearValue, color}

        {-# INLINE readVkColor #-}
        readVkColor p
          = peekByteOff p #{offset VkClearValue, color}

        {-# INLINE writeVkColor #-}
        writeVkColor p
          = pokeByteOff p #{offset VkClearValue, color}

instance {-# OVERLAPPING #-} HasField "color" VkClearValue where
        type FieldType "color" VkClearValue = VkClearColorValue
        type FieldOptional "color" VkClearValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "color" VkClearValue =
             #{offset VkClearValue, color}
        type FieldIsArray "color" VkClearValue = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearValue, color}

instance CanReadField "color" VkClearValue where
        {-# INLINE getField #-}
        getField = vkColor

        {-# INLINE readField #-}
        readField = readVkColor

instance CanWriteField "color" VkClearValue where
        {-# INLINE writeField #-}
        writeField = writeVkColor

instance {-# OVERLAPPING #-} HasVkDepthStencil VkClearValue where
        type VkDepthStencilMType VkClearValue = VkClearDepthStencilValue

        {-# NOINLINE vkDepthStencil #-}
        vkDepthStencil x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearValue, depthStencil})

        {-# INLINE vkDepthStencilByteOffset #-}
        vkDepthStencilByteOffset ~_
          = #{offset VkClearValue, depthStencil}

        {-# INLINE readVkDepthStencil #-}
        readVkDepthStencil p
          = peekByteOff p #{offset VkClearValue, depthStencil}

        {-# INLINE writeVkDepthStencil #-}
        writeVkDepthStencil p
          = pokeByteOff p #{offset VkClearValue, depthStencil}

instance {-# OVERLAPPING #-} HasField "depthStencil" VkClearValue
         where
        type FieldType "depthStencil" VkClearValue =
             VkClearDepthStencilValue
        type FieldOptional "depthStencil" VkClearValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthStencil" VkClearValue =
             #{offset VkClearValue, depthStencil}
        type FieldIsArray "depthStencil" VkClearValue = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearValue, depthStencil}

instance CanReadField "depthStencil" VkClearValue where
        {-# INLINE getField #-}
        getField = vkDepthStencil

        {-# INLINE readField #-}
        readField = readVkDepthStencil

instance CanWriteField "depthStencil" VkClearValue where
        {-# INLINE writeField #-}
        writeField = writeVkDepthStencil

instance Show VkClearValue where
        showsPrec d x
          = showString "VkClearValue {" .
              showString "vkColor = " .
                showsPrec d (vkColor x) .
                  showString ", " .
                    showString "vkDepthStencil = " .
                      showsPrec d (vkDepthStencil x) . showChar '}'
