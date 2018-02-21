#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkClearRect (VkClearRect(..))
       where
import           Foreign.Storable                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkRect2D (VkRect2D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                      (unsafeDupablePerformIO)

-- | > typedef struct VkClearRect {
--   >     VkRect2D       rect;
--   >     uint32_t       baseArrayLayer;
--   >     uint32_t       layerCount;
--   > } VkClearRect;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkClearRect.html VkClearRect registry at www.khronos.org>
data VkClearRect = VkClearRect## Addr## ByteArray##

instance Eq VkClearRect where
        (VkClearRect## a _) == x@(VkClearRect## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkClearRect where
        (VkClearRect## a _) `compare` x@(VkClearRect## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkClearRect where
        sizeOf ~_ = #{size VkClearRect}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkClearRect}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkClearRect where
        unsafeAddr (VkClearRect## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkClearRect## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkClearRect## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkClearRect where
        type StructFields VkClearRect =
             '["rect", "baseArrayLayer", "layerCount"] -- ' closing tick for hsc2hs
        type CUnionType VkClearRect = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkClearRect = 'False -- ' closing tick for hsc2hs
        type StructExtends VkClearRect = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkRect VkClearRect where
        type VkRectMType VkClearRect = VkRect2D

        {-# NOINLINE vkRect #-}
        vkRect x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearRect, rect})

        {-# INLINE vkRectByteOffset #-}
        vkRectByteOffset ~_ = #{offset VkClearRect, rect}

        {-# INLINE readVkRect #-}
        readVkRect p
          = peekByteOff p #{offset VkClearRect, rect}

        {-# INLINE writeVkRect #-}
        writeVkRect p
          = pokeByteOff p #{offset VkClearRect, rect}

instance {-# OVERLAPPING #-} HasField "rect" VkClearRect where
        type FieldType "rect" VkClearRect = VkRect2D
        type FieldOptional "rect" VkClearRect = 'False -- ' closing tick for hsc2hs
        type FieldOffset "rect" VkClearRect =
             #{offset VkClearRect, rect}
        type FieldIsArray "rect" VkClearRect = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearRect, rect}

instance CanReadField "rect" VkClearRect where
        {-# INLINE getField #-}
        getField = vkRect

        {-# INLINE readField #-}
        readField = readVkRect

instance CanWriteField "rect" VkClearRect where
        {-# INLINE writeField #-}
        writeField = writeVkRect

instance {-# OVERLAPPING #-} HasVkBaseArrayLayer VkClearRect where
        type VkBaseArrayLayerMType VkClearRect = Word32

        {-# NOINLINE vkBaseArrayLayer #-}
        vkBaseArrayLayer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearRect, baseArrayLayer})

        {-# INLINE vkBaseArrayLayerByteOffset #-}
        vkBaseArrayLayerByteOffset ~_
          = #{offset VkClearRect, baseArrayLayer}

        {-# INLINE readVkBaseArrayLayer #-}
        readVkBaseArrayLayer p
          = peekByteOff p #{offset VkClearRect, baseArrayLayer}

        {-# INLINE writeVkBaseArrayLayer #-}
        writeVkBaseArrayLayer p
          = pokeByteOff p #{offset VkClearRect, baseArrayLayer}

instance {-# OVERLAPPING #-} HasField "baseArrayLayer" VkClearRect
         where
        type FieldType "baseArrayLayer" VkClearRect = Word32
        type FieldOptional "baseArrayLayer" VkClearRect = 'False -- ' closing tick for hsc2hs
        type FieldOffset "baseArrayLayer" VkClearRect =
             #{offset VkClearRect, baseArrayLayer}
        type FieldIsArray "baseArrayLayer" VkClearRect = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearRect, baseArrayLayer}

instance CanReadField "baseArrayLayer" VkClearRect where
        {-# INLINE getField #-}
        getField = vkBaseArrayLayer

        {-# INLINE readField #-}
        readField = readVkBaseArrayLayer

instance CanWriteField "baseArrayLayer" VkClearRect where
        {-# INLINE writeField #-}
        writeField = writeVkBaseArrayLayer

instance {-# OVERLAPPING #-} HasVkLayerCount VkClearRect where
        type VkLayerCountMType VkClearRect = Word32

        {-# NOINLINE vkLayerCount #-}
        vkLayerCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearRect, layerCount})

        {-# INLINE vkLayerCountByteOffset #-}
        vkLayerCountByteOffset ~_
          = #{offset VkClearRect, layerCount}

        {-# INLINE readVkLayerCount #-}
        readVkLayerCount p
          = peekByteOff p #{offset VkClearRect, layerCount}

        {-# INLINE writeVkLayerCount #-}
        writeVkLayerCount p
          = pokeByteOff p #{offset VkClearRect, layerCount}

instance {-# OVERLAPPING #-} HasField "layerCount" VkClearRect
         where
        type FieldType "layerCount" VkClearRect = Word32
        type FieldOptional "layerCount" VkClearRect = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layerCount" VkClearRect =
             #{offset VkClearRect, layerCount}
        type FieldIsArray "layerCount" VkClearRect = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearRect, layerCount}

instance CanReadField "layerCount" VkClearRect where
        {-# INLINE getField #-}
        getField = vkLayerCount

        {-# INLINE readField #-}
        readField = readVkLayerCount

instance CanWriteField "layerCount" VkClearRect where
        {-# INLINE writeField #-}
        writeField = writeVkLayerCount

instance Show VkClearRect where
        showsPrec d x
          = showString "VkClearRect {" .
              showString "vkRect = " .
                showsPrec d (vkRect x) .
                  showString ", " .
                    showString "vkBaseArrayLayer = " .
                      showsPrec d (vkBaseArrayLayer x) .
                        showString ", " .
                          showString "vkLayerCount = " .
                            showsPrec d (vkLayerCount x) . showChar '}'
