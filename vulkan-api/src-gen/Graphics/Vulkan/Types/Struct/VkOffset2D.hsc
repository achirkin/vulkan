#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkOffset2D (VkOffset2D(..))
       where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkOffset2D {
--   >     int32_t        x;
--   >     int32_t        y;
--   > } VkOffset2D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkOffset2D.html VkOffset2D registry at www.khronos.org>
data VkOffset2D = VkOffset2D## Addr## ByteArray##

instance Eq VkOffset2D where
        (VkOffset2D## a _) == x@(VkOffset2D## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkOffset2D where
        (VkOffset2D## a _) `compare` x@(VkOffset2D## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkOffset2D where
        sizeOf ~_ = #{size VkOffset2D}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkOffset2D}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkOffset2D where
        unsafeAddr (VkOffset2D## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkOffset2D## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkOffset2D## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkOffset2D where
        type StructFields VkOffset2D = '["x", "y"] -- ' closing tick for hsc2hs
        type CUnionType VkOffset2D = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkOffset2D = 'False -- ' closing tick for hsc2hs
        type StructExtends VkOffset2D = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkX VkOffset2D where
        type VkXMType VkOffset2D = Int32

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkOffset2D, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkOffset2D, x}

        {-# INLINE readVkX #-}
        readVkX p = peekByteOff p #{offset VkOffset2D, x}

        {-# INLINE writeVkX #-}
        writeVkX p = pokeByteOff p #{offset VkOffset2D, x}

instance {-# OVERLAPPING #-} HasField "x" VkOffset2D where
        type FieldType "x" VkOffset2D = Int32
        type FieldOptional "x" VkOffset2D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "x" VkOffset2D =
             #{offset VkOffset2D, x}
        type FieldIsArray "x" VkOffset2D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkOffset2D, x}

instance CanReadField "x" VkOffset2D where
        {-# INLINE getField #-}
        getField = vkX

        {-# INLINE readField #-}
        readField = readVkX

instance CanWriteField "x" VkOffset2D where
        {-# INLINE writeField #-}
        writeField = writeVkX

instance {-# OVERLAPPING #-} HasVkY VkOffset2D where
        type VkYMType VkOffset2D = Int32

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkOffset2D, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkOffset2D, y}

        {-# INLINE readVkY #-}
        readVkY p = peekByteOff p #{offset VkOffset2D, y}

        {-# INLINE writeVkY #-}
        writeVkY p = pokeByteOff p #{offset VkOffset2D, y}

instance {-# OVERLAPPING #-} HasField "y" VkOffset2D where
        type FieldType "y" VkOffset2D = Int32
        type FieldOptional "y" VkOffset2D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "y" VkOffset2D =
             #{offset VkOffset2D, y}
        type FieldIsArray "y" VkOffset2D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkOffset2D, y}

instance CanReadField "y" VkOffset2D where
        {-# INLINE getField #-}
        getField = vkY

        {-# INLINE readField #-}
        readField = readVkY

instance CanWriteField "y" VkOffset2D where
        {-# INLINE writeField #-}
        writeField = writeVkY

instance Show VkOffset2D where
        showsPrec d x
          = showString "VkOffset2D {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " . showsPrec d (vkY x) . showChar '}'
