#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkXYColorEXT (VkXYColorEXT(..))
       where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | Chromaticity coordinate
--
--   > typedef struct VkXYColorEXT {
--   >     float   x;
--   >     float   y;
--   > } VkXYColorEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkXYColorEXT.html VkXYColorEXT registry at www.khronos.org>
data VkXYColorEXT = VkXYColorEXT## Addr## ByteArray##

instance Eq VkXYColorEXT where
        (VkXYColorEXT## a _) == x@(VkXYColorEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkXYColorEXT where
        (VkXYColorEXT## a _) `compare` x@(VkXYColorEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkXYColorEXT where
        sizeOf ~_ = #{size VkXYColorEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkXYColorEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkXYColorEXT where
        unsafeAddr (VkXYColorEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkXYColorEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkXYColorEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkXYColorEXT where
        type StructFields VkXYColorEXT = '["x", "y"] -- ' closing tick for hsc2hs
        type CUnionType VkXYColorEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkXYColorEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkXYColorEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkX VkXYColorEXT where
        type VkXMType VkXYColorEXT = #{type float}

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXYColorEXT, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkXYColorEXT, x}

        {-# INLINE readVkX #-}
        readVkX p = peekByteOff p #{offset VkXYColorEXT, x}

        {-# INLINE writeVkX #-}
        writeVkX p = pokeByteOff p #{offset VkXYColorEXT, x}

instance {-# OVERLAPPING #-} HasField "x" VkXYColorEXT where
        type FieldType "x" VkXYColorEXT = #{type float}
        type FieldOptional "x" VkXYColorEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "x" VkXYColorEXT =
             #{offset VkXYColorEXT, x}
        type FieldIsArray "x" VkXYColorEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkXYColorEXT, x}

instance CanReadField "x" VkXYColorEXT where
        {-# INLINE getField #-}
        getField = vkX

        {-# INLINE readField #-}
        readField = readVkX

instance CanWriteField "x" VkXYColorEXT where
        {-# INLINE writeField #-}
        writeField = writeVkX

instance {-# OVERLAPPING #-} HasVkY VkXYColorEXT where
        type VkYMType VkXYColorEXT = #{type float}

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXYColorEXT, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkXYColorEXT, y}

        {-# INLINE readVkY #-}
        readVkY p = peekByteOff p #{offset VkXYColorEXT, y}

        {-# INLINE writeVkY #-}
        writeVkY p = pokeByteOff p #{offset VkXYColorEXT, y}

instance {-# OVERLAPPING #-} HasField "y" VkXYColorEXT where
        type FieldType "y" VkXYColorEXT = #{type float}
        type FieldOptional "y" VkXYColorEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "y" VkXYColorEXT =
             #{offset VkXYColorEXT, y}
        type FieldIsArray "y" VkXYColorEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkXYColorEXT, y}

instance CanReadField "y" VkXYColorEXT where
        {-# INLINE getField #-}
        getField = vkY

        {-# INLINE readField #-}
        readField = readVkY

instance CanWriteField "y" VkXYColorEXT where
        {-# INLINE writeField #-}
        writeField = writeVkY

instance Show VkXYColorEXT where
        showsPrec d x
          = showString "VkXYColorEXT {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " . showsPrec d (vkY x) . showChar '}'
