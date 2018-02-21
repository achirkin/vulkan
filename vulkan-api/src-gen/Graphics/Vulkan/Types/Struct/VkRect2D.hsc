#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRect2D (VkRect2D(..)) where
import           Foreign.Storable                        (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkExtent2D (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.VkOffset2D (VkOffset2D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                        (unsafeDupablePerformIO)

-- | > typedef struct VkRect2D {
--   >     VkOffset2D     offset;
--   >     VkExtent2D     extent;
--   > } VkRect2D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkRect2D.html VkRect2D registry at www.khronos.org>
data VkRect2D = VkRect2D## Addr## ByteArray##

instance Eq VkRect2D where
        (VkRect2D## a _) == x@(VkRect2D## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRect2D where
        (VkRect2D## a _) `compare` x@(VkRect2D## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRect2D where
        sizeOf ~_ = #{size VkRect2D}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkRect2D}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRect2D where
        unsafeAddr (VkRect2D## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRect2D## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRect2D## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRect2D where
        type StructFields VkRect2D = '["offset", "extent"] -- ' closing tick for hsc2hs
        type CUnionType VkRect2D = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRect2D = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRect2D = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkOffset VkRect2D where
        type VkOffsetMType VkRect2D = VkOffset2D

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRect2D, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_ = #{offset VkRect2D, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkRect2D, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkRect2D, offset}

instance {-# OVERLAPPING #-} HasField "offset" VkRect2D where
        type FieldType "offset" VkRect2D = VkOffset2D
        type FieldOptional "offset" VkRect2D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkRect2D =
             #{offset VkRect2D, offset}
        type FieldIsArray "offset" VkRect2D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRect2D, offset}

instance CanReadField "offset" VkRect2D where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkRect2D where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-} HasVkExtent VkRect2D where
        type VkExtentMType VkRect2D = VkExtent2D

        {-# NOINLINE vkExtent #-}
        vkExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRect2D, extent})

        {-# INLINE vkExtentByteOffset #-}
        vkExtentByteOffset ~_ = #{offset VkRect2D, extent}

        {-# INLINE readVkExtent #-}
        readVkExtent p
          = peekByteOff p #{offset VkRect2D, extent}

        {-# INLINE writeVkExtent #-}
        writeVkExtent p
          = pokeByteOff p #{offset VkRect2D, extent}

instance {-# OVERLAPPING #-} HasField "extent" VkRect2D where
        type FieldType "extent" VkRect2D = VkExtent2D
        type FieldOptional "extent" VkRect2D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkRect2D =
             #{offset VkRect2D, extent}
        type FieldIsArray "extent" VkRect2D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRect2D, extent}

instance CanReadField "extent" VkRect2D where
        {-# INLINE getField #-}
        getField = vkExtent

        {-# INLINE readField #-}
        readField = readVkExtent

instance CanWriteField "extent" VkRect2D where
        {-# INLINE writeField #-}
        writeField = writeVkExtent

instance Show VkRect2D where
        showsPrec d x
          = showString "VkRect2D {" .
              showString "vkOffset = " .
                showsPrec d (vkOffset x) .
                  showString ", " .
                    showString "vkExtent = " . showsPrec d (vkExtent x) . showChar '}'
