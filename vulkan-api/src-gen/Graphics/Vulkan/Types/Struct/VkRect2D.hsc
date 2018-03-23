#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRect2D (VkRect2D(..)) where
import           Foreign.Storable                        (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkExtent2D (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.VkOffset2D (VkOffset2D)
import           System.IO.Unsafe                        (unsafeDupablePerformIO)

-- | > typedef struct VkRect2D {
--   >     VkOffset2D     offset;
--   >     VkExtent2D     extent;
--   > } VkRect2D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkRect2D.html VkRect2D registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} CanReadField "offset" VkRect2D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRect2D, offset})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkRect2D, offset}

instance {-# OVERLAPPING #-} CanWriteField "offset" VkRect2D where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkRect2D, offset}

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

instance {-# OVERLAPPING #-} CanReadField "extent" VkRect2D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRect2D, extent})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkRect2D, extent}

instance {-# OVERLAPPING #-} CanWriteField "extent" VkRect2D where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkRect2D, extent}

instance Show VkRect2D where
        showsPrec d x
          = showString "VkRect2D {" .
              showString "offset = " .
                showsPrec d (getField @"offset" x) .
                  showString ", " .
                    showString "extent = " .
                      showsPrec d (getField @"extent" x) . showChar '}'
