#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSampleLocationEXT
       (VkSampleLocationEXT(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkSampleLocationEXT {
--   >     float                            x;
--   >     float                            y;
--   > } VkSampleLocationEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSampleLocationEXT.html VkSampleLocationEXT registry at www.khronos.org>
data VkSampleLocationEXT = VkSampleLocationEXT## Addr## ByteArray##

instance Eq VkSampleLocationEXT where
        (VkSampleLocationEXT## a _) == x@(VkSampleLocationEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSampleLocationEXT where
        (VkSampleLocationEXT## a _) `compare` x@(VkSampleLocationEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSampleLocationEXT where
        sizeOf ~_ = #{size VkSampleLocationEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSampleLocationEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSampleLocationEXT where
        unsafeAddr (VkSampleLocationEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSampleLocationEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSampleLocationEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSampleLocationEXT where
        type StructFields VkSampleLocationEXT = '["x", "y"] -- ' closing tick for hsc2hs
        type CUnionType VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSampleLocationEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "x" VkSampleLocationEXT where
        type FieldType "x" VkSampleLocationEXT = #{type float}
        type FieldOptional "x" VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "x" VkSampleLocationEXT =
             #{offset VkSampleLocationEXT, x}
        type FieldIsArray "x" VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSampleLocationEXT, x}

instance {-# OVERLAPPING #-} CanReadField "x" VkSampleLocationEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationEXT, x})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSampleLocationEXT, x}

instance {-# OVERLAPPING #-} CanWriteField "x" VkSampleLocationEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSampleLocationEXT, x}

instance {-# OVERLAPPING #-} HasField "y" VkSampleLocationEXT where
        type FieldType "y" VkSampleLocationEXT = #{type float}
        type FieldOptional "y" VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "y" VkSampleLocationEXT =
             #{offset VkSampleLocationEXT, y}
        type FieldIsArray "y" VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSampleLocationEXT, y}

instance {-# OVERLAPPING #-} CanReadField "y" VkSampleLocationEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationEXT, y})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSampleLocationEXT, y}

instance {-# OVERLAPPING #-} CanWriteField "y" VkSampleLocationEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSampleLocationEXT, y}

instance Show VkSampleLocationEXT where
        showsPrec d x
          = showString "VkSampleLocationEXT {" .
              showString "x = " .
                showsPrec d (getField @"x" x) .
                  showString ", " .
                    showString "y = " . showsPrec d (getField @"y" x) . showChar '}'
