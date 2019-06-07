#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.XYColorEXT
       (VkXYColorEXT, VkXYColorEXT') where -- ' closing tick for hsc2hs
import Foreign.Storable                 (Storable (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | Chromaticity coordinate
--
--   > typedef struct VkXYColorEXT {
--   >     float   x;
--   >     float   y;
--   > } VkXYColorEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkXYColorEXT VkXYColorEXT registry at www.khronos.org>
type VkXYColorEXT = VulkanStruct VkXYColorEXT' -- ' closing tick for hsc2hs

data VkXYColorEXT' -- ' closing tick for hsc2hs

instance Eq VkXYColorEXT where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkXYColorEXT where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

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

instance VulkanMarshal VkXYColorEXT where
        type StructFields VkXYColorEXT = '["x", "y"] -- ' closing tick for hsc2hs
        type CUnionType VkXYColorEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkXYColorEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkXYColorEXT = '[] -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-} CanReadField "x" VkXYColorEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXYColorEXT, x})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkXYColorEXT, x}

instance {-# OVERLAPPING #-} CanWriteField "x" VkXYColorEXT where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkXYColorEXT, x}

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

instance {-# OVERLAPPING #-} CanReadField "y" VkXYColorEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXYColorEXT, y})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkXYColorEXT, y}

instance {-# OVERLAPPING #-} CanWriteField "y" VkXYColorEXT where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkXYColorEXT, y}

instance Show VkXYColorEXT where
        showsPrec d x
          = showString "VkXYColorEXT {" .
              showString "x = " .
                showsPrec d (getField @"x" x) .
                  showString ", " .
                    showString "y = " . showsPrec d (getField @"y" x) . showChar '}'
