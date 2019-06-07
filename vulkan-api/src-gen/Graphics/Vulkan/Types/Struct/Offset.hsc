#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Offset
       (VkOffset2D, VkOffset2D', VkOffset3D, VkOffset3D') where
import Foreign.Storable                 (Storable (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkOffset2D {
--   >     int32_t        x;
--   >     int32_t        y;
--   > } VkOffset2D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkOffset2D VkOffset2D registry at www.khronos.org>
type VkOffset2D = VulkanStruct VkOffset2D' -- ' closing tick for hsc2hs

data VkOffset2D' -- ' closing tick for hsc2hs

instance Eq VkOffset2D where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkOffset2D where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

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

instance VulkanMarshal VkOffset2D where
        type StructFields VkOffset2D = '["x", "y"] -- ' closing tick for hsc2hs
        type CUnionType VkOffset2D = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkOffset2D = 'False -- ' closing tick for hsc2hs
        type StructExtends VkOffset2D = '[] -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-} CanReadField "x" VkOffset2D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkOffset2D, x})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkOffset2D, x}

instance {-# OVERLAPPING #-} CanWriteField "x" VkOffset2D where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkOffset2D, x}

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

instance {-# OVERLAPPING #-} CanReadField "y" VkOffset2D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkOffset2D, y})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkOffset2D, y}

instance {-# OVERLAPPING #-} CanWriteField "y" VkOffset2D where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkOffset2D, y}

instance Show VkOffset2D where
        showsPrec d x
          = showString "VkOffset2D {" .
              showString "x = " .
                showsPrec d (getField @"x" x) .
                  showString ", " .
                    showString "y = " . showsPrec d (getField @"y" x) . showChar '}'

-- | > typedef struct VkOffset3D {
--   >     int32_t        x;
--   >     int32_t        y;
--   >     int32_t        z;
--   > } VkOffset3D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkOffset3D VkOffset3D registry at www.khronos.org>
type VkOffset3D = VulkanStruct VkOffset3D' -- ' closing tick for hsc2hs

data VkOffset3D' -- ' closing tick for hsc2hs

instance Eq VkOffset3D where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkOffset3D where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkOffset3D where
        sizeOf ~_ = #{size VkOffset3D}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkOffset3D}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkOffset3D where
        type StructFields VkOffset3D = '["x", "y", "z"] -- ' closing tick for hsc2hs
        type CUnionType VkOffset3D = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkOffset3D = 'False -- ' closing tick for hsc2hs
        type StructExtends VkOffset3D = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "x" VkOffset3D where
        type FieldType "x" VkOffset3D = Int32
        type FieldOptional "x" VkOffset3D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "x" VkOffset3D =
             #{offset VkOffset3D, x}
        type FieldIsArray "x" VkOffset3D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkOffset3D, x}

instance {-# OVERLAPPING #-} CanReadField "x" VkOffset3D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkOffset3D, x})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkOffset3D, x}

instance {-# OVERLAPPING #-} CanWriteField "x" VkOffset3D where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkOffset3D, x}

instance {-# OVERLAPPING #-} HasField "y" VkOffset3D where
        type FieldType "y" VkOffset3D = Int32
        type FieldOptional "y" VkOffset3D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "y" VkOffset3D =
             #{offset VkOffset3D, y}
        type FieldIsArray "y" VkOffset3D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkOffset3D, y}

instance {-# OVERLAPPING #-} CanReadField "y" VkOffset3D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkOffset3D, y})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkOffset3D, y}

instance {-# OVERLAPPING #-} CanWriteField "y" VkOffset3D where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkOffset3D, y}

instance {-# OVERLAPPING #-} HasField "z" VkOffset3D where
        type FieldType "z" VkOffset3D = Int32
        type FieldOptional "z" VkOffset3D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "z" VkOffset3D =
             #{offset VkOffset3D, z}
        type FieldIsArray "z" VkOffset3D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkOffset3D, z}

instance {-# OVERLAPPING #-} CanReadField "z" VkOffset3D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkOffset3D, z})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkOffset3D, z}

instance {-# OVERLAPPING #-} CanWriteField "z" VkOffset3D where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkOffset3D, z}

instance Show VkOffset3D where
        showsPrec d x
          = showString "VkOffset3D {" .
              showString "x = " .
                showsPrec d (getField @"x" x) .
                  showString ", " .
                    showString "y = " .
                      showsPrec d (getField @"y" x) .
                        showString ", " .
                          showString "z = " . showsPrec d (getField @"z" x) . showChar '}'
