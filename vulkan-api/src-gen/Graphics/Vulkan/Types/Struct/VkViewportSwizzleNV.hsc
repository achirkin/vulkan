#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkViewportSwizzleNV
       (VkViewportSwizzleNV(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkViewportCoordinateSwizzleNV (VkViewportCoordinateSwizzleNV)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkViewportSwizzleNV {
--   >     VkViewportCoordinateSwizzleNV          x;
--   >     VkViewportCoordinateSwizzleNV          y;
--   >     VkViewportCoordinateSwizzleNV          z;
--   >     VkViewportCoordinateSwizzleNV          w;
--   > } VkViewportSwizzleNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkViewportSwizzleNV VkViewportSwizzleNV registry at www.khronos.org>
data VkViewportSwizzleNV = VkViewportSwizzleNV## Addr## ByteArray##

instance Eq VkViewportSwizzleNV where
        (VkViewportSwizzleNV## a _) == x@(VkViewportSwizzleNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkViewportSwizzleNV where
        (VkViewportSwizzleNV## a _) `compare` x@(VkViewportSwizzleNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkViewportSwizzleNV where
        sizeOf ~_ = #{size VkViewportSwizzleNV}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViewportSwizzleNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkViewportSwizzleNV where
        unsafeAddr (VkViewportSwizzleNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkViewportSwizzleNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkViewportSwizzleNV## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkViewportSwizzleNV where
        type StructFields VkViewportSwizzleNV = '["x", "y", "z", "w"] -- ' closing tick for hsc2hs
        type CUnionType VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkViewportSwizzleNV = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "x" VkViewportSwizzleNV where
        type FieldType "x" VkViewportSwizzleNV =
             VkViewportCoordinateSwizzleNV
        type FieldOptional "x" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "x" VkViewportSwizzleNV =
             #{offset VkViewportSwizzleNV, x}
        type FieldIsArray "x" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportSwizzleNV, x}

instance {-# OVERLAPPING #-} CanReadField "x" VkViewportSwizzleNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, x})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewportSwizzleNV, x}

instance {-# OVERLAPPING #-} CanWriteField "x" VkViewportSwizzleNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewportSwizzleNV, x}

instance {-# OVERLAPPING #-} HasField "y" VkViewportSwizzleNV where
        type FieldType "y" VkViewportSwizzleNV =
             VkViewportCoordinateSwizzleNV
        type FieldOptional "y" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "y" VkViewportSwizzleNV =
             #{offset VkViewportSwizzleNV, y}
        type FieldIsArray "y" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportSwizzleNV, y}

instance {-# OVERLAPPING #-} CanReadField "y" VkViewportSwizzleNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, y})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewportSwizzleNV, y}

instance {-# OVERLAPPING #-} CanWriteField "y" VkViewportSwizzleNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewportSwizzleNV, y}

instance {-# OVERLAPPING #-} HasField "z" VkViewportSwizzleNV where
        type FieldType "z" VkViewportSwizzleNV =
             VkViewportCoordinateSwizzleNV
        type FieldOptional "z" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "z" VkViewportSwizzleNV =
             #{offset VkViewportSwizzleNV, z}
        type FieldIsArray "z" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportSwizzleNV, z}

instance {-# OVERLAPPING #-} CanReadField "z" VkViewportSwizzleNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, z})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewportSwizzleNV, z}

instance {-# OVERLAPPING #-} CanWriteField "z" VkViewportSwizzleNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewportSwizzleNV, z}

instance {-# OVERLAPPING #-} HasField "w" VkViewportSwizzleNV where
        type FieldType "w" VkViewportSwizzleNV =
             VkViewportCoordinateSwizzleNV
        type FieldOptional "w" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "w" VkViewportSwizzleNV =
             #{offset VkViewportSwizzleNV, w}
        type FieldIsArray "w" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportSwizzleNV, w}

instance {-# OVERLAPPING #-} CanReadField "w" VkViewportSwizzleNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, w})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewportSwizzleNV, w}

instance {-# OVERLAPPING #-} CanWriteField "w" VkViewportSwizzleNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewportSwizzleNV, w}

instance Show VkViewportSwizzleNV where
        showsPrec d x
          = showString "VkViewportSwizzleNV {" .
              showString "x = " .
                showsPrec d (getField @"x" x) .
                  showString ", " .
                    showString "y = " .
                      showsPrec d (getField @"y" x) .
                        showString ", " .
                          showString "z = " .
                            showsPrec d (getField @"z" x) .
                              showString ", " .
                                showString "w = " . showsPrec d (getField @"w" x) . showChar '}'
