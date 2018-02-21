#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkViewportSwizzleNV
       (VkViewportSwizzleNV(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkViewportCoordinateSwizzleNV (VkViewportCoordinateSwizzleNV)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkViewportSwizzleNV {
--   >     VkViewportCoordinateSwizzleNV          x;
--   >     VkViewportCoordinateSwizzleNV          y;
--   >     VkViewportCoordinateSwizzleNV          z;
--   >     VkViewportCoordinateSwizzleNV          w;
--   > } VkViewportSwizzleNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkViewportSwizzleNV.html VkViewportSwizzleNV registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkX VkViewportSwizzleNV where
        type VkXMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkViewportSwizzleNV, x}

        {-# INLINE readVkX #-}
        readVkX p
          = peekByteOff p #{offset VkViewportSwizzleNV, x}

        {-# INLINE writeVkX #-}
        writeVkX p
          = pokeByteOff p #{offset VkViewportSwizzleNV, x}

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

instance CanReadField "x" VkViewportSwizzleNV where
        {-# INLINE getField #-}
        getField = vkX

        {-# INLINE readField #-}
        readField = readVkX

instance CanWriteField "x" VkViewportSwizzleNV where
        {-# INLINE writeField #-}
        writeField = writeVkX

instance {-# OVERLAPPING #-} HasVkY VkViewportSwizzleNV where
        type VkYMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkViewportSwizzleNV, y}

        {-# INLINE readVkY #-}
        readVkY p
          = peekByteOff p #{offset VkViewportSwizzleNV, y}

        {-# INLINE writeVkY #-}
        writeVkY p
          = pokeByteOff p #{offset VkViewportSwizzleNV, y}

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

instance CanReadField "y" VkViewportSwizzleNV where
        {-# INLINE getField #-}
        getField = vkY

        {-# INLINE readField #-}
        readField = readVkY

instance CanWriteField "y" VkViewportSwizzleNV where
        {-# INLINE writeField #-}
        writeField = writeVkY

instance {-# OVERLAPPING #-} HasVkZ VkViewportSwizzleNV where
        type VkZMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkZ #-}
        vkZ x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, z})

        {-# INLINE vkZByteOffset #-}
        vkZByteOffset ~_ = #{offset VkViewportSwizzleNV, z}

        {-# INLINE readVkZ #-}
        readVkZ p
          = peekByteOff p #{offset VkViewportSwizzleNV, z}

        {-# INLINE writeVkZ #-}
        writeVkZ p
          = pokeByteOff p #{offset VkViewportSwizzleNV, z}

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

instance CanReadField "z" VkViewportSwizzleNV where
        {-# INLINE getField #-}
        getField = vkZ

        {-# INLINE readField #-}
        readField = readVkZ

instance CanWriteField "z" VkViewportSwizzleNV where
        {-# INLINE writeField #-}
        writeField = writeVkZ

instance {-# OVERLAPPING #-} HasVkW VkViewportSwizzleNV where
        type VkWMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkW #-}
        vkW x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, w})

        {-# INLINE vkWByteOffset #-}
        vkWByteOffset ~_ = #{offset VkViewportSwizzleNV, w}

        {-# INLINE readVkW #-}
        readVkW p
          = peekByteOff p #{offset VkViewportSwizzleNV, w}

        {-# INLINE writeVkW #-}
        writeVkW p
          = pokeByteOff p #{offset VkViewportSwizzleNV, w}

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

instance CanReadField "w" VkViewportSwizzleNV where
        {-# INLINE getField #-}
        getField = vkW

        {-# INLINE readField #-}
        readField = readVkW

instance CanWriteField "w" VkViewportSwizzleNV where
        {-# INLINE writeField #-}
        writeField = writeVkW

instance Show VkViewportSwizzleNV where
        showsPrec d x
          = showString "VkViewportSwizzleNV {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " .
                      showsPrec d (vkY x) .
                        showString ", " .
                          showString "vkZ = " .
                            showsPrec d (vkZ x) .
                              showString ", " .
                                showString "vkW = " . showsPrec d (vkW x) . showChar '}'
