#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkOffset3D (VkOffset3D(..))
       where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkOffset3D {
--   >     int32_t        x;
--   >     int32_t        y;
--   >     int32_t        z;
--   > } VkOffset3D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkOffset3D.html VkOffset3D registry at www.khronos.org>
data VkOffset3D = VkOffset3D## Addr## ByteArray##

instance Eq VkOffset3D where
        (VkOffset3D## a _) == x@(VkOffset3D## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkOffset3D where
        (VkOffset3D## a _) `compare` x@(VkOffset3D## b _)
          = cmpBytes## (sizeOf x) a b

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

instance VulkanMarshalPrim VkOffset3D where
        unsafeAddr (VkOffset3D## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkOffset3D## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkOffset3D## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkOffset3D where
        type StructFields VkOffset3D = '["x", "y", "z"] -- ' closing tick for hsc2hs
        type CUnionType VkOffset3D = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkOffset3D = 'False -- ' closing tick for hsc2hs
        type StructExtends VkOffset3D = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkX VkOffset3D where
        type VkXMType VkOffset3D = Int32

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkOffset3D, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkOffset3D, x}

        {-# INLINE readVkX #-}
        readVkX p = peekByteOff p #{offset VkOffset3D, x}

        {-# INLINE writeVkX #-}
        writeVkX p = pokeByteOff p #{offset VkOffset3D, x}

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

instance CanReadField "x" VkOffset3D where
        {-# INLINE getField #-}
        getField = vkX

        {-# INLINE readField #-}
        readField = readVkX

instance CanWriteField "x" VkOffset3D where
        {-# INLINE writeField #-}
        writeField = writeVkX

instance {-# OVERLAPPING #-} HasVkY VkOffset3D where
        type VkYMType VkOffset3D = Int32

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkOffset3D, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkOffset3D, y}

        {-# INLINE readVkY #-}
        readVkY p = peekByteOff p #{offset VkOffset3D, y}

        {-# INLINE writeVkY #-}
        writeVkY p = pokeByteOff p #{offset VkOffset3D, y}

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

instance CanReadField "y" VkOffset3D where
        {-# INLINE getField #-}
        getField = vkY

        {-# INLINE readField #-}
        readField = readVkY

instance CanWriteField "y" VkOffset3D where
        {-# INLINE writeField #-}
        writeField = writeVkY

instance {-# OVERLAPPING #-} HasVkZ VkOffset3D where
        type VkZMType VkOffset3D = Int32

        {-# NOINLINE vkZ #-}
        vkZ x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkOffset3D, z})

        {-# INLINE vkZByteOffset #-}
        vkZByteOffset ~_ = #{offset VkOffset3D, z}

        {-# INLINE readVkZ #-}
        readVkZ p = peekByteOff p #{offset VkOffset3D, z}

        {-# INLINE writeVkZ #-}
        writeVkZ p = pokeByteOff p #{offset VkOffset3D, z}

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

instance CanReadField "z" VkOffset3D where
        {-# INLINE getField #-}
        getField = vkZ

        {-# INLINE readField #-}
        readField = readVkZ

instance CanWriteField "z" VkOffset3D where
        {-# INLINE writeField #-}
        writeField = writeVkZ

instance Show VkOffset3D where
        showsPrec d x
          = showString "VkOffset3D {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " .
                      showsPrec d (vkY x) .
                        showString ", " .
                          showString "vkZ = " . showsPrec d (vkZ x) . showChar '}'
