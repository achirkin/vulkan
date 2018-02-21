#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkViewport (VkViewport(..))
       where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkViewport {
--   >     float          x;
--   >     float          y;
--   >     float          width;
--   >     float          height;
--   >     float          minDepth;
--   >     float          maxDepth;
--   > } VkViewport;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkViewport.html VkViewport registry at www.khronos.org>
data VkViewport = VkViewport## Addr## ByteArray##

instance Eq VkViewport where
        (VkViewport## a _) == x@(VkViewport## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkViewport where
        (VkViewport## a _) `compare` x@(VkViewport## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkViewport where
        sizeOf ~_ = #{size VkViewport}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViewport}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkViewport where
        unsafeAddr (VkViewport## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkViewport## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkViewport## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkViewport where
        type StructFields VkViewport =
             '["x", "y", "width", "height", "minDepth", "maxDepth"] -- ' closing tick for hsc2hs
        type CUnionType VkViewport = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkViewport = 'False -- ' closing tick for hsc2hs
        type StructExtends VkViewport = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkX VkViewport where
        type VkXMType VkViewport = #{type float}

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkViewport, x}

        {-# INLINE readVkX #-}
        readVkX p = peekByteOff p #{offset VkViewport, x}

        {-# INLINE writeVkX #-}
        writeVkX p = pokeByteOff p #{offset VkViewport, x}

instance {-# OVERLAPPING #-} HasField "x" VkViewport where
        type FieldType "x" VkViewport = #{type float}
        type FieldOptional "x" VkViewport = 'False -- ' closing tick for hsc2hs
        type FieldOffset "x" VkViewport =
             #{offset VkViewport, x}
        type FieldIsArray "x" VkViewport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewport, x}

instance CanReadField "x" VkViewport where
        {-# INLINE getField #-}
        getField = vkX

        {-# INLINE readField #-}
        readField = readVkX

instance CanWriteField "x" VkViewport where
        {-# INLINE writeField #-}
        writeField = writeVkX

instance {-# OVERLAPPING #-} HasVkY VkViewport where
        type VkYMType VkViewport = #{type float}

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkViewport, y}

        {-# INLINE readVkY #-}
        readVkY p = peekByteOff p #{offset VkViewport, y}

        {-# INLINE writeVkY #-}
        writeVkY p = pokeByteOff p #{offset VkViewport, y}

instance {-# OVERLAPPING #-} HasField "y" VkViewport where
        type FieldType "y" VkViewport = #{type float}
        type FieldOptional "y" VkViewport = 'False -- ' closing tick for hsc2hs
        type FieldOffset "y" VkViewport =
             #{offset VkViewport, y}
        type FieldIsArray "y" VkViewport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewport, y}

instance CanReadField "y" VkViewport where
        {-# INLINE getField #-}
        getField = vkY

        {-# INLINE readField #-}
        readField = readVkY

instance CanWriteField "y" VkViewport where
        {-# INLINE writeField #-}
        writeField = writeVkY

instance {-# OVERLAPPING #-} HasVkWidth VkViewport where
        type VkWidthMType VkViewport = #{type float}

        {-# NOINLINE vkWidth #-}
        vkWidth x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, width})

        {-# INLINE vkWidthByteOffset #-}
        vkWidthByteOffset ~_ = #{offset VkViewport, width}

        {-# INLINE readVkWidth #-}
        readVkWidth p
          = peekByteOff p #{offset VkViewport, width}

        {-# INLINE writeVkWidth #-}
        writeVkWidth p
          = pokeByteOff p #{offset VkViewport, width}

instance {-# OVERLAPPING #-} HasField "width" VkViewport where
        type FieldType "width" VkViewport = #{type float}
        type FieldOptional "width" VkViewport = 'False -- ' closing tick for hsc2hs
        type FieldOffset "width" VkViewport =
             #{offset VkViewport, width}
        type FieldIsArray "width" VkViewport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewport, width}

instance CanReadField "width" VkViewport where
        {-# INLINE getField #-}
        getField = vkWidth

        {-# INLINE readField #-}
        readField = readVkWidth

instance CanWriteField "width" VkViewport where
        {-# INLINE writeField #-}
        writeField = writeVkWidth

instance {-# OVERLAPPING #-} HasVkHeight VkViewport where
        type VkHeightMType VkViewport = #{type float}

        {-# NOINLINE vkHeight #-}
        vkHeight x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, height})

        {-# INLINE vkHeightByteOffset #-}
        vkHeightByteOffset ~_ = #{offset VkViewport, height}

        {-# INLINE readVkHeight #-}
        readVkHeight p
          = peekByteOff p #{offset VkViewport, height}

        {-# INLINE writeVkHeight #-}
        writeVkHeight p
          = pokeByteOff p #{offset VkViewport, height}

instance {-# OVERLAPPING #-} HasField "height" VkViewport where
        type FieldType "height" VkViewport = #{type float}
        type FieldOptional "height" VkViewport = 'False -- ' closing tick for hsc2hs
        type FieldOffset "height" VkViewport =
             #{offset VkViewport, height}
        type FieldIsArray "height" VkViewport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewport, height}

instance CanReadField "height" VkViewport where
        {-# INLINE getField #-}
        getField = vkHeight

        {-# INLINE readField #-}
        readField = readVkHeight

instance CanWriteField "height" VkViewport where
        {-# INLINE writeField #-}
        writeField = writeVkHeight

instance {-# OVERLAPPING #-} HasVkMinDepth VkViewport where
        type VkMinDepthMType VkViewport = #{type float}

        {-# NOINLINE vkMinDepth #-}
        vkMinDepth x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, minDepth})

        {-# INLINE vkMinDepthByteOffset #-}
        vkMinDepthByteOffset ~_
          = #{offset VkViewport, minDepth}

        {-# INLINE readVkMinDepth #-}
        readVkMinDepth p
          = peekByteOff p #{offset VkViewport, minDepth}

        {-# INLINE writeVkMinDepth #-}
        writeVkMinDepth p
          = pokeByteOff p #{offset VkViewport, minDepth}

instance {-# OVERLAPPING #-} HasField "minDepth" VkViewport where
        type FieldType "minDepth" VkViewport = #{type float}
        type FieldOptional "minDepth" VkViewport = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minDepth" VkViewport =
             #{offset VkViewport, minDepth}
        type FieldIsArray "minDepth" VkViewport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewport, minDepth}

instance CanReadField "minDepth" VkViewport where
        {-# INLINE getField #-}
        getField = vkMinDepth

        {-# INLINE readField #-}
        readField = readVkMinDepth

instance CanWriteField "minDepth" VkViewport where
        {-# INLINE writeField #-}
        writeField = writeVkMinDepth

instance {-# OVERLAPPING #-} HasVkMaxDepth VkViewport where
        type VkMaxDepthMType VkViewport = #{type float}

        {-# NOINLINE vkMaxDepth #-}
        vkMaxDepth x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, maxDepth})

        {-# INLINE vkMaxDepthByteOffset #-}
        vkMaxDepthByteOffset ~_
          = #{offset VkViewport, maxDepth}

        {-# INLINE readVkMaxDepth #-}
        readVkMaxDepth p
          = peekByteOff p #{offset VkViewport, maxDepth}

        {-# INLINE writeVkMaxDepth #-}
        writeVkMaxDepth p
          = pokeByteOff p #{offset VkViewport, maxDepth}

instance {-# OVERLAPPING #-} HasField "maxDepth" VkViewport where
        type FieldType "maxDepth" VkViewport = #{type float}
        type FieldOptional "maxDepth" VkViewport = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDepth" VkViewport =
             #{offset VkViewport, maxDepth}
        type FieldIsArray "maxDepth" VkViewport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewport, maxDepth}

instance CanReadField "maxDepth" VkViewport where
        {-# INLINE getField #-}
        getField = vkMaxDepth

        {-# INLINE readField #-}
        readField = readVkMaxDepth

instance CanWriteField "maxDepth" VkViewport where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDepth

instance Show VkViewport where
        showsPrec d x
          = showString "VkViewport {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " .
                      showsPrec d (vkY x) .
                        showString ", " .
                          showString "vkWidth = " .
                            showsPrec d (vkWidth x) .
                              showString ", " .
                                showString "vkHeight = " .
                                  showsPrec d (vkHeight x) .
                                    showString ", " .
                                      showString "vkMinDepth = " .
                                        showsPrec d (vkMinDepth x) .
                                          showString ", " .
                                            showString "vkMaxDepth = " .
                                              showsPrec d (vkMaxDepth x) . showChar '}'
