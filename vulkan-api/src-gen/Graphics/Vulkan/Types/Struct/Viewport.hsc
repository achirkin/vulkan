#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Viewport
       (VkViewport(..), VkViewportSwizzleNV(..), VkViewportWScalingNV(..))
       where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Base                                               (Addr##, ByteArray##,
                                                                         byteArrayContents##,
                                                                         plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.ViewportCoordinateSwizzleNV (VkViewportCoordinateSwizzleNV)
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkViewport {
--   >     float          x;
--   >     float          y;
--   >     float          width;
--   >     float          height;
--   >     float          minDepth;
--   >     float          maxDepth;
--   > } VkViewport;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkViewport VkViewport registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} CanReadField "x" VkViewport where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, x})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkViewport, x}

instance {-# OVERLAPPING #-} CanWriteField "x" VkViewport where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkViewport, x}

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

instance {-# OVERLAPPING #-} CanReadField "y" VkViewport where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, y})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkViewport, y}

instance {-# OVERLAPPING #-} CanWriteField "y" VkViewport where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkViewport, y}

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

instance {-# OVERLAPPING #-} CanReadField "width" VkViewport where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, width})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkViewport, width}

instance {-# OVERLAPPING #-} CanWriteField "width" VkViewport where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewport, width}

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

instance {-# OVERLAPPING #-} CanReadField "height" VkViewport where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, height})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewport, height}

instance {-# OVERLAPPING #-} CanWriteField "height" VkViewport
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewport, height}

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

instance {-# OVERLAPPING #-} CanReadField "minDepth" VkViewport
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, minDepth})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewport, minDepth}

instance {-# OVERLAPPING #-} CanWriteField "minDepth" VkViewport
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewport, minDepth}

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

instance {-# OVERLAPPING #-} CanReadField "maxDepth" VkViewport
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewport, maxDepth})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewport, maxDepth}

instance {-# OVERLAPPING #-} CanWriteField "maxDepth" VkViewport
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewport, maxDepth}

instance Show VkViewport where
        showsPrec d x
          = showString "VkViewport {" .
              showString "x = " .
                showsPrec d (getField @"x" x) .
                  showString ", " .
                    showString "y = " .
                      showsPrec d (getField @"y" x) .
                        showString ", " .
                          showString "width = " .
                            showsPrec d (getField @"width" x) .
                              showString ", " .
                                showString "height = " .
                                  showsPrec d (getField @"height" x) .
                                    showString ", " .
                                      showString "minDepth = " .
                                        showsPrec d (getField @"minDepth" x) .
                                          showString ", " .
                                            showString "maxDepth = " .
                                              showsPrec d (getField @"maxDepth" x) . showChar '}'

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

-- | > typedef struct VkViewportWScalingNV {
--   >     float          xcoeff;
--   >     float          ycoeff;
--   > } VkViewportWScalingNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkViewportWScalingNV VkViewportWScalingNV registry at www.khronos.org>
data VkViewportWScalingNV = VkViewportWScalingNV## Addr## ByteArray##

instance Eq VkViewportWScalingNV where
        (VkViewportWScalingNV## a _) == x@(VkViewportWScalingNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkViewportWScalingNV where
        (VkViewportWScalingNV## a _) `compare` x@(VkViewportWScalingNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkViewportWScalingNV where
        sizeOf ~_ = #{size VkViewportWScalingNV}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViewportWScalingNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkViewportWScalingNV where
        unsafeAddr (VkViewportWScalingNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkViewportWScalingNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkViewportWScalingNV## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkViewportWScalingNV where
        type StructFields VkViewportWScalingNV = '["xcoeff", "ycoeff"] -- ' closing tick for hsc2hs
        type CUnionType VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkViewportWScalingNV = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "xcoeff" VkViewportWScalingNV
         where
        type FieldType "xcoeff" VkViewportWScalingNV =
             #{type float}
        type FieldOptional "xcoeff" VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "xcoeff" VkViewportWScalingNV =
             #{offset VkViewportWScalingNV, xcoeff}
        type FieldIsArray "xcoeff" VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportWScalingNV, xcoeff}

instance {-# OVERLAPPING #-}
         CanReadField "xcoeff" VkViewportWScalingNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportWScalingNV, xcoeff})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewportWScalingNV, xcoeff}

instance {-# OVERLAPPING #-}
         CanWriteField "xcoeff" VkViewportWScalingNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewportWScalingNV, xcoeff}

instance {-# OVERLAPPING #-} HasField "ycoeff" VkViewportWScalingNV
         where
        type FieldType "ycoeff" VkViewportWScalingNV =
             #{type float}
        type FieldOptional "ycoeff" VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ycoeff" VkViewportWScalingNV =
             #{offset VkViewportWScalingNV, ycoeff}
        type FieldIsArray "ycoeff" VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportWScalingNV, ycoeff}

instance {-# OVERLAPPING #-}
         CanReadField "ycoeff" VkViewportWScalingNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportWScalingNV, ycoeff})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewportWScalingNV, ycoeff}

instance {-# OVERLAPPING #-}
         CanWriteField "ycoeff" VkViewportWScalingNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewportWScalingNV, ycoeff}

instance Show VkViewportWScalingNV where
        showsPrec d x
          = showString "VkViewportWScalingNV {" .
              showString "xcoeff = " .
                showsPrec d (getField @"xcoeff" x) .
                  showString ", " .
                    showString "ycoeff = " .
                      showsPrec d (getField @"ycoeff" x) . showChar '}'
