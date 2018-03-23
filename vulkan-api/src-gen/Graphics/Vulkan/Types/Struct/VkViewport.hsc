#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkViewport (VkViewport(..))
       where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkViewport {
--   >     float          x;
--   >     float          y;
--   >     float          width;
--   >     float          height;
--   >     float          minDepth;
--   >     float          maxDepth;
--   > } VkViewport;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkViewport.html VkViewport registry at www.khronos.org>
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
