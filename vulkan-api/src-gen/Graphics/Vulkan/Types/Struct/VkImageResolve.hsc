#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageResolve
       (VkImageResolve(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkExtent3D               (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers (VkImageSubresourceLayers)
import           Graphics.Vulkan.Types.Struct.VkOffset3D               (VkOffset3D)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkImageResolve {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffset;
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffset;
--   >     VkExtent3D             extent;
--   > } VkImageResolve;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkImageResolve.html VkImageResolve registry at www.khronos.org>
data VkImageResolve = VkImageResolve## Addr## ByteArray##

instance Eq VkImageResolve where
        (VkImageResolve## a _) == x@(VkImageResolve## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageResolve where
        (VkImageResolve## a _) `compare` x@(VkImageResolve## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageResolve where
        sizeOf ~_ = #{size VkImageResolve}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageResolve}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageResolve where
        unsafeAddr (VkImageResolve## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageResolve## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageResolve## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageResolve where
        type StructFields VkImageResolve =
             '["srcSubresource", "srcOffset", "dstSubresource", "dstOffset", -- ' closing tick for hsc2hs
               "extent"]
        type CUnionType VkImageResolve = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageResolve = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageResolve = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "srcSubresource" VkImageResolve where
        type FieldType "srcSubresource" VkImageResolve =
             VkImageSubresourceLayers
        type FieldOptional "srcSubresource" VkImageResolve = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcSubresource" VkImageResolve =
             #{offset VkImageResolve, srcSubresource}
        type FieldIsArray "srcSubresource" VkImageResolve = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageResolve, srcSubresource}

instance {-# OVERLAPPING #-}
         CanReadField "srcSubresource" VkImageResolve where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageResolve, srcSubresource})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageResolve, srcSubresource}

instance {-# OVERLAPPING #-}
         CanWriteField "srcSubresource" VkImageResolve where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageResolve, srcSubresource}

instance {-# OVERLAPPING #-} HasField "srcOffset" VkImageResolve
         where
        type FieldType "srcOffset" VkImageResolve = VkOffset3D
        type FieldOptional "srcOffset" VkImageResolve = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcOffset" VkImageResolve =
             #{offset VkImageResolve, srcOffset}
        type FieldIsArray "srcOffset" VkImageResolve = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageResolve, srcOffset}

instance {-# OVERLAPPING #-}
         CanReadField "srcOffset" VkImageResolve where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageResolve, srcOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageResolve, srcOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "srcOffset" VkImageResolve where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageResolve, srcOffset}

instance {-# OVERLAPPING #-}
         HasField "dstSubresource" VkImageResolve where
        type FieldType "dstSubresource" VkImageResolve =
             VkImageSubresourceLayers
        type FieldOptional "dstSubresource" VkImageResolve = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstSubresource" VkImageResolve =
             #{offset VkImageResolve, dstSubresource}
        type FieldIsArray "dstSubresource" VkImageResolve = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageResolve, dstSubresource}

instance {-# OVERLAPPING #-}
         CanReadField "dstSubresource" VkImageResolve where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageResolve, dstSubresource})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageResolve, dstSubresource}

instance {-# OVERLAPPING #-}
         CanWriteField "dstSubresource" VkImageResolve where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageResolve, dstSubresource}

instance {-# OVERLAPPING #-} HasField "dstOffset" VkImageResolve
         where
        type FieldType "dstOffset" VkImageResolve = VkOffset3D
        type FieldOptional "dstOffset" VkImageResolve = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstOffset" VkImageResolve =
             #{offset VkImageResolve, dstOffset}
        type FieldIsArray "dstOffset" VkImageResolve = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageResolve, dstOffset}

instance {-# OVERLAPPING #-}
         CanReadField "dstOffset" VkImageResolve where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageResolve, dstOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageResolve, dstOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "dstOffset" VkImageResolve where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageResolve, dstOffset}

instance {-# OVERLAPPING #-} HasField "extent" VkImageResolve where
        type FieldType "extent" VkImageResolve = VkExtent3D
        type FieldOptional "extent" VkImageResolve = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkImageResolve =
             #{offset VkImageResolve, extent}
        type FieldIsArray "extent" VkImageResolve = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageResolve, extent}

instance {-# OVERLAPPING #-} CanReadField "extent" VkImageResolve
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageResolve, extent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageResolve, extent}

instance {-# OVERLAPPING #-} CanWriteField "extent" VkImageResolve
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageResolve, extent}

instance Show VkImageResolve where
        showsPrec d x
          = showString "VkImageResolve {" .
              showString "srcSubresource = " .
                showsPrec d (getField @"srcSubresource" x) .
                  showString ", " .
                    showString "srcOffset = " .
                      showsPrec d (getField @"srcOffset" x) .
                        showString ", " .
                          showString "dstSubresource = " .
                            showsPrec d (getField @"dstSubresource" x) .
                              showString ", " .
                                showString "dstOffset = " .
                                  showsPrec d (getField @"dstOffset" x) .
                                    showString ", " .
                                      showString "extent = " .
                                        showsPrec d (getField @"extent" x) . showChar '}'
