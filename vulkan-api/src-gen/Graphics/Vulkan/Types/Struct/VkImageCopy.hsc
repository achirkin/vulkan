#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageCopy (VkImageCopy(..))
       where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkExtent3D               (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers (VkImageSubresourceLayers)
import           Graphics.Vulkan.Types.Struct.VkOffset3D               (VkOffset3D)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkImageCopy {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffset;
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffset;
--   >     VkExtent3D             extent;
--   > } VkImageCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkImageCopyVkImageCopy registry at www.khronos.org>
data VkImageCopy = VkImageCopy## Addr## ByteArray##

instance Eq VkImageCopy where
        (VkImageCopy## a _) == x@(VkImageCopy## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageCopy where
        (VkImageCopy## a _) `compare` x@(VkImageCopy## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageCopy where
        sizeOf ~_ = #{size VkImageCopy}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageCopy}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageCopy where
        unsafeAddr (VkImageCopy## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageCopy## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageCopy## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageCopy where
        type StructFields VkImageCopy =
             '["srcSubresource", "srcOffset", "dstSubresource", "dstOffset", -- ' closing tick for hsc2hs
               "extent"]
        type CUnionType VkImageCopy = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageCopy = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageCopy = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "srcSubresource" VkImageCopy
         where
        type FieldType "srcSubresource" VkImageCopy =
             VkImageSubresourceLayers
        type FieldOptional "srcSubresource" VkImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcSubresource" VkImageCopy =
             #{offset VkImageCopy, srcSubresource}
        type FieldIsArray "srcSubresource" VkImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCopy, srcSubresource}

instance {-# OVERLAPPING #-}
         CanReadField "srcSubresource" VkImageCopy where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCopy, srcSubresource})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCopy, srcSubresource}

instance {-# OVERLAPPING #-}
         CanWriteField "srcSubresource" VkImageCopy where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCopy, srcSubresource}

instance {-# OVERLAPPING #-} HasField "srcOffset" VkImageCopy where
        type FieldType "srcOffset" VkImageCopy = VkOffset3D
        type FieldOptional "srcOffset" VkImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcOffset" VkImageCopy =
             #{offset VkImageCopy, srcOffset}
        type FieldIsArray "srcOffset" VkImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCopy, srcOffset}

instance {-# OVERLAPPING #-} CanReadField "srcOffset" VkImageCopy
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCopy, srcOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCopy, srcOffset}

instance {-# OVERLAPPING #-} CanWriteField "srcOffset" VkImageCopy
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCopy, srcOffset}

instance {-# OVERLAPPING #-} HasField "dstSubresource" VkImageCopy
         where
        type FieldType "dstSubresource" VkImageCopy =
             VkImageSubresourceLayers
        type FieldOptional "dstSubresource" VkImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstSubresource" VkImageCopy =
             #{offset VkImageCopy, dstSubresource}
        type FieldIsArray "dstSubresource" VkImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCopy, dstSubresource}

instance {-# OVERLAPPING #-}
         CanReadField "dstSubresource" VkImageCopy where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCopy, dstSubresource})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCopy, dstSubresource}

instance {-# OVERLAPPING #-}
         CanWriteField "dstSubresource" VkImageCopy where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCopy, dstSubresource}

instance {-# OVERLAPPING #-} HasField "dstOffset" VkImageCopy where
        type FieldType "dstOffset" VkImageCopy = VkOffset3D
        type FieldOptional "dstOffset" VkImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstOffset" VkImageCopy =
             #{offset VkImageCopy, dstOffset}
        type FieldIsArray "dstOffset" VkImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCopy, dstOffset}

instance {-# OVERLAPPING #-} CanReadField "dstOffset" VkImageCopy
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCopy, dstOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCopy, dstOffset}

instance {-# OVERLAPPING #-} CanWriteField "dstOffset" VkImageCopy
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCopy, dstOffset}

instance {-# OVERLAPPING #-} HasField "extent" VkImageCopy where
        type FieldType "extent" VkImageCopy = VkExtent3D
        type FieldOptional "extent" VkImageCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkImageCopy =
             #{offset VkImageCopy, extent}
        type FieldIsArray "extent" VkImageCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCopy, extent}

instance {-# OVERLAPPING #-} CanReadField "extent" VkImageCopy
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCopy, extent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCopy, extent}

instance {-# OVERLAPPING #-} CanWriteField "extent" VkImageCopy
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCopy, extent}

instance Show VkImageCopy where
        showsPrec d x
          = showString "VkImageCopy {" .
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
