#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties
       (VkSparseImageFormatProperties(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Base                                            (Addr##,
                                                                      ByteArray##,
                                                                      byteArrayContents##,
                                                                      plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags       (VkImageAspectFlags)
import           Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags (VkSparseImageFormatFlags)
import           Graphics.Vulkan.Types.Struct.VkExtent3D             (VkExtent3D)
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageFormatProperties {
--   >     VkImageAspectFlags     aspectMask;
--   >     VkExtent3D             imageGranularity;
--   >     VkSparseImageFormatFlags flags;
--   > } VkSparseImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageFormatProperties VkSparseImageFormatProperties registry at www.khronos.org>
data VkSparseImageFormatProperties = VkSparseImageFormatProperties## Addr##
                                                                    ByteArray##

instance Eq VkSparseImageFormatProperties where
        (VkSparseImageFormatProperties## a _) ==
          x@(VkSparseImageFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageFormatProperties where
        (VkSparseImageFormatProperties## a _) `compare`
          x@(VkSparseImageFormatProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageFormatProperties where
        sizeOf ~_ = #{size VkSparseImageFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageFormatProperties where
        unsafeAddr (VkSparseImageFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageFormatProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageFormatProperties where
        type StructFields VkSparseImageFormatProperties =
             '["aspectMask", "imageGranularity", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageFormatProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkSparseImageFormatProperties where
        type FieldType "aspectMask" VkSparseImageFormatProperties =
             VkImageAspectFlags
        type FieldOptional "aspectMask" VkSparseImageFormatProperties =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkSparseImageFormatProperties =
             #{offset VkSparseImageFormatProperties, aspectMask}
        type FieldIsArray "aspectMask" VkSparseImageFormatProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties, aspectMask}

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkSparseImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkSparseImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "imageGranularity" VkSparseImageFormatProperties where
        type FieldType "imageGranularity" VkSparseImageFormatProperties =
             VkExtent3D
        type FieldOptional "imageGranularity" VkSparseImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageGranularity" VkSparseImageFormatProperties =
             #{offset VkSparseImageFormatProperties, imageGranularity}
        type FieldIsArray "imageGranularity" VkSparseImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties, imageGranularity}

instance {-# OVERLAPPING #-}
         CanReadField "imageGranularity" VkSparseImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties, imageGranularity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties, imageGranularity}

instance {-# OVERLAPPING #-}
         CanWriteField "imageGranularity" VkSparseImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties, imageGranularity}

instance {-# OVERLAPPING #-}
         HasField "flags" VkSparseImageFormatProperties where
        type FieldType "flags" VkSparseImageFormatProperties =
             VkSparseImageFormatFlags
        type FieldOptional "flags" VkSparseImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSparseImageFormatProperties =
             #{offset VkSparseImageFormatProperties, flags}
        type FieldIsArray "flags" VkSparseImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkSparseImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkSparseImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties, flags}

instance Show VkSparseImageFormatProperties where
        showsPrec d x
          = showString "VkSparseImageFormatProperties {" .
              showString "aspectMask = " .
                showsPrec d (getField @"aspectMask" x) .
                  showString ", " .
                    showString "imageGranularity = " .
                      showsPrec d (getField @"imageGranularity" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) . showChar '}'
