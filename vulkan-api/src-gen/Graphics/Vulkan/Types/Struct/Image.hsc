#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.Image
       (VkImageBlit(..), VkImageCopy(..), VkImageCreateInfo(..),
        VkImageFormatListCreateInfoKHR(..), VkImageFormatProperties(..),
        VkImageFormatProperties2(..), VkImageFormatProperties2KHR,
        VkImageMemoryBarrier(..), VkImageMemoryRequirementsInfo2(..),
        VkImageMemoryRequirementsInfo2KHR,
        VkImagePlaneMemoryRequirementsInfo(..),
        VkImagePlaneMemoryRequirementsInfoKHR, VkImageResolve(..),
        VkImageSparseMemoryRequirementsInfo2(..),
        VkImageSparseMemoryRequirementsInfo2KHR, VkImageSubresource(..),
        VkImageSubresourceLayers(..), VkImageSubresourceRange(..),
        VkImageSwapchainCreateInfoKHR(..), VkImageViewCreateInfo(..),
        VkImageViewUsageCreateInfo(..), VkImageViewUsageCreateInfoKHR)
       where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                Proxy##,
                                                                byteArrayContents##,
                                                                plusAddr##,
                                                                proxy##)
import           GHC.TypeLits                                  (KnownNat,
                                                                natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkDeviceSize)
import           Graphics.Vulkan.Types.Bitmasks                (VkImageViewCreateFlags)
import           Graphics.Vulkan.Types.Enum.AccessFlags        (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.Format             (VkFormat)
import           Graphics.Vulkan.Types.Enum.Image              (VkImageAspectFlagBits,
                                                                VkImageAspectFlags,
                                                                VkImageCreateFlags,
                                                                VkImageLayout,
                                                                VkImageTiling,
                                                                VkImageType,
                                                                VkImageUsageFlags,
                                                                VkImageViewType)
import           Graphics.Vulkan.Types.Enum.SampleCountFlags   (VkSampleCountFlagBits,
                                                                VkSampleCountFlags)
import           Graphics.Vulkan.Types.Enum.SharingMode        (VkSharingMode)
import           Graphics.Vulkan.Types.Enum.StructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                 (VkImage,
                                                                VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.ComponentMapping (VkComponentMapping)
import           Graphics.Vulkan.Types.Struct.Extent           (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.Offset           (VkOffset3D)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkImageBlit {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffsets[2];
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffsets[2];
--   > } VkImageBlit;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageBlit VkImageBlit registry at www.khronos.org>
data VkImageBlit = VkImageBlit## Addr## ByteArray##

instance Eq VkImageBlit where
        (VkImageBlit## a _) == x@(VkImageBlit## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageBlit where
        (VkImageBlit## a _) `compare` x@(VkImageBlit## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageBlit where
        sizeOf ~_ = #{size VkImageBlit}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageBlit}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageBlit where
        unsafeAddr (VkImageBlit## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageBlit## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageBlit## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageBlit where
        type StructFields VkImageBlit =
             '["srcSubresource", "srcOffsets", "dstSubresource", "dstOffsets"] -- ' closing tick for hsc2hs
        type CUnionType VkImageBlit = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageBlit = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageBlit = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "srcSubresource" VkImageBlit
         where
        type FieldType "srcSubresource" VkImageBlit =
             VkImageSubresourceLayers
        type FieldOptional "srcSubresource" VkImageBlit = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcSubresource" VkImageBlit =
             #{offset VkImageBlit, srcSubresource}
        type FieldIsArray "srcSubresource" VkImageBlit = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageBlit, srcSubresource}

instance {-# OVERLAPPING #-}
         CanReadField "srcSubresource" VkImageBlit where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageBlit, srcSubresource})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageBlit, srcSubresource}

instance {-# OVERLAPPING #-}
         CanWriteField "srcSubresource" VkImageBlit where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageBlit, srcSubresource}

instance {-# OVERLAPPING #-} HasField "srcOffsets" VkImageBlit
         where
        type FieldType "srcOffsets" VkImageBlit = VkOffset3D
        type FieldOptional "srcOffsets" VkImageBlit = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcOffsets" VkImageBlit =
             #{offset VkImageBlit, srcOffsets}
        type FieldIsArray "srcOffsets" VkImageBlit = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageBlit, srcOffsets}

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "srcOffsets" idx VkImageBlit) =>
         CanReadFieldArray "srcOffsets" idx VkImageBlit
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "srcOffsets" 0 VkImageBlit #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "srcOffsets" 1 VkImageBlit #-}
        type FieldArrayLength "srcOffsets" VkImageBlit = 2

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 2

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkImageBlit, srcOffsets} +
                      sizeOf (undefined :: VkOffset3D) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkImageBlit, srcOffsets} +
                 sizeOf (undefined :: VkOffset3D) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "srcOffsets" idx VkImageBlit) =>
         CanWriteFieldArray "srcOffsets" idx VkImageBlit
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "srcOffsets" 0 VkImageBlit #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "srcOffsets" 1 VkImageBlit #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkImageBlit, srcOffsets} +
                 sizeOf (undefined :: VkOffset3D) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "dstSubresource" VkImageBlit
         where
        type FieldType "dstSubresource" VkImageBlit =
             VkImageSubresourceLayers
        type FieldOptional "dstSubresource" VkImageBlit = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstSubresource" VkImageBlit =
             #{offset VkImageBlit, dstSubresource}
        type FieldIsArray "dstSubresource" VkImageBlit = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageBlit, dstSubresource}

instance {-# OVERLAPPING #-}
         CanReadField "dstSubresource" VkImageBlit where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageBlit, dstSubresource})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageBlit, dstSubresource}

instance {-# OVERLAPPING #-}
         CanWriteField "dstSubresource" VkImageBlit where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageBlit, dstSubresource}

instance {-# OVERLAPPING #-} HasField "dstOffsets" VkImageBlit
         where
        type FieldType "dstOffsets" VkImageBlit = VkOffset3D
        type FieldOptional "dstOffsets" VkImageBlit = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstOffsets" VkImageBlit =
             #{offset VkImageBlit, dstOffsets}
        type FieldIsArray "dstOffsets" VkImageBlit = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageBlit, dstOffsets}

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "dstOffsets" idx VkImageBlit) =>
         CanReadFieldArray "dstOffsets" idx VkImageBlit
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "dstOffsets" 0 VkImageBlit #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "dstOffsets" 1 VkImageBlit #-}
        type FieldArrayLength "dstOffsets" VkImageBlit = 2

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 2

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkImageBlit, dstOffsets} +
                      sizeOf (undefined :: VkOffset3D) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkImageBlit, dstOffsets} +
                 sizeOf (undefined :: VkOffset3D) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "dstOffsets" idx VkImageBlit) =>
         CanWriteFieldArray "dstOffsets" idx VkImageBlit
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "dstOffsets" 0 VkImageBlit #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "dstOffsets" 1 VkImageBlit #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkImageBlit, dstOffsets} +
                 sizeOf (undefined :: VkOffset3D) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance Show VkImageBlit where
        showsPrec d x
          = showString "VkImageBlit {" .
              showString "srcSubresource = " .
                showsPrec d (getField @"srcSubresource" x) .
                  showString ", " .
                    (showString "srcOffsets = [" .
                       showsPrec d
                         (let s = sizeOf (undefined :: FieldType "srcOffsets" VkImageBlit)
                              o = fieldOffset @"srcOffsets" @VkImageBlit
                              f i
                                = peekByteOff (unsafePtr x) i ::
                                    IO (FieldType "srcOffsets" VkImageBlit)
                            in
                            unsafeDupablePerformIO . mapM f $
                              map (\ i -> o + i * s) [0 .. 2 - 1])
                         . showChar ']')
                      .
                      showString ", " .
                        showString "dstSubresource = " .
                          showsPrec d (getField @"dstSubresource" x) .
                            showString ", " .
                              (showString "dstOffsets = [" .
                                 showsPrec d
                                   (let s = sizeOf (undefined :: FieldType "dstOffsets" VkImageBlit)
                                        o = fieldOffset @"dstOffsets" @VkImageBlit
                                        f i
                                          = peekByteOff (unsafePtr x) i ::
                                              IO (FieldType "dstOffsets" VkImageBlit)
                                      in
                                      unsafeDupablePerformIO . mapM f $
                                        map (\ i -> o + i * s) [0 .. 2 - 1])
                                   . showChar ']')
                                . showChar '}'

-- | > typedef struct VkImageCopy {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffset;
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffset;
--   >     VkExtent3D             extent;
--   > } VkImageCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageCopy VkImageCopy registry at www.khronos.org>
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

-- | > typedef struct VkImageCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkImageCreateFlags     flags;
--   >     VkImageType            imageType;
--   >     VkFormat               format;
--   >     VkExtent3D             extent;
--   >     uint32_t               mipLevels;
--   >     uint32_t               arrayLayers;
--   >     VkSampleCountFlagBits  samples;
--   >     VkImageTiling          tiling;
--   >     VkImageUsageFlags      usage;
--   >     VkSharingMode          sharingMode;
--   >     uint32_t               queueFamilyIndexCount;
--   >     const uint32_t*        pQueueFamilyIndices;
--   >     VkImageLayout          initialLayout;
--   > } VkImageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageCreateInfo VkImageCreateInfo registry at www.khronos.org>
data VkImageCreateInfo = VkImageCreateInfo## Addr## ByteArray##

instance Eq VkImageCreateInfo where
        (VkImageCreateInfo## a _) == x@(VkImageCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageCreateInfo where
        (VkImageCreateInfo## a _) `compare` x@(VkImageCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageCreateInfo where
        sizeOf ~_ = #{size VkImageCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageCreateInfo where
        unsafeAddr (VkImageCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageCreateInfo where
        type StructFields VkImageCreateInfo =
             '["sType", "pNext", "flags", "imageType", "format", "extent", -- ' closing tick for hsc2hs
               "mipLevels", "arrayLayers", "samples", "tiling", "usage",
               "sharingMode", "queueFamilyIndexCount", "pQueueFamilyIndices",
               "initialLayout"]
        type CUnionType VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkImageCreateInfo
         where
        type FieldType "sType" VkImageCreateInfo = VkStructureType
        type FieldOptional "sType" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageCreateInfo =
             #{offset VkImageCreateInfo, sType}
        type FieldIsArray "sType" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, sType}

instance {-# OVERLAPPING #-} CanReadField "sType" VkImageCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkImageCreateInfo
         where
        type FieldType "pNext" VkImageCreateInfo = Ptr Void
        type FieldOptional "pNext" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageCreateInfo =
             #{offset VkImageCreateInfo, pNext}
        type FieldIsArray "pNext" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, pNext}

instance {-# OVERLAPPING #-} CanReadField "pNext" VkImageCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkImageCreateInfo
         where
        type FieldType "flags" VkImageCreateInfo = VkImageCreateFlags
        type FieldOptional "flags" VkImageCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImageCreateInfo =
             #{offset VkImageCreateInfo, flags}
        type FieldIsArray "flags" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, flags}

instance {-# OVERLAPPING #-} CanReadField "flags" VkImageCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, flags}

instance {-# OVERLAPPING #-} HasField "imageType" VkImageCreateInfo
         where
        type FieldType "imageType" VkImageCreateInfo = VkImageType
        type FieldOptional "imageType" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageType" VkImageCreateInfo =
             #{offset VkImageCreateInfo, imageType}
        type FieldIsArray "imageType" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, imageType}

instance {-# OVERLAPPING #-}
         CanReadField "imageType" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, imageType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, imageType}

instance {-# OVERLAPPING #-}
         CanWriteField "imageType" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, imageType}

instance {-# OVERLAPPING #-} HasField "format" VkImageCreateInfo
         where
        type FieldType "format" VkImageCreateInfo = VkFormat
        type FieldOptional "format" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkImageCreateInfo =
             #{offset VkImageCreateInfo, format}
        type FieldIsArray "format" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, format}

instance {-# OVERLAPPING #-} HasField "extent" VkImageCreateInfo
         where
        type FieldType "extent" VkImageCreateInfo = VkExtent3D
        type FieldOptional "extent" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkImageCreateInfo =
             #{offset VkImageCreateInfo, extent}
        type FieldIsArray "extent" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, extent}

instance {-# OVERLAPPING #-}
         CanReadField "extent" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, extent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, extent}

instance {-# OVERLAPPING #-}
         CanWriteField "extent" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, extent}

instance {-# OVERLAPPING #-} HasField "mipLevels" VkImageCreateInfo
         where
        type FieldType "mipLevels" VkImageCreateInfo = Word32
        type FieldOptional "mipLevels" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mipLevels" VkImageCreateInfo =
             #{offset VkImageCreateInfo, mipLevels}
        type FieldIsArray "mipLevels" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, mipLevels}

instance {-# OVERLAPPING #-}
         CanReadField "mipLevels" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, mipLevels})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, mipLevels}

instance {-# OVERLAPPING #-}
         CanWriteField "mipLevels" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, mipLevels}

instance {-# OVERLAPPING #-}
         HasField "arrayLayers" VkImageCreateInfo where
        type FieldType "arrayLayers" VkImageCreateInfo = Word32
        type FieldOptional "arrayLayers" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "arrayLayers" VkImageCreateInfo =
             #{offset VkImageCreateInfo, arrayLayers}
        type FieldIsArray "arrayLayers" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, arrayLayers}

instance {-# OVERLAPPING #-}
         CanReadField "arrayLayers" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, arrayLayers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, arrayLayers}

instance {-# OVERLAPPING #-}
         CanWriteField "arrayLayers" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, arrayLayers}

instance {-# OVERLAPPING #-} HasField "samples" VkImageCreateInfo
         where
        type FieldType "samples" VkImageCreateInfo = VkSampleCountFlagBits
        type FieldOptional "samples" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "samples" VkImageCreateInfo =
             #{offset VkImageCreateInfo, samples}
        type FieldIsArray "samples" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, samples}

instance {-# OVERLAPPING #-}
         CanReadField "samples" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, samples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, samples}

instance {-# OVERLAPPING #-}
         CanWriteField "samples" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, samples}

instance {-# OVERLAPPING #-} HasField "tiling" VkImageCreateInfo
         where
        type FieldType "tiling" VkImageCreateInfo = VkImageTiling
        type FieldOptional "tiling" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tiling" VkImageCreateInfo =
             #{offset VkImageCreateInfo, tiling}
        type FieldIsArray "tiling" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, tiling}

instance {-# OVERLAPPING #-}
         CanReadField "tiling" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, tiling})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, tiling}

instance {-# OVERLAPPING #-}
         CanWriteField "tiling" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, tiling}

instance {-# OVERLAPPING #-} HasField "usage" VkImageCreateInfo
         where
        type FieldType "usage" VkImageCreateInfo = VkImageUsageFlags
        type FieldOptional "usage" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkImageCreateInfo =
             #{offset VkImageCreateInfo, usage}
        type FieldIsArray "usage" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, usage}

instance {-# OVERLAPPING #-} CanReadField "usage" VkImageCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, usage}

instance {-# OVERLAPPING #-}
         HasField "sharingMode" VkImageCreateInfo where
        type FieldType "sharingMode" VkImageCreateInfo = VkSharingMode
        type FieldOptional "sharingMode" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sharingMode" VkImageCreateInfo =
             #{offset VkImageCreateInfo, sharingMode}
        type FieldIsArray "sharingMode" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageCreateInfo, sharingMode}

instance {-# OVERLAPPING #-}
         CanReadField "sharingMode" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, sharingMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, sharingMode}

instance {-# OVERLAPPING #-}
         CanWriteField "sharingMode" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, sharingMode}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndexCount" VkImageCreateInfo where
        type FieldType "queueFamilyIndexCount" VkImageCreateInfo = Word32
        type FieldOptional "queueFamilyIndexCount" VkImageCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndexCount" VkImageCreateInfo =
             #{offset VkImageCreateInfo, queueFamilyIndexCount}
        type FieldIsArray "queueFamilyIndexCount" VkImageCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageCreateInfo, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyIndexCount" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, queueFamilyIndexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyIndexCount" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         HasField "pQueueFamilyIndices" VkImageCreateInfo where
        type FieldType "pQueueFamilyIndices" VkImageCreateInfo = Ptr Word32
        type FieldOptional "pQueueFamilyIndices" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pQueueFamilyIndices" VkImageCreateInfo =
             #{offset VkImageCreateInfo, pQueueFamilyIndices}
        type FieldIsArray "pQueueFamilyIndices" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageCreateInfo, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pQueueFamilyIndices" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, pQueueFamilyIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pQueueFamilyIndices" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         HasField "initialLayout" VkImageCreateInfo where
        type FieldType "initialLayout" VkImageCreateInfo = VkImageLayout
        type FieldOptional "initialLayout" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "initialLayout" VkImageCreateInfo =
             #{offset VkImageCreateInfo, initialLayout}
        type FieldIsArray "initialLayout" VkImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageCreateInfo, initialLayout}

instance {-# OVERLAPPING #-}
         CanReadField "initialLayout" VkImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageCreateInfo, initialLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageCreateInfo, initialLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "initialLayout" VkImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageCreateInfo, initialLayout}

instance Show VkImageCreateInfo where
        showsPrec d x
          = showString "VkImageCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "imageType = " .
                                  showsPrec d (getField @"imageType" x) .
                                    showString ", " .
                                      showString "format = " .
                                        showsPrec d (getField @"format" x) .
                                          showString ", " .
                                            showString "extent = " .
                                              showsPrec d (getField @"extent" x) .
                                                showString ", " .
                                                  showString "mipLevels = " .
                                                    showsPrec d (getField @"mipLevels" x) .
                                                      showString ", " .
                                                        showString "arrayLayers = " .
                                                          showsPrec d (getField @"arrayLayers" x) .
                                                            showString ", " .
                                                              showString "samples = " .
                                                                showsPrec d (getField @"samples" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "tiling = " .
                                                                      showsPrec d
                                                                        (getField @"tiling" x)
                                                                        .
                                                                        showString ", " .
                                                                          showString "usage = " .
                                                                            showsPrec d
                                                                              (getField @"usage" x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "sharingMode = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"sharingMode"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "queueFamilyIndexCount = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"queueFamilyIndexCount"
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "pQueueFamilyIndices = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (getField
                                                                                                   @"pQueueFamilyIndices"
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "initialLayout = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (getField
                                                                                                         @"initialLayout"
                                                                                                         x)
                                                                                                      .
                                                                                                      showChar
                                                                                                        '}'

-- | > typedef struct VkImageFormatListCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               viewFormatCount;
--   >     const VkFormat*      pViewFormats;
--   > } VkImageFormatListCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageFormatListCreateInfoKHR VkImageFormatListCreateInfoKHR registry at www.khronos.org>
data VkImageFormatListCreateInfoKHR = VkImageFormatListCreateInfoKHR## Addr##
                                                                      ByteArray##

instance Eq VkImageFormatListCreateInfoKHR where
        (VkImageFormatListCreateInfoKHR## a _) ==
          x@(VkImageFormatListCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageFormatListCreateInfoKHR where
        (VkImageFormatListCreateInfoKHR## a _) `compare`
          x@(VkImageFormatListCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageFormatListCreateInfoKHR where
        sizeOf ~_ = #{size VkImageFormatListCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageFormatListCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageFormatListCreateInfoKHR where
        unsafeAddr (VkImageFormatListCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageFormatListCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageFormatListCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageFormatListCreateInfoKHR where
        type StructFields VkImageFormatListCreateInfoKHR =
             '["sType", "pNext", "viewFormatCount", "pViewFormats"] -- ' closing tick for hsc2hs
        type CUnionType VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageFormatListCreateInfoKHR =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageFormatListCreateInfoKHR where
        type FieldType "sType" VkImageFormatListCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, sType}
        type FieldIsArray "sType" VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageFormatListCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageFormatListCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageFormatListCreateInfoKHR where
        type FieldType "pNext" VkImageFormatListCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageFormatListCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageFormatListCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "viewFormatCount" VkImageFormatListCreateInfoKHR where
        type FieldType "viewFormatCount" VkImageFormatListCreateInfoKHR =
             Word32
        type FieldOptional "viewFormatCount" VkImageFormatListCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "viewFormatCount" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}
        type FieldIsArray "viewFormatCount" VkImageFormatListCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

instance {-# OVERLAPPING #-}
         CanReadField "viewFormatCount" VkImageFormatListCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, viewFormatCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

instance {-# OVERLAPPING #-}
         CanWriteField "viewFormatCount" VkImageFormatListCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

instance {-# OVERLAPPING #-}
         HasField "pViewFormats" VkImageFormatListCreateInfoKHR where
        type FieldType "pViewFormats" VkImageFormatListCreateInfoKHR =
             Ptr VkFormat
        type FieldOptional "pViewFormats" VkImageFormatListCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewFormats" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, pViewFormats}
        type FieldIsArray "pViewFormats" VkImageFormatListCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

instance {-# OVERLAPPING #-}
         CanReadField "pViewFormats" VkImageFormatListCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, pViewFormats})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewFormats" VkImageFormatListCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

instance Show VkImageFormatListCreateInfoKHR where
        showsPrec d x
          = showString "VkImageFormatListCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "viewFormatCount = " .
                            showsPrec d (getField @"viewFormatCount" x) .
                              showString ", " .
                                showString "pViewFormats = " .
                                  showsPrec d (getField @"pViewFormats" x) . showChar '}'

-- | > typedef struct VkImageFormatProperties {
--   >     VkExtent3D             maxExtent;
--   >     uint32_t               maxMipLevels;
--   >     uint32_t               maxArrayLayers;
--   >     VkSampleCountFlags     sampleCounts;
--   >     VkDeviceSize           maxResourceSize;
--   > } VkImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageFormatProperties VkImageFormatProperties registry at www.khronos.org>
data VkImageFormatProperties = VkImageFormatProperties## Addr##
                                                        ByteArray##

instance Eq VkImageFormatProperties where
        (VkImageFormatProperties## a _) == x@(VkImageFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageFormatProperties where
        (VkImageFormatProperties## a _) `compare`
          x@(VkImageFormatProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageFormatProperties where
        sizeOf ~_ = #{size VkImageFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageFormatProperties where
        unsafeAddr (VkImageFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageFormatProperties## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageFormatProperties where
        type StructFields VkImageFormatProperties =
             '["maxExtent", "maxMipLevels", "maxArrayLayers", "sampleCounts", -- ' closing tick for hsc2hs
               "maxResourceSize"]
        type CUnionType VkImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkImageFormatProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "maxExtent" VkImageFormatProperties where
        type FieldType "maxExtent" VkImageFormatProperties = VkExtent3D
        type FieldOptional "maxExtent" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxExtent" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxExtent}
        type FieldIsArray "maxExtent" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxExtent}

instance {-# OVERLAPPING #-}
         CanReadField "maxExtent" VkImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties, maxExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "maxExtent" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties, maxExtent}

instance {-# OVERLAPPING #-}
         HasField "maxMipLevels" VkImageFormatProperties where
        type FieldType "maxMipLevels" VkImageFormatProperties = Word32
        type FieldOptional "maxMipLevels" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMipLevels" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxMipLevels}
        type FieldIsArray "maxMipLevels" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxMipLevels}

instance {-# OVERLAPPING #-}
         CanReadField "maxMipLevels" VkImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxMipLevels})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties, maxMipLevels}

instance {-# OVERLAPPING #-}
         CanWriteField "maxMipLevels" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties, maxMipLevels}

instance {-# OVERLAPPING #-}
         HasField "maxArrayLayers" VkImageFormatProperties where
        type FieldType "maxArrayLayers" VkImageFormatProperties = Word32
        type FieldOptional "maxArrayLayers" VkImageFormatProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxArrayLayers" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxArrayLayers}
        type FieldIsArray "maxArrayLayers" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxArrayLayers}

instance {-# OVERLAPPING #-}
         CanReadField "maxArrayLayers" VkImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxArrayLayers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties, maxArrayLayers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxArrayLayers" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties, maxArrayLayers}

instance {-# OVERLAPPING #-}
         HasField "sampleCounts" VkImageFormatProperties where
        type FieldType "sampleCounts" VkImageFormatProperties =
             VkSampleCountFlags
        type FieldOptional "sampleCounts" VkImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sampleCounts" VkImageFormatProperties =
             #{offset VkImageFormatProperties, sampleCounts}
        type FieldIsArray "sampleCounts" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, sampleCounts}

instance {-# OVERLAPPING #-}
         CanReadField "sampleCounts" VkImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, sampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties, sampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleCounts" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties, sampleCounts}

instance {-# OVERLAPPING #-}
         HasField "maxResourceSize" VkImageFormatProperties where
        type FieldType "maxResourceSize" VkImageFormatProperties =
             VkDeviceSize
        type FieldOptional "maxResourceSize" VkImageFormatProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxResourceSize" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxResourceSize}
        type FieldIsArray "maxResourceSize" VkImageFormatProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxResourceSize}

instance {-# OVERLAPPING #-}
         CanReadField "maxResourceSize" VkImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxResourceSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties, maxResourceSize}

instance {-# OVERLAPPING #-}
         CanWriteField "maxResourceSize" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties, maxResourceSize}

instance Show VkImageFormatProperties where
        showsPrec d x
          = showString "VkImageFormatProperties {" .
              showString "maxExtent = " .
                showsPrec d (getField @"maxExtent" x) .
                  showString ", " .
                    showString "maxMipLevels = " .
                      showsPrec d (getField @"maxMipLevels" x) .
                        showString ", " .
                          showString "maxArrayLayers = " .
                            showsPrec d (getField @"maxArrayLayers" x) .
                              showString ", " .
                                showString "sampleCounts = " .
                                  showsPrec d (getField @"sampleCounts" x) .
                                    showString ", " .
                                      showString "maxResourceSize = " .
                                        showsPrec d (getField @"maxResourceSize" x) . showChar '}'

-- | > typedef struct VkImageFormatProperties2 {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkImageFormatProperties          imageFormatProperties;
--   > } VkImageFormatProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageFormatProperties2 VkImageFormatProperties2 registry at www.khronos.org>
data VkImageFormatProperties2 = VkImageFormatProperties2## Addr##
                                                          ByteArray##

instance Eq VkImageFormatProperties2 where
        (VkImageFormatProperties2## a _) ==
          x@(VkImageFormatProperties2## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageFormatProperties2 where
        (VkImageFormatProperties2## a _) `compare`
          x@(VkImageFormatProperties2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageFormatProperties2 where
        sizeOf ~_ = #{size VkImageFormatProperties2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageFormatProperties2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageFormatProperties2 where
        unsafeAddr (VkImageFormatProperties2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageFormatProperties2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageFormatProperties2## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageFormatProperties2 where
        type StructFields VkImageFormatProperties2 =
             '["sType", "pNext", "imageFormatProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageFormatProperties2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkImageFormatProperties2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageFormatProperties2 where
        type FieldType "sType" VkImageFormatProperties2 = VkStructureType
        type FieldOptional "sType" VkImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageFormatProperties2 =
             #{offset VkImageFormatProperties2, sType}
        type FieldIsArray "sType" VkImageFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageFormatProperties2 where
        type FieldType "pNext" VkImageFormatProperties2 = Ptr Void
        type FieldOptional "pNext" VkImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageFormatProperties2 =
             #{offset VkImageFormatProperties2, pNext}
        type FieldIsArray "pNext" VkImageFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         HasField "imageFormatProperties" VkImageFormatProperties2 where
        type FieldType "imageFormatProperties" VkImageFormatProperties2 =
             VkImageFormatProperties
        type FieldOptional "imageFormatProperties" VkImageFormatProperties2
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageFormatProperties" VkImageFormatProperties2 =
             #{offset VkImageFormatProperties2, imageFormatProperties}
        type FieldIsArray "imageFormatProperties" VkImageFormatProperties2
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties2, imageFormatProperties}

instance {-# OVERLAPPING #-}
         CanReadField "imageFormatProperties" VkImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2, imageFormatProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties2, imageFormatProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "imageFormatProperties" VkImageFormatProperties2
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties2, imageFormatProperties}

instance Show VkImageFormatProperties2 where
        showsPrec d x
          = showString "VkImageFormatProperties2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "imageFormatProperties = " .
                            showsPrec d (getField @"imageFormatProperties" x) . showChar '}'

-- | Alias for `VkImageFormatProperties2`
type VkImageFormatProperties2KHR = VkImageFormatProperties2

-- | > typedef struct VkImageMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   >     VkImageLayout          oldLayout;
--   >     VkImageLayout          newLayout;
--   >     uint32_t               srcQueueFamilyIndex;
--   >     uint32_t               dstQueueFamilyIndex;
--   >     VkImage                image;
--   >     VkImageSubresourceRange subresourceRange;
--   > } VkImageMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageMemoryBarrier VkImageMemoryBarrier registry at www.khronos.org>
data VkImageMemoryBarrier = VkImageMemoryBarrier## Addr## ByteArray##

instance Eq VkImageMemoryBarrier where
        (VkImageMemoryBarrier## a _) == x@(VkImageMemoryBarrier## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageMemoryBarrier where
        (VkImageMemoryBarrier## a _) `compare` x@(VkImageMemoryBarrier## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageMemoryBarrier where
        sizeOf ~_ = #{size VkImageMemoryBarrier}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageMemoryBarrier}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageMemoryBarrier where
        unsafeAddr (VkImageMemoryBarrier## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageMemoryBarrier## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageMemoryBarrier## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageMemoryBarrier where
        type StructFields VkImageMemoryBarrier =
             '["sType", "pNext", "srcAccessMask", "dstAccessMask", "oldLayout", -- ' closing tick for hsc2hs
               "newLayout", "srcQueueFamilyIndex", "dstQueueFamilyIndex", "image",
               "subresourceRange"]
        type CUnionType VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageMemoryBarrier = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkImageMemoryBarrier
         where
        type FieldType "sType" VkImageMemoryBarrier = VkStructureType
        type FieldOptional "sType" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, sType}
        type FieldIsArray "sType" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageMemoryBarrier, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkImageMemoryBarrier
         where
        type FieldType "pNext" VkImageMemoryBarrier = Ptr Void
        type FieldOptional "pNext" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, pNext}
        type FieldIsArray "pNext" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         HasField "srcAccessMask" VkImageMemoryBarrier where
        type FieldType "srcAccessMask" VkImageMemoryBarrier = VkAccessFlags
        type FieldOptional "srcAccessMask" VkImageMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "srcAccessMask" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, srcAccessMask}
        type FieldIsArray "srcAccessMask" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "srcAccessMask" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, srcAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "srcAccessMask" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dstAccessMask" VkImageMemoryBarrier where
        type FieldType "dstAccessMask" VkImageMemoryBarrier = VkAccessFlags
        type FieldOptional "dstAccessMask" VkImageMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dstAccessMask" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, dstAccessMask}
        type FieldIsArray "dstAccessMask" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "dstAccessMask" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, dstAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "dstAccessMask" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         HasField "oldLayout" VkImageMemoryBarrier where
        type FieldType "oldLayout" VkImageMemoryBarrier = VkImageLayout
        type FieldOptional "oldLayout" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "oldLayout" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, oldLayout}
        type FieldIsArray "oldLayout" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageMemoryBarrier, oldLayout}

instance {-# OVERLAPPING #-}
         CanReadField "oldLayout" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, oldLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, oldLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "oldLayout" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, oldLayout}

instance {-# OVERLAPPING #-}
         HasField "newLayout" VkImageMemoryBarrier where
        type FieldType "newLayout" VkImageMemoryBarrier = VkImageLayout
        type FieldOptional "newLayout" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "newLayout" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, newLayout}
        type FieldIsArray "newLayout" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageMemoryBarrier, newLayout}

instance {-# OVERLAPPING #-}
         CanReadField "newLayout" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, newLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, newLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "newLayout" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, newLayout}

instance {-# OVERLAPPING #-}
         HasField "srcQueueFamilyIndex" VkImageMemoryBarrier where
        type FieldType "srcQueueFamilyIndex" VkImageMemoryBarrier = Word32
        type FieldOptional "srcQueueFamilyIndex" VkImageMemoryBarrier =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "srcQueueFamilyIndex" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}
        type FieldIsArray "srcQueueFamilyIndex" VkImageMemoryBarrier =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "srcQueueFamilyIndex" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, srcQueueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "srcQueueFamilyIndex" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "dstQueueFamilyIndex" VkImageMemoryBarrier where
        type FieldType "dstQueueFamilyIndex" VkImageMemoryBarrier = Word32
        type FieldOptional "dstQueueFamilyIndex" VkImageMemoryBarrier =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dstQueueFamilyIndex" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}
        type FieldIsArray "dstQueueFamilyIndex" VkImageMemoryBarrier =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "dstQueueFamilyIndex" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, dstQueueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "dstQueueFamilyIndex" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-} HasField "image" VkImageMemoryBarrier
         where
        type FieldType "image" VkImageMemoryBarrier = VkImage
        type FieldOptional "image" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, image}
        type FieldIsArray "image" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageMemoryBarrier, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, image}

instance {-# OVERLAPPING #-}
         HasField "subresourceRange" VkImageMemoryBarrier where
        type FieldType "subresourceRange" VkImageMemoryBarrier =
             VkImageSubresourceRange
        type FieldOptional "subresourceRange" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subresourceRange" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, subresourceRange}
        type FieldIsArray "subresourceRange" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryBarrier, subresourceRange}

instance {-# OVERLAPPING #-}
         CanReadField "subresourceRange" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, subresourceRange})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, subresourceRange}

instance {-# OVERLAPPING #-}
         CanWriteField "subresourceRange" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, subresourceRange}

instance Show VkImageMemoryBarrier where
        showsPrec d x
          = showString "VkImageMemoryBarrier {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "srcAccessMask = " .
                            showsPrec d (getField @"srcAccessMask" x) .
                              showString ", " .
                                showString "dstAccessMask = " .
                                  showsPrec d (getField @"dstAccessMask" x) .
                                    showString ", " .
                                      showString "oldLayout = " .
                                        showsPrec d (getField @"oldLayout" x) .
                                          showString ", " .
                                            showString "newLayout = " .
                                              showsPrec d (getField @"newLayout" x) .
                                                showString ", " .
                                                  showString "srcQueueFamilyIndex = " .
                                                    showsPrec d (getField @"srcQueueFamilyIndex" x)
                                                      .
                                                      showString ", " .
                                                        showString "dstQueueFamilyIndex = " .
                                                          showsPrec d
                                                            (getField @"dstQueueFamilyIndex" x)
                                                            .
                                                            showString ", " .
                                                              showString "image = " .
                                                                showsPrec d (getField @"image" x) .
                                                                  showString ", " .
                                                                    showString "subresourceRange = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"subresourceRange"
                                                                           x)
                                                                        . showChar '}'

-- | > typedef struct VkImageMemoryRequirementsInfo2 {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageMemoryRequirementsInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageMemoryRequirementsInfo2 VkImageMemoryRequirementsInfo2 registry at www.khronos.org>
data VkImageMemoryRequirementsInfo2 = VkImageMemoryRequirementsInfo2## Addr##
                                                                      ByteArray##

instance Eq VkImageMemoryRequirementsInfo2 where
        (VkImageMemoryRequirementsInfo2## a _) ==
          x@(VkImageMemoryRequirementsInfo2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageMemoryRequirementsInfo2 where
        (VkImageMemoryRequirementsInfo2## a _) `compare`
          x@(VkImageMemoryRequirementsInfo2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageMemoryRequirementsInfo2 where
        sizeOf ~_ = #{size VkImageMemoryRequirementsInfo2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageMemoryRequirementsInfo2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageMemoryRequirementsInfo2 where
        unsafeAddr (VkImageMemoryRequirementsInfo2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageMemoryRequirementsInfo2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageMemoryRequirementsInfo2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageMemoryRequirementsInfo2 where
        type StructFields VkImageMemoryRequirementsInfo2 =
             '["sType", "pNext", "image"] -- ' closing tick for hsc2hs
        type CUnionType VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageMemoryRequirementsInfo2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageMemoryRequirementsInfo2 where
        type FieldType "sType" VkImageMemoryRequirementsInfo2 =
             VkStructureType
        type FieldOptional "sType" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageMemoryRequirementsInfo2 =
             #{offset VkImageMemoryRequirementsInfo2, sType}
        type FieldIsArray "sType" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageMemoryRequirementsInfo2 where
        type FieldType "pNext" VkImageMemoryRequirementsInfo2 = Ptr Void
        type FieldOptional "pNext" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageMemoryRequirementsInfo2 =
             #{offset VkImageMemoryRequirementsInfo2, pNext}
        type FieldIsArray "pNext" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         HasField "image" VkImageMemoryRequirementsInfo2 where
        type FieldType "image" VkImageMemoryRequirementsInfo2 = VkImage
        type FieldOptional "image" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageMemoryRequirementsInfo2 =
             #{offset VkImageMemoryRequirementsInfo2, image}
        type FieldIsArray "image" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkImageMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkImageMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2, image}

instance Show VkImageMemoryRequirementsInfo2 where
        showsPrec d x
          = showString "VkImageMemoryRequirementsInfo2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "image = " .
                            showsPrec d (getField @"image" x) . showChar '}'

-- | Alias for `VkImageMemoryRequirementsInfo2`
type VkImageMemoryRequirementsInfo2KHR =
     VkImageMemoryRequirementsInfo2

-- | > typedef struct VkImagePlaneMemoryRequirementsInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImageAspectFlagBits            planeAspect;
--   > } VkImagePlaneMemoryRequirementsInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImagePlaneMemoryRequirementsInfo VkImagePlaneMemoryRequirementsInfo registry at www.khronos.org>
data VkImagePlaneMemoryRequirementsInfo = VkImagePlaneMemoryRequirementsInfo## Addr##
                                                                              ByteArray##

instance Eq VkImagePlaneMemoryRequirementsInfo where
        (VkImagePlaneMemoryRequirementsInfo## a _) ==
          x@(VkImagePlaneMemoryRequirementsInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImagePlaneMemoryRequirementsInfo where
        (VkImagePlaneMemoryRequirementsInfo## a _) `compare`
          x@(VkImagePlaneMemoryRequirementsInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImagePlaneMemoryRequirementsInfo where
        sizeOf ~_ = #{size VkImagePlaneMemoryRequirementsInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImagePlaneMemoryRequirementsInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImagePlaneMemoryRequirementsInfo where
        unsafeAddr (VkImagePlaneMemoryRequirementsInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImagePlaneMemoryRequirementsInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImagePlaneMemoryRequirementsInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImagePlaneMemoryRequirementsInfo where
        type StructFields VkImagePlaneMemoryRequirementsInfo =
             '["sType", "pNext", "planeAspect"] -- ' closing tick for hsc2hs
        type CUnionType VkImagePlaneMemoryRequirementsInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImagePlaneMemoryRequirementsInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImagePlaneMemoryRequirementsInfo =
             '[VkImageMemoryRequirementsInfo2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImagePlaneMemoryRequirementsInfo where
        type FieldType "sType" VkImagePlaneMemoryRequirementsInfo =
             VkStructureType
        type FieldOptional "sType" VkImagePlaneMemoryRequirementsInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImagePlaneMemoryRequirementsInfo =
             #{offset VkImagePlaneMemoryRequirementsInfo, sType}
        type FieldIsArray "sType" VkImagePlaneMemoryRequirementsInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImagePlaneMemoryRequirementsInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImagePlaneMemoryRequirementsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImagePlaneMemoryRequirementsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImagePlaneMemoryRequirementsInfo where
        type FieldType "pNext" VkImagePlaneMemoryRequirementsInfo =
             Ptr Void
        type FieldOptional "pNext" VkImagePlaneMemoryRequirementsInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImagePlaneMemoryRequirementsInfo =
             #{offset VkImagePlaneMemoryRequirementsInfo, pNext}
        type FieldIsArray "pNext" VkImagePlaneMemoryRequirementsInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImagePlaneMemoryRequirementsInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImagePlaneMemoryRequirementsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImagePlaneMemoryRequirementsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "planeAspect" VkImagePlaneMemoryRequirementsInfo where
        type FieldType "planeAspect" VkImagePlaneMemoryRequirementsInfo =
             VkImageAspectFlagBits
        type FieldOptional "planeAspect" VkImagePlaneMemoryRequirementsInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "planeAspect" VkImagePlaneMemoryRequirementsInfo =
             #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect}
        type FieldIsArray "planeAspect" VkImagePlaneMemoryRequirementsInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect}

instance {-# OVERLAPPING #-}
         CanReadField "planeAspect" VkImagePlaneMemoryRequirementsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect}

instance {-# OVERLAPPING #-}
         CanWriteField "planeAspect" VkImagePlaneMemoryRequirementsInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect}

instance Show VkImagePlaneMemoryRequirementsInfo where
        showsPrec d x
          = showString "VkImagePlaneMemoryRequirementsInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "planeAspect = " .
                            showsPrec d (getField @"planeAspect" x) . showChar '}'

-- | Alias for `VkImagePlaneMemoryRequirementsInfo`
type VkImagePlaneMemoryRequirementsInfoKHR =
     VkImagePlaneMemoryRequirementsInfo

-- | > typedef struct VkImageResolve {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffset;
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffset;
--   >     VkExtent3D             extent;
--   > } VkImageResolve;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageResolve VkImageResolve registry at www.khronos.org>
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

-- | > typedef struct VkImageSparseMemoryRequirementsInfo2 {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageSparseMemoryRequirementsInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageSparseMemoryRequirementsInfo2 VkImageSparseMemoryRequirementsInfo2 registry at www.khronos.org>
data VkImageSparseMemoryRequirementsInfo2 = VkImageSparseMemoryRequirementsInfo2## Addr##
                                                                                  ByteArray##

instance Eq VkImageSparseMemoryRequirementsInfo2 where
        (VkImageSparseMemoryRequirementsInfo2## a _) ==
          x@(VkImageSparseMemoryRequirementsInfo2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSparseMemoryRequirementsInfo2 where
        (VkImageSparseMemoryRequirementsInfo2## a _) `compare`
          x@(VkImageSparseMemoryRequirementsInfo2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSparseMemoryRequirementsInfo2 where
        sizeOf ~_
          = #{size VkImageSparseMemoryRequirementsInfo2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageSparseMemoryRequirementsInfo2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSparseMemoryRequirementsInfo2
         where
        unsafeAddr (VkImageSparseMemoryRequirementsInfo2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSparseMemoryRequirementsInfo2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSparseMemoryRequirementsInfo2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSparseMemoryRequirementsInfo2 where
        type StructFields VkImageSparseMemoryRequirementsInfo2 =
             '["sType", "pNext", "image"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSparseMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSparseMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSparseMemoryRequirementsInfo2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageSparseMemoryRequirementsInfo2 where
        type FieldType "sType" VkImageSparseMemoryRequirementsInfo2 =
             VkStructureType
        type FieldOptional "sType" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageSparseMemoryRequirementsInfo2 =
             #{offset VkImageSparseMemoryRequirementsInfo2, sType}
        type FieldIsArray "sType" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageSparseMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageSparseMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageSparseMemoryRequirementsInfo2 where
        type FieldType "pNext" VkImageSparseMemoryRequirementsInfo2 =
             Ptr Void
        type FieldOptional "pNext" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageSparseMemoryRequirementsInfo2 =
             #{offset VkImageSparseMemoryRequirementsInfo2, pNext}
        type FieldIsArray "pNext" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageSparseMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageSparseMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         HasField "image" VkImageSparseMemoryRequirementsInfo2 where
        type FieldType "image" VkImageSparseMemoryRequirementsInfo2 =
             VkImage
        type FieldOptional "image" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageSparseMemoryRequirementsInfo2 =
             #{offset VkImageSparseMemoryRequirementsInfo2, image}
        type FieldIsArray "image" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkImageSparseMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkImageSparseMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, image}

instance Show VkImageSparseMemoryRequirementsInfo2 where
        showsPrec d x
          = showString "VkImageSparseMemoryRequirementsInfo2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "image = " .
                            showsPrec d (getField @"image" x) . showChar '}'

-- | Alias for `VkImageSparseMemoryRequirementsInfo2`
type VkImageSparseMemoryRequirementsInfo2KHR =
     VkImageSparseMemoryRequirementsInfo2

-- | > typedef struct VkImageSubresource {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               mipLevel;
--   >     uint32_t               arrayLayer;
--   > } VkImageSubresource;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageSubresource VkImageSubresource registry at www.khronos.org>
data VkImageSubresource = VkImageSubresource## Addr## ByteArray##

instance Eq VkImageSubresource where
        (VkImageSubresource## a _) == x@(VkImageSubresource## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSubresource where
        (VkImageSubresource## a _) `compare` x@(VkImageSubresource## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSubresource where
        sizeOf ~_ = #{size VkImageSubresource}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageSubresource}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSubresource where
        unsafeAddr (VkImageSubresource## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSubresource## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSubresource## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSubresource where
        type StructFields VkImageSubresource =
             '["aspectMask", "mipLevel", "arrayLayer"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSubresource = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkImageSubresource where
        type FieldType "aspectMask" VkImageSubresource = VkImageAspectFlags
        type FieldOptional "aspectMask" VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkImageSubresource =
             #{offset VkImageSubresource, aspectMask}
        type FieldIsArray "aspectMask" VkImageSubresource = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageSubresource, aspectMask}

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkImageSubresource where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresource, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresource, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkImageSubresource where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresource, aspectMask}

instance {-# OVERLAPPING #-} HasField "mipLevel" VkImageSubresource
         where
        type FieldType "mipLevel" VkImageSubresource = Word32
        type FieldOptional "mipLevel" VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mipLevel" VkImageSubresource =
             #{offset VkImageSubresource, mipLevel}
        type FieldIsArray "mipLevel" VkImageSubresource = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageSubresource, mipLevel}

instance {-# OVERLAPPING #-}
         CanReadField "mipLevel" VkImageSubresource where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresource, mipLevel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresource, mipLevel}

instance {-# OVERLAPPING #-}
         CanWriteField "mipLevel" VkImageSubresource where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresource, mipLevel}

instance {-# OVERLAPPING #-}
         HasField "arrayLayer" VkImageSubresource where
        type FieldType "arrayLayer" VkImageSubresource = Word32
        type FieldOptional "arrayLayer" VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type FieldOffset "arrayLayer" VkImageSubresource =
             #{offset VkImageSubresource, arrayLayer}
        type FieldIsArray "arrayLayer" VkImageSubresource = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageSubresource, arrayLayer}

instance {-# OVERLAPPING #-}
         CanReadField "arrayLayer" VkImageSubresource where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresource, arrayLayer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresource, arrayLayer}

instance {-# OVERLAPPING #-}
         CanWriteField "arrayLayer" VkImageSubresource where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresource, arrayLayer}

instance Show VkImageSubresource where
        showsPrec d x
          = showString "VkImageSubresource {" .
              showString "aspectMask = " .
                showsPrec d (getField @"aspectMask" x) .
                  showString ", " .
                    showString "mipLevel = " .
                      showsPrec d (getField @"mipLevel" x) .
                        showString ", " .
                          showString "arrayLayer = " .
                            showsPrec d (getField @"arrayLayer" x) . showChar '}'

-- | > typedef struct VkImageSubresourceLayers {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               mipLevel;
--   >     uint32_t               baseArrayLayer;
--   >     uint32_t               layerCount;
--   > } VkImageSubresourceLayers;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageSubresourceLayers VkImageSubresourceLayers registry at www.khronos.org>
data VkImageSubresourceLayers = VkImageSubresourceLayers## Addr##
                                                          ByteArray##

instance Eq VkImageSubresourceLayers where
        (VkImageSubresourceLayers## a _) ==
          x@(VkImageSubresourceLayers## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSubresourceLayers where
        (VkImageSubresourceLayers## a _) `compare`
          x@(VkImageSubresourceLayers## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSubresourceLayers where
        sizeOf ~_ = #{size VkImageSubresourceLayers}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageSubresourceLayers}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSubresourceLayers where
        unsafeAddr (VkImageSubresourceLayers## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSubresourceLayers## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSubresourceLayers## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSubresourceLayers where
        type StructFields VkImageSubresourceLayers =
             '["aspectMask", "mipLevel", "baseArrayLayer", "layerCount"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSubresourceLayers = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkImageSubresourceLayers where
        type FieldType "aspectMask" VkImageSubresourceLayers =
             VkImageAspectFlags
        type FieldOptional "aspectMask" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkImageSubresourceLayers =
             #{offset VkImageSubresourceLayers, aspectMask}
        type FieldIsArray "aspectMask" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceLayers, aspectMask}

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkImageSubresourceLayers where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceLayers, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceLayers, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "mipLevel" VkImageSubresourceLayers where
        type FieldType "mipLevel" VkImageSubresourceLayers = Word32
        type FieldOptional "mipLevel" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mipLevel" VkImageSubresourceLayers =
             #{offset VkImageSubresourceLayers, mipLevel}
        type FieldIsArray "mipLevel" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceLayers, mipLevel}

instance {-# OVERLAPPING #-}
         CanReadField "mipLevel" VkImageSubresourceLayers where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, mipLevel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceLayers, mipLevel}

instance {-# OVERLAPPING #-}
         CanWriteField "mipLevel" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceLayers, mipLevel}

instance {-# OVERLAPPING #-}
         HasField "baseArrayLayer" VkImageSubresourceLayers where
        type FieldType "baseArrayLayer" VkImageSubresourceLayers = Word32
        type FieldOptional "baseArrayLayer" VkImageSubresourceLayers =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "baseArrayLayer" VkImageSubresourceLayers =
             #{offset VkImageSubresourceLayers, baseArrayLayer}
        type FieldIsArray "baseArrayLayer" VkImageSubresourceLayers =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceLayers, baseArrayLayer}

instance {-# OVERLAPPING #-}
         CanReadField "baseArrayLayer" VkImageSubresourceLayers where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, baseArrayLayer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceLayers, baseArrayLayer}

instance {-# OVERLAPPING #-}
         CanWriteField "baseArrayLayer" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceLayers, baseArrayLayer}

instance {-# OVERLAPPING #-}
         HasField "layerCount" VkImageSubresourceLayers where
        type FieldType "layerCount" VkImageSubresourceLayers = Word32
        type FieldOptional "layerCount" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layerCount" VkImageSubresourceLayers =
             #{offset VkImageSubresourceLayers, layerCount}
        type FieldIsArray "layerCount" VkImageSubresourceLayers = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceLayers, layerCount}

instance {-# OVERLAPPING #-}
         CanReadField "layerCount" VkImageSubresourceLayers where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceLayers, layerCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceLayers, layerCount}

instance {-# OVERLAPPING #-}
         CanWriteField "layerCount" VkImageSubresourceLayers where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceLayers, layerCount}

instance Show VkImageSubresourceLayers where
        showsPrec d x
          = showString "VkImageSubresourceLayers {" .
              showString "aspectMask = " .
                showsPrec d (getField @"aspectMask" x) .
                  showString ", " .
                    showString "mipLevel = " .
                      showsPrec d (getField @"mipLevel" x) .
                        showString ", " .
                          showString "baseArrayLayer = " .
                            showsPrec d (getField @"baseArrayLayer" x) .
                              showString ", " .
                                showString "layerCount = " .
                                  showsPrec d (getField @"layerCount" x) . showChar '}'

-- | > typedef struct VkImageSubresourceRange {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               baseMipLevel;
--   >     uint32_t               levelCount;
--   >     uint32_t               baseArrayLayer;
--   >     uint32_t               layerCount;
--   > } VkImageSubresourceRange;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageSubresourceRange VkImageSubresourceRange registry at www.khronos.org>
data VkImageSubresourceRange = VkImageSubresourceRange## Addr##
                                                        ByteArray##

instance Eq VkImageSubresourceRange where
        (VkImageSubresourceRange## a _) == x@(VkImageSubresourceRange## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSubresourceRange where
        (VkImageSubresourceRange## a _) `compare`
          x@(VkImageSubresourceRange## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSubresourceRange where
        sizeOf ~_ = #{size VkImageSubresourceRange}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageSubresourceRange}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSubresourceRange where
        unsafeAddr (VkImageSubresourceRange## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSubresourceRange## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSubresourceRange## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSubresourceRange where
        type StructFields VkImageSubresourceRange =
             '["aspectMask", "baseMipLevel", "levelCount", "baseArrayLayer", -- ' closing tick for hsc2hs
               "layerCount"]
        type CUnionType VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSubresourceRange = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkImageSubresourceRange where
        type FieldType "aspectMask" VkImageSubresourceRange =
             VkImageAspectFlags
        type FieldOptional "aspectMask" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkImageSubresourceRange =
             #{offset VkImageSubresourceRange, aspectMask}
        type FieldIsArray "aspectMask" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceRange, aspectMask}

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkImageSubresourceRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceRange, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceRange, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "baseMipLevel" VkImageSubresourceRange where
        type FieldType "baseMipLevel" VkImageSubresourceRange = Word32
        type FieldOptional "baseMipLevel" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "baseMipLevel" VkImageSubresourceRange =
             #{offset VkImageSubresourceRange, baseMipLevel}
        type FieldIsArray "baseMipLevel" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceRange, baseMipLevel}

instance {-# OVERLAPPING #-}
         CanReadField "baseMipLevel" VkImageSubresourceRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, baseMipLevel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceRange, baseMipLevel}

instance {-# OVERLAPPING #-}
         CanWriteField "baseMipLevel" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceRange, baseMipLevel}

instance {-# OVERLAPPING #-}
         HasField "levelCount" VkImageSubresourceRange where
        type FieldType "levelCount" VkImageSubresourceRange = Word32
        type FieldOptional "levelCount" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "levelCount" VkImageSubresourceRange =
             #{offset VkImageSubresourceRange, levelCount}
        type FieldIsArray "levelCount" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceRange, levelCount}

instance {-# OVERLAPPING #-}
         CanReadField "levelCount" VkImageSubresourceRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, levelCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceRange, levelCount}

instance {-# OVERLAPPING #-}
         CanWriteField "levelCount" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceRange, levelCount}

instance {-# OVERLAPPING #-}
         HasField "baseArrayLayer" VkImageSubresourceRange where
        type FieldType "baseArrayLayer" VkImageSubresourceRange = Word32
        type FieldOptional "baseArrayLayer" VkImageSubresourceRange =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "baseArrayLayer" VkImageSubresourceRange =
             #{offset VkImageSubresourceRange, baseArrayLayer}
        type FieldIsArray "baseArrayLayer" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceRange, baseArrayLayer}

instance {-# OVERLAPPING #-}
         CanReadField "baseArrayLayer" VkImageSubresourceRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, baseArrayLayer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceRange, baseArrayLayer}

instance {-# OVERLAPPING #-}
         CanWriteField "baseArrayLayer" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceRange, baseArrayLayer}

instance {-# OVERLAPPING #-}
         HasField "layerCount" VkImageSubresourceRange where
        type FieldType "layerCount" VkImageSubresourceRange = Word32
        type FieldOptional "layerCount" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layerCount" VkImageSubresourceRange =
             #{offset VkImageSubresourceRange, layerCount}
        type FieldIsArray "layerCount" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceRange, layerCount}

instance {-# OVERLAPPING #-}
         CanReadField "layerCount" VkImageSubresourceRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, layerCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceRange, layerCount}

instance {-# OVERLAPPING #-}
         CanWriteField "layerCount" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceRange, layerCount}

instance Show VkImageSubresourceRange where
        showsPrec d x
          = showString "VkImageSubresourceRange {" .
              showString "aspectMask = " .
                showsPrec d (getField @"aspectMask" x) .
                  showString ", " .
                    showString "baseMipLevel = " .
                      showsPrec d (getField @"baseMipLevel" x) .
                        showString ", " .
                          showString "levelCount = " .
                            showsPrec d (getField @"levelCount" x) .
                              showString ", " .
                                showString "baseArrayLayer = " .
                                  showsPrec d (getField @"baseArrayLayer" x) .
                                    showString ", " .
                                      showString "layerCount = " .
                                        showsPrec d (getField @"layerCount" x) . showChar '}'

-- | > typedef struct VkImageSwapchainCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR   swapchain;
--   > } VkImageSwapchainCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageSwapchainCreateInfoKHR VkImageSwapchainCreateInfoKHR registry at www.khronos.org>
data VkImageSwapchainCreateInfoKHR = VkImageSwapchainCreateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkImageSwapchainCreateInfoKHR where
        (VkImageSwapchainCreateInfoKHR## a _) ==
          x@(VkImageSwapchainCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSwapchainCreateInfoKHR where
        (VkImageSwapchainCreateInfoKHR## a _) `compare`
          x@(VkImageSwapchainCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSwapchainCreateInfoKHR where
        sizeOf ~_ = #{size VkImageSwapchainCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageSwapchainCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSwapchainCreateInfoKHR where
        unsafeAddr (VkImageSwapchainCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSwapchainCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSwapchainCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSwapchainCreateInfoKHR where
        type StructFields VkImageSwapchainCreateInfoKHR =
             '["sType", "pNext", "swapchain"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSwapchainCreateInfoKHR =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageSwapchainCreateInfoKHR where
        type FieldType "sType" VkImageSwapchainCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageSwapchainCreateInfoKHR =
             #{offset VkImageSwapchainCreateInfoKHR, sType}
        type FieldIsArray "sType" VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageSwapchainCreateInfoKHR where
        type FieldType "pNext" VkImageSwapchainCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageSwapchainCreateInfoKHR =
             #{offset VkImageSwapchainCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchain" VkImageSwapchainCreateInfoKHR where
        type FieldType "swapchain" VkImageSwapchainCreateInfoKHR =
             VkSwapchainKHR
        type FieldOptional "swapchain" VkImageSwapchainCreateInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "swapchain" VkImageSwapchainCreateInfoKHR =
             #{offset VkImageSwapchainCreateInfoKHR, swapchain}
        type FieldIsArray "swapchain" VkImageSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSwapchainCreateInfoKHR, swapchain}

instance {-# OVERLAPPING #-}
         CanReadField "swapchain" VkImageSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHR, swapchain})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHR, swapchain}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchain" VkImageSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHR, swapchain}

instance Show VkImageSwapchainCreateInfoKHR where
        showsPrec d x
          = showString "VkImageSwapchainCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "swapchain = " .
                            showsPrec d (getField @"swapchain" x) . showChar '}'

-- | > typedef struct VkImageViewCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkImageViewCreateFlags flags;
--   >     VkImage                image;
--   >     VkImageViewType        viewType;
--   >     VkFormat               format;
--   >     VkComponentMapping     components;
--   >     VkImageSubresourceRange subresourceRange;
--   > } VkImageViewCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageViewCreateInfo VkImageViewCreateInfo registry at www.khronos.org>
data VkImageViewCreateInfo = VkImageViewCreateInfo## Addr##
                                                    ByteArray##

instance Eq VkImageViewCreateInfo where
        (VkImageViewCreateInfo## a _) == x@(VkImageViewCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageViewCreateInfo where
        (VkImageViewCreateInfo## a _) `compare`
          x@(VkImageViewCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageViewCreateInfo where
        sizeOf ~_ = #{size VkImageViewCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageViewCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageViewCreateInfo where
        unsafeAddr (VkImageViewCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageViewCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageViewCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageViewCreateInfo where
        type StructFields VkImageViewCreateInfo =
             '["sType", "pNext", "flags", "image", "viewType", "format", -- ' closing tick for hsc2hs
               "components", "subresourceRange"]
        type CUnionType VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageViewCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkImageViewCreateInfo
         where
        type FieldType "sType" VkImageViewCreateInfo = VkStructureType
        type FieldOptional "sType" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, sType}
        type FieldIsArray "sType" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkImageViewCreateInfo
         where
        type FieldType "pNext" VkImageViewCreateInfo = Ptr Void
        type FieldOptional "pNext" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, pNext}
        type FieldIsArray "pNext" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkImageViewCreateInfo
         where
        type FieldType "flags" VkImageViewCreateInfo =
             VkImageViewCreateFlags
        type FieldOptional "flags" VkImageViewCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, flags}
        type FieldIsArray "flags" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, flags}

instance {-# OVERLAPPING #-} HasField "image" VkImageViewCreateInfo
         where
        type FieldType "image" VkImageViewCreateInfo = VkImage
        type FieldOptional "image" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, image}
        type FieldIsArray "image" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, image}

instance {-# OVERLAPPING #-}
         HasField "viewType" VkImageViewCreateInfo where
        type FieldType "viewType" VkImageViewCreateInfo = VkImageViewType
        type FieldOptional "viewType" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewType" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, viewType}
        type FieldIsArray "viewType" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, viewType}

instance {-# OVERLAPPING #-}
         CanReadField "viewType" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, viewType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, viewType}

instance {-# OVERLAPPING #-}
         CanWriteField "viewType" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, viewType}

instance {-# OVERLAPPING #-}
         HasField "format" VkImageViewCreateInfo where
        type FieldType "format" VkImageViewCreateInfo = VkFormat
        type FieldOptional "format" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, format}
        type FieldIsArray "format" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         HasField "components" VkImageViewCreateInfo where
        type FieldType "components" VkImageViewCreateInfo =
             VkComponentMapping
        type FieldOptional "components" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "components" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, components}
        type FieldIsArray "components" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewCreateInfo, components}

instance {-# OVERLAPPING #-}
         CanReadField "components" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, components})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, components}

instance {-# OVERLAPPING #-}
         CanWriteField "components" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, components}

instance {-# OVERLAPPING #-}
         HasField "subresourceRange" VkImageViewCreateInfo where
        type FieldType "subresourceRange" VkImageViewCreateInfo =
             VkImageSubresourceRange
        type FieldOptional "subresourceRange" VkImageViewCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subresourceRange" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, subresourceRange}
        type FieldIsArray "subresourceRange" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewCreateInfo, subresourceRange}

instance {-# OVERLAPPING #-}
         CanReadField "subresourceRange" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, subresourceRange})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, subresourceRange}

instance {-# OVERLAPPING #-}
         CanWriteField "subresourceRange" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, subresourceRange}

instance Show VkImageViewCreateInfo where
        showsPrec d x
          = showString "VkImageViewCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "image = " .
                                  showsPrec d (getField @"image" x) .
                                    showString ", " .
                                      showString "viewType = " .
                                        showsPrec d (getField @"viewType" x) .
                                          showString ", " .
                                            showString "format = " .
                                              showsPrec d (getField @"format" x) .
                                                showString ", " .
                                                  showString "components = " .
                                                    showsPrec d (getField @"components" x) .
                                                      showString ", " .
                                                        showString "subresourceRange = " .
                                                          showsPrec d
                                                            (getField @"subresourceRange" x)
                                                            . showChar '}'

-- | > typedef struct VkImageViewUsageCreateInfo {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkImageUsageFlags usage;
--   > } VkImageViewUsageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageViewUsageCreateInfo VkImageViewUsageCreateInfo registry at www.khronos.org>
data VkImageViewUsageCreateInfo = VkImageViewUsageCreateInfo## Addr##
                                                              ByteArray##

instance Eq VkImageViewUsageCreateInfo where
        (VkImageViewUsageCreateInfo## a _) ==
          x@(VkImageViewUsageCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageViewUsageCreateInfo where
        (VkImageViewUsageCreateInfo## a _) `compare`
          x@(VkImageViewUsageCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageViewUsageCreateInfo where
        sizeOf ~_ = #{size VkImageViewUsageCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageViewUsageCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageViewUsageCreateInfo where
        unsafeAddr (VkImageViewUsageCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageViewUsageCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageViewUsageCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageViewUsageCreateInfo where
        type StructFields VkImageViewUsageCreateInfo =
             '["sType", "pNext", "usage"] -- ' closing tick for hsc2hs
        type CUnionType VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageViewUsageCreateInfo =
             '[VkImageViewCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageViewUsageCreateInfo where
        type FieldType "sType" VkImageViewUsageCreateInfo = VkStructureType
        type FieldOptional "sType" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageViewUsageCreateInfo =
             #{offset VkImageViewUsageCreateInfo, sType}
        type FieldIsArray "sType" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageViewUsageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewUsageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageViewUsageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageViewUsageCreateInfo where
        type FieldType "pNext" VkImageViewUsageCreateInfo = Ptr Void
        type FieldOptional "pNext" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageViewUsageCreateInfo =
             #{offset VkImageViewUsageCreateInfo, pNext}
        type FieldIsArray "pNext" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageViewUsageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewUsageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageViewUsageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "usage" VkImageViewUsageCreateInfo where
        type FieldType "usage" VkImageViewUsageCreateInfo =
             VkImageUsageFlags
        type FieldOptional "usage" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkImageViewUsageCreateInfo =
             #{offset VkImageViewUsageCreateInfo, usage}
        type FieldIsArray "usage" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfo, usage}

instance {-# OVERLAPPING #-}
         CanReadField "usage" VkImageViewUsageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfo, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewUsageCreateInfo, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkImageViewUsageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfo, usage}

instance Show VkImageViewUsageCreateInfo where
        showsPrec d x
          = showString "VkImageViewUsageCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "usage = " .
                            showsPrec d (getField @"usage" x) . showChar '}'

-- | Alias for `VkImageViewUsageCreateInfo`
type VkImageViewUsageCreateInfoKHR = VkImageViewUsageCreateInfo
