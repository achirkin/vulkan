#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorImageInfo
       (VkDescriptorImageInfo(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageLayout (VkImageLayout)
import           Graphics.Vulkan.Types.Handles            (VkImageView,
                                                           VkSampler)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorImageInfo {
--   >     VkSampler       sampler;
--   >     VkImageView     imageView;
--   >     VkImageLayout   imageLayout;
--   > } VkDescriptorImageInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDescriptorImageInfo VkDescriptorImageInfo registry at www.khronos.org>
data VkDescriptorImageInfo = VkDescriptorImageInfo## Addr##
                                                    ByteArray##

instance Eq VkDescriptorImageInfo where
        (VkDescriptorImageInfo## a _) == x@(VkDescriptorImageInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorImageInfo where
        (VkDescriptorImageInfo## a _) `compare`
          x@(VkDescriptorImageInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorImageInfo where
        sizeOf ~_ = #{size VkDescriptorImageInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDescriptorImageInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorImageInfo where
        unsafeAddr (VkDescriptorImageInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorImageInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorImageInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorImageInfo where
        type StructFields VkDescriptorImageInfo =
             '["sampler", "imageView", "imageLayout"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorImageInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorImageInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorImageInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sampler" VkDescriptorImageInfo where
        type FieldType "sampler" VkDescriptorImageInfo = VkSampler
        type FieldOptional "sampler" VkDescriptorImageInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampler" VkDescriptorImageInfo =
             #{offset VkDescriptorImageInfo, sampler}
        type FieldIsArray "sampler" VkDescriptorImageInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDescriptorImageInfo, sampler}

instance {-# OVERLAPPING #-}
         CanReadField "sampler" VkDescriptorImageInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorImageInfo, sampler})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorImageInfo, sampler}

instance {-# OVERLAPPING #-}
         CanWriteField "sampler" VkDescriptorImageInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorImageInfo, sampler}

instance {-# OVERLAPPING #-}
         HasField "imageView" VkDescriptorImageInfo where
        type FieldType "imageView" VkDescriptorImageInfo = VkImageView
        type FieldOptional "imageView" VkDescriptorImageInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageView" VkDescriptorImageInfo =
             #{offset VkDescriptorImageInfo, imageView}
        type FieldIsArray "imageView" VkDescriptorImageInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorImageInfo, imageView}

instance {-# OVERLAPPING #-}
         CanReadField "imageView" VkDescriptorImageInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorImageInfo, imageView})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorImageInfo, imageView}

instance {-# OVERLAPPING #-}
         CanWriteField "imageView" VkDescriptorImageInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorImageInfo, imageView}

instance {-# OVERLAPPING #-}
         HasField "imageLayout" VkDescriptorImageInfo where
        type FieldType "imageLayout" VkDescriptorImageInfo = VkImageLayout
        type FieldOptional "imageLayout" VkDescriptorImageInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageLayout" VkDescriptorImageInfo =
             #{offset VkDescriptorImageInfo, imageLayout}
        type FieldIsArray "imageLayout" VkDescriptorImageInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorImageInfo, imageLayout}

instance {-# OVERLAPPING #-}
         CanReadField "imageLayout" VkDescriptorImageInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorImageInfo, imageLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorImageInfo, imageLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "imageLayout" VkDescriptorImageInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorImageInfo, imageLayout}

instance Show VkDescriptorImageInfo where
        showsPrec d x
          = showString "VkDescriptorImageInfo {" .
              showString "sampler = " .
                showsPrec d (getField @"sampler" x) .
                  showString ", " .
                    showString "imageView = " .
                      showsPrec d (getField @"imageView" x) .
                        showString ", " .
                          showString "imageLayout = " .
                            showsPrec d (getField @"imageLayout" x) . showChar '}'
