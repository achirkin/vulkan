#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorImageInfo
       (VkDescriptorImageInfo(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageLayout (VkImageLayout)
import           Graphics.Vulkan.Types.Handles            (VkImageView,
                                                           VkSampler)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorImageInfo {
--   >     VkSampler       sampler;
--   >     VkImageView     imageView;
--   >     VkImageLayout   imageLayout;
--   > } VkDescriptorImageInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorImageInfo.html VkDescriptorImageInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSampler VkDescriptorImageInfo
         where
        type VkSamplerMType VkDescriptorImageInfo = VkSampler

        {-# NOINLINE vkSampler #-}
        vkSampler x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorImageInfo, sampler})

        {-# INLINE vkSamplerByteOffset #-}
        vkSamplerByteOffset ~_
          = #{offset VkDescriptorImageInfo, sampler}

        {-# INLINE readVkSampler #-}
        readVkSampler p
          = peekByteOff p #{offset VkDescriptorImageInfo, sampler}

        {-# INLINE writeVkSampler #-}
        writeVkSampler p
          = pokeByteOff p #{offset VkDescriptorImageInfo, sampler}

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

instance CanReadField "sampler" VkDescriptorImageInfo where
        {-# INLINE getField #-}
        getField = vkSampler

        {-# INLINE readField #-}
        readField = readVkSampler

instance CanWriteField "sampler" VkDescriptorImageInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSampler

instance {-# OVERLAPPING #-} HasVkImageView VkDescriptorImageInfo
         where
        type VkImageViewMType VkDescriptorImageInfo = VkImageView

        {-# NOINLINE vkImageView #-}
        vkImageView x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorImageInfo, imageView})

        {-# INLINE vkImageViewByteOffset #-}
        vkImageViewByteOffset ~_
          = #{offset VkDescriptorImageInfo, imageView}

        {-# INLINE readVkImageView #-}
        readVkImageView p
          = peekByteOff p #{offset VkDescriptorImageInfo, imageView}

        {-# INLINE writeVkImageView #-}
        writeVkImageView p
          = pokeByteOff p #{offset VkDescriptorImageInfo, imageView}

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

instance CanReadField "imageView" VkDescriptorImageInfo where
        {-# INLINE getField #-}
        getField = vkImageView

        {-# INLINE readField #-}
        readField = readVkImageView

instance CanWriteField "imageView" VkDescriptorImageInfo where
        {-# INLINE writeField #-}
        writeField = writeVkImageView

instance {-# OVERLAPPING #-} HasVkImageLayout VkDescriptorImageInfo
         where
        type VkImageLayoutMType VkDescriptorImageInfo = VkImageLayout

        {-# NOINLINE vkImageLayout #-}
        vkImageLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorImageInfo, imageLayout})

        {-# INLINE vkImageLayoutByteOffset #-}
        vkImageLayoutByteOffset ~_
          = #{offset VkDescriptorImageInfo, imageLayout}

        {-# INLINE readVkImageLayout #-}
        readVkImageLayout p
          = peekByteOff p #{offset VkDescriptorImageInfo, imageLayout}

        {-# INLINE writeVkImageLayout #-}
        writeVkImageLayout p
          = pokeByteOff p #{offset VkDescriptorImageInfo, imageLayout}

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

instance CanReadField "imageLayout" VkDescriptorImageInfo where
        {-# INLINE getField #-}
        getField = vkImageLayout

        {-# INLINE readField #-}
        readField = readVkImageLayout

instance CanWriteField "imageLayout" VkDescriptorImageInfo where
        {-# INLINE writeField #-}
        writeField = writeVkImageLayout

instance Show VkDescriptorImageInfo where
        showsPrec d x
          = showString "VkDescriptorImageInfo {" .
              showString "vkSampler = " .
                showsPrec d (vkSampler x) .
                  showString ", " .
                    showString "vkImageView = " .
                      showsPrec d (vkImageView x) .
                        showString ", " .
                          showString "vkImageLayout = " .
                            showsPrec d (vkImageLayout x) . showChar '}'
