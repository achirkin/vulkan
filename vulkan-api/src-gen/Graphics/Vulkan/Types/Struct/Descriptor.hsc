#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Descriptor
       (VkDescriptorBufferInfo(..), VkDescriptorImageInfo(..),
        VkDescriptorPoolCreateInfo(..), VkDescriptorPoolSize(..),
        VkDescriptorSetAllocateInfo(..), VkDescriptorSetLayoutBinding(..),
        VkDescriptorSetLayoutBindingFlagsCreateInfoEXT(..),
        VkDescriptorSetLayoutCreateInfo(..),
        VkDescriptorSetLayoutSupport(..), VkDescriptorSetLayoutSupportKHR,
        VkDescriptorSetVariableDescriptorCountAllocateInfoEXT(..),
        VkDescriptorSetVariableDescriptorCountLayoutSupportEXT(..),
        VkDescriptorUpdateTemplateCreateInfo(..),
        VkDescriptorUpdateTemplateCreateInfoKHR,
        VkDescriptorUpdateTemplateEntry(..),
        VkDescriptorUpdateTemplateEntryKHR)
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32,
                                                           VkDeviceSize)
import           Graphics.Vulkan.Types.Bitmasks           (VkDescriptorUpdateTemplateCreateFlags)
import           Graphics.Vulkan.Types.Enum.Descriptor    (VkDescriptorBindingFlagsEXT,
                                                           VkDescriptorPoolCreateFlags,
                                                           VkDescriptorSetLayoutCreateFlags,
                                                           VkDescriptorType,
                                                           VkDescriptorUpdateTemplateType)
import           Graphics.Vulkan.Types.Enum.Image         (VkImageLayout)
import           Graphics.Vulkan.Types.Enum.Pipeline      (VkPipelineBindPoint)
import           Graphics.Vulkan.Types.Enum.Shader        (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer,
                                                           VkDescriptorPool,
                                                           VkDescriptorSetLayout,
                                                           VkImageView,
                                                           VkPipelineLayout,
                                                           VkSampler)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorBufferInfo {
--   >     VkBuffer               buffer;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           range;
--   > } VkDescriptorBufferInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorBufferInfo VkDescriptorBufferInfo registry at www.khronos.org>
data VkDescriptorBufferInfo = VkDescriptorBufferInfo## Addr##
                                                      ByteArray##

instance Eq VkDescriptorBufferInfo where
        (VkDescriptorBufferInfo## a _) == x@(VkDescriptorBufferInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorBufferInfo where
        (VkDescriptorBufferInfo## a _) `compare`
          x@(VkDescriptorBufferInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorBufferInfo where
        sizeOf ~_ = #{size VkDescriptorBufferInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDescriptorBufferInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorBufferInfo where
        unsafeAddr (VkDescriptorBufferInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorBufferInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorBufferInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorBufferInfo where
        type StructFields VkDescriptorBufferInfo =
             '["buffer", "offset", "range"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorBufferInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "buffer" VkDescriptorBufferInfo where
        type FieldType "buffer" VkDescriptorBufferInfo = VkBuffer
        type FieldOptional "buffer" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkDescriptorBufferInfo =
             #{offset VkDescriptorBufferInfo, buffer}
        type FieldIsArray "buffer" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDescriptorBufferInfo, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkDescriptorBufferInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorBufferInfo, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorBufferInfo, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkDescriptorBufferInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorBufferInfo, buffer}

instance {-# OVERLAPPING #-}
         HasField "offset" VkDescriptorBufferInfo where
        type FieldType "offset" VkDescriptorBufferInfo = VkDeviceSize
        type FieldOptional "offset" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkDescriptorBufferInfo =
             #{offset VkDescriptorBufferInfo, offset}
        type FieldIsArray "offset" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDescriptorBufferInfo, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkDescriptorBufferInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorBufferInfo, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorBufferInfo, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkDescriptorBufferInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorBufferInfo, offset}

instance {-# OVERLAPPING #-}
         HasField "range" VkDescriptorBufferInfo where
        type FieldType "range" VkDescriptorBufferInfo = VkDeviceSize
        type FieldOptional "range" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "range" VkDescriptorBufferInfo =
             #{offset VkDescriptorBufferInfo, range}
        type FieldIsArray "range" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDescriptorBufferInfo, range}

instance {-# OVERLAPPING #-}
         CanReadField "range" VkDescriptorBufferInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorBufferInfo, range})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorBufferInfo, range}

instance {-# OVERLAPPING #-}
         CanWriteField "range" VkDescriptorBufferInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorBufferInfo, range}

instance Show VkDescriptorBufferInfo where
        showsPrec d x
          = showString "VkDescriptorBufferInfo {" .
              showString "buffer = " .
                showsPrec d (getField @"buffer" x) .
                  showString ", " .
                    showString "offset = " .
                      showsPrec d (getField @"offset" x) .
                        showString ", " .
                          showString "range = " .
                            showsPrec d (getField @"range" x) . showChar '}'

-- | > typedef struct VkDescriptorImageInfo {
--   >     VkSampler       sampler;
--   >     VkImageView     imageView;
--   >     VkImageLayout   imageLayout;
--   > } VkDescriptorImageInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorImageInfo VkDescriptorImageInfo registry at www.khronos.org>
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

-- | > typedef struct VkDescriptorPoolCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorPoolCreateFlags  flags;
--   >     uint32_t               maxSets;
--   >     uint32_t               poolSizeCount;
--   >     const VkDescriptorPoolSize* pPoolSizes;
--   > } VkDescriptorPoolCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorPoolCreateInfo VkDescriptorPoolCreateInfo registry at www.khronos.org>
data VkDescriptorPoolCreateInfo = VkDescriptorPoolCreateInfo## Addr##
                                                              ByteArray##

instance Eq VkDescriptorPoolCreateInfo where
        (VkDescriptorPoolCreateInfo## a _) ==
          x@(VkDescriptorPoolCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorPoolCreateInfo where
        (VkDescriptorPoolCreateInfo## a _) `compare`
          x@(VkDescriptorPoolCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorPoolCreateInfo where
        sizeOf ~_ = #{size VkDescriptorPoolCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDescriptorPoolCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorPoolCreateInfo where
        unsafeAddr (VkDescriptorPoolCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorPoolCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorPoolCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorPoolCreateInfo where
        type StructFields VkDescriptorPoolCreateInfo =
             '["sType", "pNext", "flags", "maxSets", "poolSizeCount", -- ' closing tick for hsc2hs
               "pPoolSizes"]
        type CUnionType VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorPoolCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorPoolCreateInfo where
        type FieldType "sType" VkDescriptorPoolCreateInfo = VkStructureType
        type FieldOptional "sType" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, sType}
        type FieldIsArray "sType" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorPoolCreateInfo where
        type FieldType "pNext" VkDescriptorPoolCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, pNext}
        type FieldIsArray "pNext" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorPoolCreateInfo where
        type FieldType "flags" VkDescriptorPoolCreateInfo =
             VkDescriptorPoolCreateFlags
        type FieldOptional "flags" VkDescriptorPoolCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, flags}
        type FieldIsArray "flags" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "maxSets" VkDescriptorPoolCreateInfo where
        type FieldType "maxSets" VkDescriptorPoolCreateInfo = Word32
        type FieldOptional "maxSets" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSets" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, maxSets}
        type FieldIsArray "maxSets" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, maxSets}

instance {-# OVERLAPPING #-}
         CanReadField "maxSets" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, maxSets})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, maxSets}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSets" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, maxSets}

instance {-# OVERLAPPING #-}
         HasField "poolSizeCount" VkDescriptorPoolCreateInfo where
        type FieldType "poolSizeCount" VkDescriptorPoolCreateInfo = Word32
        type FieldOptional "poolSizeCount" VkDescriptorPoolCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "poolSizeCount" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, poolSizeCount}
        type FieldIsArray "poolSizeCount" VkDescriptorPoolCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, poolSizeCount}

instance {-# OVERLAPPING #-}
         CanReadField "poolSizeCount" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, poolSizeCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, poolSizeCount}

instance {-# OVERLAPPING #-}
         CanWriteField "poolSizeCount" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, poolSizeCount}

instance {-# OVERLAPPING #-}
         HasField "pPoolSizes" VkDescriptorPoolCreateInfo where
        type FieldType "pPoolSizes" VkDescriptorPoolCreateInfo =
             Ptr VkDescriptorPoolSize
        type FieldOptional "pPoolSizes" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPoolSizes" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, pPoolSizes}
        type FieldIsArray "pPoolSizes" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, pPoolSizes}

instance {-# OVERLAPPING #-}
         CanReadField "pPoolSizes" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, pPoolSizes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, pPoolSizes}

instance {-# OVERLAPPING #-}
         CanWriteField "pPoolSizes" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, pPoolSizes}

instance Show VkDescriptorPoolCreateInfo where
        showsPrec d x
          = showString "VkDescriptorPoolCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "maxSets = " .
                                  showsPrec d (getField @"maxSets" x) .
                                    showString ", " .
                                      showString "poolSizeCount = " .
                                        showsPrec d (getField @"poolSizeCount" x) .
                                          showString ", " .
                                            showString "pPoolSizes = " .
                                              showsPrec d (getField @"pPoolSizes" x) . showChar '}'

-- | > typedef struct VkDescriptorPoolSize {
--   >     VkDescriptorType       type;
--   >     uint32_t               descriptorCount;
--   > } VkDescriptorPoolSize;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorPoolSize VkDescriptorPoolSize registry at www.khronos.org>
data VkDescriptorPoolSize = VkDescriptorPoolSize## Addr## ByteArray##

instance Eq VkDescriptorPoolSize where
        (VkDescriptorPoolSize## a _) == x@(VkDescriptorPoolSize## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorPoolSize where
        (VkDescriptorPoolSize## a _) `compare` x@(VkDescriptorPoolSize## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorPoolSize where
        sizeOf ~_ = #{size VkDescriptorPoolSize}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDescriptorPoolSize}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorPoolSize where
        unsafeAddr (VkDescriptorPoolSize## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorPoolSize## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorPoolSize## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorPoolSize where
        type StructFields VkDescriptorPoolSize =
             '["type", "descriptorCount"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorPoolSize = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "type" VkDescriptorPoolSize
         where
        type FieldType "type" VkDescriptorPoolSize = VkDescriptorType
        type FieldOptional "type" VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkDescriptorPoolSize =
             #{offset VkDescriptorPoolSize, type}
        type FieldIsArray "type" VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDescriptorPoolSize, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkDescriptorPoolSize where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolSize, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolSize, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkDescriptorPoolSize where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolSize, type}

instance {-# OVERLAPPING #-}
         HasField "descriptorCount" VkDescriptorPoolSize where
        type FieldType "descriptorCount" VkDescriptorPoolSize = Word32
        type FieldOptional "descriptorCount" VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorCount" VkDescriptorPoolSize =
             #{offset VkDescriptorPoolSize, descriptorCount}
        type FieldIsArray "descriptorCount" VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolSize, descriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorCount" VkDescriptorPoolSize where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolSize, descriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolSize, descriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorCount" VkDescriptorPoolSize where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolSize, descriptorCount}

instance Show VkDescriptorPoolSize where
        showsPrec d x
          = showString "VkDescriptorPoolSize {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "descriptorCount = " .
                      showsPrec d (getField @"descriptorCount" x) . showChar '}'

-- | > typedef struct VkDescriptorSetAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorPool       descriptorPool;
--   >     uint32_t               descriptorSetCount;
--   >     const VkDescriptorSetLayout* pSetLayouts;
--   > } VkDescriptorSetAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetAllocateInfo VkDescriptorSetAllocateInfo registry at www.khronos.org>
data VkDescriptorSetAllocateInfo = VkDescriptorSetAllocateInfo## Addr##
                                                                ByteArray##

instance Eq VkDescriptorSetAllocateInfo where
        (VkDescriptorSetAllocateInfo## a _) ==
          x@(VkDescriptorSetAllocateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetAllocateInfo where
        (VkDescriptorSetAllocateInfo## a _) `compare`
          x@(VkDescriptorSetAllocateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetAllocateInfo where
        sizeOf ~_ = #{size VkDescriptorSetAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDescriptorSetAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorSetAllocateInfo where
        unsafeAddr (VkDescriptorSetAllocateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorSetAllocateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetAllocateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorSetAllocateInfo where
        type StructFields VkDescriptorSetAllocateInfo =
             '["sType", "pNext", "descriptorPool", "descriptorSetCount", -- ' closing tick for hsc2hs
               "pSetLayouts"]
        type CUnionType VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetAllocateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorSetAllocateInfo where
        type FieldType "sType" VkDescriptorSetAllocateInfo =
             VkStructureType
        type FieldOptional "sType" VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorSetAllocateInfo =
             #{offset VkDescriptorSetAllocateInfo, sType}
        type FieldIsArray "sType" VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorSetAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorSetAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorSetAllocateInfo where
        type FieldType "pNext" VkDescriptorSetAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorSetAllocateInfo =
             #{offset VkDescriptorSetAllocateInfo, pNext}
        type FieldIsArray "pNext" VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorSetAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorSetAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "descriptorPool" VkDescriptorSetAllocateInfo where
        type FieldType "descriptorPool" VkDescriptorSetAllocateInfo =
             VkDescriptorPool
        type FieldOptional "descriptorPool" VkDescriptorSetAllocateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorPool" VkDescriptorSetAllocateInfo =
             #{offset VkDescriptorSetAllocateInfo, descriptorPool}
        type FieldIsArray "descriptorPool" VkDescriptorSetAllocateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetAllocateInfo, descriptorPool}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorPool" VkDescriptorSetAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, descriptorPool})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorPool}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorPool" VkDescriptorSetAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorPool}

instance {-# OVERLAPPING #-}
         HasField "descriptorSetCount" VkDescriptorSetAllocateInfo where
        type FieldType "descriptorSetCount" VkDescriptorSetAllocateInfo =
             Word32
        type FieldOptional "descriptorSetCount" VkDescriptorSetAllocateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSetCount" VkDescriptorSetAllocateInfo =
             #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}
        type FieldIsArray "descriptorSetCount" VkDescriptorSetAllocateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorSetCount" VkDescriptorSetAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, descriptorSetCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorSetCount" VkDescriptorSetAllocateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}

instance {-# OVERLAPPING #-}
         HasField "pSetLayouts" VkDescriptorSetAllocateInfo where
        type FieldType "pSetLayouts" VkDescriptorSetAllocateInfo =
             Ptr VkDescriptorSetLayout
        type FieldOptional "pSetLayouts" VkDescriptorSetAllocateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pSetLayouts" VkDescriptorSetAllocateInfo =
             #{offset VkDescriptorSetAllocateInfo, pSetLayouts}
        type FieldIsArray "pSetLayouts" VkDescriptorSetAllocateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetAllocateInfo, pSetLayouts}

instance {-# OVERLAPPING #-}
         CanReadField "pSetLayouts" VkDescriptorSetAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, pSetLayouts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, pSetLayouts}

instance {-# OVERLAPPING #-}
         CanWriteField "pSetLayouts" VkDescriptorSetAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, pSetLayouts}

instance Show VkDescriptorSetAllocateInfo where
        showsPrec d x
          = showString "VkDescriptorSetAllocateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "descriptorPool = " .
                            showsPrec d (getField @"descriptorPool" x) .
                              showString ", " .
                                showString "descriptorSetCount = " .
                                  showsPrec d (getField @"descriptorSetCount" x) .
                                    showString ", " .
                                      showString "pSetLayouts = " .
                                        showsPrec d (getField @"pSetLayouts" x) . showChar '}'

-- | > typedef struct VkDescriptorSetLayoutBinding {
--   >     uint32_t               binding;
--   >     VkDescriptorType       descriptorType;
--   >     uint32_t descriptorCount;
--   >     VkShaderStageFlags     stageFlags;
--   >     const VkSampler*       pImmutableSamplers;
--   > } VkDescriptorSetLayoutBinding;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetLayoutBinding VkDescriptorSetLayoutBinding registry at www.khronos.org>
data VkDescriptorSetLayoutBinding = VkDescriptorSetLayoutBinding## Addr##
                                                                  ByteArray##

instance Eq VkDescriptorSetLayoutBinding where
        (VkDescriptorSetLayoutBinding## a _) ==
          x@(VkDescriptorSetLayoutBinding## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetLayoutBinding where
        (VkDescriptorSetLayoutBinding## a _) `compare`
          x@(VkDescriptorSetLayoutBinding## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetLayoutBinding where
        sizeOf ~_ = #{size VkDescriptorSetLayoutBinding}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetLayoutBinding}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorSetLayoutBinding where
        unsafeAddr (VkDescriptorSetLayoutBinding## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorSetLayoutBinding## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetLayoutBinding##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorSetLayoutBinding where
        type StructFields VkDescriptorSetLayoutBinding =
             '["binding", "descriptorType", "descriptorCount", "stageFlags", -- ' closing tick for hsc2hs
               "pImmutableSamplers"]
        type CUnionType VkDescriptorSetLayoutBinding = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetLayoutBinding = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetLayoutBinding = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "binding" VkDescriptorSetLayoutBinding where
        type FieldType "binding" VkDescriptorSetLayoutBinding = Word32
        type FieldOptional "binding" VkDescriptorSetLayoutBinding = 'False -- ' closing tick for hsc2hs
        type FieldOffset "binding" VkDescriptorSetLayoutBinding =
             #{offset VkDescriptorSetLayoutBinding, binding}
        type FieldIsArray "binding" VkDescriptorSetLayoutBinding = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBinding, binding}

instance {-# OVERLAPPING #-}
         CanReadField "binding" VkDescriptorSetLayoutBinding where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, binding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, binding}

instance {-# OVERLAPPING #-}
         CanWriteField "binding" VkDescriptorSetLayoutBinding where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, binding}

instance {-# OVERLAPPING #-}
         HasField "descriptorType" VkDescriptorSetLayoutBinding where
        type FieldType "descriptorType" VkDescriptorSetLayoutBinding =
             VkDescriptorType
        type FieldOptional "descriptorType" VkDescriptorSetLayoutBinding =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorType" VkDescriptorSetLayoutBinding =
             #{offset VkDescriptorSetLayoutBinding, descriptorType}
        type FieldIsArray "descriptorType" VkDescriptorSetLayoutBinding =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBinding, descriptorType}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorType" VkDescriptorSetLayoutBinding where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, descriptorType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorType}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorType" VkDescriptorSetLayoutBinding where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorType}

instance {-# OVERLAPPING #-}
         HasField "descriptorCount" VkDescriptorSetLayoutBinding where
        type FieldType "descriptorCount" VkDescriptorSetLayoutBinding =
             Word32
        type FieldOptional "descriptorCount" VkDescriptorSetLayoutBinding =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "descriptorCount" VkDescriptorSetLayoutBinding =
             #{offset VkDescriptorSetLayoutBinding, descriptorCount}
        type FieldIsArray "descriptorCount" VkDescriptorSetLayoutBinding =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBinding, descriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorCount" VkDescriptorSetLayoutBinding where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, descriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorCount" VkDescriptorSetLayoutBinding where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorCount}

instance {-# OVERLAPPING #-}
         HasField "stageFlags" VkDescriptorSetLayoutBinding where
        type FieldType "stageFlags" VkDescriptorSetLayoutBinding =
             VkShaderStageFlags
        type FieldOptional "stageFlags" VkDescriptorSetLayoutBinding =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stageFlags" VkDescriptorSetLayoutBinding =
             #{offset VkDescriptorSetLayoutBinding, stageFlags}
        type FieldIsArray "stageFlags" VkDescriptorSetLayoutBinding =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBinding, stageFlags}

instance {-# OVERLAPPING #-}
         CanReadField "stageFlags" VkDescriptorSetLayoutBinding where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, stageFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, stageFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "stageFlags" VkDescriptorSetLayoutBinding where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, stageFlags}

instance {-# OVERLAPPING #-}
         HasField "pImmutableSamplers" VkDescriptorSetLayoutBinding where
        type FieldType "pImmutableSamplers" VkDescriptorSetLayoutBinding =
             Ptr VkSampler
        type FieldOptional "pImmutableSamplers"
               VkDescriptorSetLayoutBinding
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pImmutableSamplers" VkDescriptorSetLayoutBinding
             =
             #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}
        type FieldIsArray "pImmutableSamplers" VkDescriptorSetLayoutBinding
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}

instance {-# OVERLAPPING #-}
         CanReadField "pImmutableSamplers" VkDescriptorSetLayoutBinding
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}

instance {-# OVERLAPPING #-}
         CanWriteField "pImmutableSamplers" VkDescriptorSetLayoutBinding
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}

instance Show VkDescriptorSetLayoutBinding where
        showsPrec d x
          = showString "VkDescriptorSetLayoutBinding {" .
              showString "binding = " .
                showsPrec d (getField @"binding" x) .
                  showString ", " .
                    showString "descriptorType = " .
                      showsPrec d (getField @"descriptorType" x) .
                        showString ", " .
                          showString "descriptorCount = " .
                            showsPrec d (getField @"descriptorCount" x) .
                              showString ", " .
                                showString "stageFlags = " .
                                  showsPrec d (getField @"stageFlags" x) .
                                    showString ", " .
                                      showString "pImmutableSamplers = " .
                                        showsPrec d (getField @"pImmutableSamplers" x) .
                                          showChar '}'

-- | > typedef struct VkDescriptorSetLayoutBindingFlagsCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               bindingCount;
--   >     const VkDescriptorBindingFlagsEXT* pBindingFlags;
--   > } VkDescriptorSetLayoutBindingFlagsCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetLayoutBindingFlagsCreateInfoEXT VkDescriptorSetLayoutBindingFlagsCreateInfoEXT registry at www.khronos.org>
data VkDescriptorSetLayoutBindingFlagsCreateInfoEXT = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## Addr##
                                                                                                      ByteArray##

instance Eq VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
        (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## a _) ==
          x@(VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
        (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## a _) `compare`
          x@(VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkDescriptorSetLayoutBindingFlagsCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetLayoutBindingFlagsCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        unsafeAddr (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        type StructFields VkDescriptorSetLayoutBindingFlagsCreateInfoEXT =
             '["sType", "pNext", "bindingCount", "pBindingFlags"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorSetLayoutBindingFlagsCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetLayoutBindingFlagsCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetLayoutBindingFlagsCreateInfoEXT =
             '[VkDescriptorSetLayoutCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        type FieldType "sType"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             =
             #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        type FieldType "pNext"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             =
             #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "bindingCount"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        type FieldType "bindingCount"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = Word32
        type FieldOptional "bindingCount"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "bindingCount"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             =
             #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount}
        type FieldIsArray "bindingCount"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount}

instance {-# OVERLAPPING #-}
         CanReadField "bindingCount"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bindingCount"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount}

instance {-# OVERLAPPING #-}
         HasField "pBindingFlags"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        type FieldType "pBindingFlags"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = Ptr VkDescriptorBindingFlagsEXT
        type FieldOptional "pBindingFlags"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pBindingFlags"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             =
             #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags}
        type FieldIsArray "pBindingFlags"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags}

instance {-# OVERLAPPING #-}
         CanReadField "pBindingFlags"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "pBindingFlags"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags}

instance Show VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
        showsPrec d x
          = showString "VkDescriptorSetLayoutBindingFlagsCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "bindingCount = " .
                            showsPrec d (getField @"bindingCount" x) .
                              showString ", " .
                                showString "pBindingFlags = " .
                                  showsPrec d (getField @"pBindingFlags" x) . showChar '}'

-- | > typedef struct VkDescriptorSetLayoutCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorSetLayoutCreateFlags    flags;
--   >     uint32_t               bindingCount;
--   >     const VkDescriptorSetLayoutBinding* pBindings;
--   > } VkDescriptorSetLayoutCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetLayoutCreateInfo VkDescriptorSetLayoutCreateInfo registry at www.khronos.org>
data VkDescriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo## Addr##
                                                                        ByteArray##

instance Eq VkDescriptorSetLayoutCreateInfo where
        (VkDescriptorSetLayoutCreateInfo## a _) ==
          x@(VkDescriptorSetLayoutCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetLayoutCreateInfo where
        (VkDescriptorSetLayoutCreateInfo## a _) `compare`
          x@(VkDescriptorSetLayoutCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetLayoutCreateInfo where
        sizeOf ~_ = #{size VkDescriptorSetLayoutCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetLayoutCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorSetLayoutCreateInfo where
        unsafeAddr (VkDescriptorSetLayoutCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorSetLayoutCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetLayoutCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorSetLayoutCreateInfo where
        type StructFields VkDescriptorSetLayoutCreateInfo =
             '["sType", "pNext", "flags", "bindingCount", "pBindings"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetLayoutCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorSetLayoutCreateInfo where
        type FieldType "sType" VkDescriptorSetLayoutCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, sType}
        type FieldIsArray "sType" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorSetLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorSetLayoutCreateInfo where
        type FieldType "pNext" VkDescriptorSetLayoutCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, pNext}
        type FieldIsArray "pNext" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorSetLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorSetLayoutCreateInfo where
        type FieldType "flags" VkDescriptorSetLayoutCreateInfo =
             VkDescriptorSetLayoutCreateFlags
        type FieldOptional "flags" VkDescriptorSetLayoutCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, flags}
        type FieldIsArray "flags" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDescriptorSetLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "bindingCount" VkDescriptorSetLayoutCreateInfo where
        type FieldType "bindingCount" VkDescriptorSetLayoutCreateInfo =
             Word32
        type FieldOptional "bindingCount" VkDescriptorSetLayoutCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "bindingCount" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}
        type FieldIsArray "bindingCount" VkDescriptorSetLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}

instance {-# OVERLAPPING #-}
         CanReadField "bindingCount" VkDescriptorSetLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, bindingCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bindingCount" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}

instance {-# OVERLAPPING #-}
         HasField "pBindings" VkDescriptorSetLayoutCreateInfo where
        type FieldType "pBindings" VkDescriptorSetLayoutCreateInfo =
             Ptr VkDescriptorSetLayoutBinding
        type FieldOptional "pBindings" VkDescriptorSetLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pBindings" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, pBindings}
        type FieldIsArray "pBindings" VkDescriptorSetLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, pBindings}

instance {-# OVERLAPPING #-}
         CanReadField "pBindings" VkDescriptorSetLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, pBindings})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pBindings}

instance {-# OVERLAPPING #-}
         CanWriteField "pBindings" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pBindings}

instance Show VkDescriptorSetLayoutCreateInfo where
        showsPrec d x
          = showString "VkDescriptorSetLayoutCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "bindingCount = " .
                                  showsPrec d (getField @"bindingCount" x) .
                                    showString ", " .
                                      showString "pBindings = " .
                                        showsPrec d (getField @"pBindings" x) . showChar '}'

-- | > typedef struct VkDescriptorSetLayoutSupport {
--   >     VkStructureType sType;
--   >     void*            pNext;
--   >     VkBool32         supported;
--   > } VkDescriptorSetLayoutSupport;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetLayoutSupport VkDescriptorSetLayoutSupport registry at www.khronos.org>
data VkDescriptorSetLayoutSupport = VkDescriptorSetLayoutSupport## Addr##
                                                                  ByteArray##

instance Eq VkDescriptorSetLayoutSupport where
        (VkDescriptorSetLayoutSupport## a _) ==
          x@(VkDescriptorSetLayoutSupport## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetLayoutSupport where
        (VkDescriptorSetLayoutSupport## a _) `compare`
          x@(VkDescriptorSetLayoutSupport## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetLayoutSupport where
        sizeOf ~_ = #{size VkDescriptorSetLayoutSupport}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetLayoutSupport}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorSetLayoutSupport where
        unsafeAddr (VkDescriptorSetLayoutSupport## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorSetLayoutSupport## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetLayoutSupport##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorSetLayoutSupport where
        type StructFields VkDescriptorSetLayoutSupport =
             '["sType", "pNext", "supported"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetLayoutSupport = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetLayoutSupport = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorSetLayoutSupport where
        type FieldType "sType" VkDescriptorSetLayoutSupport =
             VkStructureType
        type FieldOptional "sType" VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorSetLayoutSupport =
             #{offset VkDescriptorSetLayoutSupport, sType}
        type FieldIsArray "sType" VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutSupport, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorSetLayoutSupport where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutSupport, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutSupport, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorSetLayoutSupport where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutSupport, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorSetLayoutSupport where
        type FieldType "pNext" VkDescriptorSetLayoutSupport = Ptr Void
        type FieldOptional "pNext" VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorSetLayoutSupport =
             #{offset VkDescriptorSetLayoutSupport, pNext}
        type FieldIsArray "pNext" VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutSupport, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorSetLayoutSupport where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutSupport, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutSupport, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorSetLayoutSupport where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutSupport, pNext}

instance {-# OVERLAPPING #-}
         HasField "supported" VkDescriptorSetLayoutSupport where
        type FieldType "supported" VkDescriptorSetLayoutSupport = VkBool32
        type FieldOptional "supported" VkDescriptorSetLayoutSupport =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "supported" VkDescriptorSetLayoutSupport =
             #{offset VkDescriptorSetLayoutSupport, supported}
        type FieldIsArray "supported" VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutSupport, supported}

instance {-# OVERLAPPING #-}
         CanReadField "supported" VkDescriptorSetLayoutSupport where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutSupport, supported})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutSupport, supported}

instance {-# OVERLAPPING #-}
         CanWriteField "supported" VkDescriptorSetLayoutSupport where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutSupport, supported}

instance Show VkDescriptorSetLayoutSupport where
        showsPrec d x
          = showString "VkDescriptorSetLayoutSupport {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "supported = " .
                            showsPrec d (getField @"supported" x) . showChar '}'

-- | Alias for `VkDescriptorSetLayoutSupport`
type VkDescriptorSetLayoutSupportKHR = VkDescriptorSetLayoutSupport

-- | > typedef struct VkDescriptorSetVariableDescriptorCountAllocateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               descriptorSetCount;
--   >     const uint32_t* pDescriptorCounts;
--   > } VkDescriptorSetVariableDescriptorCountAllocateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetVariableDescriptorCountAllocateInfoEXT VkDescriptorSetVariableDescriptorCountAllocateInfoEXT registry at www.khronos.org>
data VkDescriptorSetVariableDescriptorCountAllocateInfoEXT = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## Addr##
                                                                                                                    ByteArray##

instance Eq VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## a _) ==
          x@(VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## a _)
          `compare`
          x@(VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        sizeOf ~_
          = #{size VkDescriptorSetVariableDescriptorCountAllocateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetVariableDescriptorCountAllocateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        unsafeAddr
          (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        type StructFields
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = '["sType", "pNext", "descriptorSetCount", "pDescriptorCounts"] -- ' closing tick for hsc2hs
        type CUnionType
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = '[VkDescriptorSetAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        type FieldType "sType"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType}
        type FieldIsArray "sType"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        type FieldType "pNext"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "descriptorSetCount"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        type FieldType "descriptorSetCount"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = Word32
        type FieldOptional "descriptorSetCount"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSetCount"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount}
        type FieldIsArray "descriptorSetCount"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorSetCount"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorSetCount"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount}

instance {-# OVERLAPPING #-}
         HasField "pDescriptorCounts"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        type FieldType "pDescriptorCounts"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = Ptr Word32
        type FieldOptional "pDescriptorCounts"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDescriptorCounts"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts}
        type FieldIsArray "pDescriptorCounts"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts}

instance {-# OVERLAPPING #-}
         CanReadField "pDescriptorCounts"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "pDescriptorCounts"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts}

instance Show VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        showsPrec d x
          = showString
              "VkDescriptorSetVariableDescriptorCountAllocateInfoEXT {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "descriptorSetCount = " .
                            showsPrec d (getField @"descriptorSetCount" x) .
                              showString ", " .
                                showString "pDescriptorCounts = " .
                                  showsPrec d (getField @"pDescriptorCounts" x) . showChar '}'

-- | > typedef struct VkDescriptorSetVariableDescriptorCountLayoutSupportEXT {
--   >     VkStructureType sType;
--   >     void*            pNext;
--   >     uint32_t         maxVariableDescriptorCount;
--   > } VkDescriptorSetVariableDescriptorCountLayoutSupportEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetVariableDescriptorCountLayoutSupportEXT VkDescriptorSetVariableDescriptorCountLayoutSupportEXT registry at www.khronos.org>
data VkDescriptorSetVariableDescriptorCountLayoutSupportEXT = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## Addr##
                                                                                                                      ByteArray##

instance Eq VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## a _) ==
          x@(VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## a _)
          `compare`
          x@(VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        sizeOf ~_
          = #{size VkDescriptorSetVariableDescriptorCountLayoutSupportEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetVariableDescriptorCountLayoutSupportEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        unsafeAddr
          (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        type StructFields
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = '["sType", "pNext", "maxVariableDescriptorCount"] -- ' closing tick for hsc2hs
        type CUnionType
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'True -- ' closing tick for hsc2hs
        type StructExtends
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = '[VkDescriptorSetLayoutSupport] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        type FieldType "sType"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = VkStructureType
        type FieldOptional "sType"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType}
        type FieldIsArray "sType"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        type FieldType "pNext"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext}
        type FieldIsArray "pNext"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxVariableDescriptorCount"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        type FieldType "maxVariableDescriptorCount"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = Word32
        type FieldOptional "maxVariableDescriptorCount"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxVariableDescriptorCount"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount}
        type FieldIsArray "maxVariableDescriptorCount"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxVariableDescriptorCount"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxVariableDescriptorCount"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount}

instance Show
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        showsPrec d x
          = showString
              "VkDescriptorSetVariableDescriptorCountLayoutSupportEXT {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxVariableDescriptorCount = " .
                            showsPrec d (getField @"maxVariableDescriptorCount" x) .
                              showChar '}'

-- | > typedef struct VkDescriptorUpdateTemplateCreateInfo {
--   >     VkStructureType sType;
--   >     void*                                   pNext;
--   >     VkDescriptorUpdateTemplateCreateFlags    flags;
--   >     uint32_t                 descriptorUpdateEntryCount;
--   >     const VkDescriptorUpdateTemplateEntry* pDescriptorUpdateEntries;
--   >     VkDescriptorUpdateTemplateType templateType;
--   >     VkDescriptorSetLayout descriptorSetLayout;
--   >     VkPipelineBindPoint pipelineBindPoint;
--   >     VkPipelineLayoutpipelineLayout;
--   >     uint32_t set;
--   > } VkDescriptorUpdateTemplateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorUpdateTemplateCreateInfo VkDescriptorUpdateTemplateCreateInfo registry at www.khronos.org>
data VkDescriptorUpdateTemplateCreateInfo = VkDescriptorUpdateTemplateCreateInfo## Addr##
                                                                                  ByteArray##

instance Eq VkDescriptorUpdateTemplateCreateInfo where
        (VkDescriptorUpdateTemplateCreateInfo## a _) ==
          x@(VkDescriptorUpdateTemplateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorUpdateTemplateCreateInfo where
        (VkDescriptorUpdateTemplateCreateInfo## a _) `compare`
          x@(VkDescriptorUpdateTemplateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorUpdateTemplateCreateInfo where
        sizeOf ~_
          = #{size VkDescriptorUpdateTemplateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorUpdateTemplateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorUpdateTemplateCreateInfo
         where
        unsafeAddr (VkDescriptorUpdateTemplateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorUpdateTemplateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorUpdateTemplateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorUpdateTemplateCreateInfo where
        type StructFields VkDescriptorUpdateTemplateCreateInfo =
             '["sType", "pNext", "flags", "descriptorUpdateEntryCount", -- ' closing tick for hsc2hs
               "pDescriptorUpdateEntries", "templateType", "descriptorSetLayout",
               "pipelineBindPoint", "pipelineLayout", "set"]
        type CUnionType VkDescriptorUpdateTemplateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorUpdateTemplateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorUpdateTemplateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorUpdateTemplateCreateInfo where
        type FieldType "sType" VkDescriptorUpdateTemplateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorUpdateTemplateCreateInfo =
             #{offset VkDescriptorUpdateTemplateCreateInfo, sType}
        type FieldIsArray "sType" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorUpdateTemplateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorUpdateTemplateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorUpdateTemplateCreateInfo where
        type FieldType "pNext" VkDescriptorUpdateTemplateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorUpdateTemplateCreateInfo =
             #{offset VkDescriptorUpdateTemplateCreateInfo, pNext}
        type FieldIsArray "pNext" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorUpdateTemplateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorUpdateTemplateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorUpdateTemplateCreateInfo where
        type FieldType "flags" VkDescriptorUpdateTemplateCreateInfo =
             VkDescriptorUpdateTemplateCreateFlags
        type FieldOptional "flags" VkDescriptorUpdateTemplateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorUpdateTemplateCreateInfo =
             #{offset VkDescriptorUpdateTemplateCreateInfo, flags}
        type FieldIsArray "flags" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDescriptorUpdateTemplateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDescriptorUpdateTemplateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfo
         where
        type FieldType "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfo
             = Word32
        type FieldOptional "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount}
        type FieldIsArray "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         HasField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfo
         where
        type FieldType "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfo
             = Ptr VkDescriptorUpdateTemplateEntry
        type FieldOptional "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries}
        type FieldIsArray "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         CanReadField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         CanWriteField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         HasField "templateType" VkDescriptorUpdateTemplateCreateInfo where
        type FieldType "templateType" VkDescriptorUpdateTemplateCreateInfo
             = VkDescriptorUpdateTemplateType
        type FieldOptional "templateType"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "templateType"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, templateType}
        type FieldIsArray "templateType"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, templateType}

instance {-# OVERLAPPING #-}
         CanReadField "templateType" VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, templateType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, templateType}

instance {-# OVERLAPPING #-}
         CanWriteField "templateType" VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, templateType}

instance {-# OVERLAPPING #-}
         HasField "descriptorSetLayout" VkDescriptorUpdateTemplateCreateInfo
         where
        type FieldType "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = VkDescriptorSetLayout
        type FieldOptional "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout}
        type FieldIsArray "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         HasField "pipelineBindPoint" VkDescriptorUpdateTemplateCreateInfo
         where
        type FieldType "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfo
             = VkPipelineBindPoint
        type FieldOptional "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint}
        type FieldIsArray "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         HasField "pipelineLayout" VkDescriptorUpdateTemplateCreateInfo
         where
        type FieldType "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = VkPipelineLayout
        type FieldOptional "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout}
        type FieldIsArray "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineLayout" VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineLayout" VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout}

instance {-# OVERLAPPING #-}
         HasField "set" VkDescriptorUpdateTemplateCreateInfo where
        type FieldType "set" VkDescriptorUpdateTemplateCreateInfo = Word32
        type FieldOptional "set" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "set" VkDescriptorUpdateTemplateCreateInfo =
             #{offset VkDescriptorUpdateTemplateCreateInfo, set}
        type FieldIsArray "set" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, set}

instance {-# OVERLAPPING #-}
         CanReadField "set" VkDescriptorUpdateTemplateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, set})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, set}

instance {-# OVERLAPPING #-}
         CanWriteField "set" VkDescriptorUpdateTemplateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, set}

instance Show VkDescriptorUpdateTemplateCreateInfo where
        showsPrec d x
          = showString "VkDescriptorUpdateTemplateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "descriptorUpdateEntryCount = " .
                                  showsPrec d (getField @"descriptorUpdateEntryCount" x) .
                                    showString ", " .
                                      showString "pDescriptorUpdateEntries = " .
                                        showsPrec d (getField @"pDescriptorUpdateEntries" x) .
                                          showString ", " .
                                            showString "templateType = " .
                                              showsPrec d (getField @"templateType" x) .
                                                showString ", " .
                                                  showString "descriptorSetLayout = " .
                                                    showsPrec d (getField @"descriptorSetLayout" x)
                                                      .
                                                      showString ", " .
                                                        showString "pipelineBindPoint = " .
                                                          showsPrec d
                                                            (getField @"pipelineBindPoint" x)
                                                            .
                                                            showString ", " .
                                                              showString "pipelineLayout = " .
                                                                showsPrec d
                                                                  (getField @"pipelineLayout" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "set = " .
                                                                      showsPrec d
                                                                        (getField @"set" x)
                                                                        . showChar '}'

-- | Alias for `VkDescriptorUpdateTemplateCreateInfo`
type VkDescriptorUpdateTemplateCreateInfoKHR =
     VkDescriptorUpdateTemplateCreateInfo

-- | > typedef struct VkDescriptorUpdateTemplateEntry {
--   >     uint32_t                         dstBinding;
--   >     uint32_t                         dstArrayElement;
--   >     uint32_t                         descriptorCount;
--   >     VkDescriptorType                 descriptorType;
--   >     size_t                           offset;
--   >     size_t                           stride;
--   > } VkDescriptorUpdateTemplateEntry;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorUpdateTemplateEntry VkDescriptorUpdateTemplateEntry registry at www.khronos.org>
data VkDescriptorUpdateTemplateEntry = VkDescriptorUpdateTemplateEntry## Addr##
                                                                        ByteArray##

instance Eq VkDescriptorUpdateTemplateEntry where
        (VkDescriptorUpdateTemplateEntry## a _) ==
          x@(VkDescriptorUpdateTemplateEntry## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorUpdateTemplateEntry where
        (VkDescriptorUpdateTemplateEntry## a _) `compare`
          x@(VkDescriptorUpdateTemplateEntry## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorUpdateTemplateEntry where
        sizeOf ~_ = #{size VkDescriptorUpdateTemplateEntry}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorUpdateTemplateEntry}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorUpdateTemplateEntry where
        unsafeAddr (VkDescriptorUpdateTemplateEntry## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorUpdateTemplateEntry## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorUpdateTemplateEntry##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorUpdateTemplateEntry where
        type StructFields VkDescriptorUpdateTemplateEntry =
             '["dstBinding", "dstArrayElement", "descriptorCount", -- ' closing tick for hsc2hs
               "descriptorType", "offset", "stride"]
        type CUnionType VkDescriptorUpdateTemplateEntry = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorUpdateTemplateEntry = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorUpdateTemplateEntry = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "dstBinding" VkDescriptorUpdateTemplateEntry where
        type FieldType "dstBinding" VkDescriptorUpdateTemplateEntry =
             Word32
        type FieldOptional "dstBinding" VkDescriptorUpdateTemplateEntry =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dstBinding" VkDescriptorUpdateTemplateEntry =
             #{offset VkDescriptorUpdateTemplateEntry, dstBinding}
        type FieldIsArray "dstBinding" VkDescriptorUpdateTemplateEntry =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, dstBinding}

instance {-# OVERLAPPING #-}
         CanReadField "dstBinding" VkDescriptorUpdateTemplateEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, dstBinding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, dstBinding}

instance {-# OVERLAPPING #-}
         CanWriteField "dstBinding" VkDescriptorUpdateTemplateEntry where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, dstBinding}

instance {-# OVERLAPPING #-}
         HasField "dstArrayElement" VkDescriptorUpdateTemplateEntry where
        type FieldType "dstArrayElement" VkDescriptorUpdateTemplateEntry =
             Word32
        type FieldOptional "dstArrayElement"
               VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstArrayElement" VkDescriptorUpdateTemplateEntry
             =
             #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement}
        type FieldIsArray "dstArrayElement" VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement}

instance {-# OVERLAPPING #-}
         CanReadField "dstArrayElement" VkDescriptorUpdateTemplateEntry
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement}

instance {-# OVERLAPPING #-}
         CanWriteField "dstArrayElement" VkDescriptorUpdateTemplateEntry
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement}

instance {-# OVERLAPPING #-}
         HasField "descriptorCount" VkDescriptorUpdateTemplateEntry where
        type FieldType "descriptorCount" VkDescriptorUpdateTemplateEntry =
             Word32
        type FieldOptional "descriptorCount"
               VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorCount" VkDescriptorUpdateTemplateEntry
             =
             #{offset VkDescriptorUpdateTemplateEntry, descriptorCount}
        type FieldIsArray "descriptorCount" VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, descriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorCount" VkDescriptorUpdateTemplateEntry
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, descriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, descriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorCount" VkDescriptorUpdateTemplateEntry
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, descriptorCount}

instance {-# OVERLAPPING #-}
         HasField "descriptorType" VkDescriptorUpdateTemplateEntry where
        type FieldType "descriptorType" VkDescriptorUpdateTemplateEntry =
             VkDescriptorType
        type FieldOptional "descriptorType" VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorType" VkDescriptorUpdateTemplateEntry =
             #{offset VkDescriptorUpdateTemplateEntry, descriptorType}
        type FieldIsArray "descriptorType" VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, descriptorType}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorType" VkDescriptorUpdateTemplateEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, descriptorType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, descriptorType}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorType" VkDescriptorUpdateTemplateEntry
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, descriptorType}

instance {-# OVERLAPPING #-}
         HasField "offset" VkDescriptorUpdateTemplateEntry where
        type FieldType "offset" VkDescriptorUpdateTemplateEntry = CSize
        type FieldOptional "offset" VkDescriptorUpdateTemplateEntry =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkDescriptorUpdateTemplateEntry =
             #{offset VkDescriptorUpdateTemplateEntry, offset}
        type FieldIsArray "offset" VkDescriptorUpdateTemplateEntry = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkDescriptorUpdateTemplateEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkDescriptorUpdateTemplateEntry where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, offset}

instance {-# OVERLAPPING #-}
         HasField "stride" VkDescriptorUpdateTemplateEntry where
        type FieldType "stride" VkDescriptorUpdateTemplateEntry = CSize
        type FieldOptional "stride" VkDescriptorUpdateTemplateEntry =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stride" VkDescriptorUpdateTemplateEntry =
             #{offset VkDescriptorUpdateTemplateEntry, stride}
        type FieldIsArray "stride" VkDescriptorUpdateTemplateEntry = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, stride}

instance {-# OVERLAPPING #-}
         CanReadField "stride" VkDescriptorUpdateTemplateEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, stride})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, stride}

instance {-# OVERLAPPING #-}
         CanWriteField "stride" VkDescriptorUpdateTemplateEntry where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, stride}

instance Show VkDescriptorUpdateTemplateEntry where
        showsPrec d x
          = showString "VkDescriptorUpdateTemplateEntry {" .
              showString "dstBinding = " .
                showsPrec d (getField @"dstBinding" x) .
                  showString ", " .
                    showString "dstArrayElement = " .
                      showsPrec d (getField @"dstArrayElement" x) .
                        showString ", " .
                          showString "descriptorCount = " .
                            showsPrec d (getField @"descriptorCount" x) .
                              showString ", " .
                                showString "descriptorType = " .
                                  showsPrec d (getField @"descriptorType" x) .
                                    showString ", " .
                                      showString "offset = " .
                                        showsPrec d (getField @"offset" x) .
                                          showString ", " .
                                            showString "stride = " .
                                              showsPrec d (getField @"stride" x) . showChar '}'

-- | Alias for `VkDescriptorUpdateTemplateEntry`
type VkDescriptorUpdateTemplateEntryKHR =
     VkDescriptorUpdateTemplateEntry
