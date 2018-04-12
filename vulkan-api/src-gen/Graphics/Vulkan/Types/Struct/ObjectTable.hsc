#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ObjectTable
       (VkObjectTableCreateInfoNVX(..),
        VkObjectTableDescriptorSetEntryNVX(..), VkObjectTableEntryNVX(..),
        VkObjectTableIndexBufferEntryNVX(..),
        VkObjectTablePipelineEntryNVX(..),
        VkObjectTablePushConstantEntryNVX(..),
        VkObjectTableVertexBufferEntryNVX(..))
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.IndexType     (VkIndexType)
import           Graphics.Vulkan.Types.Enum.Object        (VkObjectEntryTypeNVX, VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.Enum.Shader        (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer,
                                                           VkDescriptorSet,
                                                           VkPipeline,
                                                           VkPipelineLayout)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkObjectTableCreateInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                                          objectCount;
--   >     const VkObjectEntryTypeNVX*       pObjectEntryTypes;
--   >     const uint32_t*                   pObjectEntryCounts;
--   >     const VkObjectEntryUsageFlagsNVX* pObjectEntryUsageFlags;
--   >     uint32_t maxUniformBuffersPerDescriptor;
--   >     uint32_t maxStorageBuffersPerDescriptor;
--   >     uint32_t maxStorageImagesPerDescriptor;
--   >     uint32_t maxSampledImagesPerDescriptor;
--   >     uint32_t maxPipelineLayouts;
--   > } VkObjectTableCreateInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTableCreateInfoNVX VkObjectTableCreateInfoNVX registry at www.khronos.org>
data VkObjectTableCreateInfoNVX = VkObjectTableCreateInfoNVX## Addr##
                                                              ByteArray##

instance Eq VkObjectTableCreateInfoNVX where
        (VkObjectTableCreateInfoNVX## a _) ==
          x@(VkObjectTableCreateInfoNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableCreateInfoNVX where
        (VkObjectTableCreateInfoNVX## a _) `compare`
          x@(VkObjectTableCreateInfoNVX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTableCreateInfoNVX where
        sizeOf ~_ = #{size VkObjectTableCreateInfoNVX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkObjectTableCreateInfoNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTableCreateInfoNVX where
        unsafeAddr (VkObjectTableCreateInfoNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTableCreateInfoNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTableCreateInfoNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTableCreateInfoNVX where
        type StructFields VkObjectTableCreateInfoNVX =
             '["sType", "pNext", "objectCount", "pObjectEntryTypes", -- ' closing tick for hsc2hs
               "pObjectEntryCounts", "pObjectEntryUsageFlags",
               "maxUniformBuffersPerDescriptor", "maxStorageBuffersPerDescriptor",
               "maxStorageImagesPerDescriptor", "maxSampledImagesPerDescriptor",
               "maxPipelineLayouts"]
        type CUnionType VkObjectTableCreateInfoNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTableCreateInfoNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTableCreateInfoNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkObjectTableCreateInfoNVX where
        type FieldType "sType" VkObjectTableCreateInfoNVX = VkStructureType
        type FieldOptional "sType" VkObjectTableCreateInfoNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkObjectTableCreateInfoNVX =
             #{offset VkObjectTableCreateInfoNVX, sType}
        type FieldIsArray "sType" VkObjectTableCreateInfoNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkObjectTableCreateInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkObjectTableCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkObjectTableCreateInfoNVX where
        type FieldType "pNext" VkObjectTableCreateInfoNVX = Ptr Void
        type FieldOptional "pNext" VkObjectTableCreateInfoNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkObjectTableCreateInfoNVX =
             #{offset VkObjectTableCreateInfoNVX, pNext}
        type FieldIsArray "pNext" VkObjectTableCreateInfoNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkObjectTableCreateInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkObjectTableCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "objectCount" VkObjectTableCreateInfoNVX where
        type FieldType "objectCount" VkObjectTableCreateInfoNVX = Word32
        type FieldOptional "objectCount" VkObjectTableCreateInfoNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectCount" VkObjectTableCreateInfoNVX =
             #{offset VkObjectTableCreateInfoNVX, objectCount}
        type FieldIsArray "objectCount" VkObjectTableCreateInfoNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, objectCount}

instance {-# OVERLAPPING #-}
         CanReadField "objectCount" VkObjectTableCreateInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, objectCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, objectCount}

instance {-# OVERLAPPING #-}
         CanWriteField "objectCount" VkObjectTableCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, objectCount}

instance {-# OVERLAPPING #-}
         HasField "pObjectEntryTypes" VkObjectTableCreateInfoNVX where
        type FieldType "pObjectEntryTypes" VkObjectTableCreateInfoNVX =
             Ptr VkObjectEntryTypeNVX
        type FieldOptional "pObjectEntryTypes" VkObjectTableCreateInfoNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pObjectEntryTypes" VkObjectTableCreateInfoNVX =
             #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}
        type FieldIsArray "pObjectEntryTypes" VkObjectTableCreateInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}

instance {-# OVERLAPPING #-}
         CanReadField "pObjectEntryTypes" VkObjectTableCreateInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "pObjectEntryTypes" VkObjectTableCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}

instance {-# OVERLAPPING #-}
         HasField "pObjectEntryCounts" VkObjectTableCreateInfoNVX where
        type FieldType "pObjectEntryCounts" VkObjectTableCreateInfoNVX =
             Ptr Word32
        type FieldOptional "pObjectEntryCounts" VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pObjectEntryCounts" VkObjectTableCreateInfoNVX =
             #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}
        type FieldIsArray "pObjectEntryCounts" VkObjectTableCreateInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}

instance {-# OVERLAPPING #-}
         CanReadField "pObjectEntryCounts" VkObjectTableCreateInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "pObjectEntryCounts" VkObjectTableCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}

instance {-# OVERLAPPING #-}
         HasField "pObjectEntryUsageFlags" VkObjectTableCreateInfoNVX where
        type FieldType "pObjectEntryUsageFlags" VkObjectTableCreateInfoNVX
             = Ptr VkObjectEntryUsageFlagsNVX
        type FieldOptional "pObjectEntryUsageFlags"
               VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pObjectEntryUsageFlags"
               VkObjectTableCreateInfoNVX
             =
             #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}
        type FieldIsArray "pObjectEntryUsageFlags"
               VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}

instance {-# OVERLAPPING #-}
         CanReadField "pObjectEntryUsageFlags" VkObjectTableCreateInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "pObjectEntryUsageFlags" VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}

instance {-# OVERLAPPING #-}
         HasField "maxUniformBuffersPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        type FieldType "maxUniformBuffersPerDescriptor"
               VkObjectTableCreateInfoNVX
             = Word32
        type FieldOptional "maxUniformBuffersPerDescriptor"
               VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxUniformBuffersPerDescriptor"
               VkObjectTableCreateInfoNVX
             =
             #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}
        type FieldIsArray "maxUniformBuffersPerDescriptor"
               VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}

instance {-# OVERLAPPING #-}
         CanReadField "maxUniformBuffersPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}

instance {-# OVERLAPPING #-}
         CanWriteField "maxUniformBuffersPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}

instance {-# OVERLAPPING #-}
         HasField "maxStorageBuffersPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        type FieldType "maxStorageBuffersPerDescriptor"
               VkObjectTableCreateInfoNVX
             = Word32
        type FieldOptional "maxStorageBuffersPerDescriptor"
               VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxStorageBuffersPerDescriptor"
               VkObjectTableCreateInfoNVX
             =
             #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}
        type FieldIsArray "maxStorageBuffersPerDescriptor"
               VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}

instance {-# OVERLAPPING #-}
         CanReadField "maxStorageBuffersPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}

instance {-# OVERLAPPING #-}
         CanWriteField "maxStorageBuffersPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}

instance {-# OVERLAPPING #-}
         HasField "maxStorageImagesPerDescriptor" VkObjectTableCreateInfoNVX
         where
        type FieldType "maxStorageImagesPerDescriptor"
               VkObjectTableCreateInfoNVX
             = Word32
        type FieldOptional "maxStorageImagesPerDescriptor"
               VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxStorageImagesPerDescriptor"
               VkObjectTableCreateInfoNVX
             =
             #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}
        type FieldIsArray "maxStorageImagesPerDescriptor"
               VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}

instance {-# OVERLAPPING #-}
         CanReadField "maxStorageImagesPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}

instance {-# OVERLAPPING #-}
         CanWriteField "maxStorageImagesPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}

instance {-# OVERLAPPING #-}
         HasField "maxSampledImagesPerDescriptor" VkObjectTableCreateInfoNVX
         where
        type FieldType "maxSampledImagesPerDescriptor"
               VkObjectTableCreateInfoNVX
             = Word32
        type FieldOptional "maxSampledImagesPerDescriptor"
               VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSampledImagesPerDescriptor"
               VkObjectTableCreateInfoNVX
             =
             #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}
        type FieldIsArray "maxSampledImagesPerDescriptor"
               VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}

instance {-# OVERLAPPING #-}
         CanReadField "maxSampledImagesPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSampledImagesPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}

instance {-# OVERLAPPING #-}
         HasField "maxPipelineLayouts" VkObjectTableCreateInfoNVX where
        type FieldType "maxPipelineLayouts" VkObjectTableCreateInfoNVX =
             Word32
        type FieldOptional "maxPipelineLayouts" VkObjectTableCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPipelineLayouts" VkObjectTableCreateInfoNVX =
             #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}
        type FieldIsArray "maxPipelineLayouts" VkObjectTableCreateInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}

instance {-# OVERLAPPING #-}
         CanReadField "maxPipelineLayouts" VkObjectTableCreateInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPipelineLayouts" VkObjectTableCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}

instance Show VkObjectTableCreateInfoNVX where
        showsPrec d x
          = showString "VkObjectTableCreateInfoNVX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "objectCount = " .
                            showsPrec d (getField @"objectCount" x) .
                              showString ", " .
                                showString "pObjectEntryTypes = " .
                                  showsPrec d (getField @"pObjectEntryTypes" x) .
                                    showString ", " .
                                      showString "pObjectEntryCounts = " .
                                        showsPrec d (getField @"pObjectEntryCounts" x) .
                                          showString ", " .
                                            showString "pObjectEntryUsageFlags = " .
                                              showsPrec d (getField @"pObjectEntryUsageFlags" x) .
                                                showString ", " .
                                                  showString "maxUniformBuffersPerDescriptor = " .
                                                    showsPrec d
                                                      (getField @"maxUniformBuffersPerDescriptor" x)
                                                      .
                                                      showString ", " .
                                                        showString
                                                          "maxStorageBuffersPerDescriptor = "
                                                          .
                                                          showsPrec d
                                                            (getField
                                                               @"maxStorageBuffersPerDescriptor"
                                                               x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "maxStorageImagesPerDescriptor = "
                                                                .
                                                                showsPrec d
                                                                  (getField
                                                                     @"maxStorageImagesPerDescriptor"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "maxSampledImagesPerDescriptor = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"maxSampledImagesPerDescriptor"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "maxPipelineLayouts = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"maxPipelineLayouts"
                                                                                 x)
                                                                              . showChar '}'

-- | > typedef struct VkObjectTableDescriptorSetEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipelineLayout             pipelineLayout;
--   >     VkDescriptorSet              descriptorSet;
--   > } VkObjectTableDescriptorSetEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTableDescriptorSetEntryNVX VkObjectTableDescriptorSetEntryNVX registry at www.khronos.org>
data VkObjectTableDescriptorSetEntryNVX = VkObjectTableDescriptorSetEntryNVX## Addr##
                                                                              ByteArray##

instance Eq VkObjectTableDescriptorSetEntryNVX where
        (VkObjectTableDescriptorSetEntryNVX## a _) ==
          x@(VkObjectTableDescriptorSetEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableDescriptorSetEntryNVX where
        (VkObjectTableDescriptorSetEntryNVX## a _) `compare`
          x@(VkObjectTableDescriptorSetEntryNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTableDescriptorSetEntryNVX where
        sizeOf ~_ = #{size VkObjectTableDescriptorSetEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTableDescriptorSetEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTableDescriptorSetEntryNVX where
        unsafeAddr (VkObjectTableDescriptorSetEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTableDescriptorSetEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTableDescriptorSetEntryNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTableDescriptorSetEntryNVX where
        type StructFields VkObjectTableDescriptorSetEntryNVX =
             '["type", "flags", "pipelineLayout", "descriptorSet"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTableDescriptorSetEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTableDescriptorSetEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTableDescriptorSetEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "type" VkObjectTableDescriptorSetEntryNVX where
        type FieldType "type" VkObjectTableDescriptorSetEntryNVX =
             VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTableDescriptorSetEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTableDescriptorSetEntryNVX =
             #{offset VkObjectTableDescriptorSetEntryNVX, type}
        type FieldIsArray "type" VkObjectTableDescriptorSetEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableDescriptorSetEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTableDescriptorSetEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTableDescriptorSetEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasField "flags" VkObjectTableDescriptorSetEntryNVX where
        type FieldType "flags" VkObjectTableDescriptorSetEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTableDescriptorSetEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTableDescriptorSetEntryNVX =
             #{offset VkObjectTableDescriptorSetEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTableDescriptorSetEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableDescriptorSetEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTableDescriptorSetEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTableDescriptorSetEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasField "pipelineLayout" VkObjectTableDescriptorSetEntryNVX where
        type FieldType "pipelineLayout" VkObjectTableDescriptorSetEntryNVX
             = VkPipelineLayout
        type FieldOptional "pipelineLayout"
               VkObjectTableDescriptorSetEntryNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineLayout"
               VkObjectTableDescriptorSetEntryNVX
             =
             #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}
        type FieldIsArray "pipelineLayout"
               VkObjectTableDescriptorSetEntryNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineLayout" VkObjectTableDescriptorSetEntryNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineLayout" VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         HasField "descriptorSet" VkObjectTableDescriptorSetEntryNVX where
        type FieldType "descriptorSet" VkObjectTableDescriptorSetEntryNVX =
             VkDescriptorSet
        type FieldOptional "descriptorSet"
               VkObjectTableDescriptorSetEntryNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSet" VkObjectTableDescriptorSetEntryNVX
             =
             #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}
        type FieldIsArray "descriptorSet"
               VkObjectTableDescriptorSetEntryNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorSet" VkObjectTableDescriptorSetEntryNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorSet" VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

instance Show VkObjectTableDescriptorSetEntryNVX where
        showsPrec d x
          = showString "VkObjectTableDescriptorSetEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) .
                        showString ", " .
                          showString "pipelineLayout = " .
                            showsPrec d (getField @"pipelineLayout" x) .
                              showString ", " .
                                showString "descriptorSet = " .
                                  showsPrec d (getField @"descriptorSet" x) . showChar '}'

-- | > typedef struct VkObjectTableEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   > } VkObjectTableEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTableEntryNVX VkObjectTableEntryNVX registry at www.khronos.org>
data VkObjectTableEntryNVX = VkObjectTableEntryNVX## Addr##
                                                    ByteArray##

instance Eq VkObjectTableEntryNVX where
        (VkObjectTableEntryNVX## a _) == x@(VkObjectTableEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableEntryNVX where
        (VkObjectTableEntryNVX## a _) `compare`
          x@(VkObjectTableEntryNVX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTableEntryNVX where
        sizeOf ~_ = #{size VkObjectTableEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkObjectTableEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTableEntryNVX where
        unsafeAddr (VkObjectTableEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTableEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTableEntryNVX## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTableEntryNVX where
        type StructFields VkObjectTableEntryNVX = '["type", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTableEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "type" VkObjectTableEntryNVX
         where
        type FieldType "type" VkObjectTableEntryNVX = VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTableEntryNVX =
             #{offset VkObjectTableEntryNVX, type}
        type FieldIsArray "type" VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkObjectTableEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTableEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTableEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableEntryNVX, type}

instance {-# OVERLAPPING #-} HasField "flags" VkObjectTableEntryNVX
         where
        type FieldType "flags" VkObjectTableEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTableEntryNVX =
             #{offset VkObjectTableEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkObjectTableEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTableEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTableEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableEntryNVX, flags}

instance Show VkObjectTableEntryNVX where
        showsPrec d x
          = showString "VkObjectTableEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) . showChar '}'

-- | > typedef struct VkObjectTableIndexBufferEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkBuffer                     buffer;
--   >     VkIndexType                  indexType;
--   > } VkObjectTableIndexBufferEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTableIndexBufferEntryNVX VkObjectTableIndexBufferEntryNVX registry at www.khronos.org>
data VkObjectTableIndexBufferEntryNVX = VkObjectTableIndexBufferEntryNVX## Addr##
                                                                          ByteArray##

instance Eq VkObjectTableIndexBufferEntryNVX where
        (VkObjectTableIndexBufferEntryNVX## a _) ==
          x@(VkObjectTableIndexBufferEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableIndexBufferEntryNVX where
        (VkObjectTableIndexBufferEntryNVX## a _) `compare`
          x@(VkObjectTableIndexBufferEntryNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTableIndexBufferEntryNVX where
        sizeOf ~_ = #{size VkObjectTableIndexBufferEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTableIndexBufferEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTableIndexBufferEntryNVX where
        unsafeAddr (VkObjectTableIndexBufferEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTableIndexBufferEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTableIndexBufferEntryNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTableIndexBufferEntryNVX where
        type StructFields VkObjectTableIndexBufferEntryNVX =
             '["type", "flags", "buffer", "indexType"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTableIndexBufferEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTableIndexBufferEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTableIndexBufferEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "type" VkObjectTableIndexBufferEntryNVX where
        type FieldType "type" VkObjectTableIndexBufferEntryNVX =
             VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTableIndexBufferEntryNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTableIndexBufferEntryNVX =
             #{offset VkObjectTableIndexBufferEntryNVX, type}
        type FieldIsArray "type" VkObjectTableIndexBufferEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableIndexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTableIndexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTableIndexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasField "flags" VkObjectTableIndexBufferEntryNVX where
        type FieldType "flags" VkObjectTableIndexBufferEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTableIndexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTableIndexBufferEntryNVX =
             #{offset VkObjectTableIndexBufferEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTableIndexBufferEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableIndexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTableIndexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTableIndexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkObjectTableIndexBufferEntryNVX where
        type FieldType "buffer" VkObjectTableIndexBufferEntryNVX = VkBuffer
        type FieldOptional "buffer" VkObjectTableIndexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkObjectTableIndexBufferEntryNVX =
             #{offset VkObjectTableIndexBufferEntryNVX, buffer}
        type FieldIsArray "buffer" VkObjectTableIndexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableIndexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkObjectTableIndexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkObjectTableIndexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         HasField "indexType" VkObjectTableIndexBufferEntryNVX where
        type FieldType "indexType" VkObjectTableIndexBufferEntryNVX =
             VkIndexType
        type FieldOptional "indexType" VkObjectTableIndexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "indexType" VkObjectTableIndexBufferEntryNVX =
             #{offset VkObjectTableIndexBufferEntryNVX, indexType}
        type FieldIsArray "indexType" VkObjectTableIndexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableIndexBufferEntryNVX, indexType}

instance {-# OVERLAPPING #-}
         CanReadField "indexType" VkObjectTableIndexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, indexType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, indexType}

instance {-# OVERLAPPING #-}
         CanWriteField "indexType" VkObjectTableIndexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, indexType}

instance Show VkObjectTableIndexBufferEntryNVX where
        showsPrec d x
          = showString "VkObjectTableIndexBufferEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) .
                              showString ", " .
                                showString "indexType = " .
                                  showsPrec d (getField @"indexType" x) . showChar '}'

-- | > typedef struct VkObjectTablePipelineEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipeline                   pipeline;
--   > } VkObjectTablePipelineEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTablePipelineEntryNVX VkObjectTablePipelineEntryNVX registry at www.khronos.org>
data VkObjectTablePipelineEntryNVX = VkObjectTablePipelineEntryNVX## Addr##
                                                                    ByteArray##

instance Eq VkObjectTablePipelineEntryNVX where
        (VkObjectTablePipelineEntryNVX## a _) ==
          x@(VkObjectTablePipelineEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTablePipelineEntryNVX where
        (VkObjectTablePipelineEntryNVX## a _) `compare`
          x@(VkObjectTablePipelineEntryNVX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTablePipelineEntryNVX where
        sizeOf ~_ = #{size VkObjectTablePipelineEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTablePipelineEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTablePipelineEntryNVX where
        unsafeAddr (VkObjectTablePipelineEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTablePipelineEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTablePipelineEntryNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTablePipelineEntryNVX where
        type StructFields VkObjectTablePipelineEntryNVX =
             '["type", "flags", "pipeline"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTablePipelineEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "type" VkObjectTablePipelineEntryNVX where
        type FieldType "type" VkObjectTablePipelineEntryNVX =
             VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTablePipelineEntryNVX =
             #{offset VkObjectTablePipelineEntryNVX, type}
        type FieldIsArray "type" VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePipelineEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTablePipelineEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePipelineEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePipelineEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTablePipelineEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePipelineEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasField "flags" VkObjectTablePipelineEntryNVX where
        type FieldType "flags" VkObjectTablePipelineEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTablePipelineEntryNVX =
             #{offset VkObjectTablePipelineEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePipelineEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTablePipelineEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePipelineEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePipelineEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTablePipelineEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePipelineEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasField "pipeline" VkObjectTablePipelineEntryNVX where
        type FieldType "pipeline" VkObjectTablePipelineEntryNVX =
             VkPipeline
        type FieldOptional "pipeline" VkObjectTablePipelineEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pipeline" VkObjectTablePipelineEntryNVX =
             #{offset VkObjectTablePipelineEntryNVX, pipeline}
        type FieldIsArray "pipeline" VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePipelineEntryNVX, pipeline}

instance {-# OVERLAPPING #-}
         CanReadField "pipeline" VkObjectTablePipelineEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePipelineEntryNVX, pipeline})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePipelineEntryNVX, pipeline}

instance {-# OVERLAPPING #-}
         CanWriteField "pipeline" VkObjectTablePipelineEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePipelineEntryNVX, pipeline}

instance Show VkObjectTablePipelineEntryNVX where
        showsPrec d x
          = showString "VkObjectTablePipelineEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) .
                        showString ", " .
                          showString "pipeline = " .
                            showsPrec d (getField @"pipeline" x) . showChar '}'

-- | > typedef struct VkObjectTablePushConstantEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipelineLayout             pipelineLayout;
--   >     VkShaderStageFlags           stageFlags;
--   > } VkObjectTablePushConstantEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTablePushConstantEntryNVX VkObjectTablePushConstantEntryNVX registry at www.khronos.org>
data VkObjectTablePushConstantEntryNVX = VkObjectTablePushConstantEntryNVX## Addr##
                                                                            ByteArray##

instance Eq VkObjectTablePushConstantEntryNVX where
        (VkObjectTablePushConstantEntryNVX## a _) ==
          x@(VkObjectTablePushConstantEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTablePushConstantEntryNVX where
        (VkObjectTablePushConstantEntryNVX## a _) `compare`
          x@(VkObjectTablePushConstantEntryNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTablePushConstantEntryNVX where
        sizeOf ~_ = #{size VkObjectTablePushConstantEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTablePushConstantEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTablePushConstantEntryNVX where
        unsafeAddr (VkObjectTablePushConstantEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTablePushConstantEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTablePushConstantEntryNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTablePushConstantEntryNVX where
        type StructFields VkObjectTablePushConstantEntryNVX =
             '["type", "flags", "pipelineLayout", "stageFlags"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTablePushConstantEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTablePushConstantEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTablePushConstantEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "type" VkObjectTablePushConstantEntryNVX where
        type FieldType "type" VkObjectTablePushConstantEntryNVX =
             VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTablePushConstantEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTablePushConstantEntryNVX =
             #{offset VkObjectTablePushConstantEntryNVX, type}
        type FieldIsArray "type" VkObjectTablePushConstantEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePushConstantEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTablePushConstantEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTablePushConstantEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasField "flags" VkObjectTablePushConstantEntryNVX where
        type FieldType "flags" VkObjectTablePushConstantEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTablePushConstantEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTablePushConstantEntryNVX =
             #{offset VkObjectTablePushConstantEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTablePushConstantEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePushConstantEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTablePushConstantEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTablePushConstantEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasField "pipelineLayout" VkObjectTablePushConstantEntryNVX where
        type FieldType "pipelineLayout" VkObjectTablePushConstantEntryNVX =
             VkPipelineLayout
        type FieldOptional "pipelineLayout"
               VkObjectTablePushConstantEntryNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineLayout" VkObjectTablePushConstantEntryNVX
             =
             #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}
        type FieldIsArray "pipelineLayout"
               VkObjectTablePushConstantEntryNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineLayout" VkObjectTablePushConstantEntryNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineLayout" VkObjectTablePushConstantEntryNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         HasField "stageFlags" VkObjectTablePushConstantEntryNVX where
        type FieldType "stageFlags" VkObjectTablePushConstantEntryNVX =
             VkShaderStageFlags
        type FieldOptional "stageFlags" VkObjectTablePushConstantEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stageFlags" VkObjectTablePushConstantEntryNVX =
             #{offset VkObjectTablePushConstantEntryNVX, stageFlags}
        type FieldIsArray "stageFlags" VkObjectTablePushConstantEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePushConstantEntryNVX, stageFlags}

instance {-# OVERLAPPING #-}
         CanReadField "stageFlags" VkObjectTablePushConstantEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, stageFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, stageFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "stageFlags" VkObjectTablePushConstantEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, stageFlags}

instance Show VkObjectTablePushConstantEntryNVX where
        showsPrec d x
          = showString "VkObjectTablePushConstantEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) .
                        showString ", " .
                          showString "pipelineLayout = " .
                            showsPrec d (getField @"pipelineLayout" x) .
                              showString ", " .
                                showString "stageFlags = " .
                                  showsPrec d (getField @"stageFlags" x) . showChar '}'

-- | > typedef struct VkObjectTableVertexBufferEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkBuffer                     buffer;
--   > } VkObjectTableVertexBufferEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTableVertexBufferEntryNVX VkObjectTableVertexBufferEntryNVX registry at www.khronos.org>
data VkObjectTableVertexBufferEntryNVX = VkObjectTableVertexBufferEntryNVX## Addr##
                                                                            ByteArray##

instance Eq VkObjectTableVertexBufferEntryNVX where
        (VkObjectTableVertexBufferEntryNVX## a _) ==
          x@(VkObjectTableVertexBufferEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableVertexBufferEntryNVX where
        (VkObjectTableVertexBufferEntryNVX## a _) `compare`
          x@(VkObjectTableVertexBufferEntryNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTableVertexBufferEntryNVX where
        sizeOf ~_ = #{size VkObjectTableVertexBufferEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTableVertexBufferEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTableVertexBufferEntryNVX where
        unsafeAddr (VkObjectTableVertexBufferEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTableVertexBufferEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTableVertexBufferEntryNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTableVertexBufferEntryNVX where
        type StructFields VkObjectTableVertexBufferEntryNVX =
             '["type", "flags", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTableVertexBufferEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTableVertexBufferEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTableVertexBufferEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "type" VkObjectTableVertexBufferEntryNVX where
        type FieldType "type" VkObjectTableVertexBufferEntryNVX =
             VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTableVertexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTableVertexBufferEntryNVX =
             #{offset VkObjectTableVertexBufferEntryNVX, type}
        type FieldIsArray "type" VkObjectTableVertexBufferEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableVertexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTableVertexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableVertexBufferEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableVertexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTableVertexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableVertexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasField "flags" VkObjectTableVertexBufferEntryNVX where
        type FieldType "flags" VkObjectTableVertexBufferEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTableVertexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTableVertexBufferEntryNVX =
             #{offset VkObjectTableVertexBufferEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTableVertexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableVertexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTableVertexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableVertexBufferEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableVertexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTableVertexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableVertexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkObjectTableVertexBufferEntryNVX where
        type FieldType "buffer" VkObjectTableVertexBufferEntryNVX =
             VkBuffer
        type FieldOptional "buffer" VkObjectTableVertexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkObjectTableVertexBufferEntryNVX =
             #{offset VkObjectTableVertexBufferEntryNVX, buffer}
        type FieldIsArray "buffer" VkObjectTableVertexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableVertexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkObjectTableVertexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableVertexBufferEntryNVX, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableVertexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkObjectTableVertexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableVertexBufferEntryNVX, buffer}

instance Show VkObjectTableVertexBufferEntryNVX where
        showsPrec d x
          = showString "VkObjectTableVertexBufferEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) . showChar '}'
