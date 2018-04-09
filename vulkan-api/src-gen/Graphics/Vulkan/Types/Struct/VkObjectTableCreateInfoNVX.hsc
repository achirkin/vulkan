#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkObjectTableCreateInfoNVX
       (VkObjectTableCreateInfoNVX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX       (VkObjectEntryTypeNVX)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX (VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkObjectTableCreateInfoNVXVkObjectTableCreateInfoNVX registry at www.khronos.org>
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
