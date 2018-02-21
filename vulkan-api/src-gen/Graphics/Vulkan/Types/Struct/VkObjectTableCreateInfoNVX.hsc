#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkObjectTableCreateInfoNVX
       (VkObjectTableCreateInfoNVX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX       (VkObjectEntryTypeNVX)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX (VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkObjectTableCreateInfoNVX.html VkObjectTableCreateInfoNVX registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkObjectTableCreateInfoNVX
         where
        type VkSTypeMType VkObjectTableCreateInfoNVX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, sType}

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

instance CanReadField "sType" VkObjectTableCreateInfoNVX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkObjectTableCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkObjectTableCreateInfoNVX
         where
        type VkPNextMType VkObjectTableCreateInfoNVX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pNext}

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

instance CanReadField "pNext" VkObjectTableCreateInfoNVX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkObjectTableCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkObjectCount VkObjectTableCreateInfoNVX where
        type VkObjectCountMType VkObjectTableCreateInfoNVX = Word32

        {-# NOINLINE vkObjectCount #-}
        vkObjectCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, objectCount})

        {-# INLINE vkObjectCountByteOffset #-}
        vkObjectCountByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, objectCount}

        {-# INLINE readVkObjectCount #-}
        readVkObjectCount p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, objectCount}

        {-# INLINE writeVkObjectCount #-}
        writeVkObjectCount p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, objectCount}

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

instance CanReadField "objectCount" VkObjectTableCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkObjectCount

        {-# INLINE readField #-}
        readField = readVkObjectCount

instance CanWriteField "objectCount" VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkObjectCount

instance {-# OVERLAPPING #-}
         HasVkPObjectEntryTypes VkObjectTableCreateInfoNVX where
        type VkPObjectEntryTypesMType VkObjectTableCreateInfoNVX =
             Ptr VkObjectEntryTypeNVX

        {-# NOINLINE vkPObjectEntryTypes #-}
        vkPObjectEntryTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes})

        {-# INLINE vkPObjectEntryTypesByteOffset #-}
        vkPObjectEntryTypesByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}

        {-# INLINE readVkPObjectEntryTypes #-}
        readVkPObjectEntryTypes p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}

        {-# INLINE writeVkPObjectEntryTypes #-}
        writeVkPObjectEntryTypes p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}

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

instance CanReadField "pObjectEntryTypes"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkPObjectEntryTypes

        {-# INLINE readField #-}
        readField = readVkPObjectEntryTypes

instance CanWriteField "pObjectEntryTypes"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPObjectEntryTypes

instance {-# OVERLAPPING #-}
         HasVkPObjectEntryCounts VkObjectTableCreateInfoNVX where
        type VkPObjectEntryCountsMType VkObjectTableCreateInfoNVX =
             Ptr Word32

        {-# NOINLINE vkPObjectEntryCounts #-}
        vkPObjectEntryCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts})

        {-# INLINE vkPObjectEntryCountsByteOffset #-}
        vkPObjectEntryCountsByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}

        {-# INLINE readVkPObjectEntryCounts #-}
        readVkPObjectEntryCounts p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}

        {-# INLINE writeVkPObjectEntryCounts #-}
        writeVkPObjectEntryCounts p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}

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

instance CanReadField "pObjectEntryCounts"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkPObjectEntryCounts

        {-# INLINE readField #-}
        readField = readVkPObjectEntryCounts

instance CanWriteField "pObjectEntryCounts"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPObjectEntryCounts

instance {-# OVERLAPPING #-}
         HasVkPObjectEntryUsageFlags VkObjectTableCreateInfoNVX where
        type VkPObjectEntryUsageFlagsMType VkObjectTableCreateInfoNVX =
             Ptr VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkPObjectEntryUsageFlags #-}
        vkPObjectEntryUsageFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags})

        {-# INLINE vkPObjectEntryUsageFlagsByteOffset #-}
        vkPObjectEntryUsageFlagsByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}

        {-# INLINE readVkPObjectEntryUsageFlags #-}
        readVkPObjectEntryUsageFlags p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}

        {-# INLINE writeVkPObjectEntryUsageFlags #-}
        writeVkPObjectEntryUsageFlags p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}

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

instance CanReadField "pObjectEntryUsageFlags"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkPObjectEntryUsageFlags

        {-# INLINE readField #-}
        readField = readVkPObjectEntryUsageFlags

instance CanWriteField "pObjectEntryUsageFlags"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPObjectEntryUsageFlags

instance {-# OVERLAPPING #-}
         HasVkMaxUniformBuffersPerDescriptor VkObjectTableCreateInfoNVX
         where
        type VkMaxUniformBuffersPerDescriptorMType
               VkObjectTableCreateInfoNVX
             = Word32

        {-# NOINLINE vkMaxUniformBuffersPerDescriptor #-}
        vkMaxUniformBuffersPerDescriptor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor})

        {-# INLINE vkMaxUniformBuffersPerDescriptorByteOffset #-}
        vkMaxUniformBuffersPerDescriptorByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}

        {-# INLINE readVkMaxUniformBuffersPerDescriptor #-}
        readVkMaxUniformBuffersPerDescriptor p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}

        {-# INLINE writeVkMaxUniformBuffersPerDescriptor #-}
        writeVkMaxUniformBuffersPerDescriptor p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}

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

instance CanReadField "maxUniformBuffersPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkMaxUniformBuffersPerDescriptor

        {-# INLINE readField #-}
        readField = readVkMaxUniformBuffersPerDescriptor

instance CanWriteField "maxUniformBuffersPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxUniformBuffersPerDescriptor

instance {-# OVERLAPPING #-}
         HasVkMaxStorageBuffersPerDescriptor VkObjectTableCreateInfoNVX
         where
        type VkMaxStorageBuffersPerDescriptorMType
               VkObjectTableCreateInfoNVX
             = Word32

        {-# NOINLINE vkMaxStorageBuffersPerDescriptor #-}
        vkMaxStorageBuffersPerDescriptor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor})

        {-# INLINE vkMaxStorageBuffersPerDescriptorByteOffset #-}
        vkMaxStorageBuffersPerDescriptorByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}

        {-# INLINE readVkMaxStorageBuffersPerDescriptor #-}
        readVkMaxStorageBuffersPerDescriptor p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}

        {-# INLINE writeVkMaxStorageBuffersPerDescriptor #-}
        writeVkMaxStorageBuffersPerDescriptor p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}

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

instance CanReadField "maxStorageBuffersPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkMaxStorageBuffersPerDescriptor

        {-# INLINE readField #-}
        readField = readVkMaxStorageBuffersPerDescriptor

instance CanWriteField "maxStorageBuffersPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxStorageBuffersPerDescriptor

instance {-# OVERLAPPING #-}
         HasVkMaxStorageImagesPerDescriptor VkObjectTableCreateInfoNVX where
        type VkMaxStorageImagesPerDescriptorMType
               VkObjectTableCreateInfoNVX
             = Word32

        {-# NOINLINE vkMaxStorageImagesPerDescriptor #-}
        vkMaxStorageImagesPerDescriptor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor})

        {-# INLINE vkMaxStorageImagesPerDescriptorByteOffset #-}
        vkMaxStorageImagesPerDescriptorByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}

        {-# INLINE readVkMaxStorageImagesPerDescriptor #-}
        readVkMaxStorageImagesPerDescriptor p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}

        {-# INLINE writeVkMaxStorageImagesPerDescriptor #-}
        writeVkMaxStorageImagesPerDescriptor p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}

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

instance CanReadField "maxStorageImagesPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkMaxStorageImagesPerDescriptor

        {-# INLINE readField #-}
        readField = readVkMaxStorageImagesPerDescriptor

instance CanWriteField "maxStorageImagesPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxStorageImagesPerDescriptor

instance {-# OVERLAPPING #-}
         HasVkMaxSampledImagesPerDescriptor VkObjectTableCreateInfoNVX where
        type VkMaxSampledImagesPerDescriptorMType
               VkObjectTableCreateInfoNVX
             = Word32

        {-# NOINLINE vkMaxSampledImagesPerDescriptor #-}
        vkMaxSampledImagesPerDescriptor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor})

        {-# INLINE vkMaxSampledImagesPerDescriptorByteOffset #-}
        vkMaxSampledImagesPerDescriptorByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}

        {-# INLINE readVkMaxSampledImagesPerDescriptor #-}
        readVkMaxSampledImagesPerDescriptor p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}

        {-# INLINE writeVkMaxSampledImagesPerDescriptor #-}
        writeVkMaxSampledImagesPerDescriptor p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}

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

instance CanReadField "maxSampledImagesPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkMaxSampledImagesPerDescriptor

        {-# INLINE readField #-}
        readField = readVkMaxSampledImagesPerDescriptor

instance CanWriteField "maxSampledImagesPerDescriptor"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxSampledImagesPerDescriptor

instance {-# OVERLAPPING #-}
         HasVkMaxPipelineLayouts VkObjectTableCreateInfoNVX where
        type VkMaxPipelineLayoutsMType VkObjectTableCreateInfoNVX = Word32

        {-# NOINLINE vkMaxPipelineLayouts #-}
        vkMaxPipelineLayouts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts})

        {-# INLINE vkMaxPipelineLayoutsByteOffset #-}
        vkMaxPipelineLayoutsByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}

        {-# INLINE readVkMaxPipelineLayouts #-}
        readVkMaxPipelineLayouts p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}

        {-# INLINE writeVkMaxPipelineLayouts #-}
        writeVkMaxPipelineLayouts p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}

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

instance CanReadField "maxPipelineLayouts"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkMaxPipelineLayouts

        {-# INLINE readField #-}
        readField = readVkMaxPipelineLayouts

instance CanWriteField "maxPipelineLayouts"
           VkObjectTableCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxPipelineLayouts

instance Show VkObjectTableCreateInfoNVX where
        showsPrec d x
          = showString "VkObjectTableCreateInfoNVX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkObjectCount = " .
                            showsPrec d (vkObjectCount x) .
                              showString ", " .
                                showString "vkPObjectEntryTypes = " .
                                  showsPrec d (vkPObjectEntryTypes x) .
                                    showString ", " .
                                      showString "vkPObjectEntryCounts = " .
                                        showsPrec d (vkPObjectEntryCounts x) .
                                          showString ", " .
                                            showString "vkPObjectEntryUsageFlags = " .
                                              showsPrec d (vkPObjectEntryUsageFlags x) .
                                                showString ", " .
                                                  showString "vkMaxUniformBuffersPerDescriptor = " .
                                                    showsPrec d (vkMaxUniformBuffersPerDescriptor x)
                                                      .
                                                      showString ", " .
                                                        showString
                                                          "vkMaxStorageBuffersPerDescriptor = "
                                                          .
                                                          showsPrec d
                                                            (vkMaxStorageBuffersPerDescriptor x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "vkMaxStorageImagesPerDescriptor = "
                                                                .
                                                                showsPrec d
                                                                  (vkMaxStorageImagesPerDescriptor
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkMaxSampledImagesPerDescriptor = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkMaxSampledImagesPerDescriptor
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkMaxPipelineLayouts = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkMaxPipelineLayouts
                                                                                 x)
                                                                              . showChar '}'
