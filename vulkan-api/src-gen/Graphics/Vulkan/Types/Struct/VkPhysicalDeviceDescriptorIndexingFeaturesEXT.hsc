#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceDescriptorIndexingFeaturesEXT
       (VkPhysicalDeviceDescriptorIndexingFeaturesEXT(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Base                                                  (Addr##,
                                                                            ByteArray##,
                                                                            byteArrayContents##,
                                                                            plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                           (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo           (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR (VkPhysicalDeviceFeatures2KHR)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceDescriptorIndexingFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32               shaderInputAttachmentArrayDynamicIndexing;
--   >     VkBool32               shaderUniformTexelBufferArrayDynamicIndexing;
--   >     VkBool32               shaderStorageTexelBufferArrayDynamicIndexing;
--   >     VkBool32               shaderUniformBufferArrayNonUniformIndexing;
--   >     VkBool32               shaderSampledImageArrayNonUniformIndexing;
--   >     VkBool32               shaderStorageBufferArrayNonUniformIndexing;
--   >     VkBool32               shaderStorageImageArrayNonUniformIndexing;
--   >     VkBool32               shaderInputAttachmentArrayNonUniformIndexing;
--   >     VkBool32               shaderUniformTexelBufferArrayNonUniformIndexing;
--   >     VkBool32               shaderStorageTexelBufferArrayNonUniformIndexing;
--   >     VkBool32               descriptorBindingUniformBufferUpdateAfterBind;
--   >     VkBool32               descriptorBindingSampledImageUpdateAfterBind;
--   >     VkBool32               descriptorBindingStorageImageUpdateAfterBind;
--   >     VkBool32               descriptorBindingStorageBufferUpdateAfterBind;
--   >     VkBool32               descriptorBindingUniformTexelBufferUpdateAfterBind;
--   >     VkBool32               descriptorBindingStorageTexelBufferUpdateAfterBind;
--   >     VkBool32               descriptorBindingUpdateUnusedWhilePending;
--   >     VkBool32               descriptorBindingPartiallyBound;
--   >     VkBool32               descriptorBindingVariableDescriptorCount;
--   >     VkBool32               runtimeDescriptorArray;
--   > } VkPhysicalDeviceDescriptorIndexingFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDeviceDescriptorIndexingFeaturesEXTVkPhysicalDeviceDescriptorIndexingFeaturesEXT registry at www.khronos.org>
data VkPhysicalDeviceDescriptorIndexingFeaturesEXT = VkPhysicalDeviceDescriptorIndexingFeaturesEXT## Addr##
                                                                                                    ByteArray##

instance Eq VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
        (VkPhysicalDeviceDescriptorIndexingFeaturesEXT## a _) ==
          x@(VkPhysicalDeviceDescriptorIndexingFeaturesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
        (VkPhysicalDeviceDescriptorIndexingFeaturesEXT## a _) `compare`
          x@(VkPhysicalDeviceDescriptorIndexingFeaturesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceDescriptorIndexingFeaturesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceDescriptorIndexingFeaturesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        unsafeAddr (VkPhysicalDeviceDescriptorIndexingFeaturesEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceDescriptorIndexingFeaturesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceDescriptorIndexingFeaturesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type StructFields VkPhysicalDeviceDescriptorIndexingFeaturesEXT =
             '["sType", "pNext", "shaderInputAttachmentArrayDynamicIndexing", -- ' closing tick for hsc2hs
               "shaderUniformTexelBufferArrayDynamicIndexing",
               "shaderStorageTexelBufferArrayDynamicIndexing",
               "shaderUniformBufferArrayNonUniformIndexing",
               "shaderSampledImageArrayNonUniformIndexing",
               "shaderStorageBufferArrayNonUniformIndexing",
               "shaderStorageImageArrayNonUniformIndexing",
               "shaderInputAttachmentArrayNonUniformIndexing",
               "shaderUniformTexelBufferArrayNonUniformIndexing",
               "shaderStorageTexelBufferArrayNonUniformIndexing",
               "descriptorBindingUniformBufferUpdateAfterBind",
               "descriptorBindingSampledImageUpdateAfterBind",
               "descriptorBindingStorageImageUpdateAfterBind",
               "descriptorBindingStorageBufferUpdateAfterBind",
               "descriptorBindingUniformTexelBufferUpdateAfterBind",
               "descriptorBindingStorageTexelBufferUpdateAfterBind",
               "descriptorBindingUpdateUnusedWhilePending",
               "descriptorBindingPartiallyBound",
               "descriptorBindingVariableDescriptorCount",
               "runtimeDescriptorArray"]
        type CUnionType VkPhysicalDeviceDescriptorIndexingFeaturesEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceDescriptorIndexingFeaturesEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceDescriptorIndexingFeaturesEXT =
             '[VkPhysicalDeviceFeatures2KHR, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "shaderInputAttachmentArrayDynamicIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "shaderInputAttachmentArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "shaderInputAttachmentArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderInputAttachmentArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayDynamicIndexing}
        type FieldIsArray "shaderInputAttachmentArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderInputAttachmentArrayDynamicIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayDynamicIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderInputAttachmentArrayDynamicIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderUniformTexelBufferArrayDynamicIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "shaderUniformTexelBufferArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "shaderUniformTexelBufferArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderUniformTexelBufferArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayDynamicIndexing}
        type FieldIsArray "shaderUniformTexelBufferArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderUniformTexelBufferArrayDynamicIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayDynamicIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderUniformTexelBufferArrayDynamicIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageTexelBufferArrayDynamicIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "shaderStorageTexelBufferArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "shaderStorageTexelBufferArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageTexelBufferArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayDynamicIndexing}
        type FieldIsArray "shaderStorageTexelBufferArrayDynamicIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageTexelBufferArrayDynamicIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayDynamicIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageTexelBufferArrayDynamicIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderUniformBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "shaderUniformBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "shaderUniformBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderUniformBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformBufferArrayNonUniformIndexing}
        type FieldIsArray "shaderUniformBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderUniformBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformBufferArrayNonUniformIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderUniformBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderSampledImageArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "shaderSampledImageArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "shaderSampledImageArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderSampledImageArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderSampledImageArrayNonUniformIndexing}
        type FieldIsArray "shaderSampledImageArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderSampledImageArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderSampledImageArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderSampledImageArrayNonUniformIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderSampledImageArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderSampledImageArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderSampledImageArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "shaderStorageBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "shaderStorageBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageBufferArrayNonUniformIndexing}
        type FieldIsArray "shaderStorageBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageBufferArrayNonUniformIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageImageArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "shaderStorageImageArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "shaderStorageImageArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageImageArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageImageArrayNonUniformIndexing}
        type FieldIsArray "shaderStorageImageArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageImageArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageImageArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageImageArrayNonUniformIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageImageArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageImageArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageImageArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderInputAttachmentArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "shaderInputAttachmentArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "shaderInputAttachmentArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderInputAttachmentArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayNonUniformIndexing}
        type FieldIsArray "shaderInputAttachmentArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderInputAttachmentArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayNonUniformIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderInputAttachmentArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderUniformTexelBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "shaderUniformTexelBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional
               "shaderUniformTexelBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderUniformTexelBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayNonUniformIndexing}
        type FieldIsArray "shaderUniformTexelBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderUniformTexelBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayNonUniformIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderUniformTexelBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageTexelBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "shaderStorageTexelBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional
               "shaderStorageTexelBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageTexelBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayNonUniformIndexing}
        type FieldIsArray "shaderStorageTexelBufferArrayNonUniformIndexing"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageTexelBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayNonUniformIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageTexelBufferArrayNonUniformIndexing"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayNonUniformIndexing}

instance {-# OVERLAPPING #-}
         HasField "descriptorBindingUniformBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "descriptorBindingUniformBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "descriptorBindingUniformBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorBindingUniformBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformBufferUpdateAfterBind}
        type FieldIsArray "descriptorBindingUniformBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorBindingUniformBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformBufferUpdateAfterBind})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorBindingUniformBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         HasField "descriptorBindingSampledImageUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "descriptorBindingSampledImageUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "descriptorBindingSampledImageUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorBindingSampledImageUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingSampledImageUpdateAfterBind}
        type FieldIsArray "descriptorBindingSampledImageUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingSampledImageUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorBindingSampledImageUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingSampledImageUpdateAfterBind})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingSampledImageUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorBindingSampledImageUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingSampledImageUpdateAfterBind}

instance {-# OVERLAPPING #-}
         HasField "descriptorBindingStorageImageUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "descriptorBindingStorageImageUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "descriptorBindingStorageImageUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorBindingStorageImageUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageImageUpdateAfterBind}
        type FieldIsArray "descriptorBindingStorageImageUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageImageUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorBindingStorageImageUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageImageUpdateAfterBind})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageImageUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorBindingStorageImageUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageImageUpdateAfterBind}

instance {-# OVERLAPPING #-}
         HasField "descriptorBindingStorageBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "descriptorBindingStorageBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "descriptorBindingStorageBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorBindingStorageBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageBufferUpdateAfterBind}
        type FieldIsArray "descriptorBindingStorageBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorBindingStorageBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageBufferUpdateAfterBind})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorBindingStorageBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         HasField "descriptorBindingUniformTexelBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "descriptorBindingUniformTexelBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional
               "descriptorBindingUniformTexelBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset
               "descriptorBindingUniformTexelBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformTexelBufferUpdateAfterBind}
        type FieldIsArray
               "descriptorBindingUniformTexelBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformTexelBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorBindingUniformTexelBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformTexelBufferUpdateAfterBind})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformTexelBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorBindingUniformTexelBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformTexelBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         HasField "descriptorBindingStorageTexelBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "descriptorBindingStorageTexelBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional
               "descriptorBindingStorageTexelBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset
               "descriptorBindingStorageTexelBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageTexelBufferUpdateAfterBind}
        type FieldIsArray
               "descriptorBindingStorageTexelBufferUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageTexelBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorBindingStorageTexelBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageTexelBufferUpdateAfterBind})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageTexelBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorBindingStorageTexelBufferUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageTexelBufferUpdateAfterBind}

instance {-# OVERLAPPING #-}
         HasField "descriptorBindingUpdateUnusedWhilePending"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "descriptorBindingUpdateUnusedWhilePending"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "descriptorBindingUpdateUnusedWhilePending"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorBindingUpdateUnusedWhilePending"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUpdateUnusedWhilePending}
        type FieldIsArray "descriptorBindingUpdateUnusedWhilePending"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUpdateUnusedWhilePending}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorBindingUpdateUnusedWhilePending"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUpdateUnusedWhilePending})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUpdateUnusedWhilePending}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorBindingUpdateUnusedWhilePending"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUpdateUnusedWhilePending}

instance {-# OVERLAPPING #-}
         HasField "descriptorBindingPartiallyBound"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "descriptorBindingPartiallyBound"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "descriptorBindingPartiallyBound"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorBindingPartiallyBound"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingPartiallyBound}
        type FieldIsArray "descriptorBindingPartiallyBound"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingPartiallyBound}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorBindingPartiallyBound"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingPartiallyBound})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingPartiallyBound}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorBindingPartiallyBound"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingPartiallyBound}

instance {-# OVERLAPPING #-}
         HasField "descriptorBindingVariableDescriptorCount"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "descriptorBindingVariableDescriptorCount"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "descriptorBindingVariableDescriptorCount"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorBindingVariableDescriptorCount"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingVariableDescriptorCount}
        type FieldIsArray "descriptorBindingVariableDescriptorCount"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingVariableDescriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorBindingVariableDescriptorCount"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingVariableDescriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingVariableDescriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorBindingVariableDescriptorCount"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingVariableDescriptorCount}

instance {-# OVERLAPPING #-}
         HasField "runtimeDescriptorArray"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        type FieldType "runtimeDescriptorArray"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = VkBool32
        type FieldOptional "runtimeDescriptorArray"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "runtimeDescriptorArray"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, runtimeDescriptorArray}
        type FieldIsArray "runtimeDescriptorArray"
               VkPhysicalDeviceDescriptorIndexingFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, runtimeDescriptorArray}

instance {-# OVERLAPPING #-}
         CanReadField "runtimeDescriptorArray"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, runtimeDescriptorArray})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, runtimeDescriptorArray}

instance {-# OVERLAPPING #-}
         CanWriteField "runtimeDescriptorArray"
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, runtimeDescriptorArray}

instance Show VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceDescriptorIndexingFeaturesEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "shaderInputAttachmentArrayDynamicIndexing = " .
                            showsPrec d
                              (getField @"shaderInputAttachmentArrayDynamicIndexing" x)
                              .
                              showString ", " .
                                showString "shaderUniformTexelBufferArrayDynamicIndexing = " .
                                  showsPrec d
                                    (getField @"shaderUniformTexelBufferArrayDynamicIndexing" x)
                                    .
                                    showString ", " .
                                      showString "shaderStorageTexelBufferArrayDynamicIndexing = " .
                                        showsPrec d
                                          (getField @"shaderStorageTexelBufferArrayDynamicIndexing"
                                             x)
                                          .
                                          showString ", " .
                                            showString
                                              "shaderUniformBufferArrayNonUniformIndexing = "
                                              .
                                              showsPrec d
                                                (getField
                                                   @"shaderUniformBufferArrayNonUniformIndexing"
                                                   x)
                                                .
                                                showString ", " .
                                                  showString
                                                    "shaderSampledImageArrayNonUniformIndexing = "
                                                    .
                                                    showsPrec d
                                                      (getField
                                                         @"shaderSampledImageArrayNonUniformIndexing"
                                                         x)
                                                      .
                                                      showString ", " .
                                                        showString
                                                          "shaderStorageBufferArrayNonUniformIndexing = "
                                                          .
                                                          showsPrec d
                                                            (getField
                                                               @"shaderStorageBufferArrayNonUniformIndexing"
                                                               x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "shaderStorageImageArrayNonUniformIndexing = "
                                                                .
                                                                showsPrec d
                                                                  (getField
                                                                     @"shaderStorageImageArrayNonUniformIndexing"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "shaderInputAttachmentArrayNonUniformIndexing = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"shaderInputAttachmentArrayNonUniformIndexing"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "shaderUniformTexelBufferArrayNonUniformIndexing = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"shaderUniformTexelBufferArrayNonUniformIndexing"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "shaderStorageTexelBufferArrayNonUniformIndexing = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"shaderStorageTexelBufferArrayNonUniformIndexing"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "descriptorBindingUniformBufferUpdateAfterBind = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"descriptorBindingUniformBufferUpdateAfterBind"
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "descriptorBindingSampledImageUpdateAfterBind = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (getField
                                                                                                   @"descriptorBindingSampledImageUpdateAfterBind"
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "descriptorBindingStorageImageUpdateAfterBind = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (getField
                                                                                                         @"descriptorBindingStorageImageUpdateAfterBind"
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "descriptorBindingStorageBufferUpdateAfterBind = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (getField
                                                                                                               @"descriptorBindingStorageBufferUpdateAfterBind"
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "descriptorBindingUniformTexelBufferUpdateAfterBind = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (getField
                                                                                                                     @"descriptorBindingUniformTexelBufferUpdateAfterBind"
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "descriptorBindingStorageTexelBufferUpdateAfterBind = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (getField
                                                                                                                           @"descriptorBindingStorageTexelBufferUpdateAfterBind"
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showString
                                                                                                                          ", "
                                                                                                                          .
                                                                                                                          showString
                                                                                                                            "descriptorBindingUpdateUnusedWhilePending = "
                                                                                                                            .
                                                                                                                            showsPrec
                                                                                                                              d
                                                                                                                              (getField
                                                                                                                                 @"descriptorBindingUpdateUnusedWhilePending"
                                                                                                                                 x)
                                                                                                                              .
                                                                                                                              showString
                                                                                                                                ", "
                                                                                                                                .
                                                                                                                                showString
                                                                                                                                  "descriptorBindingPartiallyBound = "
                                                                                                                                  .
                                                                                                                                  showsPrec
                                                                                                                                    d
                                                                                                                                    (getField
                                                                                                                                       @"descriptorBindingPartiallyBound"
                                                                                                                                       x)
                                                                                                                                    .
                                                                                                                                    showString
                                                                                                                                      ", "
                                                                                                                                      .
                                                                                                                                      showString
                                                                                                                                        "descriptorBindingVariableDescriptorCount = "
                                                                                                                                        .
                                                                                                                                        showsPrec
                                                                                                                                          d
                                                                                                                                          (getField
                                                                                                                                             @"descriptorBindingVariableDescriptorCount"
                                                                                                                                             x)
                                                                                                                                          .
                                                                                                                                          showString
                                                                                                                                            ", "
                                                                                                                                            .
                                                                                                                                            showString
                                                                                                                                              "runtimeDescriptorArray = "
                                                                                                                                              .
                                                                                                                                              showsPrec
                                                                                                                                                d
                                                                                                                                                (getField
                                                                                                                                                   @"runtimeDescriptorArray"
                                                                                                                                                   x)
                                                                                                                                                .
                                                                                                                                                showChar
                                                                                                                                                  '}'
