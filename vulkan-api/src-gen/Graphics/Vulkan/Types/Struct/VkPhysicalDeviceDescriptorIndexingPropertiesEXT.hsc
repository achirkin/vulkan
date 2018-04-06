#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceDescriptorIndexingPropertiesEXT
       (VkPhysicalDeviceDescriptorIndexingPropertiesEXT(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                             (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceDescriptorIndexingPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t               maxUpdateAfterBindDescriptorsInAllPools;
--   >     VkBool32               shaderUniformBufferArrayNonUniformIndexingNative;
--   >     VkBool32               shaderSampledImageArrayNonUniformIndexingNative;
--   >     VkBool32               shaderStorageBufferArrayNonUniformIndexingNative;
--   >     VkBool32               shaderStorageImageArrayNonUniformIndexingNative;
--   >     VkBool32               shaderInputAttachmentArrayNonUniformIndexingNative;
--   >     VkBool32               robustBufferAccessUpdateAfterBind;
--   >     VkBool32               quadDivergentImplicitLod;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindSamplers;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindUniformBuffers;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindStorageBuffers;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindSampledImages;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindStorageImages;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindInputAttachments;
--   >     uint32_t               maxPerStageUpdateAfterBindResources;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindSamplers;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindUniformBuffers;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindUniformBuffersDynamic;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindStorageBuffers;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindStorageBuffersDynamic;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindSampledImages;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindStorageImages;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindInputAttachments;
--   > } VkPhysicalDeviceDescriptorIndexingPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceDescriptorIndexingPropertiesEXT.html VkPhysicalDeviceDescriptorIndexingPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceDescriptorIndexingPropertiesEXT = VkPhysicalDeviceDescriptorIndexingPropertiesEXT## Addr##
                                                                                                        ByteArray##

instance Eq VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
        (VkPhysicalDeviceDescriptorIndexingPropertiesEXT## a _) ==
          x@(VkPhysicalDeviceDescriptorIndexingPropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
        (VkPhysicalDeviceDescriptorIndexingPropertiesEXT## a _) `compare`
          x@(VkPhysicalDeviceDescriptorIndexingPropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceDescriptorIndexingPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceDescriptorIndexingPropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        unsafeAddr (VkPhysicalDeviceDescriptorIndexingPropertiesEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceDescriptorIndexingPropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceDescriptorIndexingPropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type StructFields VkPhysicalDeviceDescriptorIndexingPropertiesEXT =
             '["sType", "pNext", "maxUpdateAfterBindDescriptorsInAllPools", -- ' closing tick for hsc2hs
               "shaderUniformBufferArrayNonUniformIndexingNative",
               "shaderSampledImageArrayNonUniformIndexingNative",
               "shaderStorageBufferArrayNonUniformIndexingNative",
               "shaderStorageImageArrayNonUniformIndexingNative",
               "shaderInputAttachmentArrayNonUniformIndexingNative",
               "robustBufferAccessUpdateAfterBind", "quadDivergentImplicitLod",
               "maxPerStageDescriptorUpdateAfterBindSamplers",
               "maxPerStageDescriptorUpdateAfterBindUniformBuffers",
               "maxPerStageDescriptorUpdateAfterBindStorageBuffers",
               "maxPerStageDescriptorUpdateAfterBindSampledImages",
               "maxPerStageDescriptorUpdateAfterBindStorageImages",
               "maxPerStageDescriptorUpdateAfterBindInputAttachments",
               "maxPerStageUpdateAfterBindResources",
               "maxDescriptorSetUpdateAfterBindSamplers",
               "maxDescriptorSetUpdateAfterBindUniformBuffers",
               "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic",
               "maxDescriptorSetUpdateAfterBindStorageBuffers",
               "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic",
               "maxDescriptorSetUpdateAfterBindSampledImages",
               "maxDescriptorSetUpdateAfterBindStorageImages",
               "maxDescriptorSetUpdateAfterBindInputAttachments"]
        type CUnionType VkPhysicalDeviceDescriptorIndexingPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceDescriptorIndexingPropertiesEXT =
             'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxUpdateAfterBindDescriptorsInAllPools"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxUpdateAfterBindDescriptorsInAllPools"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional "maxUpdateAfterBindDescriptorsInAllPools"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxUpdateAfterBindDescriptorsInAllPools"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxUpdateAfterBindDescriptorsInAllPools}
        type FieldIsArray "maxUpdateAfterBindDescriptorsInAllPools"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxUpdateAfterBindDescriptorsInAllPools}

instance {-# OVERLAPPING #-}
         CanReadField "maxUpdateAfterBindDescriptorsInAllPools"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxUpdateAfterBindDescriptorsInAllPools})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxUpdateAfterBindDescriptorsInAllPools}

instance {-# OVERLAPPING #-}
         CanWriteField "maxUpdateAfterBindDescriptorsInAllPools"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxUpdateAfterBindDescriptorsInAllPools}

instance {-# OVERLAPPING #-}
         HasField "shaderUniformBufferArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "shaderUniformBufferArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = VkBool32
        type FieldOptional
               "shaderUniformBufferArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderUniformBufferArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderUniformBufferArrayNonUniformIndexingNative}
        type FieldIsArray
               "shaderUniformBufferArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderUniformBufferArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         CanReadField "shaderUniformBufferArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderUniformBufferArrayNonUniformIndexingNative})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderUniformBufferArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderUniformBufferArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderUniformBufferArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         HasField "shaderSampledImageArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "shaderSampledImageArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = VkBool32
        type FieldOptional
               "shaderSampledImageArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderSampledImageArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderSampledImageArrayNonUniformIndexingNative}
        type FieldIsArray "shaderSampledImageArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderSampledImageArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         CanReadField "shaderSampledImageArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderSampledImageArrayNonUniformIndexingNative})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderSampledImageArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderSampledImageArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderSampledImageArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageBufferArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "shaderStorageBufferArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = VkBool32
        type FieldOptional
               "shaderStorageBufferArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageBufferArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageBufferArrayNonUniformIndexingNative}
        type FieldIsArray
               "shaderStorageBufferArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageBufferArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageBufferArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageBufferArrayNonUniformIndexingNative})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageBufferArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageBufferArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageBufferArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageImageArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "shaderStorageImageArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = VkBool32
        type FieldOptional
               "shaderStorageImageArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageImageArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageImageArrayNonUniformIndexingNative}
        type FieldIsArray "shaderStorageImageArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageImageArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageImageArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageImageArrayNonUniformIndexingNative})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageImageArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageImageArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageImageArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         HasField "shaderInputAttachmentArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "shaderInputAttachmentArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = VkBool32
        type FieldOptional
               "shaderInputAttachmentArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset
               "shaderInputAttachmentArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderInputAttachmentArrayNonUniformIndexingNative}
        type FieldIsArray
               "shaderInputAttachmentArrayNonUniformIndexingNative"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderInputAttachmentArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         CanReadField "shaderInputAttachmentArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderInputAttachmentArrayNonUniformIndexingNative})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderInputAttachmentArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderInputAttachmentArrayNonUniformIndexingNative"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderInputAttachmentArrayNonUniformIndexingNative}

instance {-# OVERLAPPING #-}
         HasField "robustBufferAccessUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "robustBufferAccessUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = VkBool32
        type FieldOptional "robustBufferAccessUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "robustBufferAccessUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, robustBufferAccessUpdateAfterBind}
        type FieldIsArray "robustBufferAccessUpdateAfterBind"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, robustBufferAccessUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanReadField "robustBufferAccessUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, robustBufferAccessUpdateAfterBind})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, robustBufferAccessUpdateAfterBind}

instance {-# OVERLAPPING #-}
         CanWriteField "robustBufferAccessUpdateAfterBind"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, robustBufferAccessUpdateAfterBind}

instance {-# OVERLAPPING #-}
         HasField "quadDivergentImplicitLod"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "quadDivergentImplicitLod"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = VkBool32
        type FieldOptional "quadDivergentImplicitLod"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "quadDivergentImplicitLod"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, quadDivergentImplicitLod}
        type FieldIsArray "quadDivergentImplicitLod"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, quadDivergentImplicitLod}

instance {-# OVERLAPPING #-}
         CanReadField "quadDivergentImplicitLod"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, quadDivergentImplicitLod})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, quadDivergentImplicitLod}

instance {-# OVERLAPPING #-}
         CanWriteField "quadDivergentImplicitLod"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, quadDivergentImplicitLod}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorUpdateAfterBindSamplers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxPerStageDescriptorUpdateAfterBindSamplers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional "maxPerStageDescriptorUpdateAfterBindSamplers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPerStageDescriptorUpdateAfterBindSamplers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSamplers}
        type FieldIsArray "maxPerStageDescriptorUpdateAfterBindSamplers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSamplers}

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorUpdateAfterBindSamplers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSamplers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSamplers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorUpdateAfterBindSamplers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSamplers}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional
               "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset
               "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindUniformBuffers}
        type FieldIsArray
               "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindUniformBuffers}

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindUniformBuffers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindUniformBuffers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindUniformBuffers}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional
               "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset
               "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageBuffers}
        type FieldIsArray
               "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageBuffers}

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageBuffers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageBuffers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageBuffers}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorUpdateAfterBindSampledImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxPerStageDescriptorUpdateAfterBindSampledImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional
               "maxPerStageDescriptorUpdateAfterBindSampledImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset
               "maxPerStageDescriptorUpdateAfterBindSampledImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSampledImages}
        type FieldIsArray
               "maxPerStageDescriptorUpdateAfterBindSampledImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSampledImages}

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorUpdateAfterBindSampledImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSampledImages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSampledImages}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorUpdateAfterBindSampledImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSampledImages}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorUpdateAfterBindStorageImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxPerStageDescriptorUpdateAfterBindStorageImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional
               "maxPerStageDescriptorUpdateAfterBindStorageImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset
               "maxPerStageDescriptorUpdateAfterBindStorageImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageImages}
        type FieldIsArray
               "maxPerStageDescriptorUpdateAfterBindStorageImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageImages}

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorUpdateAfterBindStorageImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageImages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageImages}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorUpdateAfterBindStorageImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageImages}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorUpdateAfterBindInputAttachments"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType
               "maxPerStageDescriptorUpdateAfterBindInputAttachments"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional
               "maxPerStageDescriptorUpdateAfterBindInputAttachments"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset
               "maxPerStageDescriptorUpdateAfterBindInputAttachments"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindInputAttachments}
        type FieldIsArray
               "maxPerStageDescriptorUpdateAfterBindInputAttachments"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindInputAttachments}

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorUpdateAfterBindInputAttachments"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindInputAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindInputAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField
           "maxPerStageDescriptorUpdateAfterBindInputAttachments"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindInputAttachments}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageUpdateAfterBindResources"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxPerStageUpdateAfterBindResources"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional "maxPerStageUpdateAfterBindResources"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPerStageUpdateAfterBindResources"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageUpdateAfterBindResources}
        type FieldIsArray "maxPerStageUpdateAfterBindResources"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageUpdateAfterBindResources}

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageUpdateAfterBindResources"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageUpdateAfterBindResources})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageUpdateAfterBindResources}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageUpdateAfterBindResources"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageUpdateAfterBindResources}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetUpdateAfterBindSamplers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxDescriptorSetUpdateAfterBindSamplers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional "maxDescriptorSetUpdateAfterBindSamplers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetUpdateAfterBindSamplers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSamplers}
        type FieldIsArray "maxDescriptorSetUpdateAfterBindSamplers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSamplers}

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetUpdateAfterBindSamplers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSamplers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSamplers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetUpdateAfterBindSamplers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSamplers}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetUpdateAfterBindUniformBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxDescriptorSetUpdateAfterBindUniformBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional "maxDescriptorSetUpdateAfterBindUniformBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetUpdateAfterBindUniformBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffers}
        type FieldIsArray "maxDescriptorSetUpdateAfterBindUniformBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffers}

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetUpdateAfterBindUniformBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetUpdateAfterBindUniformBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffers}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType
               "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional
               "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset
               "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffersDynamic}
        type FieldIsArray
               "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffersDynamic}

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffersDynamic})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffersDynamic}

instance {-# OVERLAPPING #-}
         CanWriteField
           "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffersDynamic}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetUpdateAfterBindStorageBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxDescriptorSetUpdateAfterBindStorageBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional "maxDescriptorSetUpdateAfterBindStorageBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetUpdateAfterBindStorageBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffers}
        type FieldIsArray "maxDescriptorSetUpdateAfterBindStorageBuffers"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffers}

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetUpdateAfterBindStorageBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetUpdateAfterBindStorageBuffers"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffers}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType
               "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional
               "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset
               "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffersDynamic}
        type FieldIsArray
               "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffersDynamic}

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffersDynamic})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffersDynamic}

instance {-# OVERLAPPING #-}
         CanWriteField
           "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffersDynamic}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetUpdateAfterBindSampledImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxDescriptorSetUpdateAfterBindSampledImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional "maxDescriptorSetUpdateAfterBindSampledImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetUpdateAfterBindSampledImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSampledImages}
        type FieldIsArray "maxDescriptorSetUpdateAfterBindSampledImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSampledImages}

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetUpdateAfterBindSampledImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSampledImages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSampledImages}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetUpdateAfterBindSampledImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSampledImages}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetUpdateAfterBindStorageImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxDescriptorSetUpdateAfterBindStorageImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional "maxDescriptorSetUpdateAfterBindStorageImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetUpdateAfterBindStorageImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageImages}
        type FieldIsArray "maxDescriptorSetUpdateAfterBindStorageImages"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageImages}

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetUpdateAfterBindStorageImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageImages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageImages}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetUpdateAfterBindStorageImages"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageImages}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetUpdateAfterBindInputAttachments"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        type FieldType "maxDescriptorSetUpdateAfterBindInputAttachments"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = Word32
        type FieldOptional
               "maxDescriptorSetUpdateAfterBindInputAttachments"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetUpdateAfterBindInputAttachments"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             =
             #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindInputAttachments}
        type FieldIsArray "maxDescriptorSetUpdateAfterBindInputAttachments"
               VkPhysicalDeviceDescriptorIndexingPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindInputAttachments}

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetUpdateAfterBindInputAttachments"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindInputAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindInputAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetUpdateAfterBindInputAttachments"
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindInputAttachments}

instance Show VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceDescriptorIndexingPropertiesEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxUpdateAfterBindDescriptorsInAllPools = " .
                            showsPrec d (getField @"maxUpdateAfterBindDescriptorsInAllPools" x)
                              .
                              showString ", " .
                                showString "shaderUniformBufferArrayNonUniformIndexingNative = " .
                                  showsPrec d
                                    (getField @"shaderUniformBufferArrayNonUniformIndexingNative" x)
                                    .
                                    showString ", " .
                                      showString
                                        "shaderSampledImageArrayNonUniformIndexingNative = "
                                        .
                                        showsPrec d
                                          (getField
                                             @"shaderSampledImageArrayNonUniformIndexingNative"
                                             x)
                                          .
                                          showString ", " .
                                            showString
                                              "shaderStorageBufferArrayNonUniformIndexingNative = "
                                              .
                                              showsPrec d
                                                (getField
                                                   @"shaderStorageBufferArrayNonUniformIndexingNative"
                                                   x)
                                                .
                                                showString ", " .
                                                  showString
                                                    "shaderStorageImageArrayNonUniformIndexingNative = "
                                                    .
                                                    showsPrec d
                                                      (getField
                                                         @"shaderStorageImageArrayNonUniformIndexingNative"
                                                         x)
                                                      .
                                                      showString ", " .
                                                        showString
                                                          "shaderInputAttachmentArrayNonUniformIndexingNative = "
                                                          .
                                                          showsPrec d
                                                            (getField
                                                               @"shaderInputAttachmentArrayNonUniformIndexingNative"
                                                               x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "robustBufferAccessUpdateAfterBind = "
                                                                .
                                                                showsPrec d
                                                                  (getField
                                                                     @"robustBufferAccessUpdateAfterBind"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "quadDivergentImplicitLod = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"quadDivergentImplicitLod"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "maxPerStageDescriptorUpdateAfterBindSamplers = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"maxPerStageDescriptorUpdateAfterBindSamplers"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "maxPerStageDescriptorUpdateAfterBindUniformBuffers = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"maxPerStageDescriptorUpdateAfterBindUniformBuffers"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "maxPerStageDescriptorUpdateAfterBindStorageBuffers = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"maxPerStageDescriptorUpdateAfterBindStorageBuffers"
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "maxPerStageDescriptorUpdateAfterBindSampledImages = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (getField
                                                                                                   @"maxPerStageDescriptorUpdateAfterBindSampledImages"
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "maxPerStageDescriptorUpdateAfterBindStorageImages = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (getField
                                                                                                         @"maxPerStageDescriptorUpdateAfterBindStorageImages"
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "maxPerStageDescriptorUpdateAfterBindInputAttachments = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (getField
                                                                                                               @"maxPerStageDescriptorUpdateAfterBindInputAttachments"
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "maxPerStageUpdateAfterBindResources = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (getField
                                                                                                                     @"maxPerStageUpdateAfterBindResources"
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "maxDescriptorSetUpdateAfterBindSamplers = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (getField
                                                                                                                           @"maxDescriptorSetUpdateAfterBindSamplers"
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showString
                                                                                                                          ", "
                                                                                                                          .
                                                                                                                          showString
                                                                                                                            "maxDescriptorSetUpdateAfterBindUniformBuffers = "
                                                                                                                            .
                                                                                                                            showsPrec
                                                                                                                              d
                                                                                                                              (getField
                                                                                                                                 @"maxDescriptorSetUpdateAfterBindUniformBuffers"
                                                                                                                                 x)
                                                                                                                              .
                                                                                                                              showString
                                                                                                                                ", "
                                                                                                                                .
                                                                                                                                showString
                                                                                                                                  "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic = "
                                                                                                                                  .
                                                                                                                                  showsPrec
                                                                                                                                    d
                                                                                                                                    (getField
                                                                                                                                       @"maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
                                                                                                                                       x)
                                                                                                                                    .
                                                                                                                                    showString
                                                                                                                                      ", "
                                                                                                                                      .
                                                                                                                                      showString
                                                                                                                                        "maxDescriptorSetUpdateAfterBindStorageBuffers = "
                                                                                                                                        .
                                                                                                                                        showsPrec
                                                                                                                                          d
                                                                                                                                          (getField
                                                                                                                                             @"maxDescriptorSetUpdateAfterBindStorageBuffers"
                                                                                                                                             x)
                                                                                                                                          .
                                                                                                                                          showString
                                                                                                                                            ", "
                                                                                                                                            .
                                                                                                                                            showString
                                                                                                                                              "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic = "
                                                                                                                                              .
                                                                                                                                              showsPrec
                                                                                                                                                d
                                                                                                                                                (getField
                                                                                                                                                   @"maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
                                                                                                                                                   x)
                                                                                                                                                .
                                                                                                                                                showString
                                                                                                                                                  ", "
                                                                                                                                                  .
                                                                                                                                                  showString
                                                                                                                                                    "maxDescriptorSetUpdateAfterBindSampledImages = "
                                                                                                                                                    .
                                                                                                                                                    showsPrec
                                                                                                                                                      d
                                                                                                                                                      (getField
                                                                                                                                                         @"maxDescriptorSetUpdateAfterBindSampledImages"
                                                                                                                                                         x)
                                                                                                                                                      .
                                                                                                                                                      showString
                                                                                                                                                        ", "
                                                                                                                                                        .
                                                                                                                                                        showString
                                                                                                                                                          "maxDescriptorSetUpdateAfterBindStorageImages = "
                                                                                                                                                          .
                                                                                                                                                          showsPrec
                                                                                                                                                            d
                                                                                                                                                            (getField
                                                                                                                                                               @"maxDescriptorSetUpdateAfterBindStorageImages"
                                                                                                                                                               x)
                                                                                                                                                            .
                                                                                                                                                            showString
                                                                                                                                                              ", "
                                                                                                                                                              .
                                                                                                                                                              showString
                                                                                                                                                                "maxDescriptorSetUpdateAfterBindInputAttachments = "
                                                                                                                                                                .
                                                                                                                                                                showsPrec
                                                                                                                                                                  d
                                                                                                                                                                  (getField
                                                                                                                                                                     @"maxDescriptorSetUpdateAfterBindInputAttachments"
                                                                                                                                                                     x)
                                                                                                                                                                  .
                                                                                                                                                                  showChar
                                                                                                                                                                    '}'
