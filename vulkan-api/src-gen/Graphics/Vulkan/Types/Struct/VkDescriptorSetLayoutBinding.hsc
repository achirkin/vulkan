#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBinding
       (VkDescriptorSetLayoutBinding(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorType   (VkDescriptorType)
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Handles                 (VkSampler)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorSetLayoutBinding {
--   >     uint32_t               binding;
--   >     VkDescriptorType       descriptorType;
--   >     uint32_t descriptorCount;
--   >     VkShaderStageFlags     stageFlags;
--   >     const VkSampler*       pImmutableSamplers;
--   > } VkDescriptorSetLayoutBinding;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorSetLayoutBinding.html VkDescriptorSetLayoutBinding registry at www.khronos.org>
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
         HasVkBinding VkDescriptorSetLayoutBinding where
        type VkBindingMType VkDescriptorSetLayoutBinding = Word32

        {-# NOINLINE vkBinding #-}
        vkBinding x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, binding})

        {-# INLINE vkBindingByteOffset #-}
        vkBindingByteOffset ~_
          = #{offset VkDescriptorSetLayoutBinding, binding}

        {-# INLINE readVkBinding #-}
        readVkBinding p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, binding}

        {-# INLINE writeVkBinding #-}
        writeVkBinding p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, binding}

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

instance CanReadField "binding" VkDescriptorSetLayoutBinding where
        {-# INLINE getField #-}
        getField = vkBinding

        {-# INLINE readField #-}
        readField = readVkBinding

instance CanWriteField "binding" VkDescriptorSetLayoutBinding where
        {-# INLINE writeField #-}
        writeField = writeVkBinding

instance {-# OVERLAPPING #-}
         HasVkDescriptorType VkDescriptorSetLayoutBinding where
        type VkDescriptorTypeMType VkDescriptorSetLayoutBinding =
             VkDescriptorType

        {-# NOINLINE vkDescriptorType #-}
        vkDescriptorType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, descriptorType})

        {-# INLINE vkDescriptorTypeByteOffset #-}
        vkDescriptorTypeByteOffset ~_
          = #{offset VkDescriptorSetLayoutBinding, descriptorType}

        {-# INLINE readVkDescriptorType #-}
        readVkDescriptorType p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorType}

        {-# INLINE writeVkDescriptorType #-}
        writeVkDescriptorType p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorType}

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

instance CanReadField "descriptorType" VkDescriptorSetLayoutBinding
         where
        {-# INLINE getField #-}
        getField = vkDescriptorType

        {-# INLINE readField #-}
        readField = readVkDescriptorType

instance CanWriteField "descriptorType"
           VkDescriptorSetLayoutBinding
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorType

instance {-# OVERLAPPING #-}
         HasVkDescriptorCount VkDescriptorSetLayoutBinding where
        type VkDescriptorCountMType VkDescriptorSetLayoutBinding = Word32

        {-# NOINLINE vkDescriptorCount #-}
        vkDescriptorCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, descriptorCount})

        {-# INLINE vkDescriptorCountByteOffset #-}
        vkDescriptorCountByteOffset ~_
          = #{offset VkDescriptorSetLayoutBinding, descriptorCount}

        {-# INLINE readVkDescriptorCount #-}
        readVkDescriptorCount p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorCount}

        {-# INLINE writeVkDescriptorCount #-}
        writeVkDescriptorCount p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorCount}

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

instance CanReadField "descriptorCount"
           VkDescriptorSetLayoutBinding
         where
        {-# INLINE getField #-}
        getField = vkDescriptorCount

        {-# INLINE readField #-}
        readField = readVkDescriptorCount

instance CanWriteField "descriptorCount"
           VkDescriptorSetLayoutBinding
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorCount

instance {-# OVERLAPPING #-}
         HasVkStageFlags VkDescriptorSetLayoutBinding where
        type VkStageFlagsMType VkDescriptorSetLayoutBinding =
             VkShaderStageFlags

        {-# NOINLINE vkStageFlags #-}
        vkStageFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, stageFlags})

        {-# INLINE vkStageFlagsByteOffset #-}
        vkStageFlagsByteOffset ~_
          = #{offset VkDescriptorSetLayoutBinding, stageFlags}

        {-# INLINE readVkStageFlags #-}
        readVkStageFlags p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, stageFlags}

        {-# INLINE writeVkStageFlags #-}
        writeVkStageFlags p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, stageFlags}

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

instance CanReadField "stageFlags" VkDescriptorSetLayoutBinding
         where
        {-# INLINE getField #-}
        getField = vkStageFlags

        {-# INLINE readField #-}
        readField = readVkStageFlags

instance CanWriteField "stageFlags" VkDescriptorSetLayoutBinding
         where
        {-# INLINE writeField #-}
        writeField = writeVkStageFlags

instance {-# OVERLAPPING #-}
         HasVkPImmutableSamplers VkDescriptorSetLayoutBinding where
        type VkPImmutableSamplersMType VkDescriptorSetLayoutBinding =
             Ptr VkSampler

        {-# NOINLINE vkPImmutableSamplers #-}
        vkPImmutableSamplers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers})

        {-# INLINE vkPImmutableSamplersByteOffset #-}
        vkPImmutableSamplersByteOffset ~_
          = #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}

        {-# INLINE readVkPImmutableSamplers #-}
        readVkPImmutableSamplers p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}

        {-# INLINE writeVkPImmutableSamplers #-}
        writeVkPImmutableSamplers p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}

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

instance CanReadField "pImmutableSamplers"
           VkDescriptorSetLayoutBinding
         where
        {-# INLINE getField #-}
        getField = vkPImmutableSamplers

        {-# INLINE readField #-}
        readField = readVkPImmutableSamplers

instance CanWriteField "pImmutableSamplers"
           VkDescriptorSetLayoutBinding
         where
        {-# INLINE writeField #-}
        writeField = writeVkPImmutableSamplers

instance Show VkDescriptorSetLayoutBinding where
        showsPrec d x
          = showString "VkDescriptorSetLayoutBinding {" .
              showString "vkBinding = " .
                showsPrec d (vkBinding x) .
                  showString ", " .
                    showString "vkDescriptorType = " .
                      showsPrec d (vkDescriptorType x) .
                        showString ", " .
                          showString "vkDescriptorCount = " .
                            showsPrec d (vkDescriptorCount x) .
                              showString ", " .
                                showString "vkStageFlags = " .
                                  showsPrec d (vkStageFlags x) .
                                    showString ", " .
                                      showString "vkPImmutableSamplers = " .
                                        showsPrec d (vkPImmutableSamplers x) . showChar '}'
