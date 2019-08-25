#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Descriptor
       (VkDescriptorBufferInfo, VkDescriptorImageInfo,
        VkDescriptorPoolCreateInfo, VkDescriptorPoolSize,
        VkDescriptorSetAllocateInfo, VkDescriptorSetLayoutBinding,
        VkDescriptorSetLayoutBindingFlagsCreateInfoEXT,
        VkDescriptorSetLayoutCreateInfo, VkDescriptorSetLayoutSupport,
        VkDescriptorSetLayoutSupportKHR,
        VkDescriptorSetVariableDescriptorCountAllocateInfoEXT,
        VkDescriptorSetVariableDescriptorCountLayoutSupportEXT,
        VkDescriptorUpdateTemplateCreateInfo,
        VkDescriptorUpdateTemplateCreateInfoKHR,
        VkDescriptorUpdateTemplateEntry,
        VkDescriptorUpdateTemplateEntryKHR)
       where
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

-- | > typedef struct VkDescriptorBufferInfo {
--   >     VkBuffer               buffer;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           range;
--   > } VkDescriptorBufferInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorBufferInfo VkDescriptorBufferInfo registry at www.khronos.org>
type VkDescriptorBufferInfo = VkStruct VkDescriptorBufferInfo' -- ' closing tick for hsc2hs

data VkDescriptorBufferInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDescriptorBufferInfo where
    type StructRep VkDescriptorBufferInfo =
         'StructMeta "VkDescriptorBufferInfo" VkDescriptorBufferInfo -- ' closing tick for hsc2hs
           #{size VkDescriptorBufferInfo}
           #{alignment VkDescriptorBufferInfo}
           '[('FieldMeta "buffer" VkBuffer 'False  -- ' closing tick for hsc2hs
                                                  #{offset VkDescriptorBufferInfo, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" VkDeviceSize 'False 
                                                      #{offset VkDescriptorBufferInfo, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "range" VkDeviceSize 'False 
                                                     #{offset VkDescriptorBufferInfo, range}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDescriptorImageInfo {
--   >     VkSampler       sampler;
--   >     VkImageView     imageView;
--   >     VkImageLayout   imageLayout;
--   > } VkDescriptorImageInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorImageInfo VkDescriptorImageInfo registry at www.khronos.org>
type VkDescriptorImageInfo = VkStruct VkDescriptorImageInfo' -- ' closing tick for hsc2hs

data VkDescriptorImageInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDescriptorImageInfo where
    type StructRep VkDescriptorImageInfo =
         'StructMeta "VkDescriptorImageInfo" VkDescriptorImageInfo  -- ' closing tick for hsc2hs
                                                                   #{size VkDescriptorImageInfo}
           #{alignment VkDescriptorImageInfo}
           '[('FieldMeta "sampler" VkSampler 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkDescriptorImageInfo, sampler}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageView" VkImageView 'False 
                                                        #{offset VkDescriptorImageInfo, imageView}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageLayout" VkImageLayout 'False 
                                                            #{offset VkDescriptorImageInfo, imageLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

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
type VkDescriptorPoolCreateInfo =
     VkStruct VkDescriptorPoolCreateInfo' -- ' closing tick for hsc2hs

data VkDescriptorPoolCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDescriptorPoolCreateInfo where
    type StructRep VkDescriptorPoolCreateInfo =
         'StructMeta "VkDescriptorPoolCreateInfo" VkDescriptorPoolCreateInfo -- ' closing tick for hsc2hs
           #{size VkDescriptorPoolCreateInfo}
           #{alignment VkDescriptorPoolCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDescriptorPoolCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDescriptorPoolCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDescriptorPoolCreateFlags 'True 
                                                                   #{offset VkDescriptorPoolCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSets" Word32 'False 
                                                 #{offset VkDescriptorPoolCreateInfo, maxSets}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "poolSizeCount" Word32 'False 
                                                       #{offset VkDescriptorPoolCreateInfo, poolSizeCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pPoolSizes" (Ptr VkDescriptorPoolSize) 'False
                #{offset VkDescriptorPoolCreateInfo, pPoolSizes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDescriptorPoolSize {
--   >     VkDescriptorType       type;
--   >     uint32_t               descriptorCount;
--   > } VkDescriptorPoolSize;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorPoolSize VkDescriptorPoolSize registry at www.khronos.org>
type VkDescriptorPoolSize = VkStruct VkDescriptorPoolSize' -- ' closing tick for hsc2hs

data VkDescriptorPoolSize' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDescriptorPoolSize where
    type StructRep VkDescriptorPoolSize =
         'StructMeta "VkDescriptorPoolSize" VkDescriptorPoolSize  -- ' closing tick for hsc2hs
                                                                 #{size VkDescriptorPoolSize}
           #{alignment VkDescriptorPoolSize}
           '[('FieldMeta "type" VkDescriptorType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDescriptorPoolSize, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorCount" Word32 'False 
                                                         #{offset VkDescriptorPoolSize, descriptorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDescriptorSetAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorPool       descriptorPool;
--   >     uint32_t               descriptorSetCount;
--   >     const VkDescriptorSetLayout* pSetLayouts;
--   > } VkDescriptorSetAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetAllocateInfo VkDescriptorSetAllocateInfo registry at www.khronos.org>
type VkDescriptorSetAllocateInfo =
     VkStruct VkDescriptorSetAllocateInfo' -- ' closing tick for hsc2hs

data VkDescriptorSetAllocateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDescriptorSetAllocateInfo where
    type StructRep VkDescriptorSetAllocateInfo =
         'StructMeta "VkDescriptorSetAllocateInfo" -- ' closing tick for hsc2hs
           VkDescriptorSetAllocateInfo
           #{size VkDescriptorSetAllocateInfo}
           #{alignment VkDescriptorSetAllocateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDescriptorSetAllocateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDescriptorSetAllocateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorPool" VkDescriptorPool 'False 
                                                                  #{offset VkDescriptorSetAllocateInfo, descriptorPool}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorSetCount" Word32 'False 
                                                            #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSetLayouts" (Ptr VkDescriptorSetLayout) 'False
                #{offset VkDescriptorSetAllocateInfo, pSetLayouts}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDescriptorSetLayoutBinding {
--   >     uint32_t               binding;
--   >     VkDescriptorType       descriptorType;
--   >     uint32_t descriptorCount;
--   >     VkShaderStageFlags     stageFlags;
--   >     const VkSampler*       pImmutableSamplers;
--   > } VkDescriptorSetLayoutBinding;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetLayoutBinding VkDescriptorSetLayoutBinding registry at www.khronos.org>
type VkDescriptorSetLayoutBinding =
     VkStruct VkDescriptorSetLayoutBinding' -- ' closing tick for hsc2hs

data VkDescriptorSetLayoutBinding' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDescriptorSetLayoutBinding where
    type StructRep VkDescriptorSetLayoutBinding =
         'StructMeta "VkDescriptorSetLayoutBinding" -- ' closing tick for hsc2hs
           VkDescriptorSetLayoutBinding
           #{size VkDescriptorSetLayoutBinding}
           #{alignment VkDescriptorSetLayoutBinding}
           '[('FieldMeta "binding" Word32 'False  -- ' closing tick for hsc2hs
                                                 #{offset VkDescriptorSetLayoutBinding, binding}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorType" VkDescriptorType 'False 
                                                                  #{offset VkDescriptorSetLayoutBinding, descriptorType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorCount" Word32 'True 
                                                        #{offset VkDescriptorSetLayoutBinding, descriptorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stageFlags" VkShaderStageFlags 'False 
                                                                #{offset VkDescriptorSetLayoutBinding, stageFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pImmutableSamplers" (Ptr VkSampler) 'True 
                                                                    #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDescriptorSetLayoutBindingFlagsCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               bindingCount;
--   >     const VkDescriptorBindingFlagsEXT* pBindingFlags;
--   > } VkDescriptorSetLayoutBindingFlagsCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetLayoutBindingFlagsCreateInfoEXT VkDescriptorSetLayoutBindingFlagsCreateInfoEXT registry at www.khronos.org>
type VkDescriptorSetLayoutBindingFlagsCreateInfoEXT =
     VkStruct VkDescriptorSetLayoutBindingFlagsCreateInfoEXT' -- ' closing tick for hsc2hs

data VkDescriptorSetLayoutBindingFlagsCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
    type StructRep VkDescriptorSetLayoutBindingFlagsCreateInfoEXT =
         'StructMeta "VkDescriptorSetLayoutBindingFlagsCreateInfoEXT" -- ' closing tick for hsc2hs
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
           #{size VkDescriptorSetLayoutBindingFlagsCreateInfoEXT}
           #{alignment VkDescriptorSetLayoutBindingFlagsCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bindingCount" Word32 'True 
                                                     #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pBindingFlags" (Ptr VkDescriptorBindingFlagsEXT) -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkDescriptorSetLayoutCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkDescriptorSetLayoutCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorSetLayoutCreateFlags    flags;
--   >     uint32_t               bindingCount;
--   >     const VkDescriptorSetLayoutBinding* pBindings;
--   > } VkDescriptorSetLayoutCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetLayoutCreateInfo VkDescriptorSetLayoutCreateInfo registry at www.khronos.org>
type VkDescriptorSetLayoutCreateInfo =
     VkStruct VkDescriptorSetLayoutCreateInfo' -- ' closing tick for hsc2hs

data VkDescriptorSetLayoutCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDescriptorSetLayoutCreateInfo where
    type StructRep VkDescriptorSetLayoutCreateInfo =
         'StructMeta "VkDescriptorSetLayoutCreateInfo" -- ' closing tick for hsc2hs
           VkDescriptorSetLayoutCreateInfo
           #{size VkDescriptorSetLayoutCreateInfo}
           #{alignment VkDescriptorSetLayoutCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDescriptorSetLayoutCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDescriptorSetLayoutCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDescriptorSetLayoutCreateFlags 'True
                #{offset VkDescriptorSetLayoutCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bindingCount" Word32 'True 
                                                     #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pBindings" (Ptr VkDescriptorSetLayoutBinding) 'False
                #{offset VkDescriptorSetLayoutCreateInfo, pBindings}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDescriptorSetLayoutSupport {
--   >     VkStructureType sType;
--   >     void*            pNext;
--   >     VkBool32         supported;
--   > } VkDescriptorSetLayoutSupport;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetLayoutSupport VkDescriptorSetLayoutSupport registry at www.khronos.org>
type VkDescriptorSetLayoutSupport =
     VkStruct VkDescriptorSetLayoutSupport' -- ' closing tick for hsc2hs

data VkDescriptorSetLayoutSupport' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDescriptorSetLayoutSupport where
    type StructRep VkDescriptorSetLayoutSupport =
         'StructMeta "VkDescriptorSetLayoutSupport" -- ' closing tick for hsc2hs
           VkDescriptorSetLayoutSupport
           #{size VkDescriptorSetLayoutSupport}
           #{alignment VkDescriptorSetLayoutSupport}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDescriptorSetLayoutSupport, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDescriptorSetLayoutSupport, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supported" VkBool32 'False 
                                                     #{offset VkDescriptorSetLayoutSupport, supported}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

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
type VkDescriptorSetVariableDescriptorCountAllocateInfoEXT =
     VkStruct VkDescriptorSetVariableDescriptorCountAllocateInfoEXT' -- ' closing tick for hsc2hs

data VkDescriptorSetVariableDescriptorCountAllocateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
    type StructRep
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         =
         'StructMeta "VkDescriptorSetVariableDescriptorCountAllocateInfoEXT" -- ' closing tick for hsc2hs
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
           #{size VkDescriptorSetVariableDescriptorCountAllocateInfoEXT}
           #{alignment VkDescriptorSetVariableDescriptorCountAllocateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorSetCount" Word32 'True 
                                                           #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDescriptorCounts" (Ptr Word32) 'False 
                                                                 #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkDescriptorSetAllocateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkDescriptorSetVariableDescriptorCountLayoutSupportEXT {
--   >     VkStructureType sType;
--   >     void*            pNext;
--   >     uint32_t         maxVariableDescriptorCount;
--   > } VkDescriptorSetVariableDescriptorCountLayoutSupportEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorSetVariableDescriptorCountLayoutSupportEXT VkDescriptorSetVariableDescriptorCountLayoutSupportEXT registry at www.khronos.org>
type VkDescriptorSetVariableDescriptorCountLayoutSupportEXT =
     VkStruct VkDescriptorSetVariableDescriptorCountLayoutSupportEXT' -- ' closing tick for hsc2hs

data VkDescriptorSetVariableDescriptorCountLayoutSupportEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
    type StructRep
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         =
         'StructMeta -- ' closing tick for hsc2hs
           "VkDescriptorSetVariableDescriptorCountLayoutSupportEXT"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
           #{size VkDescriptorSetVariableDescriptorCountLayoutSupportEXT}
           #{alignment VkDescriptorSetVariableDescriptorCountLayoutSupportEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxVariableDescriptorCount" Word32 'False 
                                                                    #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkDescriptorSetLayoutSupport] -- ' closing tick for hsc2hs

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
type VkDescriptorUpdateTemplateCreateInfo =
     VkStruct VkDescriptorUpdateTemplateCreateInfo' -- ' closing tick for hsc2hs

data VkDescriptorUpdateTemplateCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDescriptorUpdateTemplateCreateInfo where
    type StructRep VkDescriptorUpdateTemplateCreateInfo =
         'StructMeta "VkDescriptorUpdateTemplateCreateInfo" -- ' closing tick for hsc2hs
           VkDescriptorUpdateTemplateCreateInfo
           #{size VkDescriptorUpdateTemplateCreateInfo}
           #{alignment VkDescriptorUpdateTemplateCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDescriptorUpdateTemplateCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDescriptorUpdateTemplateCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDescriptorUpdateTemplateCreateFlags 'True
                #{offset VkDescriptorUpdateTemplateCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorUpdateEntryCount" Word32 'False 
                                                                    #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDescriptorUpdateEntries" -- ' closing tick for hsc2hs
                (Ptr VkDescriptorUpdateTemplateEntry)
                'False -- ' closing tick for hsc2hs
                #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "templateType" VkDescriptorUpdateTemplateType 'False
                #{offset VkDescriptorUpdateTemplateCreateInfo, templateType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorSetLayout" VkDescriptorSetLayout 'True
                #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineBindPoint" VkPipelineBindPoint 'False
                #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineLayout" VkPipelineLayout 'False 
                                                                  #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "set" Word32 'False 
                                             #{offset VkDescriptorUpdateTemplateCreateInfo, set}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

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
type VkDescriptorUpdateTemplateEntry =
     VkStruct VkDescriptorUpdateTemplateEntry' -- ' closing tick for hsc2hs

data VkDescriptorUpdateTemplateEntry' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDescriptorUpdateTemplateEntry where
    type StructRep VkDescriptorUpdateTemplateEntry =
         'StructMeta "VkDescriptorUpdateTemplateEntry" -- ' closing tick for hsc2hs
           VkDescriptorUpdateTemplateEntry
           #{size VkDescriptorUpdateTemplateEntry}
           #{alignment VkDescriptorUpdateTemplateEntry}
           '[('FieldMeta "dstBinding" Word32 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkDescriptorUpdateTemplateEntry, dstBinding}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstArrayElement" Word32 'False 
                                                         #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorCount" Word32 'False 
                                                         #{offset VkDescriptorUpdateTemplateEntry, descriptorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorType" VkDescriptorType 'False 
                                                                  #{offset VkDescriptorUpdateTemplateEntry, descriptorType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" CSize 'False 
                                               #{offset VkDescriptorUpdateTemplateEntry, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stride" CSize 'False 
                                               #{offset VkDescriptorUpdateTemplateEntry, stride}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkDescriptorUpdateTemplateEntry`
type VkDescriptorUpdateTemplateEntryKHR =
     VkDescriptorUpdateTemplateEntry
