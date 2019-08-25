#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VertexInput
       (VkVertexInputAttributeDescription,
        VkVertexInputBindingDescription,
        VkVertexInputBindingDivisorDescriptionEXT)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Format          (VkFormat)
import           Graphics.Vulkan.Types.Enum.VertexInputRate (VkVertexInputRate)

-- | > typedef struct VkVertexInputAttributeDescription {
--   >     uint32_t               location;
--   >     uint32_t               binding;
--   >     VkFormat               format;
--   >     uint32_t               offset;
--   > } VkVertexInputAttributeDescription;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkVertexInputAttributeDescription VkVertexInputAttributeDescription registry at www.khronos.org>
type VkVertexInputAttributeDescription =
     VkStruct VkVertexInputAttributeDescription' -- ' closing tick for hsc2hs

data VkVertexInputAttributeDescription' -- ' closing tick for hsc2hs

instance VulkanMarshal VkVertexInputAttributeDescription where
    type StructRep VkVertexInputAttributeDescription =
         'StructMeta "VkVertexInputAttributeDescription" -- ' closing tick for hsc2hs
           VkVertexInputAttributeDescription
           #{size VkVertexInputAttributeDescription}
           #{alignment VkVertexInputAttributeDescription}
           '[('FieldMeta "location" Word32 'False  -- ' closing tick for hsc2hs
                                                  #{offset VkVertexInputAttributeDescription, location}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "binding" Word32 'False 
                                                 #{offset VkVertexInputAttributeDescription, binding}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkFormat 'False 
                                                  #{offset VkVertexInputAttributeDescription, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" Word32 'False 
                                                #{offset VkVertexInputAttributeDescription, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkVertexInputBindingDescription {
--   >     uint32_t               binding;
--   >     uint32_t               stride;
--   >     VkVertexInputRate      inputRate;
--   > } VkVertexInputBindingDescription;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkVertexInputBindingDescription VkVertexInputBindingDescription registry at www.khronos.org>
type VkVertexInputBindingDescription =
     VkStruct VkVertexInputBindingDescription' -- ' closing tick for hsc2hs

data VkVertexInputBindingDescription' -- ' closing tick for hsc2hs

instance VulkanMarshal VkVertexInputBindingDescription where
    type StructRep VkVertexInputBindingDescription =
         'StructMeta "VkVertexInputBindingDescription" -- ' closing tick for hsc2hs
           VkVertexInputBindingDescription
           #{size VkVertexInputBindingDescription}
           #{alignment VkVertexInputBindingDescription}
           '[('FieldMeta "binding" Word32 'False  -- ' closing tick for hsc2hs
                                                 #{offset VkVertexInputBindingDescription, binding}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stride" Word32 'False 
                                                #{offset VkVertexInputBindingDescription, stride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "inputRate" VkVertexInputRate 'False 
                                                              #{offset VkVertexInputBindingDescription, inputRate}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkVertexInputBindingDivisorDescriptionEXT {
--   >     uint32_t          binding;
--   >     uint32_t          divisor;
--   > } VkVertexInputBindingDivisorDescriptionEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkVertexInputBindingDivisorDescriptionEXT VkVertexInputBindingDivisorDescriptionEXT registry at www.khronos.org>
type VkVertexInputBindingDivisorDescriptionEXT =
     VkStruct VkVertexInputBindingDivisorDescriptionEXT' -- ' closing tick for hsc2hs

data VkVertexInputBindingDivisorDescriptionEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkVertexInputBindingDivisorDescriptionEXT
         where
    type StructRep VkVertexInputBindingDivisorDescriptionEXT =
         'StructMeta "VkVertexInputBindingDivisorDescriptionEXT" -- ' closing tick for hsc2hs
           VkVertexInputBindingDivisorDescriptionEXT
           #{size VkVertexInputBindingDivisorDescriptionEXT}
           #{alignment VkVertexInputBindingDivisorDescriptionEXT}
           '[('FieldMeta "binding" Word32 'False  -- ' closing tick for hsc2hs
                                                 #{offset VkVertexInputBindingDivisorDescriptionEXT, binding}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "divisor" Word32 'False 
                                                 #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
