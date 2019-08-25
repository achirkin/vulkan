#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Base
       (VkBaseInStructure, VkBaseOutStructure) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkBaseInStructure {
--   >     VkStructureType sType;
--   >     const struct VkBaseInStructure* pNext;
--   > } VkBaseInStructure;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBaseInStructure VkBaseInStructure registry at www.khronos.org>
type VkBaseInStructure = VkStruct VkBaseInStructure' -- ' closing tick for hsc2hs

data VkBaseInStructure' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBaseInStructure where
    type StructRep VkBaseInStructure =
         'StructMeta "VkBaseInStructure" VkBaseInStructure  -- ' closing tick for hsc2hs
                                                           #{size VkBaseInStructure}
           #{alignment VkBaseInStructure}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBaseInStructure, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr VkBaseInStructure) 'False 
                                                                #{offset VkBaseInStructure, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkBaseOutStructure {
--   >     VkStructureType sType;
--   >     struct VkBaseOutStructure* pNext;
--   > } VkBaseOutStructure;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBaseOutStructure VkBaseOutStructure registry at www.khronos.org>
type VkBaseOutStructure = VkStruct VkBaseOutStructure' -- ' closing tick for hsc2hs

data VkBaseOutStructure' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBaseOutStructure where
    type StructRep VkBaseOutStructure =
         'StructMeta "VkBaseOutStructure" VkBaseOutStructure  -- ' closing tick for hsc2hs
                                                             #{size VkBaseOutStructure}
           #{alignment VkBaseOutStructure}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBaseOutStructure, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr VkBaseOutStructure) 'False 
                                                                 #{offset VkBaseOutStructure, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
