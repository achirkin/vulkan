#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Validation
       (VkValidationCacheCreateInfoEXT, VkValidationFlagsEXT) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                  (VkValidationCacheCreateFlagsEXT)
import           Graphics.Vulkan.Types.Enum.StructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Enum.ValidationC          (VkValidationCheckEXT)
import           Graphics.Vulkan.Types.Struct.InstanceCreateInfo (VkInstanceCreateInfo)

-- | > typedef struct VkValidationCacheCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkValidationCacheCreateFlagsEXT    flags;
--   >     size_t                 initialDataSize;
--   >     const void*            pInitialData;
--   > } VkValidationCacheCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkValidationCacheCreateInfoEXT VkValidationCacheCreateInfoEXT registry at www.khronos.org>
type VkValidationCacheCreateInfoEXT =
     VkStruct VkValidationCacheCreateInfoEXT' -- ' closing tick for hsc2hs

data VkValidationCacheCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkValidationCacheCreateInfoEXT where
    type StructRep VkValidationCacheCreateInfoEXT =
         'StructMeta "VkValidationCacheCreateInfoEXT" -- ' closing tick for hsc2hs
           VkValidationCacheCreateInfoEXT
           #{size VkValidationCacheCreateInfoEXT}
           #{alignment VkValidationCacheCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkValidationCacheCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkValidationCacheCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkValidationCacheCreateFlagsEXT 'True 
                                                                       #{offset VkValidationCacheCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "initialDataSize" CSize 'True 
                                                       #{offset VkValidationCacheCreateInfoEXT, initialDataSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pInitialData" (Ptr Void) 'False 
                                                          #{offset VkValidationCacheCreateInfoEXT, pInitialData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkValidationFlagsEXT {
--   >     VkStructureType                  sType;
--   >     const void*                      pNext;
--   >     uint32_t                         disabledValidationCheckCount;
--   >     VkValidationCheckEXT* pDisabledValidationChecks;
--   > } VkValidationFlagsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkValidationFlagsEXT VkValidationFlagsEXT registry at www.khronos.org>
type VkValidationFlagsEXT = VkStruct VkValidationFlagsEXT' -- ' closing tick for hsc2hs

data VkValidationFlagsEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkValidationFlagsEXT where
    type StructRep VkValidationFlagsEXT =
         'StructMeta "VkValidationFlagsEXT" VkValidationFlagsEXT  -- ' closing tick for hsc2hs
                                                                 #{size VkValidationFlagsEXT}
           #{alignment VkValidationFlagsEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkValidationFlagsEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkValidationFlagsEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "disabledValidationCheckCount" Word32 'False 
                                                                      #{offset VkValidationFlagsEXT, disabledValidationCheckCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDisabledValidationChecks" (Ptr VkValidationCheckEXT) -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkValidationFlagsEXT, pDisabledValidationChecks}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkInstanceCreateInfo] -- ' closing tick for hsc2hs
