#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Validation
       (VkValidationCacheCreateInfoEXT, VkValidationFeaturesEXT,
        VkValidationFlagsEXT)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Bitmasks                  (VkValidationCacheCreateFlagsEXT)
import Graphics.Vulkan.Types.Enum.StructureType        (VkStructureType)
import Graphics.Vulkan.Types.Enum.Validation           (VkValidationCheckEXT, VkValidationFeatureDisableEXT,
                                                        VkValidationFeatureEnableEXT)
import Graphics.Vulkan.Types.Struct.InstanceCreateInfo (VkInstanceCreateInfo)

-- | > typedef struct VkValidationCacheCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkValidationCacheCreateFlagsEXT    flags;
--   >     size_t                 initialDataSize;
--   >     const void*            pInitialData;
--   > } VkValidationCacheCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkValidationCacheCreateInfoEXT VkValidationCacheCreateInfoEXT registry at www.khronos.org>
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

-- | > typedef struct VkValidationFeaturesEXT {
--   >     VkStructureType  sType;
--   >     const void*                      pNext;
--   >     uint32_t                         enabledValidationFeatureCount;
--   >     const VkValidationFeatureEnableEXT* pEnabledValidationFeatures;
--   >     uint32_t                         disabledValidationFeatureCount;
--   >     const VkValidationFeatureDisableEXT* pDisabledValidationFeatures;
--   > } VkValidationFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkValidationFeaturesEXT VkValidationFeaturesEXT registry at www.khronos.org>
type VkValidationFeaturesEXT = VkStruct VkValidationFeaturesEXT' -- ' closing tick for hsc2hs

data VkValidationFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkValidationFeaturesEXT where
    type StructRep VkValidationFeaturesEXT =
         'StructMeta "VkValidationFeaturesEXT" VkValidationFeaturesEXT -- ' closing tick for hsc2hs
           #{size VkValidationFeaturesEXT}
           #{alignment VkValidationFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkValidationFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkValidationFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "enabledValidationFeatureCount" Word32 'True 
                                                                      #{offset VkValidationFeaturesEXT, enabledValidationFeatureCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pEnabledValidationFeatures" -- ' closing tick for hsc2hs
                (Ptr VkValidationFeatureEnableEXT)
                'False -- ' closing tick for hsc2hs
                #{offset VkValidationFeaturesEXT, pEnabledValidationFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "disabledValidationFeatureCount" Word32 'True 
                                                                       #{offset VkValidationFeaturesEXT, disabledValidationFeatureCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDisabledValidationFeatures" -- ' closing tick for hsc2hs
                (Ptr VkValidationFeatureDisableEXT)
                'False -- ' closing tick for hsc2hs
                #{offset VkValidationFeaturesEXT, pDisabledValidationFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkInstanceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkValidationFlagsEXT {
--   >     VkStructureType                  sType;
--   >     const void*                      pNext;
--   >     uint32_t                         disabledValidationCheckCount;
--   >     const VkValidationCheckEXT* pDisabledValidationChecks;
--   > } VkValidationFlagsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkValidationFlagsEXT VkValidationFlagsEXT registry at www.khronos.org>
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
