#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DrmFormatModifierProperties
       (VkDrmFormatModifierPropertiesEXT,
        VkDrmFormatModifierPropertiesListEXT)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.Format             (VkFormatFeatureFlags)
import Graphics.Vulkan.Types.Enum.StructureType      (VkStructureType)
import Graphics.Vulkan.Types.Struct.FormatProperties (VkFormatProperties2)

-- | > typedef struct VkDrmFormatModifierPropertiesEXT {
--   >     uint64_t drmFormatModifier;
--   >     uint32_t drmFormatModifierPlaneCount;
--   >     VkFormatFeatureFlags drmFormatModifierTilingFeatures;
--   > } VkDrmFormatModifierPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDrmFormatModifierPropertiesEXT VkDrmFormatModifierPropertiesEXT registry at www.khronos.org>
type VkDrmFormatModifierPropertiesEXT =
     VkStruct VkDrmFormatModifierPropertiesEXT' -- ' closing tick for hsc2hs

data VkDrmFormatModifierPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDrmFormatModifierPropertiesEXT where
    type StructRep VkDrmFormatModifierPropertiesEXT =
         'StructMeta "VkDrmFormatModifierPropertiesEXT" -- ' closing tick for hsc2hs
           VkDrmFormatModifierPropertiesEXT
           #{size VkDrmFormatModifierPropertiesEXT}
           #{alignment VkDrmFormatModifierPropertiesEXT}
           '[('FieldMeta "drmFormatModifier" Word64 'False  -- ' closing tick for hsc2hs
                                                           #{offset VkDrmFormatModifierPropertiesEXT, drmFormatModifier}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "drmFormatModifierPlaneCount" Word32 'False 
                                                                     #{offset VkDrmFormatModifierPropertiesEXT, drmFormatModifierPlaneCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "drmFormatModifierTilingFeatures" VkFormatFeatureFlags -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkDrmFormatModifierPropertiesEXT, drmFormatModifierTilingFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDrmFormatModifierPropertiesListEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     uint32_t drmFormatModifierCount;
--   >     VkDrmFormatModifierPropertiesEXT* pDrmFormatModifierProperties;
--   > } VkDrmFormatModifierPropertiesListEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDrmFormatModifierPropertiesListEXT VkDrmFormatModifierPropertiesListEXT registry at www.khronos.org>
type VkDrmFormatModifierPropertiesListEXT =
     VkStruct VkDrmFormatModifierPropertiesListEXT' -- ' closing tick for hsc2hs

data VkDrmFormatModifierPropertiesListEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDrmFormatModifierPropertiesListEXT where
    type StructRep VkDrmFormatModifierPropertiesListEXT =
         'StructMeta "VkDrmFormatModifierPropertiesListEXT" -- ' closing tick for hsc2hs
           VkDrmFormatModifierPropertiesListEXT
           #{size VkDrmFormatModifierPropertiesListEXT}
           #{alignment VkDrmFormatModifierPropertiesListEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDrmFormatModifierPropertiesListEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDrmFormatModifierPropertiesListEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "drmFormatModifierCount" Word32 'True 
                                                               #{offset VkDrmFormatModifierPropertiesListEXT, drmFormatModifierCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDrmFormatModifierProperties" -- ' closing tick for hsc2hs
                (Ptr VkDrmFormatModifierPropertiesEXT)
                'False -- ' closing tick for hsc2hs
                #{offset VkDrmFormatModifierPropertiesListEXT, pDrmFormatModifierProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkFormatProperties2] -- ' closing tick for hsc2hs
