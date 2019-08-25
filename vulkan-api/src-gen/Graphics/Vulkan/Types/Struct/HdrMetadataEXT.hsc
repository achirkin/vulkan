#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.HdrMetadataEXT
       (VkHdrMetadataEXT) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.XYColorEXT  (VkXYColorEXT)

-- | > typedef struct VkHdrMetadataEXT {
--   >     VkStructureType sType;
--   >     const void*    pNext;
--   >     VkXYColorEXT   displayPrimaryRed;
--   >     VkXYColorEXT   displayPrimaryGreen;
--   >     VkXYColorEXT   displayPrimaryBlue;
--   >     VkXYColorEXT   whitePoint;
--   >     float          maxLuminance;
--   >     float          minLuminance;
--   >     float          maxContentLightLevel;
--   >     float          maxFrameAverageLightLevel;
--   > } VkHdrMetadataEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkHdrMetadataEXT VkHdrMetadataEXT registry at www.khronos.org>
type VkHdrMetadataEXT = VkStruct VkHdrMetadataEXT' -- ' closing tick for hsc2hs

data VkHdrMetadataEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkHdrMetadataEXT where
    type StructRep VkHdrMetadataEXT =
         'StructMeta "VkHdrMetadataEXT" VkHdrMetadataEXT  -- ' closing tick for hsc2hs
                                                         #{size VkHdrMetadataEXT}
           #{alignment VkHdrMetadataEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkHdrMetadataEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkHdrMetadataEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "displayPrimaryRed" VkXYColorEXT 'False 
                                                                 #{offset VkHdrMetadataEXT, displayPrimaryRed}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "displayPrimaryGreen" VkXYColorEXT 'False 
                                                                   #{offset VkHdrMetadataEXT, displayPrimaryGreen}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "displayPrimaryBlue" VkXYColorEXT 'False 
                                                                  #{offset VkHdrMetadataEXT, displayPrimaryBlue}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "whitePoint" VkXYColorEXT 'False 
                                                          #{offset VkHdrMetadataEXT, whitePoint}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxLuminance" ( -- ' closing tick for hsc2hs
                                         #{type float}
                                         ) 'False -- ' closing tick for hsc2hs
                #{offset VkHdrMetadataEXT, maxLuminance}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minLuminance" ( -- ' closing tick for hsc2hs
                                         #{type float}
                                         ) 'False -- ' closing tick for hsc2hs
                #{offset VkHdrMetadataEXT, minLuminance}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxContentLightLevel" ( -- ' closing tick for hsc2hs
                                                 #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkHdrMetadataEXT, maxContentLightLevel}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxFrameAverageLightLevel" ( -- ' closing tick for hsc2hs
                                                      #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
