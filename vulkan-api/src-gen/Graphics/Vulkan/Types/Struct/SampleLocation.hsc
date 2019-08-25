#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SampleLocation
       (VkSampleLocationEXT, VkSampleLocationsInfoEXT) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.SampleCountFlags (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.Enum.StructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Extent         (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.Image          (VkImageMemoryBarrier)

-- | > typedef struct VkSampleLocationEXT {
--   >     float                            x;
--   >     float                            y;
--   > } VkSampleLocationEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSampleLocationEXT VkSampleLocationEXT registry at www.khronos.org>
type VkSampleLocationEXT = VkStruct VkSampleLocationEXT' -- ' closing tick for hsc2hs

data VkSampleLocationEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSampleLocationEXT where
    type StructRep VkSampleLocationEXT =
         'StructMeta "VkSampleLocationEXT" VkSampleLocationEXT  -- ' closing tick for hsc2hs
                                                               #{size VkSampleLocationEXT}
           #{alignment VkSampleLocationEXT}
           '[('FieldMeta "x" (
                              #{type float}
                              ) 'False  -- ' closing tick for hsc2hs
                                       #{offset VkSampleLocationEXT, x}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "y" ( -- ' closing tick for hsc2hs
                              #{type float}
                              ) 'False  -- ' closing tick for hsc2hs
                                       #{offset VkSampleLocationEXT, y}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSampleLocationsInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSampleCountFlagBits            sampleLocationsPerPixel;
--   >     VkExtent2D                       sampleLocationGridSize;
--   >     uint32_t                         sampleLocationsCount;
--   >     const VkSampleLocationEXT* pSampleLocations;
--   > } VkSampleLocationsInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSampleLocationsInfoEXT VkSampleLocationsInfoEXT registry at www.khronos.org>
type VkSampleLocationsInfoEXT = VkStruct VkSampleLocationsInfoEXT' -- ' closing tick for hsc2hs

data VkSampleLocationsInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSampleLocationsInfoEXT where
    type StructRep VkSampleLocationsInfoEXT =
         'StructMeta "VkSampleLocationsInfoEXT" VkSampleLocationsInfoEXT -- ' closing tick for hsc2hs
           #{size VkSampleLocationsInfoEXT}
           #{alignment VkSampleLocationsInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSampleLocationsInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSampleLocationsInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationsPerPixel" VkSampleCountFlagBits 'False
                #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationGridSize" VkExtent2D 'False 
                                                                    #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationsCount" Word32 'False 
                                                              #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSampleLocations" (Ptr VkSampleLocationEXT) 'False
                #{offset VkSampleLocationsInfoEXT, pSampleLocations}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageMemoryBarrier] -- ' closing tick for hsc2hs
