#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.CoarseSample
       (VkCoarseSampleLocationNV, VkCoarseSampleOrderCustomNV) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.ShadingRatePaletteEntryNV (VkShadingRatePaletteEntryNV)

-- | > typedef struct VkCoarseSampleLocationNV {
--   >     uint32_t                            pixelX;
--   >     uint32_t                            pixelY;
--   >     uint32_t                            sample;
--   > } VkCoarseSampleLocationNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCoarseSampleLocationNV VkCoarseSampleLocationNV registry at www.khronos.org>
type VkCoarseSampleLocationNV = VkStruct VkCoarseSampleLocationNV' -- ' closing tick for hsc2hs

data VkCoarseSampleLocationNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCoarseSampleLocationNV where
    type StructRep VkCoarseSampleLocationNV =
         'StructMeta "VkCoarseSampleLocationNV" VkCoarseSampleLocationNV -- ' closing tick for hsc2hs
           #{size VkCoarseSampleLocationNV}
           #{alignment VkCoarseSampleLocationNV}
           '[('FieldMeta "pixelX" Word32 'False  -- ' closing tick for hsc2hs
                                                #{offset VkCoarseSampleLocationNV, pixelX}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pixelY" Word32 'False 
                                                #{offset VkCoarseSampleLocationNV, pixelY}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sample" Word32 'False 
                                                #{offset VkCoarseSampleLocationNV, sample}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkCoarseSampleOrderCustomNV {
--   >     VkShadingRatePaletteEntryNV         shadingRate;
--   >     uint32_t                            sampleCount;
--   >     uint32_t                            sampleLocationCount;
--   >     const VkCoarseSampleLocationNV* pSampleLocations;
--   > } VkCoarseSampleOrderCustomNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCoarseSampleOrderCustomNV VkCoarseSampleOrderCustomNV registry at www.khronos.org>
type VkCoarseSampleOrderCustomNV =
     VkStruct VkCoarseSampleOrderCustomNV' -- ' closing tick for hsc2hs

data VkCoarseSampleOrderCustomNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCoarseSampleOrderCustomNV where
    type StructRep VkCoarseSampleOrderCustomNV =
         'StructMeta "VkCoarseSampleOrderCustomNV" -- ' closing tick for hsc2hs
           VkCoarseSampleOrderCustomNV
           #{size VkCoarseSampleOrderCustomNV}
           #{alignment VkCoarseSampleOrderCustomNV}
           '[('FieldMeta "shadingRate" VkShadingRatePaletteEntryNV 'False -- ' closing tick for hsc2hs
                #{offset VkCoarseSampleOrderCustomNV, shadingRate}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleCount" Word32 'False 
                                                     #{offset VkCoarseSampleOrderCustomNV, sampleCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationCount" Word32 'False 
                                                             #{offset VkCoarseSampleOrderCustomNV, sampleLocationCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSampleLocations" (Ptr VkCoarseSampleLocationNV) -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkCoarseSampleOrderCustomNV, pSampleLocations}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
