#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ShadingRatePaletteNV
       (VkShadingRatePaletteNV) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.ShadingRatePaletteEntryNV (VkShadingRatePaletteEntryNV)

-- | > typedef struct VkShadingRatePaletteNV {
--   >     uint32_t                                                               shadingRatePaletteEntryCount;
--   >     const VkShadingRatePaletteEntryNV*  pShadingRatePaletteEntries;
--   > } VkShadingRatePaletteNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkShadingRatePaletteNV VkShadingRatePaletteNV registry at www.khronos.org>
type VkShadingRatePaletteNV = VkStruct VkShadingRatePaletteNV' -- ' closing tick for hsc2hs

data VkShadingRatePaletteNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkShadingRatePaletteNV where
    type StructRep VkShadingRatePaletteNV =
         'StructMeta "VkShadingRatePaletteNV" VkShadingRatePaletteNV -- ' closing tick for hsc2hs
           #{size VkShadingRatePaletteNV}
           #{alignment VkShadingRatePaletteNV}
           '[('FieldMeta "shadingRatePaletteEntryCount" Word32 'False -- ' closing tick for hsc2hs
                #{offset VkShadingRatePaletteNV, shadingRatePaletteEntryCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pShadingRatePaletteEntries" -- ' closing tick for hsc2hs
                (Ptr VkShadingRatePaletteEntryNV)
                'False -- ' closing tick for hsc2hs
                #{offset VkShadingRatePaletteNV, pShadingRatePaletteEntries}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
