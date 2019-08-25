#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.TextureLODGatherFormatPropertiesAMD
       (VkTextureLODGatherFormatPropertiesAMD) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Image       (VkImageFormatProperties2)

-- | > typedef struct VkTextureLODGatherFormatPropertiesAMD {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         supportsTextureGatherLODBiasAMD;
--   > } VkTextureLODGatherFormatPropertiesAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkTextureLODGatherFormatPropertiesAMD VkTextureLODGatherFormatPropertiesAMD registry at www.khronos.org>
type VkTextureLODGatherFormatPropertiesAMD =
     VkStruct VkTextureLODGatherFormatPropertiesAMD' -- ' closing tick for hsc2hs

data VkTextureLODGatherFormatPropertiesAMD' -- ' closing tick for hsc2hs

instance VulkanMarshal VkTextureLODGatherFormatPropertiesAMD where
    type StructRep VkTextureLODGatherFormatPropertiesAMD =
         'StructMeta "VkTextureLODGatherFormatPropertiesAMD" -- ' closing tick for hsc2hs
           VkTextureLODGatherFormatPropertiesAMD
           #{size VkTextureLODGatherFormatPropertiesAMD}
           #{alignment VkTextureLODGatherFormatPropertiesAMD}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkTextureLODGatherFormatPropertiesAMD, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkTextureLODGatherFormatPropertiesAMD, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportsTextureGatherLODBiasAMD" VkBool32 'False
                #{offset VkTextureLODGatherFormatPropertiesAMD, supportsTextureGatherLODBiasAMD}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkImageFormatProperties2] -- ' closing tick for hsc2hs
