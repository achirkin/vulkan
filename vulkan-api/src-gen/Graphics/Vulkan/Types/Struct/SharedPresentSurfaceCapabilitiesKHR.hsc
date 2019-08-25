#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SharedPresentSurfaceCapabilitiesKHR
       (VkSharedPresentSurfaceCapabilitiesKHR) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Image         (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Surface     (VkSurfaceCapabilities2KHR)

-- | > typedef struct VkSharedPresentSurfaceCapabilitiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkImageUsageFlags sharedPresentSupportedUsageFlags;
--   > } VkSharedPresentSurfaceCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSharedPresentSurfaceCapabilitiesKHR VkSharedPresentSurfaceCapabilitiesKHR registry at www.khronos.org>
type VkSharedPresentSurfaceCapabilitiesKHR =
     VkStruct VkSharedPresentSurfaceCapabilitiesKHR' -- ' closing tick for hsc2hs

data VkSharedPresentSurfaceCapabilitiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSharedPresentSurfaceCapabilitiesKHR where
    type StructRep VkSharedPresentSurfaceCapabilitiesKHR =
         'StructMeta "VkSharedPresentSurfaceCapabilitiesKHR" -- ' closing tick for hsc2hs
           VkSharedPresentSurfaceCapabilitiesKHR
           #{size VkSharedPresentSurfaceCapabilitiesKHR}
           #{alignment VkSharedPresentSurfaceCapabilitiesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sharedPresentSupportedUsageFlags" VkImageUsageFlags -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkSurfaceCapabilities2KHR] -- ' closing tick for hsc2hs
