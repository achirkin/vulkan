#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SwapchainC
       (VkSwapchainCounterCreateInfoEXT, VkSwapchainCreateInfoKHR) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                    (VkBool32)
import           Graphics.Vulkan.Types.Enum.Color                   (VkColorSpaceKHR)
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR  (VkCompositeAlphaFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.Format                  (VkFormat)
import           Graphics.Vulkan.Types.Enum.Image                   (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.PresentModeKHR          (VkPresentModeKHR)
import           Graphics.Vulkan.Types.Enum.SharingMode             (VkSharingMode)
import           Graphics.Vulkan.Types.Enum.StructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Enum.Surface                 (VkSurfaceCounterFlagsEXT,
                                                                     VkSurfaceTransformFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.SwapchainCreateFlagsKHR (VkSwapchainCreateFlagsKHR)
import           Graphics.Vulkan.Types.Handles                      (VkSurfaceKHR,
                                                                     VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.Extent                (VkExtent2D)

-- | > typedef struct VkSwapchainCounterCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSurfaceCounterFlagsEXT         surfaceCounters;
--   > } VkSwapchainCounterCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSwapchainCounterCreateInfoEXT VkSwapchainCounterCreateInfoEXT registry at www.khronos.org>
type VkSwapchainCounterCreateInfoEXT =
     VkStruct VkSwapchainCounterCreateInfoEXT' -- ' closing tick for hsc2hs

data VkSwapchainCounterCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSwapchainCounterCreateInfoEXT where
    type StructRep VkSwapchainCounterCreateInfoEXT =
         'StructMeta "VkSwapchainCounterCreateInfoEXT" -- ' closing tick for hsc2hs
           VkSwapchainCounterCreateInfoEXT
           #{size VkSwapchainCounterCreateInfoEXT}
           #{alignment VkSwapchainCounterCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSwapchainCounterCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSwapchainCounterCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "surfaceCounters" VkSurfaceCounterFlagsEXT 'True
                #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSwapchainCreateInfoKHR] -- ' closing tick for hsc2hs

-- | > typedef struct VkSwapchainCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainCreateFlagsKHR        flags;
--   >     VkSurfaceKHR                     surface;
--   >     uint32_t                         minImageCount;
--   >     VkFormat                         imageFormat;
--   >     VkColorSpaceKHR                  imageColorSpace;
--   >     VkExtent2D                       imageExtent;
--   >     uint32_t                         imageArrayLayers;
--   >     VkImageUsageFlags                imageUsage;
--   >     VkSharingMode                    imageSharingMode;
--   >     uint32_t         queueFamilyIndexCount;
--   >     const uint32_t*                  pQueueFamilyIndices;
--   >     VkSurfaceTransformFlagBitsKHR    preTransform;
--   >     VkCompositeAlphaFlagBitsKHR      compositeAlpha;
--   >     VkPresentModeKHR                 presentMode;
--   >     VkBool32                         clipped;
--   >     VkSwapchainKHR   oldSwapchain;
--   > } VkSwapchainCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSwapchainCreateInfoKHR VkSwapchainCreateInfoKHR registry at www.khronos.org>
type VkSwapchainCreateInfoKHR = VkStruct VkSwapchainCreateInfoKHR' -- ' closing tick for hsc2hs

data VkSwapchainCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSwapchainCreateInfoKHR where
    type StructRep VkSwapchainCreateInfoKHR =
         'StructMeta "VkSwapchainCreateInfoKHR" VkSwapchainCreateInfoKHR -- ' closing tick for hsc2hs
           #{size VkSwapchainCreateInfoKHR}
           #{alignment VkSwapchainCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSwapchainCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSwapchainCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkSwapchainCreateFlagsKHR 'True 
                                                                 #{offset VkSwapchainCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "surface" VkSurfaceKHR 'False 
                                                       #{offset VkSwapchainCreateInfoKHR, surface}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minImageCount" Word32 'False 
                                                       #{offset VkSwapchainCreateInfoKHR, minImageCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageFormat" VkFormat 'False 
                                                       #{offset VkSwapchainCreateInfoKHR, imageFormat}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageColorSpace" VkColorSpaceKHR 'False 
                                                                  #{offset VkSwapchainCreateInfoKHR, imageColorSpace}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageExtent" VkExtent2D 'False 
                                                         #{offset VkSwapchainCreateInfoKHR, imageExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageArrayLayers" Word32 'False 
                                                          #{offset VkSwapchainCreateInfoKHR, imageArrayLayers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageUsage" VkImageUsageFlags 'False 
                                                               #{offset VkSwapchainCreateInfoKHR, imageUsage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageSharingMode" VkSharingMode 'False 
                                                                 #{offset VkSwapchainCreateInfoKHR, imageSharingMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueFamilyIndexCount" Word32 'True 
                                                              #{offset VkSwapchainCreateInfoKHR, queueFamilyIndexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pQueueFamilyIndices" (Ptr Word32) 'False 
                                                                   #{offset VkSwapchainCreateInfoKHR, pQueueFamilyIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "preTransform" VkSurfaceTransformFlagBitsKHR 'False
                #{offset VkSwapchainCreateInfoKHR, preTransform}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compositeAlpha" VkCompositeAlphaFlagBitsKHR 'False
                #{offset VkSwapchainCreateInfoKHR, compositeAlpha}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "presentMode" VkPresentModeKHR 'False 
                                                               #{offset VkSwapchainCreateInfoKHR, presentMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "clipped" VkBool32 'False 
                                                   #{offset VkSwapchainCreateInfoKHR, clipped}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "oldSwapchain" VkSwapchainKHR 'True 
                                                             #{offset VkSwapchainCreateInfoKHR, oldSwapchain}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
