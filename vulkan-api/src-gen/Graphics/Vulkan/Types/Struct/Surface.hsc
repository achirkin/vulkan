#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Surface
       (VkSurfaceCapabilities2EXT, VkSurfaceCapabilities2KHR,
        VkSurfaceCapabilitiesKHR, VkSurfaceFormat2KHR, VkSurfaceFormatKHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Color                  (VkColorSpaceKHR)
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR (VkCompositeAlphaFlagsKHR)
import           Graphics.Vulkan.Types.Enum.Format                 (VkFormat)
import           Graphics.Vulkan.Types.Enum.Image                  (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType          (VkStructureType)
import           Graphics.Vulkan.Types.Enum.Surface                (VkSurfaceCounterFlagsEXT,
                                                                    VkSurfaceTransformFlagBitsKHR,
                                                                    VkSurfaceTransformFlagsKHR)
import           Graphics.Vulkan.Types.Struct.Extent               (VkExtent2D)

-- | > typedef struct VkSurfaceCapabilities2EXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         minImageCount;
--   >     uint32_t                         maxImageCount;
--   >     VkExtent2D                       currentExtent;
--   >     VkExtent2D                       minImageExtent;
--   >     VkExtent2D                       maxImageExtent;
--   >     uint32_t                         maxImageArrayLayers;
--   >     VkSurfaceTransformFlagsKHR       supportedTransforms;
--   >     VkSurfaceTransformFlagBitsKHR    currentTransform;
--   >     VkCompositeAlphaFlagsKHR         supportedCompositeAlpha;
--   >     VkImageUsageFlags                supportedUsageFlags;
--   >     VkSurfaceCounterFlagsEXT supportedSurfaceCounters;
--   > } VkSurfaceCapabilities2EXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceCapabilities2EXT VkSurfaceCapabilities2EXT registry at www.khronos.org>
type VkSurfaceCapabilities2EXT =
     VkStruct VkSurfaceCapabilities2EXT' -- ' closing tick for hsc2hs

data VkSurfaceCapabilities2EXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSurfaceCapabilities2EXT where
    type StructRep VkSurfaceCapabilities2EXT =
         'StructMeta "VkSurfaceCapabilities2EXT" VkSurfaceCapabilities2EXT -- ' closing tick for hsc2hs
           #{size VkSurfaceCapabilities2EXT}
           #{alignment VkSurfaceCapabilities2EXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSurfaceCapabilities2EXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSurfaceCapabilities2EXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minImageCount" Word32 'False 
                                                       #{offset VkSurfaceCapabilities2EXT, minImageCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxImageCount" Word32 'False 
                                                       #{offset VkSurfaceCapabilities2EXT, maxImageCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "currentExtent" VkExtent2D 'False 
                                                           #{offset VkSurfaceCapabilities2EXT, currentExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minImageExtent" VkExtent2D 'False 
                                                            #{offset VkSurfaceCapabilities2EXT, minImageExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxImageExtent" VkExtent2D 'False 
                                                            #{offset VkSurfaceCapabilities2EXT, maxImageExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxImageArrayLayers" Word32 'False 
                                                             #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedTransforms" VkSurfaceTransformFlagsKHR 'True
                #{offset VkSurfaceCapabilities2EXT, supportedTransforms}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "currentTransform" VkSurfaceTransformFlagBitsKHR 'False
                #{offset VkSurfaceCapabilities2EXT, currentTransform}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedCompositeAlpha" VkCompositeAlphaFlagsKHR -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedUsageFlags" VkImageUsageFlags 'True 
                                                                       #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedSurfaceCounters" VkSurfaceCounterFlagsEXT -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSurfaceCapabilities2KHR {
--   >     VkStructureType sType;
--   >     void*   pNext;
--   >     VkSurfaceCapabilitiesKHR surfaceCapabilities;
--   > } VkSurfaceCapabilities2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceCapabilities2KHR VkSurfaceCapabilities2KHR registry at www.khronos.org>
type VkSurfaceCapabilities2KHR =
     VkStruct VkSurfaceCapabilities2KHR' -- ' closing tick for hsc2hs

data VkSurfaceCapabilities2KHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSurfaceCapabilities2KHR where
    type StructRep VkSurfaceCapabilities2KHR =
         'StructMeta "VkSurfaceCapabilities2KHR" VkSurfaceCapabilities2KHR -- ' closing tick for hsc2hs
           #{size VkSurfaceCapabilities2KHR}
           #{alignment VkSurfaceCapabilities2KHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSurfaceCapabilities2KHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSurfaceCapabilities2KHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "surfaceCapabilities" VkSurfaceCapabilitiesKHR 'False
                #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSurfaceCapabilitiesKHR {
--   >     uint32_t                         minImageCount;
--   >     uint32_t                         maxImageCount;
--   >     VkExtent2D                       currentExtent;
--   >     VkExtent2D                       minImageExtent;
--   >     VkExtent2D                       maxImageExtent;
--   >     uint32_t                         maxImageArrayLayers;
--   >     VkSurfaceTransformFlagsKHR       supportedTransforms;
--   >     VkSurfaceTransformFlagBitsKHR    currentTransform;
--   >     VkCompositeAlphaFlagsKHR         supportedCompositeAlpha;
--   >     VkImageUsageFlags                supportedUsageFlags;
--   > } VkSurfaceCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceCapabilitiesKHR VkSurfaceCapabilitiesKHR registry at www.khronos.org>
type VkSurfaceCapabilitiesKHR = VkStruct VkSurfaceCapabilitiesKHR' -- ' closing tick for hsc2hs

data VkSurfaceCapabilitiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSurfaceCapabilitiesKHR where
    type StructRep VkSurfaceCapabilitiesKHR =
         'StructMeta "VkSurfaceCapabilitiesKHR" VkSurfaceCapabilitiesKHR -- ' closing tick for hsc2hs
           #{size VkSurfaceCapabilitiesKHR}
           #{alignment VkSurfaceCapabilitiesKHR}
           '[('FieldMeta "minImageCount" Word32 'False  -- ' closing tick for hsc2hs
                                                       #{offset VkSurfaceCapabilitiesKHR, minImageCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxImageCount" Word32 'False 
                                                       #{offset VkSurfaceCapabilitiesKHR, maxImageCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "currentExtent" VkExtent2D 'False 
                                                           #{offset VkSurfaceCapabilitiesKHR, currentExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minImageExtent" VkExtent2D 'False 
                                                            #{offset VkSurfaceCapabilitiesKHR, minImageExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxImageExtent" VkExtent2D 'False 
                                                            #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxImageArrayLayers" Word32 'False 
                                                             #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedTransforms" VkSurfaceTransformFlagsKHR 'True
                #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "currentTransform" VkSurfaceTransformFlagBitsKHR 'False
                #{offset VkSurfaceCapabilitiesKHR, currentTransform}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedCompositeAlpha" VkCompositeAlphaFlagsKHR -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedUsageFlags" VkImageUsageFlags 'True 
                                                                       #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSurfaceFormat2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkSurfaceFormatKHR surfaceFormat;
--   > } VkSurfaceFormat2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceFormat2KHR VkSurfaceFormat2KHR registry at www.khronos.org>
type VkSurfaceFormat2KHR = VkStruct VkSurfaceFormat2KHR' -- ' closing tick for hsc2hs

data VkSurfaceFormat2KHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSurfaceFormat2KHR where
    type StructRep VkSurfaceFormat2KHR =
         'StructMeta "VkSurfaceFormat2KHR" VkSurfaceFormat2KHR  -- ' closing tick for hsc2hs
                                                               #{size VkSurfaceFormat2KHR}
           #{alignment VkSurfaceFormat2KHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSurfaceFormat2KHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSurfaceFormat2KHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "surfaceFormat" VkSurfaceFormatKHR 'False 
                                                                   #{offset VkSurfaceFormat2KHR, surfaceFormat}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSurfaceFormatKHR {
--   >     VkFormat                         format;
--   >     VkColorSpaceKHR                  colorSpace;
--   > } VkSurfaceFormatKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceFormatKHR VkSurfaceFormatKHR registry at www.khronos.org>
type VkSurfaceFormatKHR = VkStruct VkSurfaceFormatKHR' -- ' closing tick for hsc2hs

data VkSurfaceFormatKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSurfaceFormatKHR where
    type StructRep VkSurfaceFormatKHR =
         'StructMeta "VkSurfaceFormatKHR" VkSurfaceFormatKHR  -- ' closing tick for hsc2hs
                                                             #{size VkSurfaceFormatKHR}
           #{alignment VkSurfaceFormatKHR}
           '[('FieldMeta "format" VkFormat 'False  -- ' closing tick for hsc2hs
                                                  #{offset VkSurfaceFormatKHR, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "colorSpace" VkColorSpaceKHR 'False 
                                                             #{offset VkSurfaceFormatKHR, colorSpace}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
