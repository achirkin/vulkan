#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Display
       (VkDisplayEventInfoEXT, VkDisplayModeCreateInfoKHR,
        VkDisplayModeParametersKHR, VkDisplayModeProperties2KHR,
        VkDisplayModePropertiesKHR, VkDisplayPlaneCapabilities2KHR,
        VkDisplayPlaneCapabilitiesKHR, VkDisplayPlaneInfo2KHR,
        VkDisplayPlaneProperties2KHR, VkDisplayPlanePropertiesKHR,
        VkDisplayPowerInfoEXT, VkDisplayPresentInfoKHR,
        VkDisplayProperties2KHR, VkDisplayPropertiesKHR,
        VkDisplaySurfaceCreateInfoKHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks           (VkDisplayModeCreateFlagsKHR,
                                                           VkDisplaySurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.Display       (VkDisplayEventTypeEXT,
                                                           VkDisplayPlaneAlphaFlagBitsKHR,
                                                           VkDisplayPlaneAlphaFlagsKHR,
                                                           VkDisplayPowerStateEXT)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Enum.Surface       (VkSurfaceTransformFlagBitsKHR,
                                                           VkSurfaceTransformFlagsKHR)
import           Graphics.Vulkan.Types.Handles            (VkDisplayKHR,
                                                           VkDisplayModeKHR)
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.Offset      (VkOffset2D)
import           Graphics.Vulkan.Types.Struct.Present     (VkPresentInfoKHR)
import           Graphics.Vulkan.Types.Struct.Rect        (VkRect2D)

-- | > typedef struct VkDisplayEventInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayEventTypeEXT            displayEvent;
--   > } VkDisplayEventInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayEventInfoEXT VkDisplayEventInfoEXT registry at www.khronos.org>
type VkDisplayEventInfoEXT = VkStruct VkDisplayEventInfoEXT' -- ' closing tick for hsc2hs

data VkDisplayEventInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayEventInfoEXT where
    type StructRep VkDisplayEventInfoEXT =
         'StructMeta "VkDisplayEventInfoEXT" VkDisplayEventInfoEXT  -- ' closing tick for hsc2hs
                                                                   #{size VkDisplayEventInfoEXT}
           #{alignment VkDisplayEventInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDisplayEventInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDisplayEventInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "displayEvent" VkDisplayEventTypeEXT 'False 
                                                                     #{offset VkDisplayEventInfoEXT, displayEvent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayModeCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayModeCreateFlagsKHR      flags;
--   >     VkDisplayModeParametersKHR       parameters;
--   > } VkDisplayModeCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayModeCreateInfoKHR VkDisplayModeCreateInfoKHR registry at www.khronos.org>
type VkDisplayModeCreateInfoKHR =
     VkStruct VkDisplayModeCreateInfoKHR' -- ' closing tick for hsc2hs

data VkDisplayModeCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayModeCreateInfoKHR where
    type StructRep VkDisplayModeCreateInfoKHR =
         'StructMeta "VkDisplayModeCreateInfoKHR" VkDisplayModeCreateInfoKHR -- ' closing tick for hsc2hs
           #{size VkDisplayModeCreateInfoKHR}
           #{alignment VkDisplayModeCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDisplayModeCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDisplayModeCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDisplayModeCreateFlagsKHR 'True 
                                                                   #{offset VkDisplayModeCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "parameters" VkDisplayModeParametersKHR 'False
                #{offset VkDisplayModeCreateInfoKHR, parameters}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayModeParametersKHR {
--   >     VkExtent2D                       visibleRegion;
--   >     uint32_t                         refreshRate;
--   > } VkDisplayModeParametersKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayModeParametersKHR VkDisplayModeParametersKHR registry at www.khronos.org>
type VkDisplayModeParametersKHR =
     VkStruct VkDisplayModeParametersKHR' -- ' closing tick for hsc2hs

data VkDisplayModeParametersKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayModeParametersKHR where
    type StructRep VkDisplayModeParametersKHR =
         'StructMeta "VkDisplayModeParametersKHR" VkDisplayModeParametersKHR -- ' closing tick for hsc2hs
           #{size VkDisplayModeParametersKHR}
           #{alignment VkDisplayModeParametersKHR}
           '[('FieldMeta "visibleRegion" VkExtent2D 'False  -- ' closing tick for hsc2hs
                                                           #{offset VkDisplayModeParametersKHR, visibleRegion}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "refreshRate" Word32 'False 
                                                     #{offset VkDisplayModeParametersKHR, refreshRate}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayModeProperties2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDisplayModePropertiesKHR displayModeProperties;
--   > } VkDisplayModeProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayModeProperties2KHR VkDisplayModeProperties2KHR registry at www.khronos.org>
type VkDisplayModeProperties2KHR =
     VkStruct VkDisplayModeProperties2KHR' -- ' closing tick for hsc2hs

data VkDisplayModeProperties2KHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayModeProperties2KHR where
    type StructRep VkDisplayModeProperties2KHR =
         'StructMeta "VkDisplayModeProperties2KHR" -- ' closing tick for hsc2hs
           VkDisplayModeProperties2KHR
           #{size VkDisplayModeProperties2KHR}
           #{alignment VkDisplayModeProperties2KHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDisplayModeProperties2KHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDisplayModeProperties2KHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "displayModeProperties" VkDisplayModePropertiesKHR -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkDisplayModeProperties2KHR, displayModeProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayModePropertiesKHR {
--   >     VkDisplayModeKHR                 displayMode;
--   >     VkDisplayModeParametersKHR       parameters;
--   > } VkDisplayModePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayModePropertiesKHR VkDisplayModePropertiesKHR registry at www.khronos.org>
type VkDisplayModePropertiesKHR =
     VkStruct VkDisplayModePropertiesKHR' -- ' closing tick for hsc2hs

data VkDisplayModePropertiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayModePropertiesKHR where
    type StructRep VkDisplayModePropertiesKHR =
         'StructMeta "VkDisplayModePropertiesKHR" VkDisplayModePropertiesKHR -- ' closing tick for hsc2hs
           #{size VkDisplayModePropertiesKHR}
           #{alignment VkDisplayModePropertiesKHR}
           '[('FieldMeta "displayMode" VkDisplayModeKHR 'False  -- ' closing tick for hsc2hs
                                                               #{offset VkDisplayModePropertiesKHR, displayMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "parameters" VkDisplayModeParametersKHR 'False
                #{offset VkDisplayModePropertiesKHR, parameters}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayPlaneCapabilities2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDisplayPlaneCapabilitiesKHR capabilities;
--   > } VkDisplayPlaneCapabilities2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPlaneCapabilities2KHR VkDisplayPlaneCapabilities2KHR registry at www.khronos.org>
type VkDisplayPlaneCapabilities2KHR =
     VkStruct VkDisplayPlaneCapabilities2KHR' -- ' closing tick for hsc2hs

data VkDisplayPlaneCapabilities2KHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayPlaneCapabilities2KHR where
    type StructRep VkDisplayPlaneCapabilities2KHR =
         'StructMeta "VkDisplayPlaneCapabilities2KHR" -- ' closing tick for hsc2hs
           VkDisplayPlaneCapabilities2KHR
           #{size VkDisplayPlaneCapabilities2KHR}
           #{alignment VkDisplayPlaneCapabilities2KHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDisplayPlaneCapabilities2KHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDisplayPlaneCapabilities2KHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "capabilities" VkDisplayPlaneCapabilitiesKHR 'False
                #{offset VkDisplayPlaneCapabilities2KHR, capabilities}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayPlaneCapabilitiesKHR {
--   >     VkDisplayPlaneAlphaFlagsKHR      supportedAlpha;
--   >     VkOffset2D                       minSrcPosition;
--   >     VkOffset2D                       maxSrcPosition;
--   >     VkExtent2D                       minSrcExtent;
--   >     VkExtent2D                       maxSrcExtent;
--   >     VkOffset2D                       minDstPosition;
--   >     VkOffset2D                       maxDstPosition;
--   >     VkExtent2D                       minDstExtent;
--   >     VkExtent2D                       maxDstExtent;
--   > } VkDisplayPlaneCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPlaneCapabilitiesKHR VkDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
type VkDisplayPlaneCapabilitiesKHR =
     VkStruct VkDisplayPlaneCapabilitiesKHR' -- ' closing tick for hsc2hs

data VkDisplayPlaneCapabilitiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayPlaneCapabilitiesKHR where
    type StructRep VkDisplayPlaneCapabilitiesKHR =
         'StructMeta "VkDisplayPlaneCapabilitiesKHR" -- ' closing tick for hsc2hs
           VkDisplayPlaneCapabilitiesKHR
           #{size VkDisplayPlaneCapabilitiesKHR}
           #{alignment VkDisplayPlaneCapabilitiesKHR}
           '[('FieldMeta "supportedAlpha" VkDisplayPlaneAlphaFlagsKHR 'True -- ' closing tick for hsc2hs
                #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minSrcPosition" VkOffset2D 'False 
                                                            #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSrcPosition" VkOffset2D 'False 
                                                            #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minSrcExtent" VkExtent2D 'False 
                                                          #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSrcExtent" VkExtent2D 'False 
                                                          #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minDstPosition" VkOffset2D 'False 
                                                            #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDstPosition" VkOffset2D 'False 
                                                            #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minDstExtent" VkExtent2D 'False 
                                                          #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDstExtent" VkExtent2D 'False 
                                                          #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayPlaneInfo2KHR {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkDisplayModeKHR mode;
--   >     uint32_t planeIndex;
--   > } VkDisplayPlaneInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPlaneInfo2KHR VkDisplayPlaneInfo2KHR registry at www.khronos.org>
type VkDisplayPlaneInfo2KHR = VkStruct VkDisplayPlaneInfo2KHR' -- ' closing tick for hsc2hs

data VkDisplayPlaneInfo2KHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayPlaneInfo2KHR where
    type StructRep VkDisplayPlaneInfo2KHR =
         'StructMeta "VkDisplayPlaneInfo2KHR" VkDisplayPlaneInfo2KHR -- ' closing tick for hsc2hs
           #{size VkDisplayPlaneInfo2KHR}
           #{alignment VkDisplayPlaneInfo2KHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDisplayPlaneInfo2KHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDisplayPlaneInfo2KHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mode" VkDisplayModeKHR 'False 
                                                        #{offset VkDisplayPlaneInfo2KHR, mode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "planeIndex" Word32 'False 
                                                    #{offset VkDisplayPlaneInfo2KHR, planeIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayPlaneProperties2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDisplayPlanePropertiesKHR displayPlaneProperties;
--   > } VkDisplayPlaneProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPlaneProperties2KHR VkDisplayPlaneProperties2KHR registry at www.khronos.org>
type VkDisplayPlaneProperties2KHR =
     VkStruct VkDisplayPlaneProperties2KHR' -- ' closing tick for hsc2hs

data VkDisplayPlaneProperties2KHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayPlaneProperties2KHR where
    type StructRep VkDisplayPlaneProperties2KHR =
         'StructMeta "VkDisplayPlaneProperties2KHR" -- ' closing tick for hsc2hs
           VkDisplayPlaneProperties2KHR
           #{size VkDisplayPlaneProperties2KHR}
           #{alignment VkDisplayPlaneProperties2KHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDisplayPlaneProperties2KHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDisplayPlaneProperties2KHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "displayPlaneProperties" VkDisplayPlanePropertiesKHR -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkDisplayPlaneProperties2KHR, displayPlaneProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayPlanePropertiesKHR {
--   >     VkDisplayKHR                     currentDisplay;
--   >     uint32_t                         currentStackIndex;
--   > } VkDisplayPlanePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPlanePropertiesKHR VkDisplayPlanePropertiesKHR registry at www.khronos.org>
type VkDisplayPlanePropertiesKHR =
     VkStruct VkDisplayPlanePropertiesKHR' -- ' closing tick for hsc2hs

data VkDisplayPlanePropertiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayPlanePropertiesKHR where
    type StructRep VkDisplayPlanePropertiesKHR =
         'StructMeta "VkDisplayPlanePropertiesKHR" -- ' closing tick for hsc2hs
           VkDisplayPlanePropertiesKHR
           #{size VkDisplayPlanePropertiesKHR}
           #{alignment VkDisplayPlanePropertiesKHR}
           '[('FieldMeta "currentDisplay" VkDisplayKHR 'False  -- ' closing tick for hsc2hs
                                                              #{offset VkDisplayPlanePropertiesKHR, currentDisplay}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "currentStackIndex" Word32 'False 
                                                           #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayPowerInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayPowerStateEXT           powerState;
--   > } VkDisplayPowerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPowerInfoEXT VkDisplayPowerInfoEXT registry at www.khronos.org>
type VkDisplayPowerInfoEXT = VkStruct VkDisplayPowerInfoEXT' -- ' closing tick for hsc2hs

data VkDisplayPowerInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayPowerInfoEXT where
    type StructRep VkDisplayPowerInfoEXT =
         'StructMeta "VkDisplayPowerInfoEXT" VkDisplayPowerInfoEXT  -- ' closing tick for hsc2hs
                                                                   #{size VkDisplayPowerInfoEXT}
           #{alignment VkDisplayPowerInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDisplayPowerInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDisplayPowerInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "powerState" VkDisplayPowerStateEXT 'False 
                                                                    #{offset VkDisplayPowerInfoEXT, powerState}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayPresentInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkRect2D                         srcRect;
--   >     VkRect2D                         dstRect;
--   >     VkBool32                         persistent;
--   > } VkDisplayPresentInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPresentInfoKHR VkDisplayPresentInfoKHR registry at www.khronos.org>
type VkDisplayPresentInfoKHR = VkStruct VkDisplayPresentInfoKHR' -- ' closing tick for hsc2hs

data VkDisplayPresentInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayPresentInfoKHR where
    type StructRep VkDisplayPresentInfoKHR =
         'StructMeta "VkDisplayPresentInfoKHR" VkDisplayPresentInfoKHR -- ' closing tick for hsc2hs
           #{size VkDisplayPresentInfoKHR}
           #{alignment VkDisplayPresentInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDisplayPresentInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDisplayPresentInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcRect" VkRect2D 'False 
                                                   #{offset VkDisplayPresentInfoKHR, srcRect}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstRect" VkRect2D 'False 
                                                   #{offset VkDisplayPresentInfoKHR, dstRect}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "persistent" VkBool32 'False 
                                                      #{offset VkDisplayPresentInfoKHR, persistent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayProperties2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDisplayPropertiesKHR displayProperties;
--   > } VkDisplayProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayProperties2KHR VkDisplayProperties2KHR registry at www.khronos.org>
type VkDisplayProperties2KHR = VkStruct VkDisplayProperties2KHR' -- ' closing tick for hsc2hs

data VkDisplayProperties2KHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayProperties2KHR where
    type StructRep VkDisplayProperties2KHR =
         'StructMeta "VkDisplayProperties2KHR" VkDisplayProperties2KHR -- ' closing tick for hsc2hs
           #{size VkDisplayProperties2KHR}
           #{alignment VkDisplayProperties2KHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDisplayProperties2KHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDisplayProperties2KHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "displayProperties" VkDisplayPropertiesKHR 'False
                #{offset VkDisplayProperties2KHR, displayProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplayPropertiesKHR {
--   >     VkDisplayKHR                     display;
--   >     const char*                      displayName;
--   >     VkExtent2D                       physicalDimensions;
--   >     VkExtent2D                       physicalResolution;
--   >     VkSurfaceTransformFlagsKHR       supportedTransforms;
--   >     VkBool32                         planeReorderPossible;
--   >     VkBool32                         persistentContent;
--   > } VkDisplayPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPropertiesKHR VkDisplayPropertiesKHR registry at www.khronos.org>
type VkDisplayPropertiesKHR = VkStruct VkDisplayPropertiesKHR' -- ' closing tick for hsc2hs

data VkDisplayPropertiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplayPropertiesKHR where
    type StructRep VkDisplayPropertiesKHR =
         'StructMeta "VkDisplayPropertiesKHR" VkDisplayPropertiesKHR -- ' closing tick for hsc2hs
           #{size VkDisplayPropertiesKHR}
           #{alignment VkDisplayPropertiesKHR}
           '[('FieldMeta "display" VkDisplayKHR 'False  -- ' closing tick for hsc2hs
                                                       #{offset VkDisplayPropertiesKHR, display}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "displayName" CString 'False 
                                                      #{offset VkDisplayPropertiesKHR, displayName}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "physicalDimensions" VkExtent2D 'False 
                                                                #{offset VkDisplayPropertiesKHR, physicalDimensions}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "physicalResolution" VkExtent2D 'False 
                                                                #{offset VkDisplayPropertiesKHR, physicalResolution}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedTransforms" VkSurfaceTransformFlagsKHR 'True
                #{offset VkDisplayPropertiesKHR, supportedTransforms}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "planeReorderPossible" VkBool32 'False 
                                                                #{offset VkDisplayPropertiesKHR, planeReorderPossible}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "persistentContent" VkBool32 'False 
                                                             #{offset VkDisplayPropertiesKHR, persistentContent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDisplaySurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplaySurfaceCreateFlagsKHR   flags;
--   >     VkDisplayModeKHR                 displayMode;
--   >     uint32_t                         planeIndex;
--   >     uint32_t                         planeStackIndex;
--   >     VkSurfaceTransformFlagBitsKHR    transform;
--   >     float                            globalAlpha;
--   >     VkDisplayPlaneAlphaFlagBitsKHR   alphaMode;
--   >     VkExtent2D                       imageExtent;
--   > } VkDisplaySurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplaySurfaceCreateInfoKHR VkDisplaySurfaceCreateInfoKHR registry at www.khronos.org>
type VkDisplaySurfaceCreateInfoKHR =
     VkStruct VkDisplaySurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

data VkDisplaySurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDisplaySurfaceCreateInfoKHR where
    type StructRep VkDisplaySurfaceCreateInfoKHR =
         'StructMeta "VkDisplaySurfaceCreateInfoKHR" -- ' closing tick for hsc2hs
           VkDisplaySurfaceCreateInfoKHR
           #{size VkDisplaySurfaceCreateInfoKHR}
           #{alignment VkDisplaySurfaceCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDisplaySurfaceCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDisplaySurfaceCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDisplaySurfaceCreateFlagsKHR 'True 
                                                                      #{offset VkDisplaySurfaceCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "displayMode" VkDisplayModeKHR 'False 
                                                               #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "planeIndex" Word32 'False 
                                                    #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "planeStackIndex" Word32 'False 
                                                         #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transform" VkSurfaceTransformFlagBitsKHR 'False
                #{offset VkDisplaySurfaceCreateInfoKHR, transform}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "globalAlpha" ( -- ' closing tick for hsc2hs
                                        #{type float}
                                        ) 'False -- ' closing tick for hsc2hs
                #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "alphaMode" VkDisplayPlaneAlphaFlagBitsKHR 'False
                #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageExtent" VkExtent2D 'False 
                                                         #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
