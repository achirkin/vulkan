#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Device
       (VkDeviceCreateInfo, VkDeviceEventInfoEXT,
        VkDeviceGeneratedCommandsFeaturesNVX,
        VkDeviceGeneratedCommandsLimitsNVX, VkDeviceGroupBindSparseInfo,
        VkDeviceGroupBindSparseInfoKHR,
        VkDeviceGroupCommandBufferBeginInfo,
        VkDeviceGroupCommandBufferBeginInfoKHR,
        VkDeviceGroupDeviceCreateInfo, VkDeviceGroupDeviceCreateInfoKHR,
        VkDeviceGroupPresentCapabilitiesKHR, VkDeviceGroupPresentInfoKHR,
        VkDeviceGroupRenderPassBeginInfo,
        VkDeviceGroupRenderPassBeginInfoKHR, VkDeviceGroupSubmitInfo,
        VkDeviceGroupSubmitInfoKHR, VkDeviceGroupSwapchainCreateInfoKHR,
        VkDeviceQueueCreateInfo, VkDeviceQueueGlobalPriorityCreateInfoEXT,
        VkDeviceQueueInfo2)
       where
import           Graphics.Vulkan.Constants                           (VK_MAX_DEVICE_GROUP_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                     (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks                      (VkDeviceCreateFlags)
import           Graphics.Vulkan.Types.Enum.Device                   (VkDeviceEventTypeEXT,
                                                                      VkDeviceGroupPresentModeFlagBitsKHR,
                                                                      VkDeviceGroupPresentModeFlagsKHR,
                                                                      VkDeviceQueueCreateFlags)
import           Graphics.Vulkan.Types.Enum.Queue                    (VkQueueGlobalPriorityEXT)
import           Graphics.Vulkan.Types.Enum.StructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Handles                       (VkPhysicalDevice)
import           Graphics.Vulkan.Types.Struct.Bind                   (VkBindSparseInfo)
import           Graphics.Vulkan.Types.Struct.Command                (VkCommandBufferBeginInfo)
import           Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import           Graphics.Vulkan.Types.Struct.Present                (VkPresentInfoKHR)
import           Graphics.Vulkan.Types.Struct.Rect                   (VkRect2D)
import           Graphics.Vulkan.Types.Struct.RenderPass             (VkRenderPassBeginInfo)
import           Graphics.Vulkan.Types.Struct.SubmitInfo             (VkSubmitInfo)
import           Graphics.Vulkan.Types.Struct.SwapchainC             (VkSwapchainCreateInfoKHR)

-- | > typedef struct VkDeviceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*     pNext;
--   >     VkDeviceCreateFlags    flags;
--   >     uint32_t        queueCreateInfoCount;
--   >     const VkDeviceQueueCreateInfo* pQueueCreateInfos;
--   >     uint32_t               enabledLayerCount;
--   >     const char* const*      ppEnabledLayerNames;
--   >     uint32_t               enabledExtensionCount;
--   >     const char* const*      ppEnabledExtensionNames;
--   >     const VkPhysicalDeviceFeatures* pEnabledFeatures;
--   > } VkDeviceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceCreateInfo VkDeviceCreateInfo registry at www.khronos.org>
type VkDeviceCreateInfo = VkStruct VkDeviceCreateInfo' -- ' closing tick for hsc2hs

data VkDeviceCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceCreateInfo where
    type StructRep VkDeviceCreateInfo =
         'StructMeta "VkDeviceCreateInfo" VkDeviceCreateInfo  -- ' closing tick for hsc2hs
                                                             #{size VkDeviceCreateInfo}
           #{alignment VkDeviceCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDeviceCreateFlags 'True 
                                                           #{offset VkDeviceCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueCreateInfoCount" Word32 'False 
                                                              #{offset VkDeviceCreateInfo, queueCreateInfoCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pQueueCreateInfos" (Ptr VkDeviceQueueCreateInfo) -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkDeviceCreateInfo, pQueueCreateInfos}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "enabledLayerCount" Word32 'True 
                                                          #{offset VkDeviceCreateInfo, enabledLayerCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ppEnabledLayerNames" (Ptr CString) 'False 
                                                                    #{offset VkDeviceCreateInfo, ppEnabledLayerNames}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "enabledExtensionCount" Word32 'True 
                                                              #{offset VkDeviceCreateInfo, enabledExtensionCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ppEnabledExtensionNames" (Ptr CString) 'False
                #{offset VkDeviceCreateInfo, ppEnabledExtensionNames}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pEnabledFeatures" (Ptr VkPhysicalDeviceFeatures) 'True
                #{offset VkDeviceCreateInfo, pEnabledFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDeviceEventInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceEventTypeEXT             deviceEvent;
--   > } VkDeviceEventInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceEventInfoEXT VkDeviceEventInfoEXT registry at www.khronos.org>
type VkDeviceEventInfoEXT = VkStruct VkDeviceEventInfoEXT' -- ' closing tick for hsc2hs

data VkDeviceEventInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceEventInfoEXT where
    type StructRep VkDeviceEventInfoEXT =
         'StructMeta "VkDeviceEventInfoEXT" VkDeviceEventInfoEXT  -- ' closing tick for hsc2hs
                                                                 #{size VkDeviceEventInfoEXT}
           #{alignment VkDeviceEventInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceEventInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceEventInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceEvent" VkDeviceEventTypeEXT 'False 
                                                                   #{offset VkDeviceEventInfoEXT, deviceEvent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDeviceGeneratedCommandsFeaturesNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         computeBindingPointSupport;
--   > } VkDeviceGeneratedCommandsFeaturesNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGeneratedCommandsFeaturesNVX VkDeviceGeneratedCommandsFeaturesNVX registry at www.khronos.org>
type VkDeviceGeneratedCommandsFeaturesNVX =
     VkStruct VkDeviceGeneratedCommandsFeaturesNVX' -- ' closing tick for hsc2hs

data VkDeviceGeneratedCommandsFeaturesNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceGeneratedCommandsFeaturesNVX where
    type StructRep VkDeviceGeneratedCommandsFeaturesNVX =
         'StructMeta "VkDeviceGeneratedCommandsFeaturesNVX" -- ' closing tick for hsc2hs
           VkDeviceGeneratedCommandsFeaturesNVX
           #{size VkDeviceGeneratedCommandsFeaturesNVX}
           #{alignment VkDeviceGeneratedCommandsFeaturesNVX}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "computeBindingPointSupport" VkBool32 'False 
                                                                      #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDeviceGeneratedCommandsLimitsNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         maxIndirectCommandsLayoutTokenCount;
--   >     uint32_t                         maxObjectEntryCounts;
--   >     uint32_t                         minSequenceCountBufferOffsetAlignment;
--   >     uint32_t                         minSequenceIndexBufferOffsetAlignment;
--   >     uint32_t                         minCommandsTokenBufferOffsetAlignment;
--   > } VkDeviceGeneratedCommandsLimitsNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGeneratedCommandsLimitsNVX VkDeviceGeneratedCommandsLimitsNVX registry at www.khronos.org>
type VkDeviceGeneratedCommandsLimitsNVX =
     VkStruct VkDeviceGeneratedCommandsLimitsNVX' -- ' closing tick for hsc2hs

data VkDeviceGeneratedCommandsLimitsNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceGeneratedCommandsLimitsNVX where
    type StructRep VkDeviceGeneratedCommandsLimitsNVX =
         'StructMeta "VkDeviceGeneratedCommandsLimitsNVX" -- ' closing tick for hsc2hs
           VkDeviceGeneratedCommandsLimitsNVX
           #{size VkDeviceGeneratedCommandsLimitsNVX}
           #{alignment VkDeviceGeneratedCommandsLimitsNVX}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxIndirectCommandsLayoutTokenCount" Word32 'False
                #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxObjectEntryCounts" Word32 'False 
                                                              #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minSequenceCountBufferOffsetAlignment" Word32 'False
                #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minSequenceIndexBufferOffsetAlignment" Word32 'False
                #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minCommandsTokenBufferOffsetAlignment" Word32 'False
                #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDeviceGroupBindSparseInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         resourceDeviceIndex;
--   >     uint32_t                         memoryDeviceIndex;
--   > } VkDeviceGroupBindSparseInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupBindSparseInfo VkDeviceGroupBindSparseInfo registry at www.khronos.org>
type VkDeviceGroupBindSparseInfo =
     VkStruct VkDeviceGroupBindSparseInfo' -- ' closing tick for hsc2hs

data VkDeviceGroupBindSparseInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceGroupBindSparseInfo where
    type StructRep VkDeviceGroupBindSparseInfo =
         'StructMeta "VkDeviceGroupBindSparseInfo" -- ' closing tick for hsc2hs
           VkDeviceGroupBindSparseInfo
           #{size VkDeviceGroupBindSparseInfo}
           #{alignment VkDeviceGroupBindSparseInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceGroupBindSparseInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceGroupBindSparseInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "resourceDeviceIndex" Word32 'False 
                                                             #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryDeviceIndex" Word32 'False 
                                                           #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkBindSparseInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkDeviceGroupBindSparseInfo`
type VkDeviceGroupBindSparseInfoKHR = VkDeviceGroupBindSparseInfo

-- | > typedef struct VkDeviceGroupCommandBufferBeginInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   > } VkDeviceGroupCommandBufferBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupCommandBufferBeginInfo VkDeviceGroupCommandBufferBeginInfo registry at www.khronos.org>
type VkDeviceGroupCommandBufferBeginInfo =
     VkStruct VkDeviceGroupCommandBufferBeginInfo' -- ' closing tick for hsc2hs

data VkDeviceGroupCommandBufferBeginInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceGroupCommandBufferBeginInfo where
    type StructRep VkDeviceGroupCommandBufferBeginInfo =
         'StructMeta "VkDeviceGroupCommandBufferBeginInfo" -- ' closing tick for hsc2hs
           VkDeviceGroupCommandBufferBeginInfo
           #{size VkDeviceGroupCommandBufferBeginInfo}
           #{alignment VkDeviceGroupCommandBufferBeginInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceGroupCommandBufferBeginInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceGroupCommandBufferBeginInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceMask" Word32 'False 
                                                    #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkCommandBufferBeginInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkDeviceGroupCommandBufferBeginInfo`
type VkDeviceGroupCommandBufferBeginInfoKHR =
     VkDeviceGroupCommandBufferBeginInfo

-- | > typedef struct VkDeviceGroupDeviceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     const VkPhysicalDevice*  pPhysicalDevices;
--   > } VkDeviceGroupDeviceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupDeviceCreateInfo VkDeviceGroupDeviceCreateInfo registry at www.khronos.org>
type VkDeviceGroupDeviceCreateInfo =
     VkStruct VkDeviceGroupDeviceCreateInfo' -- ' closing tick for hsc2hs

data VkDeviceGroupDeviceCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceGroupDeviceCreateInfo where
    type StructRep VkDeviceGroupDeviceCreateInfo =
         'StructMeta "VkDeviceGroupDeviceCreateInfo" -- ' closing tick for hsc2hs
           VkDeviceGroupDeviceCreateInfo
           #{size VkDeviceGroupDeviceCreateInfo}
           #{alignment VkDeviceGroupDeviceCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceGroupDeviceCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceGroupDeviceCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "physicalDeviceCount" Word32 'True 
                                                            #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pPhysicalDevices" (Ptr VkPhysicalDevice) 'False
                #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkDeviceGroupDeviceCreateInfo`
type VkDeviceGroupDeviceCreateInfoKHR =
     VkDeviceGroupDeviceCreateInfo

-- | > typedef struct VkDeviceGroupPresentCapabilitiesKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         presentMask[VK_MAX_DEVICE_GROUP_SIZE];
--   >     VkDeviceGroupPresentModeFlagsKHR modes;
--   > } VkDeviceGroupPresentCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupPresentCapabilitiesKHR VkDeviceGroupPresentCapabilitiesKHR registry at www.khronos.org>
type VkDeviceGroupPresentCapabilitiesKHR =
     VkStruct VkDeviceGroupPresentCapabilitiesKHR' -- ' closing tick for hsc2hs

data VkDeviceGroupPresentCapabilitiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceGroupPresentCapabilitiesKHR where
    type StructRep VkDeviceGroupPresentCapabilitiesKHR =
         'StructMeta "VkDeviceGroupPresentCapabilitiesKHR" -- ' closing tick for hsc2hs
           VkDeviceGroupPresentCapabilitiesKHR
           #{size VkDeviceGroupPresentCapabilitiesKHR}
           #{alignment VkDeviceGroupPresentCapabilitiesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceGroupPresentCapabilitiesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "presentMask" Word32 'False 
                                                     #{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}
                VK_MAX_DEVICE_GROUP_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "modes" VkDeviceGroupPresentModeFlagsKHR 'False
                #{offset VkDeviceGroupPresentCapabilitiesKHR, modes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDeviceGroupPresentInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         swapchainCount;
--   >     const uint32_t* pDeviceMasks;
--   >     VkDeviceGroupPresentModeFlagBitsKHR mode;
--   > } VkDeviceGroupPresentInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupPresentInfoKHR VkDeviceGroupPresentInfoKHR registry at www.khronos.org>
type VkDeviceGroupPresentInfoKHR =
     VkStruct VkDeviceGroupPresentInfoKHR' -- ' closing tick for hsc2hs

data VkDeviceGroupPresentInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceGroupPresentInfoKHR where
    type StructRep VkDeviceGroupPresentInfoKHR =
         'StructMeta "VkDeviceGroupPresentInfoKHR" -- ' closing tick for hsc2hs
           VkDeviceGroupPresentInfoKHR
           #{size VkDeviceGroupPresentInfoKHR}
           #{alignment VkDeviceGroupPresentInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceGroupPresentInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceGroupPresentInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "swapchainCount" Word32 'True 
                                                       #{offset VkDeviceGroupPresentInfoKHR, swapchainCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDeviceMasks" (Ptr Word32) 'False 
                                                            #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mode" VkDeviceGroupPresentModeFlagBitsKHR 'False
                #{offset VkDeviceGroupPresentInfoKHR, mode}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

-- | > typedef struct VkDeviceGroupRenderPassBeginInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   >     uint32_t         deviceRenderAreaCount;
--   >     const VkRect2D*  pDeviceRenderAreas;
--   > } VkDeviceGroupRenderPassBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupRenderPassBeginInfo VkDeviceGroupRenderPassBeginInfo registry at www.khronos.org>
type VkDeviceGroupRenderPassBeginInfo =
     VkStruct VkDeviceGroupRenderPassBeginInfo' -- ' closing tick for hsc2hs

data VkDeviceGroupRenderPassBeginInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceGroupRenderPassBeginInfo where
    type StructRep VkDeviceGroupRenderPassBeginInfo =
         'StructMeta "VkDeviceGroupRenderPassBeginInfo" -- ' closing tick for hsc2hs
           VkDeviceGroupRenderPassBeginInfo
           #{size VkDeviceGroupRenderPassBeginInfo}
           #{alignment VkDeviceGroupRenderPassBeginInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceGroupRenderPassBeginInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceGroupRenderPassBeginInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceMask" Word32 'False 
                                                    #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceRenderAreaCount" Word32 'True 
                                                              #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDeviceRenderAreas" (Ptr VkRect2D) 'False 
                                                                    #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkDeviceGroupRenderPassBeginInfo`
type VkDeviceGroupRenderPassBeginInfoKHR =
     VkDeviceGroupRenderPassBeginInfo

-- | > typedef struct VkDeviceGroupSubmitInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         waitSemaphoreCount;
--   >     const uint32_t*    pWaitSemaphoreDeviceIndices;
--   >     uint32_t         commandBufferCount;
--   >     const uint32_t*    pCommandBufferDeviceMasks;
--   >     uint32_t         signalSemaphoreCount;
--   >     const uint32_t*  pSignalSemaphoreDeviceIndices;
--   > } VkDeviceGroupSubmitInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupSubmitInfo VkDeviceGroupSubmitInfo registry at www.khronos.org>
type VkDeviceGroupSubmitInfo = VkStruct VkDeviceGroupSubmitInfo' -- ' closing tick for hsc2hs

data VkDeviceGroupSubmitInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceGroupSubmitInfo where
    type StructRep VkDeviceGroupSubmitInfo =
         'StructMeta "VkDeviceGroupSubmitInfo" VkDeviceGroupSubmitInfo -- ' closing tick for hsc2hs
           #{size VkDeviceGroupSubmitInfo}
           #{alignment VkDeviceGroupSubmitInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceGroupSubmitInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceGroupSubmitInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "waitSemaphoreCount" Word32 'True 
                                                           #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pWaitSemaphoreDeviceIndices" (Ptr Word32) 'False
                #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "commandBufferCount" Word32 'True 
                                                           #{offset VkDeviceGroupSubmitInfo, commandBufferCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pCommandBufferDeviceMasks" (Ptr Word32) 'False
                #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "signalSemaphoreCount" Word32 'True 
                                                             #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSignalSemaphoreDeviceIndices" (Ptr Word32) 'False
                #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSubmitInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkDeviceGroupSubmitInfo`
type VkDeviceGroupSubmitInfoKHR = VkDeviceGroupSubmitInfo

-- | > typedef struct VkDeviceGroupSwapchainCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceGroupPresentModeFlagsKHR                         modes;
--   > } VkDeviceGroupSwapchainCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupSwapchainCreateInfoKHR VkDeviceGroupSwapchainCreateInfoKHR registry at www.khronos.org>
type VkDeviceGroupSwapchainCreateInfoKHR =
     VkStruct VkDeviceGroupSwapchainCreateInfoKHR' -- ' closing tick for hsc2hs

data VkDeviceGroupSwapchainCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceGroupSwapchainCreateInfoKHR where
    type StructRep VkDeviceGroupSwapchainCreateInfoKHR =
         'StructMeta "VkDeviceGroupSwapchainCreateInfoKHR" -- ' closing tick for hsc2hs
           VkDeviceGroupSwapchainCreateInfoKHR
           #{size VkDeviceGroupSwapchainCreateInfoKHR}
           #{alignment VkDeviceGroupSwapchainCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "modes" VkDeviceGroupPresentModeFlagsKHR 'False
                #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSwapchainCreateInfoKHR] -- ' closing tick for hsc2hs

-- | > typedef struct VkDeviceQueueCreateInfo {
--   >     VkStructureType sType;
--   >     const void*     pNext;
--   >     VkDeviceQueueCreateFlags    flags;
--   >     uint32_t        queueFamilyIndex;
--   >     uint32_t        queueCount;
--   >     const float*    pQueuePriorities;
--   > } VkDeviceQueueCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceQueueCreateInfo VkDeviceQueueCreateInfo registry at www.khronos.org>
type VkDeviceQueueCreateInfo = VkStruct VkDeviceQueueCreateInfo' -- ' closing tick for hsc2hs

data VkDeviceQueueCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceQueueCreateInfo where
    type StructRep VkDeviceQueueCreateInfo =
         'StructMeta "VkDeviceQueueCreateInfo" VkDeviceQueueCreateInfo -- ' closing tick for hsc2hs
           #{size VkDeviceQueueCreateInfo}
           #{alignment VkDeviceQueueCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceQueueCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceQueueCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDeviceQueueCreateFlags 'True 
                                                                #{offset VkDeviceQueueCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueFamilyIndex" Word32 'False 
                                                          #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueCount" Word32 'False 
                                                    #{offset VkDeviceQueueCreateInfo, queueCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pQueuePriorities" (Ptr  -- ' closing tick for hsc2hs
                                                 #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkDeviceQueueCreateInfo, pQueuePriorities}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDeviceQueueGlobalPriorityCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                    pNext;
--   >     VkQueueGlobalPriorityEXT       globalPriority;
--   > } VkDeviceQueueGlobalPriorityCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceQueueGlobalPriorityCreateInfoEXT VkDeviceQueueGlobalPriorityCreateInfoEXT registry at www.khronos.org>
type VkDeviceQueueGlobalPriorityCreateInfoEXT =
     VkStruct VkDeviceQueueGlobalPriorityCreateInfoEXT' -- ' closing tick for hsc2hs

data VkDeviceQueueGlobalPriorityCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
    type StructRep VkDeviceQueueGlobalPriorityCreateInfoEXT =
         'StructMeta "VkDeviceQueueGlobalPriorityCreateInfoEXT" -- ' closing tick for hsc2hs
           VkDeviceQueueGlobalPriorityCreateInfoEXT
           #{size VkDeviceQueueGlobalPriorityCreateInfoEXT}
           #{alignment VkDeviceQueueGlobalPriorityCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "globalPriority" VkQueueGlobalPriorityEXT 'False
                #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkDeviceQueueCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkDeviceQueueInfo2 {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     VkDeviceQueueCreateFlags            flags;
--   >     uint32_t                            queueFamilyIndex;
--   >     uint32_t                            queueIndex;
--   > } VkDeviceQueueInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceQueueInfo2 VkDeviceQueueInfo2 registry at www.khronos.org>
type VkDeviceQueueInfo2 = VkStruct VkDeviceQueueInfo2' -- ' closing tick for hsc2hs

data VkDeviceQueueInfo2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceQueueInfo2 where
    type StructRep VkDeviceQueueInfo2 =
         'StructMeta "VkDeviceQueueInfo2" VkDeviceQueueInfo2  -- ' closing tick for hsc2hs
                                                             #{size VkDeviceQueueInfo2}
           #{alignment VkDeviceQueueInfo2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeviceQueueInfo2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDeviceQueueInfo2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDeviceQueueCreateFlags 'False 
                                                                 #{offset VkDeviceQueueInfo2, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueFamilyIndex" Word32 'False 
                                                          #{offset VkDeviceQueueInfo2, queueFamilyIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueIndex" Word32 'False 
                                                    #{offset VkDeviceQueueInfo2, queueIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
