#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Bind
       (VkBindBufferMemoryDeviceGroupInfo,
        VkBindBufferMemoryDeviceGroupInfoKHR, VkBindBufferMemoryInfo,
        VkBindBufferMemoryInfoKHR, VkBindImageMemoryDeviceGroupInfo,
        VkBindImageMemoryDeviceGroupInfoKHR, VkBindImageMemoryInfo,
        VkBindImageMemoryInfoKHR, VkBindImageMemorySwapchainInfoKHR,
        VkBindImagePlaneMemoryInfo, VkBindImagePlaneMemoryInfoKHR,
        VkBindSparseInfo)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.Image         (VkImageAspectFlagBits)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer,
                                                           VkDeviceMemory,
                                                           VkImage, VkSemaphore,
                                                           VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.Rect        (VkRect2D)
import           Graphics.Vulkan.Types.Struct.Sparse      (VkSparseBufferMemoryBindInfo,
                                                           VkSparseImageMemoryBindInfo,
                                                           VkSparseImageOpaqueMemoryBindInfo)

-- | > typedef struct VkBindBufferMemoryDeviceGroupInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   > } VkBindBufferMemoryDeviceGroupInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindBufferMemoryDeviceGroupInfo VkBindBufferMemoryDeviceGroupInfo registry at www.khronos.org>
type VkBindBufferMemoryDeviceGroupInfo =
     VkStruct VkBindBufferMemoryDeviceGroupInfo' -- ' closing tick for hsc2hs

data VkBindBufferMemoryDeviceGroupInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBindBufferMemoryDeviceGroupInfo where
    type StructRep VkBindBufferMemoryDeviceGroupInfo =
         'StructMeta "VkBindBufferMemoryDeviceGroupInfo" -- ' closing tick for hsc2hs
           VkBindBufferMemoryDeviceGroupInfo
           #{size VkBindBufferMemoryDeviceGroupInfo}
           #{alignment VkBindBufferMemoryDeviceGroupInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBindBufferMemoryDeviceGroupInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBindBufferMemoryDeviceGroupInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceIndexCount" Word32 'True 
                                                         #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDeviceIndices" (Ptr Word32) 'False 
                                                              #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkBindBufferMemoryInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkBindBufferMemoryDeviceGroupInfo`
type VkBindBufferMemoryDeviceGroupInfoKHR =
     VkBindBufferMemoryDeviceGroupInfo

-- | > typedef struct VkBindBufferMemoryInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBuffer                         buffer;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindBufferMemoryInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindBufferMemoryInfo VkBindBufferMemoryInfo registry at www.khronos.org>
type VkBindBufferMemoryInfo = VkStruct VkBindBufferMemoryInfo' -- ' closing tick for hsc2hs

data VkBindBufferMemoryInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBindBufferMemoryInfo where
    type StructRep VkBindBufferMemoryInfo =
         'StructMeta "VkBindBufferMemoryInfo" VkBindBufferMemoryInfo -- ' closing tick for hsc2hs
           #{size VkBindBufferMemoryInfo}
           #{alignment VkBindBufferMemoryInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBindBufferMemoryInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBindBufferMemoryInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" VkBuffer 'False 
                                                  #{offset VkBindBufferMemoryInfo, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memory" VkDeviceMemory 'False 
                                                        #{offset VkBindBufferMemoryInfo, memory}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryOffset" VkDeviceSize 'False 
                                                            #{offset VkBindBufferMemoryInfo, memoryOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkBindBufferMemoryInfo`
type VkBindBufferMemoryInfoKHR = VkBindBufferMemoryInfo

-- | > typedef struct VkBindImageMemoryDeviceGroupInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   >     uint32_t         splitInstanceBindRegionCount;
--   >     const VkRect2D*  pSplitInstanceBindRegions;
--   > } VkBindImageMemoryDeviceGroupInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindImageMemoryDeviceGroupInfo VkBindImageMemoryDeviceGroupInfo registry at www.khronos.org>
type VkBindImageMemoryDeviceGroupInfo =
     VkStruct VkBindImageMemoryDeviceGroupInfo' -- ' closing tick for hsc2hs

data VkBindImageMemoryDeviceGroupInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBindImageMemoryDeviceGroupInfo where
    type StructRep VkBindImageMemoryDeviceGroupInfo =
         'StructMeta "VkBindImageMemoryDeviceGroupInfo" -- ' closing tick for hsc2hs
           VkBindImageMemoryDeviceGroupInfo
           #{size VkBindImageMemoryDeviceGroupInfo}
           #{alignment VkBindImageMemoryDeviceGroupInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBindImageMemoryDeviceGroupInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBindImageMemoryDeviceGroupInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceIndexCount" Word32 'True 
                                                         #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDeviceIndices" (Ptr Word32) 'False 
                                                              #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "splitInstanceBindRegionCount" Word32 'True 
                                                                     #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSplitInstanceBindRegions" (Ptr VkRect2D) 'False
                #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkBindImageMemoryInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkBindImageMemoryDeviceGroupInfo`
type VkBindImageMemoryDeviceGroupInfoKHR =
     VkBindImageMemoryDeviceGroupInfo

-- | > typedef struct VkBindImageMemoryInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage                          image;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindImageMemoryInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindImageMemoryInfo VkBindImageMemoryInfo registry at www.khronos.org>
type VkBindImageMemoryInfo = VkStruct VkBindImageMemoryInfo' -- ' closing tick for hsc2hs

data VkBindImageMemoryInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBindImageMemoryInfo where
    type StructRep VkBindImageMemoryInfo =
         'StructMeta "VkBindImageMemoryInfo" VkBindImageMemoryInfo  -- ' closing tick for hsc2hs
                                                                   #{size VkBindImageMemoryInfo}
           #{alignment VkBindImageMemoryInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBindImageMemoryInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBindImageMemoryInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "image" VkImage 'False 
                                                #{offset VkBindImageMemoryInfo, image}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memory" VkDeviceMemory 'False 
                                                        #{offset VkBindImageMemoryInfo, memory}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryOffset" VkDeviceSize 'False 
                                                            #{offset VkBindImageMemoryInfo, memoryOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkBindImageMemoryInfo`
type VkBindImageMemoryInfoKHR = VkBindImageMemoryInfo

-- | > typedef struct VkBindImageMemorySwapchainInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR swapchain;
--   >     uint32_t                         imageIndex;
--   > } VkBindImageMemorySwapchainInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindImageMemorySwapchainInfoKHR VkBindImageMemorySwapchainInfoKHR registry at www.khronos.org>
type VkBindImageMemorySwapchainInfoKHR =
     VkStruct VkBindImageMemorySwapchainInfoKHR' -- ' closing tick for hsc2hs

data VkBindImageMemorySwapchainInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBindImageMemorySwapchainInfoKHR where
    type StructRep VkBindImageMemorySwapchainInfoKHR =
         'StructMeta "VkBindImageMemorySwapchainInfoKHR" -- ' closing tick for hsc2hs
           VkBindImageMemorySwapchainInfoKHR
           #{size VkBindImageMemorySwapchainInfoKHR}
           #{alignment VkBindImageMemorySwapchainInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBindImageMemorySwapchainInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBindImageMemorySwapchainInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "swapchain" VkSwapchainKHR 'False 
                                                           #{offset VkBindImageMemorySwapchainInfoKHR, swapchain}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageIndex" Word32 'False 
                                                    #{offset VkBindImageMemorySwapchainInfoKHR, imageIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkBindImageMemoryInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkBindImagePlaneMemoryInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImageAspectFlagBits            planeAspect;
--   > } VkBindImagePlaneMemoryInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindImagePlaneMemoryInfo VkBindImagePlaneMemoryInfo registry at www.khronos.org>
type VkBindImagePlaneMemoryInfo =
     VkStruct VkBindImagePlaneMemoryInfo' -- ' closing tick for hsc2hs

data VkBindImagePlaneMemoryInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBindImagePlaneMemoryInfo where
    type StructRep VkBindImagePlaneMemoryInfo =
         'StructMeta "VkBindImagePlaneMemoryInfo" VkBindImagePlaneMemoryInfo -- ' closing tick for hsc2hs
           #{size VkBindImagePlaneMemoryInfo}
           #{alignment VkBindImagePlaneMemoryInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBindImagePlaneMemoryInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBindImagePlaneMemoryInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "planeAspect" VkImageAspectFlagBits 'False 
                                                                    #{offset VkBindImagePlaneMemoryInfo, planeAspect}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkBindImageMemoryInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkBindImagePlaneMemoryInfo`
type VkBindImagePlaneMemoryInfoKHR = VkBindImagePlaneMemoryInfo

-- | > typedef struct VkBindSparseInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               waitSemaphoreCount;
--   >     const VkSemaphore*     pWaitSemaphores;
--   >     uint32_t               bufferBindCount;
--   >     const VkSparseBufferMemoryBindInfo* pBufferBinds;
--   >     uint32_t               imageOpaqueBindCount;
--   >     const VkSparseImageOpaqueMemoryBindInfo* pImageOpaqueBinds;
--   >     uint32_t               imageBindCount;
--   >     const VkSparseImageMemoryBindInfo* pImageBinds;
--   >     uint32_t               signalSemaphoreCount;
--   >     const VkSemaphore*     pSignalSemaphores;
--   > } VkBindSparseInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindSparseInfo VkBindSparseInfo registry at www.khronos.org>
type VkBindSparseInfo = VkStruct VkBindSparseInfo' -- ' closing tick for hsc2hs

data VkBindSparseInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBindSparseInfo where
    type StructRep VkBindSparseInfo =
         'StructMeta "VkBindSparseInfo" VkBindSparseInfo  -- ' closing tick for hsc2hs
                                                         #{size VkBindSparseInfo}
           #{alignment VkBindSparseInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBindSparseInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBindSparseInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "waitSemaphoreCount" Word32 'True 
                                                           #{offset VkBindSparseInfo, waitSemaphoreCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pWaitSemaphores" (Ptr VkSemaphore) 'False 
                                                                    #{offset VkBindSparseInfo, pWaitSemaphores}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferBindCount" Word32 'True 
                                                        #{offset VkBindSparseInfo, bufferBindCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pBufferBinds" (Ptr VkSparseBufferMemoryBindInfo) -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkBindSparseInfo, pBufferBinds}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageOpaqueBindCount" Word32 'True 
                                                             #{offset VkBindSparseInfo, imageOpaqueBindCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pImageOpaqueBinds" -- ' closing tick for hsc2hs
                (Ptr VkSparseImageOpaqueMemoryBindInfo)
                'False -- ' closing tick for hsc2hs
                #{offset VkBindSparseInfo, pImageOpaqueBinds}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageBindCount" Word32 'True 
                                                       #{offset VkBindSparseInfo, imageBindCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pImageBinds" (Ptr VkSparseImageMemoryBindInfo) 'False
                #{offset VkBindSparseInfo, pImageBinds}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "signalSemaphoreCount" Word32 'True 
                                                             #{offset VkBindSparseInfo, signalSemaphoreCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSignalSemaphores" (Ptr VkSemaphore) 'False 
                                                                      #{offset VkBindSparseInfo, pSignalSemaphores}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
