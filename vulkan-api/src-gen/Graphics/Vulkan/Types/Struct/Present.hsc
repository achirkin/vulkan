#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Present
       (VkPresentInfoKHR, VkPresentRegionKHR, VkPresentRegionsKHR,
        VkPresentTimeGOOGLE, VkPresentTimesInfoGOOGLE)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Result        (VkResult)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkSemaphore,
                                                           VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.Rect        (VkRectLayerKHR)

-- | > typedef struct VkPresentInfoKHR {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     uint32_t         waitSemaphoreCount;
--   >     const VkSemaphore* pWaitSemaphores;
--   >     uint32_t                         swapchainCount;
--   >     const VkSwapchainKHR* pSwapchains;
--   >     const uint32_t* pImageIndices;
--   >     VkResult* pResults;
--   > } VkPresentInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentInfoKHR VkPresentInfoKHR registry at www.khronos.org>
type VkPresentInfoKHR = VkStruct VkPresentInfoKHR' -- ' closing tick for hsc2hs

data VkPresentInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPresentInfoKHR where
    type StructRep VkPresentInfoKHR =
         'StructMeta "VkPresentInfoKHR" VkPresentInfoKHR  -- ' closing tick for hsc2hs
                                                         #{size VkPresentInfoKHR}
           #{alignment VkPresentInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPresentInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPresentInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "waitSemaphoreCount" Word32 'True 
                                                           #{offset VkPresentInfoKHR, waitSemaphoreCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pWaitSemaphores" (Ptr VkSemaphore) 'False 
                                                                    #{offset VkPresentInfoKHR, pWaitSemaphores}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "swapchainCount" Word32 'False 
                                                        #{offset VkPresentInfoKHR, swapchainCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSwapchains" (Ptr VkSwapchainKHR) 'False 
                                                                   #{offset VkPresentInfoKHR, pSwapchains}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pImageIndices" (Ptr Word32) 'False 
                                                             #{offset VkPresentInfoKHR, pImageIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pResults" (Ptr VkResult) 'True 
                                                         #{offset VkPresentInfoKHR, pResults}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPresentRegionKHR {
--   >     uint32_t         rectangleCount;
--   >     const VkRectLayerKHR*   pRectangles;
--   > } VkPresentRegionKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentRegionKHR VkPresentRegionKHR registry at www.khronos.org>
type VkPresentRegionKHR = VkStruct VkPresentRegionKHR' -- ' closing tick for hsc2hs

data VkPresentRegionKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPresentRegionKHR where
    type StructRep VkPresentRegionKHR =
         'StructMeta "VkPresentRegionKHR" VkPresentRegionKHR  -- ' closing tick for hsc2hs
                                                             #{size VkPresentRegionKHR}
           #{alignment VkPresentRegionKHR}
           '[('FieldMeta "rectangleCount" Word32 'True  -- ' closing tick for hsc2hs
                                                       #{offset VkPresentRegionKHR, rectangleCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pRectangles" (Ptr VkRectLayerKHR) 'True 
                                                                  #{offset VkPresentRegionKHR, pRectangles}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPresentRegionsKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         swapchainCount;
--   >     const VkPresentRegionKHR*   pRegions;
--   > } VkPresentRegionsKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentRegionsKHR VkPresentRegionsKHR registry at www.khronos.org>
type VkPresentRegionsKHR = VkStruct VkPresentRegionsKHR' -- ' closing tick for hsc2hs

data VkPresentRegionsKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPresentRegionsKHR where
    type StructRep VkPresentRegionsKHR =
         'StructMeta "VkPresentRegionsKHR" VkPresentRegionsKHR  -- ' closing tick for hsc2hs
                                                               #{size VkPresentRegionsKHR}
           #{alignment VkPresentRegionsKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPresentRegionsKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPresentRegionsKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "swapchainCount" Word32 'False 
                                                        #{offset VkPresentRegionsKHR, swapchainCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pRegions" (Ptr VkPresentRegionKHR) 'True 
                                                                   #{offset VkPresentRegionsKHR, pRegions}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

-- | > typedef struct VkPresentTimeGOOGLE {
--   >     uint32_t                         presentID;
--   >     uint64_t                         desiredPresentTime;
--   > } VkPresentTimeGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentTimeGOOGLE VkPresentTimeGOOGLE registry at www.khronos.org>
type VkPresentTimeGOOGLE = VkStruct VkPresentTimeGOOGLE' -- ' closing tick for hsc2hs

data VkPresentTimeGOOGLE' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPresentTimeGOOGLE where
    type StructRep VkPresentTimeGOOGLE =
         'StructMeta "VkPresentTimeGOOGLE" VkPresentTimeGOOGLE  -- ' closing tick for hsc2hs
                                                               #{size VkPresentTimeGOOGLE}
           #{alignment VkPresentTimeGOOGLE}
           '[('FieldMeta "presentID" Word32 'False  -- ' closing tick for hsc2hs
                                                   #{offset VkPresentTimeGOOGLE, presentID}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "desiredPresentTime" Word64 'False 
                                                            #{offset VkPresentTimeGOOGLE, desiredPresentTime}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPresentTimesInfoGOOGLE {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         swapchainCount;
--   >     const VkPresentTimeGOOGLE*   pTimes;
--   > } VkPresentTimesInfoGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentTimesInfoGOOGLE VkPresentTimesInfoGOOGLE registry at www.khronos.org>
type VkPresentTimesInfoGOOGLE = VkStruct VkPresentTimesInfoGOOGLE' -- ' closing tick for hsc2hs

data VkPresentTimesInfoGOOGLE' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPresentTimesInfoGOOGLE where
    type StructRep VkPresentTimesInfoGOOGLE =
         'StructMeta "VkPresentTimesInfoGOOGLE" VkPresentTimesInfoGOOGLE -- ' closing tick for hsc2hs
           #{size VkPresentTimesInfoGOOGLE}
           #{alignment VkPresentTimesInfoGOOGLE}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPresentTimesInfoGOOGLE, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPresentTimesInfoGOOGLE, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "swapchainCount" Word32 'False 
                                                        #{offset VkPresentTimesInfoGOOGLE, swapchainCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pTimes" (Ptr VkPresentTimeGOOGLE) 'True 
                                                                  #{offset VkPresentTimesInfoGOOGLE, pTimes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPresentInfoKHR] -- ' closing tick for hsc2hs
