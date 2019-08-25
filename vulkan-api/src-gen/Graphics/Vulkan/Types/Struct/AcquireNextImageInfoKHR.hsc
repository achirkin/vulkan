#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.AcquireNextImageInfoKHR
       (VkAcquireNextImageInfoKHR) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkFence, VkSemaphore,
                                                           VkSwapchainKHR)

-- | > typedef struct VkAcquireNextImageInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR swapchain;
--   >     uint64_t                         timeout;
--   >     VkSemaphore semaphore;
--   >     VkFence fence;
--   >     uint32_t                         deviceMask;
--   > } VkAcquireNextImageInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAcquireNextImageInfoKHR VkAcquireNextImageInfoKHR registry at www.khronos.org>
type VkAcquireNextImageInfoKHR =
     VkStruct VkAcquireNextImageInfoKHR' -- ' closing tick for hsc2hs

data VkAcquireNextImageInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAcquireNextImageInfoKHR where
    type StructRep VkAcquireNextImageInfoKHR =
         'StructMeta "VkAcquireNextImageInfoKHR" VkAcquireNextImageInfoKHR -- ' closing tick for hsc2hs
           #{size VkAcquireNextImageInfoKHR}
           #{alignment VkAcquireNextImageInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAcquireNextImageInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkAcquireNextImageInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "swapchain" VkSwapchainKHR 'False 
                                                           #{offset VkAcquireNextImageInfoKHR, swapchain}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "timeout" Word64 'False 
                                                 #{offset VkAcquireNextImageInfoKHR, timeout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "semaphore" VkSemaphore 'True 
                                                       #{offset VkAcquireNextImageInfoKHR, semaphore}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fence" VkFence 'True 
                                               #{offset VkAcquireNextImageInfoKHR, fence}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceMask" Word32 'False 
                                                    #{offset VkAcquireNextImageInfoKHR, deviceMask}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
