#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.AllocationCallbacks
       (VkAllocationCallbacks) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Funcpointers (PFN_vkAllocationFunction,
                                                     PFN_vkFreeFunction,
                                                     PFN_vkInternalAllocationNotification,
                                                     PFN_vkInternalFreeNotification,
                                                     PFN_vkReallocationFunction)

-- | > typedef struct VkAllocationCallbacks {
--   >     void*           pUserData;
--   >     PFN_vkAllocationFunction   pfnAllocation;
--   >     PFN_vkReallocationFunction pfnReallocation;
--   >     PFN_vkFreeFunction    pfnFree;
--   >     PFN_vkInternalAllocationNotification pfnInternalAllocation;
--   >     PFN_vkInternalFreeNotification pfnInternalFree;
--   > } VkAllocationCallbacks;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAllocationCallbacks VkAllocationCallbacks registry at www.khronos.org>
type VkAllocationCallbacks = VkStruct VkAllocationCallbacks' -- ' closing tick for hsc2hs

data VkAllocationCallbacks' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAllocationCallbacks where
    type StructRep VkAllocationCallbacks =
         'StructMeta "VkAllocationCallbacks" VkAllocationCallbacks  -- ' closing tick for hsc2hs
                                                                   #{size VkAllocationCallbacks}
           #{alignment VkAllocationCallbacks}
           '[('FieldMeta "pUserData" (Ptr Void) 'True  -- ' closing tick for hsc2hs
                                                      #{offset VkAllocationCallbacks, pUserData}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pfnAllocation" PFN_vkAllocationFunction 'False
                #{offset VkAllocationCallbacks, pfnAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pfnReallocation" PFN_vkReallocationFunction 'False
                #{offset VkAllocationCallbacks, pfnReallocation}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pfnFree" PFN_vkFreeFunction 'False 
                                                             #{offset VkAllocationCallbacks, pfnFree}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pfnInternalAllocation" -- ' closing tick for hsc2hs
                PFN_vkInternalAllocationNotification
                'True -- ' closing tick for hsc2hs
                #{offset VkAllocationCallbacks, pfnInternalAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pfnInternalFree" PFN_vkInternalFreeNotification 'True
                #{offset VkAllocationCallbacks, pfnInternalFree}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
