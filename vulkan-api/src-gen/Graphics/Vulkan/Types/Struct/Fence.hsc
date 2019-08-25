#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Fence
       (VkFenceCreateInfo, VkFenceGetFdInfoKHR) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.External      (VkExternalFenceHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.Fence         (VkFenceCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkFence)

-- | > typedef struct VkFenceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkFenceCreateFlags     flags;
--   > } VkFenceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFenceCreateInfo VkFenceCreateInfo registry at www.khronos.org>
type VkFenceCreateInfo = VkStruct VkFenceCreateInfo' -- ' closing tick for hsc2hs

data VkFenceCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkFenceCreateInfo where
    type StructRep VkFenceCreateInfo =
         'StructMeta "VkFenceCreateInfo" VkFenceCreateInfo  -- ' closing tick for hsc2hs
                                                           #{size VkFenceCreateInfo}
           #{alignment VkFenceCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkFenceCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkFenceCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkFenceCreateFlags 'True 
                                                          #{offset VkFenceCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkFenceGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence                                fence;
--   >     VkExternalFenceHandleTypeFlagBits   handleType;
--   > } VkFenceGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFenceGetFdInfoKHR VkFenceGetFdInfoKHR registry at www.khronos.org>
type VkFenceGetFdInfoKHR = VkStruct VkFenceGetFdInfoKHR' -- ' closing tick for hsc2hs

data VkFenceGetFdInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkFenceGetFdInfoKHR where
    type StructRep VkFenceGetFdInfoKHR =
         'StructMeta "VkFenceGetFdInfoKHR" VkFenceGetFdInfoKHR  -- ' closing tick for hsc2hs
                                                               #{size VkFenceGetFdInfoKHR}
           #{alignment VkFenceGetFdInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkFenceGetFdInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkFenceGetFdInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fence" VkFence 'False 
                                                #{offset VkFenceGetFdInfoKHR, fence}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalFenceHandleTypeFlagBits 'False
                #{offset VkFenceGetFdInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
