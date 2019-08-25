#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Semaphore
       (VkSemaphoreCreateInfo, VkSemaphoreGetFdInfoKHR) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkSemaphoreCreateFlags)
import           Graphics.Vulkan.Types.Enum.External      (VkExternalSemaphoreHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkSemaphore)

-- | > typedef struct VkSemaphoreCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSemaphoreCreateFlags flags;
--   > } VkSemaphoreCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSemaphoreCreateInfo VkSemaphoreCreateInfo registry at www.khronos.org>
type VkSemaphoreCreateInfo = VkStruct VkSemaphoreCreateInfo' -- ' closing tick for hsc2hs

data VkSemaphoreCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSemaphoreCreateInfo where
    type StructRep VkSemaphoreCreateInfo =
         'StructMeta "VkSemaphoreCreateInfo" VkSemaphoreCreateInfo  -- ' closing tick for hsc2hs
                                                                   #{size VkSemaphoreCreateInfo}
           #{alignment VkSemaphoreCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSemaphoreCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSemaphoreCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkSemaphoreCreateFlags 'True 
                                                              #{offset VkSemaphoreCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSemaphoreGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore                      semaphore;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   > } VkSemaphoreGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSemaphoreGetFdInfoKHR VkSemaphoreGetFdInfoKHR registry at www.khronos.org>
type VkSemaphoreGetFdInfoKHR = VkStruct VkSemaphoreGetFdInfoKHR' -- ' closing tick for hsc2hs

data VkSemaphoreGetFdInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSemaphoreGetFdInfoKHR where
    type StructRep VkSemaphoreGetFdInfoKHR =
         'StructMeta "VkSemaphoreGetFdInfoKHR" VkSemaphoreGetFdInfoKHR -- ' closing tick for hsc2hs
           #{size VkSemaphoreGetFdInfoKHR}
           #{alignment VkSemaphoreGetFdInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSemaphoreGetFdInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSemaphoreGetFdInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "semaphore" VkSemaphore 'False 
                                                        #{offset VkSemaphoreGetFdInfoKHR, semaphore}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalSemaphoreHandleTypeFlagBits -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkSemaphoreGetFdInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
