#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Semaphore
       (VkSemaphoreCreateInfo, VkSemaphoreGetFdInfoKHR,
        VkSemaphoreSignalInfo, VkSemaphoreSignalInfoKHR,
        VkSemaphoreTypeCreateInfo, VkSemaphoreTypeCreateInfoKHR,
        VkSemaphoreWaitInfo, VkSemaphoreWaitInfoKHR)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Bitmasks              (VkSemaphoreCreateFlags)
import Graphics.Vulkan.Types.Enum.External         (VkExternalSemaphoreHandleTypeFlagBits)
import Graphics.Vulkan.Types.Enum.Semaphore        (VkSemaphoreType,
                                                    VkSemaphoreWaitFlags)
import Graphics.Vulkan.Types.Enum.StructureType    (VkStructureType)
import Graphics.Vulkan.Types.Handles               (VkSemaphore)
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceExternalSemaphoreInfo)

-- | > typedef struct VkSemaphoreCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSemaphoreCreateFlags flags;
--   > } VkSemaphoreCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSemaphoreCreateInfo VkSemaphoreCreateInfo registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSemaphoreGetFdInfoKHR VkSemaphoreGetFdInfoKHR registry at www.khronos.org>
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

-- | > typedef struct VkSemaphoreSignalInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSemaphore            semaphore;
--   >     uint64_t               value;
--   > } VkSemaphoreSignalInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSemaphoreSignalInfo VkSemaphoreSignalInfo registry at www.khronos.org>
type VkSemaphoreSignalInfo = VkStruct VkSemaphoreSignalInfo' -- ' closing tick for hsc2hs

data VkSemaphoreSignalInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSemaphoreSignalInfo where
    type StructRep VkSemaphoreSignalInfo =
         'StructMeta "VkSemaphoreSignalInfo" VkSemaphoreSignalInfo  -- ' closing tick for hsc2hs
                                                                   #{size VkSemaphoreSignalInfo}
           #{alignment VkSemaphoreSignalInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSemaphoreSignalInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSemaphoreSignalInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "semaphore" VkSemaphore 'False 
                                                        #{offset VkSemaphoreSignalInfo, semaphore}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "value" Word64 'False 
                                               #{offset VkSemaphoreSignalInfo, value}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkSemaphoreSignalInfo`
type VkSemaphoreSignalInfoKHR = VkSemaphoreSignalInfo

-- | > typedef struct VkSemaphoreTypeCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSemaphoreType        semaphoreType;
--   >     uint64_t               initialValue;
--   > } VkSemaphoreTypeCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSemaphoreTypeCreateInfo VkSemaphoreTypeCreateInfo registry at www.khronos.org>
type VkSemaphoreTypeCreateInfo =
     VkStruct VkSemaphoreTypeCreateInfo' -- ' closing tick for hsc2hs

data VkSemaphoreTypeCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSemaphoreTypeCreateInfo where
    type StructRep VkSemaphoreTypeCreateInfo =
         'StructMeta "VkSemaphoreTypeCreateInfo" VkSemaphoreTypeCreateInfo -- ' closing tick for hsc2hs
           #{size VkSemaphoreTypeCreateInfo}
           #{alignment VkSemaphoreTypeCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSemaphoreTypeCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSemaphoreTypeCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "semaphoreType" VkSemaphoreType 'False 
                                                                #{offset VkSemaphoreTypeCreateInfo, semaphoreType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "initialValue" Word64 'False 
                                                      #{offset VkSemaphoreTypeCreateInfo, initialValue}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSemaphoreCreateInfo, VkPhysicalDeviceExternalSemaphoreInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkSemaphoreTypeCreateInfo`
type VkSemaphoreTypeCreateInfoKHR = VkSemaphoreTypeCreateInfo

-- | > typedef struct VkSemaphoreWaitInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSemaphoreWaitFlags flags;
--   >     uint32_t               semaphoreCount;
--   >     const VkSemaphore* pSemaphores;
--   >     const uint64_t*    pValues;
--   > } VkSemaphoreWaitInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSemaphoreWaitInfo VkSemaphoreWaitInfo registry at www.khronos.org>
type VkSemaphoreWaitInfo = VkStruct VkSemaphoreWaitInfo' -- ' closing tick for hsc2hs

data VkSemaphoreWaitInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSemaphoreWaitInfo where
    type StructRep VkSemaphoreWaitInfo =
         'StructMeta "VkSemaphoreWaitInfo" VkSemaphoreWaitInfo  -- ' closing tick for hsc2hs
                                                               #{size VkSemaphoreWaitInfo}
           #{alignment VkSemaphoreWaitInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSemaphoreWaitInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSemaphoreWaitInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkSemaphoreWaitFlags 'True 
                                                            #{offset VkSemaphoreWaitInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "semaphoreCount" Word32 'False 
                                                        #{offset VkSemaphoreWaitInfo, semaphoreCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSemaphores" (Ptr VkSemaphore) 'False 
                                                                #{offset VkSemaphoreWaitInfo, pSemaphores}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pValues" (Ptr Word64) 'False 
                                                       #{offset VkSemaphoreWaitInfo, pValues}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkSemaphoreWaitInfo`
type VkSemaphoreWaitInfoKHR = VkSemaphoreWaitInfo
