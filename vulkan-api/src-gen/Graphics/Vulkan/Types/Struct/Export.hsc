#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Export
       (VkExportFenceCreateInfo, VkExportFenceCreateInfoKHR,
        VkExportMemoryAllocateInfo, VkExportMemoryAllocateInfoKHR,
        VkExportMemoryAllocateInfoNV, VkExportSemaphoreCreateInfo,
        VkExportSemaphoreCreateInfoKHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.External      (VkExternalFenceHandleTypeFlags,
                                                           VkExternalMemoryHandleTypeFlags,
                                                           VkExternalMemoryHandleTypeFlagsNV,
                                                           VkExternalSemaphoreHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Fence       (VkFenceCreateInfo)
import           Graphics.Vulkan.Types.Struct.Memory      (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.Struct.Semaphore   (VkSemaphoreCreateInfo)

-- | > typedef struct VkExportFenceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalFenceHandleTypeFlags handleTypes;
--   > } VkExportFenceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportFenceCreateInfo VkExportFenceCreateInfo registry at www.khronos.org>
type VkExportFenceCreateInfo = VkStruct VkExportFenceCreateInfo' -- ' closing tick for hsc2hs

data VkExportFenceCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExportFenceCreateInfo where
    type StructRep VkExportFenceCreateInfo =
         'StructMeta "VkExportFenceCreateInfo" VkExportFenceCreateInfo -- ' closing tick for hsc2hs
           #{size VkExportFenceCreateInfo}
           #{alignment VkExportFenceCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExportFenceCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExportFenceCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleTypes" VkExternalFenceHandleTypeFlags 'True
                #{offset VkExportFenceCreateInfo, handleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkFenceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkExportFenceCreateInfo`
type VkExportFenceCreateInfoKHR = VkExportFenceCreateInfo

-- | > typedef struct VkExportMemoryAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlags handleTypes;
--   > } VkExportMemoryAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportMemoryAllocateInfo VkExportMemoryAllocateInfo registry at www.khronos.org>
type VkExportMemoryAllocateInfo =
     VkStruct VkExportMemoryAllocateInfo' -- ' closing tick for hsc2hs

data VkExportMemoryAllocateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExportMemoryAllocateInfo where
    type StructRep VkExportMemoryAllocateInfo =
         'StructMeta "VkExportMemoryAllocateInfo" VkExportMemoryAllocateInfo -- ' closing tick for hsc2hs
           #{size VkExportMemoryAllocateInfo}
           #{alignment VkExportMemoryAllocateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExportMemoryAllocateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExportMemoryAllocateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleTypes" VkExternalMemoryHandleTypeFlags 'True
                #{offset VkExportMemoryAllocateInfo, handleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkExportMemoryAllocateInfo`
type VkExportMemoryAllocateInfoKHR = VkExportMemoryAllocateInfo

-- | > typedef struct VkExportMemoryAllocateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleTypes;
--   > } VkExportMemoryAllocateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportMemoryAllocateInfoNV VkExportMemoryAllocateInfoNV registry at www.khronos.org>
type VkExportMemoryAllocateInfoNV =
     VkStruct VkExportMemoryAllocateInfoNV' -- ' closing tick for hsc2hs

data VkExportMemoryAllocateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExportMemoryAllocateInfoNV where
    type StructRep VkExportMemoryAllocateInfoNV =
         'StructMeta "VkExportMemoryAllocateInfoNV" -- ' closing tick for hsc2hs
           VkExportMemoryAllocateInfoNV
           #{size VkExportMemoryAllocateInfoNV}
           #{alignment VkExportMemoryAllocateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExportMemoryAllocateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExportMemoryAllocateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleTypes" VkExternalMemoryHandleTypeFlagsNV 'True
                #{offset VkExportMemoryAllocateInfoNV, handleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkExportSemaphoreCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalSemaphoreHandleTypeFlags handleTypes;
--   > } VkExportSemaphoreCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportSemaphoreCreateInfo VkExportSemaphoreCreateInfo registry at www.khronos.org>
type VkExportSemaphoreCreateInfo =
     VkStruct VkExportSemaphoreCreateInfo' -- ' closing tick for hsc2hs

data VkExportSemaphoreCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExportSemaphoreCreateInfo where
    type StructRep VkExportSemaphoreCreateInfo =
         'StructMeta "VkExportSemaphoreCreateInfo" -- ' closing tick for hsc2hs
           VkExportSemaphoreCreateInfo
           #{size VkExportSemaphoreCreateInfo}
           #{alignment VkExportSemaphoreCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExportSemaphoreCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExportSemaphoreCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleTypes" VkExternalSemaphoreHandleTypeFlags 'True
                #{offset VkExportSemaphoreCreateInfo, handleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSemaphoreCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkExportSemaphoreCreateInfo`
type VkExportSemaphoreCreateInfoKHR = VkExportSemaphoreCreateInfo
