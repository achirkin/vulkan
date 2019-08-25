#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformWin32Khr
       (VkD3D12FenceSubmitInfoKHR, VkExportFenceWin32HandleInfoKHR,
        VkExportMemoryWin32HandleInfoKHR, VkExportMemoryWin32HandleInfoNV,
        VkExportSemaphoreWin32HandleInfoKHR, VkFenceGetWin32HandleInfoKHR,
        VkImportFenceWin32HandleInfoKHR, VkImportMemoryWin32HandleInfoKHR,
        VkImportMemoryWin32HandleInfoNV,
        VkImportSemaphoreWin32HandleInfoKHR, VkMemoryGetWin32HandleInfoKHR,
        VkMemoryWin32HandlePropertiesKHR, VkSemaphoreGetWin32HandleInfoKHR,
        VkWin32KeyedMutexAcquireReleaseInfoKHR,
        VkWin32KeyedMutexAcquireReleaseInfoNV, VkWin32SurfaceCreateInfoKHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                 (VkWin32SurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.External            (VkExternalFenceHandleTypeFlagBits,
                                                                 VkExternalMemoryHandleTypeFlagBits,
                                                                 VkExternalMemoryHandleTypeFlagsNV,
                                                                 VkExternalSemaphoreHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.Fence               (VkFenceImportFlags)
import           Graphics.Vulkan.Types.Enum.SemaphoreImportFlag (VkSemaphoreImportFlags)
import           Graphics.Vulkan.Types.Enum.StructureType       (VkStructureType)
import           Graphics.Vulkan.Types.Handles                  (VkDeviceMemory,
                                                                 VkFence,
                                                                 VkSemaphore)
import           Graphics.Vulkan.Types.Include                  (DWORD, HANDLE,
                                                                 HINSTANCE,
                                                                 HWND, LPCWSTR,
                                                                 SECURITY_ATTRIBUTES)
import           Graphics.Vulkan.Types.Struct.Fence             (VkFenceCreateInfo)
import           Graphics.Vulkan.Types.Struct.Memory            (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.Struct.Semaphore         (VkSemaphoreCreateInfo)
import           Graphics.Vulkan.Types.Struct.SubmitInfo        (VkSubmitInfo)

-- | > typedef struct VkD3D12FenceSubmitInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         waitSemaphoreValuesCount;
--   >     const uint64_t* pWaitSemaphoreValues;
--   >     uint32_t         signalSemaphoreValuesCount;
--   >     const uint64_t* pSignalSemaphoreValues;
--   > } VkD3D12FenceSubmitInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkD3D12FenceSubmitInfoKHR VkD3D12FenceSubmitInfoKHR registry at www.khronos.org>
type VkD3D12FenceSubmitInfoKHR =
     VkStruct VkD3D12FenceSubmitInfoKHR' -- ' closing tick for hsc2hs

data VkD3D12FenceSubmitInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkD3D12FenceSubmitInfoKHR where
    type StructRep VkD3D12FenceSubmitInfoKHR =
         'StructMeta "VkD3D12FenceSubmitInfoKHR" VkD3D12FenceSubmitInfoKHR -- ' closing tick for hsc2hs
           #{size VkD3D12FenceSubmitInfoKHR}
           #{alignment VkD3D12FenceSubmitInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkD3D12FenceSubmitInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkD3D12FenceSubmitInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "waitSemaphoreValuesCount" Word32 'True 
                                                                 #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pWaitSemaphoreValues" (Ptr Word64) 'True 
                                                                   #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "signalSemaphoreValuesCount" Word32 'True 
                                                                   #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSignalSemaphoreValues" (Ptr Word64) 'True 
                                                                     #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSubmitInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkExportFenceWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                pNext;
--   >     const SECURITY_ATTRIBUTES* pAttributes;
--   >     DWORD                                      dwAccess;
--   >     LPCWSTR                                    name;
--   > } VkExportFenceWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportFenceWin32HandleInfoKHR VkExportFenceWin32HandleInfoKHR registry at www.khronos.org>
type VkExportFenceWin32HandleInfoKHR =
     VkStruct VkExportFenceWin32HandleInfoKHR' -- ' closing tick for hsc2hs

data VkExportFenceWin32HandleInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExportFenceWin32HandleInfoKHR where
    type StructRep VkExportFenceWin32HandleInfoKHR =
         'StructMeta "VkExportFenceWin32HandleInfoKHR" -- ' closing tick for hsc2hs
           VkExportFenceWin32HandleInfoKHR
           #{size VkExportFenceWin32HandleInfoKHR}
           #{alignment VkExportFenceWin32HandleInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExportFenceWin32HandleInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExportFenceWin32HandleInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttributes" (Ptr SECURITY_ATTRIBUTES) 'True 
                                                                       #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dwAccess" DWORD 'False 
                                                 #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" LPCWSTR 'False 
                                               #{offset VkExportFenceWin32HandleInfoKHR, name}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkFenceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkExportMemoryWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES* pAttributes;
--   >     DWORD                            dwAccess;
--   >     LPCWSTR                          name;
--   > } VkExportMemoryWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportMemoryWin32HandleInfoKHR VkExportMemoryWin32HandleInfoKHR registry at www.khronos.org>
type VkExportMemoryWin32HandleInfoKHR =
     VkStruct VkExportMemoryWin32HandleInfoKHR' -- ' closing tick for hsc2hs

data VkExportMemoryWin32HandleInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExportMemoryWin32HandleInfoKHR where
    type StructRep VkExportMemoryWin32HandleInfoKHR =
         'StructMeta "VkExportMemoryWin32HandleInfoKHR" -- ' closing tick for hsc2hs
           VkExportMemoryWin32HandleInfoKHR
           #{size VkExportMemoryWin32HandleInfoKHR}
           #{alignment VkExportMemoryWin32HandleInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExportMemoryWin32HandleInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExportMemoryWin32HandleInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttributes" (Ptr SECURITY_ATTRIBUTES) 'True 
                                                                       #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dwAccess" DWORD 'False 
                                                 #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" LPCWSTR 'False 
                                               #{offset VkExportMemoryWin32HandleInfoKHR, name}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkExportMemoryWin32HandleInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES*       pAttributes;
--   >     DWORD                            dwAccess;
--   > } VkExportMemoryWin32HandleInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportMemoryWin32HandleInfoNV VkExportMemoryWin32HandleInfoNV registry at www.khronos.org>
type VkExportMemoryWin32HandleInfoNV =
     VkStruct VkExportMemoryWin32HandleInfoNV' -- ' closing tick for hsc2hs

data VkExportMemoryWin32HandleInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExportMemoryWin32HandleInfoNV where
    type StructRep VkExportMemoryWin32HandleInfoNV =
         'StructMeta "VkExportMemoryWin32HandleInfoNV" -- ' closing tick for hsc2hs
           VkExportMemoryWin32HandleInfoNV
           #{size VkExportMemoryWin32HandleInfoNV}
           #{alignment VkExportMemoryWin32HandleInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExportMemoryWin32HandleInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExportMemoryWin32HandleInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttributes" (Ptr SECURITY_ATTRIBUTES) 'True 
                                                                       #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dwAccess" DWORD 'True 
                                                #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkExportSemaphoreWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES*       pAttributes;
--   >     DWORD                            dwAccess;
--   >     LPCWSTR                          name;
--   > } VkExportSemaphoreWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportSemaphoreWin32HandleInfoKHR VkExportSemaphoreWin32HandleInfoKHR registry at www.khronos.org>
type VkExportSemaphoreWin32HandleInfoKHR =
     VkStruct VkExportSemaphoreWin32HandleInfoKHR' -- ' closing tick for hsc2hs

data VkExportSemaphoreWin32HandleInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExportSemaphoreWin32HandleInfoKHR where
    type StructRep VkExportSemaphoreWin32HandleInfoKHR =
         'StructMeta "VkExportSemaphoreWin32HandleInfoKHR" -- ' closing tick for hsc2hs
           VkExportSemaphoreWin32HandleInfoKHR
           #{size VkExportSemaphoreWin32HandleInfoKHR}
           #{alignment VkExportSemaphoreWin32HandleInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttributes" (Ptr SECURITY_ATTRIBUTES) 'True 
                                                                       #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dwAccess" DWORD 'False 
                                                 #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" LPCWSTR 'False 
                                               #{offset VkExportSemaphoreWin32HandleInfoKHR, name}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSemaphoreCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkFenceGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence                                fence;
--   >     VkExternalFenceHandleTypeFlagBits   handleType;
--   > } VkFenceGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFenceGetWin32HandleInfoKHR VkFenceGetWin32HandleInfoKHR registry at www.khronos.org>
type VkFenceGetWin32HandleInfoKHR =
     VkStruct VkFenceGetWin32HandleInfoKHR' -- ' closing tick for hsc2hs

data VkFenceGetWin32HandleInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkFenceGetWin32HandleInfoKHR where
    type StructRep VkFenceGetWin32HandleInfoKHR =
         'StructMeta "VkFenceGetWin32HandleInfoKHR" -- ' closing tick for hsc2hs
           VkFenceGetWin32HandleInfoKHR
           #{size VkFenceGetWin32HandleInfoKHR}
           #{alignment VkFenceGetWin32HandleInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkFenceGetWin32HandleInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkFenceGetWin32HandleInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fence" VkFence 'False 
                                                #{offset VkFenceGetWin32HandleInfoKHR, fence}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalFenceHandleTypeFlagBits 'False
                #{offset VkFenceGetWin32HandleInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImportFenceWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                        pNext;
--   >     VkFence                          fence;
--   >     VkFenceImportFlags              flags;
--   >     VkExternalFenceHandleTypeFlagBits  handleType;
--   >     HANDLE                             handle;
--   >     LPCWSTR                            name;
--   > } VkImportFenceWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportFenceWin32HandleInfoKHR VkImportFenceWin32HandleInfoKHR registry at www.khronos.org>
type VkImportFenceWin32HandleInfoKHR =
     VkStruct VkImportFenceWin32HandleInfoKHR' -- ' closing tick for hsc2hs

data VkImportFenceWin32HandleInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImportFenceWin32HandleInfoKHR where
    type StructRep VkImportFenceWin32HandleInfoKHR =
         'StructMeta "VkImportFenceWin32HandleInfoKHR" -- ' closing tick for hsc2hs
           VkImportFenceWin32HandleInfoKHR
           #{size VkImportFenceWin32HandleInfoKHR}
           #{alignment VkImportFenceWin32HandleInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImportFenceWin32HandleInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImportFenceWin32HandleInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fence" VkFence 'False 
                                                #{offset VkImportFenceWin32HandleInfoKHR, fence}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkFenceImportFlags 'True 
                                                          #{offset VkImportFenceWin32HandleInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalFenceHandleTypeFlagBits 'True
                #{offset VkImportFenceWin32HandleInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handle" HANDLE 'True 
                                               #{offset VkImportFenceWin32HandleInfoKHR, handle}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" LPCWSTR 'True 
                                              #{offset VkImportFenceWin32HandleInfoKHR, name}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImportMemoryWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   >     HANDLE           handle;
--   >     LPCWSTR          name;
--   > } VkImportMemoryWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportMemoryWin32HandleInfoKHR VkImportMemoryWin32HandleInfoKHR registry at www.khronos.org>
type VkImportMemoryWin32HandleInfoKHR =
     VkStruct VkImportMemoryWin32HandleInfoKHR' -- ' closing tick for hsc2hs

data VkImportMemoryWin32HandleInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImportMemoryWin32HandleInfoKHR where
    type StructRep VkImportMemoryWin32HandleInfoKHR =
         'StructMeta "VkImportMemoryWin32HandleInfoKHR" -- ' closing tick for hsc2hs
           VkImportMemoryWin32HandleInfoKHR
           #{size VkImportMemoryWin32HandleInfoKHR}
           #{alignment VkImportMemoryWin32HandleInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImportMemoryWin32HandleInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImportMemoryWin32HandleInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalMemoryHandleTypeFlagBits 'True
                #{offset VkImportMemoryWin32HandleInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handle" HANDLE 'True 
                                               #{offset VkImportMemoryWin32HandleInfoKHR, handle}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" LPCWSTR 'True 
                                              #{offset VkImportMemoryWin32HandleInfoKHR, name}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkImportMemoryWin32HandleInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleType;
--   >     HANDLE                           handle;
--   > } VkImportMemoryWin32HandleInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportMemoryWin32HandleInfoNV VkImportMemoryWin32HandleInfoNV registry at www.khronos.org>
type VkImportMemoryWin32HandleInfoNV =
     VkStruct VkImportMemoryWin32HandleInfoNV' -- ' closing tick for hsc2hs

data VkImportMemoryWin32HandleInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImportMemoryWin32HandleInfoNV where
    type StructRep VkImportMemoryWin32HandleInfoNV =
         'StructMeta "VkImportMemoryWin32HandleInfoNV" -- ' closing tick for hsc2hs
           VkImportMemoryWin32HandleInfoNV
           #{size VkImportMemoryWin32HandleInfoNV}
           #{alignment VkImportMemoryWin32HandleInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImportMemoryWin32HandleInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImportMemoryWin32HandleInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalMemoryHandleTypeFlagsNV 'True
                #{offset VkImportMemoryWin32HandleInfoNV, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handle" HANDLE 'True 
                                               #{offset VkImportMemoryWin32HandleInfoNV, handle}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkImportSemaphoreWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore    semaphore;
--   >     VkSemaphoreImportFlags flags;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   >     HANDLE           handle;
--   >     LPCWSTR          name;
--   > } VkImportSemaphoreWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportSemaphoreWin32HandleInfoKHR VkImportSemaphoreWin32HandleInfoKHR registry at www.khronos.org>
type VkImportSemaphoreWin32HandleInfoKHR =
     VkStruct VkImportSemaphoreWin32HandleInfoKHR' -- ' closing tick for hsc2hs

data VkImportSemaphoreWin32HandleInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImportSemaphoreWin32HandleInfoKHR where
    type StructRep VkImportSemaphoreWin32HandleInfoKHR =
         'StructMeta "VkImportSemaphoreWin32HandleInfoKHR" -- ' closing tick for hsc2hs
           VkImportSemaphoreWin32HandleInfoKHR
           #{size VkImportSemaphoreWin32HandleInfoKHR}
           #{alignment VkImportSemaphoreWin32HandleInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "semaphore" VkSemaphore 'False 
                                                        #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkSemaphoreImportFlags 'True 
                                                              #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalSemaphoreHandleTypeFlagBits -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handle" HANDLE 'True 
                                               #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" LPCWSTR 'True 
                                              #{offset VkImportSemaphoreWin32HandleInfoKHR, name}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkMemoryGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceMemory                   memory;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   > } VkMemoryGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryGetWin32HandleInfoKHR VkMemoryGetWin32HandleInfoKHR registry at www.khronos.org>
type VkMemoryGetWin32HandleInfoKHR =
     VkStruct VkMemoryGetWin32HandleInfoKHR' -- ' closing tick for hsc2hs

data VkMemoryGetWin32HandleInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryGetWin32HandleInfoKHR where
    type StructRep VkMemoryGetWin32HandleInfoKHR =
         'StructMeta "VkMemoryGetWin32HandleInfoKHR" -- ' closing tick for hsc2hs
           VkMemoryGetWin32HandleInfoKHR
           #{size VkMemoryGetWin32HandleInfoKHR}
           #{alignment VkMemoryGetWin32HandleInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryGetWin32HandleInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryGetWin32HandleInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memory" VkDeviceMemory 'False 
                                                        #{offset VkMemoryGetWin32HandleInfoKHR, memory}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalMemoryHandleTypeFlagBits 'False
                #{offset VkMemoryGetWin32HandleInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkMemoryWin32HandlePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         memoryTypeBits;
--   > } VkMemoryWin32HandlePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryWin32HandlePropertiesKHR VkMemoryWin32HandlePropertiesKHR registry at www.khronos.org>
type VkMemoryWin32HandlePropertiesKHR =
     VkStruct VkMemoryWin32HandlePropertiesKHR' -- ' closing tick for hsc2hs

data VkMemoryWin32HandlePropertiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryWin32HandlePropertiesKHR where
    type StructRep VkMemoryWin32HandlePropertiesKHR =
         'StructMeta "VkMemoryWin32HandlePropertiesKHR" -- ' closing tick for hsc2hs
           VkMemoryWin32HandlePropertiesKHR
           #{size VkMemoryWin32HandlePropertiesKHR}
           #{alignment VkMemoryWin32HandlePropertiesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryWin32HandlePropertiesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryWin32HandlePropertiesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryTypeBits" Word32 'False 
                                                        #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSemaphoreGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore                      semaphore;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   > } VkSemaphoreGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSemaphoreGetWin32HandleInfoKHR VkSemaphoreGetWin32HandleInfoKHR registry at www.khronos.org>
type VkSemaphoreGetWin32HandleInfoKHR =
     VkStruct VkSemaphoreGetWin32HandleInfoKHR' -- ' closing tick for hsc2hs

data VkSemaphoreGetWin32HandleInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSemaphoreGetWin32HandleInfoKHR where
    type StructRep VkSemaphoreGetWin32HandleInfoKHR =
         'StructMeta "VkSemaphoreGetWin32HandleInfoKHR" -- ' closing tick for hsc2hs
           VkSemaphoreGetWin32HandleInfoKHR
           #{size VkSemaphoreGetWin32HandleInfoKHR}
           #{alignment VkSemaphoreGetWin32HandleInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "semaphore" VkSemaphore 'False 
                                                        #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalSemaphoreHandleTypeFlagBits -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkWin32KeyedMutexAcquireReleaseInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         acquireCount;
--   >     const VkDeviceMemory* pAcquireSyncs;
--   >     const uint64_t* pAcquireKeys;
--   >     const uint32_t* pAcquireTimeouts;
--   >     uint32_t         releaseCount;
--   >     const VkDeviceMemory* pReleaseSyncs;
--   >     const uint64_t* pReleaseKeys;
--   > } VkWin32KeyedMutexAcquireReleaseInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkWin32KeyedMutexAcquireReleaseInfoKHR VkWin32KeyedMutexAcquireReleaseInfoKHR registry at www.khronos.org>
type VkWin32KeyedMutexAcquireReleaseInfoKHR =
     VkStruct VkWin32KeyedMutexAcquireReleaseInfoKHR' -- ' closing tick for hsc2hs

data VkWin32KeyedMutexAcquireReleaseInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkWin32KeyedMutexAcquireReleaseInfoKHR where
    type StructRep VkWin32KeyedMutexAcquireReleaseInfoKHR =
         'StructMeta "VkWin32KeyedMutexAcquireReleaseInfoKHR" -- ' closing tick for hsc2hs
           VkWin32KeyedMutexAcquireReleaseInfoKHR
           #{size VkWin32KeyedMutexAcquireReleaseInfoKHR}
           #{alignment VkWin32KeyedMutexAcquireReleaseInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "acquireCount" Word32 'True 
                                                     #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAcquireSyncs" (Ptr VkDeviceMemory) 'False 
                                                                     #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAcquireKeys" (Ptr Word64) 'False 
                                                            #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAcquireTimeouts" (Ptr Word32) 'False 
                                                                #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "releaseCount" Word32 'True 
                                                     #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pReleaseSyncs" (Ptr VkDeviceMemory) 'False 
                                                                     #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pReleaseKeys" (Ptr Word64) 'False 
                                                            #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSubmitInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkWin32KeyedMutexAcquireReleaseInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         acquireCount;
--   >     const VkDeviceMemory*            pAcquireSyncs;
--   >     const uint64_t*                  pAcquireKeys;
--   >     const uint32_t*                  pAcquireTimeoutMilliseconds;
--   >     uint32_t                         releaseCount;
--   >     const VkDeviceMemory*            pReleaseSyncs;
--   >     const uint64_t*                  pReleaseKeys;
--   > } VkWin32KeyedMutexAcquireReleaseInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkWin32KeyedMutexAcquireReleaseInfoNV VkWin32KeyedMutexAcquireReleaseInfoNV registry at www.khronos.org>
type VkWin32KeyedMutexAcquireReleaseInfoNV =
     VkStruct VkWin32KeyedMutexAcquireReleaseInfoNV' -- ' closing tick for hsc2hs

data VkWin32KeyedMutexAcquireReleaseInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkWin32KeyedMutexAcquireReleaseInfoNV where
    type StructRep VkWin32KeyedMutexAcquireReleaseInfoNV =
         'StructMeta "VkWin32KeyedMutexAcquireReleaseInfoNV" -- ' closing tick for hsc2hs
           VkWin32KeyedMutexAcquireReleaseInfoNV
           #{size VkWin32KeyedMutexAcquireReleaseInfoNV}
           #{alignment VkWin32KeyedMutexAcquireReleaseInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "acquireCount" Word32 'True 
                                                     #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAcquireSyncs" (Ptr VkDeviceMemory) 'False 
                                                                     #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAcquireKeys" (Ptr Word64) 'False 
                                                            #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAcquireTimeoutMilliseconds" (Ptr Word32) 'False
                #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "releaseCount" Word32 'True 
                                                     #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pReleaseSyncs" (Ptr VkDeviceMemory) 'False 
                                                                     #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pReleaseKeys" (Ptr Word64) 'False 
                                                            #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSubmitInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkWin32SurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkWin32SurfaceCreateFlagsKHR   flags;
--   >     HINSTANCE                        hinstance;
--   >     HWND                             hwnd;
--   > } VkWin32SurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkWin32SurfaceCreateInfoKHR VkWin32SurfaceCreateInfoKHR registry at www.khronos.org>
type VkWin32SurfaceCreateInfoKHR =
     VkStruct VkWin32SurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

data VkWin32SurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkWin32SurfaceCreateInfoKHR where
    type StructRep VkWin32SurfaceCreateInfoKHR =
         'StructMeta "VkWin32SurfaceCreateInfoKHR" -- ' closing tick for hsc2hs
           VkWin32SurfaceCreateInfoKHR
           #{size VkWin32SurfaceCreateInfoKHR}
           #{alignment VkWin32SurfaceCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkWin32SurfaceCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkWin32SurfaceCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkWin32SurfaceCreateFlagsKHR 'True 
                                                                    #{offset VkWin32SurfaceCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "hinstance" HINSTANCE 'False 
                                                      #{offset VkWin32SurfaceCreateInfoKHR, hinstance}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "hwnd" HWND 'False 
                                            #{offset VkWin32SurfaceCreateInfoKHR, hwnd}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
