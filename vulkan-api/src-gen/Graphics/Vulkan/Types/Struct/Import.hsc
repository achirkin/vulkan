#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Import
       (VkImportFenceFdInfoKHR, VkImportMemoryFdInfoKHR,
        VkImportMemoryHostPointerInfoEXT, VkImportSemaphoreFdInfoKHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.External            (VkExternalFenceHandleTypeFlagBits,
                                                                 VkExternalMemoryHandleTypeFlagBits,
                                                                 VkExternalSemaphoreHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.Fence               (VkFenceImportFlags)
import           Graphics.Vulkan.Types.Enum.SemaphoreImportFlag (VkSemaphoreImportFlags)
import           Graphics.Vulkan.Types.Enum.StructureType       (VkStructureType)
import           Graphics.Vulkan.Types.Handles                  (VkFence,
                                                                 VkSemaphore)
import           Graphics.Vulkan.Types.Struct.Memory            (VkMemoryAllocateInfo)

-- | > typedef struct VkImportFenceFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence              fence;
--   >     VkFenceImportFlags  flags;
--   >     VkExternalFenceHandleTypeFlagBits   handleType;
--   >     int                                    fd;
--   > } VkImportFenceFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportFenceFdInfoKHR VkImportFenceFdInfoKHR registry at www.khronos.org>
type VkImportFenceFdInfoKHR = VkStruct VkImportFenceFdInfoKHR' -- ' closing tick for hsc2hs

data VkImportFenceFdInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImportFenceFdInfoKHR where
    type StructRep VkImportFenceFdInfoKHR =
         'StructMeta "VkImportFenceFdInfoKHR" VkImportFenceFdInfoKHR -- ' closing tick for hsc2hs
           #{size VkImportFenceFdInfoKHR}
           #{alignment VkImportFenceFdInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImportFenceFdInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImportFenceFdInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fence" VkFence 'False 
                                                #{offset VkImportFenceFdInfoKHR, fence}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkFenceImportFlags 'True 
                                                          #{offset VkImportFenceFdInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalFenceHandleTypeFlagBits 'False
                #{offset VkImportFenceFdInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fd" CInt 'False 
                                          #{offset VkImportFenceFdInfoKHR, fd}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImportMemoryFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   >     int                              fd;
--   > } VkImportMemoryFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportMemoryFdInfoKHR VkImportMemoryFdInfoKHR registry at www.khronos.org>
type VkImportMemoryFdInfoKHR = VkStruct VkImportMemoryFdInfoKHR' -- ' closing tick for hsc2hs

data VkImportMemoryFdInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImportMemoryFdInfoKHR where
    type StructRep VkImportMemoryFdInfoKHR =
         'StructMeta "VkImportMemoryFdInfoKHR" VkImportMemoryFdInfoKHR -- ' closing tick for hsc2hs
           #{size VkImportMemoryFdInfoKHR}
           #{alignment VkImportMemoryFdInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImportMemoryFdInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImportMemoryFdInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalMemoryHandleTypeFlagBits 'True
                #{offset VkImportMemoryFdInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fd" CInt 'False 
                                          #{offset VkImportMemoryFdInfoKHR, fd}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkImportMemoryHostPointerInfoEXT {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   >     void* pHostPointer;
--   > } VkImportMemoryHostPointerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportMemoryHostPointerInfoEXT VkImportMemoryHostPointerInfoEXT registry at www.khronos.org>
type VkImportMemoryHostPointerInfoEXT =
     VkStruct VkImportMemoryHostPointerInfoEXT' -- ' closing tick for hsc2hs

data VkImportMemoryHostPointerInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImportMemoryHostPointerInfoEXT where
    type StructRep VkImportMemoryHostPointerInfoEXT =
         'StructMeta "VkImportMemoryHostPointerInfoEXT" -- ' closing tick for hsc2hs
           VkImportMemoryHostPointerInfoEXT
           #{size VkImportMemoryHostPointerInfoEXT}
           #{alignment VkImportMemoryHostPointerInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImportMemoryHostPointerInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImportMemoryHostPointerInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalMemoryHandleTypeFlagBits 'False
                #{offset VkImportMemoryHostPointerInfoEXT, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pHostPointer" (Ptr Void) 'False 
                                                          #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkImportSemaphoreFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore    semaphore;
--   >     VkSemaphoreImportFlags flags;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   >     int                              fd;
--   > } VkImportSemaphoreFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportSemaphoreFdInfoKHR VkImportSemaphoreFdInfoKHR registry at www.khronos.org>
type VkImportSemaphoreFdInfoKHR =
     VkStruct VkImportSemaphoreFdInfoKHR' -- ' closing tick for hsc2hs

data VkImportSemaphoreFdInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImportSemaphoreFdInfoKHR where
    type StructRep VkImportSemaphoreFdInfoKHR =
         'StructMeta "VkImportSemaphoreFdInfoKHR" VkImportSemaphoreFdInfoKHR -- ' closing tick for hsc2hs
           #{size VkImportSemaphoreFdInfoKHR}
           #{alignment VkImportSemaphoreFdInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImportSemaphoreFdInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImportSemaphoreFdInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "semaphore" VkSemaphore 'False 
                                                        #{offset VkImportSemaphoreFdInfoKHR, semaphore}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkSemaphoreImportFlags 'True 
                                                              #{offset VkImportSemaphoreFdInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalSemaphoreHandleTypeFlagBits -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkImportSemaphoreFdInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fd" CInt 'False 
                                          #{offset VkImportSemaphoreFdInfoKHR, fd}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
