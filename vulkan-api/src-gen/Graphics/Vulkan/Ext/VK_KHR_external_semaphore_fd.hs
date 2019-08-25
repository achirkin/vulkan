{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_semaphore_fd
       (-- * Vulkan extension: @VK_KHR_external_semaphore_fd@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @80@
        --
        -- Required extensions: 'VK_KHR_external_semaphore'.
        --

        -- ** Required extensions: 'VK_KHR_external_semaphore'.
        module Graphics.Vulkan.Marshal, VkExternalFenceFeatureBitmask(..),
        VkExternalFenceHandleTypeBitmask(..),
        VkExternalMemoryFeatureBitmask(..),
        VkExternalMemoryFeatureBitmaskNV(..),
        VkExternalMemoryHandleTypeBitmaskNV(..),
        VkExternalMemoryHandleTypeBitmask(..),
        VkExternalSemaphoreFeatureBitmask(..),
        VkExternalSemaphoreHandleTypeBitmask(..),
        VkExternalFenceFeatureFlagBits(),
        VkExternalFenceFeatureFlagBitsKHR(..),
        VkExternalFenceFeatureFlags(), VkExternalFenceHandleTypeFlagBits(),
        VkExternalFenceHandleTypeFlagBitsKHR(..),
        VkExternalFenceHandleTypeFlags(),
        VkExternalMemoryFeatureFlagBits(),
        VkExternalMemoryFeatureFlagBitsKHR(..),
        VkExternalMemoryFeatureFlagBitsNV(),
        VkExternalMemoryFeatureFlags(), VkExternalMemoryFeatureFlagsNV(),
        VkExternalMemoryHandleTypeFlagBits(),
        VkExternalMemoryHandleTypeFlagBitsKHR(..),
        VkExternalMemoryHandleTypeFlagBitsNV(),
        VkExternalMemoryHandleTypeFlags(),
        VkExternalMemoryHandleTypeFlagsNV(),
        VkExternalSemaphoreFeatureFlagBits(),
        VkExternalSemaphoreFeatureFlagBitsKHR(..),
        VkExternalSemaphoreFeatureFlags(),
        VkExternalSemaphoreHandleTypeFlagBits(),
        VkExternalSemaphoreHandleTypeFlagBitsKHR(..),
        VkExternalSemaphoreHandleTypeFlags(), VkBool32(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkImportSemaphoreFdInfoKHR, VkSemaphoreGetFdInfoKHR,
        VkSemaphoreImportBitmask(..), VkSemaphoreImportFlagBits(),
        VkSemaphoreImportFlagBitsKHR(..), VkSemaphoreImportFlags(),
        VkStructureType(..), -- > #include "vk_platform.h"
                             VkImportSemaphoreFdKHR,
        pattern VkImportSemaphoreFdKHR, HS_vkImportSemaphoreFdKHR,
        PFN_vkImportSemaphoreFdKHR, VkGetSemaphoreFdKHR,
        pattern VkGetSemaphoreFdKHR, HS_vkGetSemaphoreFdKHR,
        PFN_vkGetSemaphoreFdKHR, VkResult(..), VkBuffer, VkBufferView,
        VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkCommandPool, VkCommandPool_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        VkDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT_T(),
        VkDescriptorPool, VkDescriptorPool_T(), VkDescriptorSet,
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorSet_T(), VkDescriptorUpdateTemplate,
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDescriptorUpdateTemplate_T(), VkDevice, VkDeviceMemory,
        VkDeviceMemory_T(), VkDevice_T(), VkDisplayKHR, VkDisplayKHR_T(),
        VkDisplayModeKHR, VkDisplayModeKHR_T(), VkEvent, VkEvent_T(),
        VkFence, VkFence_T(), VkFramebuffer, VkFramebuffer_T(), VkImage,
        VkImageView, VkImageView_T(), VkImage_T(),
        VkIndirectCommandsLayoutNVX, VkIndirectCommandsLayoutNVX_T(),
        VkInstance, VkInstance_T(), VkObjectTableNVX, VkObjectTableNVX_T(),
        VkPhysicalDevice, VkPhysicalDevice_T(), VkPipeline,
        VkPipelineCache, VkPipelineCache_T(), VkPipelineLayout,
        VkPipelineLayout_T(), VkPipeline_T(), VkQueryPool, VkQueryPool_T(),
        VkQueue, VkQueue_T(), VkRenderPass, VkRenderPass_T(), VkSampler,
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversionKHR,
        VkSamplerYcbcrConversionKHR_T(), VkSamplerYcbcrConversion_T(),
        VkSampler_T(), VkSemaphore, VkSemaphore_T(), VkShaderModule,
        VkShaderModule_T(), VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR,
        VkSwapchainKHR_T(), VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VkImportFenceFdInfoKHR, VkImportMemoryFdInfoKHR,
        VkImportMemoryHostPointerInfoEXT, VkSemaphoreCreateInfo,
        VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION,
        VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR)
       where
import           GHC.Ptr                                        (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                   (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.External
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SemaphoreImportFlag
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Import
import           Graphics.Vulkan.Types.Struct.Semaphore

pattern VkImportSemaphoreFdKHR :: CString

pattern VkImportSemaphoreFdKHR <-
        (is_VkImportSemaphoreFdKHR -> True)
  where
    VkImportSemaphoreFdKHR = _VkImportSemaphoreFdKHR

{-# INLINE _VkImportSemaphoreFdKHR #-}

_VkImportSemaphoreFdKHR :: CString
_VkImportSemaphoreFdKHR = Ptr "vkImportSemaphoreFdKHR\NUL"#

{-# INLINE is_VkImportSemaphoreFdKHR #-}

is_VkImportSemaphoreFdKHR :: CString -> Bool
is_VkImportSemaphoreFdKHR
  = (EQ ==) . cmpCStrings _VkImportSemaphoreFdKHR

type VkImportSemaphoreFdKHR = "vkImportSemaphoreFdKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
--   > VkResult vkImportSemaphoreFdKHR
--   >     ( VkDevice device
--   >     , const VkImportSemaphoreFdInfoKHR* pImportSemaphoreFdInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkImportSemaphoreFdKHR vkImportSemaphoreFdKHR registry at www.khronos.org>
type HS_vkImportSemaphoreFdKHR =
     VkDevice -- ^ device
              -> Ptr VkImportSemaphoreFdInfoKHR -- ^ pImportSemaphoreFdInfo
                                                -> IO VkResult

type PFN_vkImportSemaphoreFdKHR = FunPtr HS_vkImportSemaphoreFdKHR

foreign import ccall unsafe "dynamic"
               unwrapVkImportSemaphoreFdKHRUnsafe ::
               PFN_vkImportSemaphoreFdKHR -> HS_vkImportSemaphoreFdKHR

foreign import ccall safe "dynamic"
               unwrapVkImportSemaphoreFdKHRSafe ::
               PFN_vkImportSemaphoreFdKHR -> HS_vkImportSemaphoreFdKHR

instance VulkanProc "vkImportSemaphoreFdKHR" where
    type VkProcType "vkImportSemaphoreFdKHR" =
         HS_vkImportSemaphoreFdKHR
    vkProcSymbol = _VkImportSemaphoreFdKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkImportSemaphoreFdKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkImportSemaphoreFdKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetSemaphoreFdKHR :: CString

pattern VkGetSemaphoreFdKHR <- (is_VkGetSemaphoreFdKHR -> True)
  where
    VkGetSemaphoreFdKHR = _VkGetSemaphoreFdKHR

{-# INLINE _VkGetSemaphoreFdKHR #-}

_VkGetSemaphoreFdKHR :: CString
_VkGetSemaphoreFdKHR = Ptr "vkGetSemaphoreFdKHR\NUL"#

{-# INLINE is_VkGetSemaphoreFdKHR #-}

is_VkGetSemaphoreFdKHR :: CString -> Bool
is_VkGetSemaphoreFdKHR = (EQ ==) . cmpCStrings _VkGetSemaphoreFdKHR

type VkGetSemaphoreFdKHR = "vkGetSemaphoreFdKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetSemaphoreFdKHR
--   >     ( VkDevice device
--   >     , const VkSemaphoreGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetSemaphoreFdKHR vkGetSemaphoreFdKHR registry at www.khronos.org>
type HS_vkGetSemaphoreFdKHR =
     VkDevice -- ^ device
              -> Ptr VkSemaphoreGetFdInfoKHR -- ^ pGetFdInfo
                                             -> Ptr CInt -- ^ pFd
                                                         -> IO VkResult

type PFN_vkGetSemaphoreFdKHR = FunPtr HS_vkGetSemaphoreFdKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetSemaphoreFdKHRUnsafe ::
               PFN_vkGetSemaphoreFdKHR -> HS_vkGetSemaphoreFdKHR

foreign import ccall safe "dynamic" unwrapVkGetSemaphoreFdKHRSafe
               :: PFN_vkGetSemaphoreFdKHR -> HS_vkGetSemaphoreFdKHR

instance VulkanProc "vkGetSemaphoreFdKHR" where
    type VkProcType "vkGetSemaphoreFdKHR" = HS_vkGetSemaphoreFdKHR
    vkProcSymbol = _VkGetSemaphoreFdKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetSemaphoreFdKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetSemaphoreFdKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME -> True)
  where
    VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
      = _VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  = Ptr "VK_KHR_external_semaphore_fd\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME

type VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME =
     "VK_KHR_external_semaphore_fd"

pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR =
        VkStructureType 1000079000

pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR =
        VkStructureType 1000079001
