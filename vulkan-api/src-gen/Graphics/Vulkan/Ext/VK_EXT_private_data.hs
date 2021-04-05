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
module Graphics.Vulkan.Ext.VK_EXT_private_data
       (-- * Vulkan extension: @VK_EXT_private_data@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Matthew Rusch @mattruschnv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @296@
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
        VkBuildAccelerationStructureFlagsNV(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorBindingFlagsEXT(..), VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDirectFBSurfaceCreateFlagsEXT(..),
        VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkGeometryFlagsNV(..),
        VkGeometryInstanceFlagsNV(..), VkHeadlessSurfaceCreateFlagsEXT(..),
        VkIOSSurfaceCreateFlagsMVK(..),
        VkImagePipeSurfaceCreateFlagsFUCHSIA(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMetalSurfaceCreateFlagsEXT(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageReductionStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationDepthClipStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineRasterizationStateStreamCreateFlagsEXT(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkResolveModeFlagsKHR(..),
        VkSemaphoreCreateFlags(..), VkSemaphoreImportFlagsKHR(..),
        VkSemaphoreWaitFlagsKHR(..),
        VkStreamDescriptorSurfaceCreateFlagsGGP(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..), VkDeviceCreateInfo,
        VkDevicePrivateDataCreateInfoEXT,
        VkDeviceDiagnosticsConfigBitmaskNV(..), VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceDiagnosticsConfigFlagBitsNV(),
        VkDeviceDiagnosticsConfigFlagsNV(),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkPhysicalDeviceFeatures,
        VkPhysicalDeviceFeatures2, VkPhysicalDevicePrivateDataFeaturesEXT,
        VkPrivateDataSlotCreateBitmaskEXT(..),
        VkPrivateDataSlotCreateFlagBitsEXT(),
        VkPrivateDataSlotCreateFlagsEXT(), VkPrivateDataSlotCreateInfoEXT,
        VkStructureType(..), -- > #include "vk_platform.h"
                             VkCreatePrivateDataSlotEXT,
        pattern VkCreatePrivateDataSlotEXT, HS_vkCreatePrivateDataSlotEXT,
        PFN_vkCreatePrivateDataSlotEXT, VkDestroyPrivateDataSlotEXT,
        pattern VkDestroyPrivateDataSlotEXT,
        HS_vkDestroyPrivateDataSlotEXT, PFN_vkDestroyPrivateDataSlotEXT,
        VkSetPrivateDataEXT, pattern VkSetPrivateDataEXT,
        HS_vkSetPrivateDataEXT, PFN_vkSetPrivateDataEXT,
        VkGetPrivateDataEXT, pattern VkGetPrivateDataEXT,
        HS_vkGetPrivateDataEXT, PFN_vkGetPrivateDataEXT,
        VkInternalAllocationType(..), VkObjectType(..), VkResult(..),
        VkSystemAllocationScope(..), newVkAllocationFunction,
        newVkDebugReportCallbackEXT, newVkDebugUtilsMessengerCallbackEXT,
        newVkFreeFunction, newVkInternalAllocationNotification,
        newVkInternalFreeNotification, newVkReallocationFunction,
        newVkVoidFunction, unwrapVkAllocationFunction,
        unwrapVkDebugReportCallbackEXT,
        unwrapVkDebugUtilsMessengerCallbackEXT, unwrapVkFreeFunction,
        unwrapVkInternalAllocationNotification,
        unwrapVkInternalFreeNotification, unwrapVkReallocationFunction,
        unwrapVkVoidFunction, HS_vkAllocationFunction,
        HS_vkDebugReportCallbackEXT, HS_vkDebugUtilsMessengerCallbackEXT,
        HS_vkFreeFunction, HS_vkInternalAllocationNotification,
        HS_vkInternalFreeNotification, HS_vkReallocationFunction,
        HS_vkVoidFunction, PFN_vkAllocationFunction,
        PFN_vkDebugReportCallbackEXT, PFN_vkDebugUtilsMessengerCallbackEXT,
        PFN_vkFreeFunction, PFN_vkInternalAllocationNotification,
        PFN_vkInternalFreeNotification, PFN_vkReallocationFunction,
        PFN_vkVoidFunction, VkAccelerationStructureKHR,
        VkAccelerationStructureKHR_T(), VkAccelerationStructureNV,
        VkAccelerationStructureNV_T(), VkBuffer, VkBufferView,
        VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkCommandPool, VkCommandPool_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        VkDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT_T(),
        VkDeferredOperationKHR, VkDeferredOperationKHR_T(),
        VkDescriptorPool, VkDescriptorPool_T(), VkDescriptorSet,
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorSet_T(), VkDescriptorUpdateTemplate,
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDescriptorUpdateTemplate_T(), VkDevice, VkDeviceMemory,
        VkDeviceMemory_T(), VkDevice_T(), VkDisplayKHR, VkDisplayKHR_T(),
        VkDisplayModeKHR, VkDisplayModeKHR_T(), VkEvent, VkEvent_T(),
        VkFence, VkFence_T(), VkFramebuffer, VkFramebuffer_T(), VkImage,
        VkImageView, VkImageView_T(), VkImage_T(),
        VkIndirectCommandsLayoutNV, VkIndirectCommandsLayoutNV_T(),
        VkInstance, VkInstance_T(), VkPerformanceConfigurationINTEL,
        VkPerformanceConfigurationINTEL_T(), VkPhysicalDevice,
        VkPhysicalDevice_T(), VkPipeline, VkPipelineCache,
        VkPipelineCache_T(), VkPipelineLayout, VkPipelineLayout_T(),
        VkPipeline_T(), VkPrivateDataSlotEXT, VkPrivateDataSlotEXT_T(),
        VkQueryPool, VkQueryPool_T(), VkQueue, VkQueue_T(), VkRenderPass,
        VkRenderPass_T(), VkSampler, VkSamplerYcbcrConversion,
        VkSamplerYcbcrConversionKHR, VkSamplerYcbcrConversionKHR_T(),
        VkSamplerYcbcrConversion_T(), VkSampler_T(), VkSemaphore,
        VkSemaphore_T(), VkShaderModule, VkShaderModule_T(), VkSurfaceKHR,
        VkSurfaceKHR_T(), VkSwapchainKHR, VkSwapchainKHR_T(),
        VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VkAllocationCallbacks, VK_EXT_PRIVATE_DATA_SPEC_VERSION,
        pattern VK_EXT_PRIVATE_DATA_SPEC_VERSION,
        VK_EXT_PRIVATE_DATA_EXTENSION_NAME,
        pattern VK_EXT_PRIVATE_DATA_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT,
        pattern VK_OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT)
       where
import GHC.Ptr                                                   (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                              (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.InternalAllocationType
import Graphics.Vulkan.Types.Enum.ObjectType
import Graphics.Vulkan.Types.Enum.PrivateDataSlotCreateFlagsEXT
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.SystemAllocationScope
import Graphics.Vulkan.Types.Funcpointers
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.AllocationCallbacks
import Graphics.Vulkan.Types.Struct.Device                       (VkDeviceCreateInfo,
                                                                  VkDevicePrivateDataCreateInfoEXT,
                                                                  VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice               (VkPhysicalDeviceFeatures2,
                                                                  VkPhysicalDevicePrivateDataFeaturesEXT)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures       (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.PrivateDataSlotCreateInfoEXT

pattern VkCreatePrivateDataSlotEXT :: CString

pattern VkCreatePrivateDataSlotEXT <-
        (is_VkCreatePrivateDataSlotEXT -> True)
  where
    VkCreatePrivateDataSlotEXT = _VkCreatePrivateDataSlotEXT

{-# INLINE _VkCreatePrivateDataSlotEXT #-}

_VkCreatePrivateDataSlotEXT :: CString
_VkCreatePrivateDataSlotEXT = Ptr "vkCreatePrivateDataSlotEXT\NUL"#

{-# INLINE is_VkCreatePrivateDataSlotEXT #-}

is_VkCreatePrivateDataSlotEXT :: CString -> Bool
is_VkCreatePrivateDataSlotEXT
  = (EQ ==) . cmpCStrings _VkCreatePrivateDataSlotEXT

type VkCreatePrivateDataSlotEXT = "vkCreatePrivateDataSlotEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreatePrivateDataSlotEXT
--   >     ( VkDevice device
--   >     , const VkPrivateDataSlotCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPrivateDataSlotEXT* pPrivateDataSlot
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreatePrivateDataSlotEXT vkCreatePrivateDataSlotEXT registry at www.khronos.org>
type HS_vkCreatePrivateDataSlotEXT =
     VkDevice -- ^ device
              ->
       Ptr VkPrivateDataSlotCreateInfoEXT -- ^ pCreateInfo
                                          ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkPrivateDataSlotEXT -- ^ pPrivateDataSlot
                                    -> IO VkResult

type PFN_vkCreatePrivateDataSlotEXT =
     FunPtr HS_vkCreatePrivateDataSlotEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCreatePrivateDataSlotEXTUnsafe ::
               PFN_vkCreatePrivateDataSlotEXT -> HS_vkCreatePrivateDataSlotEXT

foreign import ccall safe "dynamic"
               unwrapVkCreatePrivateDataSlotEXTSafe ::
               PFN_vkCreatePrivateDataSlotEXT -> HS_vkCreatePrivateDataSlotEXT

instance VulkanProc "vkCreatePrivateDataSlotEXT" where
    type VkProcType "vkCreatePrivateDataSlotEXT" =
         HS_vkCreatePrivateDataSlotEXT
    vkProcSymbol = _VkCreatePrivateDataSlotEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreatePrivateDataSlotEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreatePrivateDataSlotEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDestroyPrivateDataSlotEXT :: CString

pattern VkDestroyPrivateDataSlotEXT <-
        (is_VkDestroyPrivateDataSlotEXT -> True)
  where
    VkDestroyPrivateDataSlotEXT = _VkDestroyPrivateDataSlotEXT

{-# INLINE _VkDestroyPrivateDataSlotEXT #-}

_VkDestroyPrivateDataSlotEXT :: CString
_VkDestroyPrivateDataSlotEXT
  = Ptr "vkDestroyPrivateDataSlotEXT\NUL"#

{-# INLINE is_VkDestroyPrivateDataSlotEXT #-}

is_VkDestroyPrivateDataSlotEXT :: CString -> Bool
is_VkDestroyPrivateDataSlotEXT
  = (EQ ==) . cmpCStrings _VkDestroyPrivateDataSlotEXT

type VkDestroyPrivateDataSlotEXT = "vkDestroyPrivateDataSlotEXT"

-- | > void vkDestroyPrivateDataSlotEXT
--   >     ( VkDevice device
--   >     , VkPrivateDataSlotEXT privateDataSlot
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkDestroyPrivateDataSlotEXT vkDestroyPrivateDataSlotEXT registry at www.khronos.org>
type HS_vkDestroyPrivateDataSlotEXT =
     VkDevice -- ^ device
              ->
       VkPrivateDataSlotEXT -- ^ privateDataSlot
                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> IO ()

type PFN_vkDestroyPrivateDataSlotEXT =
     FunPtr HS_vkDestroyPrivateDataSlotEXT

foreign import ccall unsafe "dynamic"
               unwrapVkDestroyPrivateDataSlotEXTUnsafe ::
               PFN_vkDestroyPrivateDataSlotEXT -> HS_vkDestroyPrivateDataSlotEXT

foreign import ccall safe "dynamic"
               unwrapVkDestroyPrivateDataSlotEXTSafe ::
               PFN_vkDestroyPrivateDataSlotEXT -> HS_vkDestroyPrivateDataSlotEXT

instance VulkanProc "vkDestroyPrivateDataSlotEXT" where
    type VkProcType "vkDestroyPrivateDataSlotEXT" =
         HS_vkDestroyPrivateDataSlotEXT
    vkProcSymbol = _VkDestroyPrivateDataSlotEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkDestroyPrivateDataSlotEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDestroyPrivateDataSlotEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkSetPrivateDataEXT :: CString

pattern VkSetPrivateDataEXT <- (is_VkSetPrivateDataEXT -> True)
  where
    VkSetPrivateDataEXT = _VkSetPrivateDataEXT

{-# INLINE _VkSetPrivateDataEXT #-}

_VkSetPrivateDataEXT :: CString
_VkSetPrivateDataEXT = Ptr "vkSetPrivateDataEXT\NUL"#

{-# INLINE is_VkSetPrivateDataEXT #-}

is_VkSetPrivateDataEXT :: CString -> Bool
is_VkSetPrivateDataEXT = (EQ ==) . cmpCStrings _VkSetPrivateDataEXT

type VkSetPrivateDataEXT = "vkSetPrivateDataEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkSetPrivateDataEXT
--   >     ( VkDevice device
--   >     , VkObjectType objectType
--   >     , uint64_t objectHandle
--   >     , VkPrivateDataSlotEXT privateDataSlot
--   >     , uint64_t data
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkSetPrivateDataEXT vkSetPrivateDataEXT registry at www.khronos.org>
type HS_vkSetPrivateDataEXT =
     VkDevice -- ^ device
              ->
       VkObjectType -- ^ objectType
                    ->
         Word64 -- ^ objectHandle
                -> VkPrivateDataSlotEXT -- ^ privateDataSlot
                                        -> Word64 -- ^ data
                                                  -> IO VkResult

type PFN_vkSetPrivateDataEXT = FunPtr HS_vkSetPrivateDataEXT

foreign import ccall unsafe "dynamic"
               unwrapVkSetPrivateDataEXTUnsafe ::
               PFN_vkSetPrivateDataEXT -> HS_vkSetPrivateDataEXT

foreign import ccall safe "dynamic" unwrapVkSetPrivateDataEXTSafe
               :: PFN_vkSetPrivateDataEXT -> HS_vkSetPrivateDataEXT

instance VulkanProc "vkSetPrivateDataEXT" where
    type VkProcType "vkSetPrivateDataEXT" = HS_vkSetPrivateDataEXT
    vkProcSymbol = _VkSetPrivateDataEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkSetPrivateDataEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkSetPrivateDataEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPrivateDataEXT :: CString

pattern VkGetPrivateDataEXT <- (is_VkGetPrivateDataEXT -> True)
  where
    VkGetPrivateDataEXT = _VkGetPrivateDataEXT

{-# INLINE _VkGetPrivateDataEXT #-}

_VkGetPrivateDataEXT :: CString
_VkGetPrivateDataEXT = Ptr "vkGetPrivateDataEXT\NUL"#

{-# INLINE is_VkGetPrivateDataEXT #-}

is_VkGetPrivateDataEXT :: CString -> Bool
is_VkGetPrivateDataEXT = (EQ ==) . cmpCStrings _VkGetPrivateDataEXT

type VkGetPrivateDataEXT = "vkGetPrivateDataEXT"

-- | > void vkGetPrivateDataEXT
--   >     ( VkDevice device
--   >     , VkObjectType objectType
--   >     , uint64_t objectHandle
--   >     , VkPrivateDataSlotEXT privateDataSlot
--   >     , uint64_t* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPrivateDataEXT vkGetPrivateDataEXT registry at www.khronos.org>
type HS_vkGetPrivateDataEXT =
     VkDevice -- ^ device
              ->
       VkObjectType -- ^ objectType
                    ->
         Word64 -- ^ objectHandle
                -> VkPrivateDataSlotEXT -- ^ privateDataSlot
                                        -> Ptr Word64 -- ^ pData
                                                      -> IO ()

type PFN_vkGetPrivateDataEXT = FunPtr HS_vkGetPrivateDataEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetPrivateDataEXTUnsafe ::
               PFN_vkGetPrivateDataEXT -> HS_vkGetPrivateDataEXT

foreign import ccall safe "dynamic" unwrapVkGetPrivateDataEXTSafe
               :: PFN_vkGetPrivateDataEXT -> HS_vkGetPrivateDataEXT

instance VulkanProc "vkGetPrivateDataEXT" where
    type VkProcType "vkGetPrivateDataEXT" = HS_vkGetPrivateDataEXT
    vkProcSymbol = _VkGetPrivateDataEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetPrivateDataEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetPrivateDataEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_PRIVATE_DATA_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_PRIVATE_DATA_SPEC_VERSION = 1

type VK_EXT_PRIVATE_DATA_SPEC_VERSION = 1

pattern VK_EXT_PRIVATE_DATA_EXTENSION_NAME :: CString

pattern VK_EXT_PRIVATE_DATA_EXTENSION_NAME <-
        (is_VK_EXT_PRIVATE_DATA_EXTENSION_NAME -> True)
  where
    VK_EXT_PRIVATE_DATA_EXTENSION_NAME
      = _VK_EXT_PRIVATE_DATA_EXTENSION_NAME

{-# INLINE _VK_EXT_PRIVATE_DATA_EXTENSION_NAME #-}

_VK_EXT_PRIVATE_DATA_EXTENSION_NAME :: CString
_VK_EXT_PRIVATE_DATA_EXTENSION_NAME
  = Ptr "VK_EXT_private_data\NUL"#

{-# INLINE is_VK_EXT_PRIVATE_DATA_EXTENSION_NAME #-}

is_VK_EXT_PRIVATE_DATA_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_PRIVATE_DATA_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_PRIVATE_DATA_EXTENSION_NAME

type VK_EXT_PRIVATE_DATA_EXTENSION_NAME = "VK_EXT_private_data"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT
        = VkStructureType 1000295000

pattern VK_STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT =
        VkStructureType 1000295001

pattern VK_STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT =
        VkStructureType 1000295002

pattern VK_OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT :: VkObjectType

pattern VK_OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT =
        VkObjectType 1000295000
