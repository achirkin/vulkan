{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_device_group
       (-- * Vulkan extension: @VK_KHR_device_group@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @61@
        --
        -- Required extensions: 'VK_KHR_device_group_creation'.
        --

        -- ** Required extensions: 'VK_KHR_device_group_creation'.
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupBindSparseInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupCommandBufferBeginInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupRenderPassBeginInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupSubmitInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlagBitsKHR,
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateFlagsInfoKHR,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Enum.VkPeerMemoryFeatureFlagBitsKHR,
        VkGetDeviceGroupPeerMemoryFeaturesKHR,
        pattern VkGetDeviceGroupPeerMemoryFeaturesKHR,
        HS_vkGetDeviceGroupPeerMemoryFeaturesKHR,
        PFN_vkGetDeviceGroupPeerMemoryFeaturesKHR, VkCmdSetDeviceMaskKHR,
        pattern VkCmdSetDeviceMaskKHR, HS_vkCmdSetDeviceMaskKHR,
        PFN_vkCmdSetDeviceMaskKHR, VkCmdDispatchBaseKHR,
        pattern VkCmdDispatchBaseKHR, HS_vkCmdDispatchBaseKHR,
        PFN_vkCmdDispatchBaseKHR, module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkPeerMemoryFeatureFlags,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_DEVICE_GROUP_SPEC_VERSION,
        pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION,
        VK_KHR_DEVICE_GROUP_EXTENSION_NAME,
        pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR,
        pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR,
        pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR,
        pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR,
        pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR,
        pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR,
        pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR,
        pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR,
        pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR,
        -- ** Required extensions: 'VK_KHR_bind_memory2', 'VK_KHR_device_group_creation'.
        module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryDeviceGroupInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemoryDeviceGroupInfoKHR,
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR,
        pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR,
        -- ** Required extensions: 'VK_KHR_surface', 'VK_KHR_device_group_creation'.
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentCapabilitiesKHR,
        module Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        pattern VkGetDeviceGroupPresentCapabilitiesKHR,
        HS_vkGetDeviceGroupPresentCapabilitiesKHR,
        PFN_vkGetDeviceGroupPresentCapabilitiesKHR,
        pattern VkGetDeviceGroupSurfacePresentModesKHR,
        HS_vkGetDeviceGroupSurfacePresentModesKHR,
        PFN_vkGetDeviceGroupSurfacePresentModesKHR,
        pattern VkGetPhysicalDevicePresentRectanglesKHR,
        HS_vkGetPhysicalDevicePresentRectanglesKHR,
        PFN_vkGetPhysicalDevicePresentRectanglesKHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR,
        -- ** Required extensions: 'VK_KHR_swapchain', 'VK_KHR_device_group_creation'.
        module Graphics.Vulkan.Types.Struct.VkAcquireNextImageInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemorySwapchainInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkColorSpaceKHR,
        module Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupSwapchainCreateInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkImageCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Struct.VkImageSwapchainCreateInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkPresentInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkPresentModeKHR,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        module Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR,
        -- > #include "vk_platform.h"
        pattern VkAcquireNextImage2KHR, HS_vkAcquireNextImage2KHR,
        PFN_vkAcquireNextImage2KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR)
       where
import           GHC.Ptr
                                                                                      (Ptr (..))
import           Graphics.Vulkan.Core_1_1
                                                                                      (pattern VK_DEPENDENCY_DEVICE_GROUP_BIT,
                                                                                      pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT,
                                                                                      pattern VK_PIPELINE_CREATE_DISPATCH_BASE,
                                                                                      pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT,
                                                                                      pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO,
                                                                                      pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO,
                                                                                      pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO,
                                                                                      pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO,
                                                                                      pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO,
                                                                                      pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO,
                                                                                      pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO)
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
                                                                                      (HS_vkAcquireNextImage2KHR,
                                                                                      HS_vkGetDeviceGroupPresentCapabilitiesKHR,
                                                                                      HS_vkGetDeviceGroupSurfacePresentModesKHR,
                                                                                      HS_vkGetPhysicalDevicePresentRectanglesKHR,
                                                                                      PFN_vkAcquireNextImage2KHR,
                                                                                      PFN_vkGetDeviceGroupPresentCapabilitiesKHR,
                                                                                      PFN_vkGetDeviceGroupSurfacePresentModesKHR,
                                                                                      PFN_vkGetPhysicalDevicePresentRectanglesKHR,
                                                                                      pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR,
                                                                                      pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR,
                                                                                      pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR,
                                                                                      pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR,
                                                                                      pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR,
                                                                                      pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR,
                                                                                      pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR,
                                                                                      pattern VkAcquireNextImage2KHR,
                                                                                      pattern VkGetDeviceGroupPresentCapabilitiesKHR,
                                                                                      pattern VkGetDeviceGroupSurfacePresentModesKHR,
                                                                                      pattern VkGetPhysicalDevicePresentRectanglesKHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
                                                                                      (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkColorSpaceKHR
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlagBitsKHR
import           Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlags
                                                                                      (VkMemoryAllocateBitmask (..))
import           Graphics.Vulkan.Types.Enum.VkPeerMemoryFeatureFlagBitsKHR
import           Graphics.Vulkan.Types.Enum.VkPeerMemoryFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkPresentModeKHR
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAcquireNextImageInfoKHR
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryDeviceGroupInfoKHR
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryDeviceGroupInfoKHR
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo
import           Graphics.Vulkan.Types.Struct.VkBindImageMemorySwapchainInfoKHR
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupBindSparseInfoKHR
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupCommandBufferBeginInfoKHR
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentCapabilitiesKHR
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentInfoKHR
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupRenderPassBeginInfoKHR
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupSubmitInfoKHR
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupSwapchainCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkImageSwapchainCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateFlagsInfoKHR
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR
import           Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR

pattern VkGetDeviceGroupPeerMemoryFeaturesKHR :: CString

pattern VkGetDeviceGroupPeerMemoryFeaturesKHR <-
        (is_VkGetDeviceGroupPeerMemoryFeaturesKHR -> True)
  where VkGetDeviceGroupPeerMemoryFeaturesKHR
          = _VkGetDeviceGroupPeerMemoryFeaturesKHR

{-# INLINE _VkGetDeviceGroupPeerMemoryFeaturesKHR #-}

_VkGetDeviceGroupPeerMemoryFeaturesKHR :: CString
_VkGetDeviceGroupPeerMemoryFeaturesKHR
  = Ptr "vkGetDeviceGroupPeerMemoryFeaturesKHR\NUL"#

{-# INLINE is_VkGetDeviceGroupPeerMemoryFeaturesKHR #-}

is_VkGetDeviceGroupPeerMemoryFeaturesKHR :: CString -> Bool
is_VkGetDeviceGroupPeerMemoryFeaturesKHR
  = (EQ ==) . cmpCStrings _VkGetDeviceGroupPeerMemoryFeaturesKHR

type VkGetDeviceGroupPeerMemoryFeaturesKHR =
     "vkGetDeviceGroupPeerMemoryFeaturesKHR"

-- | This is an alias for `vkGetDeviceGroupPeerMemoryFeatures`.
--
--   > void vkGetDeviceGroupPeerMemoryFeaturesKHR
--   >     ( VkDevice device
--   >     , uint32_t heapIndex
--   >     , uint32_t localDeviceIndex
--   >     , uint32_t remoteDeviceIndex
--   >     , VkPeerMemoryFeatureFlags* pPeerMemoryFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPeerMemoryFeaturesKHR vkGetDeviceGroupPeerMemoryFeaturesKHR registry at www.khronos.org>
type HS_vkGetDeviceGroupPeerMemoryFeaturesKHR =
     VkDevice -- ^ device
              ->
       Word32 -- ^ heapIndex
              -> Word32 -- ^ localDeviceIndex
                        -> Word32 -- ^ remoteDeviceIndex
                                  -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                  -> IO ()

type PFN_vkGetDeviceGroupPeerMemoryFeaturesKHR =
     FunPtr HS_vkGetDeviceGroupPeerMemoryFeaturesKHR

foreign import ccall "dynamic"
               unwrapVkGetDeviceGroupPeerMemoryFeaturesKHR ::
               PFN_vkGetDeviceGroupPeerMemoryFeaturesKHR ->
                 HS_vkGetDeviceGroupPeerMemoryFeaturesKHR

instance VulkanProc "vkGetDeviceGroupPeerMemoryFeaturesKHR" where
        type VkProcType "vkGetDeviceGroupPeerMemoryFeaturesKHR" =
             HS_vkGetDeviceGroupPeerMemoryFeaturesKHR
        vkProcSymbol = _VkGetDeviceGroupPeerMemoryFeaturesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceGroupPeerMemoryFeaturesKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetDeviceMaskKHR :: CString

pattern VkCmdSetDeviceMaskKHR <- (is_VkCmdSetDeviceMaskKHR -> True)
  where VkCmdSetDeviceMaskKHR = _VkCmdSetDeviceMaskKHR

{-# INLINE _VkCmdSetDeviceMaskKHR #-}

_VkCmdSetDeviceMaskKHR :: CString
_VkCmdSetDeviceMaskKHR = Ptr "vkCmdSetDeviceMaskKHR\NUL"#

{-# INLINE is_VkCmdSetDeviceMaskKHR #-}

is_VkCmdSetDeviceMaskKHR :: CString -> Bool
is_VkCmdSetDeviceMaskKHR
  = (EQ ==) . cmpCStrings _VkCmdSetDeviceMaskKHR

type VkCmdSetDeviceMaskKHR = "vkCmdSetDeviceMaskKHR"

-- | This is an alias for `vkCmdSetDeviceMask`.
--
--   Queues: 'graphics', 'compute', 'transfer'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetDeviceMaskKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t deviceMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDeviceMaskKHR vkCmdSetDeviceMaskKHR registry at www.khronos.org>
type HS_vkCmdSetDeviceMaskKHR = VkCommandBuffer -- ^ commandBuffer
                                                -> Word32 -- ^ deviceMask
                                                          -> IO ()

type PFN_vkCmdSetDeviceMaskKHR = FunPtr HS_vkCmdSetDeviceMaskKHR

foreign import ccall "dynamic" unwrapVkCmdSetDeviceMaskKHR ::
               PFN_vkCmdSetDeviceMaskKHR -> HS_vkCmdSetDeviceMaskKHR

instance VulkanProc "vkCmdSetDeviceMaskKHR" where
        type VkProcType "vkCmdSetDeviceMaskKHR" = HS_vkCmdSetDeviceMaskKHR
        vkProcSymbol = _VkCmdSetDeviceMaskKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetDeviceMaskKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdDispatchBaseKHR :: CString

pattern VkCmdDispatchBaseKHR <- (is_VkCmdDispatchBaseKHR -> True)
  where VkCmdDispatchBaseKHR = _VkCmdDispatchBaseKHR

{-# INLINE _VkCmdDispatchBaseKHR #-}

_VkCmdDispatchBaseKHR :: CString
_VkCmdDispatchBaseKHR = Ptr "vkCmdDispatchBaseKHR\NUL"#

{-# INLINE is_VkCmdDispatchBaseKHR #-}

is_VkCmdDispatchBaseKHR :: CString -> Bool
is_VkCmdDispatchBaseKHR
  = (EQ ==) . cmpCStrings _VkCmdDispatchBaseKHR

type VkCmdDispatchBaseKHR = "vkCmdDispatchBaseKHR"

-- | This is an alias for `vkCmdDispatchBase`.
--
--   Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdDispatchBaseKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t baseGroupX
--   >     , uint32_t baseGroupY
--   >     , uint32_t baseGroupZ
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatchBaseKHR vkCmdDispatchBaseKHR registry at www.khronos.org>
type HS_vkCmdDispatchBaseKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ baseGroupX
              -> Word32 -- ^ baseGroupY
                        -> Word32 -- ^ baseGroupZ
                                  -> Word32 -- ^ groupCountX
                                            -> Word32 -- ^ groupCountY
                                                      -> Word32 -- ^ groupCountZ
                                                                -> IO ()

type PFN_vkCmdDispatchBaseKHR = FunPtr HS_vkCmdDispatchBaseKHR

foreign import ccall "dynamic" unwrapVkCmdDispatchBaseKHR ::
               PFN_vkCmdDispatchBaseKHR -> HS_vkCmdDispatchBaseKHR

instance VulkanProc "vkCmdDispatchBaseKHR" where
        type VkProcType "vkCmdDispatchBaseKHR" = HS_vkCmdDispatchBaseKHR
        vkProcSymbol = _VkCmdDispatchBaseKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDispatchBaseKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION = 3

type VK_KHR_DEVICE_GROUP_SPEC_VERSION = 3

pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME :: CString

pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME <-
        (is_VK_KHR_DEVICE_GROUP_EXTENSION_NAME -> True)
  where VK_KHR_DEVICE_GROUP_EXTENSION_NAME
          = _VK_KHR_DEVICE_GROUP_EXTENSION_NAME

{-# INLINE _VK_KHR_DEVICE_GROUP_EXTENSION_NAME #-}

_VK_KHR_DEVICE_GROUP_EXTENSION_NAME :: CString
_VK_KHR_DEVICE_GROUP_EXTENSION_NAME
  = Ptr "VK_KHR_device_group\NUL"#

{-# INLINE is_VK_KHR_DEVICE_GROUP_EXTENSION_NAME #-}

is_VK_KHR_DEVICE_GROUP_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DEVICE_GROUP_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_DEVICE_GROUP_EXTENSION_NAME

type VK_KHR_DEVICE_GROUP_EXTENSION_NAME = "VK_KHR_device_group"

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR =
        VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR =
        VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR
        = VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR =
        VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR =
        VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO

pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR =
        VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT

pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR =
        VK_PEER_MEMORY_FEATURE_COPY_DST_BIT

pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR =
        VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT

pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR =
        VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT

pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR =
        VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT

pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR =
        VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT

pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR =
        VK_PIPELINE_CREATE_DISPATCH_BASE

pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR =
        VK_DEPENDENCY_DEVICE_GROUP_BIT

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR
        = VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR =
        VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO

pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR =
        VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
