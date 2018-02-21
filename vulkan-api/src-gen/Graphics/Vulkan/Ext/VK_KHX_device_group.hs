{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHX_device_group
       (-- * Vulkan extension: @VK_KHX_device_group@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @KHX@
        --
        -- type: @device@
        --
        -- Extension number: @61@
        --
        -- Required extensions: 'VK_KHX_device_group_creation'.
        --

        -- ** Required extensions: 'VK_KHX_device_group_creation'.
        module Graphics.Vulkan.Types.Struct.VkBindSparseInfo,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkClearColorValue,
        module Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue,
        module Graphics.Vulkan.Types.Struct.VkClearValue,
        module Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo,
        module Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo,
        module Graphics.Vulkan.Types.Enum.VkCommandBufferUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupBindSparseInfoKHX,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupCommandBufferBeginInfoKHX,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupRenderPassBeginInfoKHX,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupSubmitInfoKHX,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Enum.VkImageAspectFlags,
        module Graphics.Vulkan.Types.Struct.VkImageSubresource,
        module Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlagsKHX,
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateFlagsInfoKHX,
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo,
        module Graphics.Vulkan.Types.Struct.VkOffset2D,
        module Graphics.Vulkan.Types.Struct.VkOffset3D,
        module Graphics.Vulkan.Types.Enum.VkPeerMemoryFeatureFlagsKHX,
        module Graphics.Vulkan.Types.Enum.VkPipelineStageFlags,
        module Graphics.Vulkan.Types.Enum.VkQueryControlFlags,
        module Graphics.Vulkan.Types.Enum.VkQueryPipelineStatisticFlags,
        module Graphics.Vulkan.Types.Struct.VkRect2D,
        module Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseBufferMemoryBindInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBind,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBindInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseImageOpaqueMemoryBindInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseMemoryBind,
        module Graphics.Vulkan.Types.Enum.VkSparseMemoryBindFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkSubmitInfo,
        -- > #include "vk_platform.h"
        vkGetDeviceGroupPeerMemoryFeaturesKHX, vkCmdSetDeviceMaskKHX,
        vkCmdDispatchBaseKHX, module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Handles,
        VK_KHX_DEVICE_GROUP_SPEC_VERSION,
        pattern VK_KHX_DEVICE_GROUP_SPEC_VERSION,
        VK_KHX_DEVICE_GROUP_EXTENSION_NAME,
        pattern VK_KHX_DEVICE_GROUP_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX,
        pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX,
        pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHX,
        pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX,
        -- ** Required extensions: 'VK_KHR_bind_memory2', 'VK_KHX_device_group_creation'.
        module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryDeviceGroupInfoKHX,
        module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemoryDeviceGroupInfoKHX,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHX,
        pattern VK_IMAGE_CREATE_BIND_SFR_BIT_KHX,
        -- ** Required extensions: 'VK_KHR_surface', 'VK_KHX_device_group_creation'.
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentCapabilitiesKHX,
        module Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHX,
        -- > #include "vk_platform.h"
        vkGetDeviceGroupPresentCapabilitiesKHX,
        vkGetDeviceGroupSurfacePresentModesKHX,
        vkGetPhysicalDevicePresentRectanglesKHX,
        module Graphics.Vulkan.Types.Enum.VkResult,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX,
        -- ** Required extensions: 'VK_KHR_swapchain', 'VK_KHX_device_group_creation'.
        module Graphics.Vulkan.Types.Struct.VkAcquireNextImageInfoKHX,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemorySwapchainInfoKHX,
        module Graphics.Vulkan.Types.Enum.VkColorSpaceKHR,
        module Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentInfoKHX,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupSwapchainCreateInfoKHX,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkImageCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Struct.VkImageSwapchainCreateInfoKHX,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkPresentInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkPresentModeKHR,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        module Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR,
        -- > #include "vk_platform.h"
        vkAcquireNextImage2KHX,
        pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX,
        pattern VK_SWAPCHAIN_CREATE_BIND_SFR_BIT_KHX)
       where
import           GHC.Ptr
                                                                                      (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkColorSpaceKHR
import           Graphics.Vulkan.Types.Enum.VkCommandBufferUsageFlags
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkDependencyFlags
                                                                                      (VkDependencyBitmask (..),
                                                                                      VkDependencyFlagBits)
import           Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHX
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlagsKHX
import           Graphics.Vulkan.Types.Enum.VkPeerMemoryFeatureFlagsKHX
import           Graphics.Vulkan.Types.Enum.VkPipelineCreateFlags
                                                                                      (VkPipelineCreateBitmask (..),
                                                                                      VkPipelineCreateFlagBits)
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags
import           Graphics.Vulkan.Types.Enum.VkPresentModeKHR
import           Graphics.Vulkan.Types.Enum.VkQueryControlFlags
import           Graphics.Vulkan.Types.Enum.VkQueryPipelineStatisticFlags
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkSparseMemoryBindFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAcquireNextImageInfoKHX
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryDeviceGroupInfoKHX
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfoKHR
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryDeviceGroupInfoKHX
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR
import           Graphics.Vulkan.Types.Struct.VkBindImageMemorySwapchainInfoKHX
import           Graphics.Vulkan.Types.Struct.VkBindSparseInfo
import           Graphics.Vulkan.Types.Struct.VkClearColorValue
import           Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue
import           Graphics.Vulkan.Types.Struct.VkClearValue
import           Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo
import           Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupBindSparseInfoKHX
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupCommandBufferBeginInfoKHX
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentCapabilitiesKHX
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentInfoKHX
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupRenderPassBeginInfoKHX
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupSubmitInfoKHX
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupSwapchainCreateInfoKHX
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkImageSubresource
import           Graphics.Vulkan.Types.Struct.VkImageSwapchainCreateInfoKHX
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateFlagsInfoKHX
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo
import           Graphics.Vulkan.Types.Struct.VkOffset2D
import           Graphics.Vulkan.Types.Struct.VkOffset3D
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR
import           Graphics.Vulkan.Types.Struct.VkRect2D
import           Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo
import           Graphics.Vulkan.Types.Struct.VkSparseBufferMemoryBindInfo
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBind
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBindInfo
import           Graphics.Vulkan.Types.Struct.VkSparseImageOpaqueMemoryBindInfo
import           Graphics.Vulkan.Types.Struct.VkSparseMemoryBind
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo
import           Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR

-- | > () vkGetDeviceGroupPeerMemoryFeaturesKHX
--   >     ( VkDevice device
--   >     , uint32_t heapIndex
--   >     , uint32_t localDeviceIndex
--   >     , uint32_t remoteDeviceIndex
--   >     , VkPeerMemoryFeatureFlagsKHX* pPeerMemoryFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetDeviceGroupPeerMemoryFeaturesKHX.html vkGetDeviceGroupPeerMemoryFeaturesKHX registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceGroupPeerMemoryFeaturesKHX"
               vkGetDeviceGroupPeerMemoryFeaturesKHX ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ heapIndex
                        ->
                   Word32 -- ^ localDeviceIndex
                          -> Word32 -- ^ remoteDeviceIndex
                                    -> Ptr VkPeerMemoryFeatureFlagsKHX -- ^ pPeerMemoryFeatures
                                                                       -> IO ()

-- | queues: 'graphics', 'compute', 'transfer'.
--
--   renderpass: @both@
--
--   > () vkCmdSetDeviceMaskKHX
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t deviceMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCmdSetDeviceMaskKHX.html vkCmdSetDeviceMaskKHX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDeviceMaskKHX"
               vkCmdSetDeviceMaskKHX :: VkCommandBuffer -- ^ commandBuffer
                                                        -> Word32 -- ^ deviceMask
                                                                  -> IO ()

-- | queues: 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdDispatchBaseKHX
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t baseGroupX
--   >     , uint32_t baseGroupY
--   >     , uint32_t baseGroupZ
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCmdDispatchBaseKHX.html vkCmdDispatchBaseKHX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDispatchBaseKHX"
               vkCmdDispatchBaseKHX ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ baseGroupX
                        -> Word32 -- ^ baseGroupY
                                  -> Word32 -- ^ baseGroupZ
                                            -> Word32 -- ^ groupCountX
                                                      -> Word32 -- ^ groupCountY
                                                                -> Word32 -- ^ groupCountZ
                                                                          -> IO ()

pattern VK_KHX_DEVICE_GROUP_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHX_DEVICE_GROUP_SPEC_VERSION = 2

type VK_KHX_DEVICE_GROUP_SPEC_VERSION = 2

pattern VK_KHX_DEVICE_GROUP_EXTENSION_NAME :: CString

pattern VK_KHX_DEVICE_GROUP_EXTENSION_NAME <-
        (is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME -> True)
  where VK_KHX_DEVICE_GROUP_EXTENSION_NAME
          = _VK_KHX_DEVICE_GROUP_EXTENSION_NAME

{-# INLINE _VK_KHX_DEVICE_GROUP_EXTENSION_NAME #-}

_VK_KHX_DEVICE_GROUP_EXTENSION_NAME :: CString
_VK_KHX_DEVICE_GROUP_EXTENSION_NAME
  = Ptr "VK_KHX_device_group\NUL"#

{-# INLINE is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME #-}

is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME :: CString -> Bool
is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME
  = eqCStrings _VK_KHX_DEVICE_GROUP_EXTENSION_NAME

type VK_KHX_DEVICE_GROUP_EXTENSION_NAME = "VK_KHX_device_group"

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX =
        VkStructureType 1000060000

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX =
        VkStructureType 1000060003

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX
        = VkStructureType 1000060004

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX =
        VkStructureType 1000060005

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX =
        VkStructureType 1000060006

pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX =
        VkStructureType 1000060010

-- | bitpos = @3@
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX =
        VkPipelineCreateFlagBits 8

-- | bitpos = @4@
pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHX ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHX =
        VkPipelineCreateFlagBits 16

-- | Dependency is across devices
--
--   bitpos = @2@
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX :: VkDependencyFlagBits

pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX = VkDependencyFlagBits 4

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHX
        = VkStructureType 1000060013

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHX =
        VkStructureType 1000060014

-- | Allows using VkBindImageMemoryDeviceGroupInfoKHX::pSFRRects when binding memory to the image
--
--   bitpos = @6@
pattern VK_IMAGE_CREATE_BIND_SFR_BIT_KHX :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_BIND_SFR_BIT_KHX = VkImageCreateFlagBits 64

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDeviceGroupPresentCapabilitiesKHX
--   >     ( VkDevice device
--   >     , VkDeviceGroupPresentCapabilitiesKHX* pDeviceGroupPresentCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetDeviceGroupPresentCapabilitiesKHX.html vkGetDeviceGroupPresentCapabilitiesKHX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetDeviceGroupPresentCapabilitiesKHX"
               vkGetDeviceGroupPresentCapabilitiesKHX ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceGroupPresentCapabilitiesKHX -- ^ pDeviceGroupPresentCapabilities
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetDeviceGroupSurfacePresentModesKHX
--   >     ( VkDevice device
--   >     , VkSurfaceKHR surface
--   >     , VkDeviceGroupPresentModeFlagsKHX* pModes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetDeviceGroupSurfacePresentModesKHX.html vkGetDeviceGroupSurfacePresentModesKHX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetDeviceGroupSurfacePresentModesKHX"
               vkGetDeviceGroupSurfacePresentModesKHX ::
               VkDevice -- ^ device
                        ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkDeviceGroupPresentModeFlagsKHX -- ^ pModes
                                                                      -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDevicePresentRectanglesKHX
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , uint32_t* pRectCount
--   >     , VkRect2D* pRects
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDevicePresentRectanglesKHX.html vkGetPhysicalDevicePresentRectanglesKHX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDevicePresentRectanglesKHX"
               vkGetPhysicalDevicePresentRectanglesKHX ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pRectCount
                                            -> Ptr VkRect2D -- ^ pRects
                                                            -> IO VkResult

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX =
        VkStructureType 1000060007

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkAcquireNextImage2KHX
--   >     ( VkDevice device
--   >     , const VkAcquireNextImageInfoKHX* pAcquireInfo
--   >     , uint32_t* pImageIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkAcquireNextImage2KHX.html vkAcquireNextImage2KHX registry at www.khronos.org>
foreign import ccall unsafe "vkAcquireNextImage2KHX"
               vkAcquireNextImage2KHX ::
               VkDevice -- ^ device
                        ->
                 Ptr VkAcquireNextImageInfoKHX -- ^ pAcquireInfo
                                               -> Ptr Word32 -- ^ pImageIndex
                                                             -> IO VkResult

pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX =
        VkStructureType 1000060008

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX =
        VkStructureType 1000060009

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX =
        VkStructureType 1000060011

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX =
        VkStructureType 1000060012

-- | Allow images with VK_IMAGE_CREATE_BIND_SFR_BIT_KHX
--
--   bitpos = @0@
pattern VK_SWAPCHAIN_CREATE_BIND_SFR_BIT_KHX ::
        VkSwapchainCreateFlagBitsKHR

pattern VK_SWAPCHAIN_CREATE_BIND_SFR_BIT_KHX =
        VkSwapchainCreateFlagBitsKHR 1
