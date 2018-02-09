#include "vulkan/vulkan.h"

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Common
       (-- * API Constants
        VK_MAX_PHYSICAL_DEVICE_NAME_SIZE,
        pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE, VK_UUID_SIZE,
        pattern VK_UUID_SIZE, VK_LUID_SIZE_KHR, pattern VK_LUID_SIZE_KHR,
        VK_MAX_EXTENSION_NAME_SIZE, pattern VK_MAX_EXTENSION_NAME_SIZE,
        VK_MAX_DESCRIPTION_SIZE, pattern VK_MAX_DESCRIPTION_SIZE,
        VK_MAX_MEMORY_TYPES, pattern VK_MAX_MEMORY_TYPES,
        VK_MAX_MEMORY_HEAPS, pattern VK_MAX_MEMORY_HEAPS,
        pattern VK_LOD_CLAMP_NONE, VK_REMAINING_MIP_LEVELS,
        pattern VK_REMAINING_MIP_LEVELS, VK_REMAINING_ARRAY_LAYERS,
        pattern VK_REMAINING_ARRAY_LAYERS, VK_WHOLE_SIZE,
        pattern VK_WHOLE_SIZE, VK_ATTACHMENT_UNUSED,
        pattern VK_ATTACHMENT_UNUSED, VK_TRUE, pattern VK_TRUE, VK_FALSE,
        pattern VK_FALSE, VK_QUEUE_FAMILY_IGNORED,
        pattern VK_QUEUE_FAMILY_IGNORED, VK_QUEUE_FAMILY_EXTERNAL_KHR,
        pattern VK_QUEUE_FAMILY_EXTERNAL_KHR, VK_QUEUE_FAMILY_FOREIGN_EXT,
        pattern VK_QUEUE_FAMILY_FOREIGN_EXT, VK_SUBPASS_EXTERNAL,
        pattern VK_SUBPASS_EXTERNAL, VK_MAX_DEVICE_GROUP_SIZE_KHX,
        pattern VK_MAX_DEVICE_GROUP_SIZE_KHX, -- * Types and enumerations
                                              --

                                              -- *** WSI extensions
                                              --

                                              -- ** External types
                                              Display, VisualID, Window,
        RROutput, ANativeWindow, MirConnection, MirSurface, WlDisplay,
        WlSurface, HINSTANCE, HWND, HANDLE, SECURITY_ATTRIBUTES, DWORD,
        LPCWSTR, XcbConnectionT, XcbVisualidT, XcbWindowT,
        -- ** Define pragmas
        _VK_MAKE_VERSION, _VK_VERSION_MAJOR, _VK_VERSION_MINOR,
        _VK_VERSION_PATCH, -- | ===== @VK_API_VERSION@
                           -- > // DEPRECATED: This define has been removed. Specific version defines (e.g. VK_API_VERSION_1_0), or the VK_MAKE_VERSION macro, should be used instead.
                           -- > //##define VK_API_VERSION VK_MAKE_VERSION(1, 0, 0) // Patch version should always be set to 0
                           VK_API_VERSION_1_0, pattern VK_API_VERSION_1_0,
        VK_HEADER_VERSION, pattern VK_HEADER_VERSION, Ptr(), -- | ===== @VK_DEFINE_HANDLE@
                                                             -- Dispatchable handles are represented as `Foreign.Ptr`
                                                             --
                                                             -- >
                                                             -- > ##define VK_DEFINE_HANDLE(object) typedef struct object####_T* object;
                                                             VkPtr(..),
        -- | ===== @VK_DEFINE_NON_DISPATCHABLE_HANDLE@
        -- Non-dispatchable handles are represented as `VkPtr`
        --
        -- >
        -- > ##if !defined(VK_DEFINE_NON_DISPATCHABLE_HANDLE)
        -- > ##if defined(__LP64__) || defined(_WIN64) || (defined(__x86_64__) && !defined(__ILP32__) ) || defined(_M_X64) || defined(__ia64) || defined (_M_IA64) || defined(__aarch64__) || defined(__powerpc64__)
        -- >         ##define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef struct object####_T *object;
        -- > ##else
        -- >         ##define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef uint64_t object;
        -- > ##endif
        -- > ##endif
        -- >
        VulkanPtr(..), pattern VK_NULL_HANDLE, -- ** Base types
                                               VkSampleMask(..),
        VkBool32(..), VkFlags(..), VkDeviceSize(..), -- ** External types
                                                     --

                                                     -- *** Basic C types, pulled in via vk_platform.h
                                                     CChar, Word8, Word32,
        Word64, Int32, -- ** Bitmasks
                       --

                       -- *** Bitmask types
                       VkFramebufferCreateFlags(..),
        VkQueryPoolCreateFlags(..), VkRenderPassCreateFlags(..),
        VkSamplerCreateFlags(..), VkPipelineLayoutCreateFlags(..),
        VkPipelineCacheCreateFlags(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineShaderStageCreateFlags(..),
        VkDescriptorSetLayoutCreateFlags, VkBufferViewCreateFlags(..),
        VkInstanceCreateFlags(..), VkDeviceCreateFlags(..),
        VkDeviceQueueCreateFlags(..), VkQueueFlags, VkMemoryPropertyFlags,
        VkMemoryHeapFlags, VkAccessFlags, VkBufferUsageFlags,
        VkBufferCreateFlags, VkShaderStageFlags, VkImageUsageFlags,
        VkImageCreateFlags, VkImageViewCreateFlags(..),
        VkPipelineCreateFlags, VkColorComponentFlags, VkFenceCreateFlags,
        VkSemaphoreCreateFlags(..), VkFormatFeatureFlags,
        VkQueryControlFlags, VkQueryResultFlags,
        VkShaderModuleCreateFlags(..), VkEventCreateFlags(..),
        VkCommandPoolCreateFlags, VkCommandPoolResetFlags,
        VkCommandBufferResetFlags, VkCommandBufferUsageFlags,
        VkQueryPipelineStatisticFlags, VkMemoryMapFlags(..),
        VkImageAspectFlags, VkSparseMemoryBindFlags,
        VkSparseImageFormatFlags, VkSubpassDescriptionFlags,
        VkPipelineStageFlags, VkSampleCountFlags,
        VkAttachmentDescriptionFlags, VkStencilFaceFlags, VkCullModeFlags,
        VkDescriptorPoolCreateFlags, VkDescriptorPoolResetFlags(..),
        VkDependencyFlags, VkIndirectCommandsLayoutUsageFlagsNVX,
        VkObjectEntryUsageFlagsNVX,
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        -- *** WSI extensions
        VkCompositeAlphaFlagsKHR, VkDisplayPlaneAlphaFlagsKHR,
        VkSurfaceTransformFlagsKHR, VkSwapchainCreateFlagsKHR,
        VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..),
        VkAndroidSurfaceCreateFlagsKHR(..), VkMirSurfaceCreateFlagsKHR(..),
        VkViSurfaceCreateFlagsNN(..), VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXlibSurfaceCreateFlagsKHR(..),
        VkXcbSurfaceCreateFlagsKHR(..), VkIOSSurfaceCreateFlagsMVK(..),
        VkMacOSSurfaceCreateFlagsMVK(..), VkPeerMemoryFeatureFlagsKHX,
        VkMemoryAllocateFlagsKHX, VkDeviceGroupPresentModeFlagsKHX,
        VkDebugReportFlagsEXT, VkCommandPoolTrimFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsNV, VkExternalMemoryFeatureFlagsNV,
        VkExternalMemoryHandleTypeFlagsKHR,
        VkExternalMemoryFeatureFlagsKHR,
        VkExternalSemaphoreHandleTypeFlagsKHR,
        VkExternalSemaphoreFeatureFlagsKHR, VkSemaphoreImportFlagsKHR,
        VkExternalFenceHandleTypeFlagsKHR, VkExternalFenceFeatureFlagsKHR,
        VkFenceImportFlagsKHR, VkSurfaceCounterFlagsEXT,
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkValidationCacheCreateFlagsEXT(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        -- ** Handles
        --

        -- *** Types which can be void pointers or class pointers, selected at compile time
        VkInstance, VkInstance_T(), VkPhysicalDevice, VkPhysicalDevice_T(),
        VkDevice, VkDevice_T(), VkQueue, VkQueue_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkDeviceMemory, VkDeviceMemory_T(),
        VkCommandPool, VkCommandPool_T(), VkBuffer, VkBuffer_T(),
        VkBufferView, VkBufferView_T(), VkImage, VkImage_T(), VkImageView,
        VkImageView_T(), VkShaderModule, VkShaderModule_T(), VkPipeline,
        VkPipeline_T(), VkPipelineLayout, VkPipelineLayout_T(), VkSampler,
        VkSampler_T(), VkDescriptorSet, VkDescriptorSet_T(),
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(), VkDescriptorPool,
        VkDescriptorPool_T(), VkFence, VkFence_T(), VkSemaphore,
        VkSemaphore_T(), VkEvent, VkEvent_T(), VkQueryPool,
        VkQueryPool_T(), VkFramebuffer, VkFramebuffer_T(), VkRenderPass,
        VkRenderPass_T(), VkPipelineCache, VkPipelineCache_T(),
        VkObjectTableNVX, VkObjectTableNVX_T(),
        VkIndirectCommandsLayoutNVX, VkIndirectCommandsLayoutNVX_T(),
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkSamplerYcbcrConversionKHR, VkSamplerYcbcrConversionKHR_T(),
        VkValidationCacheEXT, VkValidationCacheEXT_T(), -- *** WSI extensions
                                                        VkDisplayKHR,
        VkDisplayKHR_T(), VkDisplayModeKHR, VkDisplayModeKHR_T(),
        VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR, VkSwapchainKHR_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        -- ** Enums
        --

        -- *** Types generated from corresponding enums tags below
        VkAttachmentLoadOp(..), pattern VK_ATTACHMENT_LOAD_OP_LOAD,
        pattern VK_ATTACHMENT_LOAD_OP_CLEAR,
        pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE, VkAttachmentStoreOp(..),
        pattern VK_ATTACHMENT_STORE_OP_STORE,
        pattern VK_ATTACHMENT_STORE_OP_DONT_CARE, VkBlendFactor(..),
        pattern VK_BLEND_FACTOR_ZERO, pattern VK_BLEND_FACTOR_ONE,
        pattern VK_BLEND_FACTOR_SRC_COLOR,
        pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR,
        pattern VK_BLEND_FACTOR_DST_COLOR,
        pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR,
        pattern VK_BLEND_FACTOR_SRC_ALPHA,
        pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
        pattern VK_BLEND_FACTOR_DST_ALPHA,
        pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA,
        pattern VK_BLEND_FACTOR_CONSTANT_COLOR,
        pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR,
        pattern VK_BLEND_FACTOR_CONSTANT_ALPHA,
        pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA,
        pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE,
        pattern VK_BLEND_FACTOR_SRC1_COLOR,
        pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR,
        pattern VK_BLEND_FACTOR_SRC1_ALPHA,
        pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA, VkBlendOp(..),
        pattern VK_BLEND_OP_ADD, pattern VK_BLEND_OP_SUBTRACT,
        pattern VK_BLEND_OP_REVERSE_SUBTRACT, pattern VK_BLEND_OP_MIN,
        pattern VK_BLEND_OP_MAX, VkBorderColor(..),
        pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK,
        pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK,
        pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
        pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK,
        pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
        pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE,
        VkFramebufferCreateFlagBits(..), VkQueryPoolCreateFlagBits(..),
        VkRenderPassCreateFlagBits(..), VkSamplerCreateFlagBits(..),
        VkPipelineCacheHeaderVersion(..),
        pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE,
        VkPipelineLayoutCreateFlagBits(..),
        VkPipelineCacheCreateFlagBits(..),
        VkPipelineDepthStencilStateCreateFlagBits(..),
        VkPipelineDynamicStateCreateFlagBits(..),
        VkPipelineColorBlendStateCreateFlagBits(..),
        VkPipelineMultisampleStateCreateFlagBits(..),
        VkPipelineRasterizationStateCreateFlagBits(..),
        VkPipelineViewportStateCreateFlagBits(..),
        VkPipelineTessellationStateCreateFlagBits(..),
        VkPipelineInputAssemblyStateCreateFlagBits(..),
        VkPipelineVertexInputStateCreateFlagBits(..),
        VkPipelineShaderStageCreateFlagBits(..),
        VkDescriptorSetLayoutCreateFlagBits(..),
        VkBufferViewCreateFlagBits(..), VkInstanceCreateFlagBits(..),
        VkDeviceQueueCreateFlagBits(..), VkBufferCreateFlagBits(..),
        pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT,
        pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT,
        pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT,
        VkBufferUsageFlagBits(..),
        pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT,
        pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT,
        pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
        pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
        pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
        pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
        pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT,
        VkColorComponentFlagBits(..), pattern VK_COLOR_COMPONENT_R_BIT,
        pattern VK_COLOR_COMPONENT_G_BIT, pattern VK_COLOR_COMPONENT_B_BIT,
        pattern VK_COLOR_COMPONENT_A_BIT, VkComponentSwizzle(..),
        pattern VK_COMPONENT_SWIZZLE_IDENTITY,
        pattern VK_COMPONENT_SWIZZLE_ZERO,
        pattern VK_COMPONENT_SWIZZLE_ONE, pattern VK_COMPONENT_SWIZZLE_R,
        pattern VK_COMPONENT_SWIZZLE_G, pattern VK_COMPONENT_SWIZZLE_B,
        pattern VK_COMPONENT_SWIZZLE_A, VkCommandPoolCreateFlagBits(..),
        pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT,
        pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
        VkCommandPoolResetFlagBits(..),
        pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT,
        VkCommandBufferResetFlagBits(..),
        pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT,
        VkCommandBufferLevel(..), pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY,
        VkCommandBufferUsageFlagBits(..),
        pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT,
        pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT,
        VkCompareOp(..), pattern VK_COMPARE_OP_NEVER,
        pattern VK_COMPARE_OP_LESS, pattern VK_COMPARE_OP_EQUAL,
        pattern VK_COMPARE_OP_LESS_OR_EQUAL, pattern VK_COMPARE_OP_GREATER,
        pattern VK_COMPARE_OP_NOT_EQUAL,
        pattern VK_COMPARE_OP_GREATER_OR_EQUAL,
        pattern VK_COMPARE_OP_ALWAYS, VkCullModeFlagBits(..),
        pattern VK_CULL_MODE_NONE, pattern VK_CULL_MODE_FRONT_BIT,
        pattern VK_CULL_MODE_BACK_BIT, pattern VK_CULL_MODE_FRONT_AND_BACK,
        VkDescriptorType(..), pattern VK_DESCRIPTOR_TYPE_SAMPLER,
        pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
        pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,
        pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
        pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER,
        pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
        pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
        pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
        pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC,
        pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
        VkDeviceCreateFlagBits(..), VkDynamicState(..),
        pattern VK_DYNAMIC_STATE_VIEWPORT,
        pattern VK_DYNAMIC_STATE_SCISSOR,
        pattern VK_DYNAMIC_STATE_LINE_WIDTH,
        pattern VK_DYNAMIC_STATE_DEPTH_BIAS,
        pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS,
        pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS,
        pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK,
        pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK,
        pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE,
        VkFenceCreateFlagBits(..), pattern VK_FENCE_CREATE_SIGNALED_BIT,
        VkPolygonMode(..), pattern VK_POLYGON_MODE_FILL,
        pattern VK_POLYGON_MODE_LINE, pattern VK_POLYGON_MODE_POINT,
        VkFormat(..), pattern VK_FORMAT_UNDEFINED,
        pattern VK_FORMAT_R4G4_UNORM_PACK8,
        pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16,
        pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16,
        pattern VK_FORMAT_R5G6B5_UNORM_PACK16,
        pattern VK_FORMAT_B5G6R5_UNORM_PACK16,
        pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16,
        pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16,
        pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16,
        pattern VK_FORMAT_R8_UNORM, pattern VK_FORMAT_R8_SNORM,
        pattern VK_FORMAT_R8_USCALED, pattern VK_FORMAT_R8_SSCALED,
        pattern VK_FORMAT_R8_UINT, pattern VK_FORMAT_R8_SINT,
        pattern VK_FORMAT_R8_SRGB, pattern VK_FORMAT_R8G8_UNORM,
        pattern VK_FORMAT_R8G8_SNORM, pattern VK_FORMAT_R8G8_USCALED,
        pattern VK_FORMAT_R8G8_SSCALED, pattern VK_FORMAT_R8G8_UINT,
        pattern VK_FORMAT_R8G8_SINT, pattern VK_FORMAT_R8G8_SRGB,
        pattern VK_FORMAT_R8G8B8_UNORM, pattern VK_FORMAT_R8G8B8_SNORM,
        pattern VK_FORMAT_R8G8B8_USCALED, pattern VK_FORMAT_R8G8B8_SSCALED,
        pattern VK_FORMAT_R8G8B8_UINT, pattern VK_FORMAT_R8G8B8_SINT,
        pattern VK_FORMAT_R8G8B8_SRGB, pattern VK_FORMAT_B8G8R8_UNORM,
        pattern VK_FORMAT_B8G8R8_SNORM, pattern VK_FORMAT_B8G8R8_USCALED,
        pattern VK_FORMAT_B8G8R8_SSCALED, pattern VK_FORMAT_B8G8R8_UINT,
        pattern VK_FORMAT_B8G8R8_SINT, pattern VK_FORMAT_B8G8R8_SRGB,
        pattern VK_FORMAT_R8G8B8A8_UNORM, pattern VK_FORMAT_R8G8B8A8_SNORM,
        pattern VK_FORMAT_R8G8B8A8_USCALED,
        pattern VK_FORMAT_R8G8B8A8_SSCALED,
        pattern VK_FORMAT_R8G8B8A8_UINT, pattern VK_FORMAT_R8G8B8A8_SINT,
        pattern VK_FORMAT_R8G8B8A8_SRGB, pattern VK_FORMAT_B8G8R8A8_UNORM,
        pattern VK_FORMAT_B8G8R8A8_SNORM,
        pattern VK_FORMAT_B8G8R8A8_USCALED,
        pattern VK_FORMAT_B8G8R8A8_SSCALED,
        pattern VK_FORMAT_B8G8R8A8_UINT, pattern VK_FORMAT_B8G8R8A8_SINT,
        pattern VK_FORMAT_B8G8R8A8_SRGB,
        pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32,
        pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32,
        pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32,
        pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32,
        pattern VK_FORMAT_A8B8G8R8_UINT_PACK32,
        pattern VK_FORMAT_A8B8G8R8_SINT_PACK32,
        pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32,
        pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32,
        pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32,
        pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32,
        pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32,
        pattern VK_FORMAT_A2R10G10B10_UINT_PACK32,
        pattern VK_FORMAT_A2R10G10B10_SINT_PACK32,
        pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32,
        pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32,
        pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32,
        pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32,
        pattern VK_FORMAT_A2B10G10R10_UINT_PACK32,
        pattern VK_FORMAT_A2B10G10R10_SINT_PACK32,
        pattern VK_FORMAT_R16_UNORM, pattern VK_FORMAT_R16_SNORM,
        pattern VK_FORMAT_R16_USCALED, pattern VK_FORMAT_R16_SSCALED,
        pattern VK_FORMAT_R16_UINT, pattern VK_FORMAT_R16_SINT,
        pattern VK_FORMAT_R16_SFLOAT, pattern VK_FORMAT_R16G16_UNORM,
        pattern VK_FORMAT_R16G16_SNORM, pattern VK_FORMAT_R16G16_USCALED,
        pattern VK_FORMAT_R16G16_SSCALED, pattern VK_FORMAT_R16G16_UINT,
        pattern VK_FORMAT_R16G16_SINT, pattern VK_FORMAT_R16G16_SFLOAT,
        pattern VK_FORMAT_R16G16B16_UNORM,
        pattern VK_FORMAT_R16G16B16_SNORM,
        pattern VK_FORMAT_R16G16B16_USCALED,
        pattern VK_FORMAT_R16G16B16_SSCALED,
        pattern VK_FORMAT_R16G16B16_UINT, pattern VK_FORMAT_R16G16B16_SINT,
        pattern VK_FORMAT_R16G16B16_SFLOAT,
        pattern VK_FORMAT_R16G16B16A16_UNORM,
        pattern VK_FORMAT_R16G16B16A16_SNORM,
        pattern VK_FORMAT_R16G16B16A16_USCALED,
        pattern VK_FORMAT_R16G16B16A16_SSCALED,
        pattern VK_FORMAT_R16G16B16A16_UINT,
        pattern VK_FORMAT_R16G16B16A16_SINT,
        pattern VK_FORMAT_R16G16B16A16_SFLOAT, pattern VK_FORMAT_R32_UINT,
        pattern VK_FORMAT_R32_SINT, pattern VK_FORMAT_R32_SFLOAT,
        pattern VK_FORMAT_R32G32_UINT, pattern VK_FORMAT_R32G32_SINT,
        pattern VK_FORMAT_R32G32_SFLOAT, pattern VK_FORMAT_R32G32B32_UINT,
        pattern VK_FORMAT_R32G32B32_SINT,
        pattern VK_FORMAT_R32G32B32_SFLOAT,
        pattern VK_FORMAT_R32G32B32A32_UINT,
        pattern VK_FORMAT_R32G32B32A32_SINT,
        pattern VK_FORMAT_R32G32B32A32_SFLOAT, pattern VK_FORMAT_R64_UINT,
        pattern VK_FORMAT_R64_SINT, pattern VK_FORMAT_R64_SFLOAT,
        pattern VK_FORMAT_R64G64_UINT, pattern VK_FORMAT_R64G64_SINT,
        pattern VK_FORMAT_R64G64_SFLOAT, pattern VK_FORMAT_R64G64B64_UINT,
        pattern VK_FORMAT_R64G64B64_SINT,
        pattern VK_FORMAT_R64G64B64_SFLOAT,
        pattern VK_FORMAT_R64G64B64A64_UINT,
        pattern VK_FORMAT_R64G64B64A64_SINT,
        pattern VK_FORMAT_R64G64B64A64_SFLOAT,
        pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32,
        pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32,
        pattern VK_FORMAT_D16_UNORM, pattern VK_FORMAT_X8_D24_UNORM_PACK32,
        pattern VK_FORMAT_D32_SFLOAT, pattern VK_FORMAT_S8_UINT,
        pattern VK_FORMAT_D16_UNORM_S8_UINT,
        pattern VK_FORMAT_D24_UNORM_S8_UINT,
        pattern VK_FORMAT_D32_SFLOAT_S8_UINT,
        pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK,
        pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK,
        pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK,
        pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK,
        pattern VK_FORMAT_BC2_UNORM_BLOCK,
        pattern VK_FORMAT_BC2_SRGB_BLOCK,
        pattern VK_FORMAT_BC3_UNORM_BLOCK,
        pattern VK_FORMAT_BC3_SRGB_BLOCK,
        pattern VK_FORMAT_BC4_UNORM_BLOCK,
        pattern VK_FORMAT_BC4_SNORM_BLOCK,
        pattern VK_FORMAT_BC5_UNORM_BLOCK,
        pattern VK_FORMAT_BC5_SNORM_BLOCK,
        pattern VK_FORMAT_BC6H_UFLOAT_BLOCK,
        pattern VK_FORMAT_BC6H_SFLOAT_BLOCK,
        pattern VK_FORMAT_BC7_UNORM_BLOCK,
        pattern VK_FORMAT_BC7_SRGB_BLOCK,
        pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK,
        pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK,
        pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK,
        pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK,
        pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK,
        pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK,
        pattern VK_FORMAT_EAC_R11_UNORM_BLOCK,
        pattern VK_FORMAT_EAC_R11_SNORM_BLOCK,
        pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK,
        pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK,
        pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK,
        pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK,
        pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK,
        VkFormatFeatureFlagBits(..),
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT,
        pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT,
        pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT,
        pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT,
        pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT,
        pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT,
        pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT,
        pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT,
        pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT,
        pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT,
        pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT,
        pattern VK_FORMAT_FEATURE_BLIT_DST_BIT,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT,
        VkFrontFace(..), pattern VK_FRONT_FACE_COUNTER_CLOCKWISE,
        pattern VK_FRONT_FACE_CLOCKWISE, VkImageAspectFlagBits(..),
        pattern VK_IMAGE_ASPECT_COLOR_BIT,
        pattern VK_IMAGE_ASPECT_DEPTH_BIT,
        pattern VK_IMAGE_ASPECT_STENCIL_BIT,
        pattern VK_IMAGE_ASPECT_METADATA_BIT, VkImageCreateFlagBits(..),
        pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT,
        pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT,
        pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT,
        pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT,
        pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT, VkImageLayout(..),
        pattern VK_IMAGE_LAYOUT_UNDEFINED, pattern VK_IMAGE_LAYOUT_GENERAL,
        pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_PREINITIALIZED, VkImageTiling(..),
        pattern VK_IMAGE_TILING_OPTIMAL, pattern VK_IMAGE_TILING_LINEAR,
        VkImageType(..), pattern VK_IMAGE_TYPE_1D,
        pattern VK_IMAGE_TYPE_2D, pattern VK_IMAGE_TYPE_3D,
        VkImageUsageFlagBits(..), pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT,
        pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT,
        pattern VK_IMAGE_USAGE_SAMPLED_BIT,
        pattern VK_IMAGE_USAGE_STORAGE_BIT,
        pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
        pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
        pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT,
        pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT, VkImageViewType(..),
        pattern VK_IMAGE_VIEW_TYPE_1D, pattern VK_IMAGE_VIEW_TYPE_2D,
        pattern VK_IMAGE_VIEW_TYPE_3D, pattern VK_IMAGE_VIEW_TYPE_CUBE,
        pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY,
        pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY,
        pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY, VkSharingMode(..),
        pattern VK_SHARING_MODE_EXCLUSIVE,
        pattern VK_SHARING_MODE_CONCURRENT, VkIndexType(..),
        pattern VK_INDEX_TYPE_UINT16, pattern VK_INDEX_TYPE_UINT32,
        VkLogicOp(..), pattern VK_LOGIC_OP_CLEAR, pattern VK_LOGIC_OP_AND,
        pattern VK_LOGIC_OP_AND_REVERSE, pattern VK_LOGIC_OP_COPY,
        pattern VK_LOGIC_OP_AND_INVERTED, pattern VK_LOGIC_OP_NO_OP,
        pattern VK_LOGIC_OP_XOR, pattern VK_LOGIC_OP_OR,
        pattern VK_LOGIC_OP_NOR, pattern VK_LOGIC_OP_EQUIVALENT,
        pattern VK_LOGIC_OP_INVERT, pattern VK_LOGIC_OP_OR_REVERSE,
        pattern VK_LOGIC_OP_COPY_INVERTED, pattern VK_LOGIC_OP_OR_INVERTED,
        pattern VK_LOGIC_OP_NAND, pattern VK_LOGIC_OP_SET,
        VkMemoryHeapFlagBits(..), pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT,
        VkAccessFlagBits(..), pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT,
        pattern VK_ACCESS_INDEX_READ_BIT,
        pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT,
        pattern VK_ACCESS_UNIFORM_READ_BIT,
        pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT,
        pattern VK_ACCESS_SHADER_READ_BIT,
        pattern VK_ACCESS_SHADER_WRITE_BIT,
        pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT,
        pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT,
        pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT,
        pattern VK_ACCESS_TRANSFER_READ_BIT,
        pattern VK_ACCESS_TRANSFER_WRITE_BIT,
        pattern VK_ACCESS_HOST_READ_BIT, pattern VK_ACCESS_HOST_WRITE_BIT,
        pattern VK_ACCESS_MEMORY_READ_BIT,
        pattern VK_ACCESS_MEMORY_WRITE_BIT, VkMemoryPropertyFlagBits(..),
        pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
        pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT,
        pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT,
        pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT,
        VkPhysicalDeviceType(..), pattern VK_PHYSICAL_DEVICE_TYPE_OTHER,
        pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU,
        pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU,
        pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU,
        pattern VK_PHYSICAL_DEVICE_TYPE_CPU, VkPipelineBindPoint(..),
        pattern VK_PIPELINE_BIND_POINT_GRAPHICS,
        pattern VK_PIPELINE_BIND_POINT_COMPUTE,
        VkPipelineCreateFlagBits(..),
        pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT,
        pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT,
        pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT, VkPrimitiveTopology(..),
        pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST,
        pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST,
        pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP,
        pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
        pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
        pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN,
        pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY,
        pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY,
        pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY,
        pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY,
        pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST,
        VkQueryControlFlagBits(..), pattern VK_QUERY_CONTROL_PRECISE_BIT,
        VkQueryPipelineStatisticFlagBits(..),
        pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT,
        pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT,
        pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT,
        pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT,
        pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT,
        pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT,
        pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT,
        pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT,
        pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT,
        pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT,
        pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT,
        VkQueryResultFlagBits(..), pattern VK_QUERY_RESULT_64_BIT,
        pattern VK_QUERY_RESULT_WAIT_BIT,
        pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT,
        pattern VK_QUERY_RESULT_PARTIAL_BIT, VkQueryType(..),
        pattern VK_QUERY_TYPE_OCCLUSION,
        pattern VK_QUERY_TYPE_PIPELINE_STATISTICS,
        pattern VK_QUERY_TYPE_TIMESTAMP, VkQueueFlagBits(..),
        pattern VK_QUEUE_GRAPHICS_BIT, pattern VK_QUEUE_COMPUTE_BIT,
        pattern VK_QUEUE_TRANSFER_BIT, pattern VK_QUEUE_SPARSE_BINDING_BIT,
        VkSubpassContents(..), pattern VK_SUBPASS_CONTENTS_INLINE,
        pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS,
        VkResult(..), -- ** Return codes (positive values)
                      pattern VK_SUCCESS, pattern VK_NOT_READY,
        pattern VK_TIMEOUT, pattern VK_EVENT_SET, pattern VK_EVENT_RESET,
        pattern VK_INCOMPLETE, -- ** Error codes (negative values)
                               pattern VK_ERROR_OUT_OF_HOST_MEMORY,
        pattern VK_ERROR_OUT_OF_DEVICE_MEMORY,
        pattern VK_ERROR_INITIALIZATION_FAILED,
        pattern VK_ERROR_DEVICE_LOST, pattern VK_ERROR_MEMORY_MAP_FAILED,
        pattern VK_ERROR_LAYER_NOT_PRESENT,
        pattern VK_ERROR_EXTENSION_NOT_PRESENT,
        pattern VK_ERROR_FEATURE_NOT_PRESENT,
        pattern VK_ERROR_INCOMPATIBLE_DRIVER,
        pattern VK_ERROR_TOO_MANY_OBJECTS,
        pattern VK_ERROR_FORMAT_NOT_SUPPORTED,
        pattern VK_ERROR_FRAGMENTED_POOL, VkShaderStageFlagBits(..),
        pattern VK_SHADER_STAGE_VERTEX_BIT,
        pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT,
        pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT,
        pattern VK_SHADER_STAGE_GEOMETRY_BIT,
        pattern VK_SHADER_STAGE_FRAGMENT_BIT,
        pattern VK_SHADER_STAGE_COMPUTE_BIT,
        pattern VK_SHADER_STAGE_ALL_GRAPHICS, pattern VK_SHADER_STAGE_ALL,
        VkSparseMemoryBindFlagBits(..),
        pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT,
        VkStencilFaceFlagBits(..), pattern VK_STENCIL_FACE_FRONT_BIT,
        pattern VK_STENCIL_FACE_BACK_BIT,
        pattern VK_STENCIL_FRONT_AND_BACK, VkStencilOp(..),
        pattern VK_STENCIL_OP_KEEP, pattern VK_STENCIL_OP_ZERO,
        pattern VK_STENCIL_OP_REPLACE,
        pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP,
        pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP,
        pattern VK_STENCIL_OP_INVERT,
        pattern VK_STENCIL_OP_INCREMENT_AND_WRAP,
        pattern VK_STENCIL_OP_DECREMENT_AND_WRAP, VkStructureType(..),
        pattern VK_STRUCTURE_TYPE_APPLICATION_INFO,
        pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_SUBMIT_INFO,
        pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE,
        pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO,
        pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
        pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET,
        pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO,
        pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,
        pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
        pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
        pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER,
        pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO,
        VkSystemAllocationScope(..),
        pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND,
        pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT,
        pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE,
        pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE,
        pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE,
        VkInternalAllocationType(..),
        pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE,
        VkSamplerAddressMode(..), pattern VK_SAMPLER_ADDRESS_MODE_REPEAT,
        pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT,
        pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
        pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER, -- value="4" reserved for VK_KHR_sampler_mirror_clamp_to_edge
                                                         --                 enum VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE; do not
                                                         --                 alias!
                                                         VkFilter(..),
        pattern VK_FILTER_NEAREST, pattern VK_FILTER_LINEAR,
        VkSamplerMipmapMode(..), pattern VK_SAMPLER_MIPMAP_MODE_NEAREST,
        pattern VK_SAMPLER_MIPMAP_MODE_LINEAR, VkVertexInputRate(..),
        pattern VK_VERTEX_INPUT_RATE_VERTEX,
        pattern VK_VERTEX_INPUT_RATE_INSTANCE, VkPipelineStageFlagBits(..),
        pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
        pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT,
        pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT,
        pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT,
        pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT,
        pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT,
        pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT,
        pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
        pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT,
        pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT,
        pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        pattern VK_PIPELINE_STAGE_TRANSFER_BIT,
        pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT,
        pattern VK_PIPELINE_STAGE_HOST_BIT,
        pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT,
        pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
        VkSparseImageFormatFlagBits(..),
        pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT,
        pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT,
        pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT,
        VkSampleCountFlagBits(..), pattern VK_SAMPLE_COUNT_1_BIT,
        pattern VK_SAMPLE_COUNT_2_BIT, pattern VK_SAMPLE_COUNT_4_BIT,
        pattern VK_SAMPLE_COUNT_8_BIT, pattern VK_SAMPLE_COUNT_16_BIT,
        pattern VK_SAMPLE_COUNT_32_BIT, pattern VK_SAMPLE_COUNT_64_BIT,
        VkAttachmentDescriptionFlagBits(..),
        pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT,
        VkDescriptorPoolCreateFlagBits(..),
        pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT,
        VkDependencyFlagBits(..), pattern VK_DEPENDENCY_BY_REGION_BIT,
        VkObjectType(..), pattern VK_OBJECT_TYPE_UNKNOWN,
        pattern VK_OBJECT_TYPE_INSTANCE,
        pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE,
        pattern VK_OBJECT_TYPE_DEVICE, pattern VK_OBJECT_TYPE_QUEUE,
        pattern VK_OBJECT_TYPE_SEMAPHORE,
        pattern VK_OBJECT_TYPE_COMMAND_BUFFER,
        pattern VK_OBJECT_TYPE_FENCE, pattern VK_OBJECT_TYPE_DEVICE_MEMORY,
        pattern VK_OBJECT_TYPE_BUFFER, pattern VK_OBJECT_TYPE_IMAGE,
        pattern VK_OBJECT_TYPE_EVENT, pattern VK_OBJECT_TYPE_QUERY_POOL,
        pattern VK_OBJECT_TYPE_BUFFER_VIEW,
        pattern VK_OBJECT_TYPE_IMAGE_VIEW,
        pattern VK_OBJECT_TYPE_SHADER_MODULE,
        pattern VK_OBJECT_TYPE_PIPELINE_CACHE,
        pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT,
        pattern VK_OBJECT_TYPE_RENDER_PASS,
        pattern VK_OBJECT_TYPE_PIPELINE,
        pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,
        pattern VK_OBJECT_TYPE_SAMPLER,
        pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL,
        pattern VK_OBJECT_TYPE_DESCRIPTOR_SET,
        pattern VK_OBJECT_TYPE_FRAMEBUFFER,
        pattern VK_OBJECT_TYPE_COMMAND_POOL,
        -- *** Extensions
        VkIndirectCommandsLayoutUsageFlagBitsNVX(..),
        pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX,
        pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX,
        pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX,
        pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX,
        VkIndirectCommandsTokenTypeNVX(..),
        pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX,
        pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX,
        pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX,
        pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX,
        pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX,
        pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX,
        pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX,
        pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX,
        VkObjectEntryUsageFlagBitsNVX(..),
        pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX,
        pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX,
        VkObjectEntryTypeNVX(..),
        pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX,
        pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX,
        pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX,
        pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX,
        pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX,
        VkDescriptorUpdateTemplateTypeKHR(..),
        pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR,
        pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR,
        VkViewportCoordinateSwizzleNV(..),
        pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV,
        pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV,
        pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV,
        pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV,
        pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV,
        pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV,
        pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV,
        pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV,
        VkDiscardRectangleModeEXT(..),
        pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT,
        pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT,
        VkSubpassDescriptionFlagBits(..), VkPointClippingBehaviorKHR(..),
        pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR,
        pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR,
        VkCoverageModulationModeNV(..),
        pattern VK_COVERAGE_MODULATION_MODE_NONE_NV,
        pattern VK_COVERAGE_MODULATION_MODE_RGB_NV,
        pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV,
        pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV,
        VkValidationCacheHeaderVersionEXT(..),
        pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT,
        VkShaderInfoTypeAMD(..),
        pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD,
        pattern VK_SHADER_INFO_TYPE_BINARY_AMD,
        pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD,
        VkQueueGlobalPriorityEXT(..),
        pattern VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT,
        pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT,
        pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT,
        pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT,
        VkConservativeRasterizationModeEXT(..),
        pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT,
        pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT,
        pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT,
        -- *** WSI extensions
        VkColorSpaceKHR(..), pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
        VkCompositeAlphaFlagBitsKHR(..),
        pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR,
        pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR,
        pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR,
        VkDisplayPlaneAlphaFlagBitsKHR(..),
        pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR,
        pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR,
        pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR,
        pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR,
        VkPresentModeKHR(..), pattern VK_PRESENT_MODE_IMMEDIATE_KHR,
        pattern VK_PRESENT_MODE_MAILBOX_KHR,
        pattern VK_PRESENT_MODE_FIFO_KHR,
        pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR,
        VkSurfaceTransformFlagBitsKHR(..),
        pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR,
        pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR,
        pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR,
        pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR,
        pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR,
        pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR,
        pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR,
        pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR,
        pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR,
        VkDebugReportFlagBitsEXT(..),
        pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT,
        pattern VK_DEBUG_REPORT_WARNING_BIT_EXT,
        pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT,
        pattern VK_DEBUG_REPORT_ERROR_BIT_EXT,
        pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT,
        VkDebugReportObjectTypeEXT(..),
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT,
        VkRasterizationOrderAMD(..),
        pattern VK_RASTERIZATION_ORDER_STRICT_AMD,
        pattern VK_RASTERIZATION_ORDER_RELAXED_AMD,
        VkExternalMemoryHandleTypeFlagBitsNV(..),
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV,
        VkExternalMemoryFeatureFlagBitsNV(..),
        pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV,
        pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV,
        pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV,
        VkValidationCheckEXT(..), pattern VK_VALIDATION_CHECK_ALL_EXT,
        pattern VK_VALIDATION_CHECK_SHADERS_EXT,
        -- Placeholder for validation enums to be defined for VK_EXT_Validation_flags extension
        VkExternalMemoryHandleTypeFlagBitsKHR(..),
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR,
        VkExternalMemoryFeatureFlagBitsKHR(..),
        pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR,
        VkExternalSemaphoreHandleTypeFlagBitsKHR(..),
        pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR,
        VkExternalSemaphoreFeatureFlagBitsKHR(..),
        pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR,
        VkSemaphoreImportFlagBitsKHR(..),
        pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR,
        VkExternalFenceHandleTypeFlagBitsKHR(..),
        pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR,
        pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR,
        pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR,
        pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR,
        VkExternalFenceFeatureFlagBitsKHR(..),
        pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR,
        pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR,
        VkFenceImportFlagBitsKHR(..),
        pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR,
        VkSurfaceCounterFlagBitsEXT(..),
        pattern VK_SURFACE_COUNTER_VBLANK_EXT, VkDisplayPowerStateEXT(..),
        pattern VK_DISPLAY_POWER_STATE_OFF_EXT,
        pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT,
        pattern VK_DISPLAY_POWER_STATE_ON_EXT, VkDeviceEventTypeEXT(..),
        pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT,
        VkDisplayEventTypeEXT(..),
        pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT,
        VkPeerMemoryFeatureFlagBitsKHX(..),
        pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX,
        pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX,
        pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX,
        pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX,
        VkMemoryAllocateFlagBitsKHX(..),
        pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX,
        VkDeviceGroupPresentModeFlagBitsKHX(..),
        pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX,
        pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX,
        pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX,
        pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX,
        VkSwapchainCreateFlagBitsKHR(..),
        VkTessellationDomainOriginKHR(..),
        pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR,
        pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR,
        VkSamplerYcbcrModelConversionKHR(..),
        pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR,
        pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR,
        pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR,
        pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR,
        pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR,
        VkSamplerYcbcrRangeKHR(..),
        pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR,
        pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR,
        VkChromaLocationKHR(..),
        pattern VK_CHROMA_LOCATION_COSITED_EVEN_KHR,
        pattern VK_CHROMA_LOCATION_MIDPOINT_KHR,
        VkSamplerReductionModeEXT(..),
        pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT,
        pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT,
        pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT, VkBlendOverlapEXT(..),
        pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT,
        pattern VK_BLEND_OVERLAP_DISJOINT_EXT,
        pattern VK_BLEND_OVERLAP_CONJOINT_EXT,
        -- ** Function pointers
        --

        -- *** The PFN_vk*Function types are used by VkAllocationCallbacks below
        PFN_vkInternalAllocationNotification,
        HS_vkInternalAllocationNotification,
        newVkInternalAllocationNotification,
        unwrapVkInternalAllocationNotification,
        PFN_vkInternalFreeNotification, HS_vkInternalFreeNotification,
        newVkInternalFreeNotification, unwrapVkInternalFreeNotification,
        PFN_vkReallocationFunction, HS_vkReallocationFunction,
        newVkReallocationFunction, unwrapVkReallocationFunction,
        PFN_vkAllocationFunction, HS_vkAllocationFunction,
        newVkAllocationFunction, unwrapVkAllocationFunction,
        PFN_vkFreeFunction, HS_vkFreeFunction, newVkFreeFunction,
        unwrapVkFreeFunction, -- *** The PFN_vkVoidFunction type are used by VkGet*ProcAddr below
                              PFN_vkVoidFunction, HS_vkVoidFunction,
        newVkVoidFunction, unwrapVkVoidFunction,
        -- *** The PFN_vkDebugReportCallbackEXT type are used by the DEBUG_REPORT extension
        PFN_vkDebugReportCallbackEXT, HS_vkDebugReportCallbackEXT,
        newVkDebugReportCallbackEXT, unwrapVkDebugReportCallbackEXT)
       where
import           Data.Bits                       (Bits (..), FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Data.Void                       (Void)
import           Foreign.C.String                (CString)
import           Foreign.C.Types                 (CChar, CULong (..),
                                                  CWchar (..))
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE :: (Num a, Eq a) => a

pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256

type VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256

pattern VK_UUID_SIZE :: (Num a, Eq a) => a

pattern VK_UUID_SIZE = 16

type VK_UUID_SIZE = 16

pattern VK_LUID_SIZE_KHR :: (Num a, Eq a) => a

pattern VK_LUID_SIZE_KHR = 8

type VK_LUID_SIZE_KHR = 8

pattern VK_MAX_EXTENSION_NAME_SIZE :: (Num a, Eq a) => a

pattern VK_MAX_EXTENSION_NAME_SIZE = 256

type VK_MAX_EXTENSION_NAME_SIZE = 256

pattern VK_MAX_DESCRIPTION_SIZE :: (Num a, Eq a) => a

pattern VK_MAX_DESCRIPTION_SIZE = 256

type VK_MAX_DESCRIPTION_SIZE = 256

pattern VK_MAX_MEMORY_TYPES :: (Num a, Eq a) => a

pattern VK_MAX_MEMORY_TYPES = 32

type VK_MAX_MEMORY_TYPES = 32

-- | The maximum number of unique memory heaps, each of which supporting 1 or more memory types
pattern VK_MAX_MEMORY_HEAPS :: (Num a, Eq a) => a

pattern VK_MAX_MEMORY_HEAPS = 16

type VK_MAX_MEMORY_HEAPS = 16

pattern VK_LOD_CLAMP_NONE :: (Fractional a, Eq a) => a

pattern VK_LOD_CLAMP_NONE = 1000.0

pattern VK_REMAINING_MIP_LEVELS :: Word32

pattern VK_REMAINING_MIP_LEVELS = 4294967295

type VK_REMAINING_MIP_LEVELS = 4294967295

pattern VK_REMAINING_ARRAY_LAYERS :: Word32

pattern VK_REMAINING_ARRAY_LAYERS = 4294967295

type VK_REMAINING_ARRAY_LAYERS = 4294967295

pattern VK_WHOLE_SIZE :: Word64

pattern VK_WHOLE_SIZE = 18446744073709551615

type VK_WHOLE_SIZE = 18446744073709551615

pattern VK_ATTACHMENT_UNUSED :: Word32

pattern VK_ATTACHMENT_UNUSED = 4294967295

type VK_ATTACHMENT_UNUSED = 4294967295

pattern VK_TRUE :: (Num a, Eq a) => a

pattern VK_TRUE = 1

type VK_TRUE = 1

pattern VK_FALSE :: (Num a, Eq a) => a

pattern VK_FALSE = 0

type VK_FALSE = 0

pattern VK_QUEUE_FAMILY_IGNORED :: Word32

pattern VK_QUEUE_FAMILY_IGNORED = 4294967295

type VK_QUEUE_FAMILY_IGNORED = 4294967295

pattern VK_QUEUE_FAMILY_EXTERNAL_KHR :: Word32

pattern VK_QUEUE_FAMILY_EXTERNAL_KHR = 4294967294

type VK_QUEUE_FAMILY_EXTERNAL_KHR = 4294967294

pattern VK_QUEUE_FAMILY_FOREIGN_EXT :: Word32

pattern VK_QUEUE_FAMILY_FOREIGN_EXT = 4294967293

type VK_QUEUE_FAMILY_FOREIGN_EXT = 4294967293

pattern VK_SUBPASS_EXTERNAL :: Word32

pattern VK_SUBPASS_EXTERNAL = 4294967295

type VK_SUBPASS_EXTERNAL = 4294967295

pattern VK_MAX_DEVICE_GROUP_SIZE_KHX :: (Num a, Eq a) => a

pattern VK_MAX_DEVICE_GROUP_SIZE_KHX = 32

type VK_MAX_DEVICE_GROUP_SIZE_KHX = 32

-- | Requires @X11/Xlib.h@
data Display

-- | Requires @X11/Xlib.h@
type VisualID = CULong

-- | Requires @X11/Xlib.h@
type Window = CULong

-- | Requires @X11/extensions/Xrandr.h@
type RROutput = CULong

-- | Requires @android/native_window.h@
data ANativeWindow

-- | Requires @mir_toolkit/client_types.h@
data MirConnection

-- | Requires @mir_toolkit/client_types.h@
data MirSurface

-- | Requires @wayland-client.h@
data WlDisplay

-- | Requires @wayland-client.h@
data WlSurface

-- | Requires @windows.h@
type HINSTANCE = Ptr ()

-- | Requires @windows.h@
type HWND = Ptr ()

-- | Requires @windows.h@
type HANDLE = Ptr ()

-- | Requires @windows.h@
data SECURITY_ATTRIBUTES

-- | Requires @windows.h@
type DWORD = Word32

-- | Requires @windows.h@
type LPCWSTR = Ptr CWchar

-- | Requires @xcb/xcb.h@
data XcbConnectionT

-- | Requires @xcb/xcb.h@
data XcbVisualidT

-- | Requires @xcb/xcb.h@
data XcbWindowT

-- | > ##define VK_MAKE_VERSION(major, minor, patch) \
--   >     (((major) << 22) | ((minor) << 12) | (patch))
_VK_MAKE_VERSION :: Bits a => a -> a -> a -> a
_VK_MAKE_VERSION major minor patch
  = unsafeShiftL major 22 .|. unsafeShiftL minor 12 .|. patch

{-# INLINE _VK_MAKE_VERSION #-}
##define VK_MAKE_VERSION(major, minor, patch) _VK_MAKE_VERSION major minor patch

-- | > ##define VK_VERSION_MAJOR(version) ((uint32_t)(version) >> 22)
_VK_VERSION_MAJOR :: Bits a => a -> a
_VK_VERSION_MAJOR version = unsafeShiftR version 22

{-# INLINE _VK_VERSION_MAJOR #-}
##define VK_VERSION_MAJOR(version) _VK_VERSION_MAJOR version

-- | > ##define VK_VERSION_MINOR(version) (((uint32_t)(version) >> 12) & 0x3ff)
_VK_VERSION_MINOR :: (Bits a, Num a) => a -> a
_VK_VERSION_MINOR version = unsafeShiftR version 12 .&. 1023

{-# INLINE _VK_VERSION_MINOR #-}
##define VK_VERSION_MINOR(version) _VK_VERSION_MINOR version

-- | > ##define VK_VERSION_PATCH(version) ((uint32_t)(version) & 0xfff)
_VK_VERSION_PATCH :: (Bits a, Num a) => a -> a
_VK_VERSION_PATCH = (.&. 4095)

{-# INLINE _VK_VERSION_PATCH #-}
##define VK_VERSION_PATCH(version) _VK_VERSION_PATCH version

-- | > // Vulkan 1.0 version number
--   > ##define VK_API_VERSION_1_0 VK_MAKE_VERSION(1, 0, 0)// Patch version should always be set to 0
pattern VK_API_VERSION_1_0 :: (Num a, Eq a) => a

pattern VK_API_VERSION_1_0 = 4194304

type VK_API_VERSION_1_0 = 4194304

-- | > // Version of this file
--   > ##define VK_HEADER_VERSION 67
pattern VK_HEADER_VERSION :: (Num a, Eq a) => a

pattern VK_HEADER_VERSION = 67

type VK_HEADER_VERSION = 67

newtype VkSampleMask = VkSampleMask Word32
                         deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                   Storable, Real, Data, Generic)

instance Show VkSampleMask where
        {-# INLINE show #-}
        show (VkSampleMask x) = show x

instance Read VkSampleMask where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS Word32)

newtype VkBool32 = VkBool32 Word32
                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                               Storable, Real, Data, Generic)

instance Show VkBool32 where
        {-# INLINE show #-}
        show (VkBool32 x) = show x

instance Read VkBool32 where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS Word32)

newtype VkFlags = VkFlags Word32
                    deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                              Storable, Real, Data, Generic)

instance Show VkFlags where
        {-# INLINE show #-}
        show (VkFlags x) = show x

instance Read VkFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS Word32)

newtype VkDeviceSize = VkDeviceSize Word64
                         deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                   Storable, Real, Data, Generic)

instance Show VkDeviceSize where
        {-# INLINE show #-}
        show (VkDeviceSize x) = show x

instance Read VkDeviceSize where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS Word64)

newtype VkFramebufferCreateFlags = VkFramebufferCreateFlags VkFlags
                                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                               FiniteBits, Storable, Real, Data, Generic)

instance Show VkFramebufferCreateFlags where
        {-# INLINE show #-}
        show (VkFramebufferCreateFlags x) = show x

instance Read VkFramebufferCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkQueryPoolCreateFlags = VkQueryPoolCreateFlags VkFlags
                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                             FiniteBits, Storable, Real, Data, Generic)

instance Show VkQueryPoolCreateFlags where
        {-# INLINE show #-}
        show (VkQueryPoolCreateFlags x) = show x

instance Read VkQueryPoolCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags VkFlags
                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                              FiniteBits, Storable, Real, Data, Generic)

instance Show VkRenderPassCreateFlags where
        {-# INLINE show #-}
        show (VkRenderPassCreateFlags x) = show x

instance Read VkRenderPassCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSamplerCreateFlags = VkSamplerCreateFlags VkFlags
                                 deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                           Storable, Real, Data, Generic)

instance Show VkSamplerCreateFlags where
        {-# INLINE show #-}
        show (VkSamplerCreateFlags x) = show x

instance Read VkSamplerCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineLayoutCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineLayoutCreateFlags x) = show x

instance Read VkPipelineLayoutCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineCacheCreateFlags = VkPipelineCacheCreateFlags VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineCacheCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineCacheCreateFlags x) = show x

instance Read VkPipelineCacheCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags VkFlags
                                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                             Bits, FiniteBits, Storable, Real, Data,
                                                             Generic)

instance Show VkPipelineDepthStencilStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineDepthStencilStateCreateFlags x) = show x

instance Read VkPipelineDepthStencilStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags VkFlags
                                              deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                        FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineDynamicStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineDynamicStateCreateFlags x) = show x

instance Read VkPipelineDynamicStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags VkFlags
                                                 deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                           Bits, FiniteBits, Storable, Real, Data,
                                                           Generic)

instance Show VkPipelineColorBlendStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineColorBlendStateCreateFlags x) = show x

instance Read VkPipelineColorBlendStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkPipelineMultisampleStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineMultisampleStateCreateFlags x) = show x

instance Read VkPipelineMultisampleStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags VkFlags
                                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                              Bits, FiniteBits, Storable, Real,
                                                              Data, Generic)

instance Show VkPipelineRasterizationStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineRasterizationStateCreateFlags x) = show x

instance Read VkPipelineRasterizationStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags VkFlags
                                               deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                         Bits, FiniteBits, Storable, Real, Data,
                                                         Generic)

instance Show VkPipelineViewportStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineViewportStateCreateFlags x) = show x

instance Read VkPipelineViewportStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags VkFlags
                                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                             Bits, FiniteBits, Storable, Real, Data,
                                                             Generic)

instance Show VkPipelineTessellationStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineTessellationStateCreateFlags x) = show x

instance Read VkPipelineTessellationStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags VkFlags
                                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                              Bits, FiniteBits, Storable, Real,
                                                              Data, Generic)

instance Show VkPipelineInputAssemblyStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineInputAssemblyStateCreateFlags x) = show x

instance Read VkPipelineInputAssemblyStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkPipelineVertexInputStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineVertexInputStateCreateFlags x) = show x

instance Read VkPipelineVertexInputStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags VkFlags
                                             deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                       FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineShaderStageCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineShaderStageCreateFlags x) = show x

instance Read VkPipelineShaderStageCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkDescriptorSetLayoutCreateFlags =
     VkDescriptorSetLayoutCreateFlagBits

newtype VkBufferViewCreateFlags = VkBufferViewCreateFlags VkFlags
                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                              FiniteBits, Storable, Real, Data, Generic)

instance Show VkBufferViewCreateFlags where
        {-# INLINE show #-}
        show (VkBufferViewCreateFlags x) = show x

instance Read VkBufferViewCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkInstanceCreateFlags = VkInstanceCreateFlags VkFlags
                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                            Storable, Real, Data, Generic)

instance Show VkInstanceCreateFlags where
        {-# INLINE show #-}
        show (VkInstanceCreateFlags x) = show x

instance Read VkInstanceCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDeviceCreateFlags = VkDeviceCreateFlags VkFlags
                                deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                          Storable, Real, Data, Generic)

instance Show VkDeviceCreateFlags where
        {-# INLINE show #-}
        show (VkDeviceCreateFlags x) = show x

instance Read VkDeviceCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlags VkFlags
                                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                               FiniteBits, Storable, Real, Data, Generic)

instance Show VkDeviceQueueCreateFlags where
        {-# INLINE show #-}
        show (VkDeviceQueueCreateFlags x) = show x

instance Read VkDeviceQueueCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkQueueFlags = VkQueueFlagBits

type VkMemoryPropertyFlags = VkMemoryPropertyFlagBits

type VkMemoryHeapFlags = VkMemoryHeapFlagBits

type VkAccessFlags = VkAccessFlagBits

type VkBufferUsageFlags = VkBufferUsageFlagBits

type VkBufferCreateFlags = VkBufferCreateFlagBits

type VkShaderStageFlags = VkShaderStageFlagBits

type VkImageUsageFlags = VkImageUsageFlagBits

type VkImageCreateFlags = VkImageCreateFlagBits

newtype VkImageViewCreateFlags = VkImageViewCreateFlags VkFlags
                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                             FiniteBits, Storable, Real, Data, Generic)

instance Show VkImageViewCreateFlags where
        {-# INLINE show #-}
        show (VkImageViewCreateFlags x) = show x

instance Read VkImageViewCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkPipelineCreateFlags = VkPipelineCreateFlagBits

type VkColorComponentFlags = VkColorComponentFlagBits

type VkFenceCreateFlags = VkFenceCreateFlagBits

newtype VkSemaphoreCreateFlags = VkSemaphoreCreateFlags VkFlags
                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                             FiniteBits, Storable, Real, Data, Generic)

instance Show VkSemaphoreCreateFlags where
        {-# INLINE show #-}
        show (VkSemaphoreCreateFlags x) = show x

instance Read VkSemaphoreCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkFormatFeatureFlags = VkFormatFeatureFlagBits

type VkQueryControlFlags = VkQueryControlFlagBits

type VkQueryResultFlags = VkQueryResultFlagBits

newtype VkShaderModuleCreateFlags = VkShaderModuleCreateFlags VkFlags
                                      deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                FiniteBits, Storable, Real, Data, Generic)

instance Show VkShaderModuleCreateFlags where
        {-# INLINE show #-}
        show (VkShaderModuleCreateFlags x) = show x

instance Read VkShaderModuleCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkEventCreateFlags = VkEventCreateFlags VkFlags
                               deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                         Storable, Real, Data, Generic)

instance Show VkEventCreateFlags where
        {-# INLINE show #-}
        show (VkEventCreateFlags x) = show x

instance Read VkEventCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkCommandPoolCreateFlags = VkCommandPoolCreateFlagBits

type VkCommandPoolResetFlags = VkCommandPoolResetFlagBits

type VkCommandBufferResetFlags = VkCommandBufferResetFlagBits

type VkCommandBufferUsageFlags = VkCommandBufferUsageFlagBits

type VkQueryPipelineStatisticFlags =
     VkQueryPipelineStatisticFlagBits

newtype VkMemoryMapFlags = VkMemoryMapFlags VkFlags
                             deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                       Storable, Real, Data, Generic)

instance Show VkMemoryMapFlags where
        {-# INLINE show #-}
        show (VkMemoryMapFlags x) = show x

instance Read VkMemoryMapFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkImageAspectFlags = VkImageAspectFlagBits

type VkSparseMemoryBindFlags = VkSparseMemoryBindFlagBits

type VkSparseImageFormatFlags = VkSparseImageFormatFlagBits

type VkSubpassDescriptionFlags = VkSubpassDescriptionFlagBits

type VkPipelineStageFlags = VkPipelineStageFlagBits

type VkSampleCountFlags = VkSampleCountFlagBits

type VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlagBits

type VkStencilFaceFlags = VkStencilFaceFlagBits

type VkCullModeFlags = VkCullModeFlagBits

type VkDescriptorPoolCreateFlags = VkDescriptorPoolCreateFlagBits

newtype VkDescriptorPoolResetFlags = VkDescriptorPoolResetFlags VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkDescriptorPoolResetFlags where
        {-# INLINE show #-}
        show (VkDescriptorPoolResetFlags x) = show x

instance Read VkDescriptorPoolResetFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkDependencyFlags = VkDependencyFlagBits

type VkIndirectCommandsLayoutUsageFlagsNVX =
     VkIndirectCommandsLayoutUsageFlagBitsNVX

type VkObjectEntryUsageFlagsNVX = VkObjectEntryUsageFlagBitsNVX

newtype VkDescriptorUpdateTemplateCreateFlagsKHR = VkDescriptorUpdateTemplateCreateFlagsKHR VkFlags
                                                     deriving (Eq, Ord, Num, Bounded, Enum,
                                                               Integral, Bits, FiniteBits, Storable,
                                                               Real, Data, Generic)

instance Show VkDescriptorUpdateTemplateCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkDescriptorUpdateTemplateCreateFlagsKHR x) = show x

instance Read VkDescriptorUpdateTemplateCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkCompositeAlphaFlagsKHR = VkCompositeAlphaFlagBitsKHR

type VkDisplayPlaneAlphaFlagsKHR = VkDisplayPlaneAlphaFlagBitsKHR

type VkSurfaceTransformFlagsKHR = VkSurfaceTransformFlagBitsKHR

type VkSwapchainCreateFlagsKHR = VkSwapchainCreateFlagBitsKHR

newtype VkDisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkDisplayModeCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkDisplayModeCreateFlagsKHR x) = show x

instance Read VkDisplayModeCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR VkFlags
                                           deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                     FiniteBits, Storable, Real, Data, Generic)

instance Show VkDisplaySurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkDisplaySurfaceCreateFlagsKHR x) = show x

instance Read VkDisplaySurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkAndroidSurfaceCreateFlagsKHR = VkAndroidSurfaceCreateFlagsKHR VkFlags
                                           deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                     FiniteBits, Storable, Real, Data, Generic)

instance Show VkAndroidSurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkAndroidSurfaceCreateFlagsKHR x) = show x

instance Read VkAndroidSurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMirSurfaceCreateFlagsKHR = VkMirSurfaceCreateFlagsKHR VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkMirSurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkMirSurfaceCreateFlagsKHR x) = show x

instance Read VkMirSurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkViSurfaceCreateFlagsNN = VkViSurfaceCreateFlagsNN VkFlags
                                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                               FiniteBits, Storable, Real, Data, Generic)

instance Show VkViSurfaceCreateFlagsNN where
        {-# INLINE show #-}
        show (VkViSurfaceCreateFlagsNN x) = show x

instance Read VkViSurfaceCreateFlagsNN where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkWaylandSurfaceCreateFlagsKHR = VkWaylandSurfaceCreateFlagsKHR VkFlags
                                           deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                     FiniteBits, Storable, Real, Data, Generic)

instance Show VkWaylandSurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkWaylandSurfaceCreateFlagsKHR x) = show x

instance Read VkWaylandSurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkWin32SurfaceCreateFlagsKHR = VkWin32SurfaceCreateFlagsKHR VkFlags
                                         deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                   FiniteBits, Storable, Real, Data, Generic)

instance Show VkWin32SurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkWin32SurfaceCreateFlagsKHR x) = show x

instance Read VkWin32SurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkXlibSurfaceCreateFlagsKHR = VkXlibSurfaceCreateFlagsKHR VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkXlibSurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkXlibSurfaceCreateFlagsKHR x) = show x

instance Read VkXlibSurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkXcbSurfaceCreateFlagsKHR = VkXcbSurfaceCreateFlagsKHR VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkXcbSurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkXcbSurfaceCreateFlagsKHR x) = show x

instance Read VkXcbSurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkIOSSurfaceCreateFlagsMVK = VkIOSSurfaceCreateFlagsMVK VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkIOSSurfaceCreateFlagsMVK where
        {-# INLINE show #-}
        show (VkIOSSurfaceCreateFlagsMVK x) = show x

instance Read VkIOSSurfaceCreateFlagsMVK where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMacOSSurfaceCreateFlagsMVK = VkMacOSSurfaceCreateFlagsMVK VkFlags
                                         deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                   FiniteBits, Storable, Real, Data, Generic)

instance Show VkMacOSSurfaceCreateFlagsMVK where
        {-# INLINE show #-}
        show (VkMacOSSurfaceCreateFlagsMVK x) = show x

instance Read VkMacOSSurfaceCreateFlagsMVK where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkPeerMemoryFeatureFlagsKHX = VkPeerMemoryFeatureFlagBitsKHX

type VkMemoryAllocateFlagsKHX = VkMemoryAllocateFlagBitsKHX

type VkDeviceGroupPresentModeFlagsKHX =
     VkDeviceGroupPresentModeFlagBitsKHX

type VkDebugReportFlagsEXT = VkDebugReportFlagBitsEXT

newtype VkCommandPoolTrimFlagsKHR = VkCommandPoolTrimFlagsKHR VkFlags
                                      deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                FiniteBits, Storable, Real, Data, Generic)

instance Show VkCommandPoolTrimFlagsKHR where
        {-# INLINE show #-}
        show (VkCommandPoolTrimFlagsKHR x) = show x

instance Read VkCommandPoolTrimFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkExternalMemoryHandleTypeFlagsNV =
     VkExternalMemoryHandleTypeFlagBitsNV

type VkExternalMemoryFeatureFlagsNV =
     VkExternalMemoryFeatureFlagBitsNV

type VkExternalMemoryHandleTypeFlagsKHR =
     VkExternalMemoryHandleTypeFlagBitsKHR

type VkExternalMemoryFeatureFlagsKHR =
     VkExternalMemoryFeatureFlagBitsKHR

type VkExternalSemaphoreHandleTypeFlagsKHR =
     VkExternalSemaphoreHandleTypeFlagBitsKHR

type VkExternalSemaphoreFeatureFlagsKHR =
     VkExternalSemaphoreFeatureFlagBitsKHR

type VkSemaphoreImportFlagsKHR = VkSemaphoreImportFlagBitsKHR

type VkExternalFenceHandleTypeFlagsKHR =
     VkExternalFenceHandleTypeFlagBitsKHR

type VkExternalFenceFeatureFlagsKHR =
     VkExternalFenceFeatureFlagBitsKHR

type VkFenceImportFlagsKHR = VkFenceImportFlagBitsKHR

type VkSurfaceCounterFlagsEXT = VkSurfaceCounterFlagBitsEXT

newtype VkPipelineViewportSwizzleStateCreateFlagsNV = VkPipelineViewportSwizzleStateCreateFlagsNV VkFlags
                                                        deriving (Eq, Ord, Num, Bounded, Enum,
                                                                  Integral, Bits, FiniteBits,
                                                                  Storable, Real, Data, Generic)

instance Show VkPipelineViewportSwizzleStateCreateFlagsNV where
        {-# INLINE show #-}
        show (VkPipelineViewportSwizzleStateCreateFlagsNV x) = show x

instance Read VkPipelineViewportSwizzleStateCreateFlagsNV where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDiscardRectangleStateCreateFlagsEXT = VkPipelineDiscardRectangleStateCreateFlagsEXT VkFlags
                                                          deriving (Eq, Ord, Num, Bounded, Enum,
                                                                    Integral, Bits, FiniteBits,
                                                                    Storable, Real, Data, Generic)

instance Show VkPipelineDiscardRectangleStateCreateFlagsEXT where
        {-# INLINE show #-}
        show (VkPipelineDiscardRectangleStateCreateFlagsEXT x) = show x

instance Read VkPipelineDiscardRectangleStateCreateFlagsEXT where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineCoverageToColorStateCreateFlagsNV = VkPipelineCoverageToColorStateCreateFlagsNV VkFlags
                                                        deriving (Eq, Ord, Num, Bounded, Enum,
                                                                  Integral, Bits, FiniteBits,
                                                                  Storable, Real, Data, Generic)

instance Show VkPipelineCoverageToColorStateCreateFlagsNV where
        {-# INLINE show #-}
        show (VkPipelineCoverageToColorStateCreateFlagsNV x) = show x

instance Read VkPipelineCoverageToColorStateCreateFlagsNV where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineCoverageModulationStateCreateFlagsNV = VkPipelineCoverageModulationStateCreateFlagsNV VkFlags
                                                           deriving (Eq, Ord, Num, Bounded, Enum,
                                                                     Integral, Bits, FiniteBits,
                                                                     Storable, Real, Data, Generic)

instance Show VkPipelineCoverageModulationStateCreateFlagsNV where
        {-# INLINE show #-}
        show (VkPipelineCoverageModulationStateCreateFlagsNV x) = show x

instance Read VkPipelineCoverageModulationStateCreateFlagsNV where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT VkFlags
                                            deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                      FiniteBits, Storable, Real, Data, Generic)

instance Show VkValidationCacheCreateFlagsEXT where
        {-# INLINE show #-}
        show (VkValidationCacheCreateFlagsEXT x) = show x

instance Read VkValidationCacheCreateFlagsEXT where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT VkFlags
                                                                   deriving (Eq, Ord, Num, Bounded,
                                                                             Enum, Integral, Bits,
                                                                             FiniteBits, Storable,
                                                                             Real, Data, Generic)

instance Show
           VkPipelineRasterizationConservativeStateCreateFlagsEXT
         where
        {-# INLINE show #-}
        show (VkPipelineRasterizationConservativeStateCreateFlagsEXT x)
          = show x

instance Read
           VkPipelineRasterizationConservativeStateCreateFlagsEXT
         where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

type VkInstance = Ptr VkInstance_T

-- | Opaque data type referenced by VkInstance
data VkInstance_T

type VkPhysicalDevice = Ptr VkPhysicalDevice_T

-- | Opaque data type referenced by VkPhysicalDevice
data VkPhysicalDevice_T

type VkDevice = Ptr VkDevice_T

-- | Opaque data type referenced by VkDevice
data VkDevice_T

type VkQueue = Ptr VkQueue_T

-- | Opaque data type referenced by VkQueue
data VkQueue_T

type VkCommandBuffer = Ptr VkCommandBuffer_T

-- | Opaque data type referenced by VkCommandBuffer
data VkCommandBuffer_T

type VkDeviceMemory = VkPtr VkDeviceMemory_T

-- | Opaque data type referenced by VkDeviceMemory
data VkDeviceMemory_T

type VkCommandPool = VkPtr VkCommandPool_T

-- | Opaque data type referenced by VkCommandPool
data VkCommandPool_T

type VkBuffer = VkPtr VkBuffer_T

-- | Opaque data type referenced by VkBuffer
data VkBuffer_T

type VkBufferView = VkPtr VkBufferView_T

-- | Opaque data type referenced by VkBufferView
data VkBufferView_T

type VkImage = VkPtr VkImage_T

-- | Opaque data type referenced by VkImage
data VkImage_T

type VkImageView = VkPtr VkImageView_T

-- | Opaque data type referenced by VkImageView
data VkImageView_T

type VkShaderModule = VkPtr VkShaderModule_T

-- | Opaque data type referenced by VkShaderModule
data VkShaderModule_T

type VkPipeline = VkPtr VkPipeline_T

-- | Opaque data type referenced by VkPipeline
data VkPipeline_T

type VkPipelineLayout = VkPtr VkPipelineLayout_T

-- | Opaque data type referenced by VkPipelineLayout
data VkPipelineLayout_T

type VkSampler = VkPtr VkSampler_T

-- | Opaque data type referenced by VkSampler
data VkSampler_T

type VkDescriptorSet = VkPtr VkDescriptorSet_T

-- | Opaque data type referenced by VkDescriptorSet
data VkDescriptorSet_T

type VkDescriptorSetLayout = VkPtr VkDescriptorSetLayout_T

-- | Opaque data type referenced by VkDescriptorSetLayout
data VkDescriptorSetLayout_T

type VkDescriptorPool = VkPtr VkDescriptorPool_T

-- | Opaque data type referenced by VkDescriptorPool
data VkDescriptorPool_T

type VkFence = VkPtr VkFence_T

-- | Opaque data type referenced by VkFence
data VkFence_T

type VkSemaphore = VkPtr VkSemaphore_T

-- | Opaque data type referenced by VkSemaphore
data VkSemaphore_T

type VkEvent = VkPtr VkEvent_T

-- | Opaque data type referenced by VkEvent
data VkEvent_T

type VkQueryPool = VkPtr VkQueryPool_T

-- | Opaque data type referenced by VkQueryPool
data VkQueryPool_T

type VkFramebuffer = VkPtr VkFramebuffer_T

-- | Opaque data type referenced by VkFramebuffer
data VkFramebuffer_T

type VkRenderPass = VkPtr VkRenderPass_T

-- | Opaque data type referenced by VkRenderPass
data VkRenderPass_T

type VkPipelineCache = VkPtr VkPipelineCache_T

-- | Opaque data type referenced by VkPipelineCache
data VkPipelineCache_T

type VkObjectTableNVX = VkPtr VkObjectTableNVX_T

-- | Opaque data type referenced by VkObjectTableNVX
data VkObjectTableNVX_T

type VkIndirectCommandsLayoutNVX =
     VkPtr VkIndirectCommandsLayoutNVX_T

-- | Opaque data type referenced by VkIndirectCommandsLayoutNVX
data VkIndirectCommandsLayoutNVX_T

type VkDescriptorUpdateTemplateKHR =
     VkPtr VkDescriptorUpdateTemplateKHR_T

-- | Opaque data type referenced by VkDescriptorUpdateTemplateKHR
data VkDescriptorUpdateTemplateKHR_T

type VkSamplerYcbcrConversionKHR =
     VkPtr VkSamplerYcbcrConversionKHR_T

-- | Opaque data type referenced by VkSamplerYcbcrConversionKHR
data VkSamplerYcbcrConversionKHR_T

type VkValidationCacheEXT = VkPtr VkValidationCacheEXT_T

-- | Opaque data type referenced by VkValidationCacheEXT
data VkValidationCacheEXT_T

type VkDisplayKHR = VkPtr VkDisplayKHR_T

-- | Opaque data type referenced by VkDisplayKHR
data VkDisplayKHR_T

type VkDisplayModeKHR = VkPtr VkDisplayModeKHR_T

-- | Opaque data type referenced by VkDisplayModeKHR
data VkDisplayModeKHR_T

type VkSurfaceKHR = VkPtr VkSurfaceKHR_T

-- | Opaque data type referenced by VkSurfaceKHR
data VkSurfaceKHR_T

type VkSwapchainKHR = VkPtr VkSwapchainKHR_T

-- | Opaque data type referenced by VkSwapchainKHR
data VkSwapchainKHR_T

type VkDebugReportCallbackEXT = VkPtr VkDebugReportCallbackEXT_T

-- | Opaque data type referenced by VkDebugReportCallbackEXT
data VkDebugReportCallbackEXT_T

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentLoadOp.html VkAttachmentLoadOp registry at www.khronos.org>
newtype VkAttachmentLoadOp = VkAttachmentLoadOp Int32
                               deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkAttachmentLoadOp where
        showsPrec _ VK_ATTACHMENT_LOAD_OP_LOAD
          = showString "VK_ATTACHMENT_LOAD_OP_LOAD"
        showsPrec _ VK_ATTACHMENT_LOAD_OP_CLEAR
          = showString "VK_ATTACHMENT_LOAD_OP_CLEAR"
        showsPrec _ VK_ATTACHMENT_LOAD_OP_DONT_CARE
          = showString "VK_ATTACHMENT_LOAD_OP_DONT_CARE"
        showsPrec p (VkAttachmentLoadOp x)
          = showParen (p >= 11)
              (showString "VkAttachmentLoadOp " . showsPrec 11 x)

instance Read VkAttachmentLoadOp where
        readPrec
          = parens
              (choose
                 [("VK_ATTACHMENT_LOAD_OP_LOAD", pure VK_ATTACHMENT_LOAD_OP_LOAD),
                  ("VK_ATTACHMENT_LOAD_OP_CLEAR", pure VK_ATTACHMENT_LOAD_OP_CLEAR),
                  ("VK_ATTACHMENT_LOAD_OP_DONT_CARE",
                   pure VK_ATTACHMENT_LOAD_OP_DONT_CARE)]
                 +++
                 prec 10
                   (expectP (Ident "VkAttachmentLoadOp") >>
                      (VkAttachmentLoadOp <$> step readPrec)))

pattern VK_ATTACHMENT_LOAD_OP_LOAD :: VkAttachmentLoadOp

pattern VK_ATTACHMENT_LOAD_OP_LOAD = VkAttachmentLoadOp 0

pattern VK_ATTACHMENT_LOAD_OP_CLEAR :: VkAttachmentLoadOp

pattern VK_ATTACHMENT_LOAD_OP_CLEAR = VkAttachmentLoadOp 1

pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE :: VkAttachmentLoadOp

pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE = VkAttachmentLoadOp 2

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentStoreOp.html VkAttachmentStoreOp registry at www.khronos.org>
newtype VkAttachmentStoreOp = VkAttachmentStoreOp Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkAttachmentStoreOp where
        showsPrec _ VK_ATTACHMENT_STORE_OP_STORE
          = showString "VK_ATTACHMENT_STORE_OP_STORE"
        showsPrec _ VK_ATTACHMENT_STORE_OP_DONT_CARE
          = showString "VK_ATTACHMENT_STORE_OP_DONT_CARE"
        showsPrec p (VkAttachmentStoreOp x)
          = showParen (p >= 11)
              (showString "VkAttachmentStoreOp " . showsPrec 11 x)

instance Read VkAttachmentStoreOp where
        readPrec
          = parens
              (choose
                 [("VK_ATTACHMENT_STORE_OP_STORE",
                   pure VK_ATTACHMENT_STORE_OP_STORE),
                  ("VK_ATTACHMENT_STORE_OP_DONT_CARE",
                   pure VK_ATTACHMENT_STORE_OP_DONT_CARE)]
                 +++
                 prec 10
                   (expectP (Ident "VkAttachmentStoreOp") >>
                      (VkAttachmentStoreOp <$> step readPrec)))

pattern VK_ATTACHMENT_STORE_OP_STORE :: VkAttachmentStoreOp

pattern VK_ATTACHMENT_STORE_OP_STORE = VkAttachmentStoreOp 0

pattern VK_ATTACHMENT_STORE_OP_DONT_CARE :: VkAttachmentStoreOp

pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = VkAttachmentStoreOp 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBlendFactor.html VkBlendFactor registry at www.khronos.org>
newtype VkBlendFactor = VkBlendFactor Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkBlendFactor where
        showsPrec _ VK_BLEND_FACTOR_ZERO
          = showString "VK_BLEND_FACTOR_ZERO"
        showsPrec _ VK_BLEND_FACTOR_ONE = showString "VK_BLEND_FACTOR_ONE"
        showsPrec _ VK_BLEND_FACTOR_SRC_COLOR
          = showString "VK_BLEND_FACTOR_SRC_COLOR"
        showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR
          = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR"
        showsPrec _ VK_BLEND_FACTOR_DST_COLOR
          = showString "VK_BLEND_FACTOR_DST_COLOR"
        showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR
          = showString "VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR"
        showsPrec _ VK_BLEND_FACTOR_SRC_ALPHA
          = showString "VK_BLEND_FACTOR_SRC_ALPHA"
        showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
          = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA"
        showsPrec _ VK_BLEND_FACTOR_DST_ALPHA
          = showString "VK_BLEND_FACTOR_DST_ALPHA"
        showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA
          = showString "VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA"
        showsPrec _ VK_BLEND_FACTOR_CONSTANT_COLOR
          = showString "VK_BLEND_FACTOR_CONSTANT_COLOR"
        showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR
          = showString "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR"
        showsPrec _ VK_BLEND_FACTOR_CONSTANT_ALPHA
          = showString "VK_BLEND_FACTOR_CONSTANT_ALPHA"
        showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA
          = showString "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA"
        showsPrec _ VK_BLEND_FACTOR_SRC_ALPHA_SATURATE
          = showString "VK_BLEND_FACTOR_SRC_ALPHA_SATURATE"
        showsPrec _ VK_BLEND_FACTOR_SRC1_COLOR
          = showString "VK_BLEND_FACTOR_SRC1_COLOR"
        showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR
          = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR"
        showsPrec _ VK_BLEND_FACTOR_SRC1_ALPHA
          = showString "VK_BLEND_FACTOR_SRC1_ALPHA"
        showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
          = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA"
        showsPrec p (VkBlendFactor x)
          = showParen (p >= 11)
              (showString "VkBlendFactor " . showsPrec 11 x)

instance Read VkBlendFactor where
        readPrec
          = parens
              (choose
                 [("VK_BLEND_FACTOR_ZERO", pure VK_BLEND_FACTOR_ZERO),
                  ("VK_BLEND_FACTOR_ONE", pure VK_BLEND_FACTOR_ONE),
                  ("VK_BLEND_FACTOR_SRC_COLOR", pure VK_BLEND_FACTOR_SRC_COLOR),
                  ("VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR",
                   pure VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR),
                  ("VK_BLEND_FACTOR_DST_COLOR", pure VK_BLEND_FACTOR_DST_COLOR),
                  ("VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR",
                   pure VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR),
                  ("VK_BLEND_FACTOR_SRC_ALPHA", pure VK_BLEND_FACTOR_SRC_ALPHA),
                  ("VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA",
                   pure VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA),
                  ("VK_BLEND_FACTOR_DST_ALPHA", pure VK_BLEND_FACTOR_DST_ALPHA),
                  ("VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA",
                   pure VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA),
                  ("VK_BLEND_FACTOR_CONSTANT_COLOR",
                   pure VK_BLEND_FACTOR_CONSTANT_COLOR),
                  ("VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR",
                   pure VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR),
                  ("VK_BLEND_FACTOR_CONSTANT_ALPHA",
                   pure VK_BLEND_FACTOR_CONSTANT_ALPHA),
                  ("VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA",
                   pure VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA),
                  ("VK_BLEND_FACTOR_SRC_ALPHA_SATURATE",
                   pure VK_BLEND_FACTOR_SRC_ALPHA_SATURATE),
                  ("VK_BLEND_FACTOR_SRC1_COLOR", pure VK_BLEND_FACTOR_SRC1_COLOR),
                  ("VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR",
                   pure VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR),
                  ("VK_BLEND_FACTOR_SRC1_ALPHA", pure VK_BLEND_FACTOR_SRC1_ALPHA),
                  ("VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA",
                   pure VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA)]
                 +++
                 prec 10
                   (expectP (Ident "VkBlendFactor") >>
                      (VkBlendFactor <$> step readPrec)))

pattern VK_BLEND_FACTOR_ZERO :: VkBlendFactor

pattern VK_BLEND_FACTOR_ZERO = VkBlendFactor 0

pattern VK_BLEND_FACTOR_ONE :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE = VkBlendFactor 1

pattern VK_BLEND_FACTOR_SRC_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_SRC_COLOR = VkBlendFactor 2

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = VkBlendFactor 3

pattern VK_BLEND_FACTOR_DST_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_DST_COLOR = VkBlendFactor 4

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR = VkBlendFactor 5

pattern VK_BLEND_FACTOR_SRC_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_SRC_ALPHA = VkBlendFactor 6

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = VkBlendFactor 7

pattern VK_BLEND_FACTOR_DST_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_DST_ALPHA = VkBlendFactor 8

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = VkBlendFactor 9

pattern VK_BLEND_FACTOR_CONSTANT_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_CONSTANT_COLOR = VkBlendFactor 10

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = VkBlendFactor 11

pattern VK_BLEND_FACTOR_CONSTANT_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_CONSTANT_ALPHA = VkBlendFactor 12

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = VkBlendFactor 13

pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE :: VkBlendFactor

pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE = VkBlendFactor 14

pattern VK_BLEND_FACTOR_SRC1_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_SRC1_COLOR = VkBlendFactor 15

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR = VkBlendFactor 16

pattern VK_BLEND_FACTOR_SRC1_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_SRC1_ALPHA = VkBlendFactor 17

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA = VkBlendFactor 18

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBlendOp.html VkBlendOp registry at www.khronos.org>
newtype VkBlendOp = VkBlendOp Int32
                      deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkBlendOp where
        showsPrec _ VK_BLEND_OP_ADD = showString "VK_BLEND_OP_ADD"
        showsPrec _ VK_BLEND_OP_SUBTRACT
          = showString "VK_BLEND_OP_SUBTRACT"
        showsPrec _ VK_BLEND_OP_REVERSE_SUBTRACT
          = showString "VK_BLEND_OP_REVERSE_SUBTRACT"
        showsPrec _ VK_BLEND_OP_MIN = showString "VK_BLEND_OP_MIN"
        showsPrec _ VK_BLEND_OP_MAX = showString "VK_BLEND_OP_MAX"
        showsPrec p (VkBlendOp x)
          = showParen (p >= 11) (showString "VkBlendOp " . showsPrec 11 x)

instance Read VkBlendOp where
        readPrec
          = parens
              (choose
                 [("VK_BLEND_OP_ADD", pure VK_BLEND_OP_ADD),
                  ("VK_BLEND_OP_SUBTRACT", pure VK_BLEND_OP_SUBTRACT),
                  ("VK_BLEND_OP_REVERSE_SUBTRACT",
                   pure VK_BLEND_OP_REVERSE_SUBTRACT),
                  ("VK_BLEND_OP_MIN", pure VK_BLEND_OP_MIN),
                  ("VK_BLEND_OP_MAX", pure VK_BLEND_OP_MAX)]
                 +++
                 prec 10
                   (expectP (Ident "VkBlendOp") >> (VkBlendOp <$> step readPrec)))

pattern VK_BLEND_OP_ADD :: VkBlendOp

pattern VK_BLEND_OP_ADD = VkBlendOp 0

pattern VK_BLEND_OP_SUBTRACT :: VkBlendOp

pattern VK_BLEND_OP_SUBTRACT = VkBlendOp 1

pattern VK_BLEND_OP_REVERSE_SUBTRACT :: VkBlendOp

pattern VK_BLEND_OP_REVERSE_SUBTRACT = VkBlendOp 2

pattern VK_BLEND_OP_MIN :: VkBlendOp

pattern VK_BLEND_OP_MIN = VkBlendOp 3

pattern VK_BLEND_OP_MAX :: VkBlendOp

pattern VK_BLEND_OP_MAX = VkBlendOp 4

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBorderColor.html VkBorderColor registry at www.khronos.org>
newtype VkBorderColor = VkBorderColor Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkBorderColor where
        showsPrec _ VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
          = showString "VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK"
        showsPrec _ VK_BORDER_COLOR_INT_TRANSPARENT_BLACK
          = showString "VK_BORDER_COLOR_INT_TRANSPARENT_BLACK"
        showsPrec _ VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK
          = showString "VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK"
        showsPrec _ VK_BORDER_COLOR_INT_OPAQUE_BLACK
          = showString "VK_BORDER_COLOR_INT_OPAQUE_BLACK"
        showsPrec _ VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE
          = showString "VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE"
        showsPrec _ VK_BORDER_COLOR_INT_OPAQUE_WHITE
          = showString "VK_BORDER_COLOR_INT_OPAQUE_WHITE"
        showsPrec p (VkBorderColor x)
          = showParen (p >= 11)
              (showString "VkBorderColor " . showsPrec 11 x)

instance Read VkBorderColor where
        readPrec
          = parens
              (choose
                 [("VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK",
                   pure VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK),
                  ("VK_BORDER_COLOR_INT_TRANSPARENT_BLACK",
                   pure VK_BORDER_COLOR_INT_TRANSPARENT_BLACK),
                  ("VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK",
                   pure VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK),
                  ("VK_BORDER_COLOR_INT_OPAQUE_BLACK",
                   pure VK_BORDER_COLOR_INT_OPAQUE_BLACK),
                  ("VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE",
                   pure VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE),
                  ("VK_BORDER_COLOR_INT_OPAQUE_WHITE",
                   pure VK_BORDER_COLOR_INT_OPAQUE_WHITE)]
                 +++
                 prec 10
                   (expectP (Ident "VkBorderColor") >>
                      (VkBorderColor <$> step readPrec)))

pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK :: VkBorderColor

pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VkBorderColor 0

pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK :: VkBorderColor

pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = VkBorderColor 1

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK :: VkBorderColor

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = VkBorderColor 2

pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK :: VkBorderColor

pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK = VkBorderColor 3

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE :: VkBorderColor

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = VkBorderColor 4

pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE :: VkBorderColor

pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE = VkBorderColor 5

newtype VkFramebufferCreateFlagBits = VkFramebufferCreateFlagBits VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkFramebufferCreateFlagBits where
        {-# INLINE show #-}
        show (VkFramebufferCreateFlagBits x) = show x

instance Read VkFramebufferCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkQueryPoolCreateFlagBits = VkQueryPoolCreateFlagBits VkFlags
                                      deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                FiniteBits, Storable, Real, Data, Generic)

instance Show VkQueryPoolCreateFlagBits where
        {-# INLINE show #-}
        show (VkQueryPoolCreateFlagBits x) = show x

instance Read VkQueryPoolCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkRenderPassCreateFlagBits = VkRenderPassCreateFlagBits VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkRenderPassCreateFlagBits where
        {-# INLINE show #-}
        show (VkRenderPassCreateFlagBits x) = show x

instance Read VkRenderPassCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSamplerCreateFlagBits = VkSamplerCreateFlagBits VkFlags
                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                              FiniteBits, Storable, Real, Data, Generic)

instance Show VkSamplerCreateFlagBits where
        {-# INLINE show #-}
        show (VkSamplerCreateFlagBits x) = show x

instance Read VkSamplerCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineCacheHeaderVersion.html VkPipelineCacheHeaderVersion registry at www.khronos.org>
newtype VkPipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion Int32
                                         deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                   Generic)

instance Show VkPipelineCacheHeaderVersion where
        showsPrec _ VK_PIPELINE_CACHE_HEADER_VERSION_ONE
          = showString "VK_PIPELINE_CACHE_HEADER_VERSION_ONE"
        showsPrec p (VkPipelineCacheHeaderVersion x)
          = showParen (p >= 11)
              (showString "VkPipelineCacheHeaderVersion " . showsPrec 11 x)

instance Read VkPipelineCacheHeaderVersion where
        readPrec
          = parens
              (choose
                 [("VK_PIPELINE_CACHE_HEADER_VERSION_ONE",
                   pure VK_PIPELINE_CACHE_HEADER_VERSION_ONE)]
                 +++
                 prec 10
                   (expectP (Ident "VkPipelineCacheHeaderVersion") >>
                      (VkPipelineCacheHeaderVersion <$> step readPrec)))

pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE ::
        VkPipelineCacheHeaderVersion

pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE =
        VkPipelineCacheHeaderVersion 1

newtype VkPipelineLayoutCreateFlagBits = VkPipelineLayoutCreateFlagBits VkFlags
                                           deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                     FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineLayoutCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineLayoutCreateFlagBits x) = show x

instance Read VkPipelineLayoutCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineCacheCreateFlagBits = VkPipelineCacheCreateFlagBits VkFlags
                                          deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                    FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineCacheCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineCacheCreateFlagBits x) = show x

instance Read VkPipelineCacheCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDepthStencilStateCreateFlagBits = VkPipelineDepthStencilStateCreateFlagBits VkFlags
                                                      deriving (Eq, Ord, Num, Bounded, Enum,
                                                                Integral, Bits, FiniteBits,
                                                                Storable, Real, Data, Generic)

instance Show VkPipelineDepthStencilStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineDepthStencilStateCreateFlagBits x) = show x

instance Read VkPipelineDepthStencilStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDynamicStateCreateFlagBits = VkPipelineDynamicStateCreateFlagBits VkFlags
                                                 deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                           Bits, FiniteBits, Storable, Real, Data,
                                                           Generic)

instance Show VkPipelineDynamicStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineDynamicStateCreateFlagBits x) = show x

instance Read VkPipelineDynamicStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineColorBlendStateCreateFlagBits = VkPipelineColorBlendStateCreateFlagBits VkFlags
                                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                              Bits, FiniteBits, Storable, Real,
                                                              Data, Generic)

instance Show VkPipelineColorBlendStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineColorBlendStateCreateFlagBits x) = show x

instance Read VkPipelineColorBlendStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineMultisampleStateCreateFlagBits = VkPipelineMultisampleStateCreateFlagBits VkFlags
                                                     deriving (Eq, Ord, Num, Bounded, Enum,
                                                               Integral, Bits, FiniteBits, Storable,
                                                               Real, Data, Generic)

instance Show VkPipelineMultisampleStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineMultisampleStateCreateFlagBits x) = show x

instance Read VkPipelineMultisampleStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineRasterizationStateCreateFlagBits = VkPipelineRasterizationStateCreateFlagBits VkFlags
                                                       deriving (Eq, Ord, Num, Bounded, Enum,
                                                                 Integral, Bits, FiniteBits,
                                                                 Storable, Real, Data, Generic)

instance Show VkPipelineRasterizationStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineRasterizationStateCreateFlagBits x) = show x

instance Read VkPipelineRasterizationStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineViewportStateCreateFlagBits = VkPipelineViewportStateCreateFlagBits VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkPipelineViewportStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineViewportStateCreateFlagBits x) = show x

instance Read VkPipelineViewportStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineTessellationStateCreateFlagBits = VkPipelineTessellationStateCreateFlagBits VkFlags
                                                      deriving (Eq, Ord, Num, Bounded, Enum,
                                                                Integral, Bits, FiniteBits,
                                                                Storable, Real, Data, Generic)

instance Show VkPipelineTessellationStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineTessellationStateCreateFlagBits x) = show x

instance Read VkPipelineTessellationStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineInputAssemblyStateCreateFlagBits = VkPipelineInputAssemblyStateCreateFlagBits VkFlags
                                                       deriving (Eq, Ord, Num, Bounded, Enum,
                                                                 Integral, Bits, FiniteBits,
                                                                 Storable, Real, Data, Generic)

instance Show VkPipelineInputAssemblyStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineInputAssemblyStateCreateFlagBits x) = show x

instance Read VkPipelineInputAssemblyStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineVertexInputStateCreateFlagBits = VkPipelineVertexInputStateCreateFlagBits VkFlags
                                                     deriving (Eq, Ord, Num, Bounded, Enum,
                                                               Integral, Bits, FiniteBits, Storable,
                                                               Real, Data, Generic)

instance Show VkPipelineVertexInputStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineVertexInputStateCreateFlagBits x) = show x

instance Read VkPipelineVertexInputStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineShaderStageCreateFlagBits = VkPipelineShaderStageCreateFlagBits VkFlags
                                                deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                          Bits, FiniteBits, Storable, Real, Data,
                                                          Generic)

instance Show VkPipelineShaderStageCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineShaderStageCreateFlagBits x) = show x

instance Read VkPipelineShaderStageCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorSetLayoutCreateFlagBits.html VkDescriptorSetLayoutCreateFlagBits registry at www.khronos.org>
newtype VkDescriptorSetLayoutCreateFlagBits = VkDescriptorSetLayoutCreateFlagBits Int32
                                                deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                          Storable, Enum, Data, Generic)

instance Show VkDescriptorSetLayoutCreateFlagBits where
        showsPrec p (VkDescriptorSetLayoutCreateFlagBits x)
          = showParen (p >= 11)
              (showString "VkDescriptorSetLayoutCreateFlagBits " .
                 showsPrec 11 x)

instance Read VkDescriptorSetLayoutCreateFlagBits where
        readPrec
          = parens
              (choose [] +++
                 prec 10
                   (expectP (Ident "VkDescriptorSetLayoutCreateFlagBits") >>
                      (VkDescriptorSetLayoutCreateFlagBits <$> step readPrec)))

newtype VkBufferViewCreateFlagBits = VkBufferViewCreateFlagBits VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkBufferViewCreateFlagBits where
        {-# INLINE show #-}
        show (VkBufferViewCreateFlagBits x) = show x

instance Read VkBufferViewCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkInstanceCreateFlagBits = VkInstanceCreateFlagBits VkFlags
                                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                               FiniteBits, Storable, Real, Data, Generic)

instance Show VkInstanceCreateFlagBits where
        {-# INLINE show #-}
        show (VkInstanceCreateFlagBits x) = show x

instance Read VkInstanceCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkDeviceQueueCreateFlagBits where
        {-# INLINE show #-}
        show (VkDeviceQueueCreateFlagBits x) = show x

instance Read VkDeviceQueueCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBufferCreateFlagBits.html VkBufferCreateFlagBits registry at www.khronos.org>
newtype VkBufferCreateFlagBits = VkBufferCreateFlagBits Int32
                                   deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                             Enum, Data, Generic)

instance Show VkBufferCreateFlagBits where
        showsPrec _ VK_BUFFER_CREATE_SPARSE_BINDING_BIT
          = showString "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
        showsPrec _ VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
          = showString "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
        showsPrec _ VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
          = showString "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
        showsPrec p (VkBufferCreateFlagBits x)
          = showParen (p >= 11)
              (showString "VkBufferCreateFlagBits " . showsPrec 11 x)

instance Read VkBufferCreateFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_BUFFER_CREATE_SPARSE_BINDING_BIT",
                   pure VK_BUFFER_CREATE_SPARSE_BINDING_BIT),
                  ("VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT",
                   pure VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT),
                  ("VK_BUFFER_CREATE_SPARSE_ALIASED_BIT",
                   pure VK_BUFFER_CREATE_SPARSE_ALIASED_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkBufferCreateFlagBits") >>
                      (VkBufferCreateFlagBits <$> step readPrec)))

-- | Buffer should support sparse backing
--
--   bitpos = @0@
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT ::
        VkBufferCreateFlagBits

pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT =
        VkBufferCreateFlagBits 1

-- | Buffer should support sparse backing with partial residency
--
--   bitpos = @1@
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT ::
        VkBufferCreateFlagBits

pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT =
        VkBufferCreateFlagBits 2

-- | Buffer should support constent data access to physical memory ranges mapped into multiple locations of sparse buffers
--
--   bitpos = @2@
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT ::
        VkBufferCreateFlagBits

pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT =
        VkBufferCreateFlagBits 4

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBufferUsageFlagBits.html VkBufferUsageFlagBits registry at www.khronos.org>
newtype VkBufferUsageFlagBits = VkBufferUsageFlagBits Int32
                                  deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                            Data, Generic)

instance Show VkBufferUsageFlagBits where
        showsPrec _ VK_BUFFER_USAGE_TRANSFER_SRC_BIT
          = showString "VK_BUFFER_USAGE_TRANSFER_SRC_BIT"
        showsPrec _ VK_BUFFER_USAGE_TRANSFER_DST_BIT
          = showString "VK_BUFFER_USAGE_TRANSFER_DST_BIT"
        showsPrec _ VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_STORAGE_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_INDEX_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_INDEX_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_VERTEX_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT"
        showsPrec p (VkBufferUsageFlagBits x)
          = showParen (p >= 11)
              (showString "VkBufferUsageFlagBits " . showsPrec 11 x)

instance Read VkBufferUsageFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_BUFFER_USAGE_TRANSFER_SRC_BIT",
                   pure VK_BUFFER_USAGE_TRANSFER_SRC_BIT),
                  ("VK_BUFFER_USAGE_TRANSFER_DST_BIT",
                   pure VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                  ("VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_STORAGE_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_INDEX_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_VERTEX_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkBufferUsageFlagBits") >>
                      (VkBufferUsageFlagBits <$> step readPrec)))

-- | Can be used as a source of transfer operations
--
--   bitpos = @0@
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT = VkBufferUsageFlagBits 1

-- | Can be used as a destination of transfer operations
--
--   bitpos = @1@
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT = VkBufferUsageFlagBits 2

-- | Can be used as TBO
--
--   bitpos = @2@
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT ::
        VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT =
        VkBufferUsageFlagBits 4

-- | Can be used as IBO
--
--   bitpos = @3@
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT ::
        VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT =
        VkBufferUsageFlagBits 8

-- | Can be used as UBO
--
--   bitpos = @4@
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT =
        VkBufferUsageFlagBits 16

-- | Can be used as SSBO
--
--   bitpos = @5@
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT =
        VkBufferUsageFlagBits 32

-- | Can be used as source of fixed-function index fetch (index buffer)
--
--   bitpos = @6@
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT = VkBufferUsageFlagBits 64

-- | Can be used as source of fixed-function vertex fetch (VBO)
--
--   bitpos = @7@
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT =
        VkBufferUsageFlagBits 128

-- | Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
--
--   bitpos = @8@
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT ::
        VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT =
        VkBufferUsageFlagBits 256

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkColorComponentFlagBits.html VkColorComponentFlagBits registry at www.khronos.org>
newtype VkColorComponentFlagBits = VkColorComponentFlagBits Int32
                                     deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                               Enum, Data, Generic)

instance Show VkColorComponentFlagBits where
        showsPrec _ VK_COLOR_COMPONENT_R_BIT
          = showString "VK_COLOR_COMPONENT_R_BIT"
        showsPrec _ VK_COLOR_COMPONENT_G_BIT
          = showString "VK_COLOR_COMPONENT_G_BIT"
        showsPrec _ VK_COLOR_COMPONENT_B_BIT
          = showString "VK_COLOR_COMPONENT_B_BIT"
        showsPrec _ VK_COLOR_COMPONENT_A_BIT
          = showString "VK_COLOR_COMPONENT_A_BIT"
        showsPrec p (VkColorComponentFlagBits x)
          = showParen (p >= 11)
              (showString "VkColorComponentFlagBits " . showsPrec 11 x)

instance Read VkColorComponentFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_COLOR_COMPONENT_R_BIT", pure VK_COLOR_COMPONENT_R_BIT),
                  ("VK_COLOR_COMPONENT_G_BIT", pure VK_COLOR_COMPONENT_G_BIT),
                  ("VK_COLOR_COMPONENT_B_BIT", pure VK_COLOR_COMPONENT_B_BIT),
                  ("VK_COLOR_COMPONENT_A_BIT", pure VK_COLOR_COMPONENT_A_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkColorComponentFlagBits") >>
                      (VkColorComponentFlagBits <$> step readPrec)))

-- | bitpos = @0@
pattern VK_COLOR_COMPONENT_R_BIT :: VkColorComponentFlagBits

pattern VK_COLOR_COMPONENT_R_BIT = VkColorComponentFlagBits 1

-- | bitpos = @1@
pattern VK_COLOR_COMPONENT_G_BIT :: VkColorComponentFlagBits

pattern VK_COLOR_COMPONENT_G_BIT = VkColorComponentFlagBits 2

-- | bitpos = @2@
pattern VK_COLOR_COMPONENT_B_BIT :: VkColorComponentFlagBits

pattern VK_COLOR_COMPONENT_B_BIT = VkColorComponentFlagBits 4

-- | bitpos = @3@
pattern VK_COLOR_COMPONENT_A_BIT :: VkColorComponentFlagBits

pattern VK_COLOR_COMPONENT_A_BIT = VkColorComponentFlagBits 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkComponentSwizzle.html VkComponentSwizzle registry at www.khronos.org>
newtype VkComponentSwizzle = VkComponentSwizzle Int32
                               deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkComponentSwizzle where
        showsPrec _ VK_COMPONENT_SWIZZLE_IDENTITY
          = showString "VK_COMPONENT_SWIZZLE_IDENTITY"
        showsPrec _ VK_COMPONENT_SWIZZLE_ZERO
          = showString "VK_COMPONENT_SWIZZLE_ZERO"
        showsPrec _ VK_COMPONENT_SWIZZLE_ONE
          = showString "VK_COMPONENT_SWIZZLE_ONE"
        showsPrec _ VK_COMPONENT_SWIZZLE_R
          = showString "VK_COMPONENT_SWIZZLE_R"
        showsPrec _ VK_COMPONENT_SWIZZLE_G
          = showString "VK_COMPONENT_SWIZZLE_G"
        showsPrec _ VK_COMPONENT_SWIZZLE_B
          = showString "VK_COMPONENT_SWIZZLE_B"
        showsPrec _ VK_COMPONENT_SWIZZLE_A
          = showString "VK_COMPONENT_SWIZZLE_A"
        showsPrec p (VkComponentSwizzle x)
          = showParen (p >= 11)
              (showString "VkComponentSwizzle " . showsPrec 11 x)

instance Read VkComponentSwizzle where
        readPrec
          = parens
              (choose
                 [("VK_COMPONENT_SWIZZLE_IDENTITY",
                   pure VK_COMPONENT_SWIZZLE_IDENTITY),
                  ("VK_COMPONENT_SWIZZLE_ZERO", pure VK_COMPONENT_SWIZZLE_ZERO),
                  ("VK_COMPONENT_SWIZZLE_ONE", pure VK_COMPONENT_SWIZZLE_ONE),
                  ("VK_COMPONENT_SWIZZLE_R", pure VK_COMPONENT_SWIZZLE_R),
                  ("VK_COMPONENT_SWIZZLE_G", pure VK_COMPONENT_SWIZZLE_G),
                  ("VK_COMPONENT_SWIZZLE_B", pure VK_COMPONENT_SWIZZLE_B),
                  ("VK_COMPONENT_SWIZZLE_A", pure VK_COMPONENT_SWIZZLE_A)]
                 +++
                 prec 10
                   (expectP (Ident "VkComponentSwizzle") >>
                      (VkComponentSwizzle <$> step readPrec)))

pattern VK_COMPONENT_SWIZZLE_IDENTITY :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_IDENTITY = VkComponentSwizzle 0

pattern VK_COMPONENT_SWIZZLE_ZERO :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_ZERO = VkComponentSwizzle 1

pattern VK_COMPONENT_SWIZZLE_ONE :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_ONE = VkComponentSwizzle 2

pattern VK_COMPONENT_SWIZZLE_R :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_R = VkComponentSwizzle 3

pattern VK_COMPONENT_SWIZZLE_G :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_G = VkComponentSwizzle 4

pattern VK_COMPONENT_SWIZZLE_B :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_B = VkComponentSwizzle 5

pattern VK_COMPONENT_SWIZZLE_A :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_A = VkComponentSwizzle 6

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandPoolCreateFlagBits.html VkCommandPoolCreateFlagBits registry at www.khronos.org>
newtype VkCommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits Int32
                                        deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                                  Enum, Data, Generic)

instance Show VkCommandPoolCreateFlagBits where
        showsPrec _ VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
          = showString "VK_COMMAND_POOL_CREATE_TRANSIENT_BIT"
        showsPrec _ VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
          = showString "VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
        showsPrec p (VkCommandPoolCreateFlagBits x)
          = showParen (p >= 11)
              (showString "VkCommandPoolCreateFlagBits " . showsPrec 11 x)

instance Read VkCommandPoolCreateFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_POOL_CREATE_TRANSIENT_BIT",
                   pure VK_COMMAND_POOL_CREATE_TRANSIENT_BIT),
                  ("VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT",
                   pure VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandPoolCreateFlagBits") >>
                      (VkCommandPoolCreateFlagBits <$> step readPrec)))

-- | Command buffers have a short lifetime
--
--   bitpos = @0@
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT ::
        VkCommandPoolCreateFlagBits

pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT =
        VkCommandPoolCreateFlagBits 1

-- | Command buffers may release their memory individually
--
--   bitpos = @1@
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT ::
        VkCommandPoolCreateFlagBits

pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT =
        VkCommandPoolCreateFlagBits 2

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandPoolResetFlagBits.html VkCommandPoolResetFlagBits registry at www.khronos.org>
newtype VkCommandPoolResetFlagBits = VkCommandPoolResetFlagBits Int32
                                       deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                                 Enum, Data, Generic)

instance Show VkCommandPoolResetFlagBits where
        showsPrec _ VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
          = showString "VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
        showsPrec p (VkCommandPoolResetFlagBits x)
          = showParen (p >= 11)
              (showString "VkCommandPoolResetFlagBits " . showsPrec 11 x)

instance Read VkCommandPoolResetFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT",
                   pure VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandPoolResetFlagBits") >>
                      (VkCommandPoolResetFlagBits <$> step readPrec)))

-- | Release resources owned by the pool
--
--   bitpos = @0@
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT ::
        VkCommandPoolResetFlagBits

pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT =
        VkCommandPoolResetFlagBits 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferResetFlagBits.html VkCommandBufferResetFlagBits registry at www.khronos.org>
newtype VkCommandBufferResetFlagBits = VkCommandBufferResetFlagBits Int32
                                         deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                   Storable, Enum, Data, Generic)

instance Show VkCommandBufferResetFlagBits where
        showsPrec _ VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
          = showString "VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
        showsPrec p (VkCommandBufferResetFlagBits x)
          = showParen (p >= 11)
              (showString "VkCommandBufferResetFlagBits " . showsPrec 11 x)

instance Read VkCommandBufferResetFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT",
                   pure VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandBufferResetFlagBits") >>
                      (VkCommandBufferResetFlagBits <$> step readPrec)))

-- | Release resources owned by the buffer
--
--   bitpos = @0@
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT ::
        VkCommandBufferResetFlagBits

pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT =
        VkCommandBufferResetFlagBits 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferLevel.html VkCommandBufferLevel registry at www.khronos.org>
newtype VkCommandBufferLevel = VkCommandBufferLevel Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkCommandBufferLevel where
        showsPrec _ VK_COMMAND_BUFFER_LEVEL_PRIMARY
          = showString "VK_COMMAND_BUFFER_LEVEL_PRIMARY"
        showsPrec _ VK_COMMAND_BUFFER_LEVEL_SECONDARY
          = showString "VK_COMMAND_BUFFER_LEVEL_SECONDARY"
        showsPrec p (VkCommandBufferLevel x)
          = showParen (p >= 11)
              (showString "VkCommandBufferLevel " . showsPrec 11 x)

instance Read VkCommandBufferLevel where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_BUFFER_LEVEL_PRIMARY",
                   pure VK_COMMAND_BUFFER_LEVEL_PRIMARY),
                  ("VK_COMMAND_BUFFER_LEVEL_SECONDARY",
                   pure VK_COMMAND_BUFFER_LEVEL_SECONDARY)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandBufferLevel") >>
                      (VkCommandBufferLevel <$> step readPrec)))

pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY :: VkCommandBufferLevel

pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY :: VkCommandBufferLevel

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferUsageFlagBits.html VkCommandBufferUsageFlagBits registry at www.khronos.org>
newtype VkCommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits Int32
                                         deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                   Storable, Enum, Data, Generic)

instance Show VkCommandBufferUsageFlagBits where
        showsPrec _ VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
          = showString "VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
        showsPrec _ VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
          = showString "VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
        showsPrec _ VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
          = showString "VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
        showsPrec p (VkCommandBufferUsageFlagBits x)
          = showParen (p >= 11)
              (showString "VkCommandBufferUsageFlagBits " . showsPrec 11 x)

instance Read VkCommandBufferUsageFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT",
                   pure VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT),
                  ("VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT",
                   pure VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT),
                  ("VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT",
                   pure VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandBufferUsageFlagBits") >>
                      (VkCommandBufferUsageFlagBits <$> step readPrec)))

-- | bitpos = @0@
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT ::
        VkCommandBufferUsageFlagBits

pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT =
        VkCommandBufferUsageFlagBits 1

-- | bitpos = @1@
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT ::
        VkCommandBufferUsageFlagBits

pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT =
        VkCommandBufferUsageFlagBits 2

-- | Command buffer may be submitted/executed more than once simultaneously
--
--   bitpos = @2@
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT ::
        VkCommandBufferUsageFlagBits

pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT =
        VkCommandBufferUsageFlagBits 4

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCompareOp.html VkCompareOp registry at www.khronos.org>
newtype VkCompareOp = VkCompareOp Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkCompareOp where
        showsPrec _ VK_COMPARE_OP_NEVER = showString "VK_COMPARE_OP_NEVER"
        showsPrec _ VK_COMPARE_OP_LESS = showString "VK_COMPARE_OP_LESS"
        showsPrec _ VK_COMPARE_OP_EQUAL = showString "VK_COMPARE_OP_EQUAL"
        showsPrec _ VK_COMPARE_OP_LESS_OR_EQUAL
          = showString "VK_COMPARE_OP_LESS_OR_EQUAL"
        showsPrec _ VK_COMPARE_OP_GREATER
          = showString "VK_COMPARE_OP_GREATER"
        showsPrec _ VK_COMPARE_OP_NOT_EQUAL
          = showString "VK_COMPARE_OP_NOT_EQUAL"
        showsPrec _ VK_COMPARE_OP_GREATER_OR_EQUAL
          = showString "VK_COMPARE_OP_GREATER_OR_EQUAL"
        showsPrec _ VK_COMPARE_OP_ALWAYS
          = showString "VK_COMPARE_OP_ALWAYS"
        showsPrec p (VkCompareOp x)
          = showParen (p >= 11) (showString "VkCompareOp " . showsPrec 11 x)

instance Read VkCompareOp where
        readPrec
          = parens
              (choose
                 [("VK_COMPARE_OP_NEVER", pure VK_COMPARE_OP_NEVER),
                  ("VK_COMPARE_OP_LESS", pure VK_COMPARE_OP_LESS),
                  ("VK_COMPARE_OP_EQUAL", pure VK_COMPARE_OP_EQUAL),
                  ("VK_COMPARE_OP_LESS_OR_EQUAL", pure VK_COMPARE_OP_LESS_OR_EQUAL),
                  ("VK_COMPARE_OP_GREATER", pure VK_COMPARE_OP_GREATER),
                  ("VK_COMPARE_OP_NOT_EQUAL", pure VK_COMPARE_OP_NOT_EQUAL),
                  ("VK_COMPARE_OP_GREATER_OR_EQUAL",
                   pure VK_COMPARE_OP_GREATER_OR_EQUAL),
                  ("VK_COMPARE_OP_ALWAYS", pure VK_COMPARE_OP_ALWAYS)]
                 +++
                 prec 10
                   (expectP (Ident "VkCompareOp") >> (VkCompareOp <$> step readPrec)))

pattern VK_COMPARE_OP_NEVER :: VkCompareOp

pattern VK_COMPARE_OP_NEVER = VkCompareOp 0

pattern VK_COMPARE_OP_LESS :: VkCompareOp

pattern VK_COMPARE_OP_LESS = VkCompareOp 1

pattern VK_COMPARE_OP_EQUAL :: VkCompareOp

pattern VK_COMPARE_OP_EQUAL = VkCompareOp 2

pattern VK_COMPARE_OP_LESS_OR_EQUAL :: VkCompareOp

pattern VK_COMPARE_OP_LESS_OR_EQUAL = VkCompareOp 3

pattern VK_COMPARE_OP_GREATER :: VkCompareOp

pattern VK_COMPARE_OP_GREATER = VkCompareOp 4

pattern VK_COMPARE_OP_NOT_EQUAL :: VkCompareOp

pattern VK_COMPARE_OP_NOT_EQUAL = VkCompareOp 5

pattern VK_COMPARE_OP_GREATER_OR_EQUAL :: VkCompareOp

pattern VK_COMPARE_OP_GREATER_OR_EQUAL = VkCompareOp 6

pattern VK_COMPARE_OP_ALWAYS :: VkCompareOp

pattern VK_COMPARE_OP_ALWAYS = VkCompareOp 7

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCullModeFlagBits.html VkCullModeFlagBits registry at www.khronos.org>
newtype VkCullModeFlagBits = VkCullModeFlagBits Int32
                               deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                         Data, Generic)

instance Show VkCullModeFlagBits where
        showsPrec _ VK_CULL_MODE_NONE = showString "VK_CULL_MODE_NONE"
        showsPrec _ VK_CULL_MODE_FRONT_BIT
          = showString "VK_CULL_MODE_FRONT_BIT"
        showsPrec _ VK_CULL_MODE_BACK_BIT
          = showString "VK_CULL_MODE_BACK_BIT"
        showsPrec _ VK_CULL_MODE_FRONT_AND_BACK
          = showString "VK_CULL_MODE_FRONT_AND_BACK"
        showsPrec p (VkCullModeFlagBits x)
          = showParen (p >= 11)
              (showString "VkCullModeFlagBits " . showsPrec 11 x)

instance Read VkCullModeFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_CULL_MODE_NONE", pure VK_CULL_MODE_NONE),
                  ("VK_CULL_MODE_FRONT_BIT", pure VK_CULL_MODE_FRONT_BIT),
                  ("VK_CULL_MODE_BACK_BIT", pure VK_CULL_MODE_BACK_BIT),
                  ("VK_CULL_MODE_FRONT_AND_BACK", pure VK_CULL_MODE_FRONT_AND_BACK)]
                 +++
                 prec 10
                   (expectP (Ident "VkCullModeFlagBits") >>
                      (VkCullModeFlagBits <$> step readPrec)))

pattern VK_CULL_MODE_NONE :: VkCullModeFlagBits

pattern VK_CULL_MODE_NONE = VkCullModeFlagBits 0

-- | bitpos = @0@
pattern VK_CULL_MODE_FRONT_BIT :: VkCullModeFlagBits

pattern VK_CULL_MODE_FRONT_BIT = VkCullModeFlagBits 1

-- | bitpos = @1@
pattern VK_CULL_MODE_BACK_BIT :: VkCullModeFlagBits

pattern VK_CULL_MODE_BACK_BIT = VkCullModeFlagBits 2

pattern VK_CULL_MODE_FRONT_AND_BACK :: VkCullModeFlagBits

pattern VK_CULL_MODE_FRONT_AND_BACK = VkCullModeFlagBits 3

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorType.html VkDescriptorType registry at www.khronos.org>
newtype VkDescriptorType = VkDescriptorType Int32
                             deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkDescriptorType where
        showsPrec _ VK_DESCRIPTOR_TYPE_SAMPLER
          = showString "VK_DESCRIPTOR_TYPE_SAMPLER"
        showsPrec _ VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          = showString "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
        showsPrec _ VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
          = showString "VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE"
        showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
          = showString "VK_DESCRIPTOR_TYPE_STORAGE_IMAGE"
        showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
          = showString "VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
        showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
          = showString "VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
        showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
          = showString "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
        showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
          = showString "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER"
        showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
          = showString "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
        showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
          = showString "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
        showsPrec _ VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
          = showString "VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
        showsPrec p (VkDescriptorType x)
          = showParen (p >= 11)
              (showString "VkDescriptorType " . showsPrec 11 x)

instance Read VkDescriptorType where
        readPrec
          = parens
              (choose
                 [("VK_DESCRIPTOR_TYPE_SAMPLER", pure VK_DESCRIPTOR_TYPE_SAMPLER),
                  ("VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER",
                   pure VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                  ("VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE",
                   pure VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                  ("VK_DESCRIPTOR_TYPE_STORAGE_IMAGE",
                   pure VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                  ("VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER",
                   pure VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER),
                  ("VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER",
                   pure VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER),
                  ("VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER",
                   pure VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                  ("VK_DESCRIPTOR_TYPE_STORAGE_BUFFER",
                   pure VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                  ("VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC",
                   pure VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC),
                  ("VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC",
                   pure VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC),
                  ("VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT",
                   pure VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDescriptorType") >>
                      (VkDescriptorType <$> step readPrec)))

pattern VK_DESCRIPTOR_TYPE_SAMPLER :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_SAMPLER = VkDescriptorType 0

pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER ::
        VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER =
        VkDescriptorType 1

pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = VkDescriptorType 2

pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = VkDescriptorType 3

pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER =
        VkDescriptorType 4

pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER =
        VkDescriptorType 5

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = VkDescriptorType 6

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = VkDescriptorType 7

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC ::
        VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC =
        VkDescriptorType 8

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC ::
        VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC =
        VkDescriptorType 9

pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = VkDescriptorType 10

newtype VkDeviceCreateFlagBits = VkDeviceCreateFlagBits VkFlags
                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                             FiniteBits, Storable, Real, Data, Generic)

instance Show VkDeviceCreateFlagBits where
        {-# INLINE show #-}
        show (VkDeviceCreateFlagBits x) = show x

instance Read VkDeviceCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDynamicState.html VkDynamicState registry at www.khronos.org>
newtype VkDynamicState = VkDynamicState Int32
                           deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkDynamicState where
        showsPrec _ VK_DYNAMIC_STATE_VIEWPORT
          = showString "VK_DYNAMIC_STATE_VIEWPORT"
        showsPrec _ VK_DYNAMIC_STATE_SCISSOR
          = showString "VK_DYNAMIC_STATE_SCISSOR"
        showsPrec _ VK_DYNAMIC_STATE_LINE_WIDTH
          = showString "VK_DYNAMIC_STATE_LINE_WIDTH"
        showsPrec _ VK_DYNAMIC_STATE_DEPTH_BIAS
          = showString "VK_DYNAMIC_STATE_DEPTH_BIAS"
        showsPrec _ VK_DYNAMIC_STATE_BLEND_CONSTANTS
          = showString "VK_DYNAMIC_STATE_BLEND_CONSTANTS"
        showsPrec _ VK_DYNAMIC_STATE_DEPTH_BOUNDS
          = showString "VK_DYNAMIC_STATE_DEPTH_BOUNDS"
        showsPrec _ VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK
          = showString "VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK"
        showsPrec _ VK_DYNAMIC_STATE_STENCIL_WRITE_MASK
          = showString "VK_DYNAMIC_STATE_STENCIL_WRITE_MASK"
        showsPrec _ VK_DYNAMIC_STATE_STENCIL_REFERENCE
          = showString "VK_DYNAMIC_STATE_STENCIL_REFERENCE"
        showsPrec p (VkDynamicState x)
          = showParen (p >= 11)
              (showString "VkDynamicState " . showsPrec 11 x)

instance Read VkDynamicState where
        readPrec
          = parens
              (choose
                 [("VK_DYNAMIC_STATE_VIEWPORT", pure VK_DYNAMIC_STATE_VIEWPORT),
                  ("VK_DYNAMIC_STATE_SCISSOR", pure VK_DYNAMIC_STATE_SCISSOR),
                  ("VK_DYNAMIC_STATE_LINE_WIDTH", pure VK_DYNAMIC_STATE_LINE_WIDTH),
                  ("VK_DYNAMIC_STATE_DEPTH_BIAS", pure VK_DYNAMIC_STATE_DEPTH_BIAS),
                  ("VK_DYNAMIC_STATE_BLEND_CONSTANTS",
                   pure VK_DYNAMIC_STATE_BLEND_CONSTANTS),
                  ("VK_DYNAMIC_STATE_DEPTH_BOUNDS",
                   pure VK_DYNAMIC_STATE_DEPTH_BOUNDS),
                  ("VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK",
                   pure VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK),
                  ("VK_DYNAMIC_STATE_STENCIL_WRITE_MASK",
                   pure VK_DYNAMIC_STATE_STENCIL_WRITE_MASK),
                  ("VK_DYNAMIC_STATE_STENCIL_REFERENCE",
                   pure VK_DYNAMIC_STATE_STENCIL_REFERENCE)]
                 +++
                 prec 10
                   (expectP (Ident "VkDynamicState") >>
                      (VkDynamicState <$> step readPrec)))

pattern VK_DYNAMIC_STATE_VIEWPORT :: VkDynamicState

pattern VK_DYNAMIC_STATE_VIEWPORT = VkDynamicState 0

pattern VK_DYNAMIC_STATE_SCISSOR :: VkDynamicState

pattern VK_DYNAMIC_STATE_SCISSOR = VkDynamicState 1

pattern VK_DYNAMIC_STATE_LINE_WIDTH :: VkDynamicState

pattern VK_DYNAMIC_STATE_LINE_WIDTH = VkDynamicState 2

pattern VK_DYNAMIC_STATE_DEPTH_BIAS :: VkDynamicState

pattern VK_DYNAMIC_STATE_DEPTH_BIAS = VkDynamicState 3

pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS :: VkDynamicState

pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS = VkDynamicState 4

pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS :: VkDynamicState

pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS = VkDynamicState 5

pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK :: VkDynamicState

pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = VkDynamicState 6

pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK :: VkDynamicState

pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = VkDynamicState 7

pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE :: VkDynamicState

pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE = VkDynamicState 8

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFenceCreateFlagBits.html VkFenceCreateFlagBits registry at www.khronos.org>
newtype VkFenceCreateFlagBits = VkFenceCreateFlagBits Int32
                                  deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                            Data, Generic)

instance Show VkFenceCreateFlagBits where
        showsPrec _ VK_FENCE_CREATE_SIGNALED_BIT
          = showString "VK_FENCE_CREATE_SIGNALED_BIT"
        showsPrec p (VkFenceCreateFlagBits x)
          = showParen (p >= 11)
              (showString "VkFenceCreateFlagBits " . showsPrec 11 x)

instance Read VkFenceCreateFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_FENCE_CREATE_SIGNALED_BIT",
                   pure VK_FENCE_CREATE_SIGNALED_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkFenceCreateFlagBits") >>
                      (VkFenceCreateFlagBits <$> step readPrec)))

-- | bitpos = @0@
pattern VK_FENCE_CREATE_SIGNALED_BIT :: VkFenceCreateFlagBits

pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateFlagBits 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPolygonMode.html VkPolygonMode registry at www.khronos.org>
newtype VkPolygonMode = VkPolygonMode Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkPolygonMode where
        showsPrec _ VK_POLYGON_MODE_FILL
          = showString "VK_POLYGON_MODE_FILL"
        showsPrec _ VK_POLYGON_MODE_LINE
          = showString "VK_POLYGON_MODE_LINE"
        showsPrec _ VK_POLYGON_MODE_POINT
          = showString "VK_POLYGON_MODE_POINT"
        showsPrec p (VkPolygonMode x)
          = showParen (p >= 11)
              (showString "VkPolygonMode " . showsPrec 11 x)

instance Read VkPolygonMode where
        readPrec
          = parens
              (choose
                 [("VK_POLYGON_MODE_FILL", pure VK_POLYGON_MODE_FILL),
                  ("VK_POLYGON_MODE_LINE", pure VK_POLYGON_MODE_LINE),
                  ("VK_POLYGON_MODE_POINT", pure VK_POLYGON_MODE_POINT)]
                 +++
                 prec 10
                   (expectP (Ident "VkPolygonMode") >>
                      (VkPolygonMode <$> step readPrec)))

pattern VK_POLYGON_MODE_FILL :: VkPolygonMode

pattern VK_POLYGON_MODE_FILL = VkPolygonMode 0

pattern VK_POLYGON_MODE_LINE :: VkPolygonMode

pattern VK_POLYGON_MODE_LINE = VkPolygonMode 1

pattern VK_POLYGON_MODE_POINT :: VkPolygonMode

pattern VK_POLYGON_MODE_POINT = VkPolygonMode 2

-- | Vulkan format definitions
--
--   type = @enum@
--
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFormat.html VkFormat registry at www.khronos.org>
newtype VkFormat = VkFormat Int32
                     deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkFormat where
        showsPrec _ VK_FORMAT_UNDEFINED = showString "VK_FORMAT_UNDEFINED"
        showsPrec _ VK_FORMAT_R4G4_UNORM_PACK8
          = showString "VK_FORMAT_R4G4_UNORM_PACK8"
        showsPrec _ VK_FORMAT_R4G4B4A4_UNORM_PACK16
          = showString "VK_FORMAT_R4G4B4A4_UNORM_PACK16"
        showsPrec _ VK_FORMAT_B4G4R4A4_UNORM_PACK16
          = showString "VK_FORMAT_B4G4R4A4_UNORM_PACK16"
        showsPrec _ VK_FORMAT_R5G6B5_UNORM_PACK16
          = showString "VK_FORMAT_R5G6B5_UNORM_PACK16"
        showsPrec _ VK_FORMAT_B5G6R5_UNORM_PACK16
          = showString "VK_FORMAT_B5G6R5_UNORM_PACK16"
        showsPrec _ VK_FORMAT_R5G5B5A1_UNORM_PACK16
          = showString "VK_FORMAT_R5G5B5A1_UNORM_PACK16"
        showsPrec _ VK_FORMAT_B5G5R5A1_UNORM_PACK16
          = showString "VK_FORMAT_B5G5R5A1_UNORM_PACK16"
        showsPrec _ VK_FORMAT_A1R5G5B5_UNORM_PACK16
          = showString "VK_FORMAT_A1R5G5B5_UNORM_PACK16"
        showsPrec _ VK_FORMAT_R8_UNORM = showString "VK_FORMAT_R8_UNORM"
        showsPrec _ VK_FORMAT_R8_SNORM = showString "VK_FORMAT_R8_SNORM"
        showsPrec _ VK_FORMAT_R8_USCALED
          = showString "VK_FORMAT_R8_USCALED"
        showsPrec _ VK_FORMAT_R8_SSCALED
          = showString "VK_FORMAT_R8_SSCALED"
        showsPrec _ VK_FORMAT_R8_UINT = showString "VK_FORMAT_R8_UINT"
        showsPrec _ VK_FORMAT_R8_SINT = showString "VK_FORMAT_R8_SINT"
        showsPrec _ VK_FORMAT_R8_SRGB = showString "VK_FORMAT_R8_SRGB"
        showsPrec _ VK_FORMAT_R8G8_UNORM
          = showString "VK_FORMAT_R8G8_UNORM"
        showsPrec _ VK_FORMAT_R8G8_SNORM
          = showString "VK_FORMAT_R8G8_SNORM"
        showsPrec _ VK_FORMAT_R8G8_USCALED
          = showString "VK_FORMAT_R8G8_USCALED"
        showsPrec _ VK_FORMAT_R8G8_SSCALED
          = showString "VK_FORMAT_R8G8_SSCALED"
        showsPrec _ VK_FORMAT_R8G8_UINT = showString "VK_FORMAT_R8G8_UINT"
        showsPrec _ VK_FORMAT_R8G8_SINT = showString "VK_FORMAT_R8G8_SINT"
        showsPrec _ VK_FORMAT_R8G8_SRGB = showString "VK_FORMAT_R8G8_SRGB"
        showsPrec _ VK_FORMAT_R8G8B8_UNORM
          = showString "VK_FORMAT_R8G8B8_UNORM"
        showsPrec _ VK_FORMAT_R8G8B8_SNORM
          = showString "VK_FORMAT_R8G8B8_SNORM"
        showsPrec _ VK_FORMAT_R8G8B8_USCALED
          = showString "VK_FORMAT_R8G8B8_USCALED"
        showsPrec _ VK_FORMAT_R8G8B8_SSCALED
          = showString "VK_FORMAT_R8G8B8_SSCALED"
        showsPrec _ VK_FORMAT_R8G8B8_UINT
          = showString "VK_FORMAT_R8G8B8_UINT"
        showsPrec _ VK_FORMAT_R8G8B8_SINT
          = showString "VK_FORMAT_R8G8B8_SINT"
        showsPrec _ VK_FORMAT_R8G8B8_SRGB
          = showString "VK_FORMAT_R8G8B8_SRGB"
        showsPrec _ VK_FORMAT_B8G8R8_UNORM
          = showString "VK_FORMAT_B8G8R8_UNORM"
        showsPrec _ VK_FORMAT_B8G8R8_SNORM
          = showString "VK_FORMAT_B8G8R8_SNORM"
        showsPrec _ VK_FORMAT_B8G8R8_USCALED
          = showString "VK_FORMAT_B8G8R8_USCALED"
        showsPrec _ VK_FORMAT_B8G8R8_SSCALED
          = showString "VK_FORMAT_B8G8R8_SSCALED"
        showsPrec _ VK_FORMAT_B8G8R8_UINT
          = showString "VK_FORMAT_B8G8R8_UINT"
        showsPrec _ VK_FORMAT_B8G8R8_SINT
          = showString "VK_FORMAT_B8G8R8_SINT"
        showsPrec _ VK_FORMAT_B8G8R8_SRGB
          = showString "VK_FORMAT_B8G8R8_SRGB"
        showsPrec _ VK_FORMAT_R8G8B8A8_UNORM
          = showString "VK_FORMAT_R8G8B8A8_UNORM"
        showsPrec _ VK_FORMAT_R8G8B8A8_SNORM
          = showString "VK_FORMAT_R8G8B8A8_SNORM"
        showsPrec _ VK_FORMAT_R8G8B8A8_USCALED
          = showString "VK_FORMAT_R8G8B8A8_USCALED"
        showsPrec _ VK_FORMAT_R8G8B8A8_SSCALED
          = showString "VK_FORMAT_R8G8B8A8_SSCALED"
        showsPrec _ VK_FORMAT_R8G8B8A8_UINT
          = showString "VK_FORMAT_R8G8B8A8_UINT"
        showsPrec _ VK_FORMAT_R8G8B8A8_SINT
          = showString "VK_FORMAT_R8G8B8A8_SINT"
        showsPrec _ VK_FORMAT_R8G8B8A8_SRGB
          = showString "VK_FORMAT_R8G8B8A8_SRGB"
        showsPrec _ VK_FORMAT_B8G8R8A8_UNORM
          = showString "VK_FORMAT_B8G8R8A8_UNORM"
        showsPrec _ VK_FORMAT_B8G8R8A8_SNORM
          = showString "VK_FORMAT_B8G8R8A8_SNORM"
        showsPrec _ VK_FORMAT_B8G8R8A8_USCALED
          = showString "VK_FORMAT_B8G8R8A8_USCALED"
        showsPrec _ VK_FORMAT_B8G8R8A8_SSCALED
          = showString "VK_FORMAT_B8G8R8A8_SSCALED"
        showsPrec _ VK_FORMAT_B8G8R8A8_UINT
          = showString "VK_FORMAT_B8G8R8A8_UINT"
        showsPrec _ VK_FORMAT_B8G8R8A8_SINT
          = showString "VK_FORMAT_B8G8R8A8_SINT"
        showsPrec _ VK_FORMAT_B8G8R8A8_SRGB
          = showString "VK_FORMAT_B8G8R8A8_SRGB"
        showsPrec _ VK_FORMAT_A8B8G8R8_UNORM_PACK32
          = showString "VK_FORMAT_A8B8G8R8_UNORM_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_SNORM_PACK32
          = showString "VK_FORMAT_A8B8G8R8_SNORM_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_USCALED_PACK32
          = showString "VK_FORMAT_A8B8G8R8_USCALED_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_SSCALED_PACK32
          = showString "VK_FORMAT_A8B8G8R8_SSCALED_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_UINT_PACK32
          = showString "VK_FORMAT_A8B8G8R8_UINT_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_SINT_PACK32
          = showString "VK_FORMAT_A8B8G8R8_SINT_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_SRGB_PACK32
          = showString "VK_FORMAT_A8B8G8R8_SRGB_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_UNORM_PACK32
          = showString "VK_FORMAT_A2R10G10B10_UNORM_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_SNORM_PACK32
          = showString "VK_FORMAT_A2R10G10B10_SNORM_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_USCALED_PACK32
          = showString "VK_FORMAT_A2R10G10B10_USCALED_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_SSCALED_PACK32
          = showString "VK_FORMAT_A2R10G10B10_SSCALED_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_UINT_PACK32
          = showString "VK_FORMAT_A2R10G10B10_UINT_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_SINT_PACK32
          = showString "VK_FORMAT_A2R10G10B10_SINT_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_UNORM_PACK32
          = showString "VK_FORMAT_A2B10G10R10_UNORM_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_SNORM_PACK32
          = showString "VK_FORMAT_A2B10G10R10_SNORM_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_USCALED_PACK32
          = showString "VK_FORMAT_A2B10G10R10_USCALED_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_SSCALED_PACK32
          = showString "VK_FORMAT_A2B10G10R10_SSCALED_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_UINT_PACK32
          = showString "VK_FORMAT_A2B10G10R10_UINT_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_SINT_PACK32
          = showString "VK_FORMAT_A2B10G10R10_SINT_PACK32"
        showsPrec _ VK_FORMAT_R16_UNORM = showString "VK_FORMAT_R16_UNORM"
        showsPrec _ VK_FORMAT_R16_SNORM = showString "VK_FORMAT_R16_SNORM"
        showsPrec _ VK_FORMAT_R16_USCALED
          = showString "VK_FORMAT_R16_USCALED"
        showsPrec _ VK_FORMAT_R16_SSCALED
          = showString "VK_FORMAT_R16_SSCALED"
        showsPrec _ VK_FORMAT_R16_UINT = showString "VK_FORMAT_R16_UINT"
        showsPrec _ VK_FORMAT_R16_SINT = showString "VK_FORMAT_R16_SINT"
        showsPrec _ VK_FORMAT_R16_SFLOAT
          = showString "VK_FORMAT_R16_SFLOAT"
        showsPrec _ VK_FORMAT_R16G16_UNORM
          = showString "VK_FORMAT_R16G16_UNORM"
        showsPrec _ VK_FORMAT_R16G16_SNORM
          = showString "VK_FORMAT_R16G16_SNORM"
        showsPrec _ VK_FORMAT_R16G16_USCALED
          = showString "VK_FORMAT_R16G16_USCALED"
        showsPrec _ VK_FORMAT_R16G16_SSCALED
          = showString "VK_FORMAT_R16G16_SSCALED"
        showsPrec _ VK_FORMAT_R16G16_UINT
          = showString "VK_FORMAT_R16G16_UINT"
        showsPrec _ VK_FORMAT_R16G16_SINT
          = showString "VK_FORMAT_R16G16_SINT"
        showsPrec _ VK_FORMAT_R16G16_SFLOAT
          = showString "VK_FORMAT_R16G16_SFLOAT"
        showsPrec _ VK_FORMAT_R16G16B16_UNORM
          = showString "VK_FORMAT_R16G16B16_UNORM"
        showsPrec _ VK_FORMAT_R16G16B16_SNORM
          = showString "VK_FORMAT_R16G16B16_SNORM"
        showsPrec _ VK_FORMAT_R16G16B16_USCALED
          = showString "VK_FORMAT_R16G16B16_USCALED"
        showsPrec _ VK_FORMAT_R16G16B16_SSCALED
          = showString "VK_FORMAT_R16G16B16_SSCALED"
        showsPrec _ VK_FORMAT_R16G16B16_UINT
          = showString "VK_FORMAT_R16G16B16_UINT"
        showsPrec _ VK_FORMAT_R16G16B16_SINT
          = showString "VK_FORMAT_R16G16B16_SINT"
        showsPrec _ VK_FORMAT_R16G16B16_SFLOAT
          = showString "VK_FORMAT_R16G16B16_SFLOAT"
        showsPrec _ VK_FORMAT_R16G16B16A16_UNORM
          = showString "VK_FORMAT_R16G16B16A16_UNORM"
        showsPrec _ VK_FORMAT_R16G16B16A16_SNORM
          = showString "VK_FORMAT_R16G16B16A16_SNORM"
        showsPrec _ VK_FORMAT_R16G16B16A16_USCALED
          = showString "VK_FORMAT_R16G16B16A16_USCALED"
        showsPrec _ VK_FORMAT_R16G16B16A16_SSCALED
          = showString "VK_FORMAT_R16G16B16A16_SSCALED"
        showsPrec _ VK_FORMAT_R16G16B16A16_UINT
          = showString "VK_FORMAT_R16G16B16A16_UINT"
        showsPrec _ VK_FORMAT_R16G16B16A16_SINT
          = showString "VK_FORMAT_R16G16B16A16_SINT"
        showsPrec _ VK_FORMAT_R16G16B16A16_SFLOAT
          = showString "VK_FORMAT_R16G16B16A16_SFLOAT"
        showsPrec _ VK_FORMAT_R32_UINT = showString "VK_FORMAT_R32_UINT"
        showsPrec _ VK_FORMAT_R32_SINT = showString "VK_FORMAT_R32_SINT"
        showsPrec _ VK_FORMAT_R32_SFLOAT
          = showString "VK_FORMAT_R32_SFLOAT"
        showsPrec _ VK_FORMAT_R32G32_UINT
          = showString "VK_FORMAT_R32G32_UINT"
        showsPrec _ VK_FORMAT_R32G32_SINT
          = showString "VK_FORMAT_R32G32_SINT"
        showsPrec _ VK_FORMAT_R32G32_SFLOAT
          = showString "VK_FORMAT_R32G32_SFLOAT"
        showsPrec _ VK_FORMAT_R32G32B32_UINT
          = showString "VK_FORMAT_R32G32B32_UINT"
        showsPrec _ VK_FORMAT_R32G32B32_SINT
          = showString "VK_FORMAT_R32G32B32_SINT"
        showsPrec _ VK_FORMAT_R32G32B32_SFLOAT
          = showString "VK_FORMAT_R32G32B32_SFLOAT"
        showsPrec _ VK_FORMAT_R32G32B32A32_UINT
          = showString "VK_FORMAT_R32G32B32A32_UINT"
        showsPrec _ VK_FORMAT_R32G32B32A32_SINT
          = showString "VK_FORMAT_R32G32B32A32_SINT"
        showsPrec _ VK_FORMAT_R32G32B32A32_SFLOAT
          = showString "VK_FORMAT_R32G32B32A32_SFLOAT"
        showsPrec _ VK_FORMAT_R64_UINT = showString "VK_FORMAT_R64_UINT"
        showsPrec _ VK_FORMAT_R64_SINT = showString "VK_FORMAT_R64_SINT"
        showsPrec _ VK_FORMAT_R64_SFLOAT
          = showString "VK_FORMAT_R64_SFLOAT"
        showsPrec _ VK_FORMAT_R64G64_UINT
          = showString "VK_FORMAT_R64G64_UINT"
        showsPrec _ VK_FORMAT_R64G64_SINT
          = showString "VK_FORMAT_R64G64_SINT"
        showsPrec _ VK_FORMAT_R64G64_SFLOAT
          = showString "VK_FORMAT_R64G64_SFLOAT"
        showsPrec _ VK_FORMAT_R64G64B64_UINT
          = showString "VK_FORMAT_R64G64B64_UINT"
        showsPrec _ VK_FORMAT_R64G64B64_SINT
          = showString "VK_FORMAT_R64G64B64_SINT"
        showsPrec _ VK_FORMAT_R64G64B64_SFLOAT
          = showString "VK_FORMAT_R64G64B64_SFLOAT"
        showsPrec _ VK_FORMAT_R64G64B64A64_UINT
          = showString "VK_FORMAT_R64G64B64A64_UINT"
        showsPrec _ VK_FORMAT_R64G64B64A64_SINT
          = showString "VK_FORMAT_R64G64B64A64_SINT"
        showsPrec _ VK_FORMAT_R64G64B64A64_SFLOAT
          = showString "VK_FORMAT_R64G64B64A64_SFLOAT"
        showsPrec _ VK_FORMAT_B10G11R11_UFLOAT_PACK32
          = showString "VK_FORMAT_B10G11R11_UFLOAT_PACK32"
        showsPrec _ VK_FORMAT_E5B9G9R9_UFLOAT_PACK32
          = showString "VK_FORMAT_E5B9G9R9_UFLOAT_PACK32"
        showsPrec _ VK_FORMAT_D16_UNORM = showString "VK_FORMAT_D16_UNORM"
        showsPrec _ VK_FORMAT_X8_D24_UNORM_PACK32
          = showString "VK_FORMAT_X8_D24_UNORM_PACK32"
        showsPrec _ VK_FORMAT_D32_SFLOAT
          = showString "VK_FORMAT_D32_SFLOAT"
        showsPrec _ VK_FORMAT_S8_UINT = showString "VK_FORMAT_S8_UINT"
        showsPrec _ VK_FORMAT_D16_UNORM_S8_UINT
          = showString "VK_FORMAT_D16_UNORM_S8_UINT"
        showsPrec _ VK_FORMAT_D24_UNORM_S8_UINT
          = showString "VK_FORMAT_D24_UNORM_S8_UINT"
        showsPrec _ VK_FORMAT_D32_SFLOAT_S8_UINT
          = showString "VK_FORMAT_D32_SFLOAT_S8_UINT"
        showsPrec _ VK_FORMAT_BC1_RGB_UNORM_BLOCK
          = showString "VK_FORMAT_BC1_RGB_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC1_RGB_SRGB_BLOCK
          = showString "VK_FORMAT_BC1_RGB_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_BC1_RGBA_UNORM_BLOCK
          = showString "VK_FORMAT_BC1_RGBA_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC1_RGBA_SRGB_BLOCK
          = showString "VK_FORMAT_BC1_RGBA_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_BC2_UNORM_BLOCK
          = showString "VK_FORMAT_BC2_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC2_SRGB_BLOCK
          = showString "VK_FORMAT_BC2_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_BC3_UNORM_BLOCK
          = showString "VK_FORMAT_BC3_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC3_SRGB_BLOCK
          = showString "VK_FORMAT_BC3_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_BC4_UNORM_BLOCK
          = showString "VK_FORMAT_BC4_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC4_SNORM_BLOCK
          = showString "VK_FORMAT_BC4_SNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC5_UNORM_BLOCK
          = showString "VK_FORMAT_BC5_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC5_SNORM_BLOCK
          = showString "VK_FORMAT_BC5_SNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC6H_UFLOAT_BLOCK
          = showString "VK_FORMAT_BC6H_UFLOAT_BLOCK"
        showsPrec _ VK_FORMAT_BC6H_SFLOAT_BLOCK
          = showString "VK_FORMAT_BC6H_SFLOAT_BLOCK"
        showsPrec _ VK_FORMAT_BC7_UNORM_BLOCK
          = showString "VK_FORMAT_BC7_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC7_SRGB_BLOCK
          = showString "VK_FORMAT_BC7_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_EAC_R11_UNORM_BLOCK
          = showString "VK_FORMAT_EAC_R11_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_EAC_R11_SNORM_BLOCK
          = showString "VK_FORMAT_EAC_R11_SNORM_BLOCK"
        showsPrec _ VK_FORMAT_EAC_R11G11_UNORM_BLOCK
          = showString "VK_FORMAT_EAC_R11G11_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_EAC_R11G11_SNORM_BLOCK
          = showString "VK_FORMAT_EAC_R11G11_SNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_4x4_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_4x4_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_4x4_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_4x4_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_5x4_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_5x4_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_5x4_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_5x4_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_5x5_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_5x5_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_5x5_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_5x5_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_6x5_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_6x5_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_6x5_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_6x5_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_6x6_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_6x6_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_6x6_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_6x6_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x5_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_8x5_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x5_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_8x5_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x6_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_8x6_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x6_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_8x6_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x8_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_8x8_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x8_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_8x8_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x5_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_10x5_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x5_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_10x5_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x6_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_10x6_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x6_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_10x6_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x8_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_10x8_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x8_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_10x8_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x10_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_10x10_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x10_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_10x10_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_12x10_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_12x10_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_12x10_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_12x10_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_12x12_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_12x12_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_12x12_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_12x12_SRGB_BLOCK"
        showsPrec p (VkFormat x)
          = showParen (p >= 11) (showString "VkFormat " . showsPrec 11 x)

instance Read VkFormat where
        readPrec
          = parens
              (choose
                 [("VK_FORMAT_UNDEFINED", pure VK_FORMAT_UNDEFINED),
                  ("VK_FORMAT_R4G4_UNORM_PACK8", pure VK_FORMAT_R4G4_UNORM_PACK8),
                  ("VK_FORMAT_R4G4B4A4_UNORM_PACK16",
                   pure VK_FORMAT_R4G4B4A4_UNORM_PACK16),
                  ("VK_FORMAT_B4G4R4A4_UNORM_PACK16",
                   pure VK_FORMAT_B4G4R4A4_UNORM_PACK16),
                  ("VK_FORMAT_R5G6B5_UNORM_PACK16",
                   pure VK_FORMAT_R5G6B5_UNORM_PACK16),
                  ("VK_FORMAT_B5G6R5_UNORM_PACK16",
                   pure VK_FORMAT_B5G6R5_UNORM_PACK16),
                  ("VK_FORMAT_R5G5B5A1_UNORM_PACK16",
                   pure VK_FORMAT_R5G5B5A1_UNORM_PACK16),
                  ("VK_FORMAT_B5G5R5A1_UNORM_PACK16",
                   pure VK_FORMAT_B5G5R5A1_UNORM_PACK16),
                  ("VK_FORMAT_A1R5G5B5_UNORM_PACK16",
                   pure VK_FORMAT_A1R5G5B5_UNORM_PACK16),
                  ("VK_FORMAT_R8_UNORM", pure VK_FORMAT_R8_UNORM),
                  ("VK_FORMAT_R8_SNORM", pure VK_FORMAT_R8_SNORM),
                  ("VK_FORMAT_R8_USCALED", pure VK_FORMAT_R8_USCALED),
                  ("VK_FORMAT_R8_SSCALED", pure VK_FORMAT_R8_SSCALED),
                  ("VK_FORMAT_R8_UINT", pure VK_FORMAT_R8_UINT),
                  ("VK_FORMAT_R8_SINT", pure VK_FORMAT_R8_SINT),
                  ("VK_FORMAT_R8_SRGB", pure VK_FORMAT_R8_SRGB),
                  ("VK_FORMAT_R8G8_UNORM", pure VK_FORMAT_R8G8_UNORM),
                  ("VK_FORMAT_R8G8_SNORM", pure VK_FORMAT_R8G8_SNORM),
                  ("VK_FORMAT_R8G8_USCALED", pure VK_FORMAT_R8G8_USCALED),
                  ("VK_FORMAT_R8G8_SSCALED", pure VK_FORMAT_R8G8_SSCALED),
                  ("VK_FORMAT_R8G8_UINT", pure VK_FORMAT_R8G8_UINT),
                  ("VK_FORMAT_R8G8_SINT", pure VK_FORMAT_R8G8_SINT),
                  ("VK_FORMAT_R8G8_SRGB", pure VK_FORMAT_R8G8_SRGB),
                  ("VK_FORMAT_R8G8B8_UNORM", pure VK_FORMAT_R8G8B8_UNORM),
                  ("VK_FORMAT_R8G8B8_SNORM", pure VK_FORMAT_R8G8B8_SNORM),
                  ("VK_FORMAT_R8G8B8_USCALED", pure VK_FORMAT_R8G8B8_USCALED),
                  ("VK_FORMAT_R8G8B8_SSCALED", pure VK_FORMAT_R8G8B8_SSCALED),
                  ("VK_FORMAT_R8G8B8_UINT", pure VK_FORMAT_R8G8B8_UINT),
                  ("VK_FORMAT_R8G8B8_SINT", pure VK_FORMAT_R8G8B8_SINT),
                  ("VK_FORMAT_R8G8B8_SRGB", pure VK_FORMAT_R8G8B8_SRGB),
                  ("VK_FORMAT_B8G8R8_UNORM", pure VK_FORMAT_B8G8R8_UNORM),
                  ("VK_FORMAT_B8G8R8_SNORM", pure VK_FORMAT_B8G8R8_SNORM),
                  ("VK_FORMAT_B8G8R8_USCALED", pure VK_FORMAT_B8G8R8_USCALED),
                  ("VK_FORMAT_B8G8R8_SSCALED", pure VK_FORMAT_B8G8R8_SSCALED),
                  ("VK_FORMAT_B8G8R8_UINT", pure VK_FORMAT_B8G8R8_UINT),
                  ("VK_FORMAT_B8G8R8_SINT", pure VK_FORMAT_B8G8R8_SINT),
                  ("VK_FORMAT_B8G8R8_SRGB", pure VK_FORMAT_B8G8R8_SRGB),
                  ("VK_FORMAT_R8G8B8A8_UNORM", pure VK_FORMAT_R8G8B8A8_UNORM),
                  ("VK_FORMAT_R8G8B8A8_SNORM", pure VK_FORMAT_R8G8B8A8_SNORM),
                  ("VK_FORMAT_R8G8B8A8_USCALED", pure VK_FORMAT_R8G8B8A8_USCALED),
                  ("VK_FORMAT_R8G8B8A8_SSCALED", pure VK_FORMAT_R8G8B8A8_SSCALED),
                  ("VK_FORMAT_R8G8B8A8_UINT", pure VK_FORMAT_R8G8B8A8_UINT),
                  ("VK_FORMAT_R8G8B8A8_SINT", pure VK_FORMAT_R8G8B8A8_SINT),
                  ("VK_FORMAT_R8G8B8A8_SRGB", pure VK_FORMAT_R8G8B8A8_SRGB),
                  ("VK_FORMAT_B8G8R8A8_UNORM", pure VK_FORMAT_B8G8R8A8_UNORM),
                  ("VK_FORMAT_B8G8R8A8_SNORM", pure VK_FORMAT_B8G8R8A8_SNORM),
                  ("VK_FORMAT_B8G8R8A8_USCALED", pure VK_FORMAT_B8G8R8A8_USCALED),
                  ("VK_FORMAT_B8G8R8A8_SSCALED", pure VK_FORMAT_B8G8R8A8_SSCALED),
                  ("VK_FORMAT_B8G8R8A8_UINT", pure VK_FORMAT_B8G8R8A8_UINT),
                  ("VK_FORMAT_B8G8R8A8_SINT", pure VK_FORMAT_B8G8R8A8_SINT),
                  ("VK_FORMAT_B8G8R8A8_SRGB", pure VK_FORMAT_B8G8R8A8_SRGB),
                  ("VK_FORMAT_A8B8G8R8_UNORM_PACK32",
                   pure VK_FORMAT_A8B8G8R8_UNORM_PACK32),
                  ("VK_FORMAT_A8B8G8R8_SNORM_PACK32",
                   pure VK_FORMAT_A8B8G8R8_SNORM_PACK32),
                  ("VK_FORMAT_A8B8G8R8_USCALED_PACK32",
                   pure VK_FORMAT_A8B8G8R8_USCALED_PACK32),
                  ("VK_FORMAT_A8B8G8R8_SSCALED_PACK32",
                   pure VK_FORMAT_A8B8G8R8_SSCALED_PACK32),
                  ("VK_FORMAT_A8B8G8R8_UINT_PACK32",
                   pure VK_FORMAT_A8B8G8R8_UINT_PACK32),
                  ("VK_FORMAT_A8B8G8R8_SINT_PACK32",
                   pure VK_FORMAT_A8B8G8R8_SINT_PACK32),
                  ("VK_FORMAT_A8B8G8R8_SRGB_PACK32",
                   pure VK_FORMAT_A8B8G8R8_SRGB_PACK32),
                  ("VK_FORMAT_A2R10G10B10_UNORM_PACK32",
                   pure VK_FORMAT_A2R10G10B10_UNORM_PACK32),
                  ("VK_FORMAT_A2R10G10B10_SNORM_PACK32",
                   pure VK_FORMAT_A2R10G10B10_SNORM_PACK32),
                  ("VK_FORMAT_A2R10G10B10_USCALED_PACK32",
                   pure VK_FORMAT_A2R10G10B10_USCALED_PACK32),
                  ("VK_FORMAT_A2R10G10B10_SSCALED_PACK32",
                   pure VK_FORMAT_A2R10G10B10_SSCALED_PACK32),
                  ("VK_FORMAT_A2R10G10B10_UINT_PACK32",
                   pure VK_FORMAT_A2R10G10B10_UINT_PACK32),
                  ("VK_FORMAT_A2R10G10B10_SINT_PACK32",
                   pure VK_FORMAT_A2R10G10B10_SINT_PACK32),
                  ("VK_FORMAT_A2B10G10R10_UNORM_PACK32",
                   pure VK_FORMAT_A2B10G10R10_UNORM_PACK32),
                  ("VK_FORMAT_A2B10G10R10_SNORM_PACK32",
                   pure VK_FORMAT_A2B10G10R10_SNORM_PACK32),
                  ("VK_FORMAT_A2B10G10R10_USCALED_PACK32",
                   pure VK_FORMAT_A2B10G10R10_USCALED_PACK32),
                  ("VK_FORMAT_A2B10G10R10_SSCALED_PACK32",
                   pure VK_FORMAT_A2B10G10R10_SSCALED_PACK32),
                  ("VK_FORMAT_A2B10G10R10_UINT_PACK32",
                   pure VK_FORMAT_A2B10G10R10_UINT_PACK32),
                  ("VK_FORMAT_A2B10G10R10_SINT_PACK32",
                   pure VK_FORMAT_A2B10G10R10_SINT_PACK32),
                  ("VK_FORMAT_R16_UNORM", pure VK_FORMAT_R16_UNORM),
                  ("VK_FORMAT_R16_SNORM", pure VK_FORMAT_R16_SNORM),
                  ("VK_FORMAT_R16_USCALED", pure VK_FORMAT_R16_USCALED),
                  ("VK_FORMAT_R16_SSCALED", pure VK_FORMAT_R16_SSCALED),
                  ("VK_FORMAT_R16_UINT", pure VK_FORMAT_R16_UINT),
                  ("VK_FORMAT_R16_SINT", pure VK_FORMAT_R16_SINT),
                  ("VK_FORMAT_R16_SFLOAT", pure VK_FORMAT_R16_SFLOAT),
                  ("VK_FORMAT_R16G16_UNORM", pure VK_FORMAT_R16G16_UNORM),
                  ("VK_FORMAT_R16G16_SNORM", pure VK_FORMAT_R16G16_SNORM),
                  ("VK_FORMAT_R16G16_USCALED", pure VK_FORMAT_R16G16_USCALED),
                  ("VK_FORMAT_R16G16_SSCALED", pure VK_FORMAT_R16G16_SSCALED),
                  ("VK_FORMAT_R16G16_UINT", pure VK_FORMAT_R16G16_UINT),
                  ("VK_FORMAT_R16G16_SINT", pure VK_FORMAT_R16G16_SINT),
                  ("VK_FORMAT_R16G16_SFLOAT", pure VK_FORMAT_R16G16_SFLOAT),
                  ("VK_FORMAT_R16G16B16_UNORM", pure VK_FORMAT_R16G16B16_UNORM),
                  ("VK_FORMAT_R16G16B16_SNORM", pure VK_FORMAT_R16G16B16_SNORM),
                  ("VK_FORMAT_R16G16B16_USCALED", pure VK_FORMAT_R16G16B16_USCALED),
                  ("VK_FORMAT_R16G16B16_SSCALED", pure VK_FORMAT_R16G16B16_SSCALED),
                  ("VK_FORMAT_R16G16B16_UINT", pure VK_FORMAT_R16G16B16_UINT),
                  ("VK_FORMAT_R16G16B16_SINT", pure VK_FORMAT_R16G16B16_SINT),
                  ("VK_FORMAT_R16G16B16_SFLOAT", pure VK_FORMAT_R16G16B16_SFLOAT),
                  ("VK_FORMAT_R16G16B16A16_UNORM",
                   pure VK_FORMAT_R16G16B16A16_UNORM),
                  ("VK_FORMAT_R16G16B16A16_SNORM",
                   pure VK_FORMAT_R16G16B16A16_SNORM),
                  ("VK_FORMAT_R16G16B16A16_USCALED",
                   pure VK_FORMAT_R16G16B16A16_USCALED),
                  ("VK_FORMAT_R16G16B16A16_SSCALED",
                   pure VK_FORMAT_R16G16B16A16_SSCALED),
                  ("VK_FORMAT_R16G16B16A16_UINT", pure VK_FORMAT_R16G16B16A16_UINT),
                  ("VK_FORMAT_R16G16B16A16_SINT", pure VK_FORMAT_R16G16B16A16_SINT),
                  ("VK_FORMAT_R16G16B16A16_SFLOAT",
                   pure VK_FORMAT_R16G16B16A16_SFLOAT),
                  ("VK_FORMAT_R32_UINT", pure VK_FORMAT_R32_UINT),
                  ("VK_FORMAT_R32_SINT", pure VK_FORMAT_R32_SINT),
                  ("VK_FORMAT_R32_SFLOAT", pure VK_FORMAT_R32_SFLOAT),
                  ("VK_FORMAT_R32G32_UINT", pure VK_FORMAT_R32G32_UINT),
                  ("VK_FORMAT_R32G32_SINT", pure VK_FORMAT_R32G32_SINT),
                  ("VK_FORMAT_R32G32_SFLOAT", pure VK_FORMAT_R32G32_SFLOAT),
                  ("VK_FORMAT_R32G32B32_UINT", pure VK_FORMAT_R32G32B32_UINT),
                  ("VK_FORMAT_R32G32B32_SINT", pure VK_FORMAT_R32G32B32_SINT),
                  ("VK_FORMAT_R32G32B32_SFLOAT", pure VK_FORMAT_R32G32B32_SFLOAT),
                  ("VK_FORMAT_R32G32B32A32_UINT", pure VK_FORMAT_R32G32B32A32_UINT),
                  ("VK_FORMAT_R32G32B32A32_SINT", pure VK_FORMAT_R32G32B32A32_SINT),
                  ("VK_FORMAT_R32G32B32A32_SFLOAT",
                   pure VK_FORMAT_R32G32B32A32_SFLOAT),
                  ("VK_FORMAT_R64_UINT", pure VK_FORMAT_R64_UINT),
                  ("VK_FORMAT_R64_SINT", pure VK_FORMAT_R64_SINT),
                  ("VK_FORMAT_R64_SFLOAT", pure VK_FORMAT_R64_SFLOAT),
                  ("VK_FORMAT_R64G64_UINT", pure VK_FORMAT_R64G64_UINT),
                  ("VK_FORMAT_R64G64_SINT", pure VK_FORMAT_R64G64_SINT),
                  ("VK_FORMAT_R64G64_SFLOAT", pure VK_FORMAT_R64G64_SFLOAT),
                  ("VK_FORMAT_R64G64B64_UINT", pure VK_FORMAT_R64G64B64_UINT),
                  ("VK_FORMAT_R64G64B64_SINT", pure VK_FORMAT_R64G64B64_SINT),
                  ("VK_FORMAT_R64G64B64_SFLOAT", pure VK_FORMAT_R64G64B64_SFLOAT),
                  ("VK_FORMAT_R64G64B64A64_UINT", pure VK_FORMAT_R64G64B64A64_UINT),
                  ("VK_FORMAT_R64G64B64A64_SINT", pure VK_FORMAT_R64G64B64A64_SINT),
                  ("VK_FORMAT_R64G64B64A64_SFLOAT",
                   pure VK_FORMAT_R64G64B64A64_SFLOAT),
                  ("VK_FORMAT_B10G11R11_UFLOAT_PACK32",
                   pure VK_FORMAT_B10G11R11_UFLOAT_PACK32),
                  ("VK_FORMAT_E5B9G9R9_UFLOAT_PACK32",
                   pure VK_FORMAT_E5B9G9R9_UFLOAT_PACK32),
                  ("VK_FORMAT_D16_UNORM", pure VK_FORMAT_D16_UNORM),
                  ("VK_FORMAT_X8_D24_UNORM_PACK32",
                   pure VK_FORMAT_X8_D24_UNORM_PACK32),
                  ("VK_FORMAT_D32_SFLOAT", pure VK_FORMAT_D32_SFLOAT),
                  ("VK_FORMAT_S8_UINT", pure VK_FORMAT_S8_UINT),
                  ("VK_FORMAT_D16_UNORM_S8_UINT", pure VK_FORMAT_D16_UNORM_S8_UINT),
                  ("VK_FORMAT_D24_UNORM_S8_UINT", pure VK_FORMAT_D24_UNORM_S8_UINT),
                  ("VK_FORMAT_D32_SFLOAT_S8_UINT",
                   pure VK_FORMAT_D32_SFLOAT_S8_UINT),
                  ("VK_FORMAT_BC1_RGB_UNORM_BLOCK",
                   pure VK_FORMAT_BC1_RGB_UNORM_BLOCK),
                  ("VK_FORMAT_BC1_RGB_SRGB_BLOCK",
                   pure VK_FORMAT_BC1_RGB_SRGB_BLOCK),
                  ("VK_FORMAT_BC1_RGBA_UNORM_BLOCK",
                   pure VK_FORMAT_BC1_RGBA_UNORM_BLOCK),
                  ("VK_FORMAT_BC1_RGBA_SRGB_BLOCK",
                   pure VK_FORMAT_BC1_RGBA_SRGB_BLOCK),
                  ("VK_FORMAT_BC2_UNORM_BLOCK", pure VK_FORMAT_BC2_UNORM_BLOCK),
                  ("VK_FORMAT_BC2_SRGB_BLOCK", pure VK_FORMAT_BC2_SRGB_BLOCK),
                  ("VK_FORMAT_BC3_UNORM_BLOCK", pure VK_FORMAT_BC3_UNORM_BLOCK),
                  ("VK_FORMAT_BC3_SRGB_BLOCK", pure VK_FORMAT_BC3_SRGB_BLOCK),
                  ("VK_FORMAT_BC4_UNORM_BLOCK", pure VK_FORMAT_BC4_UNORM_BLOCK),
                  ("VK_FORMAT_BC4_SNORM_BLOCK", pure VK_FORMAT_BC4_SNORM_BLOCK),
                  ("VK_FORMAT_BC5_UNORM_BLOCK", pure VK_FORMAT_BC5_UNORM_BLOCK),
                  ("VK_FORMAT_BC5_SNORM_BLOCK", pure VK_FORMAT_BC5_SNORM_BLOCK),
                  ("VK_FORMAT_BC6H_UFLOAT_BLOCK", pure VK_FORMAT_BC6H_UFLOAT_BLOCK),
                  ("VK_FORMAT_BC6H_SFLOAT_BLOCK", pure VK_FORMAT_BC6H_SFLOAT_BLOCK),
                  ("VK_FORMAT_BC7_UNORM_BLOCK", pure VK_FORMAT_BC7_UNORM_BLOCK),
                  ("VK_FORMAT_BC7_SRGB_BLOCK", pure VK_FORMAT_BC7_SRGB_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK),
                  ("VK_FORMAT_EAC_R11_UNORM_BLOCK",
                   pure VK_FORMAT_EAC_R11_UNORM_BLOCK),
                  ("VK_FORMAT_EAC_R11_SNORM_BLOCK",
                   pure VK_FORMAT_EAC_R11_SNORM_BLOCK),
                  ("VK_FORMAT_EAC_R11G11_UNORM_BLOCK",
                   pure VK_FORMAT_EAC_R11G11_UNORM_BLOCK),
                  ("VK_FORMAT_EAC_R11G11_SNORM_BLOCK",
                   pure VK_FORMAT_EAC_R11G11_SNORM_BLOCK),
                  ("VK_FORMAT_ASTC_4x4_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_4x4_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_4x4_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_4x4_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_5x4_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_5x4_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_5x4_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_5x4_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_5x5_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_5x5_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_5x5_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_5x5_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_6x5_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_6x5_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_6x5_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_6x5_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_6x6_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_6x6_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_6x6_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_6x6_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_8x5_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_8x5_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_8x5_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_8x5_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_8x6_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_8x6_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_8x6_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_8x6_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_8x8_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_8x8_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_8x8_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_8x8_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_10x5_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_10x5_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_10x5_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_10x5_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_10x6_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_10x6_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_10x6_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_10x6_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_10x8_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_10x8_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_10x8_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_10x8_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_10x10_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_10x10_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_10x10_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_10x10_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_12x10_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_12x10_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_12x10_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_12x10_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_12x12_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_12x12_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_12x12_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_12x12_SRGB_BLOCK)]
                 +++
                 prec 10
                   (expectP (Ident "VkFormat") >> (VkFormat <$> step readPrec)))

pattern VK_FORMAT_UNDEFINED :: VkFormat

pattern VK_FORMAT_UNDEFINED = VkFormat 0

pattern VK_FORMAT_R4G4_UNORM_PACK8 :: VkFormat

pattern VK_FORMAT_R4G4_UNORM_PACK8 = VkFormat 1

pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 = VkFormat 2

pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 = VkFormat 3

pattern VK_FORMAT_R5G6B5_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_R5G6B5_UNORM_PACK16 = VkFormat 4

pattern VK_FORMAT_B5G6R5_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_B5G6R5_UNORM_PACK16 = VkFormat 5

pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 = VkFormat 6

pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 = VkFormat 7

pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 = VkFormat 8

pattern VK_FORMAT_R8_UNORM :: VkFormat

pattern VK_FORMAT_R8_UNORM = VkFormat 9

pattern VK_FORMAT_R8_SNORM :: VkFormat

pattern VK_FORMAT_R8_SNORM = VkFormat 10

pattern VK_FORMAT_R8_USCALED :: VkFormat

pattern VK_FORMAT_R8_USCALED = VkFormat 11

pattern VK_FORMAT_R8_SSCALED :: VkFormat

pattern VK_FORMAT_R8_SSCALED = VkFormat 12

pattern VK_FORMAT_R8_UINT :: VkFormat

pattern VK_FORMAT_R8_UINT = VkFormat 13

pattern VK_FORMAT_R8_SINT :: VkFormat

pattern VK_FORMAT_R8_SINT = VkFormat 14

pattern VK_FORMAT_R8_SRGB :: VkFormat

pattern VK_FORMAT_R8_SRGB = VkFormat 15

pattern VK_FORMAT_R8G8_UNORM :: VkFormat

pattern VK_FORMAT_R8G8_UNORM = VkFormat 16

pattern VK_FORMAT_R8G8_SNORM :: VkFormat

pattern VK_FORMAT_R8G8_SNORM = VkFormat 17

pattern VK_FORMAT_R8G8_USCALED :: VkFormat

pattern VK_FORMAT_R8G8_USCALED = VkFormat 18

pattern VK_FORMAT_R8G8_SSCALED :: VkFormat

pattern VK_FORMAT_R8G8_SSCALED = VkFormat 19

pattern VK_FORMAT_R8G8_UINT :: VkFormat

pattern VK_FORMAT_R8G8_UINT = VkFormat 20

pattern VK_FORMAT_R8G8_SINT :: VkFormat

pattern VK_FORMAT_R8G8_SINT = VkFormat 21

pattern VK_FORMAT_R8G8_SRGB :: VkFormat

pattern VK_FORMAT_R8G8_SRGB = VkFormat 22

pattern VK_FORMAT_R8G8B8_UNORM :: VkFormat

pattern VK_FORMAT_R8G8B8_UNORM = VkFormat 23

pattern VK_FORMAT_R8G8B8_SNORM :: VkFormat

pattern VK_FORMAT_R8G8B8_SNORM = VkFormat 24

pattern VK_FORMAT_R8G8B8_USCALED :: VkFormat

pattern VK_FORMAT_R8G8B8_USCALED = VkFormat 25

pattern VK_FORMAT_R8G8B8_SSCALED :: VkFormat

pattern VK_FORMAT_R8G8B8_SSCALED = VkFormat 26

pattern VK_FORMAT_R8G8B8_UINT :: VkFormat

pattern VK_FORMAT_R8G8B8_UINT = VkFormat 27

pattern VK_FORMAT_R8G8B8_SINT :: VkFormat

pattern VK_FORMAT_R8G8B8_SINT = VkFormat 28

pattern VK_FORMAT_R8G8B8_SRGB :: VkFormat

pattern VK_FORMAT_R8G8B8_SRGB = VkFormat 29

pattern VK_FORMAT_B8G8R8_UNORM :: VkFormat

pattern VK_FORMAT_B8G8R8_UNORM = VkFormat 30

pattern VK_FORMAT_B8G8R8_SNORM :: VkFormat

pattern VK_FORMAT_B8G8R8_SNORM = VkFormat 31

pattern VK_FORMAT_B8G8R8_USCALED :: VkFormat

pattern VK_FORMAT_B8G8R8_USCALED = VkFormat 32

pattern VK_FORMAT_B8G8R8_SSCALED :: VkFormat

pattern VK_FORMAT_B8G8R8_SSCALED = VkFormat 33

pattern VK_FORMAT_B8G8R8_UINT :: VkFormat

pattern VK_FORMAT_B8G8R8_UINT = VkFormat 34

pattern VK_FORMAT_B8G8R8_SINT :: VkFormat

pattern VK_FORMAT_B8G8R8_SINT = VkFormat 35

pattern VK_FORMAT_B8G8R8_SRGB :: VkFormat

pattern VK_FORMAT_B8G8R8_SRGB = VkFormat 36

pattern VK_FORMAT_R8G8B8A8_UNORM :: VkFormat

pattern VK_FORMAT_R8G8B8A8_UNORM = VkFormat 37

pattern VK_FORMAT_R8G8B8A8_SNORM :: VkFormat

pattern VK_FORMAT_R8G8B8A8_SNORM = VkFormat 38

pattern VK_FORMAT_R8G8B8A8_USCALED :: VkFormat

pattern VK_FORMAT_R8G8B8A8_USCALED = VkFormat 39

pattern VK_FORMAT_R8G8B8A8_SSCALED :: VkFormat

pattern VK_FORMAT_R8G8B8A8_SSCALED = VkFormat 40

pattern VK_FORMAT_R8G8B8A8_UINT :: VkFormat

pattern VK_FORMAT_R8G8B8A8_UINT = VkFormat 41

pattern VK_FORMAT_R8G8B8A8_SINT :: VkFormat

pattern VK_FORMAT_R8G8B8A8_SINT = VkFormat 42

pattern VK_FORMAT_R8G8B8A8_SRGB :: VkFormat

pattern VK_FORMAT_R8G8B8A8_SRGB = VkFormat 43

pattern VK_FORMAT_B8G8R8A8_UNORM :: VkFormat

pattern VK_FORMAT_B8G8R8A8_UNORM = VkFormat 44

pattern VK_FORMAT_B8G8R8A8_SNORM :: VkFormat

pattern VK_FORMAT_B8G8R8A8_SNORM = VkFormat 45

pattern VK_FORMAT_B8G8R8A8_USCALED :: VkFormat

pattern VK_FORMAT_B8G8R8A8_USCALED = VkFormat 46

pattern VK_FORMAT_B8G8R8A8_SSCALED :: VkFormat

pattern VK_FORMAT_B8G8R8A8_SSCALED = VkFormat 47

pattern VK_FORMAT_B8G8R8A8_UINT :: VkFormat

pattern VK_FORMAT_B8G8R8A8_UINT = VkFormat 48

pattern VK_FORMAT_B8G8R8A8_SINT :: VkFormat

pattern VK_FORMAT_B8G8R8A8_SINT = VkFormat 49

pattern VK_FORMAT_B8G8R8A8_SRGB :: VkFormat

pattern VK_FORMAT_B8G8R8A8_SRGB = VkFormat 50

pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 = VkFormat 51

pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 = VkFormat 52

pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 = VkFormat 53

pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = VkFormat 54

pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 = VkFormat 55

pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 = VkFormat 56

pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 = VkFormat 57

pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 = VkFormat 58

pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 = VkFormat 59

pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 = VkFormat 60

pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = VkFormat 61

pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 = VkFormat 62

pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 = VkFormat 63

pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 = VkFormat 64

pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 = VkFormat 65

pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 = VkFormat 66

pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = VkFormat 67

pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 = VkFormat 68

pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 = VkFormat 69

pattern VK_FORMAT_R16_UNORM :: VkFormat

pattern VK_FORMAT_R16_UNORM = VkFormat 70

pattern VK_FORMAT_R16_SNORM :: VkFormat

pattern VK_FORMAT_R16_SNORM = VkFormat 71

pattern VK_FORMAT_R16_USCALED :: VkFormat

pattern VK_FORMAT_R16_USCALED = VkFormat 72

pattern VK_FORMAT_R16_SSCALED :: VkFormat

pattern VK_FORMAT_R16_SSCALED = VkFormat 73

pattern VK_FORMAT_R16_UINT :: VkFormat

pattern VK_FORMAT_R16_UINT = VkFormat 74

pattern VK_FORMAT_R16_SINT :: VkFormat

pattern VK_FORMAT_R16_SINT = VkFormat 75

pattern VK_FORMAT_R16_SFLOAT :: VkFormat

pattern VK_FORMAT_R16_SFLOAT = VkFormat 76

pattern VK_FORMAT_R16G16_UNORM :: VkFormat

pattern VK_FORMAT_R16G16_UNORM = VkFormat 77

pattern VK_FORMAT_R16G16_SNORM :: VkFormat

pattern VK_FORMAT_R16G16_SNORM = VkFormat 78

pattern VK_FORMAT_R16G16_USCALED :: VkFormat

pattern VK_FORMAT_R16G16_USCALED = VkFormat 79

pattern VK_FORMAT_R16G16_SSCALED :: VkFormat

pattern VK_FORMAT_R16G16_SSCALED = VkFormat 80

pattern VK_FORMAT_R16G16_UINT :: VkFormat

pattern VK_FORMAT_R16G16_UINT = VkFormat 81

pattern VK_FORMAT_R16G16_SINT :: VkFormat

pattern VK_FORMAT_R16G16_SINT = VkFormat 82

pattern VK_FORMAT_R16G16_SFLOAT :: VkFormat

pattern VK_FORMAT_R16G16_SFLOAT = VkFormat 83

pattern VK_FORMAT_R16G16B16_UNORM :: VkFormat

pattern VK_FORMAT_R16G16B16_UNORM = VkFormat 84

pattern VK_FORMAT_R16G16B16_SNORM :: VkFormat

pattern VK_FORMAT_R16G16B16_SNORM = VkFormat 85

pattern VK_FORMAT_R16G16B16_USCALED :: VkFormat

pattern VK_FORMAT_R16G16B16_USCALED = VkFormat 86

pattern VK_FORMAT_R16G16B16_SSCALED :: VkFormat

pattern VK_FORMAT_R16G16B16_SSCALED = VkFormat 87

pattern VK_FORMAT_R16G16B16_UINT :: VkFormat

pattern VK_FORMAT_R16G16B16_UINT = VkFormat 88

pattern VK_FORMAT_R16G16B16_SINT :: VkFormat

pattern VK_FORMAT_R16G16B16_SINT = VkFormat 89

pattern VK_FORMAT_R16G16B16_SFLOAT :: VkFormat

pattern VK_FORMAT_R16G16B16_SFLOAT = VkFormat 90

pattern VK_FORMAT_R16G16B16A16_UNORM :: VkFormat

pattern VK_FORMAT_R16G16B16A16_UNORM = VkFormat 91

pattern VK_FORMAT_R16G16B16A16_SNORM :: VkFormat

pattern VK_FORMAT_R16G16B16A16_SNORM = VkFormat 92

pattern VK_FORMAT_R16G16B16A16_USCALED :: VkFormat

pattern VK_FORMAT_R16G16B16A16_USCALED = VkFormat 93

pattern VK_FORMAT_R16G16B16A16_SSCALED :: VkFormat

pattern VK_FORMAT_R16G16B16A16_SSCALED = VkFormat 94

pattern VK_FORMAT_R16G16B16A16_UINT :: VkFormat

pattern VK_FORMAT_R16G16B16A16_UINT = VkFormat 95

pattern VK_FORMAT_R16G16B16A16_SINT :: VkFormat

pattern VK_FORMAT_R16G16B16A16_SINT = VkFormat 96

pattern VK_FORMAT_R16G16B16A16_SFLOAT :: VkFormat

pattern VK_FORMAT_R16G16B16A16_SFLOAT = VkFormat 97

pattern VK_FORMAT_R32_UINT :: VkFormat

pattern VK_FORMAT_R32_UINT = VkFormat 98

pattern VK_FORMAT_R32_SINT :: VkFormat

pattern VK_FORMAT_R32_SINT = VkFormat 99

pattern VK_FORMAT_R32_SFLOAT :: VkFormat

pattern VK_FORMAT_R32_SFLOAT = VkFormat 100

pattern VK_FORMAT_R32G32_UINT :: VkFormat

pattern VK_FORMAT_R32G32_UINT = VkFormat 101

pattern VK_FORMAT_R32G32_SINT :: VkFormat

pattern VK_FORMAT_R32G32_SINT = VkFormat 102

pattern VK_FORMAT_R32G32_SFLOAT :: VkFormat

pattern VK_FORMAT_R32G32_SFLOAT = VkFormat 103

pattern VK_FORMAT_R32G32B32_UINT :: VkFormat

pattern VK_FORMAT_R32G32B32_UINT = VkFormat 104

pattern VK_FORMAT_R32G32B32_SINT :: VkFormat

pattern VK_FORMAT_R32G32B32_SINT = VkFormat 105

pattern VK_FORMAT_R32G32B32_SFLOAT :: VkFormat

pattern VK_FORMAT_R32G32B32_SFLOAT = VkFormat 106

pattern VK_FORMAT_R32G32B32A32_UINT :: VkFormat

pattern VK_FORMAT_R32G32B32A32_UINT = VkFormat 107

pattern VK_FORMAT_R32G32B32A32_SINT :: VkFormat

pattern VK_FORMAT_R32G32B32A32_SINT = VkFormat 108

pattern VK_FORMAT_R32G32B32A32_SFLOAT :: VkFormat

pattern VK_FORMAT_R32G32B32A32_SFLOAT = VkFormat 109

pattern VK_FORMAT_R64_UINT :: VkFormat

pattern VK_FORMAT_R64_UINT = VkFormat 110

pattern VK_FORMAT_R64_SINT :: VkFormat

pattern VK_FORMAT_R64_SINT = VkFormat 111

pattern VK_FORMAT_R64_SFLOAT :: VkFormat

pattern VK_FORMAT_R64_SFLOAT = VkFormat 112

pattern VK_FORMAT_R64G64_UINT :: VkFormat

pattern VK_FORMAT_R64G64_UINT = VkFormat 113

pattern VK_FORMAT_R64G64_SINT :: VkFormat

pattern VK_FORMAT_R64G64_SINT = VkFormat 114

pattern VK_FORMAT_R64G64_SFLOAT :: VkFormat

pattern VK_FORMAT_R64G64_SFLOAT = VkFormat 115

pattern VK_FORMAT_R64G64B64_UINT :: VkFormat

pattern VK_FORMAT_R64G64B64_UINT = VkFormat 116

pattern VK_FORMAT_R64G64B64_SINT :: VkFormat

pattern VK_FORMAT_R64G64B64_SINT = VkFormat 117

pattern VK_FORMAT_R64G64B64_SFLOAT :: VkFormat

pattern VK_FORMAT_R64G64B64_SFLOAT = VkFormat 118

pattern VK_FORMAT_R64G64B64A64_UINT :: VkFormat

pattern VK_FORMAT_R64G64B64A64_UINT = VkFormat 119

pattern VK_FORMAT_R64G64B64A64_SINT :: VkFormat

pattern VK_FORMAT_R64G64B64A64_SINT = VkFormat 120

pattern VK_FORMAT_R64G64B64A64_SFLOAT :: VkFormat

pattern VK_FORMAT_R64G64B64A64_SFLOAT = VkFormat 121

pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 :: VkFormat

pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 = VkFormat 122

pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 :: VkFormat

pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = VkFormat 123

pattern VK_FORMAT_D16_UNORM :: VkFormat

pattern VK_FORMAT_D16_UNORM = VkFormat 124

pattern VK_FORMAT_X8_D24_UNORM_PACK32 :: VkFormat

pattern VK_FORMAT_X8_D24_UNORM_PACK32 = VkFormat 125

pattern VK_FORMAT_D32_SFLOAT :: VkFormat

pattern VK_FORMAT_D32_SFLOAT = VkFormat 126

pattern VK_FORMAT_S8_UINT :: VkFormat

pattern VK_FORMAT_S8_UINT = VkFormat 127

pattern VK_FORMAT_D16_UNORM_S8_UINT :: VkFormat

pattern VK_FORMAT_D16_UNORM_S8_UINT = VkFormat 128

pattern VK_FORMAT_D24_UNORM_S8_UINT :: VkFormat

pattern VK_FORMAT_D24_UNORM_S8_UINT = VkFormat 129

pattern VK_FORMAT_D32_SFLOAT_S8_UINT :: VkFormat

pattern VK_FORMAT_D32_SFLOAT_S8_UINT = VkFormat 130

pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK = VkFormat 131

pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK = VkFormat 132

pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK = VkFormat 133

pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK = VkFormat 134

pattern VK_FORMAT_BC2_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC2_UNORM_BLOCK = VkFormat 135

pattern VK_FORMAT_BC2_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_BC2_SRGB_BLOCK = VkFormat 136

pattern VK_FORMAT_BC3_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC3_UNORM_BLOCK = VkFormat 137

pattern VK_FORMAT_BC3_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_BC3_SRGB_BLOCK = VkFormat 138

pattern VK_FORMAT_BC4_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC4_UNORM_BLOCK = VkFormat 139

pattern VK_FORMAT_BC4_SNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC4_SNORM_BLOCK = VkFormat 140

pattern VK_FORMAT_BC5_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC5_UNORM_BLOCK = VkFormat 141

pattern VK_FORMAT_BC5_SNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC5_SNORM_BLOCK = VkFormat 142

pattern VK_FORMAT_BC6H_UFLOAT_BLOCK :: VkFormat

pattern VK_FORMAT_BC6H_UFLOAT_BLOCK = VkFormat 143

pattern VK_FORMAT_BC6H_SFLOAT_BLOCK :: VkFormat

pattern VK_FORMAT_BC6H_SFLOAT_BLOCK = VkFormat 144

pattern VK_FORMAT_BC7_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC7_UNORM_BLOCK = VkFormat 145

pattern VK_FORMAT_BC7_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_BC7_SRGB_BLOCK = VkFormat 146

pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = VkFormat 147

pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = VkFormat 148

pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = VkFormat 149

pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = VkFormat 150

pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = VkFormat 151

pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = VkFormat 152

pattern VK_FORMAT_EAC_R11_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_EAC_R11_UNORM_BLOCK = VkFormat 153

pattern VK_FORMAT_EAC_R11_SNORM_BLOCK :: VkFormat

pattern VK_FORMAT_EAC_R11_SNORM_BLOCK = VkFormat 154

pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK = VkFormat 155

pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK :: VkFormat

pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK = VkFormat 156

pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK = VkFormat 157

pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK = VkFormat 158

pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK = VkFormat 159

pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK = VkFormat 160

pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK = VkFormat 161

pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK = VkFormat 162

pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK = VkFormat 163

pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK = VkFormat 164

pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK = VkFormat 165

pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK = VkFormat 166

pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK = VkFormat 167

pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK = VkFormat 168

pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK = VkFormat 169

pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK = VkFormat 170

pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK = VkFormat 171

pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK = VkFormat 172

pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK = VkFormat 173

pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK = VkFormat 174

pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK = VkFormat 175

pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK = VkFormat 176

pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK = VkFormat 177

pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK = VkFormat 178

pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK = VkFormat 179

pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK = VkFormat 180

pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK = VkFormat 181

pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK = VkFormat 182

pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK = VkFormat 183

pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK = VkFormat 184

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFormatFeatureFlagBits.html VkFormatFeatureFlagBits registry at www.khronos.org>
newtype VkFormatFeatureFlagBits = VkFormatFeatureFlagBits Int32
                                    deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                              Enum, Data, Generic)

instance Show VkFormatFeatureFlagBits where
        showsPrec _ VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT
          = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT"
        showsPrec _ VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT
          = showString "VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT"
        showsPrec _ VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
          = showString "VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT"
        showsPrec _ VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
          = showString "VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT"
        showsPrec _ VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
          = showString "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT"
        showsPrec _ VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
          = showString "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT"
        showsPrec _ VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT
          = showString "VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT"
        showsPrec _ VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
          = showString "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT"
        showsPrec _ VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
          = showString "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT"
        showsPrec _ VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
          = showString "VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT"
        showsPrec _ VK_FORMAT_FEATURE_BLIT_SRC_BIT
          = showString "VK_FORMAT_FEATURE_BLIT_SRC_BIT"
        showsPrec _ VK_FORMAT_FEATURE_BLIT_DST_BIT
          = showString "VK_FORMAT_FEATURE_BLIT_DST_BIT"
        showsPrec _ VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
          = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT"
        showsPrec p (VkFormatFeatureFlagBits x)
          = showParen (p >= 11)
              (showString "VkFormatFeatureFlagBits " . showsPrec 11 x)

instance Read VkFormatFeatureFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT",
                   pure VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT),
                  ("VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT",
                   pure VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT),
                  ("VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT",
                   pure VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT),
                  ("VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT",
                   pure VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT),
                  ("VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT",
                   pure VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT),
                  ("VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT",
                   pure VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT),
                  ("VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT",
                   pure VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT),
                  ("VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT",
                   pure VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT),
                  ("VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT",
                   pure VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT),
                  ("VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT",
                   pure VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT),
                  ("VK_FORMAT_FEATURE_BLIT_SRC_BIT",
                   pure VK_FORMAT_FEATURE_BLIT_SRC_BIT),
                  ("VK_FORMAT_FEATURE_BLIT_DST_BIT",
                   pure VK_FORMAT_FEATURE_BLIT_DST_BIT),
                  ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT",
                   pure VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkFormatFeatureFlagBits") >>
                      (VkFormatFeatureFlagBits <$> step readPrec)))

-- | Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
--
--   bitpos = @0@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT =
        VkFormatFeatureFlagBits 1

-- | Format can be used for storage images (STORAGE_IMAGE descriptor type)
--
--   bitpos = @1@
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT =
        VkFormatFeatureFlagBits 2

-- | Format supports atomic operations in case it is used for storage images
--
--   bitpos = @2@
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT =
        VkFormatFeatureFlagBits 4

-- | Format can be used for uniform texel buffers (TBOs)
--
--   bitpos = @3@
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT =
        VkFormatFeatureFlagBits 8

-- | Format can be used for storage texel buffers (IBOs)
--
--   bitpos = @4@
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT =
        VkFormatFeatureFlagBits 16

-- | Format supports atomic operations in case it is used for storage texel buffers
--
--   bitpos = @5@
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT =
        VkFormatFeatureFlagBits 32

-- | Format can be used for vertex buffers (VBOs)
--
--   bitpos = @6@
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT =
        VkFormatFeatureFlagBits 64

-- | Format can be used for color attachment images
--
--   bitpos = @7@
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT =
        VkFormatFeatureFlagBits 128

-- | Format supports blending in case it is used for color attachment images
--
--   bitpos = @8@
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT =
        VkFormatFeatureFlagBits 256

-- | Format can be used for depth/stencil attachment images
--
--   bitpos = @9@
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT =
        VkFormatFeatureFlagBits 512

-- | Format can be used as the source image of blits with vkCmdBlitImage
--
--   bitpos = @10@
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT =
        VkFormatFeatureFlagBits 1024

-- | Format can be used as the destination image of blits with vkCmdBlitImage
--
--   bitpos = @11@
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_BLIT_DST_BIT =
        VkFormatFeatureFlagBits 2048

-- | Format can be filtered with VK_FILTER_LINEAR when being sampled
--
--   bitpos = @12@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT =
        VkFormatFeatureFlagBits 4096

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFrontFace.html VkFrontFace registry at www.khronos.org>
newtype VkFrontFace = VkFrontFace Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkFrontFace where
        showsPrec _ VK_FRONT_FACE_COUNTER_CLOCKWISE
          = showString "VK_FRONT_FACE_COUNTER_CLOCKWISE"
        showsPrec _ VK_FRONT_FACE_CLOCKWISE
          = showString "VK_FRONT_FACE_CLOCKWISE"
        showsPrec p (VkFrontFace x)
          = showParen (p >= 11) (showString "VkFrontFace " . showsPrec 11 x)

instance Read VkFrontFace where
        readPrec
          = parens
              (choose
                 [("VK_FRONT_FACE_COUNTER_CLOCKWISE",
                   pure VK_FRONT_FACE_COUNTER_CLOCKWISE),
                  ("VK_FRONT_FACE_CLOCKWISE", pure VK_FRONT_FACE_CLOCKWISE)]
                 +++
                 prec 10
                   (expectP (Ident "VkFrontFace") >> (VkFrontFace <$> step readPrec)))

pattern VK_FRONT_FACE_COUNTER_CLOCKWISE :: VkFrontFace

pattern VK_FRONT_FACE_COUNTER_CLOCKWISE = VkFrontFace 0

pattern VK_FRONT_FACE_CLOCKWISE :: VkFrontFace

pattern VK_FRONT_FACE_CLOCKWISE = VkFrontFace 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageAspectFlagBits.html VkImageAspectFlagBits registry at www.khronos.org>
newtype VkImageAspectFlagBits = VkImageAspectFlagBits Int32
                                  deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                            Data, Generic)

instance Show VkImageAspectFlagBits where
        showsPrec _ VK_IMAGE_ASPECT_COLOR_BIT
          = showString "VK_IMAGE_ASPECT_COLOR_BIT"
        showsPrec _ VK_IMAGE_ASPECT_DEPTH_BIT
          = showString "VK_IMAGE_ASPECT_DEPTH_BIT"
        showsPrec _ VK_IMAGE_ASPECT_STENCIL_BIT
          = showString "VK_IMAGE_ASPECT_STENCIL_BIT"
        showsPrec _ VK_IMAGE_ASPECT_METADATA_BIT
          = showString "VK_IMAGE_ASPECT_METADATA_BIT"
        showsPrec p (VkImageAspectFlagBits x)
          = showParen (p >= 11)
              (showString "VkImageAspectFlagBits " . showsPrec 11 x)

instance Read VkImageAspectFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_ASPECT_COLOR_BIT", pure VK_IMAGE_ASPECT_COLOR_BIT),
                  ("VK_IMAGE_ASPECT_DEPTH_BIT", pure VK_IMAGE_ASPECT_DEPTH_BIT),
                  ("VK_IMAGE_ASPECT_STENCIL_BIT", pure VK_IMAGE_ASPECT_STENCIL_BIT),
                  ("VK_IMAGE_ASPECT_METADATA_BIT",
                   pure VK_IMAGE_ASPECT_METADATA_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageAspectFlagBits") >>
                      (VkImageAspectFlagBits <$> step readPrec)))

-- | bitpos = @0@
pattern VK_IMAGE_ASPECT_COLOR_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_COLOR_BIT = VkImageAspectFlagBits 1

-- | bitpos = @1@
pattern VK_IMAGE_ASPECT_DEPTH_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_DEPTH_BIT = VkImageAspectFlagBits 2

-- | bitpos = @2@
pattern VK_IMAGE_ASPECT_STENCIL_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_STENCIL_BIT = VkImageAspectFlagBits 4

-- | bitpos = @3@
pattern VK_IMAGE_ASPECT_METADATA_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_METADATA_BIT = VkImageAspectFlagBits 8

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageCreateFlagBits.html VkImageCreateFlagBits registry at www.khronos.org>
newtype VkImageCreateFlagBits = VkImageCreateFlagBits Int32
                                  deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                            Data, Generic)

instance Show VkImageCreateFlagBits where
        showsPrec _ VK_IMAGE_CREATE_SPARSE_BINDING_BIT
          = showString "VK_IMAGE_CREATE_SPARSE_BINDING_BIT"
        showsPrec _ VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
          = showString "VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
        showsPrec _ VK_IMAGE_CREATE_SPARSE_ALIASED_BIT
          = showString "VK_IMAGE_CREATE_SPARSE_ALIASED_BIT"
        showsPrec _ VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT
          = showString "VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT"
        showsPrec _ VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT
          = showString "VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
        showsPrec p (VkImageCreateFlagBits x)
          = showParen (p >= 11)
              (showString "VkImageCreateFlagBits " . showsPrec 11 x)

instance Read VkImageCreateFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_CREATE_SPARSE_BINDING_BIT",
                   pure VK_IMAGE_CREATE_SPARSE_BINDING_BIT),
                  ("VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT",
                   pure VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT),
                  ("VK_IMAGE_CREATE_SPARSE_ALIASED_BIT",
                   pure VK_IMAGE_CREATE_SPARSE_ALIASED_BIT),
                  ("VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT",
                   pure VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT),
                  ("VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT",
                   pure VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageCreateFlagBits") >>
                      (VkImageCreateFlagBits <$> step readPrec)))

-- | Image should support sparse backing
--
--   bitpos = @0@
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT =
        VkImageCreateFlagBits 1

-- | Image should support sparse backing with partial residency
--
--   bitpos = @1@
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT =
        VkImageCreateFlagBits 2

-- | Image should support constent data access to physical memory ranges mapped into multiple locations of sparse images
--
--   bitpos = @2@
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT =
        VkImageCreateFlagBits 4

-- | Allows image views to have different format than the base image
--
--   bitpos = @3@
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT =
        VkImageCreateFlagBits 8

-- | Allows creating image views with cube type from the created image
--
--   bitpos = @4@
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT =
        VkImageCreateFlagBits 16

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageLayout.html VkImageLayout registry at www.khronos.org>
newtype VkImageLayout = VkImageLayout Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageLayout where
        showsPrec _ VK_IMAGE_LAYOUT_UNDEFINED
          = showString "VK_IMAGE_LAYOUT_UNDEFINED"
        showsPrec _ VK_IMAGE_LAYOUT_GENERAL
          = showString "VK_IMAGE_LAYOUT_GENERAL"
        showsPrec _ VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_PREINITIALIZED
          = showString "VK_IMAGE_LAYOUT_PREINITIALIZED"
        showsPrec p (VkImageLayout x)
          = showParen (p >= 11)
              (showString "VkImageLayout " . showsPrec 11 x)

instance Read VkImageLayout where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_LAYOUT_UNDEFINED", pure VK_IMAGE_LAYOUT_UNDEFINED),
                  ("VK_IMAGE_LAYOUT_GENERAL", pure VK_IMAGE_LAYOUT_GENERAL),
                  ("VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_PREINITIALIZED",
                   pure VK_IMAGE_LAYOUT_PREINITIALIZED)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageLayout") >>
                      (VkImageLayout <$> step readPrec)))

-- | Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
pattern VK_IMAGE_LAYOUT_UNDEFINED :: VkImageLayout

pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0

-- | General layout when image can be used for any kind of access
pattern VK_IMAGE_LAYOUT_GENERAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout 1

-- | Optimal layout when image is only used for color attachment read/write
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VkImageLayout 2

-- | Optimal layout when image is only used for depth/stencil attachment read/write
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL ::
        VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL =
        VkImageLayout 3

-- | Optimal layout when image is used for read only depth/stencil attachment and shader access
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL ::
        VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL =
        VkImageLayout 4

-- | Optimal layout when image is used for read only shader access
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VkImageLayout 5

-- | Optimal layout when image is used only as source of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VkImageLayout 6

-- | Optimal layout when image is used only as destination of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VkImageLayout 7

-- | Initial layout used when the data is populated by the CPU
pattern VK_IMAGE_LAYOUT_PREINITIALIZED :: VkImageLayout

pattern VK_IMAGE_LAYOUT_PREINITIALIZED = VkImageLayout 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageTiling.html VkImageTiling registry at www.khronos.org>
newtype VkImageTiling = VkImageTiling Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageTiling where
        showsPrec _ VK_IMAGE_TILING_OPTIMAL
          = showString "VK_IMAGE_TILING_OPTIMAL"
        showsPrec _ VK_IMAGE_TILING_LINEAR
          = showString "VK_IMAGE_TILING_LINEAR"
        showsPrec p (VkImageTiling x)
          = showParen (p >= 11)
              (showString "VkImageTiling " . showsPrec 11 x)

instance Read VkImageTiling where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_TILING_OPTIMAL", pure VK_IMAGE_TILING_OPTIMAL),
                  ("VK_IMAGE_TILING_LINEAR", pure VK_IMAGE_TILING_LINEAR)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageTiling") >>
                      (VkImageTiling <$> step readPrec)))

pattern VK_IMAGE_TILING_OPTIMAL :: VkImageTiling

pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0

pattern VK_IMAGE_TILING_LINEAR :: VkImageTiling

pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageType.html VkImageType registry at www.khronos.org>
newtype VkImageType = VkImageType Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageType where
        showsPrec _ VK_IMAGE_TYPE_1D = showString "VK_IMAGE_TYPE_1D"
        showsPrec _ VK_IMAGE_TYPE_2D = showString "VK_IMAGE_TYPE_2D"
        showsPrec _ VK_IMAGE_TYPE_3D = showString "VK_IMAGE_TYPE_3D"
        showsPrec p (VkImageType x)
          = showParen (p >= 11) (showString "VkImageType " . showsPrec 11 x)

instance Read VkImageType where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_TYPE_1D", pure VK_IMAGE_TYPE_1D),
                  ("VK_IMAGE_TYPE_2D", pure VK_IMAGE_TYPE_2D),
                  ("VK_IMAGE_TYPE_3D", pure VK_IMAGE_TYPE_3D)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageType") >> (VkImageType <$> step readPrec)))

pattern VK_IMAGE_TYPE_1D :: VkImageType

pattern VK_IMAGE_TYPE_1D = VkImageType 0

pattern VK_IMAGE_TYPE_2D :: VkImageType

pattern VK_IMAGE_TYPE_2D = VkImageType 1

pattern VK_IMAGE_TYPE_3D :: VkImageType

pattern VK_IMAGE_TYPE_3D = VkImageType 2

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageUsageFlagBits.html VkImageUsageFlagBits registry at www.khronos.org>
newtype VkImageUsageFlagBits = VkImageUsageFlagBits Int32
                                 deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                           Data, Generic)

instance Show VkImageUsageFlagBits where
        showsPrec _ VK_IMAGE_USAGE_TRANSFER_SRC_BIT
          = showString "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
        showsPrec _ VK_IMAGE_USAGE_TRANSFER_DST_BIT
          = showString "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
        showsPrec _ VK_IMAGE_USAGE_SAMPLED_BIT
          = showString "VK_IMAGE_USAGE_SAMPLED_BIT"
        showsPrec _ VK_IMAGE_USAGE_STORAGE_BIT
          = showString "VK_IMAGE_USAGE_STORAGE_BIT"
        showsPrec _ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
        showsPrec _ VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
        showsPrec _ VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
        showsPrec _ VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
        showsPrec p (VkImageUsageFlagBits x)
          = showParen (p >= 11)
              (showString "VkImageUsageFlagBits " . showsPrec 11 x)

instance Read VkImageUsageFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_USAGE_TRANSFER_SRC_BIT",
                   pure VK_IMAGE_USAGE_TRANSFER_SRC_BIT),
                  ("VK_IMAGE_USAGE_TRANSFER_DST_BIT",
                   pure VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                  ("VK_IMAGE_USAGE_SAMPLED_BIT", pure VK_IMAGE_USAGE_SAMPLED_BIT),
                  ("VK_IMAGE_USAGE_STORAGE_BIT", pure VK_IMAGE_USAGE_STORAGE_BIT),
                  ("VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
                  ("VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT),
                  ("VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT),
                  ("VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageUsageFlagBits") >>
                      (VkImageUsageFlagBits <$> step readPrec)))

-- | Can be used as a source of transfer operations
--
--   bitpos = @0@
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageFlagBits 1

-- | Can be used as a destination of transfer operations
--
--   bitpos = @1@
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageFlagBits 2

-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
--
--   bitpos = @2@
pattern VK_IMAGE_USAGE_SAMPLED_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageFlagBits 4

-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
--
--   bitpos = @3@
pattern VK_IMAGE_USAGE_STORAGE_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageFlagBits 8

-- | Can be used as framebuffer color attachment
--
--   bitpos = @4@
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT =
        VkImageUsageFlagBits 16

-- | Can be used as framebuffer depth/stencil attachment
--
--   bitpos = @5@
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT ::
        VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT =
        VkImageUsageFlagBits 32

-- | Image data not needed outside of rendering
--
--   bitpos = @6@
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT ::
        VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT =
        VkImageUsageFlagBits 64

-- | Can be used as framebuffer input attachment
--
--   bitpos = @7@
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT =
        VkImageUsageFlagBits 128

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageViewType.html VkImageViewType registry at www.khronos.org>
newtype VkImageViewType = VkImageViewType Int32
                            deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageViewType where
        showsPrec _ VK_IMAGE_VIEW_TYPE_1D
          = showString "VK_IMAGE_VIEW_TYPE_1D"
        showsPrec _ VK_IMAGE_VIEW_TYPE_2D
          = showString "VK_IMAGE_VIEW_TYPE_2D"
        showsPrec _ VK_IMAGE_VIEW_TYPE_3D
          = showString "VK_IMAGE_VIEW_TYPE_3D"
        showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE
          = showString "VK_IMAGE_VIEW_TYPE_CUBE"
        showsPrec _ VK_IMAGE_VIEW_TYPE_1D_ARRAY
          = showString "VK_IMAGE_VIEW_TYPE_1D_ARRAY"
        showsPrec _ VK_IMAGE_VIEW_TYPE_2D_ARRAY
          = showString "VK_IMAGE_VIEW_TYPE_2D_ARRAY"
        showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
          = showString "VK_IMAGE_VIEW_TYPE_CUBE_ARRAY"
        showsPrec p (VkImageViewType x)
          = showParen (p >= 11)
              (showString "VkImageViewType " . showsPrec 11 x)

instance Read VkImageViewType where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_VIEW_TYPE_1D", pure VK_IMAGE_VIEW_TYPE_1D),
                  ("VK_IMAGE_VIEW_TYPE_2D", pure VK_IMAGE_VIEW_TYPE_2D),
                  ("VK_IMAGE_VIEW_TYPE_3D", pure VK_IMAGE_VIEW_TYPE_3D),
                  ("VK_IMAGE_VIEW_TYPE_CUBE", pure VK_IMAGE_VIEW_TYPE_CUBE),
                  ("VK_IMAGE_VIEW_TYPE_1D_ARRAY", pure VK_IMAGE_VIEW_TYPE_1D_ARRAY),
                  ("VK_IMAGE_VIEW_TYPE_2D_ARRAY", pure VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                  ("VK_IMAGE_VIEW_TYPE_CUBE_ARRAY",
                   pure VK_IMAGE_VIEW_TYPE_CUBE_ARRAY)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageViewType") >>
                      (VkImageViewType <$> step readPrec)))

pattern VK_IMAGE_VIEW_TYPE_1D :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_1D = VkImageViewType 0

pattern VK_IMAGE_VIEW_TYPE_2D :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_2D = VkImageViewType 1

pattern VK_IMAGE_VIEW_TYPE_3D :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_3D = VkImageViewType 2

pattern VK_IMAGE_VIEW_TYPE_CUBE :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_CUBE = VkImageViewType 3

pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY = VkImageViewType 4

pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY = VkImageViewType 5

pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = VkImageViewType 6

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSharingMode.html VkSharingMode registry at www.khronos.org>
newtype VkSharingMode = VkSharingMode Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSharingMode where
        showsPrec _ VK_SHARING_MODE_EXCLUSIVE
          = showString "VK_SHARING_MODE_EXCLUSIVE"
        showsPrec _ VK_SHARING_MODE_CONCURRENT
          = showString "VK_SHARING_MODE_CONCURRENT"
        showsPrec p (VkSharingMode x)
          = showParen (p >= 11)
              (showString "VkSharingMode " . showsPrec 11 x)

instance Read VkSharingMode where
        readPrec
          = parens
              (choose
                 [("VK_SHARING_MODE_EXCLUSIVE", pure VK_SHARING_MODE_EXCLUSIVE),
                  ("VK_SHARING_MODE_CONCURRENT", pure VK_SHARING_MODE_CONCURRENT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSharingMode") >>
                      (VkSharingMode <$> step readPrec)))

pattern VK_SHARING_MODE_EXCLUSIVE :: VkSharingMode

pattern VK_SHARING_MODE_EXCLUSIVE = VkSharingMode 0

pattern VK_SHARING_MODE_CONCURRENT :: VkSharingMode

pattern VK_SHARING_MODE_CONCURRENT = VkSharingMode 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkIndexType.html VkIndexType registry at www.khronos.org>
newtype VkIndexType = VkIndexType Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkIndexType where
        showsPrec _ VK_INDEX_TYPE_UINT16
          = showString "VK_INDEX_TYPE_UINT16"
        showsPrec _ VK_INDEX_TYPE_UINT32
          = showString "VK_INDEX_TYPE_UINT32"
        showsPrec p (VkIndexType x)
          = showParen (p >= 11) (showString "VkIndexType " . showsPrec 11 x)

instance Read VkIndexType where
        readPrec
          = parens
              (choose
                 [("VK_INDEX_TYPE_UINT16", pure VK_INDEX_TYPE_UINT16),
                  ("VK_INDEX_TYPE_UINT32", pure VK_INDEX_TYPE_UINT32)]
                 +++
                 prec 10
                   (expectP (Ident "VkIndexType") >> (VkIndexType <$> step readPrec)))

pattern VK_INDEX_TYPE_UINT16 :: VkIndexType

pattern VK_INDEX_TYPE_UINT16 = VkIndexType 0

pattern VK_INDEX_TYPE_UINT32 :: VkIndexType

pattern VK_INDEX_TYPE_UINT32 = VkIndexType 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkLogicOp.html VkLogicOp registry at www.khronos.org>
newtype VkLogicOp = VkLogicOp Int32
                      deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkLogicOp where
        showsPrec _ VK_LOGIC_OP_CLEAR = showString "VK_LOGIC_OP_CLEAR"
        showsPrec _ VK_LOGIC_OP_AND = showString "VK_LOGIC_OP_AND"
        showsPrec _ VK_LOGIC_OP_AND_REVERSE
          = showString "VK_LOGIC_OP_AND_REVERSE"
        showsPrec _ VK_LOGIC_OP_COPY = showString "VK_LOGIC_OP_COPY"
        showsPrec _ VK_LOGIC_OP_AND_INVERTED
          = showString "VK_LOGIC_OP_AND_INVERTED"
        showsPrec _ VK_LOGIC_OP_NO_OP = showString "VK_LOGIC_OP_NO_OP"
        showsPrec _ VK_LOGIC_OP_XOR = showString "VK_LOGIC_OP_XOR"
        showsPrec _ VK_LOGIC_OP_OR = showString "VK_LOGIC_OP_OR"
        showsPrec _ VK_LOGIC_OP_NOR = showString "VK_LOGIC_OP_NOR"
        showsPrec _ VK_LOGIC_OP_EQUIVALENT
          = showString "VK_LOGIC_OP_EQUIVALENT"
        showsPrec _ VK_LOGIC_OP_INVERT = showString "VK_LOGIC_OP_INVERT"
        showsPrec _ VK_LOGIC_OP_OR_REVERSE
          = showString "VK_LOGIC_OP_OR_REVERSE"
        showsPrec _ VK_LOGIC_OP_COPY_INVERTED
          = showString "VK_LOGIC_OP_COPY_INVERTED"
        showsPrec _ VK_LOGIC_OP_OR_INVERTED
          = showString "VK_LOGIC_OP_OR_INVERTED"
        showsPrec _ VK_LOGIC_OP_NAND = showString "VK_LOGIC_OP_NAND"
        showsPrec _ VK_LOGIC_OP_SET = showString "VK_LOGIC_OP_SET"
        showsPrec p (VkLogicOp x)
          = showParen (p >= 11) (showString "VkLogicOp " . showsPrec 11 x)

instance Read VkLogicOp where
        readPrec
          = parens
              (choose
                 [("VK_LOGIC_OP_CLEAR", pure VK_LOGIC_OP_CLEAR),
                  ("VK_LOGIC_OP_AND", pure VK_LOGIC_OP_AND),
                  ("VK_LOGIC_OP_AND_REVERSE", pure VK_LOGIC_OP_AND_REVERSE),
                  ("VK_LOGIC_OP_COPY", pure VK_LOGIC_OP_COPY),
                  ("VK_LOGIC_OP_AND_INVERTED", pure VK_LOGIC_OP_AND_INVERTED),
                  ("VK_LOGIC_OP_NO_OP", pure VK_LOGIC_OP_NO_OP),
                  ("VK_LOGIC_OP_XOR", pure VK_LOGIC_OP_XOR),
                  ("VK_LOGIC_OP_OR", pure VK_LOGIC_OP_OR),
                  ("VK_LOGIC_OP_NOR", pure VK_LOGIC_OP_NOR),
                  ("VK_LOGIC_OP_EQUIVALENT", pure VK_LOGIC_OP_EQUIVALENT),
                  ("VK_LOGIC_OP_INVERT", pure VK_LOGIC_OP_INVERT),
                  ("VK_LOGIC_OP_OR_REVERSE", pure VK_LOGIC_OP_OR_REVERSE),
                  ("VK_LOGIC_OP_COPY_INVERTED", pure VK_LOGIC_OP_COPY_INVERTED),
                  ("VK_LOGIC_OP_OR_INVERTED", pure VK_LOGIC_OP_OR_INVERTED),
                  ("VK_LOGIC_OP_NAND", pure VK_LOGIC_OP_NAND),
                  ("VK_LOGIC_OP_SET", pure VK_LOGIC_OP_SET)]
                 +++
                 prec 10
                   (expectP (Ident "VkLogicOp") >> (VkLogicOp <$> step readPrec)))

pattern VK_LOGIC_OP_CLEAR :: VkLogicOp

pattern VK_LOGIC_OP_CLEAR = VkLogicOp 0

pattern VK_LOGIC_OP_AND :: VkLogicOp

pattern VK_LOGIC_OP_AND = VkLogicOp 1

pattern VK_LOGIC_OP_AND_REVERSE :: VkLogicOp

pattern VK_LOGIC_OP_AND_REVERSE = VkLogicOp 2

pattern VK_LOGIC_OP_COPY :: VkLogicOp

pattern VK_LOGIC_OP_COPY = VkLogicOp 3

pattern VK_LOGIC_OP_AND_INVERTED :: VkLogicOp

pattern VK_LOGIC_OP_AND_INVERTED = VkLogicOp 4

pattern VK_LOGIC_OP_NO_OP :: VkLogicOp

pattern VK_LOGIC_OP_NO_OP = VkLogicOp 5

pattern VK_LOGIC_OP_XOR :: VkLogicOp

pattern VK_LOGIC_OP_XOR = VkLogicOp 6

pattern VK_LOGIC_OP_OR :: VkLogicOp

pattern VK_LOGIC_OP_OR = VkLogicOp 7

pattern VK_LOGIC_OP_NOR :: VkLogicOp

pattern VK_LOGIC_OP_NOR = VkLogicOp 8

pattern VK_LOGIC_OP_EQUIVALENT :: VkLogicOp

pattern VK_LOGIC_OP_EQUIVALENT = VkLogicOp 9

pattern VK_LOGIC_OP_INVERT :: VkLogicOp

pattern VK_LOGIC_OP_INVERT = VkLogicOp 10

pattern VK_LOGIC_OP_OR_REVERSE :: VkLogicOp

pattern VK_LOGIC_OP_OR_REVERSE = VkLogicOp 11

pattern VK_LOGIC_OP_COPY_INVERTED :: VkLogicOp

pattern VK_LOGIC_OP_COPY_INVERTED = VkLogicOp 12

pattern VK_LOGIC_OP_OR_INVERTED :: VkLogicOp

pattern VK_LOGIC_OP_OR_INVERTED = VkLogicOp 13

pattern VK_LOGIC_OP_NAND :: VkLogicOp

pattern VK_LOGIC_OP_NAND = VkLogicOp 14

pattern VK_LOGIC_OP_SET :: VkLogicOp

pattern VK_LOGIC_OP_SET = VkLogicOp 15

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryHeapFlagBits.html VkMemoryHeapFlagBits registry at www.khronos.org>
newtype VkMemoryHeapFlagBits = VkMemoryHeapFlagBits Int32
                                 deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                           Data, Generic)

instance Show VkMemoryHeapFlagBits where
        showsPrec _ VK_MEMORY_HEAP_DEVICE_LOCAL_BIT
          = showString "VK_MEMORY_HEAP_DEVICE_LOCAL_BIT"
        showsPrec p (VkMemoryHeapFlagBits x)
          = showParen (p >= 11)
              (showString "VkMemoryHeapFlagBits " . showsPrec 11 x)

instance Read VkMemoryHeapFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_MEMORY_HEAP_DEVICE_LOCAL_BIT",
                   pure VK_MEMORY_HEAP_DEVICE_LOCAL_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkMemoryHeapFlagBits") >>
                      (VkMemoryHeapFlagBits <$> step readPrec)))

-- | If set, heap represents device memory
--
--   bitpos = @0@
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT :: VkMemoryHeapFlagBits

pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapFlagBits 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAccessFlagBits.html VkAccessFlagBits registry at www.khronos.org>
newtype VkAccessFlagBits = VkAccessFlagBits Int32
                             deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                       Data, Generic)

instance Show VkAccessFlagBits where
        showsPrec _ VK_ACCESS_INDIRECT_COMMAND_READ_BIT
          = showString "VK_ACCESS_INDIRECT_COMMAND_READ_BIT"
        showsPrec _ VK_ACCESS_INDEX_READ_BIT
          = showString "VK_ACCESS_INDEX_READ_BIT"
        showsPrec _ VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT
          = showString "VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT"
        showsPrec _ VK_ACCESS_UNIFORM_READ_BIT
          = showString "VK_ACCESS_UNIFORM_READ_BIT"
        showsPrec _ VK_ACCESS_INPUT_ATTACHMENT_READ_BIT
          = showString "VK_ACCESS_INPUT_ATTACHMENT_READ_BIT"
        showsPrec _ VK_ACCESS_SHADER_READ_BIT
          = showString "VK_ACCESS_SHADER_READ_BIT"
        showsPrec _ VK_ACCESS_SHADER_WRITE_BIT
          = showString "VK_ACCESS_SHADER_WRITE_BIT"
        showsPrec _ VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
          = showString "VK_ACCESS_COLOR_ATTACHMENT_READ_BIT"
        showsPrec _ VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          = showString "VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT"
        showsPrec _ VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
          = showString "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT"
        showsPrec _ VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
          = showString "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT"
        showsPrec _ VK_ACCESS_TRANSFER_READ_BIT
          = showString "VK_ACCESS_TRANSFER_READ_BIT"
        showsPrec _ VK_ACCESS_TRANSFER_WRITE_BIT
          = showString "VK_ACCESS_TRANSFER_WRITE_BIT"
        showsPrec _ VK_ACCESS_HOST_READ_BIT
          = showString "VK_ACCESS_HOST_READ_BIT"
        showsPrec _ VK_ACCESS_HOST_WRITE_BIT
          = showString "VK_ACCESS_HOST_WRITE_BIT"
        showsPrec _ VK_ACCESS_MEMORY_READ_BIT
          = showString "VK_ACCESS_MEMORY_READ_BIT"
        showsPrec _ VK_ACCESS_MEMORY_WRITE_BIT
          = showString "VK_ACCESS_MEMORY_WRITE_BIT"
        showsPrec p (VkAccessFlagBits x)
          = showParen (p >= 11)
              (showString "VkAccessFlagBits " . showsPrec 11 x)

instance Read VkAccessFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_ACCESS_INDIRECT_COMMAND_READ_BIT",
                   pure VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                  ("VK_ACCESS_INDEX_READ_BIT", pure VK_ACCESS_INDEX_READ_BIT),
                  ("VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT",
                   pure VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT),
                  ("VK_ACCESS_UNIFORM_READ_BIT", pure VK_ACCESS_UNIFORM_READ_BIT),
                  ("VK_ACCESS_INPUT_ATTACHMENT_READ_BIT",
                   pure VK_ACCESS_INPUT_ATTACHMENT_READ_BIT),
                  ("VK_ACCESS_SHADER_READ_BIT", pure VK_ACCESS_SHADER_READ_BIT),
                  ("VK_ACCESS_SHADER_WRITE_BIT", pure VK_ACCESS_SHADER_WRITE_BIT),
                  ("VK_ACCESS_COLOR_ATTACHMENT_READ_BIT",
                   pure VK_ACCESS_COLOR_ATTACHMENT_READ_BIT),
                  ("VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT",
                   pure VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                  ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT",
                   pure VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT),
                  ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT",
                   pure VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT),
                  ("VK_ACCESS_TRANSFER_READ_BIT", pure VK_ACCESS_TRANSFER_READ_BIT),
                  ("VK_ACCESS_TRANSFER_WRITE_BIT",
                   pure VK_ACCESS_TRANSFER_WRITE_BIT),
                  ("VK_ACCESS_HOST_READ_BIT", pure VK_ACCESS_HOST_READ_BIT),
                  ("VK_ACCESS_HOST_WRITE_BIT", pure VK_ACCESS_HOST_WRITE_BIT),
                  ("VK_ACCESS_MEMORY_READ_BIT", pure VK_ACCESS_MEMORY_READ_BIT),
                  ("VK_ACCESS_MEMORY_WRITE_BIT", pure VK_ACCESS_MEMORY_WRITE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkAccessFlagBits") >>
                      (VkAccessFlagBits <$> step readPrec)))

-- | Controls coherency of indirect command reads
--
--   bitpos = @0@
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT = VkAccessFlagBits 1

-- | Controls coherency of index reads
--
--   bitpos = @1@
pattern VK_ACCESS_INDEX_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_INDEX_READ_BIT = VkAccessFlagBits 2

-- | Controls coherency of vertex attribute reads
--
--   bitpos = @2@
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VkAccessFlagBits 4

-- | Controls coherency of uniform buffer reads
--
--   bitpos = @3@
pattern VK_ACCESS_UNIFORM_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_UNIFORM_READ_BIT = VkAccessFlagBits 8

-- | Controls coherency of input attachment reads
--
--   bitpos = @4@
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = VkAccessFlagBits 16

-- | Controls coherency of shader reads
--
--   bitpos = @5@
pattern VK_ACCESS_SHADER_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_SHADER_READ_BIT = VkAccessFlagBits 32

-- | Controls coherency of shader writes
--
--   bitpos = @6@
pattern VK_ACCESS_SHADER_WRITE_BIT :: VkAccessFlagBits

pattern VK_ACCESS_SHADER_WRITE_BIT = VkAccessFlagBits 64

-- | Controls coherency of color attachment reads
--
--   bitpos = @7@
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = VkAccessFlagBits 128

-- | Controls coherency of color attachment writes
--
--   bitpos = @8@
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT :: VkAccessFlagBits

pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 256

-- | Controls coherency of depth/stencil attachment reads
--
--   bitpos = @9@
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT ::
        VkAccessFlagBits

pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT =
        VkAccessFlagBits 512

-- | Controls coherency of depth/stencil attachment writes
--
--   bitpos = @10@
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT ::
        VkAccessFlagBits

pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT =
        VkAccessFlagBits 1024

-- | Controls coherency of transfer reads
--
--   bitpos = @11@
pattern VK_ACCESS_TRANSFER_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_TRANSFER_READ_BIT = VkAccessFlagBits 2048

-- | Controls coherency of transfer writes
--
--   bitpos = @12@
pattern VK_ACCESS_TRANSFER_WRITE_BIT :: VkAccessFlagBits

pattern VK_ACCESS_TRANSFER_WRITE_BIT = VkAccessFlagBits 4096

-- | Controls coherency of host reads
--
--   bitpos = @13@
pattern VK_ACCESS_HOST_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_HOST_READ_BIT = VkAccessFlagBits 8192

-- | Controls coherency of host writes
--
--   bitpos = @14@
pattern VK_ACCESS_HOST_WRITE_BIT :: VkAccessFlagBits

pattern VK_ACCESS_HOST_WRITE_BIT = VkAccessFlagBits 16384

-- | Controls coherency of memory reads
--
--   bitpos = @15@
pattern VK_ACCESS_MEMORY_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_MEMORY_READ_BIT = VkAccessFlagBits 32768

-- | Controls coherency of memory writes
--
--   bitpos = @16@
pattern VK_ACCESS_MEMORY_WRITE_BIT :: VkAccessFlagBits

pattern VK_ACCESS_MEMORY_WRITE_BIT = VkAccessFlagBits 65536

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryPropertyFlagBits.html VkMemoryPropertyFlagBits registry at www.khronos.org>
newtype VkMemoryPropertyFlagBits = VkMemoryPropertyFlagBits Int32
                                     deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                               Enum, Data, Generic)

instance Show VkMemoryPropertyFlagBits where
        showsPrec _ VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
          = showString "VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
        showsPrec _ VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
          = showString "VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT"
        showsPrec _ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
          = showString "VK_MEMORY_PROPERTY_HOST_COHERENT_BIT"
        showsPrec _ VK_MEMORY_PROPERTY_HOST_CACHED_BIT
          = showString "VK_MEMORY_PROPERTY_HOST_CACHED_BIT"
        showsPrec _ VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
          = showString "VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
        showsPrec p (VkMemoryPropertyFlagBits x)
          = showParen (p >= 11)
              (showString "VkMemoryPropertyFlagBits " . showsPrec 11 x)

instance Read VkMemoryPropertyFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT",
                   pure VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                  ("VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT",
                   pure VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                  ("VK_MEMORY_PROPERTY_HOST_COHERENT_BIT",
                   pure VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                  ("VK_MEMORY_PROPERTY_HOST_CACHED_BIT",
                   pure VK_MEMORY_PROPERTY_HOST_CACHED_BIT),
                  ("VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT",
                   pure VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkMemoryPropertyFlagBits") >>
                      (VkMemoryPropertyFlagBits <$> step readPrec)))

-- | If otherwise stated, then allocate memory on device
--
--   bitpos = @0@
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT =
        VkMemoryPropertyFlagBits 1

-- | Memory is mappable by host
--
--   bitpos = @1@
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT =
        VkMemoryPropertyFlagBits 2

-- | Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
--
--   bitpos = @2@
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT =
        VkMemoryPropertyFlagBits 4

-- | Memory will be cached by the host
--
--   bitpos = @3@
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT =
        VkMemoryPropertyFlagBits 8

-- | Memory may be allocated by the driver when it is required
--
--   bitpos = @4@
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT =
        VkMemoryPropertyFlagBits 16

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceType.html VkPhysicalDeviceType registry at www.khronos.org>
newtype VkPhysicalDeviceType = VkPhysicalDeviceType Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkPhysicalDeviceType where
        showsPrec _ VK_PHYSICAL_DEVICE_TYPE_OTHER
          = showString "VK_PHYSICAL_DEVICE_TYPE_OTHER"
        showsPrec _ VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
          = showString "VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU"
        showsPrec _ VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
          = showString "VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU"
        showsPrec _ VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
          = showString "VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU"
        showsPrec _ VK_PHYSICAL_DEVICE_TYPE_CPU
          = showString "VK_PHYSICAL_DEVICE_TYPE_CPU"
        showsPrec p (VkPhysicalDeviceType x)
          = showParen (p >= 11)
              (showString "VkPhysicalDeviceType " . showsPrec 11 x)

instance Read VkPhysicalDeviceType where
        readPrec
          = parens
              (choose
                 [("VK_PHYSICAL_DEVICE_TYPE_OTHER",
                   pure VK_PHYSICAL_DEVICE_TYPE_OTHER),
                  ("VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU",
                   pure VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU),
                  ("VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU",
                   pure VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU),
                  ("VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU",
                   pure VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU),
                  ("VK_PHYSICAL_DEVICE_TYPE_CPU", pure VK_PHYSICAL_DEVICE_TYPE_CPU)]
                 +++
                 prec 10
                   (expectP (Ident "VkPhysicalDeviceType") >>
                      (VkPhysicalDeviceType <$> step readPrec)))

pattern VK_PHYSICAL_DEVICE_TYPE_OTHER :: VkPhysicalDeviceType

pattern VK_PHYSICAL_DEVICE_TYPE_OTHER = VkPhysicalDeviceType 0

pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU ::
        VkPhysicalDeviceType

pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU =
        VkPhysicalDeviceType 1

pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU ::
        VkPhysicalDeviceType

pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU =
        VkPhysicalDeviceType 2

pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU :: VkPhysicalDeviceType

pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU =
        VkPhysicalDeviceType 3

pattern VK_PHYSICAL_DEVICE_TYPE_CPU :: VkPhysicalDeviceType

pattern VK_PHYSICAL_DEVICE_TYPE_CPU = VkPhysicalDeviceType 4

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineBindPoint.html VkPipelineBindPoint registry at www.khronos.org>
newtype VkPipelineBindPoint = VkPipelineBindPoint Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkPipelineBindPoint where
        showsPrec _ VK_PIPELINE_BIND_POINT_GRAPHICS
          = showString "VK_PIPELINE_BIND_POINT_GRAPHICS"
        showsPrec _ VK_PIPELINE_BIND_POINT_COMPUTE
          = showString "VK_PIPELINE_BIND_POINT_COMPUTE"
        showsPrec p (VkPipelineBindPoint x)
          = showParen (p >= 11)
              (showString "VkPipelineBindPoint " . showsPrec 11 x)

instance Read VkPipelineBindPoint where
        readPrec
          = parens
              (choose
                 [("VK_PIPELINE_BIND_POINT_GRAPHICS",
                   pure VK_PIPELINE_BIND_POINT_GRAPHICS),
                  ("VK_PIPELINE_BIND_POINT_COMPUTE",
                   pure VK_PIPELINE_BIND_POINT_COMPUTE)]
                 +++
                 prec 10
                   (expectP (Ident "VkPipelineBindPoint") >>
                      (VkPipelineBindPoint <$> step readPrec)))

pattern VK_PIPELINE_BIND_POINT_GRAPHICS :: VkPipelineBindPoint

pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0

pattern VK_PIPELINE_BIND_POINT_COMPUTE :: VkPipelineBindPoint

pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineCreateFlagBits.html VkPipelineCreateFlagBits registry at www.khronos.org>
newtype VkPipelineCreateFlagBits = VkPipelineCreateFlagBits Int32
                                     deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                               Enum, Data, Generic)

instance Show VkPipelineCreateFlagBits where
        showsPrec _ VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
          = showString "VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
        showsPrec _ VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
          = showString "VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
        showsPrec _ VK_PIPELINE_CREATE_DERIVATIVE_BIT
          = showString "VK_PIPELINE_CREATE_DERIVATIVE_BIT"
        showsPrec p (VkPipelineCreateFlagBits x)
          = showParen (p >= 11)
              (showString "VkPipelineCreateFlagBits " . showsPrec 11 x)

instance Read VkPipelineCreateFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT",
                   pure VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT),
                  ("VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT",
                   pure VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT),
                  ("VK_PIPELINE_CREATE_DERIVATIVE_BIT",
                   pure VK_PIPELINE_CREATE_DERIVATIVE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkPipelineCreateFlagBits") >>
                      (VkPipelineCreateFlagBits <$> step readPrec)))

-- | bitpos = @0@
pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT =
        VkPipelineCreateFlagBits 1

-- | bitpos = @1@
pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT =
        VkPipelineCreateFlagBits 2

-- | bitpos = @2@
pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT =
        VkPipelineCreateFlagBits 4

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPrimitiveTopology.html VkPrimitiveTopology registry at www.khronos.org>
newtype VkPrimitiveTopology = VkPrimitiveTopology Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkPrimitiveTopology where
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_POINT_LIST
          = showString "VK_PRIMITIVE_TOPOLOGY_POINT_LIST"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_LIST
          = showString "VK_PRIMITIVE_TOPOLOGY_LINE_LIST"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_STRIP
          = showString "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
          = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
          = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
          = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
          = showString "VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
          = showString "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
          = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
          = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
          = showString "VK_PRIMITIVE_TOPOLOGY_PATCH_LIST"
        showsPrec p (VkPrimitiveTopology x)
          = showParen (p >= 11)
              (showString "VkPrimitiveTopology " . showsPrec 11 x)

instance Read VkPrimitiveTopology where
        readPrec
          = parens
              (choose
                 [("VK_PRIMITIVE_TOPOLOGY_POINT_LIST",
                   pure VK_PRIMITIVE_TOPOLOGY_POINT_LIST),
                  ("VK_PRIMITIVE_TOPOLOGY_LINE_LIST",
                   pure VK_PRIMITIVE_TOPOLOGY_LINE_LIST),
                  ("VK_PRIMITIVE_TOPOLOGY_LINE_STRIP",
                   pure VK_PRIMITIVE_TOPOLOGY_LINE_STRIP),
                  ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST",
                   pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST),
                  ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP",
                   pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP),
                  ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN",
                   pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN),
                  ("VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY",
                   pure VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY),
                  ("VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY",
                   pure VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY),
                  ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY",
                   pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY),
                  ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY",
                   pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY),
                  ("VK_PRIMITIVE_TOPOLOGY_PATCH_LIST",
                   pure VK_PRIMITIVE_TOPOLOGY_PATCH_LIST)]
                 +++
                 prec 10
                   (expectP (Ident "VkPrimitiveTopology") >>
                      (VkPrimitiveTopology <$> step readPrec)))

pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST = VkPrimitiveTopology 0

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST = VkPrimitiveTopology 1

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP = VkPrimitiveTopology 2

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = VkPrimitiveTopology 3

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP =
        VkPrimitiveTopology 4

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = VkPrimitiveTopology 5

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY ::
        VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY =
        VkPrimitiveTopology 6

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY ::
        VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY =
        VkPrimitiveTopology 7

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY ::
        VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY =
        VkPrimitiveTopology 8

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY ::
        VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY =
        VkPrimitiveTopology 9

pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST = VkPrimitiveTopology 10

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueryControlFlagBits.html VkQueryControlFlagBits registry at www.khronos.org>
newtype VkQueryControlFlagBits = VkQueryControlFlagBits Int32
                                   deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                             Enum, Data, Generic)

instance Show VkQueryControlFlagBits where
        showsPrec _ VK_QUERY_CONTROL_PRECISE_BIT
          = showString "VK_QUERY_CONTROL_PRECISE_BIT"
        showsPrec p (VkQueryControlFlagBits x)
          = showParen (p >= 11)
              (showString "VkQueryControlFlagBits " . showsPrec 11 x)

instance Read VkQueryControlFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_CONTROL_PRECISE_BIT",
                   pure VK_QUERY_CONTROL_PRECISE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryControlFlagBits") >>
                      (VkQueryControlFlagBits <$> step readPrec)))

-- | Require precise results to be collected by the query
--
--   bitpos = @0@
pattern VK_QUERY_CONTROL_PRECISE_BIT :: VkQueryControlFlagBits

pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlFlagBits 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueryPipelineStatisticFlagBits.html VkQueryPipelineStatisticFlagBits registry at www.khronos.org>
newtype VkQueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits Int32
                                             deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                       Storable, Enum, Data, Generic)

instance Show VkQueryPipelineStatisticFlagBits where
        showsPrec _ VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT"
        showsPrec _ VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT
          = showString "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT"
        showsPrec _ VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT
          = showString "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT"
        showsPrec p (VkQueryPipelineStatisticFlagBits x)
          = showParen (p >= 11)
              (showString "VkQueryPipelineStatisticFlagBits " . showsPrec 11 x)

instance Read VkQueryPipelineStatisticFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT",
                   pure
                     VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT",
                   pure
                     VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryPipelineStatisticFlagBits") >>
                      (VkQueryPipelineStatisticFlagBits <$> step readPrec)))

-- | Optional
--
--   bitpos = @0@
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT ::
        VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT =
        VkQueryPipelineStatisticFlagBits 1

-- | Optional
--
--   bitpos = @1@
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT =
        VkQueryPipelineStatisticFlagBits 2

-- | Optional
--
--   bitpos = @2@
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT =
        VkQueryPipelineStatisticFlagBits 4

-- | Optional
--
--   bitpos = @3@
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticFlagBits 8

-- | Optional
--
--   bitpos = @4@
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
        = VkQueryPipelineStatisticFlagBits 16

-- | Optional
--
--   bitpos = @5@
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT ::
        VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT =
        VkQueryPipelineStatisticFlagBits 32

-- | Optional
--
--   bitpos = @6@
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT ::
        VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT =
        VkQueryPipelineStatisticFlagBits 64

-- | Optional
--
--   bitpos = @7@
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticFlagBits 128

-- | Optional
--
--   bitpos = @8@
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
        = VkQueryPipelineStatisticFlagBits 256

-- | Optional
--
--   bitpos = @9@
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticFlagBits 512

-- | Optional
--
--   bitpos = @10@
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticFlagBits 1024

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueryResultFlagBits.html VkQueryResultFlagBits registry at www.khronos.org>
newtype VkQueryResultFlagBits = VkQueryResultFlagBits Int32
                                  deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                            Data, Generic)

instance Show VkQueryResultFlagBits where
        showsPrec _ VK_QUERY_RESULT_64_BIT
          = showString "VK_QUERY_RESULT_64_BIT"
        showsPrec _ VK_QUERY_RESULT_WAIT_BIT
          = showString "VK_QUERY_RESULT_WAIT_BIT"
        showsPrec _ VK_QUERY_RESULT_WITH_AVAILABILITY_BIT
          = showString "VK_QUERY_RESULT_WITH_AVAILABILITY_BIT"
        showsPrec _ VK_QUERY_RESULT_PARTIAL_BIT
          = showString "VK_QUERY_RESULT_PARTIAL_BIT"
        showsPrec p (VkQueryResultFlagBits x)
          = showParen (p >= 11)
              (showString "VkQueryResultFlagBits " . showsPrec 11 x)

instance Read VkQueryResultFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_RESULT_64_BIT", pure VK_QUERY_RESULT_64_BIT),
                  ("VK_QUERY_RESULT_WAIT_BIT", pure VK_QUERY_RESULT_WAIT_BIT),
                  ("VK_QUERY_RESULT_WITH_AVAILABILITY_BIT",
                   pure VK_QUERY_RESULT_WITH_AVAILABILITY_BIT),
                  ("VK_QUERY_RESULT_PARTIAL_BIT", pure VK_QUERY_RESULT_PARTIAL_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryResultFlagBits") >>
                      (VkQueryResultFlagBits <$> step readPrec)))

-- | Results of the queries are written to the destination buffer as 64-bit values
--
--   bitpos = @0@
pattern VK_QUERY_RESULT_64_BIT :: VkQueryResultFlagBits

pattern VK_QUERY_RESULT_64_BIT = VkQueryResultFlagBits 1

-- | Results of the queries are waited on before proceeding with the result copy
--
--   bitpos = @1@
pattern VK_QUERY_RESULT_WAIT_BIT :: VkQueryResultFlagBits

pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultFlagBits 2

-- | Besides the results of the query, the availability of the results is also written
--
--   bitpos = @2@
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT ::
        VkQueryResultFlagBits

pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT =
        VkQueryResultFlagBits 4

-- | Copy the partial results of the query even if the final results are not available
--
--   bitpos = @3@
pattern VK_QUERY_RESULT_PARTIAL_BIT :: VkQueryResultFlagBits

pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultFlagBits 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueryType.html VkQueryType registry at www.khronos.org>
newtype VkQueryType = VkQueryType Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkQueryType where
        showsPrec _ VK_QUERY_TYPE_OCCLUSION
          = showString "VK_QUERY_TYPE_OCCLUSION"
        showsPrec _ VK_QUERY_TYPE_PIPELINE_STATISTICS
          = showString "VK_QUERY_TYPE_PIPELINE_STATISTICS"
        showsPrec _ VK_QUERY_TYPE_TIMESTAMP
          = showString "VK_QUERY_TYPE_TIMESTAMP"
        showsPrec p (VkQueryType x)
          = showParen (p >= 11) (showString "VkQueryType " . showsPrec 11 x)

instance Read VkQueryType where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_TYPE_OCCLUSION", pure VK_QUERY_TYPE_OCCLUSION),
                  ("VK_QUERY_TYPE_PIPELINE_STATISTICS",
                   pure VK_QUERY_TYPE_PIPELINE_STATISTICS),
                  ("VK_QUERY_TYPE_TIMESTAMP", pure VK_QUERY_TYPE_TIMESTAMP)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryType") >> (VkQueryType <$> step readPrec)))

pattern VK_QUERY_TYPE_OCCLUSION :: VkQueryType

pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0

-- | Optional
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS :: VkQueryType

pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1

pattern VK_QUERY_TYPE_TIMESTAMP :: VkQueryType

pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueueFlagBits.html VkQueueFlagBits registry at www.khronos.org>
newtype VkQueueFlagBits = VkQueueFlagBits Int32
                            deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                      Data, Generic)

instance Show VkQueueFlagBits where
        showsPrec _ VK_QUEUE_GRAPHICS_BIT
          = showString "VK_QUEUE_GRAPHICS_BIT"
        showsPrec _ VK_QUEUE_COMPUTE_BIT
          = showString "VK_QUEUE_COMPUTE_BIT"
        showsPrec _ VK_QUEUE_TRANSFER_BIT
          = showString "VK_QUEUE_TRANSFER_BIT"
        showsPrec _ VK_QUEUE_SPARSE_BINDING_BIT
          = showString "VK_QUEUE_SPARSE_BINDING_BIT"
        showsPrec p (VkQueueFlagBits x)
          = showParen (p >= 11)
              (showString "VkQueueFlagBits " . showsPrec 11 x)

instance Read VkQueueFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_QUEUE_GRAPHICS_BIT", pure VK_QUEUE_GRAPHICS_BIT),
                  ("VK_QUEUE_COMPUTE_BIT", pure VK_QUEUE_COMPUTE_BIT),
                  ("VK_QUEUE_TRANSFER_BIT", pure VK_QUEUE_TRANSFER_BIT),
                  ("VK_QUEUE_SPARSE_BINDING_BIT", pure VK_QUEUE_SPARSE_BINDING_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueueFlagBits") >>
                      (VkQueueFlagBits <$> step readPrec)))

-- | Queue supports graphics operations
--
--   bitpos = @0@
pattern VK_QUEUE_GRAPHICS_BIT :: VkQueueFlagBits

pattern VK_QUEUE_GRAPHICS_BIT = VkQueueFlagBits 1

-- | Queue supports compute operations
--
--   bitpos = @1@
pattern VK_QUEUE_COMPUTE_BIT :: VkQueueFlagBits

pattern VK_QUEUE_COMPUTE_BIT = VkQueueFlagBits 2

-- | Queue supports transfer operations
--
--   bitpos = @2@
pattern VK_QUEUE_TRANSFER_BIT :: VkQueueFlagBits

pattern VK_QUEUE_TRANSFER_BIT = VkQueueFlagBits 4

-- | Queue supports sparse resource memory management operations
--
--   bitpos = @3@
pattern VK_QUEUE_SPARSE_BINDING_BIT :: VkQueueFlagBits

pattern VK_QUEUE_SPARSE_BINDING_BIT = VkQueueFlagBits 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubpassContents.html VkSubpassContents registry at www.khronos.org>
newtype VkSubpassContents = VkSubpassContents Int32
                              deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSubpassContents where
        showsPrec _ VK_SUBPASS_CONTENTS_INLINE
          = showString "VK_SUBPASS_CONTENTS_INLINE"
        showsPrec _ VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
          = showString "VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS"
        showsPrec p (VkSubpassContents x)
          = showParen (p >= 11)
              (showString "VkSubpassContents " . showsPrec 11 x)

instance Read VkSubpassContents where
        readPrec
          = parens
              (choose
                 [("VK_SUBPASS_CONTENTS_INLINE", pure VK_SUBPASS_CONTENTS_INLINE),
                  ("VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS",
                   pure VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS)]
                 +++
                 prec 10
                   (expectP (Ident "VkSubpassContents") >>
                      (VkSubpassContents <$> step readPrec)))

pattern VK_SUBPASS_CONTENTS_INLINE :: VkSubpassContents

pattern VK_SUBPASS_CONTENTS_INLINE = VkSubpassContents 0

pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS ::
        VkSubpassContents

pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS =
        VkSubpassContents 1

-- | API result codes
--
--   type = @enum@
--
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkResult.html VkResult registry at www.khronos.org>
newtype VkResult = VkResult Int32
                     deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkResult where
        showsPrec _ VK_SUCCESS = showString "VK_SUCCESS"
        showsPrec _ VK_NOT_READY = showString "VK_NOT_READY"
        showsPrec _ VK_TIMEOUT = showString "VK_TIMEOUT"
        showsPrec _ VK_EVENT_SET = showString "VK_EVENT_SET"
        showsPrec _ VK_EVENT_RESET = showString "VK_EVENT_RESET"
        showsPrec _ VK_INCOMPLETE = showString "VK_INCOMPLETE"
        showsPrec _ VK_ERROR_OUT_OF_HOST_MEMORY
          = showString "VK_ERROR_OUT_OF_HOST_MEMORY"
        showsPrec _ VK_ERROR_OUT_OF_DEVICE_MEMORY
          = showString "VK_ERROR_OUT_OF_DEVICE_MEMORY"
        showsPrec _ VK_ERROR_INITIALIZATION_FAILED
          = showString "VK_ERROR_INITIALIZATION_FAILED"
        showsPrec _ VK_ERROR_DEVICE_LOST
          = showString "VK_ERROR_DEVICE_LOST"
        showsPrec _ VK_ERROR_MEMORY_MAP_FAILED
          = showString "VK_ERROR_MEMORY_MAP_FAILED"
        showsPrec _ VK_ERROR_LAYER_NOT_PRESENT
          = showString "VK_ERROR_LAYER_NOT_PRESENT"
        showsPrec _ VK_ERROR_EXTENSION_NOT_PRESENT
          = showString "VK_ERROR_EXTENSION_NOT_PRESENT"
        showsPrec _ VK_ERROR_FEATURE_NOT_PRESENT
          = showString "VK_ERROR_FEATURE_NOT_PRESENT"
        showsPrec _ VK_ERROR_INCOMPATIBLE_DRIVER
          = showString "VK_ERROR_INCOMPATIBLE_DRIVER"
        showsPrec _ VK_ERROR_TOO_MANY_OBJECTS
          = showString "VK_ERROR_TOO_MANY_OBJECTS"
        showsPrec _ VK_ERROR_FORMAT_NOT_SUPPORTED
          = showString "VK_ERROR_FORMAT_NOT_SUPPORTED"
        showsPrec _ VK_ERROR_FRAGMENTED_POOL
          = showString "VK_ERROR_FRAGMENTED_POOL"
        showsPrec p (VkResult x)
          = showParen (p >= 11) (showString "VkResult " . showsPrec 11 x)

instance Read VkResult where
        readPrec
          = parens
              (choose
                 [("VK_SUCCESS", pure VK_SUCCESS),
                  ("VK_NOT_READY", pure VK_NOT_READY),
                  ("VK_TIMEOUT", pure VK_TIMEOUT),
                  ("VK_EVENT_SET", pure VK_EVENT_SET),
                  ("VK_EVENT_RESET", pure VK_EVENT_RESET),
                  ("VK_INCOMPLETE", pure VK_INCOMPLETE),
                  ("VK_ERROR_OUT_OF_HOST_MEMORY", pure VK_ERROR_OUT_OF_HOST_MEMORY),
                  ("VK_ERROR_OUT_OF_DEVICE_MEMORY",
                   pure VK_ERROR_OUT_OF_DEVICE_MEMORY),
                  ("VK_ERROR_INITIALIZATION_FAILED",
                   pure VK_ERROR_INITIALIZATION_FAILED),
                  ("VK_ERROR_DEVICE_LOST", pure VK_ERROR_DEVICE_LOST),
                  ("VK_ERROR_MEMORY_MAP_FAILED", pure VK_ERROR_MEMORY_MAP_FAILED),
                  ("VK_ERROR_LAYER_NOT_PRESENT", pure VK_ERROR_LAYER_NOT_PRESENT),
                  ("VK_ERROR_EXTENSION_NOT_PRESENT",
                   pure VK_ERROR_EXTENSION_NOT_PRESENT),
                  ("VK_ERROR_FEATURE_NOT_PRESENT",
                   pure VK_ERROR_FEATURE_NOT_PRESENT),
                  ("VK_ERROR_INCOMPATIBLE_DRIVER",
                   pure VK_ERROR_INCOMPATIBLE_DRIVER),
                  ("VK_ERROR_TOO_MANY_OBJECTS", pure VK_ERROR_TOO_MANY_OBJECTS),
                  ("VK_ERROR_FORMAT_NOT_SUPPORTED",
                   pure VK_ERROR_FORMAT_NOT_SUPPORTED),
                  ("VK_ERROR_FRAGMENTED_POOL", pure VK_ERROR_FRAGMENTED_POOL)]
                 +++
                 prec 10
                   (expectP (Ident "VkResult") >> (VkResult <$> step readPrec)))

-- | Command completed successfully
pattern VK_SUCCESS :: VkResult

pattern VK_SUCCESS = VkResult 0

-- | A fence or query has not yet completed
pattern VK_NOT_READY :: VkResult

pattern VK_NOT_READY = VkResult 1

-- | A wait operation has not completed in the specified time
pattern VK_TIMEOUT :: VkResult

pattern VK_TIMEOUT = VkResult 2

-- | An event is signaled
pattern VK_EVENT_SET :: VkResult

pattern VK_EVENT_SET = VkResult 3

-- | An event is unsignaled
pattern VK_EVENT_RESET :: VkResult

pattern VK_EVENT_RESET = VkResult 4

-- | A return array was too small for the result
pattern VK_INCOMPLETE :: VkResult

pattern VK_INCOMPLETE = VkResult 5

-- | A host memory allocation has failed
pattern VK_ERROR_OUT_OF_HOST_MEMORY :: VkResult

pattern VK_ERROR_OUT_OF_HOST_MEMORY = VkResult (-1)

-- | A device memory allocation has failed
pattern VK_ERROR_OUT_OF_DEVICE_MEMORY :: VkResult

pattern VK_ERROR_OUT_OF_DEVICE_MEMORY = VkResult (-2)

-- | Initialization of a object has failed
pattern VK_ERROR_INITIALIZATION_FAILED :: VkResult

pattern VK_ERROR_INITIALIZATION_FAILED = VkResult (-3)

-- | The logical device has been lost. See <<devsandqueues-lost-device>>
pattern VK_ERROR_DEVICE_LOST :: VkResult

pattern VK_ERROR_DEVICE_LOST = VkResult (-4)

-- | Mapping of a memory object has failed
pattern VK_ERROR_MEMORY_MAP_FAILED :: VkResult

pattern VK_ERROR_MEMORY_MAP_FAILED = VkResult (-5)

-- | Layer specified does not exist
pattern VK_ERROR_LAYER_NOT_PRESENT :: VkResult

pattern VK_ERROR_LAYER_NOT_PRESENT = VkResult (-6)

-- | Extension specified does not exist
pattern VK_ERROR_EXTENSION_NOT_PRESENT :: VkResult

pattern VK_ERROR_EXTENSION_NOT_PRESENT = VkResult (-7)

-- | Requested feature is not available on this device
pattern VK_ERROR_FEATURE_NOT_PRESENT :: VkResult

pattern VK_ERROR_FEATURE_NOT_PRESENT = VkResult (-8)

-- | Unable to find a Vulkan driver
pattern VK_ERROR_INCOMPATIBLE_DRIVER :: VkResult

pattern VK_ERROR_INCOMPATIBLE_DRIVER = VkResult (-9)

-- | Too many objects of the type have already been created
pattern VK_ERROR_TOO_MANY_OBJECTS :: VkResult

pattern VK_ERROR_TOO_MANY_OBJECTS = VkResult (-10)

-- | Requested format is not supported on this device
pattern VK_ERROR_FORMAT_NOT_SUPPORTED :: VkResult

pattern VK_ERROR_FORMAT_NOT_SUPPORTED = VkResult (-11)

-- | A requested pool allocation has failed due to fragmentation of the pool's memory -- ' closing tick for hsc2hs
pattern VK_ERROR_FRAGMENTED_POOL :: VkResult

pattern VK_ERROR_FRAGMENTED_POOL = VkResult (-12)

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkShaderStageFlagBits.html VkShaderStageFlagBits registry at www.khronos.org>
newtype VkShaderStageFlagBits = VkShaderStageFlagBits Int32
                                  deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                            Data, Generic)

instance Show VkShaderStageFlagBits where
        showsPrec _ VK_SHADER_STAGE_VERTEX_BIT
          = showString "VK_SHADER_STAGE_VERTEX_BIT"
        showsPrec _ VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
          = showString "VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT"
        showsPrec _ VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
          = showString "VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT"
        showsPrec _ VK_SHADER_STAGE_GEOMETRY_BIT
          = showString "VK_SHADER_STAGE_GEOMETRY_BIT"
        showsPrec _ VK_SHADER_STAGE_FRAGMENT_BIT
          = showString "VK_SHADER_STAGE_FRAGMENT_BIT"
        showsPrec _ VK_SHADER_STAGE_COMPUTE_BIT
          = showString "VK_SHADER_STAGE_COMPUTE_BIT"
        showsPrec _ VK_SHADER_STAGE_ALL_GRAPHICS
          = showString "VK_SHADER_STAGE_ALL_GRAPHICS"
        showsPrec _ VK_SHADER_STAGE_ALL = showString "VK_SHADER_STAGE_ALL"
        showsPrec p (VkShaderStageFlagBits x)
          = showParen (p >= 11)
              (showString "VkShaderStageFlagBits " . showsPrec 11 x)

instance Read VkShaderStageFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_SHADER_STAGE_VERTEX_BIT", pure VK_SHADER_STAGE_VERTEX_BIT),
                  ("VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT",
                   pure VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT),
                  ("VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT",
                   pure VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT),
                  ("VK_SHADER_STAGE_GEOMETRY_BIT",
                   pure VK_SHADER_STAGE_GEOMETRY_BIT),
                  ("VK_SHADER_STAGE_FRAGMENT_BIT",
                   pure VK_SHADER_STAGE_FRAGMENT_BIT),
                  ("VK_SHADER_STAGE_COMPUTE_BIT", pure VK_SHADER_STAGE_COMPUTE_BIT),
                  ("VK_SHADER_STAGE_ALL_GRAPHICS",
                   pure VK_SHADER_STAGE_ALL_GRAPHICS),
                  ("VK_SHADER_STAGE_ALL", pure VK_SHADER_STAGE_ALL)]
                 +++
                 prec 10
                   (expectP (Ident "VkShaderStageFlagBits") >>
                      (VkShaderStageFlagBits <$> step readPrec)))

-- | bitpos = @0@
pattern VK_SHADER_STAGE_VERTEX_BIT :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_VERTEX_BIT = VkShaderStageFlagBits 1

-- | bitpos = @1@
pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT ::
        VkShaderStageFlagBits

pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT =
        VkShaderStageFlagBits 2

-- | bitpos = @2@
pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT ::
        VkShaderStageFlagBits

pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT =
        VkShaderStageFlagBits 4

-- | bitpos = @3@
pattern VK_SHADER_STAGE_GEOMETRY_BIT :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_GEOMETRY_BIT = VkShaderStageFlagBits 8

-- | bitpos = @4@
pattern VK_SHADER_STAGE_FRAGMENT_BIT :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_FRAGMENT_BIT = VkShaderStageFlagBits 16

-- | bitpos = @5@
pattern VK_SHADER_STAGE_COMPUTE_BIT :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_COMPUTE_BIT = VkShaderStageFlagBits 32

pattern VK_SHADER_STAGE_ALL_GRAPHICS :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_ALL_GRAPHICS = VkShaderStageFlagBits 31

pattern VK_SHADER_STAGE_ALL :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_ALL = VkShaderStageFlagBits 2147483647

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSparseMemoryBindFlagBits.html VkSparseMemoryBindFlagBits registry at www.khronos.org>
newtype VkSparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits Int32
                                       deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                                 Enum, Data, Generic)

instance Show VkSparseMemoryBindFlagBits where
        showsPrec _ VK_SPARSE_MEMORY_BIND_METADATA_BIT
          = showString "VK_SPARSE_MEMORY_BIND_METADATA_BIT"
        showsPrec p (VkSparseMemoryBindFlagBits x)
          = showParen (p >= 11)
              (showString "VkSparseMemoryBindFlagBits " . showsPrec 11 x)

instance Read VkSparseMemoryBindFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_SPARSE_MEMORY_BIND_METADATA_BIT",
                   pure VK_SPARSE_MEMORY_BIND_METADATA_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSparseMemoryBindFlagBits") >>
                      (VkSparseMemoryBindFlagBits <$> step readPrec)))

-- | Operation binds resource metadata to memory
--
--   bitpos = @0@
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT ::
        VkSparseMemoryBindFlagBits

pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT =
        VkSparseMemoryBindFlagBits 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkStencilFaceFlagBits.html VkStencilFaceFlagBits registry at www.khronos.org>
newtype VkStencilFaceFlagBits = VkStencilFaceFlagBits Int32
                                  deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                            Data, Generic)

instance Show VkStencilFaceFlagBits where
        showsPrec _ VK_STENCIL_FACE_FRONT_BIT
          = showString "VK_STENCIL_FACE_FRONT_BIT"
        showsPrec _ VK_STENCIL_FACE_BACK_BIT
          = showString "VK_STENCIL_FACE_BACK_BIT"
        showsPrec _ VK_STENCIL_FRONT_AND_BACK
          = showString "VK_STENCIL_FRONT_AND_BACK"
        showsPrec p (VkStencilFaceFlagBits x)
          = showParen (p >= 11)
              (showString "VkStencilFaceFlagBits " . showsPrec 11 x)

instance Read VkStencilFaceFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_STENCIL_FACE_FRONT_BIT", pure VK_STENCIL_FACE_FRONT_BIT),
                  ("VK_STENCIL_FACE_BACK_BIT", pure VK_STENCIL_FACE_BACK_BIT),
                  ("VK_STENCIL_FRONT_AND_BACK", pure VK_STENCIL_FRONT_AND_BACK)]
                 +++
                 prec 10
                   (expectP (Ident "VkStencilFaceFlagBits") >>
                      (VkStencilFaceFlagBits <$> step readPrec)))

-- | Front face
--
--   bitpos = @0@
pattern VK_STENCIL_FACE_FRONT_BIT :: VkStencilFaceFlagBits

pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceFlagBits 1

-- | Back face
--
--   bitpos = @1@
pattern VK_STENCIL_FACE_BACK_BIT :: VkStencilFaceFlagBits

pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceFlagBits 2

-- | Front and back faces
pattern VK_STENCIL_FRONT_AND_BACK :: VkStencilFaceFlagBits

pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceFlagBits 3

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkStencilOp.html VkStencilOp registry at www.khronos.org>
newtype VkStencilOp = VkStencilOp Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkStencilOp where
        showsPrec _ VK_STENCIL_OP_KEEP = showString "VK_STENCIL_OP_KEEP"
        showsPrec _ VK_STENCIL_OP_ZERO = showString "VK_STENCIL_OP_ZERO"
        showsPrec _ VK_STENCIL_OP_REPLACE
          = showString "VK_STENCIL_OP_REPLACE"
        showsPrec _ VK_STENCIL_OP_INCREMENT_AND_CLAMP
          = showString "VK_STENCIL_OP_INCREMENT_AND_CLAMP"
        showsPrec _ VK_STENCIL_OP_DECREMENT_AND_CLAMP
          = showString "VK_STENCIL_OP_DECREMENT_AND_CLAMP"
        showsPrec _ VK_STENCIL_OP_INVERT
          = showString "VK_STENCIL_OP_INVERT"
        showsPrec _ VK_STENCIL_OP_INCREMENT_AND_WRAP
          = showString "VK_STENCIL_OP_INCREMENT_AND_WRAP"
        showsPrec _ VK_STENCIL_OP_DECREMENT_AND_WRAP
          = showString "VK_STENCIL_OP_DECREMENT_AND_WRAP"
        showsPrec p (VkStencilOp x)
          = showParen (p >= 11) (showString "VkStencilOp " . showsPrec 11 x)

instance Read VkStencilOp where
        readPrec
          = parens
              (choose
                 [("VK_STENCIL_OP_KEEP", pure VK_STENCIL_OP_KEEP),
                  ("VK_STENCIL_OP_ZERO", pure VK_STENCIL_OP_ZERO),
                  ("VK_STENCIL_OP_REPLACE", pure VK_STENCIL_OP_REPLACE),
                  ("VK_STENCIL_OP_INCREMENT_AND_CLAMP",
                   pure VK_STENCIL_OP_INCREMENT_AND_CLAMP),
                  ("VK_STENCIL_OP_DECREMENT_AND_CLAMP",
                   pure VK_STENCIL_OP_DECREMENT_AND_CLAMP),
                  ("VK_STENCIL_OP_INVERT", pure VK_STENCIL_OP_INVERT),
                  ("VK_STENCIL_OP_INCREMENT_AND_WRAP",
                   pure VK_STENCIL_OP_INCREMENT_AND_WRAP),
                  ("VK_STENCIL_OP_DECREMENT_AND_WRAP",
                   pure VK_STENCIL_OP_DECREMENT_AND_WRAP)]
                 +++
                 prec 10
                   (expectP (Ident "VkStencilOp") >> (VkStencilOp <$> step readPrec)))

pattern VK_STENCIL_OP_KEEP :: VkStencilOp

pattern VK_STENCIL_OP_KEEP = VkStencilOp 0

pattern VK_STENCIL_OP_ZERO :: VkStencilOp

pattern VK_STENCIL_OP_ZERO = VkStencilOp 1

pattern VK_STENCIL_OP_REPLACE :: VkStencilOp

pattern VK_STENCIL_OP_REPLACE = VkStencilOp 2

pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP :: VkStencilOp

pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP = VkStencilOp 3

pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP :: VkStencilOp

pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP = VkStencilOp 4

pattern VK_STENCIL_OP_INVERT :: VkStencilOp

pattern VK_STENCIL_OP_INVERT = VkStencilOp 5

pattern VK_STENCIL_OP_INCREMENT_AND_WRAP :: VkStencilOp

pattern VK_STENCIL_OP_INCREMENT_AND_WRAP = VkStencilOp 6

pattern VK_STENCIL_OP_DECREMENT_AND_WRAP :: VkStencilOp

pattern VK_STENCIL_OP_DECREMENT_AND_WRAP = VkStencilOp 7

-- | Structure type enumerant
--
--   type = @enum@
--
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkStructureType.html VkStructureType registry at www.khronos.org>
newtype VkStructureType = VkStructureType Int32
                            deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkStructureType where
        showsPrec _ VK_STRUCTURE_TYPE_APPLICATION_INFO
          = showString "VK_STRUCTURE_TYPE_APPLICATION_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_SUBMIT_INFO
          = showString "VK_STRUCTURE_TYPE_SUBMIT_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
          = showString "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
          = showString "VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE"
        showsPrec _ VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
          = showString "VK_STRUCTURE_TYPE_BIND_SPARSE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_FENCE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_EVENT_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO"
        showsPrec _
          VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
          = showString
              "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO"
        showsPrec _
          VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
          = showString
              "VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO"
        showsPrec _
          VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
          = showString
              "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
          = showString
              "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO"
        showsPrec _
          VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
          = showString
              "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO"
        showsPrec _
          VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
          = showString
              "VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO"
        showsPrec _
          VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
          = showString
              "VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO"
        showsPrec _
          VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
          = showString
              "VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
          = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
          = showString "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET"
        showsPrec _ VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
          = showString "VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET"
        showsPrec _ VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
          = showString "VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
          = showString "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
          = showString "VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
          = showString "VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
          = showString "VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER"
        showsPrec _ VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
          = showString "VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER"
        showsPrec _ VK_STRUCTURE_TYPE_MEMORY_BARRIER
          = showString "VK_STRUCTURE_TYPE_MEMORY_BARRIER"
        showsPrec _ VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO"
        showsPrec _ VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
          = showString "VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO"
        showsPrec p (VkStructureType x)
          = showParen (p >= 11)
              (showString "VkStructureType " . showsPrec 11 x)

instance Read VkStructureType where
        readPrec
          = parens
              (choose
                 [("VK_STRUCTURE_TYPE_APPLICATION_INFO",
                   pure VK_STRUCTURE_TYPE_APPLICATION_INFO),
                  ("VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_SUBMIT_INFO",
                   pure VK_STRUCTURE_TYPE_SUBMIT_INFO),
                  ("VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO",
                   pure VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO),
                  ("VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE",
                   pure VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE),
                  ("VK_STRUCTURE_TYPE_BIND_SPARSE_INFO",
                   pure VK_STRUCTURE_TYPE_BIND_SPARSE_INFO),
                  ("VK_STRUCTURE_TYPE_FENCE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_FENCE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_EVENT_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_EVENT_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO",
                   pure VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO),
                  ("VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET",
                   pure VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET),
                  ("VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET",
                   pure VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET),
                  ("VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO",
                   pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO),
                  ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO",
                   pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO),
                  ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO",
                   pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO),
                  ("VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO",
                   pure VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO),
                  ("VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER",
                   pure VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER),
                  ("VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER",
                   pure VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER),
                  ("VK_STRUCTURE_TYPE_MEMORY_BARRIER",
                   pure VK_STRUCTURE_TYPE_MEMORY_BARRIER),
                  ("VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO),
                  ("VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO",
                   pure VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO)]
                 +++
                 prec 10
                   (expectP (Ident "VkStructureType") >>
                      (VkStructureType <$> step readPrec)))

pattern VK_STRUCTURE_TYPE_APPLICATION_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_APPLICATION_INFO = VkStructureType 0

pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = VkStructureType 1

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO =
        VkStructureType 2

pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = VkStructureType 3

pattern VK_STRUCTURE_TYPE_SUBMIT_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_SUBMIT_INFO = VkStructureType 4

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = VkStructureType 5

pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE :: VkStructureType

pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = VkStructureType 6

pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO = VkStructureType 7

pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = VkStructureType 8

pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = VkStructureType 9

pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO = VkStructureType 10

pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO =
        VkStructureType 11

pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = VkStructureType 12

pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO =
        VkStructureType 13

pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = VkStructureType 14

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO =
        VkStructureType 15

pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO =
        VkStructureType 16

pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO =
        VkStructureType 17

pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO =
        VkStructureType 18

pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO =
        VkStructureType 19

pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
        = VkStructureType 20

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO =
        VkStructureType 21

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO =
        VkStructureType 22

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
        = VkStructureType 23

pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO =
        VkStructureType 24

pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
        = VkStructureType 25

pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO =
        VkStructureType 26

pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO =
        VkStructureType 27

pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO =
        VkStructureType 28

pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO =
        VkStructureType 29

pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO =
        VkStructureType 30

pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = VkStructureType 31

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO =
        VkStructureType 32

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO =
        VkStructureType 33

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO =
        VkStructureType 34

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET :: VkStructureType

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = VkStructureType 35

pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET :: VkStructureType

pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = VkStructureType 36

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO =
        VkStructureType 37

pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO =
        VkStructureType 38

pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO =
        VkStructureType 39

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO =
        VkStructureType 40

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO =
        VkStructureType 41

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO =
        VkStructureType 42

pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO =
        VkStructureType 43

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER :: VkStructureType

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER =
        VkStructureType 44

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = VkStructureType 45

pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER :: VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER = VkStructureType 46

-- | Reserved for internal use by the loader, layers, and ICDs
pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO =
        VkStructureType 47

-- | Reserved for internal use by the loader, layers, and ICDs
pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO =
        VkStructureType 48

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSystemAllocationScope.html VkSystemAllocationScope registry at www.khronos.org>
newtype VkSystemAllocationScope = VkSystemAllocationScope Int32
                                    deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSystemAllocationScope where
        showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_COMMAND
          = showString "VK_SYSTEM_ALLOCATION_SCOPE_COMMAND"
        showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_OBJECT
          = showString "VK_SYSTEM_ALLOCATION_SCOPE_OBJECT"
        showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_CACHE
          = showString "VK_SYSTEM_ALLOCATION_SCOPE_CACHE"
        showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_DEVICE
          = showString "VK_SYSTEM_ALLOCATION_SCOPE_DEVICE"
        showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE
          = showString "VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE"
        showsPrec p (VkSystemAllocationScope x)
          = showParen (p >= 11)
              (showString "VkSystemAllocationScope " . showsPrec 11 x)

instance Read VkSystemAllocationScope where
        readPrec
          = parens
              (choose
                 [("VK_SYSTEM_ALLOCATION_SCOPE_COMMAND",
                   pure VK_SYSTEM_ALLOCATION_SCOPE_COMMAND),
                  ("VK_SYSTEM_ALLOCATION_SCOPE_OBJECT",
                   pure VK_SYSTEM_ALLOCATION_SCOPE_OBJECT),
                  ("VK_SYSTEM_ALLOCATION_SCOPE_CACHE",
                   pure VK_SYSTEM_ALLOCATION_SCOPE_CACHE),
                  ("VK_SYSTEM_ALLOCATION_SCOPE_DEVICE",
                   pure VK_SYSTEM_ALLOCATION_SCOPE_DEVICE),
                  ("VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE",
                   pure VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE)]
                 +++
                 prec 10
                   (expectP (Ident "VkSystemAllocationScope") >>
                      (VkSystemAllocationScope <$> step readPrec)))

pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND ::
        VkSystemAllocationScope

pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND =
        VkSystemAllocationScope 0

pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT ::
        VkSystemAllocationScope

pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT =
        VkSystemAllocationScope 1

pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE :: VkSystemAllocationScope

pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE =
        VkSystemAllocationScope 2

pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE ::
        VkSystemAllocationScope

pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE =
        VkSystemAllocationScope 3

pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE ::
        VkSystemAllocationScope

pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE =
        VkSystemAllocationScope 4

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkInternalAllocationType.html VkInternalAllocationType registry at www.khronos.org>
newtype VkInternalAllocationType = VkInternalAllocationType Int32
                                     deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkInternalAllocationType where
        showsPrec _ VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE
          = showString "VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE"
        showsPrec p (VkInternalAllocationType x)
          = showParen (p >= 11)
              (showString "VkInternalAllocationType " . showsPrec 11 x)

instance Read VkInternalAllocationType where
        readPrec
          = parens
              (choose
                 [("VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE",
                   pure VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE)]
                 +++
                 prec 10
                   (expectP (Ident "VkInternalAllocationType") >>
                      (VkInternalAllocationType <$> step readPrec)))

pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE ::
        VkInternalAllocationType

pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE =
        VkInternalAllocationType 0

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerAddressMode.html VkSamplerAddressMode registry at www.khronos.org>
newtype VkSamplerAddressMode = VkSamplerAddressMode Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSamplerAddressMode where
        showsPrec _ VK_SAMPLER_ADDRESS_MODE_REPEAT
          = showString "VK_SAMPLER_ADDRESS_MODE_REPEAT"
        showsPrec _ VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
          = showString "VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT"
        showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
          = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE"
        showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
          = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER"
        showsPrec p (VkSamplerAddressMode x)
          = showParen (p >= 11)
              (showString "VkSamplerAddressMode " . showsPrec 11 x)

instance Read VkSamplerAddressMode where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_ADDRESS_MODE_REPEAT",
                   pure VK_SAMPLER_ADDRESS_MODE_REPEAT),
                  ("VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT",
                   pure VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT),
                  ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE",
                   pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE),
                  ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER",
                   pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerAddressMode") >>
                      (VkSamplerAddressMode <$> step readPrec)))

pattern VK_SAMPLER_ADDRESS_MODE_REPEAT :: VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = VkSamplerAddressMode 0

pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT =
        VkSamplerAddressMode 1

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE =
        VkSamplerAddressMode 2

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER =
        VkSamplerAddressMode 3

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFilter.html VkFilter registry at www.khronos.org>
newtype VkFilter = VkFilter Int32
                     deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkFilter where
        showsPrec _ VK_FILTER_NEAREST = showString "VK_FILTER_NEAREST"
        showsPrec _ VK_FILTER_LINEAR = showString "VK_FILTER_LINEAR"
        showsPrec p (VkFilter x)
          = showParen (p >= 11) (showString "VkFilter " . showsPrec 11 x)

instance Read VkFilter where
        readPrec
          = parens
              (choose
                 [("VK_FILTER_NEAREST", pure VK_FILTER_NEAREST),
                  ("VK_FILTER_LINEAR", pure VK_FILTER_LINEAR)]
                 +++
                 prec 10
                   (expectP (Ident "VkFilter") >> (VkFilter <$> step readPrec)))

pattern VK_FILTER_NEAREST :: VkFilter

pattern VK_FILTER_NEAREST = VkFilter 0

pattern VK_FILTER_LINEAR :: VkFilter

pattern VK_FILTER_LINEAR = VkFilter 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerMipmapMode.html VkSamplerMipmapMode registry at www.khronos.org>
newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSamplerMipmapMode where
        showsPrec _ VK_SAMPLER_MIPMAP_MODE_NEAREST
          = showString "VK_SAMPLER_MIPMAP_MODE_NEAREST"
        showsPrec _ VK_SAMPLER_MIPMAP_MODE_LINEAR
          = showString "VK_SAMPLER_MIPMAP_MODE_LINEAR"
        showsPrec p (VkSamplerMipmapMode x)
          = showParen (p >= 11)
              (showString "VkSamplerMipmapMode " . showsPrec 11 x)

instance Read VkSamplerMipmapMode where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_MIPMAP_MODE_NEAREST",
                   pure VK_SAMPLER_MIPMAP_MODE_NEAREST),
                  ("VK_SAMPLER_MIPMAP_MODE_LINEAR",
                   pure VK_SAMPLER_MIPMAP_MODE_LINEAR)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerMipmapMode") >>
                      (VkSamplerMipmapMode <$> step readPrec)))

-- | Choose nearest mip level
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST :: VkSamplerMipmapMode

pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0

-- | Linear filter between mip levels
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR :: VkSamplerMipmapMode

pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkVertexInputRate.html VkVertexInputRate registry at www.khronos.org>
newtype VkVertexInputRate = VkVertexInputRate Int32
                              deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkVertexInputRate where
        showsPrec _ VK_VERTEX_INPUT_RATE_VERTEX
          = showString "VK_VERTEX_INPUT_RATE_VERTEX"
        showsPrec _ VK_VERTEX_INPUT_RATE_INSTANCE
          = showString "VK_VERTEX_INPUT_RATE_INSTANCE"
        showsPrec p (VkVertexInputRate x)
          = showParen (p >= 11)
              (showString "VkVertexInputRate " . showsPrec 11 x)

instance Read VkVertexInputRate where
        readPrec
          = parens
              (choose
                 [("VK_VERTEX_INPUT_RATE_VERTEX", pure VK_VERTEX_INPUT_RATE_VERTEX),
                  ("VK_VERTEX_INPUT_RATE_INSTANCE",
                   pure VK_VERTEX_INPUT_RATE_INSTANCE)]
                 +++
                 prec 10
                   (expectP (Ident "VkVertexInputRate") >>
                      (VkVertexInputRate <$> step readPrec)))

pattern VK_VERTEX_INPUT_RATE_VERTEX :: VkVertexInputRate

pattern VK_VERTEX_INPUT_RATE_VERTEX = VkVertexInputRate 0

pattern VK_VERTEX_INPUT_RATE_INSTANCE :: VkVertexInputRate

pattern VK_VERTEX_INPUT_RATE_INSTANCE = VkVertexInputRate 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineStageFlagBits.html VkPipelineStageFlagBits registry at www.khronos.org>
newtype VkPipelineStageFlagBits = VkPipelineStageFlagBits Int32
                                    deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                              Enum, Data, Generic)

instance Show VkPipelineStageFlagBits where
        showsPrec _ VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
          = showString "VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT"
        showsPrec _ VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT
          = showString "VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT"
        showsPrec _ VK_PIPELINE_STAGE_VERTEX_INPUT_BIT
          = showString "VK_PIPELINE_STAGE_VERTEX_INPUT_BIT"
        showsPrec _ VK_PIPELINE_STAGE_VERTEX_SHADER_BIT
          = showString "VK_PIPELINE_STAGE_VERTEX_SHADER_BIT"
        showsPrec _ VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
          = showString "VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT"
        showsPrec _ VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
          = showString "VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT"
        showsPrec _ VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
          = showString "VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT"
        showsPrec _ VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
          = showString "VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT"
        showsPrec _ VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
          = showString "VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT"
        showsPrec _ VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
          = showString "VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT"
        showsPrec _ VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          = showString "VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT"
        showsPrec _ VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT
          = showString "VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT"
        showsPrec _ VK_PIPELINE_STAGE_TRANSFER_BIT
          = showString "VK_PIPELINE_STAGE_TRANSFER_BIT"
        showsPrec _ VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
          = showString "VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT"
        showsPrec _ VK_PIPELINE_STAGE_HOST_BIT
          = showString "VK_PIPELINE_STAGE_HOST_BIT"
        showsPrec _ VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT
          = showString "VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT"
        showsPrec _ VK_PIPELINE_STAGE_ALL_COMMANDS_BIT
          = showString "VK_PIPELINE_STAGE_ALL_COMMANDS_BIT"
        showsPrec p (VkPipelineStageFlagBits x)
          = showParen (p >= 11)
              (showString "VkPipelineStageFlagBits " . showsPrec 11 x)

instance Read VkPipelineStageFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT",
                   pure VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                  ("VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT",
                   pure VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
                  ("VK_PIPELINE_STAGE_VERTEX_INPUT_BIT",
                   pure VK_PIPELINE_STAGE_VERTEX_INPUT_BIT),
                  ("VK_PIPELINE_STAGE_VERTEX_SHADER_BIT",
                   pure VK_PIPELINE_STAGE_VERTEX_SHADER_BIT),
                  ("VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT",
                   pure VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT),
                  ("VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT",
                   pure VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT),
                  ("VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT",
                   pure VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT),
                  ("VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT",
                   pure VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                  ("VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT",
                   pure VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT),
                  ("VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT",
                   pure VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),
                  ("VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT",
                   pure VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                  ("VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT",
                   pure VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                  ("VK_PIPELINE_STAGE_TRANSFER_BIT",
                   pure VK_PIPELINE_STAGE_TRANSFER_BIT),
                  ("VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT",
                   pure VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                  ("VK_PIPELINE_STAGE_HOST_BIT", pure VK_PIPELINE_STAGE_HOST_BIT),
                  ("VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT",
                   pure VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT),
                  ("VK_PIPELINE_STAGE_ALL_COMMANDS_BIT",
                   pure VK_PIPELINE_STAGE_ALL_COMMANDS_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkPipelineStageFlagBits") >>
                      (VkPipelineStageFlagBits <$> step readPrec)))

-- | Before subsequent commands are processed
--
--   bitpos = @0@
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT =
        VkPipelineStageFlagBits 1

-- | Draw/DispatchIndirect command fetch
--
--   bitpos = @1@
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT =
        VkPipelineStageFlagBits 2

-- | Vertex/index fetch
--
--   bitpos = @2@
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT =
        VkPipelineStageFlagBits 4

-- | Vertex shading
--
--   bitpos = @3@
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT =
        VkPipelineStageFlagBits 8

-- | Tessellation control shading
--
--   bitpos = @4@
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT =
        VkPipelineStageFlagBits 16

-- | Tessellation evaluation shading
--
--   bitpos = @5@
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT =
        VkPipelineStageFlagBits 32

-- | Geometry shading
--
--   bitpos = @6@
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT =
        VkPipelineStageFlagBits 64

-- | Fragment shading
--
--   bitpos = @7@
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT =
        VkPipelineStageFlagBits 128

-- | Early fragment (depth and stencil) tests
--
--   bitpos = @8@
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT =
        VkPipelineStageFlagBits 256

-- | Late fragment (depth and stencil) tests
--
--   bitpos = @9@
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT =
        VkPipelineStageFlagBits 512

-- | Color attachment writes
--
--   bitpos = @10@
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT =
        VkPipelineStageFlagBits 1024

-- | Compute shading
--
--   bitpos = @11@
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT =
        VkPipelineStageFlagBits 2048

-- | Transfer/copy operations
--
--   bitpos = @12@
pattern VK_PIPELINE_STAGE_TRANSFER_BIT :: VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_TRANSFER_BIT =
        VkPipelineStageFlagBits 4096

-- | After previous commands have completed
--
--   bitpos = @13@
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT =
        VkPipelineStageFlagBits 8192

-- | Indicates host (CPU) is a source/sink of the dependency
--
--   bitpos = @14@
pattern VK_PIPELINE_STAGE_HOST_BIT :: VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_HOST_BIT = VkPipelineStageFlagBits 16384

-- | All stages of the graphics pipeline
--
--   bitpos = @15@
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT =
        VkPipelineStageFlagBits 32768

-- | All stages supported on the queue
--
--   bitpos = @16@
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT =
        VkPipelineStageFlagBits 65536

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSparseImageFormatFlagBits.html VkSparseImageFormatFlagBits registry at www.khronos.org>
newtype VkSparseImageFormatFlagBits = VkSparseImageFormatFlagBits Int32
                                        deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                                  Enum, Data, Generic)

instance Show VkSparseImageFormatFlagBits where
        showsPrec _ VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
          = showString "VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT"
        showsPrec _ VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
          = showString "VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT"
        showsPrec _ VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
          = showString "VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT"
        showsPrec p (VkSparseImageFormatFlagBits x)
          = showParen (p >= 11)
              (showString "VkSparseImageFormatFlagBits " . showsPrec 11 x)

instance Read VkSparseImageFormatFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT",
                   pure VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT),
                  ("VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT",
                   pure VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT),
                  ("VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT",
                   pure VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSparseImageFormatFlagBits") >>
                      (VkSparseImageFormatFlagBits <$> step readPrec)))

-- | Image uses a single mip tail region for all array layers
--
--   bitpos = @0@
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT ::
        VkSparseImageFormatFlagBits

pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT =
        VkSparseImageFormatFlagBits 1

-- | Image requires mip level dimensions to be an integer multiple of the sparse image block dimensions for non-tail mip levels.
--
--   bitpos = @1@
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT ::
        VkSparseImageFormatFlagBits

pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT =
        VkSparseImageFormatFlagBits 2

-- | Image uses a non-standard sparse image block dimensions
--
--   bitpos = @2@
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT ::
        VkSparseImageFormatFlagBits

pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT =
        VkSparseImageFormatFlagBits 4

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSampleCountFlagBits.html VkSampleCountFlagBits registry at www.khronos.org>
newtype VkSampleCountFlagBits = VkSampleCountFlagBits Int32
                                  deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                            Data, Generic)

instance Show VkSampleCountFlagBits where
        showsPrec _ VK_SAMPLE_COUNT_1_BIT
          = showString "VK_SAMPLE_COUNT_1_BIT"
        showsPrec _ VK_SAMPLE_COUNT_2_BIT
          = showString "VK_SAMPLE_COUNT_2_BIT"
        showsPrec _ VK_SAMPLE_COUNT_4_BIT
          = showString "VK_SAMPLE_COUNT_4_BIT"
        showsPrec _ VK_SAMPLE_COUNT_8_BIT
          = showString "VK_SAMPLE_COUNT_8_BIT"
        showsPrec _ VK_SAMPLE_COUNT_16_BIT
          = showString "VK_SAMPLE_COUNT_16_BIT"
        showsPrec _ VK_SAMPLE_COUNT_32_BIT
          = showString "VK_SAMPLE_COUNT_32_BIT"
        showsPrec _ VK_SAMPLE_COUNT_64_BIT
          = showString "VK_SAMPLE_COUNT_64_BIT"
        showsPrec p (VkSampleCountFlagBits x)
          = showParen (p >= 11)
              (showString "VkSampleCountFlagBits " . showsPrec 11 x)

instance Read VkSampleCountFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLE_COUNT_1_BIT", pure VK_SAMPLE_COUNT_1_BIT),
                  ("VK_SAMPLE_COUNT_2_BIT", pure VK_SAMPLE_COUNT_2_BIT),
                  ("VK_SAMPLE_COUNT_4_BIT", pure VK_SAMPLE_COUNT_4_BIT),
                  ("VK_SAMPLE_COUNT_8_BIT", pure VK_SAMPLE_COUNT_8_BIT),
                  ("VK_SAMPLE_COUNT_16_BIT", pure VK_SAMPLE_COUNT_16_BIT),
                  ("VK_SAMPLE_COUNT_32_BIT", pure VK_SAMPLE_COUNT_32_BIT),
                  ("VK_SAMPLE_COUNT_64_BIT", pure VK_SAMPLE_COUNT_64_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSampleCountFlagBits") >>
                      (VkSampleCountFlagBits <$> step readPrec)))

-- | Sample count 1 supported
--
--   bitpos = @0@
pattern VK_SAMPLE_COUNT_1_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountFlagBits 1

-- | Sample count 2 supported
--
--   bitpos = @1@
pattern VK_SAMPLE_COUNT_2_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountFlagBits 2

-- | Sample count 4 supported
--
--   bitpos = @2@
pattern VK_SAMPLE_COUNT_4_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountFlagBits 4

-- | Sample count 8 supported
--
--   bitpos = @3@
pattern VK_SAMPLE_COUNT_8_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountFlagBits 8

-- | Sample count 16 supported
--
--   bitpos = @4@
pattern VK_SAMPLE_COUNT_16_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountFlagBits 16

-- | Sample count 32 supported
--
--   bitpos = @5@
pattern VK_SAMPLE_COUNT_32_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountFlagBits 32

-- | Sample count 64 supported
--
--   bitpos = @6@
pattern VK_SAMPLE_COUNT_64_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountFlagBits 64

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentDescriptionFlagBits.html VkAttachmentDescriptionFlagBits registry at www.khronos.org>
newtype VkAttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits Int32
                                            deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                      Storable, Enum, Data, Generic)

instance Show VkAttachmentDescriptionFlagBits where
        showsPrec _ VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
          = showString "VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT"
        showsPrec p (VkAttachmentDescriptionFlagBits x)
          = showParen (p >= 11)
              (showString "VkAttachmentDescriptionFlagBits " . showsPrec 11 x)

instance Read VkAttachmentDescriptionFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT",
                   pure VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkAttachmentDescriptionFlagBits") >>
                      (VkAttachmentDescriptionFlagBits <$> step readPrec)))

-- | The attachment may alias physical memory of another attachment in the same render pass
--
--   bitpos = @0@
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT ::
        VkAttachmentDescriptionFlagBits

pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT =
        VkAttachmentDescriptionFlagBits 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorPoolCreateFlagBits.html VkDescriptorPoolCreateFlagBits registry at www.khronos.org>
newtype VkDescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits Int32
                                           deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                     Storable, Enum, Data, Generic)

instance Show VkDescriptorPoolCreateFlagBits where
        showsPrec _ VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
          = showString "VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT"
        showsPrec p (VkDescriptorPoolCreateFlagBits x)
          = showParen (p >= 11)
              (showString "VkDescriptorPoolCreateFlagBits " . showsPrec 11 x)

instance Read VkDescriptorPoolCreateFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT",
                   pure VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDescriptorPoolCreateFlagBits") >>
                      (VkDescriptorPoolCreateFlagBits <$> step readPrec)))

-- | Descriptor sets may be freed individually
--
--   bitpos = @0@
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT ::
        VkDescriptorPoolCreateFlagBits

pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT =
        VkDescriptorPoolCreateFlagBits 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDependencyFlagBits.html VkDependencyFlagBits registry at www.khronos.org>
newtype VkDependencyFlagBits = VkDependencyFlagBits Int32
                                 deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable, Enum,
                                           Data, Generic)

instance Show VkDependencyFlagBits where
        showsPrec _ VK_DEPENDENCY_BY_REGION_BIT
          = showString "VK_DEPENDENCY_BY_REGION_BIT"
        showsPrec p (VkDependencyFlagBits x)
          = showParen (p >= 11)
              (showString "VkDependencyFlagBits " . showsPrec 11 x)

instance Read VkDependencyFlagBits where
        readPrec
          = parens
              (choose
                 [("VK_DEPENDENCY_BY_REGION_BIT", pure VK_DEPENDENCY_BY_REGION_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDependencyFlagBits") >>
                      (VkDependencyFlagBits <$> step readPrec)))

-- | Dependency is per pixel region
--
--   bitpos = @0@
pattern VK_DEPENDENCY_BY_REGION_BIT :: VkDependencyFlagBits

pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyFlagBits 1

-- | Enums to track objects of various types
--
--   type = @enum@
--
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectType.html VkObjectType registry at www.khronos.org>
newtype VkObjectType = VkObjectType Int32
                         deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkObjectType where
        showsPrec _ VK_OBJECT_TYPE_UNKNOWN
          = showString "VK_OBJECT_TYPE_UNKNOWN"
        showsPrec _ VK_OBJECT_TYPE_INSTANCE
          = showString "VK_OBJECT_TYPE_INSTANCE"
        showsPrec _ VK_OBJECT_TYPE_PHYSICAL_DEVICE
          = showString "VK_OBJECT_TYPE_PHYSICAL_DEVICE"
        showsPrec _ VK_OBJECT_TYPE_DEVICE
          = showString "VK_OBJECT_TYPE_DEVICE"
        showsPrec _ VK_OBJECT_TYPE_QUEUE
          = showString "VK_OBJECT_TYPE_QUEUE"
        showsPrec _ VK_OBJECT_TYPE_SEMAPHORE
          = showString "VK_OBJECT_TYPE_SEMAPHORE"
        showsPrec _ VK_OBJECT_TYPE_COMMAND_BUFFER
          = showString "VK_OBJECT_TYPE_COMMAND_BUFFER"
        showsPrec _ VK_OBJECT_TYPE_FENCE
          = showString "VK_OBJECT_TYPE_FENCE"
        showsPrec _ VK_OBJECT_TYPE_DEVICE_MEMORY
          = showString "VK_OBJECT_TYPE_DEVICE_MEMORY"
        showsPrec _ VK_OBJECT_TYPE_BUFFER
          = showString "VK_OBJECT_TYPE_BUFFER"
        showsPrec _ VK_OBJECT_TYPE_IMAGE
          = showString "VK_OBJECT_TYPE_IMAGE"
        showsPrec _ VK_OBJECT_TYPE_EVENT
          = showString "VK_OBJECT_TYPE_EVENT"
        showsPrec _ VK_OBJECT_TYPE_QUERY_POOL
          = showString "VK_OBJECT_TYPE_QUERY_POOL"
        showsPrec _ VK_OBJECT_TYPE_BUFFER_VIEW
          = showString "VK_OBJECT_TYPE_BUFFER_VIEW"
        showsPrec _ VK_OBJECT_TYPE_IMAGE_VIEW
          = showString "VK_OBJECT_TYPE_IMAGE_VIEW"
        showsPrec _ VK_OBJECT_TYPE_SHADER_MODULE
          = showString "VK_OBJECT_TYPE_SHADER_MODULE"
        showsPrec _ VK_OBJECT_TYPE_PIPELINE_CACHE
          = showString "VK_OBJECT_TYPE_PIPELINE_CACHE"
        showsPrec _ VK_OBJECT_TYPE_PIPELINE_LAYOUT
          = showString "VK_OBJECT_TYPE_PIPELINE_LAYOUT"
        showsPrec _ VK_OBJECT_TYPE_RENDER_PASS
          = showString "VK_OBJECT_TYPE_RENDER_PASS"
        showsPrec _ VK_OBJECT_TYPE_PIPELINE
          = showString "VK_OBJECT_TYPE_PIPELINE"
        showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT
          = showString "VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
        showsPrec _ VK_OBJECT_TYPE_SAMPLER
          = showString "VK_OBJECT_TYPE_SAMPLER"
        showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_POOL
          = showString "VK_OBJECT_TYPE_DESCRIPTOR_POOL"
        showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_SET
          = showString "VK_OBJECT_TYPE_DESCRIPTOR_SET"
        showsPrec _ VK_OBJECT_TYPE_FRAMEBUFFER
          = showString "VK_OBJECT_TYPE_FRAMEBUFFER"
        showsPrec _ VK_OBJECT_TYPE_COMMAND_POOL
          = showString "VK_OBJECT_TYPE_COMMAND_POOL"
        showsPrec p (VkObjectType x)
          = showParen (p >= 11) (showString "VkObjectType " . showsPrec 11 x)

instance Read VkObjectType where
        readPrec
          = parens
              (choose
                 [("VK_OBJECT_TYPE_UNKNOWN", pure VK_OBJECT_TYPE_UNKNOWN),
                  ("VK_OBJECT_TYPE_INSTANCE", pure VK_OBJECT_TYPE_INSTANCE),
                  ("VK_OBJECT_TYPE_PHYSICAL_DEVICE",
                   pure VK_OBJECT_TYPE_PHYSICAL_DEVICE),
                  ("VK_OBJECT_TYPE_DEVICE", pure VK_OBJECT_TYPE_DEVICE),
                  ("VK_OBJECT_TYPE_QUEUE", pure VK_OBJECT_TYPE_QUEUE),
                  ("VK_OBJECT_TYPE_SEMAPHORE", pure VK_OBJECT_TYPE_SEMAPHORE),
                  ("VK_OBJECT_TYPE_COMMAND_BUFFER",
                   pure VK_OBJECT_TYPE_COMMAND_BUFFER),
                  ("VK_OBJECT_TYPE_FENCE", pure VK_OBJECT_TYPE_FENCE),
                  ("VK_OBJECT_TYPE_DEVICE_MEMORY",
                   pure VK_OBJECT_TYPE_DEVICE_MEMORY),
                  ("VK_OBJECT_TYPE_BUFFER", pure VK_OBJECT_TYPE_BUFFER),
                  ("VK_OBJECT_TYPE_IMAGE", pure VK_OBJECT_TYPE_IMAGE),
                  ("VK_OBJECT_TYPE_EVENT", pure VK_OBJECT_TYPE_EVENT),
                  ("VK_OBJECT_TYPE_QUERY_POOL", pure VK_OBJECT_TYPE_QUERY_POOL),
                  ("VK_OBJECT_TYPE_BUFFER_VIEW", pure VK_OBJECT_TYPE_BUFFER_VIEW),
                  ("VK_OBJECT_TYPE_IMAGE_VIEW", pure VK_OBJECT_TYPE_IMAGE_VIEW),
                  ("VK_OBJECT_TYPE_SHADER_MODULE",
                   pure VK_OBJECT_TYPE_SHADER_MODULE),
                  ("VK_OBJECT_TYPE_PIPELINE_CACHE",
                   pure VK_OBJECT_TYPE_PIPELINE_CACHE),
                  ("VK_OBJECT_TYPE_PIPELINE_LAYOUT",
                   pure VK_OBJECT_TYPE_PIPELINE_LAYOUT),
                  ("VK_OBJECT_TYPE_RENDER_PASS", pure VK_OBJECT_TYPE_RENDER_PASS),
                  ("VK_OBJECT_TYPE_PIPELINE", pure VK_OBJECT_TYPE_PIPELINE),
                  ("VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT",
                   pure VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT),
                  ("VK_OBJECT_TYPE_SAMPLER", pure VK_OBJECT_TYPE_SAMPLER),
                  ("VK_OBJECT_TYPE_DESCRIPTOR_POOL",
                   pure VK_OBJECT_TYPE_DESCRIPTOR_POOL),
                  ("VK_OBJECT_TYPE_DESCRIPTOR_SET",
                   pure VK_OBJECT_TYPE_DESCRIPTOR_SET),
                  ("VK_OBJECT_TYPE_FRAMEBUFFER", pure VK_OBJECT_TYPE_FRAMEBUFFER),
                  ("VK_OBJECT_TYPE_COMMAND_POOL", pure VK_OBJECT_TYPE_COMMAND_POOL)]
                 +++
                 prec 10
                   (expectP (Ident "VkObjectType") >>
                      (VkObjectType <$> step readPrec)))

pattern VK_OBJECT_TYPE_UNKNOWN :: VkObjectType

pattern VK_OBJECT_TYPE_UNKNOWN = VkObjectType 0

-- | VkInstance
pattern VK_OBJECT_TYPE_INSTANCE :: VkObjectType

pattern VK_OBJECT_TYPE_INSTANCE = VkObjectType 1

-- | VkPhysicalDevice
pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE :: VkObjectType

pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE = VkObjectType 2

-- | VkDevice
pattern VK_OBJECT_TYPE_DEVICE :: VkObjectType

pattern VK_OBJECT_TYPE_DEVICE = VkObjectType 3

-- | VkQueue
pattern VK_OBJECT_TYPE_QUEUE :: VkObjectType

pattern VK_OBJECT_TYPE_QUEUE = VkObjectType 4

-- | VkSemaphore
pattern VK_OBJECT_TYPE_SEMAPHORE :: VkObjectType

pattern VK_OBJECT_TYPE_SEMAPHORE = VkObjectType 5

-- | VkCommandBuffer
pattern VK_OBJECT_TYPE_COMMAND_BUFFER :: VkObjectType

pattern VK_OBJECT_TYPE_COMMAND_BUFFER = VkObjectType 6

-- | VkFence
pattern VK_OBJECT_TYPE_FENCE :: VkObjectType

pattern VK_OBJECT_TYPE_FENCE = VkObjectType 7

-- | VkDeviceMemory
pattern VK_OBJECT_TYPE_DEVICE_MEMORY :: VkObjectType

pattern VK_OBJECT_TYPE_DEVICE_MEMORY = VkObjectType 8

-- | VkBuffer
pattern VK_OBJECT_TYPE_BUFFER :: VkObjectType

pattern VK_OBJECT_TYPE_BUFFER = VkObjectType 9

-- | VkImage
pattern VK_OBJECT_TYPE_IMAGE :: VkObjectType

pattern VK_OBJECT_TYPE_IMAGE = VkObjectType 10

-- | VkEvent
pattern VK_OBJECT_TYPE_EVENT :: VkObjectType

pattern VK_OBJECT_TYPE_EVENT = VkObjectType 11

-- | VkQueryPool
pattern VK_OBJECT_TYPE_QUERY_POOL :: VkObjectType

pattern VK_OBJECT_TYPE_QUERY_POOL = VkObjectType 12

-- | VkBufferView
pattern VK_OBJECT_TYPE_BUFFER_VIEW :: VkObjectType

pattern VK_OBJECT_TYPE_BUFFER_VIEW = VkObjectType 13

-- | VkImageView
pattern VK_OBJECT_TYPE_IMAGE_VIEW :: VkObjectType

pattern VK_OBJECT_TYPE_IMAGE_VIEW = VkObjectType 14

-- | VkShaderModule
pattern VK_OBJECT_TYPE_SHADER_MODULE :: VkObjectType

pattern VK_OBJECT_TYPE_SHADER_MODULE = VkObjectType 15

-- | VkPipelineCache
pattern VK_OBJECT_TYPE_PIPELINE_CACHE :: VkObjectType

pattern VK_OBJECT_TYPE_PIPELINE_CACHE = VkObjectType 16

-- | VkPipelineLayout
pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT :: VkObjectType

pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT = VkObjectType 17

-- | VkRenderPass
pattern VK_OBJECT_TYPE_RENDER_PASS :: VkObjectType

pattern VK_OBJECT_TYPE_RENDER_PASS = VkObjectType 18

-- | VkPipeline
pattern VK_OBJECT_TYPE_PIPELINE :: VkObjectType

pattern VK_OBJECT_TYPE_PIPELINE = VkObjectType 19

-- | VkDescriptorSetLayout
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT :: VkObjectType

pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT = VkObjectType 20

-- | VkSampler
pattern VK_OBJECT_TYPE_SAMPLER :: VkObjectType

pattern VK_OBJECT_TYPE_SAMPLER = VkObjectType 21

-- | VkDescriptorPool
pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL :: VkObjectType

pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL = VkObjectType 22

-- | VkDescriptorSet
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET :: VkObjectType

pattern VK_OBJECT_TYPE_DESCRIPTOR_SET = VkObjectType 23

-- | VkFramebuffer
pattern VK_OBJECT_TYPE_FRAMEBUFFER :: VkObjectType

pattern VK_OBJECT_TYPE_FRAMEBUFFER = VkObjectType 24

-- | VkCommandPool
pattern VK_OBJECT_TYPE_COMMAND_POOL :: VkObjectType

pattern VK_OBJECT_TYPE_COMMAND_POOL = VkObjectType 25

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkIndirectCommandsLayoutUsageFlagBitsNVX.html VkIndirectCommandsLayoutUsageFlagBitsNVX registry at www.khronos.org>
newtype VkIndirectCommandsLayoutUsageFlagBitsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX Int32
                                                     deriving (Bits, FiniteBits, Eq, Ord, Num,
                                                               Bounded, Storable, Enum, Data,
                                                               Generic)

instance Show VkIndirectCommandsLayoutUsageFlagBitsNVX where
        showsPrec _
          VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
          = showString
              "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX"
        showsPrec _
          VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
          = showString
              "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX"
        showsPrec _
          VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
          = showString
              "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX"
        showsPrec _
          VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
          = showString
              "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX"
        showsPrec p (VkIndirectCommandsLayoutUsageFlagBitsNVX x)
          = showParen (p >= 11)
              (showString "VkIndirectCommandsLayoutUsageFlagBitsNVX " .
                 showsPrec 11 x)

instance Read VkIndirectCommandsLayoutUsageFlagBitsNVX where
        readPrec
          = parens
              (choose
                 [("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX",
                   pure
                     VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX),
                  ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX",
                   pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX),
                  ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX",
                   pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX),
                  ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX",
                   pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX)]
                 +++
                 prec 10
                   (expectP (Ident "VkIndirectCommandsLayoutUsageFlagBitsNVX") >>
                      (VkIndirectCommandsLayoutUsageFlagBitsNVX <$> step readPrec)))

-- | bitpos = @0@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
        :: VkIndirectCommandsLayoutUsageFlagBitsNVX

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
        = VkIndirectCommandsLayoutUsageFlagBitsNVX 1

-- | bitpos = @1@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
        :: VkIndirectCommandsLayoutUsageFlagBitsNVX

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
        = VkIndirectCommandsLayoutUsageFlagBitsNVX 2

-- | bitpos = @2@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
        :: VkIndirectCommandsLayoutUsageFlagBitsNVX

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
        = VkIndirectCommandsLayoutUsageFlagBitsNVX 4

-- | bitpos = @3@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
        :: VkIndirectCommandsLayoutUsageFlagBitsNVX

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
        = VkIndirectCommandsLayoutUsageFlagBitsNVX 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkIndirectCommandsTokenTypeNVX.html VkIndirectCommandsTokenTypeNVX registry at www.khronos.org>
newtype VkIndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX Int32
                                           deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                     Generic)

instance Show VkIndirectCommandsTokenTypeNVX where
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX"
        showsPrec p (VkIndirectCommandsTokenTypeNVX x)
          = showParen (p >= 11)
              (showString "VkIndirectCommandsTokenTypeNVX " . showsPrec 11 x)

instance Read VkIndirectCommandsTokenTypeNVX where
        readPrec
          = parens
              (choose
                 [("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX)]
                 +++
                 prec 10
                   (expectP (Ident "VkIndirectCommandsTokenTypeNVX") >>
                      (VkIndirectCommandsTokenTypeNVX <$> step readPrec)))

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX =
        VkIndirectCommandsTokenTypeNVX 0

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX =
        VkIndirectCommandsTokenTypeNVX 1

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX =
        VkIndirectCommandsTokenTypeNVX 2

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX =
        VkIndirectCommandsTokenTypeNVX 3

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX =
        VkIndirectCommandsTokenTypeNVX 4

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX =
        VkIndirectCommandsTokenTypeNVX 5

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX =
        VkIndirectCommandsTokenTypeNVX 6

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX =
        VkIndirectCommandsTokenTypeNVX 7

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectEntryUsageFlagBitsNVX.html VkObjectEntryUsageFlagBitsNVX registry at www.khronos.org>
newtype VkObjectEntryUsageFlagBitsNVX = VkObjectEntryUsageFlagBitsNVX Int32
                                          deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                    Storable, Enum, Data, Generic)

instance Show VkObjectEntryUsageFlagBitsNVX where
        showsPrec _ VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
          = showString "VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX"
        showsPrec _ VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
          = showString "VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX"
        showsPrec p (VkObjectEntryUsageFlagBitsNVX x)
          = showParen (p >= 11)
              (showString "VkObjectEntryUsageFlagBitsNVX " . showsPrec 11 x)

instance Read VkObjectEntryUsageFlagBitsNVX where
        readPrec
          = parens
              (choose
                 [("VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX",
                   pure VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX),
                  ("VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX",
                   pure VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX)]
                 +++
                 prec 10
                   (expectP (Ident "VkObjectEntryUsageFlagBitsNVX") >>
                      (VkObjectEntryUsageFlagBitsNVX <$> step readPrec)))

-- | bitpos = @0@
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX ::
        VkObjectEntryUsageFlagBitsNVX

pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX =
        VkObjectEntryUsageFlagBitsNVX 1

-- | bitpos = @1@
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX ::
        VkObjectEntryUsageFlagBitsNVX

pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX =
        VkObjectEntryUsageFlagBitsNVX 2

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectEntryTypeNVX.html VkObjectEntryTypeNVX registry at www.khronos.org>
newtype VkObjectEntryTypeNVX = VkObjectEntryTypeNVX Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkObjectEntryTypeNVX where
        showsPrec _ VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX"
        showsPrec p (VkObjectEntryTypeNVX x)
          = showParen (p >= 11)
              (showString "VkObjectEntryTypeNVX " . showsPrec 11 x)

instance Read VkObjectEntryTypeNVX where
        readPrec
          = parens
              (choose
                 [("VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX)]
                 +++
                 prec 10
                   (expectP (Ident "VkObjectEntryTypeNVX") >>
                      (VkObjectEntryTypeNVX <$> step readPrec)))

pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX =
        VkObjectEntryTypeNVX 0

pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX :: VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = VkObjectEntryTypeNVX 1

pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX =
        VkObjectEntryTypeNVX 2

pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX =
        VkObjectEntryTypeNVX 3

pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX =
        VkObjectEntryTypeNVX 4

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorUpdateTemplateTypeKHR.html VkDescriptorUpdateTemplateTypeKHR registry at www.khronos.org>
newtype VkDescriptorUpdateTemplateTypeKHR = VkDescriptorUpdateTemplateTypeKHR Int32
                                              deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                        Generic)

instance Show VkDescriptorUpdateTemplateTypeKHR where
        showsPrec _ VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR
          = showString
              "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR"
        showsPrec _ VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
          = showString
              "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR"
        showsPrec p (VkDescriptorUpdateTemplateTypeKHR x)
          = showParen (p >= 11)
              (showString "VkDescriptorUpdateTemplateTypeKHR " . showsPrec 11 x)

instance Read VkDescriptorUpdateTemplateTypeKHR where
        readPrec
          = parens
              (choose
                 [("VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR",
                   pure VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR),
                  ("VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR",
                   pure VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkDescriptorUpdateTemplateTypeKHR") >>
                      (VkDescriptorUpdateTemplateTypeKHR <$> step readPrec)))

-- | Create descriptor update template for descriptor set updates
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR ::
        VkDescriptorUpdateTemplateTypeKHR

pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR =
        VkDescriptorUpdateTemplateTypeKHR 0

-- | Create descriptor update template for pushed descriptor updates
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR ::
        VkDescriptorUpdateTemplateTypeKHR

pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR =
        VkDescriptorUpdateTemplateTypeKHR 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkViewportCoordinateSwizzleNV.html VkViewportCoordinateSwizzleNV registry at www.khronos.org>
newtype VkViewportCoordinateSwizzleNV = VkViewportCoordinateSwizzleNV Int32
                                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                    Generic)

instance Show VkViewportCoordinateSwizzleNV where
        showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV
          = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV"
        showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV
          = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV"
        showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV
          = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV"
        showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV
          = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV"
        showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV
          = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV"
        showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV
          = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV"
        showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV
          = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV"
        showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV
          = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV"
        showsPrec p (VkViewportCoordinateSwizzleNV x)
          = showParen (p >= 11)
              (showString "VkViewportCoordinateSwizzleNV " . showsPrec 11 x)

instance Read VkViewportCoordinateSwizzleNV where
        readPrec
          = parens
              (choose
                 [("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV",
                   pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV),
                  ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV",
                   pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV),
                  ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV",
                   pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV),
                  ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV",
                   pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV),
                  ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV",
                   pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV),
                  ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV",
                   pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV),
                  ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV",
                   pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV),
                  ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV",
                   pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV)]
                 +++
                 prec 10
                   (expectP (Ident "VkViewportCoordinateSwizzleNV") >>
                      (VkViewportCoordinateSwizzleNV <$> step readPrec)))

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV =
        VkViewportCoordinateSwizzleNV 0

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV =
        VkViewportCoordinateSwizzleNV 1

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV =
        VkViewportCoordinateSwizzleNV 2

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV =
        VkViewportCoordinateSwizzleNV 3

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV =
        VkViewportCoordinateSwizzleNV 4

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV =
        VkViewportCoordinateSwizzleNV 5

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV =
        VkViewportCoordinateSwizzleNV 6

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV =
        VkViewportCoordinateSwizzleNV 7

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDiscardRectangleModeEXT.html VkDiscardRectangleModeEXT registry at www.khronos.org>
newtype VkDiscardRectangleModeEXT = VkDiscardRectangleModeEXT Int32
                                      deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                Generic)

instance Show VkDiscardRectangleModeEXT where
        showsPrec _ VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT
          = showString "VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT"
        showsPrec _ VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT
          = showString "VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT"
        showsPrec p (VkDiscardRectangleModeEXT x)
          = showParen (p >= 11)
              (showString "VkDiscardRectangleModeEXT " . showsPrec 11 x)

instance Read VkDiscardRectangleModeEXT where
        readPrec
          = parens
              (choose
                 [("VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT",
                   pure VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT),
                  ("VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT",
                   pure VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDiscardRectangleModeEXT") >>
                      (VkDiscardRectangleModeEXT <$> step readPrec)))

pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT ::
        VkDiscardRectangleModeEXT

pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT =
        VkDiscardRectangleModeEXT 0

pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT ::
        VkDiscardRectangleModeEXT

pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT =
        VkDiscardRectangleModeEXT 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubpassDescriptionFlagBits.html VkSubpassDescriptionFlagBits registry at www.khronos.org>
newtype VkSubpassDescriptionFlagBits = VkSubpassDescriptionFlagBits Int32
                                         deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                   Storable, Enum, Data, Generic)

instance Show VkSubpassDescriptionFlagBits where
        showsPrec p (VkSubpassDescriptionFlagBits x)
          = showParen (p >= 11)
              (showString "VkSubpassDescriptionFlagBits " . showsPrec 11 x)

instance Read VkSubpassDescriptionFlagBits where
        readPrec
          = parens
              (choose [] +++
                 prec 10
                   (expectP (Ident "VkSubpassDescriptionFlagBits") >>
                      (VkSubpassDescriptionFlagBits <$> step readPrec)))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPointClippingBehaviorKHR.html VkPointClippingBehaviorKHR registry at www.khronos.org>
newtype VkPointClippingBehaviorKHR = VkPointClippingBehaviorKHR Int32
                                       deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                 Generic)

instance Show VkPointClippingBehaviorKHR where
        showsPrec _ VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR
          = showString "VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR"
        showsPrec _ VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR
          = showString "VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR"
        showsPrec p (VkPointClippingBehaviorKHR x)
          = showParen (p >= 11)
              (showString "VkPointClippingBehaviorKHR " . showsPrec 11 x)

instance Read VkPointClippingBehaviorKHR where
        readPrec
          = parens
              (choose
                 [("VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR",
                   pure VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR),
                  ("VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR",
                   pure VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkPointClippingBehaviorKHR") >>
                      (VkPointClippingBehaviorKHR <$> step readPrec)))

pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR ::
        VkPointClippingBehaviorKHR

pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR =
        VkPointClippingBehaviorKHR 0

pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR ::
        VkPointClippingBehaviorKHR

pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR =
        VkPointClippingBehaviorKHR 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCoverageModulationModeNV.html VkCoverageModulationModeNV registry at www.khronos.org>
newtype VkCoverageModulationModeNV = VkCoverageModulationModeNV Int32
                                       deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                 Generic)

instance Show VkCoverageModulationModeNV where
        showsPrec _ VK_COVERAGE_MODULATION_MODE_NONE_NV
          = showString "VK_COVERAGE_MODULATION_MODE_NONE_NV"
        showsPrec _ VK_COVERAGE_MODULATION_MODE_RGB_NV
          = showString "VK_COVERAGE_MODULATION_MODE_RGB_NV"
        showsPrec _ VK_COVERAGE_MODULATION_MODE_ALPHA_NV
          = showString "VK_COVERAGE_MODULATION_MODE_ALPHA_NV"
        showsPrec _ VK_COVERAGE_MODULATION_MODE_RGBA_NV
          = showString "VK_COVERAGE_MODULATION_MODE_RGBA_NV"
        showsPrec p (VkCoverageModulationModeNV x)
          = showParen (p >= 11)
              (showString "VkCoverageModulationModeNV " . showsPrec 11 x)

instance Read VkCoverageModulationModeNV where
        readPrec
          = parens
              (choose
                 [("VK_COVERAGE_MODULATION_MODE_NONE_NV",
                   pure VK_COVERAGE_MODULATION_MODE_NONE_NV),
                  ("VK_COVERAGE_MODULATION_MODE_RGB_NV",
                   pure VK_COVERAGE_MODULATION_MODE_RGB_NV),
                  ("VK_COVERAGE_MODULATION_MODE_ALPHA_NV",
                   pure VK_COVERAGE_MODULATION_MODE_ALPHA_NV),
                  ("VK_COVERAGE_MODULATION_MODE_RGBA_NV",
                   pure VK_COVERAGE_MODULATION_MODE_RGBA_NV)]
                 +++
                 prec 10
                   (expectP (Ident "VkCoverageModulationModeNV") >>
                      (VkCoverageModulationModeNV <$> step readPrec)))

pattern VK_COVERAGE_MODULATION_MODE_NONE_NV ::
        VkCoverageModulationModeNV

pattern VK_COVERAGE_MODULATION_MODE_NONE_NV =
        VkCoverageModulationModeNV 0

pattern VK_COVERAGE_MODULATION_MODE_RGB_NV ::
        VkCoverageModulationModeNV

pattern VK_COVERAGE_MODULATION_MODE_RGB_NV =
        VkCoverageModulationModeNV 1

pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV ::
        VkCoverageModulationModeNV

pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV =
        VkCoverageModulationModeNV 2

pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV ::
        VkCoverageModulationModeNV

pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV =
        VkCoverageModulationModeNV 3

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkValidationCacheHeaderVersionEXT.html VkValidationCacheHeaderVersionEXT registry at www.khronos.org>
newtype VkValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT Int32
                                              deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                        Generic)

instance Show VkValidationCacheHeaderVersionEXT where
        showsPrec _ VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
          = showString "VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"
        showsPrec p (VkValidationCacheHeaderVersionEXT x)
          = showParen (p >= 11)
              (showString "VkValidationCacheHeaderVersionEXT " . showsPrec 11 x)

instance Read VkValidationCacheHeaderVersionEXT where
        readPrec
          = parens
              (choose
                 [("VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT",
                   pure VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkValidationCacheHeaderVersionEXT") >>
                      (VkValidationCacheHeaderVersionEXT <$> step readPrec)))

pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT ::
        VkValidationCacheHeaderVersionEXT

pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT =
        VkValidationCacheHeaderVersionEXT 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkShaderInfoTypeAMD.html VkShaderInfoTypeAMD registry at www.khronos.org>
newtype VkShaderInfoTypeAMD = VkShaderInfoTypeAMD Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkShaderInfoTypeAMD where
        showsPrec _ VK_SHADER_INFO_TYPE_STATISTICS_AMD
          = showString "VK_SHADER_INFO_TYPE_STATISTICS_AMD"
        showsPrec _ VK_SHADER_INFO_TYPE_BINARY_AMD
          = showString "VK_SHADER_INFO_TYPE_BINARY_AMD"
        showsPrec _ VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD
          = showString "VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD"
        showsPrec p (VkShaderInfoTypeAMD x)
          = showParen (p >= 11)
              (showString "VkShaderInfoTypeAMD " . showsPrec 11 x)

instance Read VkShaderInfoTypeAMD where
        readPrec
          = parens
              (choose
                 [("VK_SHADER_INFO_TYPE_STATISTICS_AMD",
                   pure VK_SHADER_INFO_TYPE_STATISTICS_AMD),
                  ("VK_SHADER_INFO_TYPE_BINARY_AMD",
                   pure VK_SHADER_INFO_TYPE_BINARY_AMD),
                  ("VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD",
                   pure VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD)]
                 +++
                 prec 10
                   (expectP (Ident "VkShaderInfoTypeAMD") >>
                      (VkShaderInfoTypeAMD <$> step readPrec)))

pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD = VkShaderInfoTypeAMD 0

pattern VK_SHADER_INFO_TYPE_BINARY_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_BINARY_AMD = VkShaderInfoTypeAMD 1

pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD = VkShaderInfoTypeAMD 2

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueueGlobalPriorityEXT.html VkQueueGlobalPriorityEXT registry at www.khronos.org>
newtype VkQueueGlobalPriorityEXT = VkQueueGlobalPriorityEXT Int32
                                     deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkQueueGlobalPriorityEXT where
        showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT
          = showString "VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT"
        showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT
          = showString "VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT"
        showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT
          = showString "VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT"
        showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT
          = showString "VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT"
        showsPrec p (VkQueueGlobalPriorityEXT x)
          = showParen (p >= 11)
              (showString "VkQueueGlobalPriorityEXT " . showsPrec 11 x)

instance Read VkQueueGlobalPriorityEXT where
        readPrec
          = parens
              (choose
                 [("VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT",
                   pure VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT),
                  ("VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT",
                   pure VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT),
                  ("VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT",
                   pure VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT),
                  ("VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT",
                   pure VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueueGlobalPriorityEXT") >>
                      (VkQueueGlobalPriorityEXT <$> step readPrec)))

pattern VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT ::
        VkQueueGlobalPriorityEXT

pattern VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT =
        VkQueueGlobalPriorityEXT 128

pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT ::
        VkQueueGlobalPriorityEXT

pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT =
        VkQueueGlobalPriorityEXT 256

pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT ::
        VkQueueGlobalPriorityEXT

pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT =
        VkQueueGlobalPriorityEXT 512

pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT ::
        VkQueueGlobalPriorityEXT

pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT =
        VkQueueGlobalPriorityEXT 1024

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkConservativeRasterizationModeEXT.html VkConservativeRasterizationModeEXT registry at www.khronos.org>
newtype VkConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT Int32
                                               deriving (Eq, Ord, Num, Bounded, Storable, Enum,
                                                         Data, Generic)

instance Show VkConservativeRasterizationModeEXT where
        showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
          = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT"
        showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
          = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT"
        showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
          = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT"
        showsPrec p (VkConservativeRasterizationModeEXT x)
          = showParen (p >= 11)
              (showString "VkConservativeRasterizationModeEXT " . showsPrec 11 x)

instance Read VkConservativeRasterizationModeEXT where
        readPrec
          = parens
              (choose
                 [("VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT",
                   pure VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT),
                  ("VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT",
                   pure VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT),
                  ("VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT",
                   pure VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkConservativeRasterizationModeEXT") >>
                      (VkConservativeRasterizationModeEXT <$> step readPrec)))

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT ::
        VkConservativeRasterizationModeEXT

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT =
        VkConservativeRasterizationModeEXT 0

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT ::
        VkConservativeRasterizationModeEXT

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT =
        VkConservativeRasterizationModeEXT 1

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT ::
        VkConservativeRasterizationModeEXT

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT =
        VkConservativeRasterizationModeEXT 2

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkColorSpaceKHR.html VkColorSpaceKHR registry at www.khronos.org>
newtype VkColorSpaceKHR = VkColorSpaceKHR Int32
                            deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkColorSpaceKHR where
        showsPrec _ VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
          = showString "VK_COLOR_SPACE_SRGB_NONLINEAR_KHR"
        showsPrec p (VkColorSpaceKHR x)
          = showParen (p >= 11)
              (showString "VkColorSpaceKHR " . showsPrec 11 x)

instance Read VkColorSpaceKHR where
        readPrec
          = parens
              (choose
                 [("VK_COLOR_SPACE_SRGB_NONLINEAR_KHR",
                   pure VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkColorSpaceKHR") >>
                      (VkColorSpaceKHR <$> step readPrec)))

pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCompositeAlphaFlagBitsKHR.html VkCompositeAlphaFlagBitsKHR registry at www.khronos.org>
newtype VkCompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR Int32
                                        deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                                  Enum, Data, Generic)

instance Show VkCompositeAlphaFlagBitsKHR where
        showsPrec _ VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
          = showString "VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
        showsPrec _ VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
          = showString "VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
        showsPrec _ VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
          = showString "VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
        showsPrec _ VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
          = showString "VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR"
        showsPrec p (VkCompositeAlphaFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkCompositeAlphaFlagBitsKHR " . showsPrec 11 x)

instance Read VkCompositeAlphaFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR",
                   pure VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR),
                  ("VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR",
                   pure VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR),
                  ("VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR",
                   pure VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR),
                  ("VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR",
                   pure VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkCompositeAlphaFlagBitsKHR") >>
                      (VkCompositeAlphaFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR ::
        VkCompositeAlphaFlagBitsKHR

pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR =
        VkCompositeAlphaFlagBitsKHR 1

-- | bitpos = @1@
pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR ::
        VkCompositeAlphaFlagBitsKHR

pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR =
        VkCompositeAlphaFlagBitsKHR 2

-- | bitpos = @2@
pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR ::
        VkCompositeAlphaFlagBitsKHR

pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR =
        VkCompositeAlphaFlagBitsKHR 4

-- | bitpos = @3@
pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR ::
        VkCompositeAlphaFlagBitsKHR

pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR =
        VkCompositeAlphaFlagBitsKHR 8

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayPlaneAlphaFlagBitsKHR.html VkDisplayPlaneAlphaFlagBitsKHR registry at www.khronos.org>
newtype VkDisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR Int32
                                           deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                     Storable, Enum, Data, Generic)

instance Show VkDisplayPlaneAlphaFlagBitsKHR where
        showsPrec _ VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
          = showString "VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR"
        showsPrec _ VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
          = showString "VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR"
        showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
          = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR"
        showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
          = showString
              "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR"
        showsPrec p (VkDisplayPlaneAlphaFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkDisplayPlaneAlphaFlagBitsKHR " . showsPrec 11 x)

instance Read VkDisplayPlaneAlphaFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR",
                   pure VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR),
                  ("VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR",
                   pure VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR),
                  ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR",
                   pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR),
                  ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR",
                   pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkDisplayPlaneAlphaFlagBitsKHR") >>
                      (VkDisplayPlaneAlphaFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR ::
        VkDisplayPlaneAlphaFlagBitsKHR

pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR =
        VkDisplayPlaneAlphaFlagBitsKHR 1

-- | bitpos = @1@
pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR ::
        VkDisplayPlaneAlphaFlagBitsKHR

pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR =
        VkDisplayPlaneAlphaFlagBitsKHR 2

-- | bitpos = @2@
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR ::
        VkDisplayPlaneAlphaFlagBitsKHR

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR =
        VkDisplayPlaneAlphaFlagBitsKHR 4

-- | bitpos = @3@
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR ::
        VkDisplayPlaneAlphaFlagBitsKHR

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR =
        VkDisplayPlaneAlphaFlagBitsKHR 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPresentModeKHR.html VkPresentModeKHR registry at www.khronos.org>
newtype VkPresentModeKHR = VkPresentModeKHR Int32
                             deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkPresentModeKHR where
        showsPrec _ VK_PRESENT_MODE_IMMEDIATE_KHR
          = showString "VK_PRESENT_MODE_IMMEDIATE_KHR"
        showsPrec _ VK_PRESENT_MODE_MAILBOX_KHR
          = showString "VK_PRESENT_MODE_MAILBOX_KHR"
        showsPrec _ VK_PRESENT_MODE_FIFO_KHR
          = showString "VK_PRESENT_MODE_FIFO_KHR"
        showsPrec _ VK_PRESENT_MODE_FIFO_RELAXED_KHR
          = showString "VK_PRESENT_MODE_FIFO_RELAXED_KHR"
        showsPrec p (VkPresentModeKHR x)
          = showParen (p >= 11)
              (showString "VkPresentModeKHR " . showsPrec 11 x)

instance Read VkPresentModeKHR where
        readPrec
          = parens
              (choose
                 [("VK_PRESENT_MODE_IMMEDIATE_KHR",
                   pure VK_PRESENT_MODE_IMMEDIATE_KHR),
                  ("VK_PRESENT_MODE_MAILBOX_KHR", pure VK_PRESENT_MODE_MAILBOX_KHR),
                  ("VK_PRESENT_MODE_FIFO_KHR", pure VK_PRESENT_MODE_FIFO_KHR),
                  ("VK_PRESENT_MODE_FIFO_RELAXED_KHR",
                   pure VK_PRESENT_MODE_FIFO_RELAXED_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkPresentModeKHR") >>
                      (VkPresentModeKHR <$> step readPrec)))

pattern VK_PRESENT_MODE_IMMEDIATE_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_IMMEDIATE_KHR = VkPresentModeKHR 0

pattern VK_PRESENT_MODE_MAILBOX_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_MAILBOX_KHR = VkPresentModeKHR 1

pattern VK_PRESENT_MODE_FIFO_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_FIFO_KHR = VkPresentModeKHR 2

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = VkPresentModeKHR 3

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSurfaceTransformFlagBitsKHR.html VkSurfaceTransformFlagBitsKHR registry at www.khronos.org>
newtype VkSurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR Int32
                                          deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                    Storable, Enum, Data, Generic)

instance Show VkSurfaceTransformFlagBitsKHR where
        showsPrec _ VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR"
        showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR"
        showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR"
        showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR"
        showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR"
        showsPrec _
          VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
          = showString
              "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR"
        showsPrec _
          VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
          = showString
              "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR"
        showsPrec _
          VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
          = showString
              "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR"
        showsPrec _ VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR"
        showsPrec p (VkSurfaceTransformFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkSurfaceTransformFlagBitsKHR " . showsPrec 11 x)

instance Read VkSurfaceTransformFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkSurfaceTransformFlagBitsKHR") >>
                      (VkSurfaceTransformFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 1

-- | bitpos = @1@
pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 2

-- | bitpos = @2@
pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 4

-- | bitpos = @3@
pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 8

-- | bitpos = @4@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 16

-- | bitpos = @5@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 32

-- | bitpos = @6@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
        :: VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 64

-- | bitpos = @7@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
        :: VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 128

-- | bitpos = @8@
pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 256

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDebugReportFlagBitsEXT.html VkDebugReportFlagBitsEXT registry at www.khronos.org>
newtype VkDebugReportFlagBitsEXT = VkDebugReportFlagBitsEXT Int32
                                     deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                               Enum, Data, Generic)

instance Show VkDebugReportFlagBitsEXT where
        showsPrec _ VK_DEBUG_REPORT_INFORMATION_BIT_EXT
          = showString "VK_DEBUG_REPORT_INFORMATION_BIT_EXT"
        showsPrec _ VK_DEBUG_REPORT_WARNING_BIT_EXT
          = showString "VK_DEBUG_REPORT_WARNING_BIT_EXT"
        showsPrec _ VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
          = showString "VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT"
        showsPrec _ VK_DEBUG_REPORT_ERROR_BIT_EXT
          = showString "VK_DEBUG_REPORT_ERROR_BIT_EXT"
        showsPrec _ VK_DEBUG_REPORT_DEBUG_BIT_EXT
          = showString "VK_DEBUG_REPORT_DEBUG_BIT_EXT"
        showsPrec p (VkDebugReportFlagBitsEXT x)
          = showParen (p >= 11)
              (showString "VkDebugReportFlagBitsEXT " . showsPrec 11 x)

instance Read VkDebugReportFlagBitsEXT where
        readPrec
          = parens
              (choose
                 [("VK_DEBUG_REPORT_INFORMATION_BIT_EXT",
                   pure VK_DEBUG_REPORT_INFORMATION_BIT_EXT),
                  ("VK_DEBUG_REPORT_WARNING_BIT_EXT",
                   pure VK_DEBUG_REPORT_WARNING_BIT_EXT),
                  ("VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT",
                   pure VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT),
                  ("VK_DEBUG_REPORT_ERROR_BIT_EXT",
                   pure VK_DEBUG_REPORT_ERROR_BIT_EXT),
                  ("VK_DEBUG_REPORT_DEBUG_BIT_EXT",
                   pure VK_DEBUG_REPORT_DEBUG_BIT_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDebugReportFlagBitsEXT") >>
                      (VkDebugReportFlagBitsEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT ::
        VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT =
        VkDebugReportFlagBitsEXT 1

-- | bitpos = @1@
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT :: VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_WARNING_BIT_EXT =
        VkDebugReportFlagBitsEXT 2

-- | bitpos = @2@
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT ::
        VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT =
        VkDebugReportFlagBitsEXT 4

-- | bitpos = @3@
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT :: VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_ERROR_BIT_EXT = VkDebugReportFlagBitsEXT 8

-- | bitpos = @4@
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT :: VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT = VkDebugReportFlagBitsEXT 16

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDebugReportObjectTypeEXT.html VkDebugReportObjectTypeEXT registry at www.khronos.org>
newtype VkDebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT Int32
                                       deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                 Generic)

instance Show VkDebugReportObjectTypeEXT where
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT
          = showString
              "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT"
        showsPrec _
          VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
          = showString
              "VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT"
        showsPrec _
          VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT
          = showString
              "VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT"
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT"
        showsPrec p (VkDebugReportObjectTypeEXT x)
          = showParen (p >= 11)
              (showString "VkDebugReportObjectTypeEXT " . showsPrec 11 x)

instance Read VkDebugReportObjectTypeEXT where
        readPrec
          = parens
              (choose
                 [("VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT),
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDebugReportObjectTypeEXT") >>
                      (VkDebugReportObjectTypeEXT <$> step readPrec)))

pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT =
        VkDebugReportObjectTypeEXT 0

pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT =
        VkDebugReportObjectTypeEXT 1

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT =
        VkDebugReportObjectTypeEXT 2

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT =
        VkDebugReportObjectTypeEXT 3

pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT =
        VkDebugReportObjectTypeEXT 4

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT =
        VkDebugReportObjectTypeEXT 5

pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT =
        VkDebugReportObjectTypeEXT 6

pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT =
        VkDebugReportObjectTypeEXT 7

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT =
        VkDebugReportObjectTypeEXT 8

pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT =
        VkDebugReportObjectTypeEXT 9

pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT =
        VkDebugReportObjectTypeEXT 10

pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT =
        VkDebugReportObjectTypeEXT 11

pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT =
        VkDebugReportObjectTypeEXT 12

pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT =
        VkDebugReportObjectTypeEXT 13

pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT =
        VkDebugReportObjectTypeEXT 14

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT =
        VkDebugReportObjectTypeEXT 15

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT =
        VkDebugReportObjectTypeEXT 16

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT =
        VkDebugReportObjectTypeEXT 17

pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT =
        VkDebugReportObjectTypeEXT 18

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT =
        VkDebugReportObjectTypeEXT 19

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT =
        VkDebugReportObjectTypeEXT 20

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT =
        VkDebugReportObjectTypeEXT 21

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT =
        VkDebugReportObjectTypeEXT 22

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT =
        VkDebugReportObjectTypeEXT 23

pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT =
        VkDebugReportObjectTypeEXT 24

pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT =
        VkDebugReportObjectTypeEXT 25

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT =
        VkDebugReportObjectTypeEXT 26

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT =
        VkDebugReportObjectTypeEXT 27

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
        :: VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT =
        VkDebugReportObjectTypeEXT 28

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT =
        VkDebugReportObjectTypeEXT 29

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT =
        VkDebugReportObjectTypeEXT 30

pattern VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT =
        VkDebugReportObjectTypeEXT 31

pattern VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT
        :: VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT
        = VkDebugReportObjectTypeEXT 32

pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT =
        VkDebugReportObjectTypeEXT 33

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkRasterizationOrderAMD.html VkRasterizationOrderAMD registry at www.khronos.org>
newtype VkRasterizationOrderAMD = VkRasterizationOrderAMD Int32
                                    deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkRasterizationOrderAMD where
        showsPrec _ VK_RASTERIZATION_ORDER_STRICT_AMD
          = showString "VK_RASTERIZATION_ORDER_STRICT_AMD"
        showsPrec _ VK_RASTERIZATION_ORDER_RELAXED_AMD
          = showString "VK_RASTERIZATION_ORDER_RELAXED_AMD"
        showsPrec p (VkRasterizationOrderAMD x)
          = showParen (p >= 11)
              (showString "VkRasterizationOrderAMD " . showsPrec 11 x)

instance Read VkRasterizationOrderAMD where
        readPrec
          = parens
              (choose
                 [("VK_RASTERIZATION_ORDER_STRICT_AMD",
                   pure VK_RASTERIZATION_ORDER_STRICT_AMD),
                  ("VK_RASTERIZATION_ORDER_RELAXED_AMD",
                   pure VK_RASTERIZATION_ORDER_RELAXED_AMD)]
                 +++
                 prec 10
                   (expectP (Ident "VkRasterizationOrderAMD") >>
                      (VkRasterizationOrderAMD <$> step readPrec)))

pattern VK_RASTERIZATION_ORDER_STRICT_AMD ::
        VkRasterizationOrderAMD

pattern VK_RASTERIZATION_ORDER_STRICT_AMD =
        VkRasterizationOrderAMD 0

pattern VK_RASTERIZATION_ORDER_RELAXED_AMD ::
        VkRasterizationOrderAMD

pattern VK_RASTERIZATION_ORDER_RELAXED_AMD =
        VkRasterizationOrderAMD 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryHandleTypeFlagBitsNV.html VkExternalMemoryHandleTypeFlagBitsNV registry at www.khronos.org>
newtype VkExternalMemoryHandleTypeFlagBitsNV = VkExternalMemoryHandleTypeFlagBitsNV Int32
                                                 deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                           Storable, Enum, Data, Generic)

instance Show VkExternalMemoryHandleTypeFlagBitsNV where
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
          = showString
              "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
          = showString
              "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV"
        showsPrec p (VkExternalMemoryHandleTypeFlagBitsNV x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryHandleTypeFlagBitsNV " .
                 showsPrec 11 x)

instance Read VkExternalMemoryHandleTypeFlagBitsNV where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryHandleTypeFlagBitsNV") >>
                      (VkExternalMemoryHandleTypeFlagBitsNV <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV ::
        VkExternalMemoryHandleTypeFlagBitsNV

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV =
        VkExternalMemoryHandleTypeFlagBitsNV 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV ::
        VkExternalMemoryHandleTypeFlagBitsNV

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV =
        VkExternalMemoryHandleTypeFlagBitsNV 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV ::
        VkExternalMemoryHandleTypeFlagBitsNV

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV =
        VkExternalMemoryHandleTypeFlagBitsNV 4

-- | bitpos = @3@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV ::
        VkExternalMemoryHandleTypeFlagBitsNV

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV =
        VkExternalMemoryHandleTypeFlagBitsNV 8

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryFeatureFlagBitsNV.html VkExternalMemoryFeatureFlagBitsNV registry at www.khronos.org>
newtype VkExternalMemoryFeatureFlagBitsNV = VkExternalMemoryFeatureFlagBitsNV Int32
                                              deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                        Storable, Enum, Data, Generic)

instance Show VkExternalMemoryFeatureFlagBitsNV where
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV"
        showsPrec p (VkExternalMemoryFeatureFlagBitsNV x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryFeatureFlagBitsNV " . showsPrec 11 x)

instance Read VkExternalMemoryFeatureFlagBitsNV where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryFeatureFlagBitsNV") >>
                      (VkExternalMemoryFeatureFlagBitsNV <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV ::
        VkExternalMemoryFeatureFlagBitsNV

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV =
        VkExternalMemoryFeatureFlagBitsNV 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV ::
        VkExternalMemoryFeatureFlagBitsNV

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV =
        VkExternalMemoryFeatureFlagBitsNV 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV ::
        VkExternalMemoryFeatureFlagBitsNV

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV =
        VkExternalMemoryFeatureFlagBitsNV 4

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkValidationCheckEXT.html VkValidationCheckEXT registry at www.khronos.org>
newtype VkValidationCheckEXT = VkValidationCheckEXT Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkValidationCheckEXT where
        showsPrec _ VK_VALIDATION_CHECK_ALL_EXT
          = showString "VK_VALIDATION_CHECK_ALL_EXT"
        showsPrec _ VK_VALIDATION_CHECK_SHADERS_EXT
          = showString "VK_VALIDATION_CHECK_SHADERS_EXT"
        showsPrec p (VkValidationCheckEXT x)
          = showParen (p >= 11)
              (showString "VkValidationCheckEXT " . showsPrec 11 x)

instance Read VkValidationCheckEXT where
        readPrec
          = parens
              (choose
                 [("VK_VALIDATION_CHECK_ALL_EXT", pure VK_VALIDATION_CHECK_ALL_EXT),
                  ("VK_VALIDATION_CHECK_SHADERS_EXT",
                   pure VK_VALIDATION_CHECK_SHADERS_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkValidationCheckEXT") >>
                      (VkValidationCheckEXT <$> step readPrec)))

pattern VK_VALIDATION_CHECK_ALL_EXT :: VkValidationCheckEXT

pattern VK_VALIDATION_CHECK_ALL_EXT = VkValidationCheckEXT 0

pattern VK_VALIDATION_CHECK_SHADERS_EXT :: VkValidationCheckEXT

pattern VK_VALIDATION_CHECK_SHADERS_EXT = VkValidationCheckEXT 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryHandleTypeFlagBitsKHR.html VkExternalMemoryHandleTypeFlagBitsKHR registry at www.khronos.org>
newtype VkExternalMemoryHandleTypeFlagBitsKHR = VkExternalMemoryHandleTypeFlagBitsKHR Int32
                                                  deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                            Storable, Enum, Data, Generic)

instance Show VkExternalMemoryHandleTypeFlagBitsKHR where
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
          = showString
              "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR"
        showsPrec _
          VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR
          = showString
              "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR
          = showString
              "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR"
        showsPrec p (VkExternalMemoryHandleTypeFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryHandleTypeFlagBitsKHR " .
                 showsPrec 11 x)

instance Read VkExternalMemoryHandleTypeFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryHandleTypeFlagBitsKHR") >>
                      (VkExternalMemoryHandleTypeFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 4

-- | bitpos = @3@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 8

-- | bitpos = @4@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 16

-- | bitpos = @5@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 32

-- | bitpos = @6@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 64

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryFeatureFlagBitsKHR.html VkExternalMemoryFeatureFlagBitsKHR registry at www.khronos.org>
newtype VkExternalMemoryFeatureFlagBitsKHR = VkExternalMemoryFeatureFlagBitsKHR Int32
                                               deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                         Storable, Enum, Data, Generic)

instance Show VkExternalMemoryFeatureFlagBitsKHR where
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR"
        showsPrec p (VkExternalMemoryFeatureFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryFeatureFlagBitsKHR " . showsPrec 11 x)

instance Read VkExternalMemoryFeatureFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryFeatureFlagBitsKHR") >>
                      (VkExternalMemoryFeatureFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR ::
        VkExternalMemoryFeatureFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR =
        VkExternalMemoryFeatureFlagBitsKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR ::
        VkExternalMemoryFeatureFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR =
        VkExternalMemoryFeatureFlagBitsKHR 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR ::
        VkExternalMemoryFeatureFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR =
        VkExternalMemoryFeatureFlagBitsKHR 4

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalSemaphoreHandleTypeFlagBitsKHR.html VkExternalSemaphoreHandleTypeFlagBitsKHR registry at www.khronos.org>
newtype VkExternalSemaphoreHandleTypeFlagBitsKHR = VkExternalSemaphoreHandleTypeFlagBitsKHR Int32
                                                     deriving (Bits, FiniteBits, Eq, Ord, Num,
                                                               Bounded, Storable, Enum, Data,
                                                               Generic)

instance Show VkExternalSemaphoreHandleTypeFlagBitsKHR where
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
          = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
          = showString
              "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
        showsPrec _
          VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
          = showString
              "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR
          = showString
              "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR
          = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
        showsPrec p (VkExternalSemaphoreHandleTypeFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkExternalSemaphoreHandleTypeFlagBitsKHR " .
                 showsPrec 11 x)

instance Read VkExternalSemaphoreHandleTypeFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalSemaphoreHandleTypeFlagBitsKHR") >>
                      (VkExternalSemaphoreHandleTypeFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR ::
        VkExternalSemaphoreHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VkExternalSemaphoreHandleTypeFlagBitsKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR ::
        VkExternalSemaphoreHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VkExternalSemaphoreHandleTypeFlagBitsKHR 2

-- | bitpos = @2@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
        :: VkExternalSemaphoreHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
        = VkExternalSemaphoreHandleTypeFlagBitsKHR 4

-- | bitpos = @3@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR ::
        VkExternalSemaphoreHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR =
        VkExternalSemaphoreHandleTypeFlagBitsKHR 8

-- | bitpos = @4@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR ::
        VkExternalSemaphoreHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR =
        VkExternalSemaphoreHandleTypeFlagBitsKHR 16

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalSemaphoreFeatureFlagBitsKHR.html VkExternalSemaphoreFeatureFlagBitsKHR registry at www.khronos.org>
newtype VkExternalSemaphoreFeatureFlagBitsKHR = VkExternalSemaphoreFeatureFlagBitsKHR Int32
                                                  deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                            Storable, Enum, Data, Generic)

instance Show VkExternalSemaphoreFeatureFlagBitsKHR where
        showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR"
        showsPrec p (VkExternalSemaphoreFeatureFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkExternalSemaphoreFeatureFlagBitsKHR " .
                 showsPrec 11 x)

instance Read VkExternalSemaphoreFeatureFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR),
                  ("VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalSemaphoreFeatureFlagBitsKHR") >>
                      (VkExternalSemaphoreFeatureFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR ::
        VkExternalSemaphoreFeatureFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR =
        VkExternalSemaphoreFeatureFlagBitsKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR ::
        VkExternalSemaphoreFeatureFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR =
        VkExternalSemaphoreFeatureFlagBitsKHR 2

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSemaphoreImportFlagBitsKHR.html VkSemaphoreImportFlagBitsKHR registry at www.khronos.org>
newtype VkSemaphoreImportFlagBitsKHR = VkSemaphoreImportFlagBitsKHR Int32
                                         deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                   Storable, Enum, Data, Generic)

instance Show VkSemaphoreImportFlagBitsKHR where
        showsPrec _ VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR
          = showString "VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR"
        showsPrec p (VkSemaphoreImportFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkSemaphoreImportFlagBitsKHR " . showsPrec 11 x)

instance Read VkSemaphoreImportFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR",
                   pure VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkSemaphoreImportFlagBitsKHR") >>
                      (VkSemaphoreImportFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR ::
        VkSemaphoreImportFlagBitsKHR

pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR =
        VkSemaphoreImportFlagBitsKHR 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalFenceHandleTypeFlagBitsKHR.html VkExternalFenceHandleTypeFlagBitsKHR registry at www.khronos.org>
newtype VkExternalFenceHandleTypeFlagBitsKHR = VkExternalFenceHandleTypeFlagBitsKHR Int32
                                                 deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                           Storable, Enum, Data, Generic)

instance Show VkExternalFenceHandleTypeFlagBitsKHR where
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
          = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
          = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
          = showString
              "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR
          = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
        showsPrec p (VkExternalFenceHandleTypeFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkExternalFenceHandleTypeFlagBitsKHR " .
                 showsPrec 11 x)

instance Read VkExternalFenceHandleTypeFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR),
                  ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR),
                  ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR),
                  ("VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalFenceHandleTypeFlagBitsKHR") >>
                      (VkExternalFenceHandleTypeFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR ::
        VkExternalFenceHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VkExternalFenceHandleTypeFlagBitsKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR ::
        VkExternalFenceHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VkExternalFenceHandleTypeFlagBitsKHR 2

-- | bitpos = @2@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR ::
        VkExternalFenceHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR =
        VkExternalFenceHandleTypeFlagBitsKHR 4

-- | bitpos = @3@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR ::
        VkExternalFenceHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR =
        VkExternalFenceHandleTypeFlagBitsKHR 8

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalFenceFeatureFlagBitsKHR.html VkExternalFenceFeatureFlagBitsKHR registry at www.khronos.org>
newtype VkExternalFenceFeatureFlagBitsKHR = VkExternalFenceFeatureFlagBitsKHR Int32
                                              deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                        Storable, Enum, Data, Generic)

instance Show VkExternalFenceFeatureFlagBitsKHR where
        showsPrec _ VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR"
        showsPrec _ VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR"
        showsPrec p (VkExternalFenceFeatureFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkExternalFenceFeatureFlagBitsKHR " . showsPrec 11 x)

instance Read VkExternalFenceFeatureFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR),
                  ("VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalFenceFeatureFlagBitsKHR") >>
                      (VkExternalFenceFeatureFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR ::
        VkExternalFenceFeatureFlagBitsKHR

pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR =
        VkExternalFenceFeatureFlagBitsKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR ::
        VkExternalFenceFeatureFlagBitsKHR

pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR =
        VkExternalFenceFeatureFlagBitsKHR 2

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFenceImportFlagBitsKHR.html VkFenceImportFlagBitsKHR registry at www.khronos.org>
newtype VkFenceImportFlagBitsKHR = VkFenceImportFlagBitsKHR Int32
                                     deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                               Enum, Data, Generic)

instance Show VkFenceImportFlagBitsKHR where
        showsPrec _ VK_FENCE_IMPORT_TEMPORARY_BIT_KHR
          = showString "VK_FENCE_IMPORT_TEMPORARY_BIT_KHR"
        showsPrec p (VkFenceImportFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkFenceImportFlagBitsKHR " . showsPrec 11 x)

instance Read VkFenceImportFlagBitsKHR where
        readPrec
          = parens
              (choose
                 [("VK_FENCE_IMPORT_TEMPORARY_BIT_KHR",
                   pure VK_FENCE_IMPORT_TEMPORARY_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkFenceImportFlagBitsKHR") >>
                      (VkFenceImportFlagBitsKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR ::
        VkFenceImportFlagBitsKHR

pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR =
        VkFenceImportFlagBitsKHR 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSurfaceCounterFlagBitsEXT.html VkSurfaceCounterFlagBitsEXT registry at www.khronos.org>
newtype VkSurfaceCounterFlagBitsEXT = VkSurfaceCounterFlagBitsEXT Int32
                                        deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                                  Enum, Data, Generic)

instance Show VkSurfaceCounterFlagBitsEXT where
        showsPrec _ VK_SURFACE_COUNTER_VBLANK_EXT
          = showString "VK_SURFACE_COUNTER_VBLANK_EXT"
        showsPrec p (VkSurfaceCounterFlagBitsEXT x)
          = showParen (p >= 11)
              (showString "VkSurfaceCounterFlagBitsEXT " . showsPrec 11 x)

instance Read VkSurfaceCounterFlagBitsEXT where
        readPrec
          = parens
              (choose
                 [("VK_SURFACE_COUNTER_VBLANK_EXT",
                   pure VK_SURFACE_COUNTER_VBLANK_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSurfaceCounterFlagBitsEXT") >>
                      (VkSurfaceCounterFlagBitsEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_SURFACE_COUNTER_VBLANK_EXT ::
        VkSurfaceCounterFlagBitsEXT

pattern VK_SURFACE_COUNTER_VBLANK_EXT =
        VkSurfaceCounterFlagBitsEXT 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayPowerStateEXT.html VkDisplayPowerStateEXT registry at www.khronos.org>
newtype VkDisplayPowerStateEXT = VkDisplayPowerStateEXT Int32
                                   deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkDisplayPowerStateEXT where
        showsPrec _ VK_DISPLAY_POWER_STATE_OFF_EXT
          = showString "VK_DISPLAY_POWER_STATE_OFF_EXT"
        showsPrec _ VK_DISPLAY_POWER_STATE_SUSPEND_EXT
          = showString "VK_DISPLAY_POWER_STATE_SUSPEND_EXT"
        showsPrec _ VK_DISPLAY_POWER_STATE_ON_EXT
          = showString "VK_DISPLAY_POWER_STATE_ON_EXT"
        showsPrec p (VkDisplayPowerStateEXT x)
          = showParen (p >= 11)
              (showString "VkDisplayPowerStateEXT " . showsPrec 11 x)

instance Read VkDisplayPowerStateEXT where
        readPrec
          = parens
              (choose
                 [("VK_DISPLAY_POWER_STATE_OFF_EXT",
                   pure VK_DISPLAY_POWER_STATE_OFF_EXT),
                  ("VK_DISPLAY_POWER_STATE_SUSPEND_EXT",
                   pure VK_DISPLAY_POWER_STATE_SUSPEND_EXT),
                  ("VK_DISPLAY_POWER_STATE_ON_EXT",
                   pure VK_DISPLAY_POWER_STATE_ON_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDisplayPowerStateEXT") >>
                      (VkDisplayPowerStateEXT <$> step readPrec)))

pattern VK_DISPLAY_POWER_STATE_OFF_EXT :: VkDisplayPowerStateEXT

pattern VK_DISPLAY_POWER_STATE_OFF_EXT = VkDisplayPowerStateEXT 0

pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT ::
        VkDisplayPowerStateEXT

pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT =
        VkDisplayPowerStateEXT 1

pattern VK_DISPLAY_POWER_STATE_ON_EXT :: VkDisplayPowerStateEXT

pattern VK_DISPLAY_POWER_STATE_ON_EXT = VkDisplayPowerStateEXT 2

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceEventTypeEXT.html VkDeviceEventTypeEXT registry at www.khronos.org>
newtype VkDeviceEventTypeEXT = VkDeviceEventTypeEXT Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkDeviceEventTypeEXT where
        showsPrec _ VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT
          = showString "VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT"
        showsPrec p (VkDeviceEventTypeEXT x)
          = showParen (p >= 11)
              (showString "VkDeviceEventTypeEXT " . showsPrec 11 x)

instance Read VkDeviceEventTypeEXT where
        readPrec
          = parens
              (choose
                 [("VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT",
                   pure VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDeviceEventTypeEXT") >>
                      (VkDeviceEventTypeEXT <$> step readPrec)))

pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT ::
        VkDeviceEventTypeEXT

pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT =
        VkDeviceEventTypeEXT 0

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayEventTypeEXT.html VkDisplayEventTypeEXT registry at www.khronos.org>
newtype VkDisplayEventTypeEXT = VkDisplayEventTypeEXT Int32
                                  deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkDisplayEventTypeEXT where
        showsPrec _ VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT
          = showString "VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT"
        showsPrec p (VkDisplayEventTypeEXT x)
          = showParen (p >= 11)
              (showString "VkDisplayEventTypeEXT " . showsPrec 11 x)

instance Read VkDisplayEventTypeEXT where
        readPrec
          = parens
              (choose
                 [("VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT",
                   pure VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDisplayEventTypeEXT") >>
                      (VkDisplayEventTypeEXT <$> step readPrec)))

pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT ::
        VkDisplayEventTypeEXT

pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT =
        VkDisplayEventTypeEXT 0

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPeerMemoryFeatureFlagBitsKHX.html VkPeerMemoryFeatureFlagBitsKHX registry at www.khronos.org>
newtype VkPeerMemoryFeatureFlagBitsKHX = VkPeerMemoryFeatureFlagBitsKHX Int32
                                           deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                     Storable, Enum, Data, Generic)

instance Show VkPeerMemoryFeatureFlagBitsKHX where
        showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX
          = showString "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX"
        showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX
          = showString "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX"
        showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX
          = showString "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX"
        showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX
          = showString "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX"
        showsPrec p (VkPeerMemoryFeatureFlagBitsKHX x)
          = showParen (p >= 11)
              (showString "VkPeerMemoryFeatureFlagBitsKHX " . showsPrec 11 x)

instance Read VkPeerMemoryFeatureFlagBitsKHX where
        readPrec
          = parens
              (choose
                 [("VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX",
                   pure VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX),
                  ("VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX",
                   pure VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX),
                  ("VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX",
                   pure VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX),
                  ("VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX",
                   pure VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX)]
                 +++
                 prec 10
                   (expectP (Ident "VkPeerMemoryFeatureFlagBitsKHX") >>
                      (VkPeerMemoryFeatureFlagBitsKHX <$> step readPrec)))

-- | Can read with vkCmdCopy commands
--
--   bitpos = @0@
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX ::
        VkPeerMemoryFeatureFlagBitsKHX

pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX =
        VkPeerMemoryFeatureFlagBitsKHX 1

-- | Can write with vkCmdCopy commands
--
--   bitpos = @1@
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX ::
        VkPeerMemoryFeatureFlagBitsKHX

pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX =
        VkPeerMemoryFeatureFlagBitsKHX 2

-- | Can read with any access type/command
--
--   bitpos = @2@
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX ::
        VkPeerMemoryFeatureFlagBitsKHX

pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX =
        VkPeerMemoryFeatureFlagBitsKHX 4

-- | Can write with and access type/command
--
--   bitpos = @3@
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX ::
        VkPeerMemoryFeatureFlagBitsKHX

pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX =
        VkPeerMemoryFeatureFlagBitsKHX 8

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryAllocateFlagBitsKHX.html VkMemoryAllocateFlagBitsKHX registry at www.khronos.org>
newtype VkMemoryAllocateFlagBitsKHX = VkMemoryAllocateFlagBitsKHX Int32
                                        deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded, Storable,
                                                  Enum, Data, Generic)

instance Show VkMemoryAllocateFlagBitsKHX where
        showsPrec _ VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX
          = showString "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX"
        showsPrec p (VkMemoryAllocateFlagBitsKHX x)
          = showParen (p >= 11)
              (showString "VkMemoryAllocateFlagBitsKHX " . showsPrec 11 x)

instance Read VkMemoryAllocateFlagBitsKHX where
        readPrec
          = parens
              (choose
                 [("VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX",
                   pure VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX)]
                 +++
                 prec 10
                   (expectP (Ident "VkMemoryAllocateFlagBitsKHX") >>
                      (VkMemoryAllocateFlagBitsKHX <$> step readPrec)))

-- | Force allocation on specific devices
--
--   bitpos = @0@
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX ::
        VkMemoryAllocateFlagBitsKHX

pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX =
        VkMemoryAllocateFlagBitsKHX 1

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGroupPresentModeFlagBitsKHX.html VkDeviceGroupPresentModeFlagBitsKHX registry at www.khronos.org>
newtype VkDeviceGroupPresentModeFlagBitsKHX = VkDeviceGroupPresentModeFlagBitsKHX Int32
                                                deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                          Storable, Enum, Data, Generic)

instance Show VkDeviceGroupPresentModeFlagBitsKHX where
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX
          = showString "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX"
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX
          = showString "VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX"
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX
          = showString "VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX"
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX
          = showString
              "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX"
        showsPrec p (VkDeviceGroupPresentModeFlagBitsKHX x)
          = showParen (p >= 11)
              (showString "VkDeviceGroupPresentModeFlagBitsKHX " .
                 showsPrec 11 x)

instance Read VkDeviceGroupPresentModeFlagBitsKHX where
        readPrec
          = parens
              (choose
                 [("VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX),
                  ("VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX),
                  ("VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX),
                  ("VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX)]
                 +++
                 prec 10
                   (expectP (Ident "VkDeviceGroupPresentModeFlagBitsKHX") >>
                      (VkDeviceGroupPresentModeFlagBitsKHX <$> step readPrec)))

-- | Present from local memory
--
--   bitpos = @0@
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX ::
        VkDeviceGroupPresentModeFlagBitsKHX

pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX =
        VkDeviceGroupPresentModeFlagBitsKHX 1

-- | Present from remote memory
--
--   bitpos = @1@
pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX ::
        VkDeviceGroupPresentModeFlagBitsKHX

pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX =
        VkDeviceGroupPresentModeFlagBitsKHX 2

-- | Present sum of local and/or remote memory
--
--   bitpos = @2@
pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX ::
        VkDeviceGroupPresentModeFlagBitsKHX

pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX =
        VkDeviceGroupPresentModeFlagBitsKHX 4

-- | Each physical device presents from local memory
--
--   bitpos = @3@
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX ::
        VkDeviceGroupPresentModeFlagBitsKHX

pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX =
        VkDeviceGroupPresentModeFlagBitsKHX 8

-- | type = @bitmask@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSwapchainCreateFlagBitsKHR.html VkSwapchainCreateFlagBitsKHR registry at www.khronos.org>
newtype VkSwapchainCreateFlagBitsKHR = VkSwapchainCreateFlagBitsKHR Int32
                                         deriving (Bits, FiniteBits, Eq, Ord, Num, Bounded,
                                                   Storable, Enum, Data, Generic)

instance Show VkSwapchainCreateFlagBitsKHR where
        showsPrec p (VkSwapchainCreateFlagBitsKHR x)
          = showParen (p >= 11)
              (showString "VkSwapchainCreateFlagBitsKHR " . showsPrec 11 x)

instance Read VkSwapchainCreateFlagBitsKHR where
        readPrec
          = parens
              (choose [] +++
                 prec 10
                   (expectP (Ident "VkSwapchainCreateFlagBitsKHR") >>
                      (VkSwapchainCreateFlagBitsKHR <$> step readPrec)))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkTessellationDomainOriginKHR.html VkTessellationDomainOriginKHR registry at www.khronos.org>
newtype VkTessellationDomainOriginKHR = VkTessellationDomainOriginKHR Int32
                                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                    Generic)

instance Show VkTessellationDomainOriginKHR where
        showsPrec _ VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR
          = showString "VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR"
        showsPrec _ VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR
          = showString "VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR"
        showsPrec p (VkTessellationDomainOriginKHR x)
          = showParen (p >= 11)
              (showString "VkTessellationDomainOriginKHR " . showsPrec 11 x)

instance Read VkTessellationDomainOriginKHR where
        readPrec
          = parens
              (choose
                 [("VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR",
                   pure VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR),
                  ("VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR",
                   pure VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkTessellationDomainOriginKHR") >>
                      (VkTessellationDomainOriginKHR <$> step readPrec)))

pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR ::
        VkTessellationDomainOriginKHR

pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR =
        VkTessellationDomainOriginKHR 0

pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR ::
        VkTessellationDomainOriginKHR

pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR =
        VkTessellationDomainOriginKHR 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerYcbcrModelConversionKHR.html VkSamplerYcbcrModelConversionKHR registry at www.khronos.org>
newtype VkSamplerYcbcrModelConversionKHR = VkSamplerYcbcrModelConversionKHR Int32
                                             deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                       Generic)

instance Show VkSamplerYcbcrModelConversionKHR where
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR"
        showsPrec p (VkSamplerYcbcrModelConversionKHR x)
          = showParen (p >= 11)
              (showString "VkSamplerYcbcrModelConversionKHR " . showsPrec 11 x)

instance Read VkSamplerYcbcrModelConversionKHR where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerYcbcrModelConversionKHR") >>
                      (VkSamplerYcbcrModelConversionKHR <$> step readPrec)))

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR ::
        VkSamplerYcbcrModelConversionKHR

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR =
        VkSamplerYcbcrModelConversionKHR 0

-- | just range expansion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR ::
        VkSamplerYcbcrModelConversionKHR

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR =
        VkSamplerYcbcrModelConversionKHR 1

-- | aka HD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR ::
        VkSamplerYcbcrModelConversionKHR

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR =
        VkSamplerYcbcrModelConversionKHR 2

-- | aka SD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR ::
        VkSamplerYcbcrModelConversionKHR

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR =
        VkSamplerYcbcrModelConversionKHR 3

-- | aka UHD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR ::
        VkSamplerYcbcrModelConversionKHR

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR =
        VkSamplerYcbcrModelConversionKHR 4

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerYcbcrRangeKHR.html VkSamplerYcbcrRangeKHR registry at www.khronos.org>
newtype VkSamplerYcbcrRangeKHR = VkSamplerYcbcrRangeKHR Int32
                                   deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSamplerYcbcrRangeKHR where
        showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR
          = showString "VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR"
        showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR
          = showString "VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR"
        showsPrec p (VkSamplerYcbcrRangeKHR x)
          = showParen (p >= 11)
              (showString "VkSamplerYcbcrRangeKHR " . showsPrec 11 x)

instance Read VkSamplerYcbcrRangeKHR where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR",
                   pure VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR),
                  ("VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR",
                   pure VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerYcbcrRangeKHR") >>
                      (VkSamplerYcbcrRangeKHR <$> step readPrec)))

-- | Luma 0..1 maps to 0..255, chroma -0.5..0.5 to 1..255 (clamped)
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR ::
        VkSamplerYcbcrRangeKHR

pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR =
        VkSamplerYcbcrRangeKHR 0

-- | Luma 0..1 maps to 16..235, chroma -0.5..0.5 to 16..240
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR ::
        VkSamplerYcbcrRangeKHR

pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR =
        VkSamplerYcbcrRangeKHR 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkChromaLocationKHR.html VkChromaLocationKHR registry at www.khronos.org>
newtype VkChromaLocationKHR = VkChromaLocationKHR Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkChromaLocationKHR where
        showsPrec _ VK_CHROMA_LOCATION_COSITED_EVEN_KHR
          = showString "VK_CHROMA_LOCATION_COSITED_EVEN_KHR"
        showsPrec _ VK_CHROMA_LOCATION_MIDPOINT_KHR
          = showString "VK_CHROMA_LOCATION_MIDPOINT_KHR"
        showsPrec p (VkChromaLocationKHR x)
          = showParen (p >= 11)
              (showString "VkChromaLocationKHR " . showsPrec 11 x)

instance Read VkChromaLocationKHR where
        readPrec
          = parens
              (choose
                 [("VK_CHROMA_LOCATION_COSITED_EVEN_KHR",
                   pure VK_CHROMA_LOCATION_COSITED_EVEN_KHR),
                  ("VK_CHROMA_LOCATION_MIDPOINT_KHR",
                   pure VK_CHROMA_LOCATION_MIDPOINT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkChromaLocationKHR") >>
                      (VkChromaLocationKHR <$> step readPrec)))

pattern VK_CHROMA_LOCATION_COSITED_EVEN_KHR :: VkChromaLocationKHR

pattern VK_CHROMA_LOCATION_COSITED_EVEN_KHR = VkChromaLocationKHR 0

pattern VK_CHROMA_LOCATION_MIDPOINT_KHR :: VkChromaLocationKHR

pattern VK_CHROMA_LOCATION_MIDPOINT_KHR = VkChromaLocationKHR 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerReductionModeEXT.html VkSamplerReductionModeEXT registry at www.khronos.org>
newtype VkSamplerReductionModeEXT = VkSamplerReductionModeEXT Int32
                                      deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                Generic)

instance Show VkSamplerReductionModeEXT where
        showsPrec _ VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT
          = showString "VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT"
        showsPrec _ VK_SAMPLER_REDUCTION_MODE_MIN_EXT
          = showString "VK_SAMPLER_REDUCTION_MODE_MIN_EXT"
        showsPrec _ VK_SAMPLER_REDUCTION_MODE_MAX_EXT
          = showString "VK_SAMPLER_REDUCTION_MODE_MAX_EXT"
        showsPrec p (VkSamplerReductionModeEXT x)
          = showParen (p >= 11)
              (showString "VkSamplerReductionModeEXT " . showsPrec 11 x)

instance Read VkSamplerReductionModeEXT where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT",
                   pure VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT),
                  ("VK_SAMPLER_REDUCTION_MODE_MIN_EXT",
                   pure VK_SAMPLER_REDUCTION_MODE_MIN_EXT),
                  ("VK_SAMPLER_REDUCTION_MODE_MAX_EXT",
                   pure VK_SAMPLER_REDUCTION_MODE_MAX_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerReductionModeEXT") >>
                      (VkSamplerReductionModeEXT <$> step readPrec)))

pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT ::
        VkSamplerReductionModeEXT

pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT =
        VkSamplerReductionModeEXT 0

pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT ::
        VkSamplerReductionModeEXT

pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT =
        VkSamplerReductionModeEXT 1

pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT ::
        VkSamplerReductionModeEXT

pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT =
        VkSamplerReductionModeEXT 2

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBlendOverlapEXT.html VkBlendOverlapEXT registry at www.khronos.org>
newtype VkBlendOverlapEXT = VkBlendOverlapEXT Int32
                              deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkBlendOverlapEXT where
        showsPrec _ VK_BLEND_OVERLAP_UNCORRELATED_EXT
          = showString "VK_BLEND_OVERLAP_UNCORRELATED_EXT"
        showsPrec _ VK_BLEND_OVERLAP_DISJOINT_EXT
          = showString "VK_BLEND_OVERLAP_DISJOINT_EXT"
        showsPrec _ VK_BLEND_OVERLAP_CONJOINT_EXT
          = showString "VK_BLEND_OVERLAP_CONJOINT_EXT"
        showsPrec p (VkBlendOverlapEXT x)
          = showParen (p >= 11)
              (showString "VkBlendOverlapEXT " . showsPrec 11 x)

instance Read VkBlendOverlapEXT where
        readPrec
          = parens
              (choose
                 [("VK_BLEND_OVERLAP_UNCORRELATED_EXT",
                   pure VK_BLEND_OVERLAP_UNCORRELATED_EXT),
                  ("VK_BLEND_OVERLAP_DISJOINT_EXT",
                   pure VK_BLEND_OVERLAP_DISJOINT_EXT),
                  ("VK_BLEND_OVERLAP_CONJOINT_EXT",
                   pure VK_BLEND_OVERLAP_CONJOINT_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkBlendOverlapEXT") >>
                      (VkBlendOverlapEXT <$> step readPrec)))

pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT = VkBlendOverlapEXT 0

pattern VK_BLEND_OVERLAP_DISJOINT_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_DISJOINT_EXT = VkBlendOverlapEXT 1

pattern VK_BLEND_OVERLAP_CONJOINT_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_CONJOINT_EXT = VkBlendOverlapEXT 2

type HS_vkInternalAllocationNotification =
     Ptr Void ->
       #{type size_t} ->
         VkInternalAllocationType -> VkSystemAllocationScope -> IO ()

-- | > typedef void (VKAPI_PTR *PFN_vkInternalAllocationNotification)(
--   >     void*                                       pUserData,
--   >     size_t                                      size,
--   >     VkInternalAllocationType                    allocationType,
--   >     VkSystemAllocationScope                     allocationScope);
type PFN_vkInternalAllocationNotification =
     FunPtr HS_vkInternalAllocationNotification

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkInternalAllocationNotification
               ::
               HS_vkInternalAllocationNotification ->
                 IO PFN_vkInternalAllocationNotification

foreign import ccall "dynamic"
               unwrapVkInternalAllocationNotification ::
               PFN_vkInternalAllocationNotification ->
                 HS_vkInternalAllocationNotification

type HS_vkInternalFreeNotification =
     Ptr Void ->
       #{type size_t} ->
         VkInternalAllocationType -> VkSystemAllocationScope -> IO ()

-- | > typedef void (VKAPI_PTR *PFN_vkInternalFreeNotification)(
--   >     void*                                       pUserData,
--   >     size_t                                      size,
--   >     VkInternalAllocationType                    allocationType,
--   >     VkSystemAllocationScope                     allocationScope);
type PFN_vkInternalFreeNotification =
     FunPtr HS_vkInternalFreeNotification

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkInternalFreeNotification ::
               HS_vkInternalFreeNotification -> IO PFN_vkInternalFreeNotification

foreign import ccall "dynamic" unwrapVkInternalFreeNotification ::
               PFN_vkInternalFreeNotification -> HS_vkInternalFreeNotification

type HS_vkReallocationFunction =
     Ptr Void ->
       Ptr Void ->
         #{type size_t} ->
           #{type size_t} ->
             VkSystemAllocationScope -> IO (Ptr Void)

-- | > typedef void* (VKAPI_PTR *PFN_vkReallocationFunction)(
--   >     void*                                       pUserData,
--   >     void*                                       pOriginal,
--   >     size_t                                      size,
--   >     size_t                                      alignment,
--   >     VkSystemAllocationScope                     allocationScope);
type PFN_vkReallocationFunction = FunPtr HS_vkReallocationFunction

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkReallocationFunction ::
               HS_vkReallocationFunction -> IO PFN_vkReallocationFunction

foreign import ccall "dynamic" unwrapVkReallocationFunction ::
               PFN_vkReallocationFunction -> HS_vkReallocationFunction

type HS_vkAllocationFunction =
     Ptr Void ->
       #{type size_t} ->
         #{type size_t} ->
           VkSystemAllocationScope -> IO (Ptr Void)

-- | > typedef void* (VKAPI_PTR *PFN_vkAllocationFunction)(
--   >     void*                                       pUserData,
--   >     size_t                                      size,
--   >     size_t                                      alignment,
--   >     VkSystemAllocationScope                     allocationScope);
type PFN_vkAllocationFunction = FunPtr HS_vkAllocationFunction

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkAllocationFunction ::
               HS_vkAllocationFunction -> IO PFN_vkAllocationFunction

foreign import ccall "dynamic" unwrapVkAllocationFunction ::
               PFN_vkAllocationFunction -> HS_vkAllocationFunction

type HS_vkFreeFunction = Ptr Void -> Ptr Void -> IO ()

-- | > typedef void (VKAPI_PTR *PFN_vkFreeFunction)(
--   >     void*                                       pUserData,
--   >     void*                                       pMemory);
type PFN_vkFreeFunction = FunPtr HS_vkFreeFunction

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkFreeFunction ::
               HS_vkFreeFunction -> IO PFN_vkFreeFunction

foreign import ccall "dynamic" unwrapVkFreeFunction ::
               PFN_vkFreeFunction -> HS_vkFreeFunction

type HS_vkVoidFunction = IO ()

-- | > typedef void (VKAPI_PTR *PFN_vkVoidFunction)(void);
type PFN_vkVoidFunction = FunPtr HS_vkVoidFunction

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkVoidFunction ::
               HS_vkVoidFunction -> IO PFN_vkVoidFunction

foreign import ccall "dynamic" unwrapVkVoidFunction ::
               PFN_vkVoidFunction -> HS_vkVoidFunction

type HS_vkDebugReportCallbackEXT =
     VkDebugReportFlagsEXT ->
       VkDebugReportObjectTypeEXT ->
         Word64 ->
           #{type size_t} ->
             Int32 -> CString -> CString -> Ptr Void -> IO VkBool32

-- | > typedef VkBool32 (VKAPI_PTR *PFN_vkDebugReportCallbackEXT)(
--   >     VkDebugReportFlagsEXT                       flags,
--   >     VkDebugReportObjectTypeEXT                  objectType,
--   >     uint64_t                                    object,
--   >     size_t                                      location,
--   >     int32_t                                     messageCode,
--   >     const char*                                 pLayerPrefix,
--   >     const char*                                 pMessage,
--   >     void*                                       pUserData);
type PFN_vkDebugReportCallbackEXT =
     FunPtr HS_vkDebugReportCallbackEXT

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkDebugReportCallbackEXT ::
               HS_vkDebugReportCallbackEXT -> IO PFN_vkDebugReportCallbackEXT

foreign import ccall "dynamic" unwrapVkDebugReportCallbackEXT ::
               PFN_vkDebugReportCallbackEXT -> HS_vkDebugReportCallbackEXT
