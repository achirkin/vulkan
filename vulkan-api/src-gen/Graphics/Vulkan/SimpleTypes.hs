{-# LANGUAGE CPP                        #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE ViewPatterns               #-}
module Graphics.Vulkan.SimpleTypes
       (-- * API Constants
        pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE, pattern VK_UUID_SIZE,
        pattern VK_LUID_SIZE_KHR, pattern VK_MAX_EXTENSION_NAME_SIZE,
        pattern VK_MAX_DESCRIPTION_SIZE, pattern VK_MAX_MEMORY_TYPES,
        pattern VK_MAX_MEMORY_HEAPS, pattern VK_LOD_CLAMP_NONE,
        pattern VK_REMAINING_MIP_LEVELS, pattern VK_REMAINING_ARRAY_LAYERS,
        pattern VK_WHOLE_SIZE, pattern VK_ATTACHMENT_UNUSED,
        pattern VK_TRUE, pattern VK_FALSE, pattern VK_QUEUE_FAMILY_IGNORED,
        pattern VK_QUEUE_FAMILY_EXTERNAL_KHR,
        pattern VK_QUEUE_FAMILY_FOREIGN_EXT, pattern VK_SUBPASS_EXTERNAL,
        pattern VK_MAX_DEVICE_GROUP_SIZE_KHX, -- * Types and enumerations
                                              --

                                              -- ** Include pragmas
                                              --
                                              -- |
                                              -- > #include "vk_platform.h"
                                              --

                                              -- *** WSI extensions
                                              --
                                              -- |
                                              -- > #include "vulkan.h"
                                              --
                                              -- > #include <X11/Xlib.h>
                                              --
                                              -- > #include <X11/extensions/Xrandr.h>
                                              --
                                              -- > #include <android/native_window.h>
                                              --
                                              -- > #include <mir_toolkit/client_types.h>
                                              --
                                              -- > #include <wayland-client.h>
                                              --
                                              -- > #include <windows.h>
                                              --
                                              -- > #include <xcb/xcb.h>
                                              --

                                              -- ** External types
                                              Display, VisualID, Window,
        RROutput, ANativeWindow, MirConnection, MirSurface, Wl_display,
        Wl_surface, HINSTANCE, HWND, HANDLE, SECURITY_ATTRIBUTES, DWORD,
        LPCWSTR, Xcb_connection_t, Xcb_visualid_t, Xcb_window_t,
        -- ** Define pragmas
        _VK_MAKE_VERSION, _VK_VERSION_MAJOR, _VK_VERSION_MINOR,
        _VK_VERSION_PATCH, -- | ===== @VK_API_VERSION@
                           -- > // DEPRECATED: This define has been removed. Specific version defines (e.g. VK_API_VERSION_1_0), or the VK_MAKE_VERSION macro, should be used instead.
                           -- > //#define VK_API_VERSION VK_MAKE_VERSION(1, 0, 0) // Patch version should always be set to 0
                           pattern VK_API_VERSION_1_0,
        pattern VK_HEADER_VERSION, Ptr(), -- | ===== @VK_DEFINE_HANDLE@
                                          -- Dispatchable handles are represented as `Foreign.Ptr`
                                          --
                                          -- >
                                          -- > #define VK_DEFINE_HANDLE(object) typedef struct object##_T* object;
                                          VkPtr(..), VulkanPtr(..),
        pattern VK_NULL_HANDLE, -- ** Base types
                                VkSampleMask, VkBool32, VkFlags,
        VkDeviceSize, -- ** External types
                      --

                      -- *** Basic C types, pulled in via vk_platform.h
                      Foreign.C.Types.CChar, Foreign.C.Types.CFloat,
        Data.Word.Word8, Data.Word.Word32, Data.Word.Word64,
        Data.Int.Int32, Foreign.C.Types.CSize, Foreign.C.Types.CInt,
        -- ** Bitmasks
        --

        -- *** Bitmask types
        VkFramebufferCreateFlags, VkQueryPoolCreateFlags,
        VkRenderPassCreateFlags, VkSamplerCreateFlags,
        VkPipelineLayoutCreateFlags, VkPipelineCacheCreateFlags,
        VkPipelineDepthStencilStateCreateFlags,
        VkPipelineDynamicStateCreateFlags,
        VkPipelineColorBlendStateCreateFlags,
        VkPipelineMultisampleStateCreateFlags,
        VkPipelineRasterizationStateCreateFlags,
        VkPipelineViewportStateCreateFlags,
        VkPipelineTessellationStateCreateFlags,
        VkPipelineInputAssemblyStateCreateFlags,
        VkPipelineVertexInputStateCreateFlags,
        VkPipelineShaderStageCreateFlags, VkDescriptorSetLayoutCreateFlags,
        VkBufferViewCreateFlags, VkInstanceCreateFlags,
        VkDeviceCreateFlags, VkDeviceQueueCreateFlags, VkQueueFlags,
        VkMemoryPropertyFlags, VkMemoryHeapFlags, VkAccessFlags,
        VkBufferUsageFlags, VkBufferCreateFlags, VkShaderStageFlags,
        VkImageUsageFlags, VkImageCreateFlags, VkImageViewCreateFlags,
        VkPipelineCreateFlags, VkColorComponentFlags, VkFenceCreateFlags,
        VkSemaphoreCreateFlags, VkFormatFeatureFlags, VkQueryControlFlags,
        VkQueryResultFlags, VkShaderModuleCreateFlags, VkEventCreateFlags,
        VkCommandPoolCreateFlags, VkCommandPoolResetFlags,
        VkCommandBufferResetFlags, VkCommandBufferUsageFlags,
        VkQueryPipelineStatisticFlags, VkMemoryMapFlags,
        VkImageAspectFlags, VkSparseMemoryBindFlags,
        VkSparseImageFormatFlags, VkSubpassDescriptionFlags,
        VkPipelineStageFlags, VkSampleCountFlags,
        VkAttachmentDescriptionFlags, VkStencilFaceFlags, VkCullModeFlags,
        VkDescriptorPoolCreateFlags, VkDescriptorPoolResetFlags,
        VkDependencyFlags, VkIndirectCommandsLayoutUsageFlagsNVX,
        VkObjectEntryUsageFlagsNVX,
        VkDescriptorUpdateTemplateCreateFlagsKHR, -- *** WSI extensions
                                                  VkCompositeAlphaFlagsKHR,
        VkDisplayPlaneAlphaFlagsKHR, VkSurfaceTransformFlagsKHR,
        VkSwapchainCreateFlagsKHR, VkDisplayModeCreateFlagsKHR,
        VkDisplaySurfaceCreateFlagsKHR, VkAndroidSurfaceCreateFlagsKHR,
        VkMirSurfaceCreateFlagsKHR, VkViSurfaceCreateFlagsNN,
        VkWaylandSurfaceCreateFlagsKHR, VkWin32SurfaceCreateFlagsKHR,
        VkXlibSurfaceCreateFlagsKHR, VkXcbSurfaceCreateFlagsKHR,
        VkIOSSurfaceCreateFlagsMVK, VkMacOSSurfaceCreateFlagsMVK,
        VkPeerMemoryFeatureFlagsKHX, VkMemoryAllocateFlagsKHX,
        VkDeviceGroupPresentModeFlagsKHX, VkDebugReportFlagsEXT,
        VkCommandPoolTrimFlagsKHR, VkExternalMemoryHandleTypeFlagsNV,
        VkExternalMemoryFeatureFlagsNV, VkExternalMemoryHandleTypeFlagsKHR,
        VkExternalMemoryFeatureFlagsKHR,
        VkExternalSemaphoreHandleTypeFlagsKHR,
        VkExternalSemaphoreFeatureFlagsKHR, VkSemaphoreImportFlagsKHR,
        VkExternalFenceHandleTypeFlagsKHR, VkExternalFenceFeatureFlagsKHR,
        VkFenceImportFlagsKHR, VkSurfaceCounterFlagsEXT,
        VkPipelineViewportSwizzleStateCreateFlagsNV,
        VkPipelineDiscardRectangleStateCreateFlagsEXT,
        VkPipelineCoverageToColorStateCreateFlagsNV,
        VkPipelineCoverageModulationStateCreateFlagsNV,
        VkValidationCacheCreateFlagsEXT,
        VkPipelineRasterizationConservativeStateCreateFlagsEXT, -- ** Handles
                                                                --

                                                                -- *** Types which can be void pointers or class pointers, selected at compile time
                                                                VkInstance,
        VkInstance_T(), VkPhysicalDevice, VkPhysicalDevice_T(), VkDevice,
        VkDevice_T(), VkQueue, VkQueue_T(), VkCommandBuffer,
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
        VkFramebufferCreateFlagBits, VkQueryPoolCreateFlagBits,
        VkRenderPassCreateFlagBits, VkSamplerCreateFlagBits,
        VkPipelineCacheHeaderVersion(..),
        pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE,
        VkPipelineLayoutCreateFlagBits, VkPipelineCacheCreateFlagBits,
        VkPipelineDepthStencilStateCreateFlagBits,
        VkPipelineDynamicStateCreateFlagBits,
        VkPipelineColorBlendStateCreateFlagBits,
        VkPipelineMultisampleStateCreateFlagBits,
        VkPipelineRasterizationStateCreateFlagBits,
        VkPipelineViewportStateCreateFlagBits,
        VkPipelineTessellationStateCreateFlagBits,
        VkPipelineInputAssemblyStateCreateFlagBits,
        VkPipelineVertexInputStateCreateFlagBits,
        VkPipelineShaderStageCreateFlagBits,
        VkDescriptorSetLayoutCreateFlagBits(..),
        VkBufferViewCreateFlagBits, VkInstanceCreateFlagBits,
        VkDeviceQueueCreateFlagBits, VkBufferCreateFlagBits(..),
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
        VkDeviceCreateFlagBits, VkDynamicState(..),
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
import           Data.Bits        (Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.)))
import           Data.Int         (Int32)
import           Data.Void        (Void)
import           Data.Word        (Word32, Word64, Word8)
import           Foreign.C.Types  (CChar, CFloat, CInt, CSize (..), CSize,
                                   CULong (..), CWchar (..))
import           Foreign.Ptr      (FunPtr, Ptr, nullPtr)
import           Foreign.Storable (Storable)

pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE :: (Num a, Eq a) => a

pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256

pattern VK_UUID_SIZE :: (Num a, Eq a) => a

pattern VK_UUID_SIZE = 16

pattern VK_LUID_SIZE_KHR :: (Num a, Eq a) => a

pattern VK_LUID_SIZE_KHR = 8

pattern VK_MAX_EXTENSION_NAME_SIZE :: (Num a, Eq a) => a

pattern VK_MAX_EXTENSION_NAME_SIZE = 256

pattern VK_MAX_DESCRIPTION_SIZE :: (Num a, Eq a) => a

pattern VK_MAX_DESCRIPTION_SIZE = 256

pattern VK_MAX_MEMORY_TYPES :: (Num a, Eq a) => a

pattern VK_MAX_MEMORY_TYPES = 32

-- | The maximum number of unique memory heaps, each of which supporting 1 or more memory types
pattern VK_MAX_MEMORY_HEAPS :: (Num a, Eq a) => a

pattern VK_MAX_MEMORY_HEAPS = 16

pattern VK_LOD_CLAMP_NONE :: (Fractional a, Eq a) => a

pattern VK_LOD_CLAMP_NONE = 1000.0

pattern VK_REMAINING_MIP_LEVELS :: Word32

pattern VK_REMAINING_MIP_LEVELS = 4294967295

pattern VK_REMAINING_ARRAY_LAYERS :: Word32

pattern VK_REMAINING_ARRAY_LAYERS = 4294967295

pattern VK_WHOLE_SIZE :: Word64

pattern VK_WHOLE_SIZE = 18446744073709551615

pattern VK_ATTACHMENT_UNUSED :: Word32

pattern VK_ATTACHMENT_UNUSED = 4294967295

pattern VK_TRUE :: (Num a, Eq a) => a

pattern VK_TRUE = 1

pattern VK_FALSE :: (Num a, Eq a) => a

pattern VK_FALSE = 0

pattern VK_QUEUE_FAMILY_IGNORED :: Word32

pattern VK_QUEUE_FAMILY_IGNORED = 4294967295

pattern VK_QUEUE_FAMILY_EXTERNAL_KHR :: Word32

pattern VK_QUEUE_FAMILY_EXTERNAL_KHR = 4294967294

pattern VK_QUEUE_FAMILY_FOREIGN_EXT :: Word32

pattern VK_QUEUE_FAMILY_FOREIGN_EXT = 4294967293

pattern VK_SUBPASS_EXTERNAL :: Word32

pattern VK_SUBPASS_EXTERNAL = 4294967295

pattern VK_MAX_DEVICE_GROUP_SIZE_KHX :: (Num a, Eq a) => a

pattern VK_MAX_DEVICE_GROUP_SIZE_KHX = 32

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
data Wl_display

-- | Requires @wayland-client.h@
data Wl_surface

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
data Xcb_connection_t

-- | Requires @xcb/xcb.h@
type Xcb_visualid_t = CULong

-- | Requires @xcb/xcb.h@
type Xcb_window_t = CULong

-- | > #define VK_MAKE_VERSION(major, minor, patch) \
--   >     (((major) << 22) | ((minor) << 12) | (patch))
_VK_MAKE_VERSION :: Bits a => a -> a -> a -> a
_VK_MAKE_VERSION major minor patch
  = unsafeShiftL major 22 .|. unsafeShiftL minor 12 .|. patch

{-# INLINE _VK_MAKE_VERSION #-}
#define VK_MAKE_VERSION(major, minor, patch) _VK_MAKE_VERSION major minor patch

-- | > #define VK_VERSION_MAJOR(version) ((uint32_t)(version) >> 22)
_VK_VERSION_MAJOR :: Bits a => a -> a
_VK_VERSION_MAJOR version = unsafeShiftR version 22

{-# INLINE _VK_VERSION_MAJOR #-}
#define VK_VERSION_MAJOR(version) _VK_VERSION_MAJOR version

-- | > #define VK_VERSION_MINOR(version) (((uint32_t)(version) >> 12) & 0x3ff)
_VK_VERSION_MINOR :: (Bits a, Num a) => a -> a
_VK_VERSION_MINOR version = unsafeShiftR version 12 .&. 1023

{-# INLINE _VK_VERSION_MINOR #-}
#define VK_VERSION_MINOR(version) _VK_VERSION_MINOR version

-- | > #define VK_VERSION_PATCH(version) ((uint32_t)(version) & 0xfff)
_VK_VERSION_PATCH :: (Bits a, Num a) => a -> a
_VK_VERSION_PATCH = (.&. 4095)

{-# INLINE _VK_VERSION_PATCH #-}
#define VK_VERSION_PATCH(version) _VK_VERSION_PATCH version

-- | > // Vulkan 1.0 version number
--   > #define VK_API_VERSION_1_0 VK_MAKE_VERSION(1, 0, 0)// Patch version should always be set to 0
pattern VK_API_VERSION_1_0 :: (Num a, Eq a) => a

pattern VK_API_VERSION_1_0 = 4194304

-- | > // Version of this file
--   > #define VK_HEADER_VERSION 67
pattern VK_HEADER_VERSION :: (Num a, Eq a) => a

pattern VK_HEADER_VERSION = 67

instance VulkanPtr Ptr where
        vkNullPtr = nullPtr

        {-# INLINE vkNullPtr #-}

type role VkPtr phantom

-- | ===== @VK_DEFINE_NON_DISPATCHABLE_HANDLE@
-- Non-dispatchable handles are represented as `VkPtr`
--
-- >
-- > #if !defined(VK_DEFINE_NON_DISPATCHABLE_HANDLE)
-- > #if defined(__LP64__) || defined(_WIN64) || (defined(__x86_64__) && !defined(__ILP32__) ) || defined(_M_X64) || defined(__ia64) || defined (_M_IA64) || defined(__aarch64__) || defined(__powerpc64__)
-- >         #define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef struct object##_T *object;
-- > #else
-- >         #define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef uint64_t object;
-- > #endif
-- > #endif
-- >
--
#if !defined(VK_DEFINE_NON_DISPATCHABLE_HANDLE)
#if defined(__LP64__) || defined(_WIN64) || (defined(__x86_64__) && !defined(__ILP32__) ) || defined(_M_X64) || defined(__ia64) || defined (_M_IA64) || defined(__aarch64__) || defined(__powerpc64__)
newtype VkPtr a = VkPtr (Ptr a)
   deriving (Eq, Ord, Show, Storable)
instance VulkanPtr VkPtr where
   vkNullPtr = VkPtr vkNullPtr
   {-# INLINE vkNullPtr #-}
#else
--
newtype VkPtr a = VkPtr Word64
                    deriving (Eq, Ord, Show, Storable)

instance VulkanPtr VkPtr where
        vkNullPtr = VkPtr 0

        {-# INLINE vkNullPtr #-}


#endif
#endif
-- | Unify dispatchable and non-dispatchable vulkan pointer types.
class VulkanPtr ptr where
        vkNullPtr :: ptr a

isNullPtr :: (Eq (ptr a), VulkanPtr ptr) => ptr a -> Bool
isNullPtr = (vkNullPtr ==)

{-# INLINE isNullPtr #-}

-- | >
--   > #define VK_NULL_HANDLE 0
--   >
pattern VK_NULL_HANDLE :: (Eq (ptr a), VulkanPtr ptr) => ptr a

pattern VK_NULL_HANDLE <- (isNullPtr -> True)
  where VK_NULL_HANDLE = vkNullPtr

type VkSampleMask = Word32

type VkBool32 = Word32

type VkFlags = Word32

type VkDeviceSize = Word64

type VkFramebufferCreateFlags = VkFlags

type VkQueryPoolCreateFlags = VkFlags

type VkRenderPassCreateFlags = VkFlags

type VkSamplerCreateFlags = VkFlags

type VkPipelineLayoutCreateFlags = VkFlags

type VkPipelineCacheCreateFlags = VkFlags

type VkPipelineDepthStencilStateCreateFlags = VkFlags

type VkPipelineDynamicStateCreateFlags = VkFlags

type VkPipelineColorBlendStateCreateFlags = VkFlags

type VkPipelineMultisampleStateCreateFlags = VkFlags

type VkPipelineRasterizationStateCreateFlags = VkFlags

type VkPipelineViewportStateCreateFlags = VkFlags

type VkPipelineTessellationStateCreateFlags = VkFlags

type VkPipelineInputAssemblyStateCreateFlags = VkFlags

type VkPipelineVertexInputStateCreateFlags = VkFlags

type VkPipelineShaderStageCreateFlags = VkFlags

type VkDescriptorSetLayoutCreateFlags = VkFlags

type VkBufferViewCreateFlags = VkFlags

type VkInstanceCreateFlags = VkFlags

type VkDeviceCreateFlags = VkFlags

type VkDeviceQueueCreateFlags = VkFlags

type VkQueueFlags = VkFlags

type VkMemoryPropertyFlags = VkFlags

type VkMemoryHeapFlags = VkFlags

type VkAccessFlags = VkFlags

type VkBufferUsageFlags = VkFlags

type VkBufferCreateFlags = VkFlags

type VkShaderStageFlags = VkFlags

type VkImageUsageFlags = VkFlags

type VkImageCreateFlags = VkFlags

type VkImageViewCreateFlags = VkFlags

type VkPipelineCreateFlags = VkFlags

type VkColorComponentFlags = VkFlags

type VkFenceCreateFlags = VkFlags

type VkSemaphoreCreateFlags = VkFlags

type VkFormatFeatureFlags = VkFlags

type VkQueryControlFlags = VkFlags

type VkQueryResultFlags = VkFlags

type VkShaderModuleCreateFlags = VkFlags

type VkEventCreateFlags = VkFlags

type VkCommandPoolCreateFlags = VkFlags

type VkCommandPoolResetFlags = VkFlags

type VkCommandBufferResetFlags = VkFlags

type VkCommandBufferUsageFlags = VkFlags

type VkQueryPipelineStatisticFlags = VkFlags

type VkMemoryMapFlags = VkFlags

type VkImageAspectFlags = VkFlags

type VkSparseMemoryBindFlags = VkFlags

type VkSparseImageFormatFlags = VkFlags

type VkSubpassDescriptionFlags = VkFlags

type VkPipelineStageFlags = VkFlags

type VkSampleCountFlags = VkFlags

type VkAttachmentDescriptionFlags = VkFlags

type VkStencilFaceFlags = VkFlags

type VkCullModeFlags = VkFlags

type VkDescriptorPoolCreateFlags = VkFlags

type VkDescriptorPoolResetFlags = VkFlags

type VkDependencyFlags = VkFlags

type VkIndirectCommandsLayoutUsageFlagsNVX = VkFlags

type VkObjectEntryUsageFlagsNVX = VkFlags

type VkDescriptorUpdateTemplateCreateFlagsKHR = VkFlags

type VkCompositeAlphaFlagsKHR = VkFlags

type VkDisplayPlaneAlphaFlagsKHR = VkFlags

type VkSurfaceTransformFlagsKHR = VkFlags

type VkSwapchainCreateFlagsKHR = VkFlags

type VkDisplayModeCreateFlagsKHR = VkFlags

type VkDisplaySurfaceCreateFlagsKHR = VkFlags

type VkAndroidSurfaceCreateFlagsKHR = VkFlags

type VkMirSurfaceCreateFlagsKHR = VkFlags

type VkViSurfaceCreateFlagsNN = VkFlags

type VkWaylandSurfaceCreateFlagsKHR = VkFlags

type VkWin32SurfaceCreateFlagsKHR = VkFlags

type VkXlibSurfaceCreateFlagsKHR = VkFlags

type VkXcbSurfaceCreateFlagsKHR = VkFlags

type VkIOSSurfaceCreateFlagsMVK = VkFlags

type VkMacOSSurfaceCreateFlagsMVK = VkFlags

type VkPeerMemoryFeatureFlagsKHX = VkFlags

type VkMemoryAllocateFlagsKHX = VkFlags

type VkDeviceGroupPresentModeFlagsKHX = VkFlags

type VkDebugReportFlagsEXT = VkFlags

type VkCommandPoolTrimFlagsKHR = VkFlags

type VkExternalMemoryHandleTypeFlagsNV = VkFlags

type VkExternalMemoryFeatureFlagsNV = VkFlags

type VkExternalMemoryHandleTypeFlagsKHR = VkFlags

type VkExternalMemoryFeatureFlagsKHR = VkFlags

type VkExternalSemaphoreHandleTypeFlagsKHR = VkFlags

type VkExternalSemaphoreFeatureFlagsKHR = VkFlags

type VkSemaphoreImportFlagsKHR = VkFlags

type VkExternalFenceHandleTypeFlagsKHR = VkFlags

type VkExternalFenceFeatureFlagsKHR = VkFlags

type VkFenceImportFlagsKHR = VkFlags

type VkSurfaceCounterFlagsEXT = VkFlags

type VkPipelineViewportSwizzleStateCreateFlagsNV = VkFlags

type VkPipelineDiscardRectangleStateCreateFlagsEXT = VkFlags

type VkPipelineCoverageToColorStateCreateFlagsNV = VkFlags

type VkPipelineCoverageModulationStateCreateFlagsNV = VkFlags

type VkValidationCacheCreateFlagsEXT = VkFlags

type VkPipelineRasterizationConservativeStateCreateFlagsEXT =
     VkFlags

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentLoadOp.html VkAttachmentLoadOp registry at www.khronos.org>
newtype VkAttachmentLoadOp = VkAttachmentLoadOp VkFlags
                               deriving (Eq, Ord, Storable)

pattern VK_ATTACHMENT_LOAD_OP_LOAD :: VkAttachmentLoadOp

pattern VK_ATTACHMENT_LOAD_OP_LOAD = VkAttachmentLoadOp 0

pattern VK_ATTACHMENT_LOAD_OP_CLEAR :: VkAttachmentLoadOp

pattern VK_ATTACHMENT_LOAD_OP_CLEAR = VkAttachmentLoadOp 1

pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE :: VkAttachmentLoadOp

pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE = VkAttachmentLoadOp 2

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentStoreOp.html VkAttachmentStoreOp registry at www.khronos.org>
newtype VkAttachmentStoreOp = VkAttachmentStoreOp VkFlags
                                deriving (Eq, Ord, Storable)

pattern VK_ATTACHMENT_STORE_OP_STORE :: VkAttachmentStoreOp

pattern VK_ATTACHMENT_STORE_OP_STORE = VkAttachmentStoreOp 0

pattern VK_ATTACHMENT_STORE_OP_DONT_CARE :: VkAttachmentStoreOp

pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = VkAttachmentStoreOp 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBlendFactor.html VkBlendFactor registry at www.khronos.org>
newtype VkBlendFactor = VkBlendFactor VkFlags
                          deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBlendOp.html VkBlendOp registry at www.khronos.org>
newtype VkBlendOp = VkBlendOp VkFlags
                      deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBorderColor.html VkBorderColor registry at www.khronos.org>
newtype VkBorderColor = VkBorderColor VkFlags
                          deriving (Eq, Ord, Storable)

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

type VkFramebufferCreateFlagBits = VkFlags

type VkQueryPoolCreateFlagBits = VkFlags

type VkRenderPassCreateFlagBits = VkFlags

type VkSamplerCreateFlagBits = VkFlags

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineCacheHeaderVersion.html VkPipelineCacheHeaderVersion registry at www.khronos.org>
newtype VkPipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion VkFlags
                                         deriving (Eq, Ord, Storable)

pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE ::
        VkPipelineCacheHeaderVersion

pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE =
        VkPipelineCacheHeaderVersion 1

type VkPipelineLayoutCreateFlagBits = VkFlags

type VkPipelineCacheCreateFlagBits = VkFlags

type VkPipelineDepthStencilStateCreateFlagBits = VkFlags

type VkPipelineDynamicStateCreateFlagBits = VkFlags

type VkPipelineColorBlendStateCreateFlagBits = VkFlags

type VkPipelineMultisampleStateCreateFlagBits = VkFlags

type VkPipelineRasterizationStateCreateFlagBits = VkFlags

type VkPipelineViewportStateCreateFlagBits = VkFlags

type VkPipelineTessellationStateCreateFlagBits = VkFlags

type VkPipelineInputAssemblyStateCreateFlagBits = VkFlags

type VkPipelineVertexInputStateCreateFlagBits = VkFlags

type VkPipelineShaderStageCreateFlagBits = VkFlags

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorSetLayoutCreateFlagBits.html VkDescriptorSetLayoutCreateFlagBits registry at www.khronos.org>
newtype VkDescriptorSetLayoutCreateFlagBits = VkDescriptorSetLayoutCreateFlagBits VkFlags
                                                deriving (Eq, Ord, Bits, Storable)

type VkBufferViewCreateFlagBits = VkFlags

type VkInstanceCreateFlagBits = VkFlags

type VkDeviceQueueCreateFlagBits = VkFlags

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBufferCreateFlagBits.html VkBufferCreateFlagBits registry at www.khronos.org>
newtype VkBufferCreateFlagBits = VkBufferCreateFlagBits VkFlags
                                   deriving (Eq, Ord, Bits, Storable)

-- | Buffer should support sparse backing
--
--   Bit position @0@
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT ::
        VkBufferCreateFlagBits

pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT =
        VkBufferCreateFlagBits 1

-- | Buffer should support sparse backing with partial residency
--
--   Bit position @1@
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT ::
        VkBufferCreateFlagBits

pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT =
        VkBufferCreateFlagBits 2

-- | Buffer should support constent data access to physical memory ranges mapped into multiple locations of sparse buffers
--
--   Bit position @2@
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT ::
        VkBufferCreateFlagBits

pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT =
        VkBufferCreateFlagBits 4

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBufferUsageFlagBits.html VkBufferUsageFlagBits registry at www.khronos.org>
newtype VkBufferUsageFlagBits = VkBufferUsageFlagBits VkFlags
                                  deriving (Eq, Ord, Bits, Storable)

-- | Can be used as a source of transfer operations
--
--   Bit position @0@
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT = VkBufferUsageFlagBits 1

-- | Can be used as a destination of transfer operations
--
--   Bit position @1@
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT = VkBufferUsageFlagBits 2

-- | Can be used as TBO
--
--   Bit position @2@
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT ::
        VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT =
        VkBufferUsageFlagBits 4

-- | Can be used as IBO
--
--   Bit position @3@
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT ::
        VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT =
        VkBufferUsageFlagBits 8

-- | Can be used as UBO
--
--   Bit position @4@
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT =
        VkBufferUsageFlagBits 16

-- | Can be used as SSBO
--
--   Bit position @5@
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT =
        VkBufferUsageFlagBits 32

-- | Can be used as source of fixed-function index fetch (index buffer)
--
--   Bit position @6@
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT = VkBufferUsageFlagBits 64

-- | Can be used as source of fixed-function vertex fetch (VBO)
--
--   Bit position @7@
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT :: VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT =
        VkBufferUsageFlagBits 128

-- | Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
--
--   Bit position @8@
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT ::
        VkBufferUsageFlagBits

pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT =
        VkBufferUsageFlagBits 256

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkColorComponentFlagBits.html VkColorComponentFlagBits registry at www.khronos.org>
newtype VkColorComponentFlagBits = VkColorComponentFlagBits VkFlags
                                     deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_COLOR_COMPONENT_R_BIT :: VkColorComponentFlagBits

pattern VK_COLOR_COMPONENT_R_BIT = VkColorComponentFlagBits 1

-- | Bit position @1@
pattern VK_COLOR_COMPONENT_G_BIT :: VkColorComponentFlagBits

pattern VK_COLOR_COMPONENT_G_BIT = VkColorComponentFlagBits 2

-- | Bit position @2@
pattern VK_COLOR_COMPONENT_B_BIT :: VkColorComponentFlagBits

pattern VK_COLOR_COMPONENT_B_BIT = VkColorComponentFlagBits 4

-- | Bit position @3@
pattern VK_COLOR_COMPONENT_A_BIT :: VkColorComponentFlagBits

pattern VK_COLOR_COMPONENT_A_BIT = VkColorComponentFlagBits 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkComponentSwizzle.html VkComponentSwizzle registry at www.khronos.org>
newtype VkComponentSwizzle = VkComponentSwizzle VkFlags
                               deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandPoolCreateFlagBits.html VkCommandPoolCreateFlagBits registry at www.khronos.org>
newtype VkCommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits VkFlags
                                        deriving (Eq, Ord, Bits, Storable)

-- | Command buffers have a short lifetime
--
--   Bit position @0@
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT ::
        VkCommandPoolCreateFlagBits

pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT =
        VkCommandPoolCreateFlagBits 1

-- | Command buffers may release their memory individually
--
--   Bit position @1@
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT ::
        VkCommandPoolCreateFlagBits

pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT =
        VkCommandPoolCreateFlagBits 2

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandPoolResetFlagBits.html VkCommandPoolResetFlagBits registry at www.khronos.org>
newtype VkCommandPoolResetFlagBits = VkCommandPoolResetFlagBits VkFlags
                                       deriving (Eq, Ord, Bits, Storable)

-- | Release resources owned by the pool
--
--   Bit position @0@
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT ::
        VkCommandPoolResetFlagBits

pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT =
        VkCommandPoolResetFlagBits 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferResetFlagBits.html VkCommandBufferResetFlagBits registry at www.khronos.org>
newtype VkCommandBufferResetFlagBits = VkCommandBufferResetFlagBits VkFlags
                                         deriving (Eq, Ord, Bits, Storable)

-- | Release resources owned by the buffer
--
--   Bit position @0@
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT ::
        VkCommandBufferResetFlagBits

pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT =
        VkCommandBufferResetFlagBits 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferLevel.html VkCommandBufferLevel registry at www.khronos.org>
newtype VkCommandBufferLevel = VkCommandBufferLevel VkFlags
                                 deriving (Eq, Ord, Storable)

pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY :: VkCommandBufferLevel

pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY :: VkCommandBufferLevel

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferUsageFlagBits.html VkCommandBufferUsageFlagBits registry at www.khronos.org>
newtype VkCommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits VkFlags
                                         deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT ::
        VkCommandBufferUsageFlagBits

pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT =
        VkCommandBufferUsageFlagBits 1

-- | Bit position @1@
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT ::
        VkCommandBufferUsageFlagBits

pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT =
        VkCommandBufferUsageFlagBits 2

-- | Command buffer may be submitted/executed more than once simultaneously
--
--   Bit position @2@
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT ::
        VkCommandBufferUsageFlagBits

pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT =
        VkCommandBufferUsageFlagBits 4

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCompareOp.html VkCompareOp registry at www.khronos.org>
newtype VkCompareOp = VkCompareOp VkFlags
                        deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCullModeFlagBits.html VkCullModeFlagBits registry at www.khronos.org>
newtype VkCullModeFlagBits = VkCullModeFlagBits VkFlags
                               deriving (Eq, Ord, Bits, Storable)

-- | Bitmask value @0x0@
pattern VK_CULL_MODE_NONE :: VkCullModeFlagBits

pattern VK_CULL_MODE_NONE = VkCullModeFlagBits 0

-- | Bit position @0@
pattern VK_CULL_MODE_FRONT_BIT :: VkCullModeFlagBits

pattern VK_CULL_MODE_FRONT_BIT = VkCullModeFlagBits 1

-- | Bit position @1@
pattern VK_CULL_MODE_BACK_BIT :: VkCullModeFlagBits

pattern VK_CULL_MODE_BACK_BIT = VkCullModeFlagBits 2

-- | Bitmask value @0x3@
pattern VK_CULL_MODE_FRONT_AND_BACK :: VkCullModeFlagBits

pattern VK_CULL_MODE_FRONT_AND_BACK = VkCullModeFlagBits 3

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorType.html VkDescriptorType registry at www.khronos.org>
newtype VkDescriptorType = VkDescriptorType VkFlags
                             deriving (Eq, Ord, Storable)

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

type VkDeviceCreateFlagBits = VkFlags

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDynamicState.html VkDynamicState registry at www.khronos.org>
newtype VkDynamicState = VkDynamicState VkFlags
                           deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFenceCreateFlagBits.html VkFenceCreateFlagBits registry at www.khronos.org>
newtype VkFenceCreateFlagBits = VkFenceCreateFlagBits VkFlags
                                  deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_FENCE_CREATE_SIGNALED_BIT :: VkFenceCreateFlagBits

pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateFlagBits 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPolygonMode.html VkPolygonMode registry at www.khronos.org>
newtype VkPolygonMode = VkPolygonMode VkFlags
                          deriving (Eq, Ord, Storable)

pattern VK_POLYGON_MODE_FILL :: VkPolygonMode

pattern VK_POLYGON_MODE_FILL = VkPolygonMode 0

pattern VK_POLYGON_MODE_LINE :: VkPolygonMode

pattern VK_POLYGON_MODE_LINE = VkPolygonMode 1

pattern VK_POLYGON_MODE_POINT :: VkPolygonMode

pattern VK_POLYGON_MODE_POINT = VkPolygonMode 2

-- | Vulkan format definitions
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFormat.html VkFormat registry at www.khronos.org>
newtype VkFormat = VkFormat VkFlags
                     deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFormatFeatureFlagBits.html VkFormatFeatureFlagBits registry at www.khronos.org>
newtype VkFormatFeatureFlagBits = VkFormatFeatureFlagBits VkFlags
                                    deriving (Eq, Ord, Bits, Storable)

-- | Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
--
--   Bit position @0@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT =
        VkFormatFeatureFlagBits 1

-- | Format can be used for storage images (STORAGE_IMAGE descriptor type)
--
--   Bit position @1@
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT =
        VkFormatFeatureFlagBits 2

-- | Format supports atomic operations in case it is used for storage images
--
--   Bit position @2@
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT =
        VkFormatFeatureFlagBits 4

-- | Format can be used for uniform texel buffers (TBOs)
--
--   Bit position @3@
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT =
        VkFormatFeatureFlagBits 8

-- | Format can be used for storage texel buffers (IBOs)
--
--   Bit position @4@
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT =
        VkFormatFeatureFlagBits 16

-- | Format supports atomic operations in case it is used for storage texel buffers
--
--   Bit position @5@
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT =
        VkFormatFeatureFlagBits 32

-- | Format can be used for vertex buffers (VBOs)
--
--   Bit position @6@
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT =
        VkFormatFeatureFlagBits 64

-- | Format can be used for color attachment images
--
--   Bit position @7@
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT =
        VkFormatFeatureFlagBits 128

-- | Format supports blending in case it is used for color attachment images
--
--   Bit position @8@
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT =
        VkFormatFeatureFlagBits 256

-- | Format can be used for depth/stencil attachment images
--
--   Bit position @9@
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT =
        VkFormatFeatureFlagBits 512

-- | Format can be used as the source image of blits with vkCmdBlitImage
--
--   Bit position @10@
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT =
        VkFormatFeatureFlagBits 1024

-- | Format can be used as the destination image of blits with vkCmdBlitImage
--
--   Bit position @11@
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_BLIT_DST_BIT =
        VkFormatFeatureFlagBits 2048

-- | Format can be filtered with VK_FILTER_LINEAR when being sampled
--
--   Bit position @12@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT =
        VkFormatFeatureFlagBits 4096

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFrontFace.html VkFrontFace registry at www.khronos.org>
newtype VkFrontFace = VkFrontFace VkFlags
                        deriving (Eq, Ord, Storable)

pattern VK_FRONT_FACE_COUNTER_CLOCKWISE :: VkFrontFace

pattern VK_FRONT_FACE_COUNTER_CLOCKWISE = VkFrontFace 0

pattern VK_FRONT_FACE_CLOCKWISE :: VkFrontFace

pattern VK_FRONT_FACE_CLOCKWISE = VkFrontFace 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageAspectFlagBits.html VkImageAspectFlagBits registry at www.khronos.org>
newtype VkImageAspectFlagBits = VkImageAspectFlagBits VkFlags
                                  deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_IMAGE_ASPECT_COLOR_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_COLOR_BIT = VkImageAspectFlagBits 1

-- | Bit position @1@
pattern VK_IMAGE_ASPECT_DEPTH_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_DEPTH_BIT = VkImageAspectFlagBits 2

-- | Bit position @2@
pattern VK_IMAGE_ASPECT_STENCIL_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_STENCIL_BIT = VkImageAspectFlagBits 4

-- | Bit position @3@
pattern VK_IMAGE_ASPECT_METADATA_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_METADATA_BIT = VkImageAspectFlagBits 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageCreateFlagBits.html VkImageCreateFlagBits registry at www.khronos.org>
newtype VkImageCreateFlagBits = VkImageCreateFlagBits VkFlags
                                  deriving (Eq, Ord, Bits, Storable)

-- | Image should support sparse backing
--
--   Bit position @0@
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT =
        VkImageCreateFlagBits 1

-- | Image should support sparse backing with partial residency
--
--   Bit position @1@
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT =
        VkImageCreateFlagBits 2

-- | Image should support constent data access to physical memory ranges mapped into multiple locations of sparse images
--
--   Bit position @2@
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT =
        VkImageCreateFlagBits 4

-- | Allows image views to have different format than the base image
--
--   Bit position @3@
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT =
        VkImageCreateFlagBits 8

-- | Allows creating image views with cube type from the created image
--
--   Bit position @4@
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT =
        VkImageCreateFlagBits 16

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageLayout.html VkImageLayout registry at www.khronos.org>
newtype VkImageLayout = VkImageLayout VkFlags
                          deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageTiling.html VkImageTiling registry at www.khronos.org>
newtype VkImageTiling = VkImageTiling VkFlags
                          deriving (Eq, Ord, Storable)

pattern VK_IMAGE_TILING_OPTIMAL :: VkImageTiling

pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0

pattern VK_IMAGE_TILING_LINEAR :: VkImageTiling

pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageType.html VkImageType registry at www.khronos.org>
newtype VkImageType = VkImageType VkFlags
                        deriving (Eq, Ord, Storable)

pattern VK_IMAGE_TYPE_1D :: VkImageType

pattern VK_IMAGE_TYPE_1D = VkImageType 0

pattern VK_IMAGE_TYPE_2D :: VkImageType

pattern VK_IMAGE_TYPE_2D = VkImageType 1

pattern VK_IMAGE_TYPE_3D :: VkImageType

pattern VK_IMAGE_TYPE_3D = VkImageType 2

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageUsageFlagBits.html VkImageUsageFlagBits registry at www.khronos.org>
newtype VkImageUsageFlagBits = VkImageUsageFlagBits VkFlags
                                 deriving (Eq, Ord, Bits, Storable)

-- | Can be used as a source of transfer operations
--
--   Bit position @0@
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageFlagBits 1

-- | Can be used as a destination of transfer operations
--
--   Bit position @1@
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageFlagBits 2

-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
--
--   Bit position @2@
pattern VK_IMAGE_USAGE_SAMPLED_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageFlagBits 4

-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
--
--   Bit position @3@
pattern VK_IMAGE_USAGE_STORAGE_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageFlagBits 8

-- | Can be used as framebuffer color attachment
--
--   Bit position @4@
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT =
        VkImageUsageFlagBits 16

-- | Can be used as framebuffer depth/stencil attachment
--
--   Bit position @5@
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT ::
        VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT =
        VkImageUsageFlagBits 32

-- | Image data not needed outside of rendering
--
--   Bit position @6@
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT ::
        VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT =
        VkImageUsageFlagBits 64

-- | Can be used as framebuffer input attachment
--
--   Bit position @7@
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT :: VkImageUsageFlagBits

pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT =
        VkImageUsageFlagBits 128

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageViewType.html VkImageViewType registry at www.khronos.org>
newtype VkImageViewType = VkImageViewType VkFlags
                            deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSharingMode.html VkSharingMode registry at www.khronos.org>
newtype VkSharingMode = VkSharingMode VkFlags
                          deriving (Eq, Ord, Storable)

pattern VK_SHARING_MODE_EXCLUSIVE :: VkSharingMode

pattern VK_SHARING_MODE_EXCLUSIVE = VkSharingMode 0

pattern VK_SHARING_MODE_CONCURRENT :: VkSharingMode

pattern VK_SHARING_MODE_CONCURRENT = VkSharingMode 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkIndexType.html VkIndexType registry at www.khronos.org>
newtype VkIndexType = VkIndexType VkFlags
                        deriving (Eq, Ord, Storable)

pattern VK_INDEX_TYPE_UINT16 :: VkIndexType

pattern VK_INDEX_TYPE_UINT16 = VkIndexType 0

pattern VK_INDEX_TYPE_UINT32 :: VkIndexType

pattern VK_INDEX_TYPE_UINT32 = VkIndexType 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkLogicOp.html VkLogicOp registry at www.khronos.org>
newtype VkLogicOp = VkLogicOp VkFlags
                      deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryHeapFlagBits.html VkMemoryHeapFlagBits registry at www.khronos.org>
newtype VkMemoryHeapFlagBits = VkMemoryHeapFlagBits VkFlags
                                 deriving (Eq, Ord, Bits, Storable)

-- | If set, heap represents device memory
--
--   Bit position @0@
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT :: VkMemoryHeapFlagBits

pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapFlagBits 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAccessFlagBits.html VkAccessFlagBits registry at www.khronos.org>
newtype VkAccessFlagBits = VkAccessFlagBits VkFlags
                             deriving (Eq, Ord, Bits, Storable)

-- | Controls coherency of indirect command reads
--
--   Bit position @0@
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT = VkAccessFlagBits 1

-- | Controls coherency of index reads
--
--   Bit position @1@
pattern VK_ACCESS_INDEX_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_INDEX_READ_BIT = VkAccessFlagBits 2

-- | Controls coherency of vertex attribute reads
--
--   Bit position @2@
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VkAccessFlagBits 4

-- | Controls coherency of uniform buffer reads
--
--   Bit position @3@
pattern VK_ACCESS_UNIFORM_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_UNIFORM_READ_BIT = VkAccessFlagBits 8

-- | Controls coherency of input attachment reads
--
--   Bit position @4@
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = VkAccessFlagBits 16

-- | Controls coherency of shader reads
--
--   Bit position @5@
pattern VK_ACCESS_SHADER_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_SHADER_READ_BIT = VkAccessFlagBits 32

-- | Controls coherency of shader writes
--
--   Bit position @6@
pattern VK_ACCESS_SHADER_WRITE_BIT :: VkAccessFlagBits

pattern VK_ACCESS_SHADER_WRITE_BIT = VkAccessFlagBits 64

-- | Controls coherency of color attachment reads
--
--   Bit position @7@
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = VkAccessFlagBits 128

-- | Controls coherency of color attachment writes
--
--   Bit position @8@
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT :: VkAccessFlagBits

pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 256

-- | Controls coherency of depth/stencil attachment reads
--
--   Bit position @9@
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT ::
        VkAccessFlagBits

pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT =
        VkAccessFlagBits 512

-- | Controls coherency of depth/stencil attachment writes
--
--   Bit position @10@
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT ::
        VkAccessFlagBits

pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT =
        VkAccessFlagBits 1024

-- | Controls coherency of transfer reads
--
--   Bit position @11@
pattern VK_ACCESS_TRANSFER_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_TRANSFER_READ_BIT = VkAccessFlagBits 2048

-- | Controls coherency of transfer writes
--
--   Bit position @12@
pattern VK_ACCESS_TRANSFER_WRITE_BIT :: VkAccessFlagBits

pattern VK_ACCESS_TRANSFER_WRITE_BIT = VkAccessFlagBits 4096

-- | Controls coherency of host reads
--
--   Bit position @13@
pattern VK_ACCESS_HOST_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_HOST_READ_BIT = VkAccessFlagBits 8192

-- | Controls coherency of host writes
--
--   Bit position @14@
pattern VK_ACCESS_HOST_WRITE_BIT :: VkAccessFlagBits

pattern VK_ACCESS_HOST_WRITE_BIT = VkAccessFlagBits 16384

-- | Controls coherency of memory reads
--
--   Bit position @15@
pattern VK_ACCESS_MEMORY_READ_BIT :: VkAccessFlagBits

pattern VK_ACCESS_MEMORY_READ_BIT = VkAccessFlagBits 32768

-- | Controls coherency of memory writes
--
--   Bit position @16@
pattern VK_ACCESS_MEMORY_WRITE_BIT :: VkAccessFlagBits

pattern VK_ACCESS_MEMORY_WRITE_BIT = VkAccessFlagBits 65536

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryPropertyFlagBits.html VkMemoryPropertyFlagBits registry at www.khronos.org>
newtype VkMemoryPropertyFlagBits = VkMemoryPropertyFlagBits VkFlags
                                     deriving (Eq, Ord, Bits, Storable)

-- | If otherwise stated, then allocate memory on device
--
--   Bit position @0@
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT =
        VkMemoryPropertyFlagBits 1

-- | Memory is mappable by host
--
--   Bit position @1@
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT =
        VkMemoryPropertyFlagBits 2

-- | Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
--
--   Bit position @2@
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT =
        VkMemoryPropertyFlagBits 4

-- | Memory will be cached by the host
--
--   Bit position @3@
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT =
        VkMemoryPropertyFlagBits 8

-- | Memory may be allocated by the driver when it is required
--
--   Bit position @4@
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT =
        VkMemoryPropertyFlagBits 16

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceType.html VkPhysicalDeviceType registry at www.khronos.org>
newtype VkPhysicalDeviceType = VkPhysicalDeviceType VkFlags
                                 deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineBindPoint.html VkPipelineBindPoint registry at www.khronos.org>
newtype VkPipelineBindPoint = VkPipelineBindPoint VkFlags
                                deriving (Eq, Ord, Storable)

pattern VK_PIPELINE_BIND_POINT_GRAPHICS :: VkPipelineBindPoint

pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0

pattern VK_PIPELINE_BIND_POINT_COMPUTE :: VkPipelineBindPoint

pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineCreateFlagBits.html VkPipelineCreateFlagBits registry at www.khronos.org>
newtype VkPipelineCreateFlagBits = VkPipelineCreateFlagBits VkFlags
                                     deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT =
        VkPipelineCreateFlagBits 1

-- | Bit position @1@
pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT =
        VkPipelineCreateFlagBits 2

-- | Bit position @2@
pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT =
        VkPipelineCreateFlagBits 4

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPrimitiveTopology.html VkPrimitiveTopology registry at www.khronos.org>
newtype VkPrimitiveTopology = VkPrimitiveTopology VkFlags
                                deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueryControlFlagBits.html VkQueryControlFlagBits registry at www.khronos.org>
newtype VkQueryControlFlagBits = VkQueryControlFlagBits VkFlags
                                   deriving (Eq, Ord, Bits, Storable)

-- | Require precise results to be collected by the query
--
--   Bit position @0@
pattern VK_QUERY_CONTROL_PRECISE_BIT :: VkQueryControlFlagBits

pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlFlagBits 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueryPipelineStatisticFlagBits.html VkQueryPipelineStatisticFlagBits registry at www.khronos.org>
newtype VkQueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits VkFlags
                                             deriving (Eq, Ord, Bits, Storable)

-- | Optional
--
--   Bit position @0@
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT ::
        VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT =
        VkQueryPipelineStatisticFlagBits 1

-- | Optional
--
--   Bit position @1@
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT =
        VkQueryPipelineStatisticFlagBits 2

-- | Optional
--
--   Bit position @2@
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT =
        VkQueryPipelineStatisticFlagBits 4

-- | Optional
--
--   Bit position @3@
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticFlagBits 8

-- | Optional
--
--   Bit position @4@
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
        = VkQueryPipelineStatisticFlagBits 16

-- | Optional
--
--   Bit position @5@
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT ::
        VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT =
        VkQueryPipelineStatisticFlagBits 32

-- | Optional
--
--   Bit position @6@
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT ::
        VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT =
        VkQueryPipelineStatisticFlagBits 64

-- | Optional
--
--   Bit position @7@
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticFlagBits 128

-- | Optional
--
--   Bit position @8@
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
        = VkQueryPipelineStatisticFlagBits 256

-- | Optional
--
--   Bit position @9@
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticFlagBits 512

-- | Optional
--
--   Bit position @10@
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticFlagBits

pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticFlagBits 1024

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueryResultFlagBits.html VkQueryResultFlagBits registry at www.khronos.org>
newtype VkQueryResultFlagBits = VkQueryResultFlagBits VkFlags
                                  deriving (Eq, Ord, Bits, Storable)

-- | Results of the queries are written to the destination buffer as 64-bit values
--
--   Bit position @0@
pattern VK_QUERY_RESULT_64_BIT :: VkQueryResultFlagBits

pattern VK_QUERY_RESULT_64_BIT = VkQueryResultFlagBits 1

-- | Results of the queries are waited on before proceeding with the result copy
--
--   Bit position @1@
pattern VK_QUERY_RESULT_WAIT_BIT :: VkQueryResultFlagBits

pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultFlagBits 2

-- | Besides the results of the query, the availability of the results is also written
--
--   Bit position @2@
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT ::
        VkQueryResultFlagBits

pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT =
        VkQueryResultFlagBits 4

-- | Copy the partial results of the query even if the final results are not available
--
--   Bit position @3@
pattern VK_QUERY_RESULT_PARTIAL_BIT :: VkQueryResultFlagBits

pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultFlagBits 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueryType.html VkQueryType registry at www.khronos.org>
newtype VkQueryType = VkQueryType VkFlags
                        deriving (Eq, Ord, Storable)

pattern VK_QUERY_TYPE_OCCLUSION :: VkQueryType

pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0

-- | Optional
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS :: VkQueryType

pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1

pattern VK_QUERY_TYPE_TIMESTAMP :: VkQueryType

pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueueFlagBits.html VkQueueFlagBits registry at www.khronos.org>
newtype VkQueueFlagBits = VkQueueFlagBits VkFlags
                            deriving (Eq, Ord, Bits, Storable)

-- | Queue supports graphics operations
--
--   Bit position @0@
pattern VK_QUEUE_GRAPHICS_BIT :: VkQueueFlagBits

pattern VK_QUEUE_GRAPHICS_BIT = VkQueueFlagBits 1

-- | Queue supports compute operations
--
--   Bit position @1@
pattern VK_QUEUE_COMPUTE_BIT :: VkQueueFlagBits

pattern VK_QUEUE_COMPUTE_BIT = VkQueueFlagBits 2

-- | Queue supports transfer operations
--
--   Bit position @2@
pattern VK_QUEUE_TRANSFER_BIT :: VkQueueFlagBits

pattern VK_QUEUE_TRANSFER_BIT = VkQueueFlagBits 4

-- | Queue supports sparse resource memory management operations
--
--   Bit position @3@
pattern VK_QUEUE_SPARSE_BINDING_BIT :: VkQueueFlagBits

pattern VK_QUEUE_SPARSE_BINDING_BIT = VkQueueFlagBits 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubpassContents.html VkSubpassContents registry at www.khronos.org>
newtype VkSubpassContents = VkSubpassContents VkFlags
                              deriving (Eq, Ord, Storable)

pattern VK_SUBPASS_CONTENTS_INLINE :: VkSubpassContents

pattern VK_SUBPASS_CONTENTS_INLINE = VkSubpassContents 0

pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS ::
        VkSubpassContents

pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS =
        VkSubpassContents 1

-- | API result codes
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkResult.html VkResult registry at www.khronos.org>
newtype VkResult = VkResult VkFlags
                     deriving (Eq, Ord, Storable)

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

-- | A requested pool allocation has failed due to fragmentation of the pool's memory
pattern VK_ERROR_FRAGMENTED_POOL :: VkResult

pattern VK_ERROR_FRAGMENTED_POOL = VkResult (-12)

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkShaderStageFlagBits.html VkShaderStageFlagBits registry at www.khronos.org>
newtype VkShaderStageFlagBits = VkShaderStageFlagBits VkFlags
                                  deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_SHADER_STAGE_VERTEX_BIT :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_VERTEX_BIT = VkShaderStageFlagBits 1

-- | Bit position @1@
pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT ::
        VkShaderStageFlagBits

pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT =
        VkShaderStageFlagBits 2

-- | Bit position @2@
pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT ::
        VkShaderStageFlagBits

pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT =
        VkShaderStageFlagBits 4

-- | Bit position @3@
pattern VK_SHADER_STAGE_GEOMETRY_BIT :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_GEOMETRY_BIT = VkShaderStageFlagBits 8

-- | Bit position @4@
pattern VK_SHADER_STAGE_FRAGMENT_BIT :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_FRAGMENT_BIT = VkShaderStageFlagBits 16

-- | Bit position @5@
pattern VK_SHADER_STAGE_COMPUTE_BIT :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_COMPUTE_BIT = VkShaderStageFlagBits 32

-- | Bitmask value @0x1f@
pattern VK_SHADER_STAGE_ALL_GRAPHICS :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_ALL_GRAPHICS = VkShaderStageFlagBits 31

-- | Bitmask value @0x7fffffff@
pattern VK_SHADER_STAGE_ALL :: VkShaderStageFlagBits

pattern VK_SHADER_STAGE_ALL = VkShaderStageFlagBits 2147483647

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSparseMemoryBindFlagBits.html VkSparseMemoryBindFlagBits registry at www.khronos.org>
newtype VkSparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits VkFlags
                                       deriving (Eq, Ord, Bits, Storable)

-- | Operation binds resource metadata to memory
--
--   Bit position @0@
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT ::
        VkSparseMemoryBindFlagBits

pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT =
        VkSparseMemoryBindFlagBits 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkStencilFaceFlagBits.html VkStencilFaceFlagBits registry at www.khronos.org>
newtype VkStencilFaceFlagBits = VkStencilFaceFlagBits VkFlags
                                  deriving (Eq, Ord, Bits, Storable)

-- | Front face
--
--   Bit position @0@
pattern VK_STENCIL_FACE_FRONT_BIT :: VkStencilFaceFlagBits

pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceFlagBits 1

-- | Back face
--
--   Bit position @1@
pattern VK_STENCIL_FACE_BACK_BIT :: VkStencilFaceFlagBits

pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceFlagBits 2

-- | Front and back faces
--
--   Bitmask value @0x3@
pattern VK_STENCIL_FRONT_AND_BACK :: VkStencilFaceFlagBits

pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceFlagBits 3

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkStencilOp.html VkStencilOp registry at www.khronos.org>
newtype VkStencilOp = VkStencilOp VkFlags
                        deriving (Eq, Ord, Storable)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkStructureType.html VkStructureType registry at www.khronos.org>
newtype VkStructureType = VkStructureType VkFlags
                            deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSystemAllocationScope.html VkSystemAllocationScope registry at www.khronos.org>
newtype VkSystemAllocationScope = VkSystemAllocationScope VkFlags
                                    deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkInternalAllocationType.html VkInternalAllocationType registry at www.khronos.org>
newtype VkInternalAllocationType = VkInternalAllocationType VkFlags
                                     deriving (Eq, Ord, Storable)

pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE ::
        VkInternalAllocationType

pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE =
        VkInternalAllocationType 0

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerAddressMode.html VkSamplerAddressMode registry at www.khronos.org>
newtype VkSamplerAddressMode = VkSamplerAddressMode VkFlags
                                 deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFilter.html VkFilter registry at www.khronos.org>
newtype VkFilter = VkFilter VkFlags
                     deriving (Eq, Ord, Storable)

pattern VK_FILTER_NEAREST :: VkFilter

pattern VK_FILTER_NEAREST = VkFilter 0

pattern VK_FILTER_LINEAR :: VkFilter

pattern VK_FILTER_LINEAR = VkFilter 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerMipmapMode.html VkSamplerMipmapMode registry at www.khronos.org>
newtype VkSamplerMipmapMode = VkSamplerMipmapMode VkFlags
                                deriving (Eq, Ord, Storable)

-- | Choose nearest mip level
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST :: VkSamplerMipmapMode

pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0

-- | Linear filter between mip levels
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR :: VkSamplerMipmapMode

pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkVertexInputRate.html VkVertexInputRate registry at www.khronos.org>
newtype VkVertexInputRate = VkVertexInputRate VkFlags
                              deriving (Eq, Ord, Storable)

pattern VK_VERTEX_INPUT_RATE_VERTEX :: VkVertexInputRate

pattern VK_VERTEX_INPUT_RATE_VERTEX = VkVertexInputRate 0

pattern VK_VERTEX_INPUT_RATE_INSTANCE :: VkVertexInputRate

pattern VK_VERTEX_INPUT_RATE_INSTANCE = VkVertexInputRate 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineStageFlagBits.html VkPipelineStageFlagBits registry at www.khronos.org>
newtype VkPipelineStageFlagBits = VkPipelineStageFlagBits VkFlags
                                    deriving (Eq, Ord, Bits, Storable)

-- | Before subsequent commands are processed
--
--   Bit position @0@
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT =
        VkPipelineStageFlagBits 1

-- | Draw/DispatchIndirect command fetch
--
--   Bit position @1@
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT =
        VkPipelineStageFlagBits 2

-- | Vertex/index fetch
--
--   Bit position @2@
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT =
        VkPipelineStageFlagBits 4

-- | Vertex shading
--
--   Bit position @3@
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT =
        VkPipelineStageFlagBits 8

-- | Tessellation control shading
--
--   Bit position @4@
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT =
        VkPipelineStageFlagBits 16

-- | Tessellation evaluation shading
--
--   Bit position @5@
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT =
        VkPipelineStageFlagBits 32

-- | Geometry shading
--
--   Bit position @6@
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT =
        VkPipelineStageFlagBits 64

-- | Fragment shading
--
--   Bit position @7@
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT =
        VkPipelineStageFlagBits 128

-- | Early fragment (depth and stencil) tests
--
--   Bit position @8@
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT =
        VkPipelineStageFlagBits 256

-- | Late fragment (depth and stencil) tests
--
--   Bit position @9@
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT =
        VkPipelineStageFlagBits 512

-- | Color attachment writes
--
--   Bit position @10@
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT =
        VkPipelineStageFlagBits 1024

-- | Compute shading
--
--   Bit position @11@
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT =
        VkPipelineStageFlagBits 2048

-- | Transfer/copy operations
--
--   Bit position @12@
pattern VK_PIPELINE_STAGE_TRANSFER_BIT :: VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_TRANSFER_BIT =
        VkPipelineStageFlagBits 4096

-- | After previous commands have completed
--
--   Bit position @13@
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT =
        VkPipelineStageFlagBits 8192

-- | Indicates host (CPU) is a source/sink of the dependency
--
--   Bit position @14@
pattern VK_PIPELINE_STAGE_HOST_BIT :: VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_HOST_BIT = VkPipelineStageFlagBits 16384

-- | All stages of the graphics pipeline
--
--   Bit position @15@
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT =
        VkPipelineStageFlagBits 32768

-- | All stages supported on the queue
--
--   Bit position @16@
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT =
        VkPipelineStageFlagBits 65536

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSparseImageFormatFlagBits.html VkSparseImageFormatFlagBits registry at www.khronos.org>
newtype VkSparseImageFormatFlagBits = VkSparseImageFormatFlagBits VkFlags
                                        deriving (Eq, Ord, Bits, Storable)

-- | Image uses a single mip tail region for all array layers
--
--   Bit position @0@
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT ::
        VkSparseImageFormatFlagBits

pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT =
        VkSparseImageFormatFlagBits 1

-- | Image requires mip level dimensions to be an integer multiple of the sparse image block dimensions for non-tail mip levels.
--
--   Bit position @1@
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT ::
        VkSparseImageFormatFlagBits

pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT =
        VkSparseImageFormatFlagBits 2

-- | Image uses a non-standard sparse image block dimensions
--
--   Bit position @2@
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT ::
        VkSparseImageFormatFlagBits

pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT =
        VkSparseImageFormatFlagBits 4

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSampleCountFlagBits.html VkSampleCountFlagBits registry at www.khronos.org>
newtype VkSampleCountFlagBits = VkSampleCountFlagBits VkFlags
                                  deriving (Eq, Ord, Bits, Storable)

-- | Sample count 1 supported
--
--   Bit position @0@
pattern VK_SAMPLE_COUNT_1_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountFlagBits 1

-- | Sample count 2 supported
--
--   Bit position @1@
pattern VK_SAMPLE_COUNT_2_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountFlagBits 2

-- | Sample count 4 supported
--
--   Bit position @2@
pattern VK_SAMPLE_COUNT_4_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountFlagBits 4

-- | Sample count 8 supported
--
--   Bit position @3@
pattern VK_SAMPLE_COUNT_8_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountFlagBits 8

-- | Sample count 16 supported
--
--   Bit position @4@
pattern VK_SAMPLE_COUNT_16_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountFlagBits 16

-- | Sample count 32 supported
--
--   Bit position @5@
pattern VK_SAMPLE_COUNT_32_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountFlagBits 32

-- | Sample count 64 supported
--
--   Bit position @6@
pattern VK_SAMPLE_COUNT_64_BIT :: VkSampleCountFlagBits

pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountFlagBits 64

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentDescriptionFlagBits.html VkAttachmentDescriptionFlagBits registry at www.khronos.org>
newtype VkAttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits VkFlags
                                            deriving (Eq, Ord, Bits, Storable)

-- | The attachment may alias physical memory of another attachment in the same render pass
--
--   Bit position @0@
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT ::
        VkAttachmentDescriptionFlagBits

pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT =
        VkAttachmentDescriptionFlagBits 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorPoolCreateFlagBits.html VkDescriptorPoolCreateFlagBits registry at www.khronos.org>
newtype VkDescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits VkFlags
                                           deriving (Eq, Ord, Bits, Storable)

-- | Descriptor sets may be freed individually
--
--   Bit position @0@
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT ::
        VkDescriptorPoolCreateFlagBits

pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT =
        VkDescriptorPoolCreateFlagBits 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDependencyFlagBits.html VkDependencyFlagBits registry at www.khronos.org>
newtype VkDependencyFlagBits = VkDependencyFlagBits VkFlags
                                 deriving (Eq, Ord, Bits, Storable)

-- | Dependency is per pixel region
--
--   Bit position @0@
pattern VK_DEPENDENCY_BY_REGION_BIT :: VkDependencyFlagBits

pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyFlagBits 1

-- | Enums to track objects of various types
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectType.html VkObjectType registry at www.khronos.org>
newtype VkObjectType = VkObjectType VkFlags
                         deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkIndirectCommandsLayoutUsageFlagBitsNVX.html VkIndirectCommandsLayoutUsageFlagBitsNVX registry at www.khronos.org>
newtype VkIndirectCommandsLayoutUsageFlagBitsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX VkFlags
                                                     deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
        :: VkIndirectCommandsLayoutUsageFlagBitsNVX

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
        = VkIndirectCommandsLayoutUsageFlagBitsNVX 1

-- | Bit position @1@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
        :: VkIndirectCommandsLayoutUsageFlagBitsNVX

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
        = VkIndirectCommandsLayoutUsageFlagBitsNVX 2

-- | Bit position @2@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
        :: VkIndirectCommandsLayoutUsageFlagBitsNVX

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
        = VkIndirectCommandsLayoutUsageFlagBitsNVX 4

-- | Bit position @3@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
        :: VkIndirectCommandsLayoutUsageFlagBitsNVX

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
        = VkIndirectCommandsLayoutUsageFlagBitsNVX 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkIndirectCommandsTokenTypeNVX.html VkIndirectCommandsTokenTypeNVX registry at www.khronos.org>
newtype VkIndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX VkFlags
                                           deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectEntryUsageFlagBitsNVX.html VkObjectEntryUsageFlagBitsNVX registry at www.khronos.org>
newtype VkObjectEntryUsageFlagBitsNVX = VkObjectEntryUsageFlagBitsNVX VkFlags
                                          deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX ::
        VkObjectEntryUsageFlagBitsNVX

pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX =
        VkObjectEntryUsageFlagBitsNVX 1

-- | Bit position @1@
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX ::
        VkObjectEntryUsageFlagBitsNVX

pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX =
        VkObjectEntryUsageFlagBitsNVX 2

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectEntryTypeNVX.html VkObjectEntryTypeNVX registry at www.khronos.org>
newtype VkObjectEntryTypeNVX = VkObjectEntryTypeNVX VkFlags
                                 deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorUpdateTemplateTypeKHR.html VkDescriptorUpdateTemplateTypeKHR registry at www.khronos.org>
newtype VkDescriptorUpdateTemplateTypeKHR = VkDescriptorUpdateTemplateTypeKHR VkFlags
                                              deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkViewportCoordinateSwizzleNV.html VkViewportCoordinateSwizzleNV registry at www.khronos.org>
newtype VkViewportCoordinateSwizzleNV = VkViewportCoordinateSwizzleNV VkFlags
                                          deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDiscardRectangleModeEXT.html VkDiscardRectangleModeEXT registry at www.khronos.org>
newtype VkDiscardRectangleModeEXT = VkDiscardRectangleModeEXT VkFlags
                                      deriving (Eq, Ord, Storable)

pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT ::
        VkDiscardRectangleModeEXT

pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT =
        VkDiscardRectangleModeEXT 0

pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT ::
        VkDiscardRectangleModeEXT

pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT =
        VkDiscardRectangleModeEXT 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubpassDescriptionFlagBits.html VkSubpassDescriptionFlagBits registry at www.khronos.org>
newtype VkSubpassDescriptionFlagBits = VkSubpassDescriptionFlagBits VkFlags
                                         deriving (Eq, Ord, Bits, Storable)

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPointClippingBehaviorKHR.html VkPointClippingBehaviorKHR registry at www.khronos.org>
newtype VkPointClippingBehaviorKHR = VkPointClippingBehaviorKHR VkFlags
                                       deriving (Eq, Ord, Storable)

pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR ::
        VkPointClippingBehaviorKHR

pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR =
        VkPointClippingBehaviorKHR 0

pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR ::
        VkPointClippingBehaviorKHR

pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR =
        VkPointClippingBehaviorKHR 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCoverageModulationModeNV.html VkCoverageModulationModeNV registry at www.khronos.org>
newtype VkCoverageModulationModeNV = VkCoverageModulationModeNV VkFlags
                                       deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkValidationCacheHeaderVersionEXT.html VkValidationCacheHeaderVersionEXT registry at www.khronos.org>
newtype VkValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT VkFlags
                                              deriving (Eq, Ord, Storable)

pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT ::
        VkValidationCacheHeaderVersionEXT

pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT =
        VkValidationCacheHeaderVersionEXT 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkShaderInfoTypeAMD.html VkShaderInfoTypeAMD registry at www.khronos.org>
newtype VkShaderInfoTypeAMD = VkShaderInfoTypeAMD VkFlags
                                deriving (Eq, Ord, Storable)

pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD = VkShaderInfoTypeAMD 0

pattern VK_SHADER_INFO_TYPE_BINARY_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_BINARY_AMD = VkShaderInfoTypeAMD 1

pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD = VkShaderInfoTypeAMD 2

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueueGlobalPriorityEXT.html VkQueueGlobalPriorityEXT registry at www.khronos.org>
newtype VkQueueGlobalPriorityEXT = VkQueueGlobalPriorityEXT VkFlags
                                     deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkConservativeRasterizationModeEXT.html VkConservativeRasterizationModeEXT registry at www.khronos.org>
newtype VkConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT VkFlags
                                               deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkColorSpaceKHR.html VkColorSpaceKHR registry at www.khronos.org>
newtype VkColorSpaceKHR = VkColorSpaceKHR VkFlags
                            deriving (Eq, Ord, Storable)

pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCompositeAlphaFlagBitsKHR.html VkCompositeAlphaFlagBitsKHR registry at www.khronos.org>
newtype VkCompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR VkFlags
                                        deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR ::
        VkCompositeAlphaFlagBitsKHR

pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR =
        VkCompositeAlphaFlagBitsKHR 1

-- | Bit position @1@
pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR ::
        VkCompositeAlphaFlagBitsKHR

pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR =
        VkCompositeAlphaFlagBitsKHR 2

-- | Bit position @2@
pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR ::
        VkCompositeAlphaFlagBitsKHR

pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR =
        VkCompositeAlphaFlagBitsKHR 4

-- | Bit position @3@
pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR ::
        VkCompositeAlphaFlagBitsKHR

pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR =
        VkCompositeAlphaFlagBitsKHR 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayPlaneAlphaFlagBitsKHR.html VkDisplayPlaneAlphaFlagBitsKHR registry at www.khronos.org>
newtype VkDisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR VkFlags
                                           deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR ::
        VkDisplayPlaneAlphaFlagBitsKHR

pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR =
        VkDisplayPlaneAlphaFlagBitsKHR 1

-- | Bit position @1@
pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR ::
        VkDisplayPlaneAlphaFlagBitsKHR

pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR =
        VkDisplayPlaneAlphaFlagBitsKHR 2

-- | Bit position @2@
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR ::
        VkDisplayPlaneAlphaFlagBitsKHR

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR =
        VkDisplayPlaneAlphaFlagBitsKHR 4

-- | Bit position @3@
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR ::
        VkDisplayPlaneAlphaFlagBitsKHR

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR =
        VkDisplayPlaneAlphaFlagBitsKHR 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPresentModeKHR.html VkPresentModeKHR registry at www.khronos.org>
newtype VkPresentModeKHR = VkPresentModeKHR VkFlags
                             deriving (Eq, Ord, Storable)

pattern VK_PRESENT_MODE_IMMEDIATE_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_IMMEDIATE_KHR = VkPresentModeKHR 0

pattern VK_PRESENT_MODE_MAILBOX_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_MAILBOX_KHR = VkPresentModeKHR 1

pattern VK_PRESENT_MODE_FIFO_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_FIFO_KHR = VkPresentModeKHR 2

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = VkPresentModeKHR 3

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSurfaceTransformFlagBitsKHR.html VkSurfaceTransformFlagBitsKHR registry at www.khronos.org>
newtype VkSurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR VkFlags
                                          deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 1

-- | Bit position @1@
pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 2

-- | Bit position @2@
pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 4

-- | Bit position @3@
pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 8

-- | Bit position @4@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 16

-- | Bit position @5@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 32

-- | Bit position @6@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
        :: VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 64

-- | Bit position @7@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
        :: VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 128

-- | Bit position @8@
pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR ::
        VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR =
        VkSurfaceTransformFlagBitsKHR 256

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDebugReportFlagBitsEXT.html VkDebugReportFlagBitsEXT registry at www.khronos.org>
newtype VkDebugReportFlagBitsEXT = VkDebugReportFlagBitsEXT VkFlags
                                     deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT ::
        VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT =
        VkDebugReportFlagBitsEXT 1

-- | Bit position @1@
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT :: VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_WARNING_BIT_EXT =
        VkDebugReportFlagBitsEXT 2

-- | Bit position @2@
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT ::
        VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT =
        VkDebugReportFlagBitsEXT 4

-- | Bit position @3@
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT :: VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_ERROR_BIT_EXT = VkDebugReportFlagBitsEXT 8

-- | Bit position @4@
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT :: VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT = VkDebugReportFlagBitsEXT 16

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDebugReportObjectTypeEXT.html VkDebugReportObjectTypeEXT registry at www.khronos.org>
newtype VkDebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT VkFlags
                                       deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkRasterizationOrderAMD.html VkRasterizationOrderAMD registry at www.khronos.org>
newtype VkRasterizationOrderAMD = VkRasterizationOrderAMD VkFlags
                                    deriving (Eq, Ord, Storable)

pattern VK_RASTERIZATION_ORDER_STRICT_AMD ::
        VkRasterizationOrderAMD

pattern VK_RASTERIZATION_ORDER_STRICT_AMD =
        VkRasterizationOrderAMD 0

pattern VK_RASTERIZATION_ORDER_RELAXED_AMD ::
        VkRasterizationOrderAMD

pattern VK_RASTERIZATION_ORDER_RELAXED_AMD =
        VkRasterizationOrderAMD 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryHandleTypeFlagBitsNV.html VkExternalMemoryHandleTypeFlagBitsNV registry at www.khronos.org>
newtype VkExternalMemoryHandleTypeFlagBitsNV = VkExternalMemoryHandleTypeFlagBitsNV VkFlags
                                                 deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV ::
        VkExternalMemoryHandleTypeFlagBitsNV

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV =
        VkExternalMemoryHandleTypeFlagBitsNV 1

-- | Bit position @1@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV ::
        VkExternalMemoryHandleTypeFlagBitsNV

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV =
        VkExternalMemoryHandleTypeFlagBitsNV 2

-- | Bit position @2@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV ::
        VkExternalMemoryHandleTypeFlagBitsNV

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV =
        VkExternalMemoryHandleTypeFlagBitsNV 4

-- | Bit position @3@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV ::
        VkExternalMemoryHandleTypeFlagBitsNV

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV =
        VkExternalMemoryHandleTypeFlagBitsNV 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryFeatureFlagBitsNV.html VkExternalMemoryFeatureFlagBitsNV registry at www.khronos.org>
newtype VkExternalMemoryFeatureFlagBitsNV = VkExternalMemoryFeatureFlagBitsNV VkFlags
                                              deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV ::
        VkExternalMemoryFeatureFlagBitsNV

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV =
        VkExternalMemoryFeatureFlagBitsNV 1

-- | Bit position @1@
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV ::
        VkExternalMemoryFeatureFlagBitsNV

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV =
        VkExternalMemoryFeatureFlagBitsNV 2

-- | Bit position @2@
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV ::
        VkExternalMemoryFeatureFlagBitsNV

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV =
        VkExternalMemoryFeatureFlagBitsNV 4

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkValidationCheckEXT.html VkValidationCheckEXT registry at www.khronos.org>
newtype VkValidationCheckEXT = VkValidationCheckEXT VkFlags
                                 deriving (Eq, Ord, Storable)

pattern VK_VALIDATION_CHECK_ALL_EXT :: VkValidationCheckEXT

pattern VK_VALIDATION_CHECK_ALL_EXT = VkValidationCheckEXT 0

pattern VK_VALIDATION_CHECK_SHADERS_EXT :: VkValidationCheckEXT

pattern VK_VALIDATION_CHECK_SHADERS_EXT = VkValidationCheckEXT 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryHandleTypeFlagBitsKHR.html VkExternalMemoryHandleTypeFlagBitsKHR registry at www.khronos.org>
newtype VkExternalMemoryHandleTypeFlagBitsKHR = VkExternalMemoryHandleTypeFlagBitsKHR VkFlags
                                                  deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 1

-- | Bit position @1@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 2

-- | Bit position @2@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 4

-- | Bit position @3@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 8

-- | Bit position @4@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 16

-- | Bit position @5@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 32

-- | Bit position @6@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR =
        VkExternalMemoryHandleTypeFlagBitsKHR 64

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryFeatureFlagBitsKHR.html VkExternalMemoryFeatureFlagBitsKHR registry at www.khronos.org>
newtype VkExternalMemoryFeatureFlagBitsKHR = VkExternalMemoryFeatureFlagBitsKHR VkFlags
                                               deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR ::
        VkExternalMemoryFeatureFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR =
        VkExternalMemoryFeatureFlagBitsKHR 1

-- | Bit position @1@
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR ::
        VkExternalMemoryFeatureFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR =
        VkExternalMemoryFeatureFlagBitsKHR 2

-- | Bit position @2@
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR ::
        VkExternalMemoryFeatureFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR =
        VkExternalMemoryFeatureFlagBitsKHR 4

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalSemaphoreHandleTypeFlagBitsKHR.html VkExternalSemaphoreHandleTypeFlagBitsKHR registry at www.khronos.org>
newtype VkExternalSemaphoreHandleTypeFlagBitsKHR = VkExternalSemaphoreHandleTypeFlagBitsKHR VkFlags
                                                     deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR ::
        VkExternalSemaphoreHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VkExternalSemaphoreHandleTypeFlagBitsKHR 1

-- | Bit position @1@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR ::
        VkExternalSemaphoreHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VkExternalSemaphoreHandleTypeFlagBitsKHR 2

-- | Bit position @2@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
        :: VkExternalSemaphoreHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
        = VkExternalSemaphoreHandleTypeFlagBitsKHR 4

-- | Bit position @3@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR ::
        VkExternalSemaphoreHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR =
        VkExternalSemaphoreHandleTypeFlagBitsKHR 8

-- | Bit position @4@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR ::
        VkExternalSemaphoreHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR =
        VkExternalSemaphoreHandleTypeFlagBitsKHR 16

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalSemaphoreFeatureFlagBitsKHR.html VkExternalSemaphoreFeatureFlagBitsKHR registry at www.khronos.org>
newtype VkExternalSemaphoreFeatureFlagBitsKHR = VkExternalSemaphoreFeatureFlagBitsKHR VkFlags
                                                  deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR ::
        VkExternalSemaphoreFeatureFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR =
        VkExternalSemaphoreFeatureFlagBitsKHR 1

-- | Bit position @1@
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR ::
        VkExternalSemaphoreFeatureFlagBitsKHR

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR =
        VkExternalSemaphoreFeatureFlagBitsKHR 2

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSemaphoreImportFlagBitsKHR.html VkSemaphoreImportFlagBitsKHR registry at www.khronos.org>
newtype VkSemaphoreImportFlagBitsKHR = VkSemaphoreImportFlagBitsKHR VkFlags
                                         deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR ::
        VkSemaphoreImportFlagBitsKHR

pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR =
        VkSemaphoreImportFlagBitsKHR 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalFenceHandleTypeFlagBitsKHR.html VkExternalFenceHandleTypeFlagBitsKHR registry at www.khronos.org>
newtype VkExternalFenceHandleTypeFlagBitsKHR = VkExternalFenceHandleTypeFlagBitsKHR VkFlags
                                                 deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR ::
        VkExternalFenceHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VkExternalFenceHandleTypeFlagBitsKHR 1

-- | Bit position @1@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR ::
        VkExternalFenceHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VkExternalFenceHandleTypeFlagBitsKHR 2

-- | Bit position @2@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR ::
        VkExternalFenceHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR =
        VkExternalFenceHandleTypeFlagBitsKHR 4

-- | Bit position @3@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR ::
        VkExternalFenceHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR =
        VkExternalFenceHandleTypeFlagBitsKHR 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalFenceFeatureFlagBitsKHR.html VkExternalFenceFeatureFlagBitsKHR registry at www.khronos.org>
newtype VkExternalFenceFeatureFlagBitsKHR = VkExternalFenceFeatureFlagBitsKHR VkFlags
                                              deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR ::
        VkExternalFenceFeatureFlagBitsKHR

pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR =
        VkExternalFenceFeatureFlagBitsKHR 1

-- | Bit position @1@
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR ::
        VkExternalFenceFeatureFlagBitsKHR

pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR =
        VkExternalFenceFeatureFlagBitsKHR 2

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFenceImportFlagBitsKHR.html VkFenceImportFlagBitsKHR registry at www.khronos.org>
newtype VkFenceImportFlagBitsKHR = VkFenceImportFlagBitsKHR VkFlags
                                     deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR ::
        VkFenceImportFlagBitsKHR

pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR =
        VkFenceImportFlagBitsKHR 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSurfaceCounterFlagBitsEXT.html VkSurfaceCounterFlagBitsEXT registry at www.khronos.org>
newtype VkSurfaceCounterFlagBitsEXT = VkSurfaceCounterFlagBitsEXT VkFlags
                                        deriving (Eq, Ord, Bits, Storable)

-- | Bit position @0@
pattern VK_SURFACE_COUNTER_VBLANK_EXT ::
        VkSurfaceCounterFlagBitsEXT

pattern VK_SURFACE_COUNTER_VBLANK_EXT =
        VkSurfaceCounterFlagBitsEXT 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayPowerStateEXT.html VkDisplayPowerStateEXT registry at www.khronos.org>
newtype VkDisplayPowerStateEXT = VkDisplayPowerStateEXT VkFlags
                                   deriving (Eq, Ord, Storable)

pattern VK_DISPLAY_POWER_STATE_OFF_EXT :: VkDisplayPowerStateEXT

pattern VK_DISPLAY_POWER_STATE_OFF_EXT = VkDisplayPowerStateEXT 0

pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT ::
        VkDisplayPowerStateEXT

pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT =
        VkDisplayPowerStateEXT 1

pattern VK_DISPLAY_POWER_STATE_ON_EXT :: VkDisplayPowerStateEXT

pattern VK_DISPLAY_POWER_STATE_ON_EXT = VkDisplayPowerStateEXT 2

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceEventTypeEXT.html VkDeviceEventTypeEXT registry at www.khronos.org>
newtype VkDeviceEventTypeEXT = VkDeviceEventTypeEXT VkFlags
                                 deriving (Eq, Ord, Storable)

pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT ::
        VkDeviceEventTypeEXT

pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT =
        VkDeviceEventTypeEXT 0

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayEventTypeEXT.html VkDisplayEventTypeEXT registry at www.khronos.org>
newtype VkDisplayEventTypeEXT = VkDisplayEventTypeEXT VkFlags
                                  deriving (Eq, Ord, Storable)

pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT ::
        VkDisplayEventTypeEXT

pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT =
        VkDisplayEventTypeEXT 0

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPeerMemoryFeatureFlagBitsKHX.html VkPeerMemoryFeatureFlagBitsKHX registry at www.khronos.org>
newtype VkPeerMemoryFeatureFlagBitsKHX = VkPeerMemoryFeatureFlagBitsKHX VkFlags
                                           deriving (Eq, Ord, Bits, Storable)

-- | Can read with vkCmdCopy commands
--
--   Bit position @0@
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX ::
        VkPeerMemoryFeatureFlagBitsKHX

pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX =
        VkPeerMemoryFeatureFlagBitsKHX 1

-- | Can write with vkCmdCopy commands
--
--   Bit position @1@
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX ::
        VkPeerMemoryFeatureFlagBitsKHX

pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX =
        VkPeerMemoryFeatureFlagBitsKHX 2

-- | Can read with any access type/command
--
--   Bit position @2@
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX ::
        VkPeerMemoryFeatureFlagBitsKHX

pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX =
        VkPeerMemoryFeatureFlagBitsKHX 4

-- | Can write with and access type/command
--
--   Bit position @3@
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX ::
        VkPeerMemoryFeatureFlagBitsKHX

pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX =
        VkPeerMemoryFeatureFlagBitsKHX 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryAllocateFlagBitsKHX.html VkMemoryAllocateFlagBitsKHX registry at www.khronos.org>
newtype VkMemoryAllocateFlagBitsKHX = VkMemoryAllocateFlagBitsKHX VkFlags
                                        deriving (Eq, Ord, Bits, Storable)

-- | Force allocation on specific devices
--
--   Bit position @0@
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX ::
        VkMemoryAllocateFlagBitsKHX

pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX =
        VkMemoryAllocateFlagBitsKHX 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGroupPresentModeFlagBitsKHX.html VkDeviceGroupPresentModeFlagBitsKHX registry at www.khronos.org>
newtype VkDeviceGroupPresentModeFlagBitsKHX = VkDeviceGroupPresentModeFlagBitsKHX VkFlags
                                                deriving (Eq, Ord, Bits, Storable)

-- | Present from local memory
--
--   Bit position @0@
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX ::
        VkDeviceGroupPresentModeFlagBitsKHX

pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX =
        VkDeviceGroupPresentModeFlagBitsKHX 1

-- | Present from remote memory
--
--   Bit position @1@
pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX ::
        VkDeviceGroupPresentModeFlagBitsKHX

pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX =
        VkDeviceGroupPresentModeFlagBitsKHX 2

-- | Present sum of local and/or remote memory
--
--   Bit position @2@
pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX ::
        VkDeviceGroupPresentModeFlagBitsKHX

pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX =
        VkDeviceGroupPresentModeFlagBitsKHX 4

-- | Each physical device presents from local memory
--
--   Bit position @3@
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX ::
        VkDeviceGroupPresentModeFlagBitsKHX

pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX =
        VkDeviceGroupPresentModeFlagBitsKHX 8

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSwapchainCreateFlagBitsKHR.html VkSwapchainCreateFlagBitsKHR registry at www.khronos.org>
newtype VkSwapchainCreateFlagBitsKHR = VkSwapchainCreateFlagBitsKHR VkFlags
                                         deriving (Eq, Ord, Bits, Storable)

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkTessellationDomainOriginKHR.html VkTessellationDomainOriginKHR registry at www.khronos.org>
newtype VkTessellationDomainOriginKHR = VkTessellationDomainOriginKHR VkFlags
                                          deriving (Eq, Ord, Storable)

pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR ::
        VkTessellationDomainOriginKHR

pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR =
        VkTessellationDomainOriginKHR 0

pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR ::
        VkTessellationDomainOriginKHR

pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR =
        VkTessellationDomainOriginKHR 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerYcbcrModelConversionKHR.html VkSamplerYcbcrModelConversionKHR registry at www.khronos.org>
newtype VkSamplerYcbcrModelConversionKHR = VkSamplerYcbcrModelConversionKHR VkFlags
                                             deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerYcbcrRangeKHR.html VkSamplerYcbcrRangeKHR registry at www.khronos.org>
newtype VkSamplerYcbcrRangeKHR = VkSamplerYcbcrRangeKHR VkFlags
                                   deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkChromaLocationKHR.html VkChromaLocationKHR registry at www.khronos.org>
newtype VkChromaLocationKHR = VkChromaLocationKHR VkFlags
                                deriving (Eq, Ord, Storable)

pattern VK_CHROMA_LOCATION_COSITED_EVEN_KHR :: VkChromaLocationKHR

pattern VK_CHROMA_LOCATION_COSITED_EVEN_KHR = VkChromaLocationKHR 0

pattern VK_CHROMA_LOCATION_MIDPOINT_KHR :: VkChromaLocationKHR

pattern VK_CHROMA_LOCATION_MIDPOINT_KHR = VkChromaLocationKHR 1

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerReductionModeEXT.html VkSamplerReductionModeEXT registry at www.khronos.org>
newtype VkSamplerReductionModeEXT = VkSamplerReductionModeEXT VkFlags
                                      deriving (Eq, Ord, Storable)

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

-- | <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBlendOverlapEXT.html VkBlendOverlapEXT registry at www.khronos.org>
newtype VkBlendOverlapEXT = VkBlendOverlapEXT VkFlags
                              deriving (Eq, Ord, Storable)

pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT = VkBlendOverlapEXT 0

pattern VK_BLEND_OVERLAP_DISJOINT_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_DISJOINT_EXT = VkBlendOverlapEXT 1

pattern VK_BLEND_OVERLAP_CONJOINT_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_CONJOINT_EXT = VkBlendOverlapEXT 2

type HS_vkInternalAllocationNotification =
     Foreign.Ptr.Ptr Data.Void.Void ->
       Foreign.C.Types.CSize ->
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
     Foreign.Ptr.Ptr Data.Void.Void ->
       Foreign.C.Types.CSize ->
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
     Foreign.Ptr.Ptr Data.Void.Void ->
       Foreign.Ptr.Ptr Data.Void.Void ->
         Foreign.C.Types.CSize ->
           Foreign.C.Types.CSize ->
             VkSystemAllocationScope -> IO (Foreign.Ptr.Ptr Data.Void.Void)

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
     Foreign.Ptr.Ptr Data.Void.Void ->
       Foreign.C.Types.CSize ->
         Foreign.C.Types.CSize ->
           VkSystemAllocationScope -> IO (Foreign.Ptr.Ptr Data.Void.Void)

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

type HS_vkFreeFunction =
     Foreign.Ptr.Ptr Data.Void.Void ->
       Foreign.Ptr.Ptr Data.Void.Void -> IO ()

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
         Data.Word.Word64 ->
           Foreign.C.Types.CSize ->
             Data.Int.Int32 ->
               Foreign.Ptr.Ptr Foreign.C.Types.CChar ->
                 Foreign.Ptr.Ptr Foreign.C.Types.CChar ->
                   Foreign.Ptr.Ptr Data.Void.Void -> IO VkBool32

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
