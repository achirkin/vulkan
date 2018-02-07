{-# LANGUAGE CPP #-}
module Graphics.Vulkan.Ext
    (
      module Graphics.Vulkan.Ext.VK_KHR_surface
    , module Graphics.Vulkan.Ext.VK_KHR_swapchain
    , module Graphics.Vulkan.Ext.VK_KHR_display
    , module Graphics.Vulkan.Ext.VK_KHR_display_swapchain
#ifdef VK_USE_PLATFORM_XLIB_KHR
    , module Graphics.Vulkan.Ext.VK_KHR_xlib_surface
#endif

#ifdef VK_USE_PLATFORM_XCB_KHR
    , module Graphics.Vulkan.Ext.VK_KHR_xcb_surface
#endif

#ifdef VK_USE_PLATFORM_WAYLAND_KHR
    , module Graphics.Vulkan.Ext.VK_KHR_wayland_surface
#endif

#ifdef VK_USE_PLATFORM_MIR_KHR
    , module Graphics.Vulkan.Ext.VK_KHR_mir_surface
#endif

#ifdef VK_USE_PLATFORM_ANDROID_KHR
    , module Graphics.Vulkan.Ext.VK_KHR_android_surface
#endif

#ifdef VK_USE_PLATFORM_WIN32_KHR
    , module Graphics.Vulkan.Ext.VK_KHR_win32_surface
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_ANDROID_native_buffer
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_debug_report
    , module Graphics.Vulkan.Ext.VK_NV_glsl_shader
    , module Graphics.Vulkan.Ext.VK_EXT_depth_range_unrestricted
    , module Graphics.Vulkan.Ext.VK_KHR_sampler_mirror_clamp_to_edge
    , module Graphics.Vulkan.Ext.VK_IMG_filter_cubic
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_17
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_18
#endif

    , module Graphics.Vulkan.Ext.VK_AMD_rasterization_order
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_20
#endif

    , module Graphics.Vulkan.Ext.VK_AMD_shader_trinary_minmax
    , module Graphics.Vulkan.Ext.VK_AMD_shader_explicit_vertex_parameter
    , module Graphics.Vulkan.Ext.VK_EXT_debug_marker
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_24
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_25
#endif

    , module Graphics.Vulkan.Ext.VK_AMD_gcn_shader
    , module Graphics.Vulkan.Ext.VK_NV_dedicated_allocation
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_EXT_extension_28
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NVX_extension_29
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NVX_extension_30
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NVX_extension_31
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_32
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_33
#endif

    , module Graphics.Vulkan.Ext.VK_AMD_draw_indirect_count
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_35
#endif

    , module Graphics.Vulkan.Ext.VK_AMD_negative_viewport_height
    , module Graphics.Vulkan.Ext.VK_AMD_gpu_shader_half_float
    , module Graphics.Vulkan.Ext.VK_AMD_shader_ballot
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_39
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_40
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_41
#endif

    , module Graphics.Vulkan.Ext.VK_AMD_texture_gather_bias_lod
    , module Graphics.Vulkan.Ext.VK_AMD_shader_info
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_44
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_45
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_46
#endif

    , module Graphics.Vulkan.Ext.VK_AMD_shader_image_load_store_lod
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NVX_extension_48
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_GOOGLE_extension_49
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_GOOGLE_extension_50
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NVX_extension_51
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NVX_extension_52
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_53
#endif

    , module Graphics.Vulkan.Ext.VK_KHX_multiview
    , module Graphics.Vulkan.Ext.VK_IMG_format_pvrtc
    , module Graphics.Vulkan.Ext.VK_NV_external_memory_capabilities
    , module Graphics.Vulkan.Ext.VK_NV_external_memory
#ifdef VK_USE_PLATFORM_WIN32_KHR
    , module Graphics.Vulkan.Ext.VK_NV_external_memory_win32
#endif

#ifdef VK_USE_PLATFORM_WIN32_KHR
    , module Graphics.Vulkan.Ext.VK_NV_win32_keyed_mutex
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2
    , module Graphics.Vulkan.Ext.VK_KHX_device_group
    , module Graphics.Vulkan.Ext.VK_EXT_validation_flags
#ifdef VK_USE_PLATFORM_VI_NN
    , module Graphics.Vulkan.Ext.VK_NN_vi_surface
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_shader_draw_parameters
    , module Graphics.Vulkan.Ext.VK_EXT_shader_subgroup_ballot
    , module Graphics.Vulkan.Ext.VK_EXT_shader_subgroup_vote
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_ARM_extension_01
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_ARM_extension_02
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_IMG_extension_69
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_maintenance1
    , module Graphics.Vulkan.Ext.VK_KHX_device_group_creation
    , module Graphics.Vulkan.Ext.VK_KHR_external_memory_capabilities
    , module Graphics.Vulkan.Ext.VK_KHR_external_memory
#ifdef VK_USE_PLATFORM_WIN32_KHR
    , module Graphics.Vulkan.Ext.VK_KHR_external_memory_win32
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_external_memory_fd
#ifdef VK_USE_PLATFORM_WIN32_KHR
    , module Graphics.Vulkan.Ext.VK_KHR_win32_keyed_mutex
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_external_semaphore_capabilities
    , module Graphics.Vulkan.Ext.VK_KHR_external_semaphore
#ifdef VK_USE_PLATFORM_WIN32_KHR
    , module Graphics.Vulkan.Ext.VK_KHR_external_semaphore_win32
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_external_semaphore_fd
    , module Graphics.Vulkan.Ext.VK_KHR_push_descriptor
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_KHR_extension_82
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_KHR_extension_83
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_16bit_storage
    , module Graphics.Vulkan.Ext.VK_KHR_incremental_present
    , module Graphics.Vulkan.Ext.VK_KHR_descriptor_update_template
    , module Graphics.Vulkan.Ext.VK_NVX_device_generated_commands
    , module Graphics.Vulkan.Ext.VK_NV_clip_space_w_scaling
    , module Graphics.Vulkan.Ext.VK_EXT_direct_mode_display
#ifdef VK_USE_PLATFORM_XLIB_XRANDR_EXT
    , module Graphics.Vulkan.Ext.VK_EXT_acquire_xlib_display
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_display_surface_counter
    , module Graphics.Vulkan.Ext.VK_EXT_display_control
    , module Graphics.Vulkan.Ext.VK_GOOGLE_display_timing
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_KHR_extension_94
#endif

    , module Graphics.Vulkan.Ext.VK_NV_sample_mask_override_coverage
    , module Graphics.Vulkan.Ext.VK_NV_geometry_shader_passthrough
    , module Graphics.Vulkan.Ext.VK_NV_viewport_array2
    , module Graphics.Vulkan.Ext.VK_NVX_multiview_per_view_attributes
    , module Graphics.Vulkan.Ext.VK_NV_viewport_swizzle
    , module Graphics.Vulkan.Ext.VK_EXT_discard_rectangles
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_101
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_conservative_rasterization
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_103
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_104
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_swapchain_colorspace
    , module Graphics.Vulkan.Ext.VK_EXT_hdr_metadata
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_IMG_extension_107
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_IMG_extension_108
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_IMG_extension_109
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_IMG_extension_110
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_IMG_extension_111
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_shared_presentable_image
    , module Graphics.Vulkan.Ext.VK_KHR_external_fence_capabilities
    , module Graphics.Vulkan.Ext.VK_KHR_external_fence
#ifdef VK_USE_PLATFORM_WIN32_KHR
    , module Graphics.Vulkan.Ext.VK_KHR_external_fence_win32
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_external_fence_fd
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_KHR_extension_117
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_maintenance2
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_KHR_extension_119
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_get_surface_capabilities2
    , module Graphics.Vulkan.Ext.VK_KHR_variable_pointers
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_KHR_extension_122
#endif

#ifdef VK_USE_PLATFORM_IOS_MVK
    , module Graphics.Vulkan.Ext.VK_MVK_ios_surface
#endif

#ifdef VK_USE_PLATFORM_MACOS_MVK
    , module Graphics.Vulkan.Ext.VK_MVK_macos_surface
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_MVK_moltenvk
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_external_memory_dma_buf
    , module Graphics.Vulkan.Ext.VK_EXT_queue_family_foreign
    , module Graphics.Vulkan.Ext.VK_KHR_dedicated_allocation
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_EXT_extension_129
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_KHR_extension_130
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_sampler_filter_minmax
    , module Graphics.Vulkan.Ext.VK_KHR_storage_buffer_storage_class
    , module Graphics.Vulkan.Ext.VK_AMD_gpu_shader_int16
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_134
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_135
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_136
#endif

    , module Graphics.Vulkan.Ext.VK_AMD_mixed_attachment_samples
    , module Graphics.Vulkan.Ext.VK_AMD_shader_fragment_mask
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_139
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_140
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_shader_stencil_export
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_142
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_143
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_sample_locations
    , module Graphics.Vulkan.Ext.VK_KHR_relaxed_block_layout
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_KHR_extension_146
#endif

    , module Graphics.Vulkan.Ext.VK_KHR_get_memory_requirements2
    , module Graphics.Vulkan.Ext.VK_KHR_image_format_list
    , module Graphics.Vulkan.Ext.VK_EXT_blend_operation_advanced
    , module Graphics.Vulkan.Ext.VK_NV_fragment_coverage_to_color
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_151
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_152
#endif

    , module Graphics.Vulkan.Ext.VK_NV_framebuffer_mixed_samples
    , module Graphics.Vulkan.Ext.VK_NV_fill_rectangle
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_155
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_post_depth_coverage
    , module Graphics.Vulkan.Ext.VK_KHR_sampler_ycbcr_conversion
    , module Graphics.Vulkan.Ext.VK_KHR_bind_memory2
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_EXT_extension_159
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_EXT_extension_160
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_validation_cache
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_EXT_extension_162
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_shader_viewport_index_layer
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_164
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_165
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_166
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_167
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_168
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_KHR_extension_169
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_EXT_extension_170
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_QCOM_extension_171
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_QCOM_extension_172
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_QCOM_extension_173
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_QCOM_extension_174
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_global_priority
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_EXT_extension_176
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_EXT_extension_177
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_EXT_extension_178
#endif

    , module Graphics.Vulkan.Ext.VK_EXT_external_memory_host
#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_180
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_181
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_182
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_183
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_184
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_185
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_186
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_187
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_188
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_189
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_AMD_extension_190
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_NV_extension_191
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_GOOGLE_extension_192
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_GOOGLE_extension_193
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_GOOGLE_extension_194
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_GOOGLE_extension_195
#endif

#ifdef DISABLED_EXTENSIONS_STUB
    , module Graphics.Vulkan.Ext.VK_GOOGLE_extension_196
#endif

    ) where

import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Ext.VK_KHR_display
import Graphics.Vulkan.Ext.VK_KHR_display_swapchain
#ifdef VK_USE_PLATFORM_XLIB_KHR
import Graphics.Vulkan.Ext.VK_KHR_xlib_surface
#endif

#ifdef VK_USE_PLATFORM_XCB_KHR
import Graphics.Vulkan.Ext.VK_KHR_xcb_surface
#endif

#ifdef VK_USE_PLATFORM_WAYLAND_KHR
import Graphics.Vulkan.Ext.VK_KHR_wayland_surface
#endif

#ifdef VK_USE_PLATFORM_MIR_KHR
import Graphics.Vulkan.Ext.VK_KHR_mir_surface
#endif

#ifdef VK_USE_PLATFORM_ANDROID_KHR
import Graphics.Vulkan.Ext.VK_KHR_android_surface
#endif

#ifdef VK_USE_PLATFORM_WIN32_KHR
import Graphics.Vulkan.Ext.VK_KHR_win32_surface
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_ANDROID_native_buffer
#endif

import Graphics.Vulkan.Ext.VK_EXT_debug_report
import Graphics.Vulkan.Ext.VK_NV_glsl_shader
import Graphics.Vulkan.Ext.VK_EXT_depth_range_unrestricted
import Graphics.Vulkan.Ext.VK_KHR_sampler_mirror_clamp_to_edge
import Graphics.Vulkan.Ext.VK_IMG_filter_cubic
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_17
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_18
#endif

import Graphics.Vulkan.Ext.VK_AMD_rasterization_order
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_20
#endif

import Graphics.Vulkan.Ext.VK_AMD_shader_trinary_minmax
import Graphics.Vulkan.Ext.VK_AMD_shader_explicit_vertex_parameter
import Graphics.Vulkan.Ext.VK_EXT_debug_marker
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_24
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_25
#endif

import Graphics.Vulkan.Ext.VK_AMD_gcn_shader
import Graphics.Vulkan.Ext.VK_NV_dedicated_allocation
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_EXT_extension_28
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NVX_extension_29
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NVX_extension_30
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NVX_extension_31
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_32
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_33
#endif

import Graphics.Vulkan.Ext.VK_AMD_draw_indirect_count
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_35
#endif

import Graphics.Vulkan.Ext.VK_AMD_negative_viewport_height
import Graphics.Vulkan.Ext.VK_AMD_gpu_shader_half_float
import Graphics.Vulkan.Ext.VK_AMD_shader_ballot
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_39
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_40
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_41
#endif

import Graphics.Vulkan.Ext.VK_AMD_texture_gather_bias_lod
import Graphics.Vulkan.Ext.VK_AMD_shader_info
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_44
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_45
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_46
#endif

import Graphics.Vulkan.Ext.VK_AMD_shader_image_load_store_lod
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NVX_extension_48
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_GOOGLE_extension_49
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_GOOGLE_extension_50
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NVX_extension_51
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NVX_extension_52
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_53
#endif

import Graphics.Vulkan.Ext.VK_KHX_multiview
import Graphics.Vulkan.Ext.VK_IMG_format_pvrtc
import Graphics.Vulkan.Ext.VK_NV_external_memory_capabilities
import Graphics.Vulkan.Ext.VK_NV_external_memory
#ifdef VK_USE_PLATFORM_WIN32_KHR
import Graphics.Vulkan.Ext.VK_NV_external_memory_win32
#endif

#ifdef VK_USE_PLATFORM_WIN32_KHR
import Graphics.Vulkan.Ext.VK_NV_win32_keyed_mutex
#endif

import Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2
import Graphics.Vulkan.Ext.VK_KHX_device_group
import Graphics.Vulkan.Ext.VK_EXT_validation_flags
#ifdef VK_USE_PLATFORM_VI_NN
import Graphics.Vulkan.Ext.VK_NN_vi_surface
#endif

import Graphics.Vulkan.Ext.VK_KHR_shader_draw_parameters
import Graphics.Vulkan.Ext.VK_EXT_shader_subgroup_ballot
import Graphics.Vulkan.Ext.VK_EXT_shader_subgroup_vote
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_ARM_extension_01
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_ARM_extension_02
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_IMG_extension_69
#endif

import Graphics.Vulkan.Ext.VK_KHR_maintenance1
import Graphics.Vulkan.Ext.VK_KHX_device_group_creation
import Graphics.Vulkan.Ext.VK_KHR_external_memory_capabilities
import Graphics.Vulkan.Ext.VK_KHR_external_memory
#ifdef VK_USE_PLATFORM_WIN32_KHR
import Graphics.Vulkan.Ext.VK_KHR_external_memory_win32
#endif

import Graphics.Vulkan.Ext.VK_KHR_external_memory_fd
#ifdef VK_USE_PLATFORM_WIN32_KHR
import Graphics.Vulkan.Ext.VK_KHR_win32_keyed_mutex
#endif

import Graphics.Vulkan.Ext.VK_KHR_external_semaphore_capabilities
import Graphics.Vulkan.Ext.VK_KHR_external_semaphore
#ifdef VK_USE_PLATFORM_WIN32_KHR
import Graphics.Vulkan.Ext.VK_KHR_external_semaphore_win32
#endif

import Graphics.Vulkan.Ext.VK_KHR_external_semaphore_fd
import Graphics.Vulkan.Ext.VK_KHR_push_descriptor
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_KHR_extension_82
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_KHR_extension_83
#endif

import Graphics.Vulkan.Ext.VK_KHR_16bit_storage
import Graphics.Vulkan.Ext.VK_KHR_incremental_present
import Graphics.Vulkan.Ext.VK_KHR_descriptor_update_template
import Graphics.Vulkan.Ext.VK_NVX_device_generated_commands
import Graphics.Vulkan.Ext.VK_NV_clip_space_w_scaling
import Graphics.Vulkan.Ext.VK_EXT_direct_mode_display
#ifdef VK_USE_PLATFORM_XLIB_XRANDR_EXT
import Graphics.Vulkan.Ext.VK_EXT_acquire_xlib_display
#endif

import Graphics.Vulkan.Ext.VK_EXT_display_surface_counter
import Graphics.Vulkan.Ext.VK_EXT_display_control
import Graphics.Vulkan.Ext.VK_GOOGLE_display_timing
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_KHR_extension_94
#endif

import Graphics.Vulkan.Ext.VK_NV_sample_mask_override_coverage
import Graphics.Vulkan.Ext.VK_NV_geometry_shader_passthrough
import Graphics.Vulkan.Ext.VK_NV_viewport_array2
import Graphics.Vulkan.Ext.VK_NVX_multiview_per_view_attributes
import Graphics.Vulkan.Ext.VK_NV_viewport_swizzle
import Graphics.Vulkan.Ext.VK_EXT_discard_rectangles
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_101
#endif

import Graphics.Vulkan.Ext.VK_EXT_conservative_rasterization
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_103
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_104
#endif

import Graphics.Vulkan.Ext.VK_EXT_swapchain_colorspace
import Graphics.Vulkan.Ext.VK_EXT_hdr_metadata
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_IMG_extension_107
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_IMG_extension_108
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_IMG_extension_109
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_IMG_extension_110
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_IMG_extension_111
#endif

import Graphics.Vulkan.Ext.VK_KHR_shared_presentable_image
import Graphics.Vulkan.Ext.VK_KHR_external_fence_capabilities
import Graphics.Vulkan.Ext.VK_KHR_external_fence
#ifdef VK_USE_PLATFORM_WIN32_KHR
import Graphics.Vulkan.Ext.VK_KHR_external_fence_win32
#endif

import Graphics.Vulkan.Ext.VK_KHR_external_fence_fd
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_KHR_extension_117
#endif

import Graphics.Vulkan.Ext.VK_KHR_maintenance2
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_KHR_extension_119
#endif

import Graphics.Vulkan.Ext.VK_KHR_get_surface_capabilities2
import Graphics.Vulkan.Ext.VK_KHR_variable_pointers
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_KHR_extension_122
#endif

#ifdef VK_USE_PLATFORM_IOS_MVK
import Graphics.Vulkan.Ext.VK_MVK_ios_surface
#endif

#ifdef VK_USE_PLATFORM_MACOS_MVK
import Graphics.Vulkan.Ext.VK_MVK_macos_surface
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_MVK_moltenvk
#endif

import Graphics.Vulkan.Ext.VK_EXT_external_memory_dma_buf
import Graphics.Vulkan.Ext.VK_EXT_queue_family_foreign
import Graphics.Vulkan.Ext.VK_KHR_dedicated_allocation
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_EXT_extension_129
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_KHR_extension_130
#endif

import Graphics.Vulkan.Ext.VK_EXT_sampler_filter_minmax
import Graphics.Vulkan.Ext.VK_KHR_storage_buffer_storage_class
import Graphics.Vulkan.Ext.VK_AMD_gpu_shader_int16
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_134
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_135
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_136
#endif

import Graphics.Vulkan.Ext.VK_AMD_mixed_attachment_samples
import Graphics.Vulkan.Ext.VK_AMD_shader_fragment_mask
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_139
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_140
#endif

import Graphics.Vulkan.Ext.VK_EXT_shader_stencil_export
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_142
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_143
#endif

import Graphics.Vulkan.Ext.VK_EXT_sample_locations
import Graphics.Vulkan.Ext.VK_KHR_relaxed_block_layout
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_KHR_extension_146
#endif

import Graphics.Vulkan.Ext.VK_KHR_get_memory_requirements2
import Graphics.Vulkan.Ext.VK_KHR_image_format_list
import Graphics.Vulkan.Ext.VK_EXT_blend_operation_advanced
import Graphics.Vulkan.Ext.VK_NV_fragment_coverage_to_color
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_151
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_152
#endif

import Graphics.Vulkan.Ext.VK_NV_framebuffer_mixed_samples
import Graphics.Vulkan.Ext.VK_NV_fill_rectangle
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_155
#endif

import Graphics.Vulkan.Ext.VK_EXT_post_depth_coverage
import Graphics.Vulkan.Ext.VK_KHR_sampler_ycbcr_conversion
import Graphics.Vulkan.Ext.VK_KHR_bind_memory2
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_EXT_extension_159
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_EXT_extension_160
#endif

import Graphics.Vulkan.Ext.VK_EXT_validation_cache
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_EXT_extension_162
#endif

import Graphics.Vulkan.Ext.VK_EXT_shader_viewport_index_layer
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_164
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_165
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_166
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_167
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_168
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_KHR_extension_169
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_EXT_extension_170
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_QCOM_extension_171
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_QCOM_extension_172
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_QCOM_extension_173
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_QCOM_extension_174
#endif

import Graphics.Vulkan.Ext.VK_EXT_global_priority
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_EXT_extension_176
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_EXT_extension_177
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_EXT_extension_178
#endif

import Graphics.Vulkan.Ext.VK_EXT_external_memory_host
#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_180
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_181
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_182
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_183
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_184
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_185
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_186
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_187
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_188
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_189
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_AMD_extension_190
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_NV_extension_191
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_GOOGLE_extension_192
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_GOOGLE_extension_193
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_GOOGLE_extension_194
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_GOOGLE_extension_195
#endif

#ifdef DISABLED_EXTENSIONS_STUB
import Graphics.Vulkan.Ext.VK_GOOGLE_extension_196
#endif

