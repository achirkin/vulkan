{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT
       (VkDebugReportObjectTypeEXT(VkDebugReportObjectTypeEXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT,
                                   VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT))
       where
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugReportObjectTypeEXTVkDebugReportObjectTypeEXT registry at www.khronos.org>
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
        showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
          = showString "VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT"
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
                  ("VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT",
                   pure VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT)]
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

pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT ::
        VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT =
        VkDebugReportObjectTypeEXT 33
