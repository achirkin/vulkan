{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Debug
       (VkDebugReportBitmaskEXT(VkDebugReportBitmaskEXT,
                                VkDebugReportFlagsEXT, VkDebugReportFlagBitsEXT,
                                VK_DEBUG_REPORT_INFORMATION_BIT_EXT,
                                VK_DEBUG_REPORT_WARNING_BIT_EXT,
                                VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT,
                                VK_DEBUG_REPORT_ERROR_BIT_EXT, VK_DEBUG_REPORT_DEBUG_BIT_EXT),
        VkDebugReportFlagsEXT, VkDebugReportFlagBitsEXT,
        VkDebugReportObjectTypeEXT(VkDebugReportObjectTypeEXT,
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
                                   VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT),
        VkDebugUtilsMessageSeverityBitmaskEXT(VkDebugUtilsMessageSeverityBitmaskEXT,
                                              VkDebugUtilsMessageSeverityFlagsEXT,
                                              VkDebugUtilsMessageSeverityFlagBitsEXT,
                                              VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT,
                                              VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT,
                                              VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,
                                              VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT),
        VkDebugUtilsMessageSeverityFlagsEXT,
        VkDebugUtilsMessageSeverityFlagBitsEXT,
        VkDebugUtilsMessageTypeBitmaskEXT(VkDebugUtilsMessageTypeBitmaskEXT,
                                          VkDebugUtilsMessageTypeFlagsEXT,
                                          VkDebugUtilsMessageTypeFlagBitsEXT,
                                          VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT,
                                          VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
                                          VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT),
        VkDebugUtilsMessageTypeFlagsEXT,
        VkDebugUtilsMessageTypeFlagBitsEXT)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkDebugReportBitmaskEXT (a ::
                                   FlagType) = VkDebugReportBitmaskEXT VkFlags
                                               deriving (Eq, Ord, Storable)

type VkDebugReportFlagsEXT = VkDebugReportBitmaskEXT FlagMask

type VkDebugReportFlagBitsEXT = VkDebugReportBitmaskEXT FlagBit

pattern VkDebugReportFlagBitsEXT ::
        VkFlags -> VkDebugReportBitmaskEXT FlagBit

pattern VkDebugReportFlagBitsEXT n = VkDebugReportBitmaskEXT n

pattern VkDebugReportFlagsEXT ::
        VkFlags -> VkDebugReportBitmaskEXT FlagMask

pattern VkDebugReportFlagsEXT n = VkDebugReportBitmaskEXT n

deriving instance Bits (VkDebugReportBitmaskEXT FlagMask)

deriving instance FiniteBits (VkDebugReportBitmaskEXT FlagMask)

instance Show (VkDebugReportBitmaskEXT a) where
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
    showsPrec p (VkDebugReportBitmaskEXT x)
      = showParen (p >= 11)
          (showString "VkDebugReportBitmaskEXT " . showsPrec 11 x)

instance Read (VkDebugReportBitmaskEXT a) where
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
               (expectP (Ident "VkDebugReportBitmaskEXT") >>
                  (VkDebugReportBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT ::
        VkDebugReportBitmaskEXT a

pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT =
        VkDebugReportBitmaskEXT 1

-- | bitpos = @1@
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT ::
        VkDebugReportBitmaskEXT a

pattern VK_DEBUG_REPORT_WARNING_BIT_EXT = VkDebugReportBitmaskEXT 2

-- | bitpos = @2@
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT ::
        VkDebugReportBitmaskEXT a

pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT =
        VkDebugReportBitmaskEXT 4

-- | bitpos = @3@
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT :: VkDebugReportBitmaskEXT a

pattern VK_DEBUG_REPORT_ERROR_BIT_EXT = VkDebugReportBitmaskEXT 8

-- | bitpos = @4@
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT :: VkDebugReportBitmaskEXT a

pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT = VkDebugReportBitmaskEXT 16

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugReportObjectTypeEXT VkDebugReportObjectTypeEXT registry at www.khronos.org>
newtype VkDebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT Int32
                                     deriving (Eq, Ord, Enum, Storable)

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

newtype VkDebugUtilsMessageSeverityBitmaskEXT (a ::
                                                 FlagType) = VkDebugUtilsMessageSeverityBitmaskEXT VkFlags
                                                             deriving (Eq, Ord, Storable)

type VkDebugUtilsMessageSeverityFlagsEXT =
     VkDebugUtilsMessageSeverityBitmaskEXT FlagMask

type VkDebugUtilsMessageSeverityFlagBitsEXT =
     VkDebugUtilsMessageSeverityBitmaskEXT FlagBit

pattern VkDebugUtilsMessageSeverityFlagBitsEXT ::
        VkFlags -> VkDebugUtilsMessageSeverityBitmaskEXT FlagBit

pattern VkDebugUtilsMessageSeverityFlagBitsEXT n =
        VkDebugUtilsMessageSeverityBitmaskEXT n

pattern VkDebugUtilsMessageSeverityFlagsEXT ::
        VkFlags -> VkDebugUtilsMessageSeverityBitmaskEXT FlagMask

pattern VkDebugUtilsMessageSeverityFlagsEXT n =
        VkDebugUtilsMessageSeverityBitmaskEXT n

deriving instance
         Bits (VkDebugUtilsMessageSeverityBitmaskEXT FlagMask)

deriving instance
         FiniteBits (VkDebugUtilsMessageSeverityBitmaskEXT FlagMask)

instance Show (VkDebugUtilsMessageSeverityBitmaskEXT a) where
    showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
      = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT"
    showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
      = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT"
    showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
      = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT"
    showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
      = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT"
    showsPrec p (VkDebugUtilsMessageSeverityBitmaskEXT x)
      = showParen (p >= 11)
          (showString "VkDebugUtilsMessageSeverityBitmaskEXT " .
             showsPrec 11 x)

instance Read (VkDebugUtilsMessageSeverityBitmaskEXT a) where
    readPrec
      = parens
          (choose
             [("VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT",
               pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT),
              ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT",
               pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT),
              ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT",
               pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT),
              ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT",
               pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)]
             +++
             prec 10
               (expectP (Ident "VkDebugUtilsMessageSeverityBitmaskEXT") >>
                  (VkDebugUtilsMessageSeverityBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT ::
        VkDebugUtilsMessageSeverityBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT =
        VkDebugUtilsMessageSeverityBitmaskEXT 1

-- | bitpos = @4@
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT ::
        VkDebugUtilsMessageSeverityBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT =
        VkDebugUtilsMessageSeverityBitmaskEXT 16

-- | bitpos = @8@
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT ::
        VkDebugUtilsMessageSeverityBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT =
        VkDebugUtilsMessageSeverityBitmaskEXT 256

-- | bitpos = @12@
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT ::
        VkDebugUtilsMessageSeverityBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT =
        VkDebugUtilsMessageSeverityBitmaskEXT 4096

newtype VkDebugUtilsMessageTypeBitmaskEXT (a ::
                                             FlagType) = VkDebugUtilsMessageTypeBitmaskEXT VkFlags
                                                         deriving (Eq, Ord, Storable)

type VkDebugUtilsMessageTypeFlagsEXT =
     VkDebugUtilsMessageTypeBitmaskEXT FlagMask

type VkDebugUtilsMessageTypeFlagBitsEXT =
     VkDebugUtilsMessageTypeBitmaskEXT FlagBit

pattern VkDebugUtilsMessageTypeFlagBitsEXT ::
        VkFlags -> VkDebugUtilsMessageTypeBitmaskEXT FlagBit

pattern VkDebugUtilsMessageTypeFlagBitsEXT n =
        VkDebugUtilsMessageTypeBitmaskEXT n

pattern VkDebugUtilsMessageTypeFlagsEXT ::
        VkFlags -> VkDebugUtilsMessageTypeBitmaskEXT FlagMask

pattern VkDebugUtilsMessageTypeFlagsEXT n =
        VkDebugUtilsMessageTypeBitmaskEXT n

deriving instance Bits (VkDebugUtilsMessageTypeBitmaskEXT FlagMask)

deriving instance
         FiniteBits (VkDebugUtilsMessageTypeBitmaskEXT FlagMask)

instance Show (VkDebugUtilsMessageTypeBitmaskEXT a) where
    showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
      = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT"
    showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
      = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT"
    showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
      = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT"
    showsPrec p (VkDebugUtilsMessageTypeBitmaskEXT x)
      = showParen (p >= 11)
          (showString "VkDebugUtilsMessageTypeBitmaskEXT " . showsPrec 11 x)

instance Read (VkDebugUtilsMessageTypeBitmaskEXT a) where
    readPrec
      = parens
          (choose
             [("VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT",
               pure VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT),
              ("VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT",
               pure VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT),
              ("VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT",
               pure VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT)]
             +++
             prec 10
               (expectP (Ident "VkDebugUtilsMessageTypeBitmaskEXT") >>
                  (VkDebugUtilsMessageTypeBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT ::
        VkDebugUtilsMessageTypeBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT =
        VkDebugUtilsMessageTypeBitmaskEXT 1

-- | bitpos = @1@
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT ::
        VkDebugUtilsMessageTypeBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT =
        VkDebugUtilsMessageTypeBitmaskEXT 2

-- | bitpos = @2@
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT ::
        VkDebugUtilsMessageTypeBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT =
        VkDebugUtilsMessageTypeBitmaskEXT 4
