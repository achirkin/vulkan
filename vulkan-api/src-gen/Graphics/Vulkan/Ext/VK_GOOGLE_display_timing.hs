{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_GOOGLE_display_timing
       (-- * Vulkan extension: @VK_GOOGLE_display_timing@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Ian Elliott ianelliott@google.com@
        --
        -- author: @GOOGLE@
        --
        -- type: @device@
        --
        -- Extension number: @93@
        --
        -- Required extensions: 'VK_KHR_swapchain'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain'.
        module Graphics.Vulkan.Types.Struct.VkPastPresentationTimingGOOGLE,
        module Graphics.Vulkan.Types.Struct.VkPresentInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkPresentTimeGOOGLE,
        module Graphics.Vulkan.Types.Struct.VkPresentTimesInfoGOOGLE,
        module Graphics.Vulkan.Types.Struct.VkRefreshCycleDurationGOOGLE,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkGetRefreshCycleDurationGOOGLE,
        vkGetRefreshCycleDurationGOOGLESafe,
        vkGetPastPresentationTimingGOOGLE,
        vkGetPastPresentationTimingGOOGLESafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Handles,
        VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION,
        pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION,
        VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME,
        pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE)
       where
import           GHC.Ptr                                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkPastPresentationTimingGOOGLE
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR
import           Graphics.Vulkan.Types.Struct.VkPresentTimeGOOGLE
import           Graphics.Vulkan.Types.Struct.VkPresentTimesInfoGOOGLE
import           Graphics.Vulkan.Types.Struct.VkRefreshCycleDurationGOOGLE

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetRefreshCycleDurationGOOGLE
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , VkRefreshCycleDurationGOOGLE* pDisplayTimingProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetRefreshCycleDurationGOOGLE.html vkGetRefreshCycleDurationGOOGLE registry at www.khronos.org>
foreign import ccall unsafe "vkGetRefreshCycleDurationGOOGLE"
               vkGetRefreshCycleDurationGOOGLE ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                -> Ptr VkRefreshCycleDurationGOOGLE -- ^ pDisplayTimingProperties
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetRefreshCycleDurationGOOGLE
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , VkRefreshCycleDurationGOOGLE* pDisplayTimingProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetRefreshCycleDurationGOOGLE.html vkGetRefreshCycleDurationGOOGLE registry at www.khronos.org>
foreign import ccall safe "vkGetRefreshCycleDurationGOOGLE"
               vkGetRefreshCycleDurationGOOGLESafe ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                -> Ptr VkRefreshCycleDurationGOOGLE -- ^ pDisplayTimingProperties
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPastPresentationTimingGOOGLE
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , uint32_t* pPresentationTimingCount
--   >     , VkPastPresentationTimingGOOGLE* pPresentationTimings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPastPresentationTimingGOOGLE.html vkGetPastPresentationTimingGOOGLE registry at www.khronos.org>
foreign import ccall unsafe "vkGetPastPresentationTimingGOOGLE"
               vkGetPastPresentationTimingGOOGLE ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   Ptr Word32 -- ^ pPresentationTimingCount
                              -> Ptr VkPastPresentationTimingGOOGLE -- ^ pPresentationTimings
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPastPresentationTimingGOOGLE
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , uint32_t* pPresentationTimingCount
--   >     , VkPastPresentationTimingGOOGLE* pPresentationTimings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPastPresentationTimingGOOGLE.html vkGetPastPresentationTimingGOOGLE registry at www.khronos.org>
foreign import ccall safe "vkGetPastPresentationTimingGOOGLE"
               vkGetPastPresentationTimingGOOGLESafe ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   Ptr Word32 -- ^ pPresentationTimingCount
                              -> Ptr VkPastPresentationTimingGOOGLE -- ^ pPresentationTimings
                                                                    -> IO VkResult

pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1

type VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1

pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: CString

pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME <-
        (is_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME -> True)
  where VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
          = _VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME

{-# INLINE _VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME #-}

_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: CString
_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  = Ptr "VK_GOOGLE_display_timing\NUL"#

{-# INLINE is_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME #-}

is_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: CString -> Bool
is_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME

type VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME =
     "VK_GOOGLE_display_timing"

pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE =
        VkStructureType 1000092000
