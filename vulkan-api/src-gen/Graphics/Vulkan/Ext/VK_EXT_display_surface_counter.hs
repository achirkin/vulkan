{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_display_surface_counter
       (-- * Vulkan extension: @VK_EXT_display_surface_counter@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @instance@
        --
        -- Extension number: @91@
        --
        -- Required extensions: 'VK_KHR_display'.
        --

        -- ** Required extensions: 'VK_KHR_display'.
        module Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkSurfaceCapabilities2EXT,
        module Graphics.Vulkan.Types.Enum.VkSurfaceCounterFlagsEXT,
        module Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceSurfaceCapabilities2EXT,
        pattern VkGetPhysicalDeviceSurfaceCapabilities2EXT,
        HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT,
        PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT,
        unwrapVkGetPhysicalDeviceSurfaceCapabilities2EXT,
        vkGetPhysicalDeviceSurfaceCapabilities2EXT,
        vkGetPhysicalDeviceSurfaceCapabilities2EXTSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION,
        pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION,
        VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME,
        pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT)
       where
import           GHC.Ptr                                                (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.InstanceProc                   (VulkanInstanceProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSurfaceCounterFlagsEXT
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkSurfaceCapabilities2EXT

pattern VkGetPhysicalDeviceSurfaceCapabilities2EXT :: CString

pattern VkGetPhysicalDeviceSurfaceCapabilities2EXT <-
        (is_VkGetPhysicalDeviceSurfaceCapabilities2EXT -> True)
  where VkGetPhysicalDeviceSurfaceCapabilities2EXT
          = _VkGetPhysicalDeviceSurfaceCapabilities2EXT

{-# INLINE _VkGetPhysicalDeviceSurfaceCapabilities2EXT #-}

_VkGetPhysicalDeviceSurfaceCapabilities2EXT :: CString
_VkGetPhysicalDeviceSurfaceCapabilities2EXT
  = Ptr "vkGetPhysicalDeviceSurfaceCapabilities2EXT\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfaceCapabilities2EXT #-}

is_VkGetPhysicalDeviceSurfaceCapabilities2EXT :: CString -> Bool
is_VkGetPhysicalDeviceSurfaceCapabilities2EXT
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfaceCapabilities2EXT

type VkGetPhysicalDeviceSurfaceCapabilities2EXT =
     "vkGetPhysicalDeviceSurfaceCapabilities2EXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilities2EXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , VkSurfaceCapabilities2EXT* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSurfaceCapabilities2EXT.html vkGetPhysicalDeviceSurfaceCapabilities2EXT registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSurfaceCapabilities2EXT"
               vkGetPhysicalDeviceSurfaceCapabilities2EXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkSurfaceCapabilities2EXT -- ^ pSurfaceCapabilities
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilities2EXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , VkSurfaceCapabilities2EXT* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSurfaceCapabilities2EXT.html vkGetPhysicalDeviceSurfaceCapabilities2EXT registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceSurfaceCapabilities2EXT"
               vkGetPhysicalDeviceSurfaceCapabilities2EXTSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkSurfaceCapabilities2EXT -- ^ pSurfaceCapabilities
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilities2EXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , VkSurfaceCapabilities2EXT* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSurfaceCapabilities2EXT.html vkGetPhysicalDeviceSurfaceCapabilities2EXT registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkSurfaceKHR -- ^ surface
                    -> Ptr VkSurfaceCapabilities2EXT -- ^ pSurfaceCapabilities
                                                     -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT =
     FunPtr HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceCapabilities2EXT ::
               PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT ->
                 HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT

instance VulkanInstanceProc
           "vkGetPhysicalDeviceSurfaceCapabilities2EXT"
         where
        type VkInstanceProcType
               "vkGetPhysicalDeviceSurfaceCapabilities2EXT"
             = HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT
        vkInstanceProcSymbol = _VkGetPhysicalDeviceSurfaceCapabilities2EXT

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc
          = unwrapVkGetPhysicalDeviceSurfaceCapabilities2EXT

        {-# INLINE unwrapVkInstanceProc #-}

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1

type VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: CString

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME <-
        (is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME -> True)
  where VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
          = _VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME

{-# INLINE _VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME #-}

_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: CString
_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  = Ptr "VK_EXT_display_surface_counter\NUL"#

{-# INLINE is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME #-}

is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME

type VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME =
     "VK_EXT_display_surface_counter"

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT =
        VkStructureType 1000090000

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT =
        VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
