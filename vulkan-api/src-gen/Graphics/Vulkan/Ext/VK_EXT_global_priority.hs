{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_global_priority
       (-- * Vulkan extension: @VK_EXT_global_priority@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Andres Rodriguez @lostgoat@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @175@
        VkDeviceEventTypeEXT(..), VkDeviceGroupPresentModeBitmaskKHR(..),
        VkDeviceCreateFlagBits(..), VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkDeviceQueueGlobalPriorityCreateInfoEXT,
        VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkQueueBitmask(..), VkQueueGlobalPriorityEXT(..),
        VkQueueFlagBits(), VkQueueFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION,
        pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION,
        VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME,
        pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT,
        pattern VK_ERROR_NOT_PERMITTED_EXT)
       where
import           GHC.Ptr                                  (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Device
import           Graphics.Vulkan.Types.Enum.Queue
import           Graphics.Vulkan.Types.Enum.Result        (VkResult (..))
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Device      (VkDeviceQueueCreateInfo,
                                                           VkDeviceQueueGlobalPriorityCreateInfoEXT)

pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2

type VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2

pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: CString

pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME <-
        (is_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME -> True)
  where
    VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
      = _VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME

{-# INLINE _VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME #-}

_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: CString
_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
  = Ptr "VK_EXT_global_priority\NUL"#

{-# INLINE is_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME #-}

is_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME

type VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME =
     "VK_EXT_global_priority"

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
        = VkStructureType 1000174000

pattern VK_ERROR_NOT_PERMITTED_EXT :: VkResult

pattern VK_ERROR_NOT_PERMITTED_EXT = VkResult (-1000174001)
