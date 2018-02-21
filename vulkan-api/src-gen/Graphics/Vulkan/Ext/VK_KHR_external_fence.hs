{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_external_fence
       (-- * Vulkan extension: @VK_KHR_external_fence@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @114@
        --
        -- Required extensions: 'VK_KHR_external_fence_capabilities'.
        --

        -- ** Required extensions: 'VK_KHR_external_fence_capabilities'.
        module Graphics.Vulkan.Types.Struct.VkExportFenceCreateInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkFenceCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkFenceCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkFenceImportFlagsKHR,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VK_KHR_EXTERNAL_FENCE_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                                      (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkFenceCreateFlags
import           Graphics.Vulkan.Types.Enum.VkFenceImportFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Struct.VkExportFenceCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkFenceCreateInfo

pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_FENCE_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  = Ptr "VK_KHR_external_fence\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME

type VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME = "VK_KHR_external_fence"

pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR =
        VkStructureType 1000113000
