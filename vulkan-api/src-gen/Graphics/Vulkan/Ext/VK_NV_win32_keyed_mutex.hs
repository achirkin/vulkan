{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_win32_keyed_mutex
       (-- * Vulkan extension: @VK_NV_win32_keyed_mutex@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Carsten Rohde@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @59@
        --
        -- Required extensions: 'VK_NV_external_memory_win32'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_NV_external_memory_win32'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkPipelineStageFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkSubmitInfo,
        module Graphics.Vulkan.Types.Struct.VkWin32KeyedMutexAcquireReleaseInfoNV,
        -- > #include "vk_platform.h"
        VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION,
        pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION,
        VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME,
        pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV)
       where
import           GHC.Ptr
                                                                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo
import           Graphics.Vulkan.Types.Struct.VkWin32KeyedMutexAcquireReleaseInfoNV

pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

type VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString

pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME <-
        (is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME -> True)
  where VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
          = _VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME

{-# INLINE _VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME #-}

_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString
_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  = Ptr "VK_NV_win32_keyed_mutex\NUL"#

{-# INLINE is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME #-}

is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString -> Bool
is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME

type VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME =
     "VK_NV_win32_keyed_mutex"

pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
        = VkStructureType 1000058000
