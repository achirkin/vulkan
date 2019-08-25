{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_win32_keyed_mutex
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkPipelineBindPoint(..), VkPipelineCacheHeaderVersion(..),
        VkPipelineCreateBitmask(..), VkPipelineStageBitmask(..),
        VkPipelineCacheCreateFlagBits(..),
        VkPipelineColorBlendStateCreateFlagBits(..),
        VkPipelineCreateFlagBits(), VkPipelineCreateFlags(),
        VkPipelineDepthStencilStateCreateFlagBits(..),
        VkPipelineDynamicStateCreateFlagBits(..),
        VkPipelineInputAssemblyStateCreateFlagBits(..),
        VkPipelineLayoutCreateFlagBits(..),
        VkPipelineMultisampleStateCreateFlagBits(..),
        VkPipelineRasterizationStateCreateFlagBits(..),
        VkPipelineShaderStageCreateFlagBits(..), VkPipelineStageFlagBits(),
        VkPipelineStageFlags(),
        VkPipelineTessellationStateCreateFlagBits(..),
        VkPipelineVertexInputStateCreateFlagBits(..),
        VkPipelineViewportStateCreateFlagBits(..), VkStructureType(..),
        VkSubmitInfo, VkWin32KeyedMutexAcquireReleaseInfoKHR,
        -- > #include "vk_platform.h"
        VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION,
        pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION,
        VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME,
        pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR)
       where
import           GHC.Ptr                                       (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Pipeline
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.PlatformWin32Khr (VkWin32KeyedMutexAcquireReleaseInfoKHR)
import           Graphics.Vulkan.Types.Struct.SubmitInfo       (VkSubmitInfo)

pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

type VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString

pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME <-
        (is_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME -> True)
  where
    VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
      = _VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME

{-# INLINE _VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME #-}

_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString
_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
  = Ptr "VK_KHR_win32_keyed_mutex\NUL"#

{-# INLINE is_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME #-}

is_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME

type VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME =
     "VK_KHR_win32_keyed_mutex"

pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
        = VkStructureType 1000075000
