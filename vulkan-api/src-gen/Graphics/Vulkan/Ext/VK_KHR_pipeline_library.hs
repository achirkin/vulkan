{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_pipeline_library
       (-- * Vulkan extension: @VK_KHR_pipeline_library@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Christoph Kubisch @pixeljetstream@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- platform: @provisional@
        --
        -- Extension number: @291@
        VkPipelineLibraryCreateInfoKHR, VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_KHR_PIPELINE_LIBRARY_SPEC_VERSION,
        pattern VK_KHR_PIPELINE_LIBRARY_SPEC_VERSION,
        VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME,
        pattern VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME,
        pattern VK_PIPELINE_CREATE_LIBRARY_BIT_KHR,
        pattern VK_STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR)
       where
import GHC.Ptr                                           (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Enum.Pipeline               (VkPipelineCreateBitmask (..))
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.EnableBetaExtensions (VkPipelineLibraryCreateInfoKHR)

pattern VK_KHR_PIPELINE_LIBRARY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_PIPELINE_LIBRARY_SPEC_VERSION = 1

type VK_KHR_PIPELINE_LIBRARY_SPEC_VERSION = 1

pattern VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME :: CString

pattern VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME <-
        (is_VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME -> True)
  where
    VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME
      = _VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME

{-# INLINE _VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME #-}

_VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME :: CString
_VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME
  = Ptr "VK_KHR_pipeline_library\NUL"#

{-# INLINE is_VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME #-}

is_VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME

type VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME =
     "VK_KHR_pipeline_library"

-- | bitpos = @11@
pattern VK_PIPELINE_CREATE_LIBRARY_BIT_KHR ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_LIBRARY_BIT_KHR =
        VkPipelineCreateBitmask 2048

pattern VK_STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR =
        VkStructureType 1000290000
