{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_rasterization_order
       (-- * Vulkan extension: @VK_AMD_rasterization_order@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @aqnuep@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @19@
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkCullModeFlags,
        module Graphics.Vulkan.Types.Enum.VkFrontFace,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateRasterizationOrderAMD,
        module Graphics.Vulkan.Types.Enum.VkPolygonMode,
        module Graphics.Vulkan.Types.Enum.VkRasterizationOrderAMD,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION,
        pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION,
        VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME,
        pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD)
       where
import           GHC.Ptr
                                                                                                 (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkCullModeFlags
import           Graphics.Vulkan.Types.Enum.VkFrontFace
import           Graphics.Vulkan.Types.Enum.VkPolygonMode
import           Graphics.Vulkan.Types.Enum.VkRasterizationOrderAMD
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateRasterizationOrderAMD

pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1

type VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1

pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: CString

pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME <-
        (is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME -> True)
  where VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
          = _VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME

{-# INLINE _VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME #-}

_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: CString
_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  = Ptr "VK_AMD_rasterization_order\NUL"#

{-# INLINE is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME #-}

is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  = eqCStrings _VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME

type VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME =
     "VK_AMD_rasterization_order"

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
        = VkStructureType 1000018000
