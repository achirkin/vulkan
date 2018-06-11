{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_viewport_swizzle
       (-- * Vulkan extension: @VK_NV_viewport_swizzle@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell-nv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @99@
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.Offset,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.Pipeline,
        module Graphics.Vulkan.Types.Struct.Rect,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Struct.Viewport,
        module Graphics.Vulkan.Types.Enum.ViewportCoordinateSwizzleNV,
        -- > #include "vk_platform.h"
        VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION,
        pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION,
        VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME,
        pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV)
       where
import           GHC.Ptr                                                (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.ViewportCoordinateSwizzleNV
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Offset
import           Graphics.Vulkan.Types.Struct.Pipeline
import           Graphics.Vulkan.Types.Struct.Rect
import           Graphics.Vulkan.Types.Struct.Viewport

pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1

type VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1

pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: CString

pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME <-
        (is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME -> True)
  where VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
          = _VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME

{-# INLINE _VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME #-}

_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: CString
_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  = Ptr "VK_NV_viewport_swizzle\NUL"#

{-# INLINE is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME #-}

is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: CString -> Bool
is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME

type VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME =
     "VK_NV_viewport_swizzle"

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
        = VkStructureType 1000098000
