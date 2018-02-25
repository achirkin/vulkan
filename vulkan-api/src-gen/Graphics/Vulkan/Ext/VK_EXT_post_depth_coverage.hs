{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_post_depth_coverage
       (-- * Vulkan extension: @VK_EXT_post_depth_coverage@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Koch @dgkoch@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @156@
        VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION,
        pattern VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION,
        VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME,
        pattern VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION = 1

type VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION = 1

pattern VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME :: CString

pattern VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME <-
        (is_VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME -> True)
  where VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
          = _VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME

{-# INLINE _VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME #-}

_VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME :: CString
_VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
  = Ptr "VK_EXT_post_depth_coverage\NUL"#

{-# INLINE is_VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME #-}

is_VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME

type VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME =
     "VK_EXT_post_depth_coverage"
