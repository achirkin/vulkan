{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_sampler_mirror_clamp_to_edge
       (-- * Vulkan extension: @VK_KHR_sampler_mirror_clamp_to_edge@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobias Hector @tobski@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @15@
        VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION,
        pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION,
        VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME,
        pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME,
        pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE,
        pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE_KHR)
       where
import GHC.Ptr                  (Ptr (..))
import Graphics.Vulkan.Core_1_2 (pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE)
import Graphics.Vulkan.Marshal

pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 3

type VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 3

pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME ::
        CString

pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME <-
        (is_VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME -> True)
  where
    VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
      = _VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME

{-# INLINE _VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME #-}

_VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME :: CString
_VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
  = Ptr "VK_KHR_sampler_mirror_clamp_to_edge\NUL"#

{-# INLINE is_VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
           #-}

is_VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME ::
                                                      CString -> Bool
is_VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME

type VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME =
     "VK_KHR_sampler_mirror_clamp_to_edge"

-- | Alias introduced for consistency with extension suffixing rules
pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE_KHR =
        VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
