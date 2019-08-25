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
        pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE)
       where
import           GHC.Ptr                            (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.Sampler (VkSamplerAddressMode (..))

pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 1

type VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 1

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

-- | Note that this defines what was previously a core enum, and so uses the 'value' attribute rather than 'offset', and does not have a suffix. This is a special case, and should not be repeated
pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE =
        VkSamplerAddressMode 4
