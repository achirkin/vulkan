{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_QCOM_render_pass_shader_resolve
       (-- * Vulkan extension: @VK_QCOM_render_pass_shader_resolve@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Bill Licea-Kane @wwlk@
        --
        -- author: @QCOM@
        --
        -- type: @device@
        --
        -- Extension number: @172@
        VK_QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION,
        pattern VK_QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION,
        VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME,
        pattern VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME,
        pattern VK_SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM,
        pattern VK_SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM)
       where
import GHC.Ptr                            (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Enum.Subpass (VkSubpassDescriptionBitmask (..))

pattern VK_QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION = 4

type VK_QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION = 4

pattern VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME ::
        CString

pattern VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME <-
        (is_VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME -> True)
  where
    VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME
      = _VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME

{-# INLINE _VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME #-}

_VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME :: CString
_VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME
  = Ptr "VK_QCOM_render_pass_shader_resolve\NUL"#

{-# INLINE is_VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME #-}

is_VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME ::
                                                     CString -> Bool
is_VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME

type VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME =
     "VK_QCOM_render_pass_shader_resolve"

-- | bitpos = @2@
pattern VK_SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM ::
        VkSubpassDescriptionBitmask a

pattern VK_SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM =
        VkSubpassDescriptionBitmask 4

-- | bitpos = @3@
pattern VK_SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM ::
        VkSubpassDescriptionBitmask a

pattern VK_SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM =
        VkSubpassDescriptionBitmask 8
