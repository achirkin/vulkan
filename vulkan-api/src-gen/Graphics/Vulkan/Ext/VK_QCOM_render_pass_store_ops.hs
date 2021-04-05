{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_QCOM_render_pass_store_ops
       (-- * Vulkan extension: @VK_QCOM_render_pass_store_ops@
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
        -- Extension number: @302@
        VK_QCOM_render_pass_store_ops_SPEC_VERSION,
        pattern VK_QCOM_render_pass_store_ops_SPEC_VERSION,
        VK_QCOM_render_pass_store_ops_EXTENSION_NAME,
        pattern VK_QCOM_render_pass_store_ops_EXTENSION_NAME,
        pattern VK_ATTACHMENT_STORE_OP_NONE_QCOM)
       where
import GHC.Ptr                               (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Enum.Attachment (VkAttachmentStoreOp (..))

pattern VK_QCOM_render_pass_store_ops_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_QCOM_render_pass_store_ops_SPEC_VERSION = 2

type VK_QCOM_render_pass_store_ops_SPEC_VERSION = 2

pattern VK_QCOM_render_pass_store_ops_EXTENSION_NAME :: CString

pattern VK_QCOM_render_pass_store_ops_EXTENSION_NAME <-
        (is_VK_QCOM_render_pass_store_ops_EXTENSION_NAME -> True)
  where
    VK_QCOM_render_pass_store_ops_EXTENSION_NAME
      = _VK_QCOM_render_pass_store_ops_EXTENSION_NAME

{-# INLINE _VK_QCOM_render_pass_store_ops_EXTENSION_NAME #-}

_VK_QCOM_render_pass_store_ops_EXTENSION_NAME :: CString
_VK_QCOM_render_pass_store_ops_EXTENSION_NAME
  = Ptr "VK_QCOM_render_pass_store_ops\NUL"#

{-# INLINE is_VK_QCOM_render_pass_store_ops_EXTENSION_NAME #-}

is_VK_QCOM_render_pass_store_ops_EXTENSION_NAME :: CString -> Bool
is_VK_QCOM_render_pass_store_ops_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_QCOM_render_pass_store_ops_EXTENSION_NAME

type VK_QCOM_render_pass_store_ops_EXTENSION_NAME =
     "VK_QCOM_render_pass_store_ops"

pattern VK_ATTACHMENT_STORE_OP_NONE_QCOM :: VkAttachmentStoreOp

pattern VK_ATTACHMENT_STORE_OP_NONE_QCOM =
        VkAttachmentStoreOp 1000301000
