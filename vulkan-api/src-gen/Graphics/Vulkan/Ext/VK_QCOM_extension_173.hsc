#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_QCOM_extension_173
       (-- * Vulkan extension: @VK_QCOM_extension_173@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Bill Licea-Kane @billl@
        --
        -- author: @QCOM@
        --
        -- Extension number: @173@
        VK_QCOM_extension_173_SPEC_VERSION,
        pattern VK_QCOM_extension_173_SPEC_VERSION,
        VK_QCOM_extension_173_EXTENSION_NAME,
        pattern VK_QCOM_extension_173_EXTENSION_NAME)
       where
import           Foreign.C.String (CString)
import           GHC.Ptr          (Ptr (..))

pattern VK_QCOM_extension_173_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_QCOM_extension_173_SPEC_VERSION = 0

type VK_QCOM_extension_173_SPEC_VERSION = 0

pattern VK_QCOM_extension_173_EXTENSION_NAME :: CString

pattern VK_QCOM_extension_173_EXTENSION_NAME <-
        (is_VK_QCOM_extension_173_EXTENSION_NAME -> True)
  where VK_QCOM_extension_173_EXTENSION_NAME
          = _VK_QCOM_extension_173_EXTENSION_NAME

_VK_QCOM_extension_173_EXTENSION_NAME :: CString

{-# INLINE _VK_QCOM_extension_173_EXTENSION_NAME #-}
_VK_QCOM_extension_173_EXTENSION_NAME
  = Ptr "VK_QCOM_extension_173\NUL"##

is_VK_QCOM_extension_173_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_QCOM_extension_173_EXTENSION_NAME #-}
is_VK_QCOM_extension_173_EXTENSION_NAME
  = (_VK_QCOM_extension_173_EXTENSION_NAME ==)

type VK_QCOM_extension_173_EXTENSION_NAME = "VK_QCOM_extension_173"
