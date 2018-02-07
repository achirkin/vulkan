#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_shader_subgroup_vote
       (-- * Vulkan extension: @VK_EXT_shader_subgroup_vote@
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
        -- Extension number: @66@
        VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION,
        pattern VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION,
        VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME,
        pattern VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME)
       where
import           Foreign.C.String              (CString)
import           GHC.Ptr                       (Ptr (..))
import           Graphics.Vulkan.StructMembers

pattern VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION = 1

type VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION = 1

pattern VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME :: CString

pattern VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME <-
        (is_VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME -> True)
  where VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME
          = _VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME

_VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME #-}
_VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME
  = Ptr "VK_EXT_shader_subgroup_vote\NUL"##

is_VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME #-}
is_VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME
  = (_VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME ==)

type VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME =
     "VK_EXT_shader_subgroup_vote"
