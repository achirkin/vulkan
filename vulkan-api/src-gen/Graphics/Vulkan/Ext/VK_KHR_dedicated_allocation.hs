{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_dedicated_allocation
       (-- * Vulkan extension: @VK_KHR_dedicated_allocation@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @128@
        --
        -- Required extensions: 'VK_KHR_get_memory_requirements2'.
        --

        -- ** Required extensions: 'VK_KHR_get_memory_requirements2'.
        module Graphics.Vulkan.Types.Struct.Memory,
        VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION,
        pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION,
        VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME,
        pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR)
       where
import           GHC.Ptr                             (Ptr (..))
import           Graphics.Vulkan.Core_1_1            (pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
                                                      pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Struct.Memory

pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION = 3

type VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION = 3

pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString

pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME <-
        (is_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME -> True)
  where VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
          = _VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME

{-# INLINE _VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME #-}

_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString
_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
  = Ptr "VK_KHR_dedicated_allocation\NUL"#

{-# INLINE is_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME #-}

is_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME

type VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME =
     "VK_KHR_dedicated_allocation"

pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR =
        VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS

pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR =
        VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
