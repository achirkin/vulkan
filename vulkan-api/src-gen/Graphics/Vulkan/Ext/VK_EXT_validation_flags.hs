{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_validation_flags
       (-- * Vulkan extension: @VK_EXT_validation_flags@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobin Ehlis @tobine@
        --
        -- author: @GOOGLE@
        --
        -- type: @instance@
        --
        -- Extension number: @62@
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Struct.ApplicationInfo,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.InstanceCreateInfo,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Enum.ValidationC,
        module Graphics.Vulkan.Types.Struct.Validation,
        -- > #include "vk_platform.h"
        VK_EXT_VALIDATION_FLAGS_SPEC_VERSION,
        pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION,
        VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME,
        pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT)
       where
import           GHC.Ptr                                         (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.ValidationC
import           Graphics.Vulkan.Types.Struct.ApplicationInfo
import           Graphics.Vulkan.Types.Struct.InstanceCreateInfo
import           Graphics.Vulkan.Types.Struct.Validation

pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION = 1

type VK_EXT_VALIDATION_FLAGS_SPEC_VERSION = 1

pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME :: CString

pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME <-
        (is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME -> True)
  where VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
          = _VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME

{-# INLINE _VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME #-}

_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME :: CString
_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
  = Ptr "VK_EXT_validation_flags\NUL"#

{-# INLINE is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME #-}

is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME

type VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME =
     "VK_EXT_validation_flags"

pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT =
        VkStructureType 1000061000
