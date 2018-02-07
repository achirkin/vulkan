#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_sample_mask_override_coverage
       (-- * Vulkan extension: @VK_NV_sample_mask_override_coverage@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @95@
        VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION,
        pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION,
        VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME,
        pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME)
       where
import           Foreign.C.String              (CString)
import           GHC.Ptr                       (Ptr (..))
import           Graphics.Vulkan.StructMembers

pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION = 1

type VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION = 1

pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME ::
        CString

pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME <-
        (is_VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME -> True)
  where VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
          = _VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME

_VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME :: CString

{-# INLINE _VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME #-}
_VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
  = Ptr "VK_NV_sample_mask_override_coverage\NUL"##

is_VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME ::
                                                      CString -> Bool

{-# INLINE is_VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
           #-}
is_VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
  = (_VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME ==)

type VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME =
     "VK_NV_sample_mask_override_coverage"
