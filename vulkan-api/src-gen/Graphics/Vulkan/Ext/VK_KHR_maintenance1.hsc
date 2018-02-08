#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_maintenance1
       (-- * Vulkan extension: @VK_KHR_maintenance1@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @70@
        vkTrimCommandPoolKHR, VK_KHR_MAINTENANCE1_SPEC_VERSION,
        pattern VK_KHR_MAINTENANCE1_SPEC_VERSION,
        VK_KHR_MAINTENANCE1_EXTENSION_NAME,
        pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME,
        pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR,
        pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR,
        pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR,
        pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR)
       where
import           Foreign.C.String        (CString)
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Common  (VkCommandPool,
                                          VkCommandPoolTrimFlagsKHR (..),
                                          VkDevice,
                                          VkFormatFeatureFlagBits (..),
                                          VkImageCreateFlagBits (..),
                                          VkResult (..))
import           Graphics.Vulkan.Marshal

-- | > void vkTrimCommandPoolKHR
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolTrimFlagsKHR flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkTrimCommandPoolKHR.html vkTrimCommandPoolKHR registry at www.khronos.org>
foreign import ccall unsafe "vkTrimCommandPoolKHR"
               vkTrimCommandPoolKHR ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> VkCommandPoolTrimFlagsKHR -- ^ flags
                                                                      -> IO ()

pattern VK_KHR_MAINTENANCE1_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_MAINTENANCE1_SPEC_VERSION = 1

type VK_KHR_MAINTENANCE1_SPEC_VERSION = 1

pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME :: CString

pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME <-
        (is_VK_KHR_MAINTENANCE1_EXTENSION_NAME -> True)
  where VK_KHR_MAINTENANCE1_EXTENSION_NAME
          = _VK_KHR_MAINTENANCE1_EXTENSION_NAME

_VK_KHR_MAINTENANCE1_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_MAINTENANCE1_EXTENSION_NAME #-}
_VK_KHR_MAINTENANCE1_EXTENSION_NAME
  = Ptr "VK_KHR_maintenance1\NUL"##

is_VK_KHR_MAINTENANCE1_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_MAINTENANCE1_EXTENSION_NAME #-}
is_VK_KHR_MAINTENANCE1_EXTENSION_NAME
  = (_VK_KHR_MAINTENANCE1_EXTENSION_NAME ==)

type VK_KHR_MAINTENANCE1_EXTENSION_NAME = "VK_KHR_maintenance1"

pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR :: VkResult

pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR = VkResult (-1000069000)

-- | Format can be used as the source image of image transfer commands
--
--   bitpos = @14@
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR =
        VkFormatFeatureFlagBits 16384

-- | Format can be used as the destination image of image transfer commands
--
--   bitpos = @15@
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR =
        VkFormatFeatureFlagBits 32768

-- | The 3D image can be viewed as a 2D or 2D array image
--
--   bitpos = @5@
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR =
        VkImageCreateFlagBits 32
