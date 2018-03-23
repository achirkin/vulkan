{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
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
        module Graphics.Vulkan.Types.Bitmasks, vkTrimCommandPoolKHR,
        vkTrimCommandPoolKHRSafe, module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_MAINTENANCE1_SPEC_VERSION,
        pattern VK_KHR_MAINTENANCE1_SPEC_VERSION,
        VK_KHR_MAINTENANCE1_EXTENSION_NAME,
        pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME,
        pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR,
        pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR,
        pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR,
        pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR)
       where
import           GHC.Ptr                         (Ptr (..))
import           Graphics.Vulkan.Core_1_1        (pattern VK_ERROR_OUT_OF_POOL_MEMORY,
                                                  pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT,
                                                  pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT,
                                                  pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Handles

-- | This is an alias for `vkTrimCommandPool`.
--
--   > () vkTrimCommandPoolKHR
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolTrimFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkTrimCommandPoolKHR.html vkTrimCommandPoolKHR registry at www.khronos.org>
foreign import ccall unsafe "vkTrimCommandPool"
               vkTrimCommandPoolKHR ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> VkCommandPoolTrimFlags -- ^ flags
                                                                   -> IO ()

-- | This is an alias for `vkTrimCommandPool`.
--
--   > () vkTrimCommandPoolKHR
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolTrimFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkTrimCommandPoolKHR.html vkTrimCommandPoolKHR registry at www.khronos.org>
foreign import ccall safe "vkTrimCommandPool"
               vkTrimCommandPoolKHRSafe ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> VkCommandPoolTrimFlags -- ^ flags
                                                                   -> IO ()

pattern VK_KHR_MAINTENANCE1_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_MAINTENANCE1_SPEC_VERSION = 2

type VK_KHR_MAINTENANCE1_SPEC_VERSION = 2

pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME :: CString

pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME <-
        (is_VK_KHR_MAINTENANCE1_EXTENSION_NAME -> True)
  where VK_KHR_MAINTENANCE1_EXTENSION_NAME
          = _VK_KHR_MAINTENANCE1_EXTENSION_NAME

{-# INLINE _VK_KHR_MAINTENANCE1_EXTENSION_NAME #-}

_VK_KHR_MAINTENANCE1_EXTENSION_NAME :: CString
_VK_KHR_MAINTENANCE1_EXTENSION_NAME
  = Ptr "VK_KHR_maintenance1\NUL"#

{-# INLINE is_VK_KHR_MAINTENANCE1_EXTENSION_NAME #-}

is_VK_KHR_MAINTENANCE1_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_MAINTENANCE1_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_MAINTENANCE1_EXTENSION_NAME

type VK_KHR_MAINTENANCE1_EXTENSION_NAME = "VK_KHR_maintenance1"

pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR =
        VK_ERROR_OUT_OF_POOL_MEMORY

pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR =
        VK_FORMAT_FEATURE_TRANSFER_SRC_BIT

pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR =
        VK_FORMAT_FEATURE_TRANSFER_DST_BIT

pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR =
        VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
