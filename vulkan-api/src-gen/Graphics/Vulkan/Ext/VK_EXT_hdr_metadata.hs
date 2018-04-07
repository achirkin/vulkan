{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_hdr_metadata
       (-- * Vulkan extension: @VK_EXT_hdr_metadata@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Courtney Goeltzenleuchter @courtneygo@
        --
        -- author: @GOOGLE@
        --
        -- type: @device@
        --
        -- Extension number: @106@
        --
        -- Required extensions: 'VK_KHR_swapchain'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain'.
        module Graphics.Vulkan.Types.Struct.VkHdrMetadataEXT,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkXYColorEXT,
        -- > #include "vk_platform.h"
        VkSetHdrMetadataEXT, pattern VkSetHdrMetadataEXT,
        HS_vkSetHdrMetadataEXT, PFN_vkSetHdrMetadataEXT,
        unwrapVkSetHdrMetadataEXT, vkSetHdrMetadataEXT,
        vkSetHdrMetadataEXTSafe, module Graphics.Vulkan.Types.Handles,
        VK_EXT_HDR_METADATA_SPEC_VERSION,
        pattern VK_EXT_HDR_METADATA_SPEC_VERSION,
        VK_EXT_HDR_METADATA_EXTENSION_NAME,
        pattern VK_EXT_HDR_METADATA_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT)
       where
import           GHC.Ptr                                       (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                  (VulkanProc (..))
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkHdrMetadataEXT
import           Graphics.Vulkan.Types.Struct.VkXYColorEXT

pattern VkSetHdrMetadataEXT :: CString

pattern VkSetHdrMetadataEXT <- (is_VkSetHdrMetadataEXT -> True)
  where VkSetHdrMetadataEXT = _VkSetHdrMetadataEXT

{-# INLINE _VkSetHdrMetadataEXT #-}

_VkSetHdrMetadataEXT :: CString
_VkSetHdrMetadataEXT = Ptr "vkSetHdrMetadataEXT\NUL"#

{-# INLINE is_VkSetHdrMetadataEXT #-}

is_VkSetHdrMetadataEXT :: CString -> Bool
is_VkSetHdrMetadataEXT = (EQ ==) . cmpCStrings _VkSetHdrMetadataEXT

type VkSetHdrMetadataEXT = "vkSetHdrMetadataEXT"

-- | > () vkSetHdrMetadataEXT
--   >     ( VkDevice device
--   >     , uint32_t swapchainCount
--   >     , const VkSwapchainKHR* pSwapchains
--   >     , const VkHdrMetadataEXT* pMetadata
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSetHdrMetadataEXT.html vkSetHdrMetadataEXT registry at www.khronos.org>
foreign import ccall unsafe "vkSetHdrMetadataEXT"
               vkSetHdrMetadataEXT ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ swapchainCount
                        -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                              -> Ptr VkHdrMetadataEXT -- ^ pMetadata
                                                                      -> IO ()

-- | > () vkSetHdrMetadataEXT
--   >     ( VkDevice device
--   >     , uint32_t swapchainCount
--   >     , const VkSwapchainKHR* pSwapchains
--   >     , const VkHdrMetadataEXT* pMetadata
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSetHdrMetadataEXT.html vkSetHdrMetadataEXT registry at www.khronos.org>
foreign import ccall safe "vkSetHdrMetadataEXT"
               vkSetHdrMetadataEXTSafe ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ swapchainCount
                        -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                              -> Ptr VkHdrMetadataEXT -- ^ pMetadata
                                                                      -> IO ()

-- | > () vkSetHdrMetadataEXT
--   >     ( VkDevice device
--   >     , uint32_t swapchainCount
--   >     , const VkSwapchainKHR* pSwapchains
--   >     , const VkHdrMetadataEXT* pMetadata
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSetHdrMetadataEXT.html vkSetHdrMetadataEXT registry at www.khronos.org>
type HS_vkSetHdrMetadataEXT =
     VkDevice -- ^ device
              ->
       Word32 -- ^ swapchainCount
              -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                    -> Ptr VkHdrMetadataEXT -- ^ pMetadata
                                                            -> IO ()

type PFN_vkSetHdrMetadataEXT = FunPtr HS_vkSetHdrMetadataEXT

foreign import ccall "dynamic" unwrapVkSetHdrMetadataEXT ::
               PFN_vkSetHdrMetadataEXT -> HS_vkSetHdrMetadataEXT

instance VulkanProc "vkSetHdrMetadataEXT" where
        type VkProcType "vkSetHdrMetadataEXT" = HS_vkSetHdrMetadataEXT
        vkProcSymbol = _VkSetHdrMetadataEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkSetHdrMetadataEXT

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_EXT_HDR_METADATA_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_HDR_METADATA_SPEC_VERSION = 1

type VK_EXT_HDR_METADATA_SPEC_VERSION = 1

pattern VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString

pattern VK_EXT_HDR_METADATA_EXTENSION_NAME <-
        (is_VK_EXT_HDR_METADATA_EXTENSION_NAME -> True)
  where VK_EXT_HDR_METADATA_EXTENSION_NAME
          = _VK_EXT_HDR_METADATA_EXTENSION_NAME

{-# INLINE _VK_EXT_HDR_METADATA_EXTENSION_NAME #-}

_VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString
_VK_EXT_HDR_METADATA_EXTENSION_NAME
  = Ptr "VK_EXT_hdr_metadata\NUL"#

{-# INLINE is_VK_EXT_HDR_METADATA_EXTENSION_NAME #-}

is_VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_HDR_METADATA_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_HDR_METADATA_EXTENSION_NAME

type VK_EXT_HDR_METADATA_EXTENSION_NAME = "VK_EXT_hdr_metadata"

pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT =
        VkStructureType 1000105000
