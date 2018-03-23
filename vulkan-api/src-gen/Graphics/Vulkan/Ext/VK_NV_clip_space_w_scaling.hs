{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NV_clip_space_w_scaling
       (-- * Vulkan extension: @VK_NV_clip_space_w_scaling@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Eric Werness @ewerness@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @88@
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Struct.VkOffset2D,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineViewportWScalingStateCreateInfoNV,
        module Graphics.Vulkan.Types.Struct.VkRect2D,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkViewport,
        module Graphics.Vulkan.Types.Struct.VkViewportWScalingNV,
        -- > #include "vk_platform.h"
        vkCmdSetViewportWScalingNV, vkCmdSetViewportWScalingNVSafe,
        module Graphics.Vulkan.Types.Handles,
        VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION,
        pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION,
        VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME,
        pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV,
        pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV)
       where
import           GHC.Ptr
                                                                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkDynamicState
                                                                                           (VkDynamicState (..))
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkOffset2D
import           Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineViewportWScalingStateCreateInfoNV
import           Graphics.Vulkan.Types.Struct.VkRect2D
import           Graphics.Vulkan.Types.Struct.VkViewport
import           Graphics.Vulkan.Types.Struct.VkViewportWScalingNV

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetViewportWScalingNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewportWScalingNV* pViewportWScalings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetViewportWScalingNV.html vkCmdSetViewportWScalingNV registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetViewportWScalingNV"
               vkCmdSetViewportWScalingNV ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ firstViewport
                        -> Word32 -- ^ viewportCount
                                  -> Ptr VkViewportWScalingNV -- ^ pViewportWScalings
                                                              -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetViewportWScalingNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewportWScalingNV* pViewportWScalings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetViewportWScalingNV.html vkCmdSetViewportWScalingNV registry at www.khronos.org>
foreign import ccall safe "vkCmdSetViewportWScalingNV"
               vkCmdSetViewportWScalingNVSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ firstViewport
                        -> Word32 -- ^ viewportCount
                                  -> Ptr VkViewportWScalingNV -- ^ pViewportWScalings
                                                              -> IO ()

pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1

type VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1

pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString

pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME <-
        (is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME -> True)
  where VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
          = _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME

{-# INLINE _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME #-}

_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString
_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  = Ptr "VK_NV_clip_space_w_scaling\NUL"#

{-# INLINE is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME #-}

is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString -> Bool
is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME

type VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME =
     "VK_NV_clip_space_w_scaling"

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
        = VkStructureType 1000087000

pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: VkDynamicState

pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV =
        VkDynamicState 1000087000
