{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NV_clip_space_w_scaling
       (-- * Vulkan extension: @VK_NV_clip_space_w_scaling@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Eric Werness @ewerness-nv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @88@
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Struct.Offset,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.Pipeline,
        module Graphics.Vulkan.Types.Struct.Rect,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Struct.Viewport,
        -- > #include "vk_platform.h"
        VkCmdSetViewportWScalingNV, pattern VkCmdSetViewportWScalingNV,
        HS_vkCmdSetViewportWScalingNV, PFN_vkCmdSetViewportWScalingNV,
        module Graphics.Vulkan.Types.Handles,
        VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION,
        pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION,
        VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME,
        pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV,
        pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV)
       where
import           GHC.Ptr                                  (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc             (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.DynamicState  (VkDynamicState (..))
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Offset
import           Graphics.Vulkan.Types.Struct.Pipeline
import           Graphics.Vulkan.Types.Struct.Rect
import           Graphics.Vulkan.Types.Struct.Viewport

pattern VkCmdSetViewportWScalingNV :: CString

pattern VkCmdSetViewportWScalingNV <-
        (is_VkCmdSetViewportWScalingNV -> True)
  where VkCmdSetViewportWScalingNV = _VkCmdSetViewportWScalingNV

{-# INLINE _VkCmdSetViewportWScalingNV #-}

_VkCmdSetViewportWScalingNV :: CString
_VkCmdSetViewportWScalingNV = Ptr "vkCmdSetViewportWScalingNV\NUL"#

{-# INLINE is_VkCmdSetViewportWScalingNV #-}

is_VkCmdSetViewportWScalingNV :: CString -> Bool
is_VkCmdSetViewportWScalingNV
  = (EQ ==) . cmpCStrings _VkCmdSetViewportWScalingNV

type VkCmdSetViewportWScalingNV = "vkCmdSetViewportWScalingNV"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetViewportWScalingNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewportWScalingNV* pViewportWScalings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetViewportWScalingNV vkCmdSetViewportWScalingNV registry at www.khronos.org>
type HS_vkCmdSetViewportWScalingNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ firstViewport
              -> Word32 -- ^ viewportCount
                        -> Ptr VkViewportWScalingNV -- ^ pViewportWScalings
                                                    -> IO ()

type PFN_vkCmdSetViewportWScalingNV =
     FunPtr HS_vkCmdSetViewportWScalingNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetViewportWScalingNVUnsafe ::
               PFN_vkCmdSetViewportWScalingNV -> HS_vkCmdSetViewportWScalingNV

foreign import ccall safe "dynamic"
               unwrapVkCmdSetViewportWScalingNVSafe ::
               PFN_vkCmdSetViewportWScalingNV -> HS_vkCmdSetViewportWScalingNV

instance VulkanProc "vkCmdSetViewportWScalingNV" where
        type VkProcType "vkCmdSetViewportWScalingNV" =
             HS_vkCmdSetViewportWScalingNV
        vkProcSymbol = _VkCmdSetViewportWScalingNV

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCmdSetViewportWScalingNVUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetViewportWScalingNVSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
