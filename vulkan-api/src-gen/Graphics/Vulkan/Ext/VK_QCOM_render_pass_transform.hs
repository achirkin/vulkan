{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_QCOM_render_pass_transform
       (AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkClearColorValue, VkClearDepthStencilValue,
        VkClearValue, VkCommandBufferInheritanceInfo,
        VkCommandBufferInheritanceRenderPassTransformInfoQCOM, VkExtent2D,
        VkOffset2D, VkQueryControlBitmask(..),
        VkQueryPipelineStatisticBitmask(..),
        VkQueryPoolSamplingModeINTEL(..), VkQueryResultBitmask(..),
        VkQueryType(..), VkQueryControlFlagBits(), VkQueryControlFlags(),
        VkQueryPipelineStatisticFlagBits(),
        VkQueryPipelineStatisticFlags(), VkQueryPoolCreateFlagBits(..),
        VkQueryResultFlagBits(), VkQueryResultFlags(), VkRect2D,
        VkRenderPassBeginInfo, VkRenderPassTransformBeginInfoQCOM,
        VkStructureType(..), VkSurfaceCounterBitmaskEXT(..),
        VkSurfaceTransformBitmaskKHR(..), VkSurfaceCounterFlagBitsEXT(),
        VkSurfaceCounterFlagsEXT(), VkSurfaceTransformFlagBitsKHR(),
        VkSurfaceTransformFlagsKHR(),
        -- > #include "vk_platform.h"
        VK_QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION,
        pattern VK_QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION,
        VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME,
        pattern VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM,
        pattern VK_RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM)
       where
import GHC.Ptr                                          (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.Query
import Graphics.Vulkan.Types.Enum.RenderPassCreateFlags (VkRenderPassCreateBitmask (..))
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.Surface
import Graphics.Vulkan.Types.Struct.Clear               (VkClearColorValue, VkClearDepthStencilValue,
                                                         VkClearValue)
import Graphics.Vulkan.Types.Struct.Command             (VkCommandBufferInheritanceInfo,
                                                         VkCommandBufferInheritanceRenderPassTransformInfoQCOM)
import Graphics.Vulkan.Types.Struct.Extent              (VkExtent2D)
import Graphics.Vulkan.Types.Struct.Offset              (VkOffset2D)
import Graphics.Vulkan.Types.Struct.Rect                (VkRect2D)
import Graphics.Vulkan.Types.Struct.RenderPass          (VkRenderPassBeginInfo, VkRenderPassTransformBeginInfoQCOM)

pattern VK_QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION = 1

type VK_QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION = 1

pattern VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME :: CString

pattern VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME <-
        (is_VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME -> True)
  where
    VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME
      = _VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME

{-# INLINE _VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME #-}

_VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME :: CString
_VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME
  = Ptr "VK_QCOM_render_pass_transform\NUL"#

{-# INLINE is_VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME #-}

is_VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME :: CString -> Bool
is_VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME

type VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME =
     "VK_QCOM_render_pass_transform"

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM
        = VkStructureType 1000282000

pattern VK_STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM =
        VkStructureType 1000282001

-- | bitpos = @1@
pattern VK_RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM ::
        VkRenderPassCreateBitmask a

pattern VK_RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM =
        VkRenderPassCreateBitmask 2
