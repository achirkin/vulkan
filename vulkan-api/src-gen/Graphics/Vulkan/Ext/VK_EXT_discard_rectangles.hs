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
module Graphics.Vulkan.Ext.VK_EXT_discard_rectangles
       (-- * Vulkan extension: @VK_EXT_discard_rectangles@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell-nv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @100@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.Blend,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.Color,
        module Graphics.Vulkan.Types.Enum.CompareOp,
        module Graphics.Vulkan.Types.Enum.CullModeFlags,
        module Graphics.Vulkan.Types.Enum.DiscardRectangleModeEXT,
        module Graphics.Vulkan.Types.Enum.DynamicState,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Enum.Format,
        module Graphics.Vulkan.Types.Enum.FrontFace,
        module Graphics.Vulkan.Types.Struct.Pipeline,
        module Graphics.Vulkan.Types.Enum.LogicOp,
        module Graphics.Vulkan.Types.Struct.Offset,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Enum.PhysicalDeviceType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Enum.Pipeline,
        module Graphics.Vulkan.Types.Enum.PolygonMode,
        module Graphics.Vulkan.Types.Enum.PrimitiveTopology,
        module Graphics.Vulkan.Types.Struct.Rect,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Enum.Shader,
        module Graphics.Vulkan.Types.Struct.Specialization,
        module Graphics.Vulkan.Types.Enum.Stencil,
        module Graphics.Vulkan.Types.Struct.StencilOpState,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Struct.VertexInput,
        module Graphics.Vulkan.Types.Enum.VertexInputRate,
        module Graphics.Vulkan.Types.Struct.Viewport,
        -- > #include "vk_platform.h"
        VkCmdSetDiscardRectangleEXT, pattern VkCmdSetDiscardRectangleEXT,
        HS_vkCmdSetDiscardRectangleEXT, PFN_vkCmdSetDiscardRectangleEXT,
        module Graphics.Vulkan.Types.Handles,
        VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION,
        pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION,
        VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME,
        pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT,
        pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT)
       where
import           GHC.Ptr                                            (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                       (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.Blend
import           Graphics.Vulkan.Types.Enum.Color
import           Graphics.Vulkan.Types.Enum.CompareOp
import           Graphics.Vulkan.Types.Enum.CullModeFlags
import           Graphics.Vulkan.Types.Enum.DiscardRectangleModeEXT
import           Graphics.Vulkan.Types.Enum.DynamicState
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.FrontFace
import           Graphics.Vulkan.Types.Enum.LogicOp
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.Pipeline
import           Graphics.Vulkan.Types.Enum.PolygonMode
import           Graphics.Vulkan.Types.Enum.PrimitiveTopology
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.Shader
import           Graphics.Vulkan.Types.Enum.Stencil
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.VertexInputRate
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Offset
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.Pipeline
import           Graphics.Vulkan.Types.Struct.Rect
import           Graphics.Vulkan.Types.Struct.Specialization
import           Graphics.Vulkan.Types.Struct.StencilOpState
import           Graphics.Vulkan.Types.Struct.VertexInput
import           Graphics.Vulkan.Types.Struct.Viewport

pattern VkCmdSetDiscardRectangleEXT :: CString

pattern VkCmdSetDiscardRectangleEXT <-
        (is_VkCmdSetDiscardRectangleEXT -> True)
  where VkCmdSetDiscardRectangleEXT = _VkCmdSetDiscardRectangleEXT

{-# INLINE _VkCmdSetDiscardRectangleEXT #-}

_VkCmdSetDiscardRectangleEXT :: CString
_VkCmdSetDiscardRectangleEXT
  = Ptr "vkCmdSetDiscardRectangleEXT\NUL"#

{-# INLINE is_VkCmdSetDiscardRectangleEXT #-}

is_VkCmdSetDiscardRectangleEXT :: CString -> Bool
is_VkCmdSetDiscardRectangleEXT
  = (EQ ==) . cmpCStrings _VkCmdSetDiscardRectangleEXT

type VkCmdSetDiscardRectangleEXT = "vkCmdSetDiscardRectangleEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetDiscardRectangleEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstDiscardRectangle
--   >     , uint32_t discardRectangleCount
--   >     , const VkRect2D* pDiscardRectangles
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDiscardRectangleEXT vkCmdSetDiscardRectangleEXT registry at www.khronos.org>
type HS_vkCmdSetDiscardRectangleEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ firstDiscardRectangle
                               -> Word32 -- ^ discardRectangleCount
                                         -> Ptr VkRect2D -- ^ pDiscardRectangles
                                                         -> IO ()

type PFN_vkCmdSetDiscardRectangleEXT =
     FunPtr HS_vkCmdSetDiscardRectangleEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetDiscardRectangleEXTUnsafe ::
               PFN_vkCmdSetDiscardRectangleEXT -> HS_vkCmdSetDiscardRectangleEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetDiscardRectangleEXTSafe ::
               PFN_vkCmdSetDiscardRectangleEXT -> HS_vkCmdSetDiscardRectangleEXT

instance VulkanProc "vkCmdSetDiscardRectangleEXT" where
        type VkProcType "vkCmdSetDiscardRectangleEXT" =
             HS_vkCmdSetDiscardRectangleEXT
        vkProcSymbol = _VkCmdSetDiscardRectangleEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCmdSetDiscardRectangleEXTUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetDiscardRectangleEXTSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION = 1

type VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION = 1

pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME :: CString

pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME <-
        (is_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME -> True)
  where VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
          = _VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME

{-# INLINE _VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME #-}

_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME :: CString
_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  = Ptr "VK_EXT_discard_rectangles\NUL"#

{-# INLINE is_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME #-}

is_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME

type VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME =
     "VK_EXT_discard_rectangles"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
        = VkStructureType 1000099000

pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
        = VkStructureType 1000099001

pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT =
        VkDynamicState 1000099000
