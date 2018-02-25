{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_discard_rectangles
       (-- * Vulkan extension: @VK_EXT_discard_rectangles@
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
        -- Extension number: @100@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkBlendFactor,
        module Graphics.Vulkan.Types.Enum.VkBlendOp,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkColorComponentFlags,
        module Graphics.Vulkan.Types.Enum.VkCompareOp,
        module Graphics.Vulkan.Types.Enum.VkCullModeFlags,
        module Graphics.Vulkan.Types.Enum.VkDiscardRectangleModeEXT,
        module Graphics.Vulkan.Types.Enum.VkDynamicState,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkFrontFace,
        module Graphics.Vulkan.Types.Struct.VkGraphicsPipelineCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkLogicOp,
        module Graphics.Vulkan.Types.Struct.VkOffset2D,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceDiscardRectanglePropertiesEXT,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Struct.VkPipelineColorBlendAttachmentState,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkPipelineColorBlendStateCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkPipelineCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkPipelineDepthStencilStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineDiscardRectangleStateCreateInfoEXT,
        module Graphics.Vulkan.Types.Struct.VkPipelineDynamicStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineInputAssemblyStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineShaderStageCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineVertexInputStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkPolygonMode,
        module Graphics.Vulkan.Types.Enum.VkPrimitiveTopology,
        module Graphics.Vulkan.Types.Struct.VkRect2D,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkShaderStageFlags,
        module Graphics.Vulkan.Types.Struct.VkSpecializationInfo,
        module Graphics.Vulkan.Types.Struct.VkSpecializationMapEntry,
        module Graphics.Vulkan.Types.Enum.VkStencilOp,
        module Graphics.Vulkan.Types.Struct.VkStencilOpState,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkVertexInputAttributeDescription,
        module Graphics.Vulkan.Types.Struct.VkVertexInputBindingDescription,
        module Graphics.Vulkan.Types.Enum.VkVertexInputRate,
        module Graphics.Vulkan.Types.Struct.VkViewport,
        -- > #include "vk_platform.h"
        vkCmdSetDiscardRectangleEXT, module Graphics.Vulkan.Types.Handles,
        VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION,
        pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION,
        VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME,
        pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT,
        pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT)
       where
import           GHC.Ptr
                                                                                             (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkBlendFactor
import           Graphics.Vulkan.Types.Enum.VkBlendOp
import           Graphics.Vulkan.Types.Enum.VkColorComponentFlags
import           Graphics.Vulkan.Types.Enum.VkCompareOp
import           Graphics.Vulkan.Types.Enum.VkCullModeFlags
import           Graphics.Vulkan.Types.Enum.VkDiscardRectangleModeEXT
import           Graphics.Vulkan.Types.Enum.VkDynamicState
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkFrontFace
import           Graphics.Vulkan.Types.Enum.VkLogicOp
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkPipelineCreateFlags
import           Graphics.Vulkan.Types.Enum.VkPolygonMode
import           Graphics.Vulkan.Types.Enum.VkPrimitiveTopology
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags
import           Graphics.Vulkan.Types.Enum.VkStencilOp
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkVertexInputRate
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkGraphicsPipelineCreateInfo
import           Graphics.Vulkan.Types.Struct.VkOffset2D
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceDiscardRectanglePropertiesEXT
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
import           Graphics.Vulkan.Types.Struct.VkPipelineColorBlendAttachmentState
import           Graphics.Vulkan.Types.Struct.VkPipelineColorBlendStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineDepthStencilStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineDiscardRectangleStateCreateInfoEXT
import           Graphics.Vulkan.Types.Struct.VkPipelineDynamicStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineInputAssemblyStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineShaderStageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineVertexInputStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkRect2D
import           Graphics.Vulkan.Types.Struct.VkSpecializationInfo
import           Graphics.Vulkan.Types.Struct.VkSpecializationMapEntry
import           Graphics.Vulkan.Types.Struct.VkStencilOpState
import           Graphics.Vulkan.Types.Struct.VkVertexInputAttributeDescription
import           Graphics.Vulkan.Types.Struct.VkVertexInputBindingDescription
import           Graphics.Vulkan.Types.Struct.VkViewport

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetDiscardRectangleEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstDiscardRectangle
--   >     , uint32_t discardRectangleCount
--   >     , const VkRect2D* pDiscardRectangles
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCmdSetDiscardRectangleEXT.html vkCmdSetDiscardRectangleEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDiscardRectangleEXT"
               vkCmdSetDiscardRectangleEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ firstDiscardRectangle
                                         -> Word32 -- ^ discardRectangleCount
                                                   -> Ptr VkRect2D -- ^ pDiscardRectangles
                                                                   -> IO ()

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
