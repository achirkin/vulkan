{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_debug_marker
       (-- * Vulkan extension: @VK_EXT_debug_marker@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @baldurk@baldurk.org@
        --
        -- author: @Baldur Karlsson@
        --
        -- type: @device@
        --
        -- Extension number: @23@
        --
        -- Required extensions: 'VK_EXT_debug_report'.
        --

        -- ** Required extensions: 'VK_EXT_debug_report'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Struct.VkDebugMarkerMarkerInfoEXT,
        module Graphics.Vulkan.Types.Struct.VkDebugMarkerObjectNameInfoEXT,
        module Graphics.Vulkan.Types.Struct.VkDebugMarkerObjectTagInfoEXT,
        module Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkDebugMarkerSetObjectTagEXT, vkDebugMarkerSetObjectNameEXT,
        vkCmdDebugMarkerBeginEXT, vkCmdDebugMarkerEndEXT,
        vkCmdDebugMarkerInsertEXT,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_EXT_DEBUG_MARKER_SPEC_VERSION,
        pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION,
        VK_EXT_DEBUG_MARKER_EXTENSION_NAME,
        pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT)
       where
import           GHC.Ptr                                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkDebugMarkerMarkerInfoEXT
import           Graphics.Vulkan.Types.Struct.VkDebugMarkerObjectNameInfoEXT
import           Graphics.Vulkan.Types.Struct.VkDebugMarkerObjectTagInfoEXT

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDebugMarkerSetObjectTagEXT
--   >     ( VkDevice device
--   >     , const VkDebugMarkerObjectTagInfoEXT* pTagInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDebugMarkerSetObjectTagEXT.html vkDebugMarkerSetObjectTagEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugMarkerSetObjectTagEXT"
               vkDebugMarkerSetObjectTagEXT ::
               VkDevice -- ^ device
                        -> Ptr VkDebugMarkerObjectTagInfoEXT -- ^ pTagInfo
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDebugMarkerSetObjectNameEXT
--   >     ( VkDevice device
--   >     , const VkDebugMarkerObjectNameInfoEXT* pNameInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDebugMarkerSetObjectNameEXT.html vkDebugMarkerSetObjectNameEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugMarkerSetObjectNameEXT"
               vkDebugMarkerSetObjectNameEXT ::
               VkDevice -- ^ device
                        -> Ptr VkDebugMarkerObjectNameInfoEXT -- ^ pNameInfo
                                                              -> IO VkResult

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdDebugMarkerBeginEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugMarkerMarkerInfoEXT* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCmdDebugMarkerBeginEXT.html vkCmdDebugMarkerBeginEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerBeginEXT"
               vkCmdDebugMarkerBeginEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkDebugMarkerMarkerInfoEXT -- ^ pMarkerInfo
                                                                 -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdDebugMarkerEndEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCmdDebugMarkerEndEXT.html vkCmdDebugMarkerEndEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerEndEXT"
               vkCmdDebugMarkerEndEXT :: VkCommandBuffer -- ^ commandBuffer
                                                         -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdDebugMarkerInsertEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugMarkerMarkerInfoEXT* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCmdDebugMarkerInsertEXT.html vkCmdDebugMarkerInsertEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerInsertEXT"
               vkCmdDebugMarkerInsertEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkDebugMarkerMarkerInfoEXT -- ^ pMarkerInfo
                                                                 -> IO ()

pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4

type VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4

pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString

pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME <-
        (is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME -> True)
  where VK_EXT_DEBUG_MARKER_EXTENSION_NAME
          = _VK_EXT_DEBUG_MARKER_EXTENSION_NAME

{-# INLINE _VK_EXT_DEBUG_MARKER_EXTENSION_NAME #-}

_VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString
_VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  = Ptr "VK_EXT_debug_marker\NUL"#

{-# INLINE is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME #-}

is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  = eqCStrings _VK_EXT_DEBUG_MARKER_EXTENSION_NAME

type VK_EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT =
        VkStructureType 1000022000

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT =
        VkStructureType 1000022001

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT =
        VkStructureType 1000022002
