{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_debug_utils
       (-- * Vulkan extension: @VK_EXT_debug_utils@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Mark Young @MarkY_LunarG@
        --
        -- author: @EXT@
        --
        -- type: @instance@
        --
        -- Extension number: @129@
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Struct.VkApplicationInfo,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkDebugUtilsLabelEXT,
        module Graphics.Vulkan.Types.Enum.VkDebugUtilsMessageSeverityFlagsEXT,
        module Graphics.Vulkan.Types.Enum.VkDebugUtilsMessageTypeFlagsEXT,
        module Graphics.Vulkan.Types.Struct.VkDebugUtilsMessengerCallbackDataEXT,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkDebugUtilsMessengerCreateInfoEXT,
        module Graphics.Vulkan.Types.Struct.VkDebugUtilsObjectNameInfoEXT,
        module Graphics.Vulkan.Types.Struct.VkDebugUtilsObjectTagInfoEXT,
        module Graphics.Vulkan.Types.Struct.VkInstanceCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkObjectType,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkSetDebugUtilsObjectNameEXT, vkSetDebugUtilsObjectNameEXTSafe,
        vkSetDebugUtilsObjectTagEXT, vkSetDebugUtilsObjectTagEXTSafe,
        vkQueueBeginDebugUtilsLabelEXT, vkQueueBeginDebugUtilsLabelEXTSafe,
        vkQueueEndDebugUtilsLabelEXT, vkQueueEndDebugUtilsLabelEXTSafe,
        vkQueueInsertDebugUtilsLabelEXT,
        vkQueueInsertDebugUtilsLabelEXTSafe, vkCmdBeginDebugUtilsLabelEXT,
        vkCmdBeginDebugUtilsLabelEXTSafe, vkCmdEndDebugUtilsLabelEXT,
        vkCmdEndDebugUtilsLabelEXTSafe, vkCmdInsertDebugUtilsLabelEXT,
        vkCmdInsertDebugUtilsLabelEXTSafe, vkCreateDebugUtilsMessengerEXT,
        vkCreateDebugUtilsMessengerEXTSafe,
        vkDestroyDebugUtilsMessengerEXT,
        vkDestroyDebugUtilsMessengerEXTSafe, vkSubmitDebugUtilsMessageEXT,
        vkSubmitDebugUtilsMessageEXTSafe,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_EXT_DEBUG_UTILS_SPEC_VERSION,
        pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION,
        VK_EXT_DEBUG_UTILS_EXTENSION_NAME,
        pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
        pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT)
       where
import           GHC.Ptr
                                                                                    (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkDebugUtilsMessageSeverityFlagsEXT
import           Graphics.Vulkan.Types.Enum.VkDebugUtilsMessageTypeFlagsEXT
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkObjectType
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkApplicationInfo
import           Graphics.Vulkan.Types.Struct.VkDebugUtilsLabelEXT
import           Graphics.Vulkan.Types.Struct.VkDebugUtilsMessengerCallbackDataEXT
import           Graphics.Vulkan.Types.Struct.VkDebugUtilsMessengerCreateInfoEXT
import           Graphics.Vulkan.Types.Struct.VkDebugUtilsObjectNameInfoEXT
import           Graphics.Vulkan.Types.Struct.VkDebugUtilsObjectTagInfoEXT
import           Graphics.Vulkan.Types.Struct.VkInstanceCreateInfo

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetDebugUtilsObjectNameEXT
--   >     ( VkDevice device
--   >     , const VkDebugUtilsObjectNameInfoEXT* pNameInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSetDebugUtilsObjectNameEXT.html vkSetDebugUtilsObjectNameEXT registry at www.khronos.org>
foreign import ccall unsafe "vkSetDebugUtilsObjectNameEXT"
               vkSetDebugUtilsObjectNameEXT ::
               VkDevice -- ^ device
                        -> Ptr VkDebugUtilsObjectNameInfoEXT -- ^ pNameInfo
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetDebugUtilsObjectNameEXT
--   >     ( VkDevice device
--   >     , const VkDebugUtilsObjectNameInfoEXT* pNameInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSetDebugUtilsObjectNameEXT.html vkSetDebugUtilsObjectNameEXT registry at www.khronos.org>
foreign import ccall safe "vkSetDebugUtilsObjectNameEXT"
               vkSetDebugUtilsObjectNameEXTSafe ::
               VkDevice -- ^ device
                        -> Ptr VkDebugUtilsObjectNameInfoEXT -- ^ pNameInfo
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetDebugUtilsObjectTagEXT
--   >     ( VkDevice device
--   >     , const VkDebugUtilsObjectTagInfoEXT* pTagInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSetDebugUtilsObjectTagEXT.html vkSetDebugUtilsObjectTagEXT registry at www.khronos.org>
foreign import ccall unsafe "vkSetDebugUtilsObjectTagEXT"
               vkSetDebugUtilsObjectTagEXT ::
               VkDevice -- ^ device
                        -> Ptr VkDebugUtilsObjectTagInfoEXT -- ^ pTagInfo
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetDebugUtilsObjectTagEXT
--   >     ( VkDevice device
--   >     , const VkDebugUtilsObjectTagInfoEXT* pTagInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSetDebugUtilsObjectTagEXT.html vkSetDebugUtilsObjectTagEXT registry at www.khronos.org>
foreign import ccall safe "vkSetDebugUtilsObjectTagEXT"
               vkSetDebugUtilsObjectTagEXTSafe ::
               VkDevice -- ^ device
                        -> Ptr VkDebugUtilsObjectTagInfoEXT -- ^ pTagInfo
                                                            -> IO VkResult

-- | > () vkQueueBeginDebugUtilsLabelEXT
--   >     ( VkQueue queue
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueBeginDebugUtilsLabelEXT.html vkQueueBeginDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall unsafe "vkQueueBeginDebugUtilsLabelEXT"
               vkQueueBeginDebugUtilsLabelEXT ::
               VkQueue -- ^ queue
                       -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                                   -> IO ()

-- | > () vkQueueBeginDebugUtilsLabelEXT
--   >     ( VkQueue queue
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueBeginDebugUtilsLabelEXT.html vkQueueBeginDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall safe "vkQueueBeginDebugUtilsLabelEXT"
               vkQueueBeginDebugUtilsLabelEXTSafe ::
               VkQueue -- ^ queue
                       -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                                   -> IO ()

-- | > () vkQueueEndDebugUtilsLabelEXT
--   >     ( VkQueue queue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueEndDebugUtilsLabelEXT.html vkQueueEndDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall unsafe "vkQueueEndDebugUtilsLabelEXT"
               vkQueueEndDebugUtilsLabelEXT :: VkQueue -- ^ queue
                                                       -> IO ()

-- | > () vkQueueEndDebugUtilsLabelEXT
--   >     ( VkQueue queue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueEndDebugUtilsLabelEXT.html vkQueueEndDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall safe "vkQueueEndDebugUtilsLabelEXT"
               vkQueueEndDebugUtilsLabelEXTSafe :: VkQueue -- ^ queue
                                                           -> IO ()

-- | > () vkQueueInsertDebugUtilsLabelEXT
--   >     ( VkQueue queue
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueInsertDebugUtilsLabelEXT.html vkQueueInsertDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall unsafe "vkQueueInsertDebugUtilsLabelEXT"
               vkQueueInsertDebugUtilsLabelEXT ::
               VkQueue -- ^ queue
                       -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                                   -> IO ()

-- | > () vkQueueInsertDebugUtilsLabelEXT
--   >     ( VkQueue queue
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueInsertDebugUtilsLabelEXT.html vkQueueInsertDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall safe "vkQueueInsertDebugUtilsLabelEXT"
               vkQueueInsertDebugUtilsLabelEXTSafe ::
               VkQueue -- ^ queue
                       -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                                   -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBeginDebugUtilsLabelEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBeginDebugUtilsLabelEXT.html vkCmdBeginDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBeginDebugUtilsLabelEXT"
               vkCmdBeginDebugUtilsLabelEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                                           -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBeginDebugUtilsLabelEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBeginDebugUtilsLabelEXT.html vkCmdBeginDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall safe "vkCmdBeginDebugUtilsLabelEXT"
               vkCmdBeginDebugUtilsLabelEXTSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                                           -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdEndDebugUtilsLabelEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdEndDebugUtilsLabelEXT.html vkCmdEndDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdEndDebugUtilsLabelEXT"
               vkCmdEndDebugUtilsLabelEXT :: VkCommandBuffer -- ^ commandBuffer
                                                             -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdEndDebugUtilsLabelEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdEndDebugUtilsLabelEXT.html vkCmdEndDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall safe "vkCmdEndDebugUtilsLabelEXT"
               vkCmdEndDebugUtilsLabelEXTSafe :: VkCommandBuffer -- ^ commandBuffer
                                                                 -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdInsertDebugUtilsLabelEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdInsertDebugUtilsLabelEXT.html vkCmdInsertDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdInsertDebugUtilsLabelEXT"
               vkCmdInsertDebugUtilsLabelEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                                           -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdInsertDebugUtilsLabelEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdInsertDebugUtilsLabelEXT.html vkCmdInsertDebugUtilsLabelEXT registry at www.khronos.org>
foreign import ccall safe "vkCmdInsertDebugUtilsLabelEXT"
               vkCmdInsertDebugUtilsLabelEXTSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                                           -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateDebugUtilsMessengerEXT
--   >     ( VkInstance instance
--   >     , const VkDebugUtilsMessengerCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDebugUtilsMessengerEXT* pMessenger
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDebugUtilsMessengerEXT.html vkCreateDebugUtilsMessengerEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDebugUtilsMessengerEXT"
               vkCreateDebugUtilsMessengerEXT ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkDebugUtilsMessengerCreateInfoEXT -- ^ pCreateInfo
                                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDebugUtilsMessengerEXT -- ^ pMessenger
                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateDebugUtilsMessengerEXT
--   >     ( VkInstance instance
--   >     , const VkDebugUtilsMessengerCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDebugUtilsMessengerEXT* pMessenger
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDebugUtilsMessengerEXT.html vkCreateDebugUtilsMessengerEXT registry at www.khronos.org>
foreign import ccall safe "vkCreateDebugUtilsMessengerEXT"
               vkCreateDebugUtilsMessengerEXTSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkDebugUtilsMessengerCreateInfoEXT -- ^ pCreateInfo
                                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDebugUtilsMessengerEXT -- ^ pMessenger
                                                  -> IO VkResult

-- | > () vkDestroyDebugUtilsMessengerEXT
--   >     ( VkInstance instance
--   >     , VkDebugUtilsMessengerEXT messenger
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDebugUtilsMessengerEXT.html vkDestroyDebugUtilsMessengerEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDebugUtilsMessengerEXT"
               vkDestroyDebugUtilsMessengerEXT ::
               VkInstance -- ^ instance
                          ->
                 VkDebugUtilsMessengerEXT -- ^ messenger
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkDestroyDebugUtilsMessengerEXT
--   >     ( VkInstance instance
--   >     , VkDebugUtilsMessengerEXT messenger
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDebugUtilsMessengerEXT.html vkDestroyDebugUtilsMessengerEXT registry at www.khronos.org>
foreign import ccall safe "vkDestroyDebugUtilsMessengerEXT"
               vkDestroyDebugUtilsMessengerEXTSafe ::
               VkInstance -- ^ instance
                          ->
                 VkDebugUtilsMessengerEXT -- ^ messenger
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkSubmitDebugUtilsMessageEXT
--   >     ( VkInstance instance
--   >     , VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity
--   >     , VkDebugUtilsMessageTypeFlagsEXT messageTypes
--   >     , const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSubmitDebugUtilsMessageEXT.html vkSubmitDebugUtilsMessageEXT registry at www.khronos.org>
foreign import ccall unsafe "vkSubmitDebugUtilsMessageEXT"
               vkSubmitDebugUtilsMessageEXT ::
               VkInstance -- ^ instance
                          ->
                 VkDebugUtilsMessageSeverityFlagBitsEXT -- ^ messageSeverity
                                                        ->
                   VkDebugUtilsMessageTypeFlagsEXT -- ^ messageTypes
                                                   ->
                     Ptr VkDebugUtilsMessengerCallbackDataEXT -- ^ pCallbackData
                                                              -> IO ()

-- | > () vkSubmitDebugUtilsMessageEXT
--   >     ( VkInstance instance
--   >     , VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity
--   >     , VkDebugUtilsMessageTypeFlagsEXT messageTypes
--   >     , const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSubmitDebugUtilsMessageEXT.html vkSubmitDebugUtilsMessageEXT registry at www.khronos.org>
foreign import ccall safe "vkSubmitDebugUtilsMessageEXT"
               vkSubmitDebugUtilsMessageEXTSafe ::
               VkInstance -- ^ instance
                          ->
                 VkDebugUtilsMessageSeverityFlagBitsEXT -- ^ messageSeverity
                                                        ->
                   VkDebugUtilsMessageTypeFlagsEXT -- ^ messageTypes
                                                   ->
                     Ptr VkDebugUtilsMessengerCallbackDataEXT -- ^ pCallbackData
                                                              -> IO ()

pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION = 1

type VK_EXT_DEBUG_UTILS_SPEC_VERSION = 1

pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME :: CString

pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME <-
        (is_VK_EXT_DEBUG_UTILS_EXTENSION_NAME -> True)
  where VK_EXT_DEBUG_UTILS_EXTENSION_NAME
          = _VK_EXT_DEBUG_UTILS_EXTENSION_NAME

{-# INLINE _VK_EXT_DEBUG_UTILS_EXTENSION_NAME #-}

_VK_EXT_DEBUG_UTILS_EXTENSION_NAME :: CString
_VK_EXT_DEBUG_UTILS_EXTENSION_NAME = Ptr "VK_EXT_debug_utils\NUL"#

{-# INLINE is_VK_EXT_DEBUG_UTILS_EXTENSION_NAME #-}

is_VK_EXT_DEBUG_UTILS_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DEBUG_UTILS_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DEBUG_UTILS_EXTENSION_NAME

type VK_EXT_DEBUG_UTILS_EXTENSION_NAME = "VK_EXT_debug_utils"

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT =
        VkStructureType 1000128000

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT =
        VkStructureType 1000128001

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT =
        VkStructureType 1000128002

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT =
        VkStructureType 1000128003

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT =
        VkStructureType 1000128004

-- | VkDebugUtilsMessengerEXT
pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT :: VkObjectType

pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT =
        VkObjectType 1000128000
