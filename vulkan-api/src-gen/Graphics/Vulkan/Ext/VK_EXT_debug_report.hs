{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_debug_report
       (-- * Vulkan extension: @VK_EXT_debug_report@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Courtney Goeltzenleuchter @courtney@
        --
        -- author: @GOOGLE@
        --
        -- type: @instance@
        --
        -- Extension number: @12@
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Struct.VkApplicationInfo,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkDebugReportCallbackCreateInfoEXT,
        module Graphics.Vulkan.Types.Enum.VkDebugReportFlagsEXT,
        module Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkInstanceCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkCreateDebugReportCallbackEXT, vkDestroyDebugReportCallbackEXT,
        vkDebugReportMessageEXT,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_EXT_DEBUG_REPORT_SPEC_VERSION,
        pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION,
        VK_EXT_DEBUG_REPORT_EXTENSION_NAME,
        pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT,
        pattern VK_ERROR_VALIDATION_FAILED_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT,
        pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT)
       where
import           GHC.Ptr                                                         (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkDebugReportFlagsEXT
import           Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkObjectType                         (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkApplicationInfo
import           Graphics.Vulkan.Types.Struct.VkDebugReportCallbackCreateInfoEXT
import           Graphics.Vulkan.Types.Struct.VkInstanceCreateInfo

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateDebugReportCallbackEXT
--   >     ( VkInstance instance
--   >     , const VkDebugReportCallbackCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDebugReportCallbackEXT* pCallback
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateDebugReportCallbackEXT.html vkCreateDebugReportCallbackEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDebugReportCallbackEXT"
               vkCreateDebugReportCallbackEXT ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkDebugReportCallbackCreateInfoEXT -- ^ pCreateInfo
                                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDebugReportCallbackEXT -- ^ pCallback
                                                  -> IO VkResult

-- | > () vkDestroyDebugReportCallbackEXT
--   >     ( VkInstance instance
--   >     , VkDebugReportCallbackEXT callback
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDestroyDebugReportCallbackEXT.html vkDestroyDebugReportCallbackEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDebugReportCallbackEXT"
               vkDestroyDebugReportCallbackEXT ::
               VkInstance -- ^ instance
                          ->
                 VkDebugReportCallbackEXT -- ^ callback
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkDebugReportMessageEXT
--   >     ( VkInstance instance
--   >     , VkDebugReportFlagsEXT flags
--   >     , VkDebugReportObjectTypeEXT objectType
--   >     , uint64_t object
--   >     , size_t location
--   >     , int32_t messageCode
--   >     , const char* pLayerPrefix
--   >     , const char* pMessage
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDebugReportMessageEXT.html vkDebugReportMessageEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugReportMessageEXT"
               vkDebugReportMessageEXT ::
               VkInstance -- ^ instance
                          ->
                 VkDebugReportFlagsEXT -- ^ flags
                                       ->
                   VkDebugReportObjectTypeEXT -- ^ objectType
                                              ->
                     Word64 -- ^ object
                            -> CSize -- ^ location
                                     -> Int32 -- ^ messageCode
                                              -> CString -- ^ pLayerPrefix
                                                         -> CString -- ^ pMessage
                                                                    -> IO ()

pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION = 9

type VK_EXT_DEBUG_REPORT_SPEC_VERSION = 9

pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: CString

pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME <-
        (is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME -> True)
  where VK_EXT_DEBUG_REPORT_EXTENSION_NAME
          = _VK_EXT_DEBUG_REPORT_EXTENSION_NAME

{-# INLINE _VK_EXT_DEBUG_REPORT_EXTENSION_NAME #-}

_VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: CString
_VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  = Ptr "VK_EXT_debug_report\NUL"#

{-# INLINE is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME #-}

is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  = eqCStrings _VK_EXT_DEBUG_REPORT_EXTENSION_NAME

type VK_EXT_DEBUG_REPORT_EXTENSION_NAME = "VK_EXT_debug_report"

pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT =
        VkStructureType 1000011000

pattern VK_ERROR_VALIDATION_FAILED_EXT :: VkResult

pattern VK_ERROR_VALIDATION_FAILED_EXT = VkResult (-1000011001)

pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT =
        VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT

-- | VkDebugReportCallbackEXT
pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT :: VkObjectType

pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT =
        VkObjectType 1000011000

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT =
        VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
