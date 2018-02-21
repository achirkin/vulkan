{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_validation_cache
       (-- * Vulkan extension: @VK_EXT_validation_cache@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Cort Stratton @cdwfs@
        --
        -- author: @GOOGLE@
        --
        -- type: @device@
        --
        -- Extension number: @161@
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkShaderModuleCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkShaderModuleValidationCacheCreateInfoEXT,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkValidationCacheCreateInfoEXT,
        module Graphics.Vulkan.Types.Enum.VkValidationCacheHeaderVersionEXT,
        -- > #include "vk_platform.h"
        vkCreateValidationCacheEXT, vkDestroyValidationCacheEXT,
        vkMergeValidationCachesEXT, vkGetValidationCacheDataEXT,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_EXT_VALIDATION_CACHE_SPEC_VERSION,
        pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION,
        VK_EXT_VALIDATION_CACHE_EXTENSION_NAME,
        pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT,
        pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT)
       where
import           GHC.Ptr
                                                                                          (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT
                                                                                          (VkDebugReportObjectTypeEXT (..))
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkObjectType
                                                                                          (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Enum.VkValidationCacheHeaderVersionEXT
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkShaderModuleCreateInfo
import           Graphics.Vulkan.Types.Struct.VkShaderModuleValidationCacheCreateInfoEXT
import           Graphics.Vulkan.Types.Struct.VkValidationCacheCreateInfoEXT

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateValidationCacheEXT
--   >     ( VkDevice device
--   >     , const VkValidationCacheCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkValidationCacheEXT* pValidationCache
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateValidationCacheEXT.html vkCreateValidationCacheEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCreateValidationCacheEXT"
               vkCreateValidationCacheEXT ::
               VkDevice -- ^ device
                        ->
                 Ptr VkValidationCacheCreateInfoEXT -- ^ pCreateInfo
                                                    ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkValidationCacheEXT -- ^ pValidationCache
                                              -> IO VkResult

-- | > () vkDestroyValidationCacheEXT
--   >     ( VkDevice device
--   >     , VkValidationCacheEXT validationCache
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDestroyValidationCacheEXT.html vkDestroyValidationCacheEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyValidationCacheEXT"
               vkDestroyValidationCacheEXT ::
               VkDevice -- ^ device
                        ->
                 VkValidationCacheEXT -- ^ validationCache
                                      -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                   -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkMergeValidationCachesEXT
--   >     ( VkDevice device
--   >     , VkValidationCacheEXT dstCache
--   >     , uint32_t srcCacheCount
--   >     , const VkValidationCacheEXT* pSrcCaches
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkMergeValidationCachesEXT.html vkMergeValidationCachesEXT registry at www.khronos.org>
foreign import ccall unsafe "vkMergeValidationCachesEXT"
               vkMergeValidationCachesEXT ::
               VkDevice -- ^ device
                        ->
                 VkValidationCacheEXT -- ^ dstCache
                                      ->
                   Word32 -- ^ srcCacheCount
                          -> Ptr VkValidationCacheEXT -- ^ pSrcCaches
                                                      -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetValidationCacheDataEXT
--   >     ( VkDevice device
--   >     , VkValidationCacheEXT validationCache
--   >     , size_t* pDataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetValidationCacheDataEXT.html vkGetValidationCacheDataEXT registry at www.khronos.org>
foreign import ccall unsafe "vkGetValidationCacheDataEXT"
               vkGetValidationCacheDataEXT ::
               VkDevice -- ^ device
                        ->
                 VkValidationCacheEXT -- ^ validationCache
                                      -> Ptr CSize -- ^ pDataSize
                                                   -> Ptr Void -- ^ pData
                                                               -> IO VkResult

pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION = 1

type VK_EXT_VALIDATION_CACHE_SPEC_VERSION = 1

pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME :: CString

pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME <-
        (is_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME -> True)
  where VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
          = _VK_EXT_VALIDATION_CACHE_EXTENSION_NAME

{-# INLINE _VK_EXT_VALIDATION_CACHE_EXTENSION_NAME #-}

_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME :: CString
_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
  = Ptr "VK_EXT_validation_cache\NUL"#

{-# INLINE is_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME #-}

is_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
  = eqCStrings _VK_EXT_VALIDATION_CACHE_EXTENSION_NAME

type VK_EXT_VALIDATION_CACHE_EXTENSION_NAME =
     "VK_EXT_validation_cache"

pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT =
        VkStructureType 1000160000

pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
        = VkStructureType 1000160001

-- | VkValidationCacheEXT
pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT :: VkObjectType

pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT =
        VkObjectType 1000160000

pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT =
        VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
