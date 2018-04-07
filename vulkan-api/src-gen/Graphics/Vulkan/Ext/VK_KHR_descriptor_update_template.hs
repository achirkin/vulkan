{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
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
module Graphics.Vulkan.Ext.VK_KHR_descriptor_update_template
       (-- * Vulkan extension: @VK_KHR_descriptor_update_template@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Markus Tavenrath @mtavenrath@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @86@
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateCreateInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntryKHR,
        module Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateTypeKHR,
        VkCreateDescriptorUpdateTemplateKHR,
        pattern VkCreateDescriptorUpdateTemplateKHR,
        HS_vkCreateDescriptorUpdateTemplateKHR,
        PFN_vkCreateDescriptorUpdateTemplateKHR,
        unwrapVkCreateDescriptorUpdateTemplateKHR,
        vkCreateDescriptorUpdateTemplateKHR,
        vkCreateDescriptorUpdateTemplateKHRSafe,
        VkDestroyDescriptorUpdateTemplateKHR,
        pattern VkDestroyDescriptorUpdateTemplateKHR,
        HS_vkDestroyDescriptorUpdateTemplateKHR,
        PFN_vkDestroyDescriptorUpdateTemplateKHR,
        unwrapVkDestroyDescriptorUpdateTemplateKHR,
        vkDestroyDescriptorUpdateTemplateKHR,
        vkDestroyDescriptorUpdateTemplateKHRSafe,
        VkUpdateDescriptorSetWithTemplateKHR,
        pattern VkUpdateDescriptorSetWithTemplateKHR,
        HS_vkUpdateDescriptorSetWithTemplateKHR,
        PFN_vkUpdateDescriptorSetWithTemplateKHR,
        unwrapVkUpdateDescriptorSetWithTemplateKHR,
        vkUpdateDescriptorSetWithTemplateKHR,
        vkUpdateDescriptorSetWithTemplateKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkDescriptorType,
        module Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateType,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkPipelineBindPoint,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntry,
        VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION,
        pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION,
        VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME,
        pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR,
        pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR,
        pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR,
        -- ** Required extensions: 'VK_KHR_push_descriptor'.
        pattern VkCmdPushDescriptorSetWithTemplateKHR,
        HS_vkCmdPushDescriptorSetWithTemplateKHR,
        PFN_vkCmdPushDescriptorSetWithTemplateKHR,
        unwrapVkCmdPushDescriptorSetWithTemplateKHR,
        vkCmdPushDescriptorSetWithTemplateKHR,
        vkCmdPushDescriptorSetWithTemplateKHRSafe,
        pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR,
        -- ** Required extensions: 'VK_EXT_debug_report'.
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT)
       where
import           GHC.Ptr
                                                                                       (Ptr (..))
import           Graphics.Vulkan.Core_1_1
                                                                                       (pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE,
                                                                                       pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO)
import           Graphics.Vulkan.Ext.VK_EXT_debug_report
                                                                                       (pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT)
import           Graphics.Vulkan.Ext.VK_KHR_push_descriptor
                                                                                       (HS_vkCmdPushDescriptorSetWithTemplateKHR,
                                                                                       PFN_vkCmdPushDescriptorSetWithTemplateKHR,
                                                                                       pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR,
                                                                                       pattern VkCmdPushDescriptorSetWithTemplateKHR,
                                                                                       unwrapVkCmdPushDescriptorSetWithTemplateKHR,
                                                                                       vkCmdPushDescriptorSetWithTemplateKHR,
                                                                                       vkCmdPushDescriptorSetWithTemplateKHRSafe)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
                                                                                       (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkDescriptorType
import           Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateType
import           Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateTypeKHR
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntry
import           Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntryKHR

pattern VkCreateDescriptorUpdateTemplateKHR :: CString

pattern VkCreateDescriptorUpdateTemplateKHR <-
        (is_VkCreateDescriptorUpdateTemplateKHR -> True)
  where VkCreateDescriptorUpdateTemplateKHR
          = _VkCreateDescriptorUpdateTemplateKHR

{-# INLINE _VkCreateDescriptorUpdateTemplateKHR #-}

_VkCreateDescriptorUpdateTemplateKHR :: CString
_VkCreateDescriptorUpdateTemplateKHR
  = Ptr "vkCreateDescriptorUpdateTemplateKHR\NUL"#

{-# INLINE is_VkCreateDescriptorUpdateTemplateKHR #-}

is_VkCreateDescriptorUpdateTemplateKHR :: CString -> Bool
is_VkCreateDescriptorUpdateTemplateKHR
  = (EQ ==) . cmpCStrings _VkCreateDescriptorUpdateTemplateKHR

type VkCreateDescriptorUpdateTemplateKHR =
     "vkCreateDescriptorUpdateTemplateKHR"

-- | This is an alias for `vkCreateDescriptorUpdateTemplate`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , const VkDescriptorUpdateTemplateCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorUpdateTemplate* pDescriptorUpdateTemplate
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorUpdateTemplateKHR.html vkCreateDescriptorUpdateTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDescriptorUpdateTemplateKHR"
               vkCreateDescriptorUpdateTemplateKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                                    -> IO VkResult

-- | This is an alias for `vkCreateDescriptorUpdateTemplate`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , const VkDescriptorUpdateTemplateCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorUpdateTemplate* pDescriptorUpdateTemplate
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorUpdateTemplateKHR.html vkCreateDescriptorUpdateTemplateKHR registry at www.khronos.org>
foreign import ccall safe "vkCreateDescriptorUpdateTemplateKHR"
               vkCreateDescriptorUpdateTemplateKHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                                    -> IO VkResult

-- | This is an alias for `vkCreateDescriptorUpdateTemplate`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , const VkDescriptorUpdateTemplateCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorUpdateTemplate* pDescriptorUpdateTemplate
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorUpdateTemplateKHR.html vkCreateDescriptorUpdateTemplateKHR registry at www.khronos.org>
type HS_vkCreateDescriptorUpdateTemplateKHR =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                          -> IO VkResult

type PFN_vkCreateDescriptorUpdateTemplateKHR =
     FunPtr HS_vkCreateDescriptorUpdateTemplateKHR

foreign import ccall "dynamic"
               unwrapVkCreateDescriptorUpdateTemplateKHR ::
               PFN_vkCreateDescriptorUpdateTemplateKHR ->
                 HS_vkCreateDescriptorUpdateTemplateKHR

instance VulkanProc "vkCreateDescriptorUpdateTemplateKHR" where
        type VkProcType "vkCreateDescriptorUpdateTemplateKHR" =
             HS_vkCreateDescriptorUpdateTemplateKHR
        vkProcSymbol = _VkCreateDescriptorUpdateTemplateKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateDescriptorUpdateTemplateKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyDescriptorUpdateTemplateKHR :: CString

pattern VkDestroyDescriptorUpdateTemplateKHR <-
        (is_VkDestroyDescriptorUpdateTemplateKHR -> True)
  where VkDestroyDescriptorUpdateTemplateKHR
          = _VkDestroyDescriptorUpdateTemplateKHR

{-# INLINE _VkDestroyDescriptorUpdateTemplateKHR #-}

_VkDestroyDescriptorUpdateTemplateKHR :: CString
_VkDestroyDescriptorUpdateTemplateKHR
  = Ptr "vkDestroyDescriptorUpdateTemplateKHR\NUL"#

{-# INLINE is_VkDestroyDescriptorUpdateTemplateKHR #-}

is_VkDestroyDescriptorUpdateTemplateKHR :: CString -> Bool
is_VkDestroyDescriptorUpdateTemplateKHR
  = (EQ ==) . cmpCStrings _VkDestroyDescriptorUpdateTemplateKHR

type VkDestroyDescriptorUpdateTemplateKHR =
     "vkDestroyDescriptorUpdateTemplateKHR"

-- | This is an alias for `vkDestroyDescriptorUpdateTemplate`.
--
--   > () vkDestroyDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorUpdateTemplateKHR.html vkDestroyDescriptorUpdateTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDescriptorUpdateTemplateKHR"
               vkDestroyDescriptorUpdateTemplateKHR ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | This is an alias for `vkDestroyDescriptorUpdateTemplate`.
--
--   > () vkDestroyDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorUpdateTemplateKHR.html vkDestroyDescriptorUpdateTemplateKHR registry at www.khronos.org>
foreign import ccall safe "vkDestroyDescriptorUpdateTemplateKHR"
               vkDestroyDescriptorUpdateTemplateKHRSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | This is an alias for `vkDestroyDescriptorUpdateTemplate`.
--
--   > () vkDestroyDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorUpdateTemplateKHR.html vkDestroyDescriptorUpdateTemplateKHR registry at www.khronos.org>
type HS_vkDestroyDescriptorUpdateTemplateKHR =
     VkDevice -- ^ device
              ->
       VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyDescriptorUpdateTemplateKHR =
     FunPtr HS_vkDestroyDescriptorUpdateTemplateKHR

foreign import ccall "dynamic"
               unwrapVkDestroyDescriptorUpdateTemplateKHR ::
               PFN_vkDestroyDescriptorUpdateTemplateKHR ->
                 HS_vkDestroyDescriptorUpdateTemplateKHR

instance VulkanProc "vkDestroyDescriptorUpdateTemplateKHR" where
        type VkProcType "vkDestroyDescriptorUpdateTemplateKHR" =
             HS_vkDestroyDescriptorUpdateTemplateKHR
        vkProcSymbol = _VkDestroyDescriptorUpdateTemplateKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyDescriptorUpdateTemplateKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkUpdateDescriptorSetWithTemplateKHR :: CString

pattern VkUpdateDescriptorSetWithTemplateKHR <-
        (is_VkUpdateDescriptorSetWithTemplateKHR -> True)
  where VkUpdateDescriptorSetWithTemplateKHR
          = _VkUpdateDescriptorSetWithTemplateKHR

{-# INLINE _VkUpdateDescriptorSetWithTemplateKHR #-}

_VkUpdateDescriptorSetWithTemplateKHR :: CString
_VkUpdateDescriptorSetWithTemplateKHR
  = Ptr "vkUpdateDescriptorSetWithTemplateKHR\NUL"#

{-# INLINE is_VkUpdateDescriptorSetWithTemplateKHR #-}

is_VkUpdateDescriptorSetWithTemplateKHR :: CString -> Bool
is_VkUpdateDescriptorSetWithTemplateKHR
  = (EQ ==) . cmpCStrings _VkUpdateDescriptorSetWithTemplateKHR

type VkUpdateDescriptorSetWithTemplateKHR =
     "vkUpdateDescriptorSetWithTemplateKHR"

-- | This is an alias for `vkUpdateDescriptorSetWithTemplate`.
--
--   > () vkUpdateDescriptorSetWithTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorSet descriptorSet
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUpdateDescriptorSetWithTemplateKHR.html vkUpdateDescriptorSetWithTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkUpdateDescriptorSetWithTemplateKHR"
               vkUpdateDescriptorSetWithTemplateKHR ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSet -- ^ descriptorSet
                                 -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                               -> Ptr Void -- ^ pData
                                                                           -> IO ()

-- | This is an alias for `vkUpdateDescriptorSetWithTemplate`.
--
--   > () vkUpdateDescriptorSetWithTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorSet descriptorSet
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUpdateDescriptorSetWithTemplateKHR.html vkUpdateDescriptorSetWithTemplateKHR registry at www.khronos.org>
foreign import ccall safe "vkUpdateDescriptorSetWithTemplateKHR"
               vkUpdateDescriptorSetWithTemplateKHRSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSet -- ^ descriptorSet
                                 -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                               -> Ptr Void -- ^ pData
                                                                           -> IO ()

-- | This is an alias for `vkUpdateDescriptorSetWithTemplate`.
--
--   > () vkUpdateDescriptorSetWithTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorSet descriptorSet
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUpdateDescriptorSetWithTemplateKHR.html vkUpdateDescriptorSetWithTemplateKHR registry at www.khronos.org>
type HS_vkUpdateDescriptorSetWithTemplateKHR =
     VkDevice -- ^ device
              ->
       VkDescriptorSet -- ^ descriptorSet
                       -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                     -> Ptr Void -- ^ pData
                                                                 -> IO ()

type PFN_vkUpdateDescriptorSetWithTemplateKHR =
     FunPtr HS_vkUpdateDescriptorSetWithTemplateKHR

foreign import ccall "dynamic"
               unwrapVkUpdateDescriptorSetWithTemplateKHR ::
               PFN_vkUpdateDescriptorSetWithTemplateKHR ->
                 HS_vkUpdateDescriptorSetWithTemplateKHR

instance VulkanProc "vkUpdateDescriptorSetWithTemplateKHR" where
        type VkProcType "vkUpdateDescriptorSetWithTemplateKHR" =
             HS_vkUpdateDescriptorSetWithTemplateKHR
        vkProcSymbol = _VkUpdateDescriptorSetWithTemplateKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkUpdateDescriptorSetWithTemplateKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1

type VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: CString

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME <-
        (is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME -> True)
  where VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
          = _VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME

{-# INLINE _VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME #-}

_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: CString
_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  = Ptr "VK_KHR_descriptor_update_template\NUL"#

{-# INLINE is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME #-}

is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME ::
                                                    CString -> Bool
is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME

type VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME =
     "VK_KHR_descriptor_update_template"

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
        = VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO

pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR =
        VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE

pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR =
        VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
        = VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
