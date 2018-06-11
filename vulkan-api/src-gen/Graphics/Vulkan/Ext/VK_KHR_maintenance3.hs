{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
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
module Graphics.Vulkan.Ext.VK_KHR_maintenance3
       (-- * Vulkan extension: @VK_KHR_maintenance3@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @169@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Types.Struct.Descriptor,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        VkGetDescriptorSetLayoutSupportKHR,
        pattern VkGetDescriptorSetLayoutSupportKHR,
        HS_vkGetDescriptorSetLayoutSupportKHR,
        PFN_vkGetDescriptorSetLayoutSupportKHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.Descriptor,
        module Graphics.Vulkan.Types.Enum.Shader,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_MAINTENANCE3_SPEC_VERSION,
        pattern VK_KHR_MAINTENANCE3_SPEC_VERSION,
        VK_KHR_MAINTENANCE3_EXTENSION_NAME,
        pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR)
       where
import           GHC.Ptr                                     (Ptr (..))
import           Graphics.Vulkan.Core_1_1                    (pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT,
                                                              pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Descriptor
import           Graphics.Vulkan.Types.Enum.Shader
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Descriptor
import           Graphics.Vulkan.Types.Struct.PhysicalDevice

pattern VkGetDescriptorSetLayoutSupportKHR :: CString

pattern VkGetDescriptorSetLayoutSupportKHR <-
        (is_VkGetDescriptorSetLayoutSupportKHR -> True)
  where VkGetDescriptorSetLayoutSupportKHR
          = _VkGetDescriptorSetLayoutSupportKHR

{-# INLINE _VkGetDescriptorSetLayoutSupportKHR #-}

_VkGetDescriptorSetLayoutSupportKHR :: CString
_VkGetDescriptorSetLayoutSupportKHR
  = Ptr "vkGetDescriptorSetLayoutSupportKHR\NUL"#

{-# INLINE is_VkGetDescriptorSetLayoutSupportKHR #-}

is_VkGetDescriptorSetLayoutSupportKHR :: CString -> Bool
is_VkGetDescriptorSetLayoutSupportKHR
  = (EQ ==) . cmpCStrings _VkGetDescriptorSetLayoutSupportKHR

type VkGetDescriptorSetLayoutSupportKHR =
     "vkGetDescriptorSetLayoutSupportKHR"

-- | This is an alias for `vkGetDescriptorSetLayoutSupport`.
--
--   > void vkGetDescriptorSetLayoutSupportKHR
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , VkDescriptorSetLayoutSupport* pSupport
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDescriptorSetLayoutSupportKHR vkGetDescriptorSetLayoutSupportKHR registry at www.khronos.org>
type HS_vkGetDescriptorSetLayoutSupportKHR =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                           ->
         Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                          -> IO ()

type PFN_vkGetDescriptorSetLayoutSupportKHR =
     FunPtr HS_vkGetDescriptorSetLayoutSupportKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetDescriptorSetLayoutSupportKHRUnsafe ::
               PFN_vkGetDescriptorSetLayoutSupportKHR ->
                 HS_vkGetDescriptorSetLayoutSupportKHR

foreign import ccall safe "dynamic"
               unwrapVkGetDescriptorSetLayoutSupportKHRSafe ::
               PFN_vkGetDescriptorSetLayoutSupportKHR ->
                 HS_vkGetDescriptorSetLayoutSupportKHR

instance VulkanProc "vkGetDescriptorSetLayoutSupportKHR" where
        type VkProcType "vkGetDescriptorSetLayoutSupportKHR" =
             HS_vkGetDescriptorSetLayoutSupportKHR
        vkProcSymbol = _VkGetDescriptorSetLayoutSupportKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetDescriptorSetLayoutSupportKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetDescriptorSetLayoutSupportKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_MAINTENANCE3_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_MAINTENANCE3_SPEC_VERSION = 1

type VK_KHR_MAINTENANCE3_SPEC_VERSION = 1

pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME :: CString

pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME <-
        (is_VK_KHR_MAINTENANCE3_EXTENSION_NAME -> True)
  where VK_KHR_MAINTENANCE3_EXTENSION_NAME
          = _VK_KHR_MAINTENANCE3_EXTENSION_NAME

{-# INLINE _VK_KHR_MAINTENANCE3_EXTENSION_NAME #-}

_VK_KHR_MAINTENANCE3_EXTENSION_NAME :: CString
_VK_KHR_MAINTENANCE3_EXTENSION_NAME
  = Ptr "VK_KHR_maintenance3\NUL"#

{-# INLINE is_VK_KHR_MAINTENANCE3_EXTENSION_NAME #-}

is_VK_KHR_MAINTENANCE3_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_MAINTENANCE3_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_MAINTENANCE3_EXTENSION_NAME

type VK_KHR_MAINTENANCE3_EXTENSION_NAME = "VK_KHR_maintenance3"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR =
        VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
