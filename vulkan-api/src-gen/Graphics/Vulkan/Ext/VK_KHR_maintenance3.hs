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
module Graphics.Vulkan.Ext.VK_KHR_maintenance3
       (-- * Vulkan extension: @VK_KHR_maintenance3@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
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
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutSupportKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMaintenance3PropertiesKHR,
        VkGetDescriptorSetLayoutSupportKHR,
        pattern VkGetDescriptorSetLayoutSupportKHR,
        HS_vkGetDescriptorSetLayoutSupportKHR,
        PFN_vkGetDescriptorSetLayoutSupportKHR,
        unwrapVkGetDescriptorSetLayoutSupportKHR,
        vkGetDescriptorSetLayoutSupportKHR,
        vkGetDescriptorSetLayoutSupportKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkDescriptorSetLayoutCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkDescriptorType,
        module Graphics.Vulkan.Types.Enum.VkShaderStageFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBinding,
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutSupport,
        VK_KHR_MAINTENANCE3_SPEC_VERSION,
        pattern VK_KHR_MAINTENANCE3_SPEC_VERSION,
        VK_KHR_MAINTENANCE3_EXTENSION_NAME,
        pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR)
       where
import           GHC.Ptr
                                                                                         (Ptr (..))
import           Graphics.Vulkan.Core_1_1
                                                                                         (pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT,
                                                                                         pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.InstanceProc
                                                                                         (VulkanInstanceProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkDescriptorSetLayoutCreateFlags
import           Graphics.Vulkan.Types.Enum.VkDescriptorType
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBinding
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutSupport
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutSupportKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMaintenance3PropertiesKHR

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
--   > () vkGetDescriptorSetLayoutSupportKHR
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , VkDescriptorSetLayoutSupport* pSupport
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDescriptorSetLayoutSupportKHR.html vkGetDescriptorSetLayoutSupportKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDescriptorSetLayoutSupportKHR"
               vkGetDescriptorSetLayoutSupportKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                     ->
                   Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                    -> IO ()

-- | This is an alias for `vkGetDescriptorSetLayoutSupport`.
--
--   > () vkGetDescriptorSetLayoutSupportKHR
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , VkDescriptorSetLayoutSupport* pSupport
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDescriptorSetLayoutSupportKHR.html vkGetDescriptorSetLayoutSupportKHR registry at www.khronos.org>
foreign import ccall safe "vkGetDescriptorSetLayoutSupportKHR"
               vkGetDescriptorSetLayoutSupportKHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                     ->
                   Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                    -> IO ()

-- | This is an alias for `vkGetDescriptorSetLayoutSupport`.
--
--   > () vkGetDescriptorSetLayoutSupportKHR
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , VkDescriptorSetLayoutSupport* pSupport
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDescriptorSetLayoutSupportKHR.html vkGetDescriptorSetLayoutSupportKHR registry at www.khronos.org>
type HS_vkGetDescriptorSetLayoutSupportKHR =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                           ->
         Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                          -> IO ()

type PFN_vkGetDescriptorSetLayoutSupportKHR =
     FunPtr HS_vkGetDescriptorSetLayoutSupportKHR

foreign import ccall "dynamic"
               unwrapVkGetDescriptorSetLayoutSupportKHR ::
               PFN_vkGetDescriptorSetLayoutSupportKHR ->
                 HS_vkGetDescriptorSetLayoutSupportKHR

instance VulkanInstanceProc "vkGetDescriptorSetLayoutSupportKHR"
         where
        type VkInstanceProcType "vkGetDescriptorSetLayoutSupportKHR" =
             HS_vkGetDescriptorSetLayoutSupportKHR
        vkInstanceProcSymbol = _VkGetDescriptorSetLayoutSupportKHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkGetDescriptorSetLayoutSupportKHR

        {-# INLINE unwrapVkInstanceProc #-}

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
