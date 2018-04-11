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
module Graphics.Vulkan.Ext.VK_KHR_device_group_creation
       (-- * Vulkan extension: @VK_KHR_device_group_creation@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @71@
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupDeviceCreateInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupPropertiesKHR,
        VkEnumeratePhysicalDeviceGroupsKHR,
        pattern VkEnumeratePhysicalDeviceGroupsKHR,
        HS_vkEnumeratePhysicalDeviceGroupsKHR,
        PFN_vkEnumeratePhysicalDeviceGroupsKHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupProperties,
        VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION,
        pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION,
        VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME,
        pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR,
        pattern VK_MAX_DEVICE_GROUP_SIZE_KHR,
        pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR)
       where
import           GHC.Ptr                                                         (Ptr (..))
import           Graphics.Vulkan.Constants                                       (pattern VK_MAX_DEVICE_GROUP_SIZE_KHR)
import           Graphics.Vulkan.Core_1_1                                        (pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT,
                                                                                  pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO,
                                                                                  pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                                    (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupDeviceCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupPropertiesKHR

pattern VkEnumeratePhysicalDeviceGroupsKHR :: CString

pattern VkEnumeratePhysicalDeviceGroupsKHR <-
        (is_VkEnumeratePhysicalDeviceGroupsKHR -> True)
  where VkEnumeratePhysicalDeviceGroupsKHR
          = _VkEnumeratePhysicalDeviceGroupsKHR

{-# INLINE _VkEnumeratePhysicalDeviceGroupsKHR #-}

_VkEnumeratePhysicalDeviceGroupsKHR :: CString
_VkEnumeratePhysicalDeviceGroupsKHR
  = Ptr "vkEnumeratePhysicalDeviceGroupsKHR\NUL"#

{-# INLINE is_VkEnumeratePhysicalDeviceGroupsKHR #-}

is_VkEnumeratePhysicalDeviceGroupsKHR :: CString -> Bool
is_VkEnumeratePhysicalDeviceGroupsKHR
  = (EQ ==) . cmpCStrings _VkEnumeratePhysicalDeviceGroupsKHR

type VkEnumeratePhysicalDeviceGroupsKHR =
     "vkEnumeratePhysicalDeviceGroupsKHR"

-- | This is an alias for `vkEnumeratePhysicalDeviceGroups`.
--
--   Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDeviceGroupsKHR
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceGroupCount
--   >     , VkPhysicalDeviceGroupProperties* pPhysicalDeviceGroupProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumeratePhysicalDeviceGroupsKHR vkEnumeratePhysicalDeviceGroupsKHR registry at www.khronos.org>
type HS_vkEnumeratePhysicalDeviceGroupsKHR =
     VkInstance -- ^ instance
                ->
       Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                  -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                         -> IO VkResult

type PFN_vkEnumeratePhysicalDeviceGroupsKHR =
     FunPtr HS_vkEnumeratePhysicalDeviceGroupsKHR

foreign import ccall "dynamic"
               unwrapVkEnumeratePhysicalDeviceGroupsKHR ::
               PFN_vkEnumeratePhysicalDeviceGroupsKHR ->
                 HS_vkEnumeratePhysicalDeviceGroupsKHR

instance VulkanProc "vkEnumeratePhysicalDeviceGroupsKHR" where
        type VkProcType "vkEnumeratePhysicalDeviceGroupsKHR" =
             HS_vkEnumeratePhysicalDeviceGroupsKHR
        vkProcSymbol = _VkEnumeratePhysicalDeviceGroupsKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumeratePhysicalDeviceGroupsKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

type VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString

pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME <-
        (is_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME -> True)
  where VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
          = _VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME

{-# INLINE _VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME #-}

_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString
_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
  = Ptr "VK_KHR_device_group_creation\NUL"#

{-# INLINE is_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME #-}

is_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME

type VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME =
     "VK_KHR_device_group_creation"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR =
        VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO

pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR =
        VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
