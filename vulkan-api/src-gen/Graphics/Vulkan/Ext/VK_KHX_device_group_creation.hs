{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHX_device_group_creation
       (-- * Vulkan extension: @VK_KHX_device_group_creation@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @KHX@
        --
        -- type: @instance@
        --
        -- Extension number: @71@
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupDeviceCreateInfoKHX,
        module Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupPropertiesKHX,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkEnumeratePhysicalDeviceGroupsKHX,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION,
        pattern VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION,
        VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME,
        pattern VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX,
        pattern VK_MAX_DEVICE_GROUP_SIZE_KHX, VK_MAX_DEVICE_GROUP_SIZE_KHX,
        pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX)
       where
import           GHC.Ptr                                                         (Ptr (..))
import           Graphics.Vulkan.Constants                                       (VK_MAX_DEVICE_GROUP_SIZE_KHX,
                                                                                  pattern VK_MAX_DEVICE_GROUP_SIZE_KHX)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags                    (VkMemoryHeapBitmask (..),
                                                                                  VkMemoryHeapFlagBits)
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupDeviceCreateInfoKHX
import           Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupPropertiesKHX

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDeviceGroupsKHX
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceGroupCount
--   >     , VkPhysicalDeviceGroupPropertiesKHX* pPhysicalDeviceGroupProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkEnumeratePhysicalDeviceGroupsKHX.html vkEnumeratePhysicalDeviceGroupsKHX registry at www.khronos.org>
foreign import ccall unsafe "vkEnumeratePhysicalDeviceGroupsKHX"
               vkEnumeratePhysicalDeviceGroupsKHX ::
               VkInstance -- ^ instance
                          ->
                 Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                            -> Ptr VkPhysicalDeviceGroupPropertiesKHX -- ^ pPhysicalDeviceGroupProperties
                                                                      -> IO VkResult

pattern VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

type VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

pattern VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString

pattern VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME <-
        (is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME -> True)
  where VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME
          = _VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME

{-# INLINE _VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME #-}

_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString
_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME
  = Ptr "VK_KHX_device_group_creation\NUL"#

{-# INLINE is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME #-}

is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString -> Bool
is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME

type VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME =
     "VK_KHX_device_group_creation"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX =
        VkStructureType 1000070000

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX =
        VkStructureType 1000070001

-- | If set, heap allocations allocate multiple instances by default
--
--   bitpos = @1@
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX ::
        VkMemoryHeapFlagBits

pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX =
        VkMemoryHeapFlagBits 2
