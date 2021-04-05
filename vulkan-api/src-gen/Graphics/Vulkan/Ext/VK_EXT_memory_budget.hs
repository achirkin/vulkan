{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_memory_budget
       (AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkMemoryHeap, VkMemoryAllocateBitmask(..),
        VkMemoryHeapBitmask(..), VkMemoryOverallocationBehaviorAMD(..),
        VkMemoryPropertyBitmask(..), VkMemoryAllocateFlagBits(),
        VkMemoryAllocateFlagBitsKHR(..), VkMemoryAllocateFlags(),
        VkMemoryHeapFlagBits(), VkMemoryHeapFlags(),
        VkMemoryPropertyFlagBits(), VkMemoryPropertyFlags(), VkMemoryType,
        VkPhysicalDeviceMemoryBudgetPropertiesEXT,
        VkPhysicalDeviceMemoryProperties,
        VkPhysicalDeviceMemoryProperties2, VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_EXT_MEMORY_BUDGET_SPEC_VERSION,
        pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION,
        VK_EXT_MEMORY_BUDGET_EXTENSION_NAME,
        pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.Memory
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.Memory         (VkMemoryHeap, VkMemoryType)
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceMemoryBudgetPropertiesEXT,
                                                    VkPhysicalDeviceMemoryProperties,
                                                    VkPhysicalDeviceMemoryProperties2)

pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION = 1

type VK_EXT_MEMORY_BUDGET_SPEC_VERSION = 1

pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME :: CString

pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME <-
        (is_VK_EXT_MEMORY_BUDGET_EXTENSION_NAME -> True)
  where
    VK_EXT_MEMORY_BUDGET_EXTENSION_NAME
      = _VK_EXT_MEMORY_BUDGET_EXTENSION_NAME

{-# INLINE _VK_EXT_MEMORY_BUDGET_EXTENSION_NAME #-}

_VK_EXT_MEMORY_BUDGET_EXTENSION_NAME :: CString
_VK_EXT_MEMORY_BUDGET_EXTENSION_NAME
  = Ptr "VK_EXT_memory_budget\NUL"#

{-# INLINE is_VK_EXT_MEMORY_BUDGET_EXTENSION_NAME #-}

is_VK_EXT_MEMORY_BUDGET_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_MEMORY_BUDGET_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_MEMORY_BUDGET_EXTENSION_NAME

type VK_EXT_MEMORY_BUDGET_EXTENSION_NAME = "VK_EXT_memory_budget"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
        = VkStructureType 1000237000
