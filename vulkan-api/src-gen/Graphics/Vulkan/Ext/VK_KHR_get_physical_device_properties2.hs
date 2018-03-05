{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2
       (-- * Vulkan extension: @VK_KHR_get_physical_device_properties2@
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
        -- Extension number: @60@
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags,
        module Graphics.Vulkan.Types.Struct.VkFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkFormatProperties2KHR,
        module Graphics.Vulkan.Types.Enum.VkImageAspectFlags,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkMemoryHeap,
        module Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags,
        module Graphics.Vulkan.Types.Enum.VkMemoryPropertyFlags,
        module Graphics.Vulkan.Types.Struct.VkMemoryType,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties,
        module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties2KHR,
        module Graphics.Vulkan.Types.Enum.VkQueueFlags,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags,
        module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2KHR,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkGetPhysicalDeviceFeatures2KHR,
        vkGetPhysicalDeviceFeatures2KHRSafe,
        vkGetPhysicalDeviceProperties2KHR,
        vkGetPhysicalDeviceProperties2KHRSafe,
        vkGetPhysicalDeviceFormatProperties2KHR,
        vkGetPhysicalDeviceFormatProperties2KHRSafe,
        vkGetPhysicalDeviceImageFormatProperties2KHR,
        vkGetPhysicalDeviceImageFormatProperties2KHRSafe,
        vkGetPhysicalDeviceQueueFamilyProperties2KHR,
        vkGetPhysicalDeviceQueueFamilyProperties2KHRSafe,
        vkGetPhysicalDeviceMemoryProperties2KHR,
        vkGetPhysicalDeviceMemoryProperties2KHRSafe,
        vkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        vkGetPhysicalDeviceSparseImageFormatProperties2KHRSafe,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION,
        pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION,
        VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME,
        pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR)
       where
import           GHC.Ptr
                                                                                         (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags
import           Graphics.Vulkan.Types.Enum.VkMemoryPropertyFlags
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkQueueFlags
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkFormatProperties
import           Graphics.Vulkan.Types.Struct.VkFormatProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkMemoryHeap
import           Graphics.Vulkan.Types.Struct.VkMemoryType
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
import           Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties
import           Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2KHR

-- | > () vkGetPhysicalDeviceFeatures2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2KHR* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceFeatures2KHR.html vkGetPhysicalDeviceFeatures2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceFeatures2KHR"
               vkGetPhysicalDeviceFeatures2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2KHR -- ^ pFeatures
                                                                    -> IO ()

-- | > () vkGetPhysicalDeviceFeatures2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2KHR* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceFeatures2KHR.html vkGetPhysicalDeviceFeatures2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceFeatures2KHR"
               vkGetPhysicalDeviceFeatures2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2KHR -- ^ pFeatures
                                                                    -> IO ()

-- | > () vkGetPhysicalDeviceProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceProperties2KHR.html vkGetPhysicalDeviceProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceProperties2KHR"
               vkGetPhysicalDeviceProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2KHR -- ^ pProperties
                                                                      -> IO ()

-- | > () vkGetPhysicalDeviceProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceProperties2KHR.html vkGetPhysicalDeviceProperties2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceProperties2KHR"
               vkGetPhysicalDeviceProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2KHR -- ^ pProperties
                                                                      -> IO ()

-- | > () vkGetPhysicalDeviceFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2KHR* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceFormatProperties2KHR.html vkGetPhysicalDeviceFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceFormatProperties2KHR"
               vkGetPhysicalDeviceFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2KHR -- ^ pFormatProperties
                                                                          -> IO ()

-- | > () vkGetPhysicalDeviceFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2KHR* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceFormatProperties2KHR.html vkGetPhysicalDeviceFormatProperties2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceFormatProperties2KHR"
               vkGetPhysicalDeviceFormatProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2KHR -- ^ pFormatProperties
                                                                          -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceImageFormatInfo2KHR* pImageFormatInfo
--   >     , VkImageFormatProperties2KHR* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceImageFormatProperties2KHR.html vkGetPhysicalDeviceImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceImageFormatProperties2KHR"
               vkGetPhysicalDeviceImageFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceImageFormatInfo2KHR -- ^ pImageFormatInfo
                                                         ->
                   Ptr VkImageFormatProperties2KHR -- ^ pImageFormatProperties
                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceImageFormatInfo2KHR* pImageFormatInfo
--   >     , VkImageFormatProperties2KHR* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceImageFormatProperties2KHR.html vkGetPhysicalDeviceImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceImageFormatProperties2KHR"
               vkGetPhysicalDeviceImageFormatProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceImageFormatInfo2KHR -- ^ pImageFormatInfo
                                                         ->
                   Ptr VkImageFormatProperties2KHR -- ^ pImageFormatProperties
                                                   -> IO VkResult

-- | > () vkGetPhysicalDeviceQueueFamilyProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2KHR* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties2KHR.html vkGetPhysicalDeviceQueueFamilyProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
               vkGetPhysicalDeviceQueueFamilyProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2KHR -- ^ pQueueFamilyProperties
                                                               -> IO ()

-- | > () vkGetPhysicalDeviceQueueFamilyProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2KHR* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties2KHR.html vkGetPhysicalDeviceQueueFamilyProperties2KHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
               vkGetPhysicalDeviceQueueFamilyProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2KHR -- ^ pQueueFamilyProperties
                                                               -> IO ()

-- | > () vkGetPhysicalDeviceMemoryProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2KHR* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceMemoryProperties2KHR.html vkGetPhysicalDeviceMemoryProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceMemoryProperties2KHR"
               vkGetPhysicalDeviceMemoryProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceMemoryProperties2KHR -- ^ pMemoryProperties
                                                          -> IO ()

-- | > () vkGetPhysicalDeviceMemoryProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2KHR* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceMemoryProperties2KHR.html vkGetPhysicalDeviceMemoryProperties2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceMemoryProperties2KHR"
               vkGetPhysicalDeviceMemoryProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceMemoryProperties2KHR -- ^ pMemoryProperties
                                                          -> IO ()

-- | > () vkGetPhysicalDeviceSparseImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2KHR* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties2KHR.html vkGetPhysicalDeviceSparseImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
               vkGetPhysicalDeviceSparseImageFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSparseImageFormatInfo2KHR -- ^ pFormatInfo
                                                               ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkSparseImageFormatProperties2KHR -- ^ pProperties
                                                                       -> IO ()

-- | > () vkGetPhysicalDeviceSparseImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2KHR* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties2KHR.html vkGetPhysicalDeviceSparseImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
               vkGetPhysicalDeviceSparseImageFormatProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSparseImageFormatInfo2KHR -- ^ pFormatInfo
                                                               ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkSparseImageFormatProperties2KHR -- ^ pProperties
                                                                       -> IO ()

pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 1

type VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 1

pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME ::
        CString

pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME <-
        (is_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME -> True)
  where VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
          = _VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME

{-# INLINE _VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
           #-}

_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME :: CString
_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  = Ptr "VK_KHR_get_physical_device_properties2\NUL"#

{-# INLINE is_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
           #-}

is_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME ::
                                                          CString -> Bool
is_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME

type VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME =
     "VK_KHR_get_physical_device_properties2"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR =
        VkStructureType 1000059000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR =
        VkStructureType 1000059001

pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR =
        VkStructureType 1000059002

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR =
        VkStructureType 1000059003

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR =
        VkStructureType 1000059004

pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR =
        VkStructureType 1000059005

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR =
        VkStructureType 1000059006

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR =
        VkStructureType 1000059007

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
        = VkStructureType 1000059008
