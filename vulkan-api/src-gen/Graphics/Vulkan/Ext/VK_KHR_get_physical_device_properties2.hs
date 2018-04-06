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
        module Graphics.Vulkan.Types.Struct.VkFormatProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2KHR,
        VkGetPhysicalDeviceFeatures2KHR,
        pattern VkGetPhysicalDeviceFeatures2KHR,
        HS_vkGetPhysicalDeviceFeatures2KHR,
        PFN_vkGetPhysicalDeviceFeatures2KHR,
        unwrapVkGetPhysicalDeviceFeatures2KHR,
        vkGetPhysicalDeviceFeatures2KHR,
        vkGetPhysicalDeviceFeatures2KHRSafe,
        VkGetPhysicalDeviceProperties2KHR,
        pattern VkGetPhysicalDeviceProperties2KHR,
        HS_vkGetPhysicalDeviceProperties2KHR,
        PFN_vkGetPhysicalDeviceProperties2KHR,
        unwrapVkGetPhysicalDeviceProperties2KHR,
        vkGetPhysicalDeviceProperties2KHR,
        vkGetPhysicalDeviceProperties2KHRSafe,
        VkGetPhysicalDeviceFormatProperties2KHR,
        pattern VkGetPhysicalDeviceFormatProperties2KHR,
        HS_vkGetPhysicalDeviceFormatProperties2KHR,
        PFN_vkGetPhysicalDeviceFormatProperties2KHR,
        unwrapVkGetPhysicalDeviceFormatProperties2KHR,
        vkGetPhysicalDeviceFormatProperties2KHR,
        vkGetPhysicalDeviceFormatProperties2KHRSafe,
        VkGetPhysicalDeviceImageFormatProperties2KHR,
        pattern VkGetPhysicalDeviceImageFormatProperties2KHR,
        HS_vkGetPhysicalDeviceImageFormatProperties2KHR,
        PFN_vkGetPhysicalDeviceImageFormatProperties2KHR,
        unwrapVkGetPhysicalDeviceImageFormatProperties2KHR,
        vkGetPhysicalDeviceImageFormatProperties2KHR,
        vkGetPhysicalDeviceImageFormatProperties2KHRSafe,
        VkGetPhysicalDeviceQueueFamilyProperties2KHR,
        pattern VkGetPhysicalDeviceQueueFamilyProperties2KHR,
        HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR,
        PFN_vkGetPhysicalDeviceQueueFamilyProperties2KHR,
        unwrapVkGetPhysicalDeviceQueueFamilyProperties2KHR,
        vkGetPhysicalDeviceQueueFamilyProperties2KHR,
        vkGetPhysicalDeviceQueueFamilyProperties2KHRSafe,
        VkGetPhysicalDeviceMemoryProperties2KHR,
        pattern VkGetPhysicalDeviceMemoryProperties2KHR,
        HS_vkGetPhysicalDeviceMemoryProperties2KHR,
        PFN_vkGetPhysicalDeviceMemoryProperties2KHR,
        unwrapVkGetPhysicalDeviceMemoryProperties2KHR,
        vkGetPhysicalDeviceMemoryProperties2KHR,
        vkGetPhysicalDeviceMemoryProperties2KHRSafe,
        VkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        pattern VkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        HS_vkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        PFN_vkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        unwrapVkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        vkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        vkGetPhysicalDeviceSparseImageFormatProperties2KHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Enum.VkDeviceQueueCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags,
        module Graphics.Vulkan.Types.Enum.VkImageAspectFlags,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags,
        module Graphics.Vulkan.Types.Enum.VkMemoryPropertyFlags,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.VkQueueFlags,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Struct.VkFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkFormatProperties2,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties2,
        module Graphics.Vulkan.Types.Struct.VkMemoryHeap,
        module Graphics.Vulkan.Types.Struct.VkMemoryType,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties,
        module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties2,
        module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2,
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
import           Graphics.Vulkan.Core_1_1
                                                                                         (pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2,
                                                                                         pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2,
                                                                                         pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
                                                                                         pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2,
                                                                                         pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2,
                                                                                         pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2,
                                                                                         pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2,
                                                                                         pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2,
                                                                                         pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.InstanceProc
                                                                                         (VulkanInstanceProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkDeviceQueueCreateFlags
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
import           Graphics.Vulkan.Types.Struct.VkFormatProperties2
import           Graphics.Vulkan.Types.Struct.VkFormatProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkMemoryHeap
import           Graphics.Vulkan.Types.Struct.VkMemoryType
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
import           Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties
import           Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties2
import           Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2KHR

pattern VkGetPhysicalDeviceFeatures2KHR :: CString

pattern VkGetPhysicalDeviceFeatures2KHR <-
        (is_VkGetPhysicalDeviceFeatures2KHR -> True)
  where VkGetPhysicalDeviceFeatures2KHR
          = _VkGetPhysicalDeviceFeatures2KHR

{-# INLINE _VkGetPhysicalDeviceFeatures2KHR #-}

_VkGetPhysicalDeviceFeatures2KHR :: CString
_VkGetPhysicalDeviceFeatures2KHR
  = Ptr "vkGetPhysicalDeviceFeatures2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceFeatures2KHR #-}

is_VkGetPhysicalDeviceFeatures2KHR :: CString -> Bool
is_VkGetPhysicalDeviceFeatures2KHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceFeatures2KHR

type VkGetPhysicalDeviceFeatures2KHR =
     "vkGetPhysicalDeviceFeatures2KHR"

-- | This is an alias for `vkGetPhysicalDeviceFeatures2`.
--
--   > () vkGetPhysicalDeviceFeatures2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFeatures2KHR.html vkGetPhysicalDeviceFeatures2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceFeatures2KHR"
               vkGetPhysicalDeviceFeatures2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                 -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceFeatures2`.
--
--   > () vkGetPhysicalDeviceFeatures2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFeatures2KHR.html vkGetPhysicalDeviceFeatures2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceFeatures2KHR"
               vkGetPhysicalDeviceFeatures2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                 -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceFeatures2`.
--
--   > () vkGetPhysicalDeviceFeatures2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFeatures2KHR.html vkGetPhysicalDeviceFeatures2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceFeatures2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                       -> IO ()

type PFN_vkGetPhysicalDeviceFeatures2KHR =
     FunPtr HS_vkGetPhysicalDeviceFeatures2KHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceFeatures2KHR ::
               PFN_vkGetPhysicalDeviceFeatures2KHR ->
                 HS_vkGetPhysicalDeviceFeatures2KHR

instance VulkanInstanceProc "vkGetPhysicalDeviceFeatures2KHR" where
        type VkInstanceProcType "vkGetPhysicalDeviceFeatures2KHR" =
             HS_vkGetPhysicalDeviceFeatures2KHR
        vkInstanceProcSymbol = _VkGetPhysicalDeviceFeatures2KHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkGetPhysicalDeviceFeatures2KHR

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkGetPhysicalDeviceProperties2KHR :: CString

pattern VkGetPhysicalDeviceProperties2KHR <-
        (is_VkGetPhysicalDeviceProperties2KHR -> True)
  where VkGetPhysicalDeviceProperties2KHR
          = _VkGetPhysicalDeviceProperties2KHR

{-# INLINE _VkGetPhysicalDeviceProperties2KHR #-}

_VkGetPhysicalDeviceProperties2KHR :: CString
_VkGetPhysicalDeviceProperties2KHR
  = Ptr "vkGetPhysicalDeviceProperties2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceProperties2KHR #-}

is_VkGetPhysicalDeviceProperties2KHR :: CString -> Bool
is_VkGetPhysicalDeviceProperties2KHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceProperties2KHR

type VkGetPhysicalDeviceProperties2KHR =
     "vkGetPhysicalDeviceProperties2KHR"

-- | This is an alias for `vkGetPhysicalDeviceProperties2`.
--
--   > () vkGetPhysicalDeviceProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceProperties2KHR.html vkGetPhysicalDeviceProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceProperties2KHR"
               vkGetPhysicalDeviceProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                   -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceProperties2`.
--
--   > () vkGetPhysicalDeviceProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceProperties2KHR.html vkGetPhysicalDeviceProperties2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceProperties2KHR"
               vkGetPhysicalDeviceProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                   -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceProperties2`.
--
--   > () vkGetPhysicalDeviceProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceProperties2KHR.html vkGetPhysicalDeviceProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                         -> IO ()

type PFN_vkGetPhysicalDeviceProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceProperties2KHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceProperties2KHR ::
               PFN_vkGetPhysicalDeviceProperties2KHR ->
                 HS_vkGetPhysicalDeviceProperties2KHR

instance VulkanInstanceProc "vkGetPhysicalDeviceProperties2KHR"
         where
        type VkInstanceProcType "vkGetPhysicalDeviceProperties2KHR" =
             HS_vkGetPhysicalDeviceProperties2KHR
        vkInstanceProcSymbol = _VkGetPhysicalDeviceProperties2KHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkGetPhysicalDeviceProperties2KHR

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkGetPhysicalDeviceFormatProperties2KHR :: CString

pattern VkGetPhysicalDeviceFormatProperties2KHR <-
        (is_VkGetPhysicalDeviceFormatProperties2KHR -> True)
  where VkGetPhysicalDeviceFormatProperties2KHR
          = _VkGetPhysicalDeviceFormatProperties2KHR

{-# INLINE _VkGetPhysicalDeviceFormatProperties2KHR #-}

_VkGetPhysicalDeviceFormatProperties2KHR :: CString
_VkGetPhysicalDeviceFormatProperties2KHR
  = Ptr "vkGetPhysicalDeviceFormatProperties2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceFormatProperties2KHR #-}

is_VkGetPhysicalDeviceFormatProperties2KHR :: CString -> Bool
is_VkGetPhysicalDeviceFormatProperties2KHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceFormatProperties2KHR

type VkGetPhysicalDeviceFormatProperties2KHR =
     "vkGetPhysicalDeviceFormatProperties2KHR"

-- | This is an alias for `vkGetPhysicalDeviceFormatProperties2`.
--
--   > () vkGetPhysicalDeviceFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFormatProperties2KHR.html vkGetPhysicalDeviceFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceFormatProperties2KHR"
               vkGetPhysicalDeviceFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                       -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceFormatProperties2`.
--
--   > () vkGetPhysicalDeviceFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFormatProperties2KHR.html vkGetPhysicalDeviceFormatProperties2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceFormatProperties2KHR"
               vkGetPhysicalDeviceFormatProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                       -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceFormatProperties2`.
--
--   > () vkGetPhysicalDeviceFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFormatProperties2KHR.html vkGetPhysicalDeviceFormatProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceFormatProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> VkFormat -- ^ format
                                  -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                             -> IO ()

type PFN_vkGetPhysicalDeviceFormatProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceFormatProperties2KHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceFormatProperties2KHR ::
               PFN_vkGetPhysicalDeviceFormatProperties2KHR ->
                 HS_vkGetPhysicalDeviceFormatProperties2KHR

instance VulkanInstanceProc
           "vkGetPhysicalDeviceFormatProperties2KHR"
         where
        type VkInstanceProcType "vkGetPhysicalDeviceFormatProperties2KHR" =
             HS_vkGetPhysicalDeviceFormatProperties2KHR
        vkInstanceProcSymbol = _VkGetPhysicalDeviceFormatProperties2KHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc
          = unwrapVkGetPhysicalDeviceFormatProperties2KHR

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkGetPhysicalDeviceImageFormatProperties2KHR :: CString

pattern VkGetPhysicalDeviceImageFormatProperties2KHR <-
        (is_VkGetPhysicalDeviceImageFormatProperties2KHR -> True)
  where VkGetPhysicalDeviceImageFormatProperties2KHR
          = _VkGetPhysicalDeviceImageFormatProperties2KHR

{-# INLINE _VkGetPhysicalDeviceImageFormatProperties2KHR #-}

_VkGetPhysicalDeviceImageFormatProperties2KHR :: CString
_VkGetPhysicalDeviceImageFormatProperties2KHR
  = Ptr "vkGetPhysicalDeviceImageFormatProperties2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceImageFormatProperties2KHR #-}

is_VkGetPhysicalDeviceImageFormatProperties2KHR :: CString -> Bool
is_VkGetPhysicalDeviceImageFormatProperties2KHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceImageFormatProperties2KHR

type VkGetPhysicalDeviceImageFormatProperties2KHR =
     "vkGetPhysicalDeviceImageFormatProperties2KHR"

-- | This is an alias for `vkGetPhysicalDeviceImageFormatProperties2`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceImageFormatInfo2* pImageFormatInfo
--   >     , VkImageFormatProperties2* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceImageFormatProperties2KHR.html vkGetPhysicalDeviceImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceImageFormatProperties2KHR"
               vkGetPhysicalDeviceImageFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                      ->
                   Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                -> IO VkResult

-- | This is an alias for `vkGetPhysicalDeviceImageFormatProperties2`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceImageFormatInfo2* pImageFormatInfo
--   >     , VkImageFormatProperties2* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceImageFormatProperties2KHR.html vkGetPhysicalDeviceImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceImageFormatProperties2KHR"
               vkGetPhysicalDeviceImageFormatProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                      ->
                   Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                -> IO VkResult

-- | This is an alias for `vkGetPhysicalDeviceImageFormatProperties2`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceImageFormatInfo2* pImageFormatInfo
--   >     , VkImageFormatProperties2* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceImageFormatProperties2KHR.html vkGetPhysicalDeviceImageFormatProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceImageFormatProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                            ->
         Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                      -> IO VkResult

type PFN_vkGetPhysicalDeviceImageFormatProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceImageFormatProperties2KHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceImageFormatProperties2KHR ::
               PFN_vkGetPhysicalDeviceImageFormatProperties2KHR ->
                 HS_vkGetPhysicalDeviceImageFormatProperties2KHR

instance VulkanInstanceProc
           "vkGetPhysicalDeviceImageFormatProperties2KHR"
         where
        type VkInstanceProcType
               "vkGetPhysicalDeviceImageFormatProperties2KHR"
             = HS_vkGetPhysicalDeviceImageFormatProperties2KHR
        vkInstanceProcSymbol
          = _VkGetPhysicalDeviceImageFormatProperties2KHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc
          = unwrapVkGetPhysicalDeviceImageFormatProperties2KHR

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkGetPhysicalDeviceQueueFamilyProperties2KHR :: CString

pattern VkGetPhysicalDeviceQueueFamilyProperties2KHR <-
        (is_VkGetPhysicalDeviceQueueFamilyProperties2KHR -> True)
  where VkGetPhysicalDeviceQueueFamilyProperties2KHR
          = _VkGetPhysicalDeviceQueueFamilyProperties2KHR

{-# INLINE _VkGetPhysicalDeviceQueueFamilyProperties2KHR #-}

_VkGetPhysicalDeviceQueueFamilyProperties2KHR :: CString
_VkGetPhysicalDeviceQueueFamilyProperties2KHR
  = Ptr "vkGetPhysicalDeviceQueueFamilyProperties2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceQueueFamilyProperties2KHR #-}

is_VkGetPhysicalDeviceQueueFamilyProperties2KHR :: CString -> Bool
is_VkGetPhysicalDeviceQueueFamilyProperties2KHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceQueueFamilyProperties2KHR

type VkGetPhysicalDeviceQueueFamilyProperties2KHR =
     "vkGetPhysicalDeviceQueueFamilyProperties2KHR"

-- | This is an alias for `vkGetPhysicalDeviceQueueFamilyProperties2`.
--
--   > () vkGetPhysicalDeviceQueueFamilyProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties2KHR.html vkGetPhysicalDeviceQueueFamilyProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
               vkGetPhysicalDeviceQueueFamilyProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                            -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceQueueFamilyProperties2`.
--
--   > () vkGetPhysicalDeviceQueueFamilyProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties2KHR.html vkGetPhysicalDeviceQueueFamilyProperties2KHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
               vkGetPhysicalDeviceQueueFamilyProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                            -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceQueueFamilyProperties2`.
--
--   > () vkGetPhysicalDeviceQueueFamilyProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties2KHR.html vkGetPhysicalDeviceQueueFamilyProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pQueueFamilyPropertyCount
                  -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                  -> IO ()

type PFN_vkGetPhysicalDeviceQueueFamilyProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyProperties2KHR ::
               PFN_vkGetPhysicalDeviceQueueFamilyProperties2KHR ->
                 HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR

instance VulkanInstanceProc
           "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
         where
        type VkInstanceProcType
               "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
             = HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR
        vkInstanceProcSymbol
          = _VkGetPhysicalDeviceQueueFamilyProperties2KHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc
          = unwrapVkGetPhysicalDeviceQueueFamilyProperties2KHR

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkGetPhysicalDeviceMemoryProperties2KHR :: CString

pattern VkGetPhysicalDeviceMemoryProperties2KHR <-
        (is_VkGetPhysicalDeviceMemoryProperties2KHR -> True)
  where VkGetPhysicalDeviceMemoryProperties2KHR
          = _VkGetPhysicalDeviceMemoryProperties2KHR

{-# INLINE _VkGetPhysicalDeviceMemoryProperties2KHR #-}

_VkGetPhysicalDeviceMemoryProperties2KHR :: CString
_VkGetPhysicalDeviceMemoryProperties2KHR
  = Ptr "vkGetPhysicalDeviceMemoryProperties2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceMemoryProperties2KHR #-}

is_VkGetPhysicalDeviceMemoryProperties2KHR :: CString -> Bool
is_VkGetPhysicalDeviceMemoryProperties2KHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceMemoryProperties2KHR

type VkGetPhysicalDeviceMemoryProperties2KHR =
     "vkGetPhysicalDeviceMemoryProperties2KHR"

-- | This is an alias for `vkGetPhysicalDeviceMemoryProperties2`.
--
--   > () vkGetPhysicalDeviceMemoryProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceMemoryProperties2KHR.html vkGetPhysicalDeviceMemoryProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceMemoryProperties2KHR"
               vkGetPhysicalDeviceMemoryProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                         -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceMemoryProperties2`.
--
--   > () vkGetPhysicalDeviceMemoryProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceMemoryProperties2KHR.html vkGetPhysicalDeviceMemoryProperties2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceMemoryProperties2KHR"
               vkGetPhysicalDeviceMemoryProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                         -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceMemoryProperties2`.
--
--   > () vkGetPhysicalDeviceMemoryProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceMemoryProperties2KHR.html vkGetPhysicalDeviceMemoryProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceMemoryProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                               -> IO ()

type PFN_vkGetPhysicalDeviceMemoryProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceMemoryProperties2KHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceMemoryProperties2KHR ::
               PFN_vkGetPhysicalDeviceMemoryProperties2KHR ->
                 HS_vkGetPhysicalDeviceMemoryProperties2KHR

instance VulkanInstanceProc
           "vkGetPhysicalDeviceMemoryProperties2KHR"
         where
        type VkInstanceProcType "vkGetPhysicalDeviceMemoryProperties2KHR" =
             HS_vkGetPhysicalDeviceMemoryProperties2KHR
        vkInstanceProcSymbol = _VkGetPhysicalDeviceMemoryProperties2KHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc
          = unwrapVkGetPhysicalDeviceMemoryProperties2KHR

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkGetPhysicalDeviceSparseImageFormatProperties2KHR ::
        CString

pattern VkGetPhysicalDeviceSparseImageFormatProperties2KHR <-
        (is_VkGetPhysicalDeviceSparseImageFormatProperties2KHR -> True)
  where VkGetPhysicalDeviceSparseImageFormatProperties2KHR
          = _VkGetPhysicalDeviceSparseImageFormatProperties2KHR

{-# INLINE _VkGetPhysicalDeviceSparseImageFormatProperties2KHR #-}

_VkGetPhysicalDeviceSparseImageFormatProperties2KHR :: CString
_VkGetPhysicalDeviceSparseImageFormatProperties2KHR
  = Ptr "vkGetPhysicalDeviceSparseImageFormatProperties2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSparseImageFormatProperties2KHR
           #-}

is_VkGetPhysicalDeviceSparseImageFormatProperties2KHR ::
                                                      CString -> Bool
is_VkGetPhysicalDeviceSparseImageFormatProperties2KHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceSparseImageFormatProperties2KHR

type VkGetPhysicalDeviceSparseImageFormatProperties2KHR =
     "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"

-- | This is an alias for `vkGetPhysicalDeviceSparseImageFormatProperties2`.
--
--   > () vkGetPhysicalDeviceSparseImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties2KHR.html vkGetPhysicalDeviceSparseImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
               vkGetPhysicalDeviceSparseImageFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSparseImageFormatInfo2 -- ^ pFormatInfo
                                                            ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkSparseImageFormatProperties2 -- ^ pProperties
                                                                    -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceSparseImageFormatProperties2`.
--
--   > () vkGetPhysicalDeviceSparseImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties2KHR.html vkGetPhysicalDeviceSparseImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
               vkGetPhysicalDeviceSparseImageFormatProperties2KHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSparseImageFormatInfo2 -- ^ pFormatInfo
                                                            ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkSparseImageFormatProperties2 -- ^ pProperties
                                                                    -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceSparseImageFormatProperties2`.
--
--   > () vkGetPhysicalDeviceSparseImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties2KHR.html vkGetPhysicalDeviceSparseImageFormatProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSparseImageFormatProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceSparseImageFormatInfo2 -- ^ pFormatInfo
                                                  ->
         Ptr Word32 -- ^ pPropertyCount
                    -> Ptr VkSparseImageFormatProperties2 -- ^ pProperties
                                                          -> IO ()

type PFN_vkGetPhysicalDeviceSparseImageFormatProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceSparseImageFormatProperties2KHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceSparseImageFormatProperties2KHR ::
               PFN_vkGetPhysicalDeviceSparseImageFormatProperties2KHR ->
                 HS_vkGetPhysicalDeviceSparseImageFormatProperties2KHR

instance VulkanInstanceProc
           "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
         where
        type VkInstanceProcType
               "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
             = HS_vkGetPhysicalDeviceSparseImageFormatProperties2KHR
        vkInstanceProcSymbol
          = _VkGetPhysicalDeviceSparseImageFormatProperties2KHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc
          = unwrapVkGetPhysicalDeviceSparseImageFormatProperties2KHR

        {-# INLINE unwrapVkInstanceProc #-}

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

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2

pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR =
        VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR =
        VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2

pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR =
        VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR =
        VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
