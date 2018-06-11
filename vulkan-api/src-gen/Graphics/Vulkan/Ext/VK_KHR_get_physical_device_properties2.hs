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
module Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2
       (-- * Vulkan extension: @VK_KHR_get_physical_device_properties2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @60@
        module Graphics.Vulkan.Types.Struct.FormatProperties,
        module Graphics.Vulkan.Types.Struct.Image,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Struct.QueueFamilyProperties,
        module Graphics.Vulkan.Types.Struct.Sparse,
        VkGetPhysicalDeviceFeatures2KHR,
        pattern VkGetPhysicalDeviceFeatures2KHR,
        HS_vkGetPhysicalDeviceFeatures2KHR,
        PFN_vkGetPhysicalDeviceFeatures2KHR,
        VkGetPhysicalDeviceProperties2KHR,
        pattern VkGetPhysicalDeviceProperties2KHR,
        HS_vkGetPhysicalDeviceProperties2KHR,
        PFN_vkGetPhysicalDeviceProperties2KHR,
        VkGetPhysicalDeviceFormatProperties2KHR,
        pattern VkGetPhysicalDeviceFormatProperties2KHR,
        HS_vkGetPhysicalDeviceFormatProperties2KHR,
        PFN_vkGetPhysicalDeviceFormatProperties2KHR,
        VkGetPhysicalDeviceImageFormatProperties2KHR,
        pattern VkGetPhysicalDeviceImageFormatProperties2KHR,
        HS_vkGetPhysicalDeviceImageFormatProperties2KHR,
        PFN_vkGetPhysicalDeviceImageFormatProperties2KHR,
        VkGetPhysicalDeviceQueueFamilyProperties2KHR,
        pattern VkGetPhysicalDeviceQueueFamilyProperties2KHR,
        HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR,
        PFN_vkGetPhysicalDeviceQueueFamilyProperties2KHR,
        VkGetPhysicalDeviceMemoryProperties2KHR,
        pattern VkGetPhysicalDeviceMemoryProperties2KHR,
        HS_vkGetPhysicalDeviceMemoryProperties2KHR,
        PFN_vkGetPhysicalDeviceMemoryProperties2KHR,
        VkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        pattern VkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        HS_vkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        PFN_vkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Enum.Device,
        module Graphics.Vulkan.Types.Enum.Format,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Enum.Memory,
        module Graphics.Vulkan.Types.Enum.PhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.Queue,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Enum.Sparse,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.Device,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Struct.Memory,
        module Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures,
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
import           GHC.Ptr                                             (Ptr (..))
import           Graphics.Vulkan.Core_1_1                            (pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2,
                                                                      pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2,
                                                                      pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
                                                                      pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2,
                                                                      pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2,
                                                                      pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2,
                                                                      pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2,
                                                                      pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2,
                                                                      pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                        (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.Device
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.Memory
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.Queue
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.Sparse
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Device
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.FormatProperties
import           Graphics.Vulkan.Types.Struct.Image
import           Graphics.Vulkan.Types.Struct.Memory
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.QueueFamilyProperties
import           Graphics.Vulkan.Types.Struct.Sparse

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
--   > void vkGetPhysicalDeviceFeatures2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures2KHR vkGetPhysicalDeviceFeatures2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceFeatures2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                       -> IO ()

type PFN_vkGetPhysicalDeviceFeatures2KHR =
     FunPtr HS_vkGetPhysicalDeviceFeatures2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceFeatures2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceFeatures2KHR ->
                 HS_vkGetPhysicalDeviceFeatures2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceFeatures2KHRSafe ::
               PFN_vkGetPhysicalDeviceFeatures2KHR ->
                 HS_vkGetPhysicalDeviceFeatures2KHR

instance VulkanProc "vkGetPhysicalDeviceFeatures2KHR" where
        type VkProcType "vkGetPhysicalDeviceFeatures2KHR" =
             HS_vkGetPhysicalDeviceFeatures2KHR
        vkProcSymbol = _VkGetPhysicalDeviceFeatures2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkGetPhysicalDeviceFeatures2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetPhysicalDeviceFeatures2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
--   > void vkGetPhysicalDeviceProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties2KHR vkGetPhysicalDeviceProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                         -> IO ()

type PFN_vkGetPhysicalDeviceProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceProperties2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceProperties2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceProperties2KHR ->
                 HS_vkGetPhysicalDeviceProperties2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceProperties2KHRSafe ::
               PFN_vkGetPhysicalDeviceProperties2KHR ->
                 HS_vkGetPhysicalDeviceProperties2KHR

instance VulkanProc "vkGetPhysicalDeviceProperties2KHR" where
        type VkProcType "vkGetPhysicalDeviceProperties2KHR" =
             HS_vkGetPhysicalDeviceProperties2KHR
        vkProcSymbol = _VkGetPhysicalDeviceProperties2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceProperties2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetPhysicalDeviceProperties2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
--   > void vkGetPhysicalDeviceFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties2KHR vkGetPhysicalDeviceFormatProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceFormatProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> VkFormat -- ^ format
                                  -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                             -> IO ()

type PFN_vkGetPhysicalDeviceFormatProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceFormatProperties2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceFormatProperties2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceFormatProperties2KHR ->
                 HS_vkGetPhysicalDeviceFormatProperties2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceFormatProperties2KHRSafe ::
               PFN_vkGetPhysicalDeviceFormatProperties2KHR ->
                 HS_vkGetPhysicalDeviceFormatProperties2KHR

instance VulkanProc "vkGetPhysicalDeviceFormatProperties2KHR" where
        type VkProcType "vkGetPhysicalDeviceFormatProperties2KHR" =
             HS_vkGetPhysicalDeviceFormatProperties2KHR
        vkProcSymbol = _VkGetPhysicalDeviceFormatProperties2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceFormatProperties2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceFormatProperties2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceImageFormatProperties2KHR vkGetPhysicalDeviceImageFormatProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceImageFormatProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                            ->
         Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                      -> IO VkResult

type PFN_vkGetPhysicalDeviceImageFormatProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceImageFormatProperties2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceImageFormatProperties2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceImageFormatProperties2KHR ->
                 HS_vkGetPhysicalDeviceImageFormatProperties2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceImageFormatProperties2KHRSafe ::
               PFN_vkGetPhysicalDeviceImageFormatProperties2KHR ->
                 HS_vkGetPhysicalDeviceImageFormatProperties2KHR

instance VulkanProc "vkGetPhysicalDeviceImageFormatProperties2KHR"
         where
        type VkProcType "vkGetPhysicalDeviceImageFormatProperties2KHR" =
             HS_vkGetPhysicalDeviceImageFormatProperties2KHR
        vkProcSymbol = _VkGetPhysicalDeviceImageFormatProperties2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceImageFormatProperties2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceImageFormatProperties2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
--   > void vkGetPhysicalDeviceQueueFamilyProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties2KHR vkGetPhysicalDeviceQueueFamilyProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pQueueFamilyPropertyCount
                  -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                  -> IO ()

type PFN_vkGetPhysicalDeviceQueueFamilyProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyProperties2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceQueueFamilyProperties2KHR ->
                 HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyProperties2KHRSafe ::
               PFN_vkGetPhysicalDeviceQueueFamilyProperties2KHR ->
                 HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR

instance VulkanProc "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
         where
        type VkProcType "vkGetPhysicalDeviceQueueFamilyProperties2KHR" =
             HS_vkGetPhysicalDeviceQueueFamilyProperties2KHR
        vkProcSymbol = _VkGetPhysicalDeviceQueueFamilyProperties2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceQueueFamilyProperties2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceQueueFamilyProperties2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
--   > void vkGetPhysicalDeviceMemoryProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties2KHR vkGetPhysicalDeviceMemoryProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceMemoryProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                               -> IO ()

type PFN_vkGetPhysicalDeviceMemoryProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceMemoryProperties2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceMemoryProperties2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceMemoryProperties2KHR ->
                 HS_vkGetPhysicalDeviceMemoryProperties2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceMemoryProperties2KHRSafe ::
               PFN_vkGetPhysicalDeviceMemoryProperties2KHR ->
                 HS_vkGetPhysicalDeviceMemoryProperties2KHR

instance VulkanProc "vkGetPhysicalDeviceMemoryProperties2KHR" where
        type VkProcType "vkGetPhysicalDeviceMemoryProperties2KHR" =
             HS_vkGetPhysicalDeviceMemoryProperties2KHR
        vkProcSymbol = _VkGetPhysicalDeviceMemoryProperties2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceMemoryProperties2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceMemoryProperties2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
--   > void vkGetPhysicalDeviceSparseImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSparseImageFormatProperties2KHR vkGetPhysicalDeviceSparseImageFormatProperties2KHR registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSparseImageFormatProperties2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceSparseImageFormatProperties2KHR ->
                 HS_vkGetPhysicalDeviceSparseImageFormatProperties2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSparseImageFormatProperties2KHRSafe ::
               PFN_vkGetPhysicalDeviceSparseImageFormatProperties2KHR ->
                 HS_vkGetPhysicalDeviceSparseImageFormatProperties2KHR

instance VulkanProc
           "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
         where
        type VkProcType
               "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
             = HS_vkGetPhysicalDeviceSparseImageFormatProperties2KHR
        vkProcSymbol = _VkGetPhysicalDeviceSparseImageFormatProperties2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceSparseImageFormatProperties2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceSparseImageFormatProperties2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
