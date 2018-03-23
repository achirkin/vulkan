{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_get_memory_requirements2
       (-- * Vulkan extension: @VK_KHR_get_memory_requirements2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jason Ekstrand @jekstrand@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @147@
        module Graphics.Vulkan.Types.Struct.VkBufferMemoryRequirementsInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkMemoryRequirements2KHR,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements2KHR,
        vkGetImageMemoryRequirements2KHR,
        vkGetImageMemoryRequirements2KHRSafe,
        vkGetBufferMemoryRequirements2KHR,
        vkGetBufferMemoryRequirements2KHRSafe,
        vkGetImageSparseMemoryRequirements2KHR,
        vkGetImageSparseMemoryRequirements2KHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkImageAspectFlags,
        module Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkBufferMemoryRequirementsInfo2,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2,
        module Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2,
        module Graphics.Vulkan.Types.Struct.VkMemoryRequirements,
        module Graphics.Vulkan.Types.Struct.VkMemoryRequirements2,
        module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements2,
        VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION,
        pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION,
        VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME,
        pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR,
        pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR)
       where
import           GHC.Ptr
                                                                                       (Ptr (..))
import           Graphics.Vulkan.Core_1_1
                                                                                       (pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2,
                                                                                       pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2,
                                                                                       pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2,
                                                                                       pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,
                                                                                       pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags
import           Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkBufferMemoryRequirementsInfo2
import           Graphics.Vulkan.Types.Struct.VkBufferMemoryRequirementsInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2
import           Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2
import           Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements2
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements2KHR
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements2
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements2KHR

-- | This is an alias for `vkGetImageMemoryRequirements2`.
--
--   > () vkGetImageMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkImageMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageMemoryRequirements2KHR.html vkGetImageMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageMemoryRequirements2"
               vkGetImageMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                    ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

-- | This is an alias for `vkGetImageMemoryRequirements2`.
--
--   > () vkGetImageMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkImageMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageMemoryRequirements2KHR.html vkGetImageMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetImageMemoryRequirements2"
               vkGetImageMemoryRequirements2KHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                    ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

-- | This is an alias for `vkGetBufferMemoryRequirements2`.
--
--   > () vkGetBufferMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkBufferMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetBufferMemoryRequirements2KHR.html vkGetBufferMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetBufferMemoryRequirements2"
               vkGetBufferMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                     ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

-- | This is an alias for `vkGetBufferMemoryRequirements2`.
--
--   > () vkGetBufferMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkBufferMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetBufferMemoryRequirements2KHR.html vkGetBufferMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetBufferMemoryRequirements2"
               vkGetBufferMemoryRequirements2KHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                     ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

-- | This is an alias for `vkGetImageSparseMemoryRequirements2`.
--
--   > () vkGetImageSparseMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkImageSparseMemoryRequirementsInfo2* pInfo
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements2* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSparseMemoryRequirements2KHR.html vkGetImageSparseMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageSparseMemoryRequirements2"
               vkGetImageSparseMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                          ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                      -> IO ()

-- | This is an alias for `vkGetImageSparseMemoryRequirements2`.
--
--   > () vkGetImageSparseMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkImageSparseMemoryRequirementsInfo2* pInfo
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements2* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSparseMemoryRequirements2KHR.html vkGetImageSparseMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall safe "vkGetImageSparseMemoryRequirements2"
               vkGetImageSparseMemoryRequirements2KHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                          ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                      -> IO ()

pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION = 1

type VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION = 1

pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME :: CString

pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME <-
        (is_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME -> True)
  where VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
          = _VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME

{-# INLINE _VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME #-}

_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME :: CString
_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
  = Ptr "VK_KHR_get_memory_requirements2\NUL"#

{-# INLINE is_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME #-}

is_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME ::
                                                   CString -> Bool
is_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME

type VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME =
     "VK_KHR_get_memory_requirements2"

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR =
        VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR =
        VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2

pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
        = VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2

pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR =
        VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR =
        VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
