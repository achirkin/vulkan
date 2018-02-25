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
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Enum.VkImageAspectFlags,
        module Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkMemoryRequirements,
        module Graphics.Vulkan.Types.Struct.VkMemoryRequirements2KHR,
        module Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags,
        module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements2KHR,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkGetImageMemoryRequirements2KHR,
        vkGetBufferMemoryRequirements2KHR,
        vkGetImageSparseMemoryRequirements2KHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Handles,
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
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags
import           Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkBufferMemoryRequirementsInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements2KHR
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements2KHR

-- | > () vkGetImageMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkImageMemoryRequirementsInfo2KHR* pInfo
--   >     , VkMemoryRequirements2KHR* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetImageMemoryRequirements2KHR.html vkGetImageMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageMemoryRequirements2KHR"
               vkGetImageMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageMemoryRequirementsInfo2KHR -- ^ pInfo
                                                       ->
                   Ptr VkMemoryRequirements2KHR -- ^ pMemoryRequirements
                                                -> IO ()

-- | > () vkGetBufferMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkBufferMemoryRequirementsInfo2KHR* pInfo
--   >     , VkMemoryRequirements2KHR* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetBufferMemoryRequirements2KHR.html vkGetBufferMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetBufferMemoryRequirements2KHR"
               vkGetBufferMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferMemoryRequirementsInfo2KHR -- ^ pInfo
                                                        ->
                   Ptr VkMemoryRequirements2KHR -- ^ pMemoryRequirements
                                                -> IO ()

-- | > () vkGetImageSparseMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkImageSparseMemoryRequirementsInfo2KHR* pInfo
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements2KHR* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetImageSparseMemoryRequirements2KHR.html vkGetImageSparseMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetImageSparseMemoryRequirements2KHR"
               vkGetImageSparseMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageSparseMemoryRequirementsInfo2KHR -- ^ pInfo
                                                             ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements2KHR -- ^ pSparseMemoryRequirements
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

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR =
        VkStructureType 1000146000

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR =
        VkStructureType 1000146001

pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
        = VkStructureType 1000146002

pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR =
        VkStructureType 1000146003

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR =
        VkStructureType 1000146004
