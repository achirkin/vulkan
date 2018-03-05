{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_bind_memory2
       (-- * Vulkan extension: @VK_KHR_bind_memory2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobias Hector @tobias@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @158@
        module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkBindBufferMemory2KHR, vkBindBufferMemory2KHRSafe,
        vkBindImageMemory2KHR, vkBindImageMemory2KHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_BIND_MEMORY_2_SPEC_VERSION,
        pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION,
        VK_KHR_BIND_MEMORY_2_EXTENSION_NAME,
        pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR,
        pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR)
       where
import           GHC.Ptr                                                (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags          (VkImageCreateBitmask (..),
                                                                         VkImageCreateFlagBits)
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfoKHR
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory2KHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindBufferMemoryInfoKHR* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkBindBufferMemory2KHR.html vkBindBufferMemory2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkBindBufferMemory2KHR"
               vkBindBufferMemory2KHR ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindBufferMemoryInfoKHR -- ^ pBindInfos
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory2KHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindBufferMemoryInfoKHR* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkBindBufferMemory2KHR.html vkBindBufferMemory2KHR registry at www.khronos.org>
foreign import ccall safe "vkBindBufferMemory2KHR"
               vkBindBufferMemory2KHRSafe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindBufferMemoryInfoKHR -- ^ pBindInfos
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory2KHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindImageMemoryInfoKHR* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkBindImageMemory2KHR.html vkBindImageMemory2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkBindImageMemory2KHR"
               vkBindImageMemory2KHR ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindImageMemoryInfoKHR -- ^ pBindInfos
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory2KHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindImageMemoryInfoKHR* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkBindImageMemory2KHR.html vkBindImageMemory2KHR registry at www.khronos.org>
foreign import ccall safe "vkBindImageMemory2KHR"
               vkBindImageMemory2KHRSafe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindImageMemoryInfoKHR -- ^ pBindInfos
                                                                  -> IO VkResult

pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION = 1

type VK_KHR_BIND_MEMORY_2_SPEC_VERSION = 1

pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME :: CString

pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME <-
        (is_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME -> True)
  where VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
          = _VK_KHR_BIND_MEMORY_2_EXTENSION_NAME

{-# INLINE _VK_KHR_BIND_MEMORY_2_EXTENSION_NAME #-}

_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME :: CString
_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
  = Ptr "VK_KHR_bind_memory2\NUL"#

{-# INLINE is_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME #-}

is_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_BIND_MEMORY_2_EXTENSION_NAME

type VK_KHR_BIND_MEMORY_2_EXTENSION_NAME = "VK_KHR_bind_memory2"

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR =
        VkStructureType 1000157000

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR =
        VkStructureType 1000157001

-- | bitpos = @10@
pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR = VkImageCreateFlagBits 1024
