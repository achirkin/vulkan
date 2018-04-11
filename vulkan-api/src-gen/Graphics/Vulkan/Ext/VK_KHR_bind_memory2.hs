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
        VkBindBufferMemory2KHR, pattern VkBindBufferMemory2KHR,
        HS_vkBindBufferMemory2KHR, PFN_vkBindBufferMemory2KHR,
        VkBindImageMemory2KHR, pattern VkBindImageMemory2KHR,
        HS_vkBindImageMemory2KHR, PFN_vkBindImageMemory2KHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfo,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo,
        VK_KHR_BIND_MEMORY_2_SPEC_VERSION,
        pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION,
        VK_KHR_BIND_MEMORY_2_EXTENSION_NAME,
        pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR,
        pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR)
       where
import           GHC.Ptr                                                (Ptr (..))
import           Graphics.Vulkan.Core_1_1                               (pattern VK_IMAGE_CREATE_ALIAS_BIT,
                                                                         pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO,
                                                                         pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                           (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfo
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfoKHR
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR

pattern VkBindBufferMemory2KHR :: CString

pattern VkBindBufferMemory2KHR <-
        (is_VkBindBufferMemory2KHR -> True)
  where VkBindBufferMemory2KHR = _VkBindBufferMemory2KHR

{-# INLINE _VkBindBufferMemory2KHR #-}

_VkBindBufferMemory2KHR :: CString
_VkBindBufferMemory2KHR = Ptr "vkBindBufferMemory2KHR\NUL"#

{-# INLINE is_VkBindBufferMemory2KHR #-}

is_VkBindBufferMemory2KHR :: CString -> Bool
is_VkBindBufferMemory2KHR
  = (EQ ==) . cmpCStrings _VkBindBufferMemory2KHR

type VkBindBufferMemory2KHR = "vkBindBufferMemory2KHR"

-- | This is an alias for `vkBindBufferMemory2`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory2KHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindBufferMemoryInfo* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindBufferMemory2KHR vkBindBufferMemory2KHR registry at www.khronos.org>
type HS_vkBindBufferMemory2KHR =
     VkDevice -- ^ device
              -> Word32 -- ^ bindInfoCount
                        -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                      -> IO VkResult

type PFN_vkBindBufferMemory2KHR = FunPtr HS_vkBindBufferMemory2KHR

foreign import ccall "dynamic" unwrapVkBindBufferMemory2KHR ::
               PFN_vkBindBufferMemory2KHR -> HS_vkBindBufferMemory2KHR

instance VulkanProc "vkBindBufferMemory2KHR" where
        type VkProcType "vkBindBufferMemory2KHR" =
             HS_vkBindBufferMemory2KHR
        vkProcSymbol = _VkBindBufferMemory2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkBindBufferMemory2KHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkBindImageMemory2KHR :: CString

pattern VkBindImageMemory2KHR <- (is_VkBindImageMemory2KHR -> True)
  where VkBindImageMemory2KHR = _VkBindImageMemory2KHR

{-# INLINE _VkBindImageMemory2KHR #-}

_VkBindImageMemory2KHR :: CString
_VkBindImageMemory2KHR = Ptr "vkBindImageMemory2KHR\NUL"#

{-# INLINE is_VkBindImageMemory2KHR #-}

is_VkBindImageMemory2KHR :: CString -> Bool
is_VkBindImageMemory2KHR
  = (EQ ==) . cmpCStrings _VkBindImageMemory2KHR

type VkBindImageMemory2KHR = "vkBindImageMemory2KHR"

-- | This is an alias for `vkBindImageMemory2`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory2KHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindImageMemoryInfo* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindImageMemory2KHR vkBindImageMemory2KHR registry at www.khronos.org>
type HS_vkBindImageMemory2KHR =
     VkDevice -- ^ device
              -> Word32 -- ^ bindInfoCount
                        -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                     -> IO VkResult

type PFN_vkBindImageMemory2KHR = FunPtr HS_vkBindImageMemory2KHR

foreign import ccall "dynamic" unwrapVkBindImageMemory2KHR ::
               PFN_vkBindImageMemory2KHR -> HS_vkBindImageMemory2KHR

instance VulkanProc "vkBindImageMemory2KHR" where
        type VkProcType "vkBindImageMemory2KHR" = HS_vkBindImageMemory2KHR
        vkProcSymbol = _VkBindImageMemory2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkBindImageMemory2KHR

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR =
        VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR =
        VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO

pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR = VK_IMAGE_CREATE_ALIAS_BIT
