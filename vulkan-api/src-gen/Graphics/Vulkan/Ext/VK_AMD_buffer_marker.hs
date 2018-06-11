{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
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
module Graphics.Vulkan.Ext.VK_AMD_buffer_marker
       (-- * Vulkan extension: @VK_AMD_buffer_marker@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @drakos-amd@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @180@
        VkCmdWriteBufferMarkerAMD, pattern VkCmdWriteBufferMarkerAMD,
        HS_vkCmdWriteBufferMarkerAMD, PFN_vkCmdWriteBufferMarkerAMD,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.Pipeline,
        module Graphics.Vulkan.Types.Handles,
        VK_AMD_BUFFER_MARKER_SPEC_VERSION,
        pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION,
        VK_AMD_BUFFER_MARKER_EXTENSION_NAME,
        pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME)
       where
import           GHC.Ptr                             (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc        (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Pipeline
import           Graphics.Vulkan.Types.Handles

pattern VkCmdWriteBufferMarkerAMD :: CString

pattern VkCmdWriteBufferMarkerAMD <-
        (is_VkCmdWriteBufferMarkerAMD -> True)
  where VkCmdWriteBufferMarkerAMD = _VkCmdWriteBufferMarkerAMD

{-# INLINE _VkCmdWriteBufferMarkerAMD #-}

_VkCmdWriteBufferMarkerAMD :: CString
_VkCmdWriteBufferMarkerAMD = Ptr "vkCmdWriteBufferMarkerAMD\NUL"#

{-# INLINE is_VkCmdWriteBufferMarkerAMD #-}

is_VkCmdWriteBufferMarkerAMD :: CString -> Bool
is_VkCmdWriteBufferMarkerAMD
  = (EQ ==) . cmpCStrings _VkCmdWriteBufferMarkerAMD

type VkCmdWriteBufferMarkerAMD = "vkCmdWriteBufferMarkerAMD"

-- | Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   Pipeline: @transfer@
--
--   > void vkCmdWriteBufferMarkerAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlagBits pipelineStage
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , uint32_t marker
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdWriteBufferMarkerAMD vkCmdWriteBufferMarkerAMD registry at www.khronos.org>
type HS_vkCmdWriteBufferMarkerAMD =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkPipelineStageFlagBits -- ^ pipelineStage
                               ->
         VkBuffer -- ^ dstBuffer
                  -> VkDeviceSize -- ^ dstOffset
                                  -> Word32 -- ^ marker
                                            -> IO ()

type PFN_vkCmdWriteBufferMarkerAMD =
     FunPtr HS_vkCmdWriteBufferMarkerAMD

foreign import ccall unsafe "dynamic"
               unwrapVkCmdWriteBufferMarkerAMDUnsafe ::
               PFN_vkCmdWriteBufferMarkerAMD -> HS_vkCmdWriteBufferMarkerAMD

foreign import ccall safe "dynamic"
               unwrapVkCmdWriteBufferMarkerAMDSafe ::
               PFN_vkCmdWriteBufferMarkerAMD -> HS_vkCmdWriteBufferMarkerAMD

instance VulkanProc "vkCmdWriteBufferMarkerAMD" where
        type VkProcType "vkCmdWriteBufferMarkerAMD" =
             HS_vkCmdWriteBufferMarkerAMD
        vkProcSymbol = _VkCmdWriteBufferMarkerAMD

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCmdWriteBufferMarkerAMDUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCmdWriteBufferMarkerAMDSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION = 1

type VK_AMD_BUFFER_MARKER_SPEC_VERSION = 1

pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME :: CString

pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME <-
        (is_VK_AMD_BUFFER_MARKER_EXTENSION_NAME -> True)
  where VK_AMD_BUFFER_MARKER_EXTENSION_NAME
          = _VK_AMD_BUFFER_MARKER_EXTENSION_NAME

{-# INLINE _VK_AMD_BUFFER_MARKER_EXTENSION_NAME #-}

_VK_AMD_BUFFER_MARKER_EXTENSION_NAME :: CString
_VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  = Ptr "VK_AMD_buffer_marker\NUL"#

{-# INLINE is_VK_AMD_BUFFER_MARKER_EXTENSION_NAME #-}

is_VK_AMD_BUFFER_MARKER_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_AMD_BUFFER_MARKER_EXTENSION_NAME

type VK_AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"
