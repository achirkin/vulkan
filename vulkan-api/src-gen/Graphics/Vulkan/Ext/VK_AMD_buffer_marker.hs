{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_AMD_buffer_marker
       (-- * Vulkan extension: @VK_AMD_buffer_marker@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @aqnuep@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @180@
        vkCmdWriteBufferMarkerAMD, vkCmdWriteBufferMarkerAMDSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkPipelineStageFlags,
        module Graphics.Vulkan.Types.Handles,
        VK_AMD_BUFFER_MARKER_SPEC_VERSION,
        pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION,
        VK_AMD_BUFFER_MARKER_EXTENSION_NAME,
        pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME)
       where
import           GHC.Ptr                                         (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags
import           Graphics.Vulkan.Types.Handles

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   pipeline: @transfer@
--
--   > () vkCmdWriteBufferMarkerAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlagBits pipelineStage
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , uint32_t marker
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdWriteBufferMarkerAMD.html vkCmdWriteBufferMarkerAMD registry at www.khronos.org>
foreign import ccall unsafe "vkCmdWriteBufferMarkerAMD"
               vkCmdWriteBufferMarkerAMD ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineStageFlagBits -- ^ pipelineStage
                                         ->
                   VkBuffer -- ^ dstBuffer
                            -> VkDeviceSize -- ^ dstOffset
                                            -> Word32 -- ^ marker
                                                      -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   pipeline: @transfer@
--
--   > () vkCmdWriteBufferMarkerAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlagBits pipelineStage
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , uint32_t marker
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdWriteBufferMarkerAMD.html vkCmdWriteBufferMarkerAMD registry at www.khronos.org>
foreign import ccall safe "vkCmdWriteBufferMarkerAMD"
               vkCmdWriteBufferMarkerAMDSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineStageFlagBits -- ^ pipelineStage
                                         ->
                   VkBuffer -- ^ dstBuffer
                            -> VkDeviceSize -- ^ dstOffset
                                            -> Word32 -- ^ marker
                                                      -> IO ()

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
