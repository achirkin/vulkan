#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_AMD_draw_indirect_count
       (-- * Vulkan extension: @VK_AMD_draw_indirect_count@
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
        -- Extension number: @34@
        vkCmdDrawIndirectCountAMD, vkCmdDrawIndexedIndirectCountAMD,
        VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION,
        pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION,
        VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME,
        pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME)
       where
import           Foreign.C.String        (CString)
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Common  (VkBuffer, VkCommandBuffer,
                                          VkDeviceSize (..), Word32)
import           Graphics.Vulkan.Marshal

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdDrawIndirectCountAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDrawIndirectCountAMD.html vkCmdDrawIndirectCountAMD registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDrawIndirectCountAMD"
               vkCmdDrawIndirectCountAMD ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkBuffer -- ^ countBuffer
                              -> VkDeviceSize -- ^ countBufferOffset
                                              -> Word32 -- ^ maxDrawCount
                                                        -> Word32 -- ^ stride
                                                                  -> IO ()

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdDrawIndexedIndirectCountAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDrawIndexedIndirectCountAMD.html vkCmdDrawIndexedIndirectCountAMD registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDrawIndexedIndirectCountAMD"
               vkCmdDrawIndexedIndirectCountAMD ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkBuffer -- ^ countBuffer
                              -> VkDeviceSize -- ^ countBufferOffset
                                              -> Word32 -- ^ maxDrawCount
                                                        -> Word32 -- ^ stride
                                                                  -> IO ()

pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1

type VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1

pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: CString

pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME <-
        (is_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME -> True)
  where VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
          = _VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME

_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: CString

{-# INLINE _VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME #-}
_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  = Ptr "VK_AMD_draw_indirect_count\NUL"##

is_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME #-}
is_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  = (_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME ==)

type VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME =
     "VK_AMD_draw_indirect_count"
