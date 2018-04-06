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
        VkCmdDrawIndirectCountAMD, pattern VkCmdDrawIndirectCountAMD,
        HS_vkCmdDrawIndirectCountAMD, PFN_vkCmdDrawIndirectCountAMD,
        unwrapVkCmdDrawIndirectCountAMD, vkCmdDrawIndirectCountAMD,
        vkCmdDrawIndirectCountAMDSafe, VkCmdDrawIndexedIndirectCountAMD,
        pattern VkCmdDrawIndexedIndirectCountAMD,
        HS_vkCmdDrawIndexedIndirectCountAMD,
        PFN_vkCmdDrawIndexedIndirectCountAMD,
        unwrapVkCmdDrawIndexedIndirectCountAMD,
        vkCmdDrawIndexedIndirectCountAMD,
        vkCmdDrawIndexedIndirectCountAMDSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Handles,
        VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION,
        pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION,
        VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME,
        pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME)
       where
import           GHC.Ptr                              (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.InstanceProc (VulkanInstanceProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Handles

pattern VkCmdDrawIndirectCountAMD :: CString

pattern VkCmdDrawIndirectCountAMD <-
        (is_VkCmdDrawIndirectCountAMD -> True)
  where VkCmdDrawIndirectCountAMD = _VkCmdDrawIndirectCountAMD

{-# INLINE _VkCmdDrawIndirectCountAMD #-}

_VkCmdDrawIndirectCountAMD :: CString
_VkCmdDrawIndirectCountAMD = Ptr "vkCmdDrawIndirectCountAMD\NUL"#

{-# INLINE is_VkCmdDrawIndirectCountAMD #-}

is_VkCmdDrawIndirectCountAMD :: CString -> Bool
is_VkCmdDrawIndirectCountAMD
  = (EQ ==) . cmpCStrings _VkCmdDrawIndirectCountAMD

type VkCmdDrawIndirectCountAMD = "vkCmdDrawIndirectCountAMD"

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndirectCountAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndirectCountAMD.html vkCmdDrawIndirectCountAMD registry at www.khronos.org>
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

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndirectCountAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndirectCountAMD.html vkCmdDrawIndirectCountAMD registry at www.khronos.org>
foreign import ccall safe "vkCmdDrawIndirectCountAMD"
               vkCmdDrawIndirectCountAMDSafe ::
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

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndirectCountAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndirectCountAMD.html vkCmdDrawIndirectCountAMD registry at www.khronos.org>
type HS_vkCmdDrawIndirectCountAMD =
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

type PFN_vkCmdDrawIndirectCountAMD =
     FunPtr HS_vkCmdDrawIndirectCountAMD

foreign import ccall "dynamic" unwrapVkCmdDrawIndirectCountAMD ::
               PFN_vkCmdDrawIndirectCountAMD -> HS_vkCmdDrawIndirectCountAMD

instance VulkanInstanceProc "vkCmdDrawIndirectCountAMD" where
        type VkInstanceProcType "vkCmdDrawIndirectCountAMD" =
             HS_vkCmdDrawIndirectCountAMD
        vkInstanceProcSymbol = _VkCmdDrawIndirectCountAMD

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkCmdDrawIndirectCountAMD

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkCmdDrawIndexedIndirectCountAMD :: CString

pattern VkCmdDrawIndexedIndirectCountAMD <-
        (is_VkCmdDrawIndexedIndirectCountAMD -> True)
  where VkCmdDrawIndexedIndirectCountAMD
          = _VkCmdDrawIndexedIndirectCountAMD

{-# INLINE _VkCmdDrawIndexedIndirectCountAMD #-}

_VkCmdDrawIndexedIndirectCountAMD :: CString
_VkCmdDrawIndexedIndirectCountAMD
  = Ptr "vkCmdDrawIndexedIndirectCountAMD\NUL"#

{-# INLINE is_VkCmdDrawIndexedIndirectCountAMD #-}

is_VkCmdDrawIndexedIndirectCountAMD :: CString -> Bool
is_VkCmdDrawIndexedIndirectCountAMD
  = (EQ ==) . cmpCStrings _VkCmdDrawIndexedIndirectCountAMD

type VkCmdDrawIndexedIndirectCountAMD =
     "vkCmdDrawIndexedIndirectCountAMD"

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndexedIndirectCountAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndexedIndirectCountAMD.html vkCmdDrawIndexedIndirectCountAMD registry at www.khronos.org>
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

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndexedIndirectCountAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndexedIndirectCountAMD.html vkCmdDrawIndexedIndirectCountAMD registry at www.khronos.org>
foreign import ccall safe "vkCmdDrawIndexedIndirectCountAMD"
               vkCmdDrawIndexedIndirectCountAMDSafe ::
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

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndexedIndirectCountAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndexedIndirectCountAMD.html vkCmdDrawIndexedIndirectCountAMD registry at www.khronos.org>
type HS_vkCmdDrawIndexedIndirectCountAMD =
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

type PFN_vkCmdDrawIndexedIndirectCountAMD =
     FunPtr HS_vkCmdDrawIndexedIndirectCountAMD

foreign import ccall "dynamic"
               unwrapVkCmdDrawIndexedIndirectCountAMD ::
               PFN_vkCmdDrawIndexedIndirectCountAMD ->
                 HS_vkCmdDrawIndexedIndirectCountAMD

instance VulkanInstanceProc "vkCmdDrawIndexedIndirectCountAMD"
         where
        type VkInstanceProcType "vkCmdDrawIndexedIndirectCountAMD" =
             HS_vkCmdDrawIndexedIndirectCountAMD
        vkInstanceProcSymbol = _VkCmdDrawIndexedIndirectCountAMD

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkCmdDrawIndexedIndirectCountAMD

        {-# INLINE unwrapVkInstanceProc #-}

pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1

type VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1

pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: CString

pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME <-
        (is_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME -> True)
  where VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
          = _VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME

{-# INLINE _VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME #-}

_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: CString
_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  = Ptr "VK_AMD_draw_indirect_count\NUL"#

{-# INLINE is_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME #-}

is_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME

type VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME =
     "VK_AMD_draw_indirect_count"
