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
module Graphics.Vulkan.Ext.VK_KHR_draw_indirect_count
       (-- * Vulkan extension: @VK_KHR_draw_indirect_count@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell-nv@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @170@
        VkCmdDrawIndirectCountKHR, pattern VkCmdDrawIndirectCountKHR,
        HS_vkCmdDrawIndirectCountKHR, PFN_vkCmdDrawIndirectCountKHR,
        VkCmdDrawIndexedIndirectCountKHR,
        pattern VkCmdDrawIndexedIndirectCountKHR,
        HS_vkCmdDrawIndexedIndirectCountKHR,
        PFN_vkCmdDrawIndexedIndirectCountKHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION,
        pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION,
        VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME,
        pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME)
       where
import           GHC.Ptr                         (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc    (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Handles

pattern VkCmdDrawIndirectCountKHR :: CString

pattern VkCmdDrawIndirectCountKHR <-
        (is_VkCmdDrawIndirectCountKHR -> True)
  where VkCmdDrawIndirectCountKHR = _VkCmdDrawIndirectCountKHR

{-# INLINE _VkCmdDrawIndirectCountKHR #-}

_VkCmdDrawIndirectCountKHR :: CString
_VkCmdDrawIndirectCountKHR = Ptr "vkCmdDrawIndirectCountKHR\NUL"#

{-# INLINE is_VkCmdDrawIndirectCountKHR #-}

is_VkCmdDrawIndirectCountKHR :: CString -> Bool
is_VkCmdDrawIndirectCountKHR
  = (EQ ==) . cmpCStrings _VkCmdDrawIndirectCountKHR

type VkCmdDrawIndirectCountKHR = "vkCmdDrawIndirectCountKHR"

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawIndirectCountKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndirectCountKHR vkCmdDrawIndirectCountKHR registry at www.khronos.org>
type HS_vkCmdDrawIndirectCountKHR =
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

type PFN_vkCmdDrawIndirectCountKHR =
     FunPtr HS_vkCmdDrawIndirectCountKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDrawIndirectCountKHRUnsafe ::
               PFN_vkCmdDrawIndirectCountKHR -> HS_vkCmdDrawIndirectCountKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdDrawIndirectCountKHRSafe ::
               PFN_vkCmdDrawIndirectCountKHR -> HS_vkCmdDrawIndirectCountKHR

instance VulkanProc "vkCmdDrawIndirectCountKHR" where
        type VkProcType "vkCmdDrawIndirectCountKHR" =
             HS_vkCmdDrawIndirectCountKHR
        vkProcSymbol = _VkCmdDrawIndirectCountKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCmdDrawIndirectCountKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDrawIndirectCountKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdDrawIndexedIndirectCountKHR :: CString

pattern VkCmdDrawIndexedIndirectCountKHR <-
        (is_VkCmdDrawIndexedIndirectCountKHR -> True)
  where VkCmdDrawIndexedIndirectCountKHR
          = _VkCmdDrawIndexedIndirectCountKHR

{-# INLINE _VkCmdDrawIndexedIndirectCountKHR #-}

_VkCmdDrawIndexedIndirectCountKHR :: CString
_VkCmdDrawIndexedIndirectCountKHR
  = Ptr "vkCmdDrawIndexedIndirectCountKHR\NUL"#

{-# INLINE is_VkCmdDrawIndexedIndirectCountKHR #-}

is_VkCmdDrawIndexedIndirectCountKHR :: CString -> Bool
is_VkCmdDrawIndexedIndirectCountKHR
  = (EQ ==) . cmpCStrings _VkCmdDrawIndexedIndirectCountKHR

type VkCmdDrawIndexedIndirectCountKHR =
     "vkCmdDrawIndexedIndirectCountKHR"

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawIndexedIndirectCountKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndexedIndirectCountKHR vkCmdDrawIndexedIndirectCountKHR registry at www.khronos.org>
type HS_vkCmdDrawIndexedIndirectCountKHR =
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

type PFN_vkCmdDrawIndexedIndirectCountKHR =
     FunPtr HS_vkCmdDrawIndexedIndirectCountKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDrawIndexedIndirectCountKHRUnsafe ::
               PFN_vkCmdDrawIndexedIndirectCountKHR ->
                 HS_vkCmdDrawIndexedIndirectCountKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdDrawIndexedIndirectCountKHRSafe ::
               PFN_vkCmdDrawIndexedIndirectCountKHR ->
                 HS_vkCmdDrawIndexedIndirectCountKHR

instance VulkanProc "vkCmdDrawIndexedIndirectCountKHR" where
        type VkProcType "vkCmdDrawIndexedIndirectCountKHR" =
             HS_vkCmdDrawIndexedIndirectCountKHR
        vkProcSymbol = _VkCmdDrawIndexedIndirectCountKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkCmdDrawIndexedIndirectCountKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDrawIndexedIndirectCountKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1

type VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1

pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: CString

pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME <-
        (is_VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME -> True)
  where VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
          = _VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME

{-# INLINE _VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME #-}

_VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: CString
_VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  = Ptr "VK_KHR_draw_indirect_count\NUL"#

{-# INLINE is_VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME #-}

is_VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME

type VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME =
     "VK_KHR_draw_indirect_count"
