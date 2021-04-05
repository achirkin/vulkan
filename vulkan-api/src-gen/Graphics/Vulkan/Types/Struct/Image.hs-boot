{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-missing-methods #-}
module Graphics.Vulkan.Types.Struct.Image (VkImageSubresource, VkImageMemoryBarrier) where
import Graphics.Vulkan.Marshal.Internal

type VkImageSubresource = VkStruct VkImageSubresource'
data VkImageSubresource'
instance VulkanMarshal (VkStruct VkImageSubresource') where

type VkImageMemoryBarrier = VkStruct VkImageMemoryBarrier'
data VkImageMemoryBarrier'
instance VulkanMarshal (VkStruct VkImageMemoryBarrier') where
