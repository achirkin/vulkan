{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-missing-methods #-}
module Graphics.Vulkan.Types.Struct.WriteDescriptorSet (VkWriteDescriptorSet) where
import Graphics.Vulkan.Marshal.Internal
type VkWriteDescriptorSet = VkStruct VkWriteDescriptorSet'
data VkWriteDescriptorSet'
instance VulkanMarshal (VkStruct VkWriteDescriptorSet') where
