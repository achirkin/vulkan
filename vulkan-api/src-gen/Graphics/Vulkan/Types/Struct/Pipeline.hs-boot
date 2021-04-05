{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-missing-methods #-}
module Graphics.Vulkan.Types.Struct.Pipeline (VkPipelineShaderStageCreateInfo) where
import Graphics.Vulkan.Marshal.Internal

type VkPipelineShaderStageCreateInfo = VkStruct VkPipelineShaderStageCreateInfo'
data VkPipelineShaderStageCreateInfo'
instance VulkanMarshal (VkStruct VkPipelineShaderStageCreateInfo') where
