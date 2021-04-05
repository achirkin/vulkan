{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-missing-methods #-}
module Graphics.Vulkan.Types.Struct.EnableBetaExtensions (VkBindAccelerationStructureMemoryInfoKHR) where
import Graphics.Vulkan.Marshal.Internal

type VkBindAccelerationStructureMemoryInfoKHR = VkStruct VkBindAccelerationStructureMemoryInfoKHR'
data VkBindAccelerationStructureMemoryInfoKHR'
instance VulkanMarshal VkBindAccelerationStructureMemoryInfoKHR
