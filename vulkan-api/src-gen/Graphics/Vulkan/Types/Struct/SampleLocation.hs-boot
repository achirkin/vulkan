{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-missing-methods #-}
module Graphics.Vulkan.Types.Struct.SampleLocation (VkSampleLocationsInfoEXT) where
import Graphics.Vulkan.Marshal.Internal

type VkSampleLocationsInfoEXT = VkStruct VkSampleLocationsInfoEXT'
data VkSampleLocationsInfoEXT'
instance VulkanMarshal (VkStruct VkSampleLocationsInfoEXT') where
