#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SubresourceLayout
       (VkSubresourceLayout) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes  (VkDeviceSize)

-- | > typedef struct VkSubresourceLayout {
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           size;
--   >     VkDeviceSize           rowPitch;
--   >     VkDeviceSize           arrayPitch;
--   >     VkDeviceSize           depthPitch;
--   > } VkSubresourceLayout;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubresourceLayout VkSubresourceLayout registry at www.khronos.org>
type VkSubresourceLayout = VkStruct VkSubresourceLayout' -- ' closing tick for hsc2hs

data VkSubresourceLayout' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSubresourceLayout where
    type StructRep VkSubresourceLayout =
         'StructMeta "VkSubresourceLayout" VkSubresourceLayout  -- ' closing tick for hsc2hs
                                                               #{size VkSubresourceLayout}
           #{alignment VkSubresourceLayout}
           '[('FieldMeta "offset" VkDeviceSize 'False  -- ' closing tick for hsc2hs
                                                      #{offset VkSubresourceLayout, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "size" VkDeviceSize 'False 
                                                    #{offset VkSubresourceLayout, size}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rowPitch" VkDeviceSize 'False 
                                                        #{offset VkSubresourceLayout, rowPitch}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "arrayPitch" VkDeviceSize 'False 
                                                          #{offset VkSubresourceLayout, arrayPitch}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthPitch" VkDeviceSize 'False 
                                                          #{offset VkSubresourceLayout, depthPitch}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
