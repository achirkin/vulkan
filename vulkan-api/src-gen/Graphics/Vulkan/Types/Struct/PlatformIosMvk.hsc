#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformIosMvk
       (VkIOSSurfaceCreateInfoMVK) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkIOSSurfaceCreateFlagsMVK)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkIOSSurfaceCreateInfoMVK {
--   >     VkStructureType sType;
--   >     const void*                                    pNext;
--   >     VkIOSSurfaceCreateFlagsMVK     flags;
--   >     const void*                                    pView;
--   > } VkIOSSurfaceCreateInfoMVK;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkIOSSurfaceCreateInfoMVK VkIOSSurfaceCreateInfoMVK registry at www.khronos.org>
type VkIOSSurfaceCreateInfoMVK =
     VkStruct VkIOSSurfaceCreateInfoMVK' -- ' closing tick for hsc2hs

data VkIOSSurfaceCreateInfoMVK' -- ' closing tick for hsc2hs

instance VulkanMarshal VkIOSSurfaceCreateInfoMVK where
    type StructRep VkIOSSurfaceCreateInfoMVK =
         'StructMeta "VkIOSSurfaceCreateInfoMVK" VkIOSSurfaceCreateInfoMVK -- ' closing tick for hsc2hs
           #{size VkIOSSurfaceCreateInfoMVK}
           #{alignment VkIOSSurfaceCreateInfoMVK}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkIOSSurfaceCreateInfoMVK, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkIOSSurfaceCreateInfoMVK, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkIOSSurfaceCreateFlagsMVK 'True 
                                                                  #{offset VkIOSSurfaceCreateInfoMVK, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pView" (Ptr Void) 'False 
                                                   #{offset VkIOSSurfaceCreateInfoMVK, pView}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
