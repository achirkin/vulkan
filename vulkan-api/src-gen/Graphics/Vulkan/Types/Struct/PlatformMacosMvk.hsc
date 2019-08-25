#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformMacosMvk
       (VkMacOSSurfaceCreateInfoMVK) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkMacOSSurfaceCreateFlagsMVK)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkMacOSSurfaceCreateInfoMVK {
--   >     VkStructureType sType;
--   >     const void*                                    pNext;
--   >     VkMacOSSurfaceCreateFlagsMVK   flags;
--   >     const void*                                    pView;
--   > } VkMacOSSurfaceCreateInfoMVK;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMacOSSurfaceCreateInfoMVK VkMacOSSurfaceCreateInfoMVK registry at www.khronos.org>
type VkMacOSSurfaceCreateInfoMVK =
     VkStruct VkMacOSSurfaceCreateInfoMVK' -- ' closing tick for hsc2hs

data VkMacOSSurfaceCreateInfoMVK' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMacOSSurfaceCreateInfoMVK where
    type StructRep VkMacOSSurfaceCreateInfoMVK =
         'StructMeta "VkMacOSSurfaceCreateInfoMVK" -- ' closing tick for hsc2hs
           VkMacOSSurfaceCreateInfoMVK
           #{size VkMacOSSurfaceCreateInfoMVK}
           #{alignment VkMacOSSurfaceCreateInfoMVK}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMacOSSurfaceCreateInfoMVK, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMacOSSurfaceCreateInfoMVK, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkMacOSSurfaceCreateFlagsMVK 'True 
                                                                    #{offset VkMacOSSurfaceCreateInfoMVK, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pView" (Ptr Void) 'False 
                                                   #{offset VkMacOSSurfaceCreateInfoMVK, pView}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
