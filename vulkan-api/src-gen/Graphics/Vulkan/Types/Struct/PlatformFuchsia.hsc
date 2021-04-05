#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformFuchsia
       (VkImagePipeSurfaceCreateInfoFUCHSIA) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Bitmasks           (VkImagePipeSurfaceCreateFlagsFUCHSIA)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Include            (Zx_handle_t)

-- | > typedef struct VkImagePipeSurfaceCreateInfoFUCHSIA {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImagePipeSurfaceCreateFlagsFUCHSIA   flags;
--   >     zx_handle_t                      imagePipeHandle;
--   > } VkImagePipeSurfaceCreateInfoFUCHSIA;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImagePipeSurfaceCreateInfoFUCHSIA VkImagePipeSurfaceCreateInfoFUCHSIA registry at www.khronos.org>
type VkImagePipeSurfaceCreateInfoFUCHSIA =
     VkStruct VkImagePipeSurfaceCreateInfoFUCHSIA' -- ' closing tick for hsc2hs

data VkImagePipeSurfaceCreateInfoFUCHSIA' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImagePipeSurfaceCreateInfoFUCHSIA where
    type StructRep VkImagePipeSurfaceCreateInfoFUCHSIA =
         'StructMeta "VkImagePipeSurfaceCreateInfoFUCHSIA" -- ' closing tick for hsc2hs
           VkImagePipeSurfaceCreateInfoFUCHSIA
           #{size VkImagePipeSurfaceCreateInfoFUCHSIA}
           #{alignment VkImagePipeSurfaceCreateInfoFUCHSIA}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImagePipeSurfaceCreateInfoFUCHSIA, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImagePipeSurfaceCreateInfoFUCHSIA, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkImagePipeSurfaceCreateFlagsFUCHSIA 'True
                #{offset VkImagePipeSurfaceCreateInfoFUCHSIA, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imagePipeHandle" Zx_handle_t 'False 
                                                              #{offset VkImagePipeSurfaceCreateInfoFUCHSIA, imagePipeHandle}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
