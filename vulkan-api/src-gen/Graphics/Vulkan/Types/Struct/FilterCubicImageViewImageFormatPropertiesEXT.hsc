#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.FilterCubicImageViewImageFormatPropertiesEXT
       (VkFilterCubicImageViewImageFormatPropertiesEXT) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Struct.Image       (VkImageFormatProperties2)

-- | > typedef struct VkFilterCubicImageViewImageFormatPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         filterCubic;
--   >     VkBool32                         filterCubicMinmax;
--   > } VkFilterCubicImageViewImageFormatPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFilterCubicImageViewImageFormatPropertiesEXT VkFilterCubicImageViewImageFormatPropertiesEXT registry at www.khronos.org>
type VkFilterCubicImageViewImageFormatPropertiesEXT =
     VkStruct VkFilterCubicImageViewImageFormatPropertiesEXT' -- ' closing tick for hsc2hs

data VkFilterCubicImageViewImageFormatPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkFilterCubicImageViewImageFormatPropertiesEXT
         where
    type StructRep VkFilterCubicImageViewImageFormatPropertiesEXT =
         'StructMeta "VkFilterCubicImageViewImageFormatPropertiesEXT" -- ' closing tick for hsc2hs
           VkFilterCubicImageViewImageFormatPropertiesEXT
           #{size VkFilterCubicImageViewImageFormatPropertiesEXT}
           #{alignment VkFilterCubicImageViewImageFormatPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkFilterCubicImageViewImageFormatPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkFilterCubicImageViewImageFormatPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "filterCubic" VkBool32 'False 
                                                       #{offset VkFilterCubicImageViewImageFormatPropertiesEXT, filterCubic}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "filterCubicMinmax" VkBool32 'False 
                                                             #{offset VkFilterCubicImageViewImageFormatPropertiesEXT, filterCubicMinmax}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkImageFormatProperties2] -- ' closing tick for hsc2hs
