#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DispatchIndirectCommand
       (VkDispatchIndirectCommand) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkDispatchIndirectCommand {
--   >     uint32_t               x;
--   >     uint32_t               y;
--   >     uint32_t               z;
--   > } VkDispatchIndirectCommand;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDispatchIndirectCommand VkDispatchIndirectCommand registry at www.khronos.org>
type VkDispatchIndirectCommand =
     VkStruct VkDispatchIndirectCommand' -- ' closing tick for hsc2hs

data VkDispatchIndirectCommand' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDispatchIndirectCommand where
    type StructRep VkDispatchIndirectCommand =
         'StructMeta "VkDispatchIndirectCommand" VkDispatchIndirectCommand -- ' closing tick for hsc2hs
           #{size VkDispatchIndirectCommand}
           #{alignment VkDispatchIndirectCommand}
           '[('FieldMeta "x" Word32 'False  -- ' closing tick for hsc2hs
                                           #{offset VkDispatchIndirectCommand, x}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "y" Word32 'False 
                                           #{offset VkDispatchIndirectCommand, y}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "z" Word32 'False 
                                           #{offset VkDispatchIndirectCommand, z}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
