#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.StencilOpState
       (VkStencilOpState) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.CompareOp (VkCompareOp)
import           Graphics.Vulkan.Types.Enum.Stencil   (VkStencilOp)

-- | > typedef struct VkStencilOpState {
--   >     VkStencilOp            failOp;
--   >     VkStencilOp            passOp;
--   >     VkStencilOp            depthFailOp;
--   >     VkCompareOp            compareOp;
--   >     uint32_t               compareMask;
--   >     uint32_t               writeMask;
--   >     uint32_t               reference;
--   > } VkStencilOpState;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkStencilOpState VkStencilOpState registry at www.khronos.org>
type VkStencilOpState = VkStruct VkStencilOpState' -- ' closing tick for hsc2hs

data VkStencilOpState' -- ' closing tick for hsc2hs

instance VulkanMarshal VkStencilOpState where
    type StructRep VkStencilOpState =
         'StructMeta "VkStencilOpState" VkStencilOpState  -- ' closing tick for hsc2hs
                                                         #{size VkStencilOpState}
           #{alignment VkStencilOpState}
           '[('FieldMeta "failOp" VkStencilOp 'False  -- ' closing tick for hsc2hs
                                                     #{offset VkStencilOpState, failOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "passOp" VkStencilOp 'False 
                                                     #{offset VkStencilOpState, passOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthFailOp" VkStencilOp 'False 
                                                          #{offset VkStencilOpState, depthFailOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compareOp" VkCompareOp 'False 
                                                        #{offset VkStencilOpState, compareOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compareMask" Word32 'False 
                                                     #{offset VkStencilOpState, compareMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "writeMask" Word32 'False 
                                                   #{offset VkStencilOpState, writeMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "reference" Word32 'False 
                                                   #{offset VkStencilOpState, reference}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
