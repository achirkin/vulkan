#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.CooperativeMatrixPropertiesNV
       (VkCooperativeMatrixPropertiesNV) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.Component     (VkComponentTypeNV)
import Graphics.Vulkan.Types.Enum.ScopeNV       (VkScopeNV)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkCooperativeMatrixPropertiesNV {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     uint32_t                            MSize;
--   >     uint32_t                            NSize;
--   >     uint32_t                            KSize;
--   >     VkComponentTypeNV                   AType;
--   >     VkComponentTypeNV                   BType;
--   >     VkComponentTypeNV                   CType;
--   >     VkComponentTypeNV                   DType;
--   >     VkScopeNV                           scope;
--   > } VkCooperativeMatrixPropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCooperativeMatrixPropertiesNV VkCooperativeMatrixPropertiesNV registry at www.khronos.org>
type VkCooperativeMatrixPropertiesNV =
     VkStruct VkCooperativeMatrixPropertiesNV' -- ' closing tick for hsc2hs

data VkCooperativeMatrixPropertiesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCooperativeMatrixPropertiesNV where
    type StructRep VkCooperativeMatrixPropertiesNV =
         'StructMeta "VkCooperativeMatrixPropertiesNV" -- ' closing tick for hsc2hs
           VkCooperativeMatrixPropertiesNV
           #{size VkCooperativeMatrixPropertiesNV}
           #{alignment VkCooperativeMatrixPropertiesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCooperativeMatrixPropertiesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkCooperativeMatrixPropertiesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "MSize" Word32 'False 
                                               #{offset VkCooperativeMatrixPropertiesNV, MSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "NSize" Word32 'False 
                                               #{offset VkCooperativeMatrixPropertiesNV, NSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "KSize" Word32 'False 
                                               #{offset VkCooperativeMatrixPropertiesNV, KSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "AType" VkComponentTypeNV 'False 
                                                          #{offset VkCooperativeMatrixPropertiesNV, AType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "BType" VkComponentTypeNV 'False 
                                                          #{offset VkCooperativeMatrixPropertiesNV, BType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "CType" VkComponentTypeNV 'False 
                                                          #{offset VkCooperativeMatrixPropertiesNV, CType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "DType" VkComponentTypeNV 'False 
                                                          #{offset VkCooperativeMatrixPropertiesNV, DType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "scope" VkScopeNV 'False 
                                                  #{offset VkCooperativeMatrixPropertiesNV, scope}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
