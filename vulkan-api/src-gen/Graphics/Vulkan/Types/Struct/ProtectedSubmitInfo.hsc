#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ProtectedSubmitInfo
       (VkProtectedSubmitInfo) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.SubmitInfo  (VkSubmitInfo)

-- | > typedef struct VkProtectedSubmitInfo {
--   >     VkStructureType sType;
--   >     const void*                     pNext;
--   >     VkBool32                        protectedSubmit;
--   > } VkProtectedSubmitInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkProtectedSubmitInfo VkProtectedSubmitInfo registry at www.khronos.org>
type VkProtectedSubmitInfo = VkStruct VkProtectedSubmitInfo' -- ' closing tick for hsc2hs

data VkProtectedSubmitInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkProtectedSubmitInfo where
    type StructRep VkProtectedSubmitInfo =
         'StructMeta "VkProtectedSubmitInfo" VkProtectedSubmitInfo  -- ' closing tick for hsc2hs
                                                                   #{size VkProtectedSubmitInfo}
           #{alignment VkProtectedSubmitInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkProtectedSubmitInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkProtectedSubmitInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "protectedSubmit" VkBool32 'False 
                                                           #{offset VkProtectedSubmitInfo, protectedSubmit}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSubmitInfo] -- ' closing tick for hsc2hs
