#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.InitializePerformanceApiInfoINTEL
       (VkInitializePerformanceApiInfoINTEL) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkInitializePerformanceApiInfoINTEL {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     void*               pUserData;
--   > } VkInitializePerformanceApiInfoINTEL;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkInitializePerformanceApiInfoINTEL VkInitializePerformanceApiInfoINTEL registry at www.khronos.org>
type VkInitializePerformanceApiInfoINTEL =
     VkStruct VkInitializePerformanceApiInfoINTEL' -- ' closing tick for hsc2hs

data VkInitializePerformanceApiInfoINTEL' -- ' closing tick for hsc2hs

instance VulkanMarshal VkInitializePerformanceApiInfoINTEL where
    type StructRep VkInitializePerformanceApiInfoINTEL =
         'StructMeta "VkInitializePerformanceApiInfoINTEL" -- ' closing tick for hsc2hs
           VkInitializePerformanceApiInfoINTEL
           #{size VkInitializePerformanceApiInfoINTEL}
           #{alignment VkInitializePerformanceApiInfoINTEL}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkInitializePerformanceApiInfoINTEL, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkInitializePerformanceApiInfoINTEL, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pUserData" (Ptr Void) 'True 
                                                      #{offset VkInitializePerformanceApiInfoINTEL, pUserData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
