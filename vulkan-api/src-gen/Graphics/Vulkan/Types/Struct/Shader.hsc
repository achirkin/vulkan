#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Shader
       (VkShaderModuleCreateInfo,
        VkShaderModuleValidationCacheCreateInfoEXT,
        VkShaderResourceUsageAMD, VkShaderStatisticsInfoAMD)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkShaderModuleCreateFlags)
import           Graphics.Vulkan.Types.Enum.Shader        (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkValidationCacheEXT)

-- | > typedef struct VkShaderModuleCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkShaderModuleCreateFlags flags;
--   >     size_t                 codeSize;
--   >     const uint32_t*            pCode;
--   > } VkShaderModuleCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkShaderModuleCreateInfo VkShaderModuleCreateInfo registry at www.khronos.org>
type VkShaderModuleCreateInfo = VkStruct VkShaderModuleCreateInfo' -- ' closing tick for hsc2hs

data VkShaderModuleCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkShaderModuleCreateInfo where
    type StructRep VkShaderModuleCreateInfo =
         'StructMeta "VkShaderModuleCreateInfo" VkShaderModuleCreateInfo -- ' closing tick for hsc2hs
           #{size VkShaderModuleCreateInfo}
           #{alignment VkShaderModuleCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkShaderModuleCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkShaderModuleCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkShaderModuleCreateFlags 'True 
                                                                 #{offset VkShaderModuleCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "codeSize" CSize 'False 
                                                 #{offset VkShaderModuleCreateInfo, codeSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pCode" (Ptr Word32) 'False 
                                                     #{offset VkShaderModuleCreateInfo, pCode}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkShaderModuleValidationCacheCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkValidationCacheEXT    validationCache;
--   > } VkShaderModuleValidationCacheCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkShaderModuleValidationCacheCreateInfoEXT VkShaderModuleValidationCacheCreateInfoEXT registry at www.khronos.org>
type VkShaderModuleValidationCacheCreateInfoEXT =
     VkStruct VkShaderModuleValidationCacheCreateInfoEXT' -- ' closing tick for hsc2hs

data VkShaderModuleValidationCacheCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkShaderModuleValidationCacheCreateInfoEXT
         where
    type StructRep VkShaderModuleValidationCacheCreateInfoEXT =
         'StructMeta "VkShaderModuleValidationCacheCreateInfoEXT" -- ' closing tick for hsc2hs
           VkShaderModuleValidationCacheCreateInfoEXT
           #{size VkShaderModuleValidationCacheCreateInfoEXT}
           #{alignment VkShaderModuleValidationCacheCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "validationCache" VkValidationCacheEXT 'False 
                                                                       #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkShaderModuleCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkShaderResourceUsageAMD {
--   >     uint32_t numUsedVgprs;
--   >     uint32_t numUsedSgprs;
--   >     uint32_t ldsSizePerLocalWorkGroup;
--   >     size_t ldsUsageSizeInBytes;
--   >     size_t scratchMemUsageInBytes;
--   > } VkShaderResourceUsageAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkShaderResourceUsageAMD VkShaderResourceUsageAMD registry at www.khronos.org>
type VkShaderResourceUsageAMD = VkStruct VkShaderResourceUsageAMD' -- ' closing tick for hsc2hs

data VkShaderResourceUsageAMD' -- ' closing tick for hsc2hs

instance VulkanMarshal VkShaderResourceUsageAMD where
    type StructRep VkShaderResourceUsageAMD =
         'StructMeta "VkShaderResourceUsageAMD" VkShaderResourceUsageAMD -- ' closing tick for hsc2hs
           #{size VkShaderResourceUsageAMD}
           #{alignment VkShaderResourceUsageAMD}
           '[('FieldMeta "numUsedVgprs" Word32 'False  -- ' closing tick for hsc2hs
                                                      #{offset VkShaderResourceUsageAMD, numUsedVgprs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "numUsedSgprs" Word32 'False 
                                                      #{offset VkShaderResourceUsageAMD, numUsedSgprs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ldsSizePerLocalWorkGroup" Word32 'False 
                                                                  #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ldsUsageSizeInBytes" CSize 'False 
                                                            #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "scratchMemUsageInBytes" CSize 'False 
                                                               #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkShaderStatisticsInfoAMD {
--   >     VkShaderStageFlags shaderStageMask;
--   >     VkShaderResourceUsageAMD resourceUsage;
--   >     uint32_t numPhysicalVgprs;
--   >     uint32_t numPhysicalSgprs;
--   >     uint32_t numAvailableVgprs;
--   >     uint32_t numAvailableSgprs;
--   >     uint32_t computeWorkGroupSize[3];
--   > } VkShaderStatisticsInfoAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkShaderStatisticsInfoAMD VkShaderStatisticsInfoAMD registry at www.khronos.org>
type VkShaderStatisticsInfoAMD =
     VkStruct VkShaderStatisticsInfoAMD' -- ' closing tick for hsc2hs

data VkShaderStatisticsInfoAMD' -- ' closing tick for hsc2hs

instance VulkanMarshal VkShaderStatisticsInfoAMD where
    type StructRep VkShaderStatisticsInfoAMD =
         'StructMeta "VkShaderStatisticsInfoAMD" VkShaderStatisticsInfoAMD -- ' closing tick for hsc2hs
           #{size VkShaderStatisticsInfoAMD}
           #{alignment VkShaderStatisticsInfoAMD}
           '[('FieldMeta "shaderStageMask" VkShaderStageFlags 'False  -- ' closing tick for hsc2hs
                                                                     #{offset VkShaderStatisticsInfoAMD, shaderStageMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "resourceUsage" VkShaderResourceUsageAMD 'False
                #{offset VkShaderStatisticsInfoAMD, resourceUsage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "numPhysicalVgprs" Word32 'False 
                                                          #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "numPhysicalSgprs" Word32 'False 
                                                          #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "numAvailableVgprs" Word32 'False 
                                                           #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "numAvailableSgprs" Word32 'False 
                                                           #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "computeWorkGroupSize" Word32 'False 
                                                              #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}
                3
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
