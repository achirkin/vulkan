#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Performance
       (VkPerformanceConfigurationAcquireInfoINTEL,
        VkPerformanceCounterDescriptionKHR, VkPerformanceCounterKHR,
        VkPerformanceCounterResultKHR, VkPerformanceMarkerInfoINTEL,
        VkPerformanceOverrideInfoINTEL, VkPerformanceQuerySubmitInfoKHR,
        VkPerformanceStreamMarkerInfoINTEL, VkPerformanceValueDataINTEL,
        VkPerformanceValueINTEL)
       where
import Graphics.Vulkan.Constants                (VK_MAX_DESCRIPTION_SIZE,
                                                 VK_UUID_SIZE)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import Graphics.Vulkan.Types.Enum.Performance   (VkPerformanceConfigurationTypeINTEL,
                                                 VkPerformanceCounterDescriptionFlagsKHR,
                                                 VkPerformanceCounterScopeKHR,
                                                 VkPerformanceCounterStorageKHR,
                                                 VkPerformanceCounterUnitKHR,
                                                 VkPerformanceOverrideTypeINTEL,
                                                 VkPerformanceValueTypeINTEL)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Struct.SubmitInfo  (VkSubmitInfo)

-- | > typedef struct VkPerformanceConfigurationAcquireInfoINTEL {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     VkPerformanceConfigurationTypeINTEL type;
--   > } VkPerformanceConfigurationAcquireInfoINTEL;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceConfigurationAcquireInfoINTEL VkPerformanceConfigurationAcquireInfoINTEL registry at www.khronos.org>
type VkPerformanceConfigurationAcquireInfoINTEL =
     VkStruct VkPerformanceConfigurationAcquireInfoINTEL' -- ' closing tick for hsc2hs

data VkPerformanceConfigurationAcquireInfoINTEL' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPerformanceConfigurationAcquireInfoINTEL
         where
    type StructRep VkPerformanceConfigurationAcquireInfoINTEL =
         'StructMeta "VkPerformanceConfigurationAcquireInfoINTEL" -- ' closing tick for hsc2hs
           VkPerformanceConfigurationAcquireInfoINTEL
           #{size VkPerformanceConfigurationAcquireInfoINTEL}
           #{alignment VkPerformanceConfigurationAcquireInfoINTEL}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPerformanceConfigurationAcquireInfoINTEL, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPerformanceConfigurationAcquireInfoINTEL, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkPerformanceConfigurationTypeINTEL 'False
                #{offset VkPerformanceConfigurationAcquireInfoINTEL, type}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPerformanceCounterDescriptionKHR {
--   >     VkStructureType sType;
--   >     const void*                             pNext;
--   >     VkPerformanceCounterDescriptionFlagsKHR flags;
--   >     char                                    name[VK_MAX_DESCRIPTION_SIZE];
--   >     char                                    category[VK_MAX_DESCRIPTION_SIZE];
--   >     char                                    description[VK_MAX_DESCRIPTION_SIZE];
--   > } VkPerformanceCounterDescriptionKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceCounterDescriptionKHR VkPerformanceCounterDescriptionKHR registry at www.khronos.org>
type VkPerformanceCounterDescriptionKHR =
     VkStruct VkPerformanceCounterDescriptionKHR' -- ' closing tick for hsc2hs

data VkPerformanceCounterDescriptionKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPerformanceCounterDescriptionKHR where
    type StructRep VkPerformanceCounterDescriptionKHR =
         'StructMeta "VkPerformanceCounterDescriptionKHR" -- ' closing tick for hsc2hs
           VkPerformanceCounterDescriptionKHR
           #{size VkPerformanceCounterDescriptionKHR}
           #{alignment VkPerformanceCounterDescriptionKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPerformanceCounterDescriptionKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPerformanceCounterDescriptionKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPerformanceCounterDescriptionFlagsKHR 'True
                #{offset VkPerformanceCounterDescriptionKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" CChar 'False 
                                             #{offset VkPerformanceCounterDescriptionKHR, name}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "category" CChar 'False 
                                                 #{offset VkPerformanceCounterDescriptionKHR, category}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "description" CChar 'False 
                                                    #{offset VkPerformanceCounterDescriptionKHR, description}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPerformanceCounterKHR {
--   >     VkStructureType sType;
--   >     const void*                             pNext;
--   >     VkPerformanceCounterUnitKHR        unit;
--   >     VkPerformanceCounterScopeKHR       scope;
--   >     VkPerformanceCounterStorageKHR     storage;
--   >     uint8_t uuid[VK_UUID_SIZE];
--   > } VkPerformanceCounterKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceCounterKHR VkPerformanceCounterKHR registry at www.khronos.org>
type VkPerformanceCounterKHR = VkStruct VkPerformanceCounterKHR' -- ' closing tick for hsc2hs

data VkPerformanceCounterKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPerformanceCounterKHR where
    type StructRep VkPerformanceCounterKHR =
         'StructMeta "VkPerformanceCounterKHR" VkPerformanceCounterKHR -- ' closing tick for hsc2hs
           #{size VkPerformanceCounterKHR}
           #{alignment VkPerformanceCounterKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPerformanceCounterKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPerformanceCounterKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "unit" VkPerformanceCounterUnitKHR 'False 
                                                                   #{offset VkPerformanceCounterKHR, unit}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "scope" VkPerformanceCounterScopeKHR 'False 
                                                                     #{offset VkPerformanceCounterKHR, scope}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storage" VkPerformanceCounterStorageKHR 'False
                #{offset VkPerformanceCounterKHR, storage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uuid" Word8 'False 
                                             #{offset VkPerformanceCounterKHR, uuid}
                VK_UUID_SIZE
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | // Union of all the possible return types a counter result could return
--
--   > typedef union VkPerformanceCounterResultKHR {
--   >     int32_t  int32;
--   >     int64_t  int64;
--   >     uint32_t uint32;
--   >     uint64_t uint64;
--   >     float    float32;
--   >     double   float64;
--   > } VkPerformanceCounterResultKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceCounterResultKHR VkPerformanceCounterResultKHR registry at www.khronos.org>
type VkPerformanceCounterResultKHR =
     VkStruct VkPerformanceCounterResultKHR' -- ' closing tick for hsc2hs

data VkPerformanceCounterResultKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPerformanceCounterResultKHR where
    type StructRep VkPerformanceCounterResultKHR =
         'StructMeta "VkPerformanceCounterResultKHR" -- ' closing tick for hsc2hs
           VkPerformanceCounterResultKHR
           #{size VkPerformanceCounterResultKHR}
           #{alignment VkPerformanceCounterResultKHR}
           '[('FieldMeta "int32" Int32 'False  -- ' closing tick for hsc2hs
                                              #{offset VkPerformanceCounterResultKHR, int32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "int64" Int64 'False 
                                              #{offset VkPerformanceCounterResultKHR, int64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uint32" Word32 'False 
                                                #{offset VkPerformanceCounterResultKHR, uint32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uint64" Word64 'False 
                                                #{offset VkPerformanceCounterResultKHR, uint64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "float32" ( -- ' closing tick for hsc2hs
                                    #{type float}
                                    ) 'False  -- ' closing tick for hsc2hs
                                             #{offset VkPerformanceCounterResultKHR, float32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "float64" ( -- ' closing tick for hsc2hs
                                    #{type double}
                                    ) 'False  -- ' closing tick for hsc2hs
                                             #{offset VkPerformanceCounterResultKHR, float64}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPerformanceMarkerInfoINTEL {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     uint64_t                            marker;
--   > } VkPerformanceMarkerInfoINTEL;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceMarkerInfoINTEL VkPerformanceMarkerInfoINTEL registry at www.khronos.org>
type VkPerformanceMarkerInfoINTEL =
     VkStruct VkPerformanceMarkerInfoINTEL' -- ' closing tick for hsc2hs

data VkPerformanceMarkerInfoINTEL' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPerformanceMarkerInfoINTEL where
    type StructRep VkPerformanceMarkerInfoINTEL =
         'StructMeta "VkPerformanceMarkerInfoINTEL" -- ' closing tick for hsc2hs
           VkPerformanceMarkerInfoINTEL
           #{size VkPerformanceMarkerInfoINTEL}
           #{alignment VkPerformanceMarkerInfoINTEL}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPerformanceMarkerInfoINTEL, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPerformanceMarkerInfoINTEL, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "marker" Word64 'False 
                                                #{offset VkPerformanceMarkerInfoINTEL, marker}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPerformanceOverrideInfoINTEL {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     VkPerformanceOverrideTypeINTEL      type;
--   >     VkBool32                            enable;
--   >     uint64_t                            parameter;
--   > } VkPerformanceOverrideInfoINTEL;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceOverrideInfoINTEL VkPerformanceOverrideInfoINTEL registry at www.khronos.org>
type VkPerformanceOverrideInfoINTEL =
     VkStruct VkPerformanceOverrideInfoINTEL' -- ' closing tick for hsc2hs

data VkPerformanceOverrideInfoINTEL' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPerformanceOverrideInfoINTEL where
    type StructRep VkPerformanceOverrideInfoINTEL =
         'StructMeta "VkPerformanceOverrideInfoINTEL" -- ' closing tick for hsc2hs
           VkPerformanceOverrideInfoINTEL
           #{size VkPerformanceOverrideInfoINTEL}
           #{alignment VkPerformanceOverrideInfoINTEL}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPerformanceOverrideInfoINTEL, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPerformanceOverrideInfoINTEL, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkPerformanceOverrideTypeINTEL 'False 
                                                                      #{offset VkPerformanceOverrideInfoINTEL, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "enable" VkBool32 'False 
                                                  #{offset VkPerformanceOverrideInfoINTEL, enable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "parameter" Word64 'False 
                                                   #{offset VkPerformanceOverrideInfoINTEL, parameter}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPerformanceQuerySubmitInfoKHR {
--   >     VkStructureType sType;
--   >     const void*         pNext;
--   >     uint32_t            counterPassIndex;
--   > } VkPerformanceQuerySubmitInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceQuerySubmitInfoKHR VkPerformanceQuerySubmitInfoKHR registry at www.khronos.org>
type VkPerformanceQuerySubmitInfoKHR =
     VkStruct VkPerformanceQuerySubmitInfoKHR' -- ' closing tick for hsc2hs

data VkPerformanceQuerySubmitInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPerformanceQuerySubmitInfoKHR where
    type StructRep VkPerformanceQuerySubmitInfoKHR =
         'StructMeta "VkPerformanceQuerySubmitInfoKHR" -- ' closing tick for hsc2hs
           VkPerformanceQuerySubmitInfoKHR
           #{size VkPerformanceQuerySubmitInfoKHR}
           #{alignment VkPerformanceQuerySubmitInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPerformanceQuerySubmitInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPerformanceQuerySubmitInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "counterPassIndex" Word32 'False 
                                                          #{offset VkPerformanceQuerySubmitInfoKHR, counterPassIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSubmitInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPerformanceStreamMarkerInfoINTEL {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     uint32_t                            marker;
--   > } VkPerformanceStreamMarkerInfoINTEL;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceStreamMarkerInfoINTEL VkPerformanceStreamMarkerInfoINTEL registry at www.khronos.org>
type VkPerformanceStreamMarkerInfoINTEL =
     VkStruct VkPerformanceStreamMarkerInfoINTEL' -- ' closing tick for hsc2hs

data VkPerformanceStreamMarkerInfoINTEL' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPerformanceStreamMarkerInfoINTEL where
    type StructRep VkPerformanceStreamMarkerInfoINTEL =
         'StructMeta "VkPerformanceStreamMarkerInfoINTEL" -- ' closing tick for hsc2hs
           VkPerformanceStreamMarkerInfoINTEL
           #{size VkPerformanceStreamMarkerInfoINTEL}
           #{alignment VkPerformanceStreamMarkerInfoINTEL}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPerformanceStreamMarkerInfoINTEL, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPerformanceStreamMarkerInfoINTEL, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "marker" Word32 'False 
                                                #{offset VkPerformanceStreamMarkerInfoINTEL, marker}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef union VkPerformanceValueDataINTEL {
--   >     uint32_t                           value32;
--   >     uint64_t                           value64;
--   >     float                               valueFloat;
--   >     VkBool32                             valueBool;
--   >     const char*  valueString;
--   > } VkPerformanceValueDataINTEL;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceValueDataINTEL VkPerformanceValueDataINTEL registry at www.khronos.org>
type VkPerformanceValueDataINTEL =
     VkStruct VkPerformanceValueDataINTEL' -- ' closing tick for hsc2hs

data VkPerformanceValueDataINTEL' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPerformanceValueDataINTEL where
    type StructRep VkPerformanceValueDataINTEL =
         'StructMeta "VkPerformanceValueDataINTEL" -- ' closing tick for hsc2hs
           VkPerformanceValueDataINTEL
           #{size VkPerformanceValueDataINTEL}
           #{alignment VkPerformanceValueDataINTEL}
           '[('FieldMeta "value32" Word32 'False  -- ' closing tick for hsc2hs
                                                 #{offset VkPerformanceValueDataINTEL, value32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "value64" Word64 'False 
                                                 #{offset VkPerformanceValueDataINTEL, value64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "valueFloat" ( -- ' closing tick for hsc2hs
                                       #{type float}
                                       ) 'False -- ' closing tick for hsc2hs
                #{offset VkPerformanceValueDataINTEL, valueFloat}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "valueBool" VkBool32 'False 
                                                     #{offset VkPerformanceValueDataINTEL, valueBool}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "valueString" CString 'False 
                                                      #{offset VkPerformanceValueDataINTEL, valueString}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPerformanceValueINTEL {
--   >     VkPerformanceValueTypeINTEL        type;
--   >     VkPerformanceValueDataINTEL        data;
--   > } VkPerformanceValueINTEL;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceValueINTEL VkPerformanceValueINTEL registry at www.khronos.org>
type VkPerformanceValueINTEL = VkStruct VkPerformanceValueINTEL' -- ' closing tick for hsc2hs

data VkPerformanceValueINTEL' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPerformanceValueINTEL where
    type StructRep VkPerformanceValueINTEL =
         'StructMeta "VkPerformanceValueINTEL" VkPerformanceValueINTEL -- ' closing tick for hsc2hs
           #{size VkPerformanceValueINTEL}
           #{alignment VkPerformanceValueINTEL}
           '[('FieldMeta "type" VkPerformanceValueTypeINTEL 'False  -- ' closing tick for hsc2hs
                                                                   #{offset VkPerformanceValueINTEL, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "data" VkPerformanceValueDataINTEL 'False 
                                                                   #{offset VkPerformanceValueINTEL, data}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
