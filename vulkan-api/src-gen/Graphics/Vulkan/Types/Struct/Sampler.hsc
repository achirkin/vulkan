#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Sampler
       (VkSamplerCreateInfo, VkSamplerReductionModeCreateInfoEXT,
        VkSamplerYcbcrConversionCreateInfo,
        VkSamplerYcbcrConversionCreateInfoKHR,
        VkSamplerYcbcrConversionImageFormatProperties,
        VkSamplerYcbcrConversionImageFormatPropertiesKHR,
        VkSamplerYcbcrConversionInfo, VkSamplerYcbcrConversionInfoKHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks                (VkSamplerCreateFlags)
import           Graphics.Vulkan.Types.Enum.BorderColor        (VkBorderColor)
import           Graphics.Vulkan.Types.Enum.ChromaLocation     (VkChromaLocation)
import           Graphics.Vulkan.Types.Enum.CompareOp          (VkCompareOp)
import           Graphics.Vulkan.Types.Enum.Filter             (VkFilter)
import           Graphics.Vulkan.Types.Enum.Format             (VkFormat)
import           Graphics.Vulkan.Types.Enum.Sampler            (VkSamplerAddressMode,
                                                                VkSamplerMipmapMode,
                                                                VkSamplerReductionModeEXT,
                                                                VkSamplerYcbcrModelConversion,
                                                                VkSamplerYcbcrRange)
import           Graphics.Vulkan.Types.Enum.StructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                 (VkSamplerYcbcrConversion)
import           Graphics.Vulkan.Types.Struct.ComponentMapping (VkComponentMapping)
import           Graphics.Vulkan.Types.Struct.Image            (VkImageFormatProperties2,
                                                                VkImageViewCreateInfo)

-- | > typedef struct VkSamplerCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSamplerCreateFlags   flags;
--   >     VkFilter               magFilter;
--   >     VkFilter               minFilter;
--   >     VkSamplerMipmapMode    mipmapMode;
--   >     VkSamplerAddressMode   addressModeU;
--   >     VkSamplerAddressMode   addressModeV;
--   >     VkSamplerAddressMode   addressModeW;
--   >     float                  mipLodBias;
--   >     VkBool32               anisotropyEnable;
--   >     float                  maxAnisotropy;
--   >     VkBool32               compareEnable;
--   >     VkCompareOp            compareOp;
--   >     float                  minLod;
--   >     float                  maxLod;
--   >     VkBorderColor          borderColor;
--   >     VkBool32               unnormalizedCoordinates;
--   > } VkSamplerCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerCreateInfo VkSamplerCreateInfo registry at www.khronos.org>
type VkSamplerCreateInfo = VkStruct VkSamplerCreateInfo' -- ' closing tick for hsc2hs

data VkSamplerCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSamplerCreateInfo where
    type StructRep VkSamplerCreateInfo =
         'StructMeta "VkSamplerCreateInfo" VkSamplerCreateInfo  -- ' closing tick for hsc2hs
                                                               #{size VkSamplerCreateInfo}
           #{alignment VkSamplerCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSamplerCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSamplerCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkSamplerCreateFlags 'True 
                                                            #{offset VkSamplerCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "magFilter" VkFilter 'False 
                                                     #{offset VkSamplerCreateInfo, magFilter}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minFilter" VkFilter 'False 
                                                     #{offset VkSamplerCreateInfo, minFilter}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mipmapMode" VkSamplerMipmapMode 'False 
                                                                 #{offset VkSamplerCreateInfo, mipmapMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "addressModeU" VkSamplerAddressMode 'False 
                                                                    #{offset VkSamplerCreateInfo, addressModeU}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "addressModeV" VkSamplerAddressMode 'False 
                                                                    #{offset VkSamplerCreateInfo, addressModeV}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "addressModeW" VkSamplerAddressMode 'False 
                                                                    #{offset VkSamplerCreateInfo, addressModeW}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mipLodBias" ( -- ' closing tick for hsc2hs
                                       #{type float}
                                       ) 'False -- ' closing tick for hsc2hs
                #{offset VkSamplerCreateInfo, mipLodBias}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "anisotropyEnable" VkBool32 'False 
                                                            #{offset VkSamplerCreateInfo, anisotropyEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxAnisotropy" ( -- ' closing tick for hsc2hs
                                          #{type float}
                                          ) 'False -- ' closing tick for hsc2hs
                #{offset VkSamplerCreateInfo, maxAnisotropy}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compareEnable" VkBool32 'False 
                                                         #{offset VkSamplerCreateInfo, compareEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compareOp" VkCompareOp 'False 
                                                        #{offset VkSamplerCreateInfo, compareOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minLod" ( -- ' closing tick for hsc2hs
                                   #{type float}
                                   ) 'False  -- ' closing tick for hsc2hs
                                            #{offset VkSamplerCreateInfo, minLod}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxLod" ( -- ' closing tick for hsc2hs
                                   #{type float}
                                   ) 'False  -- ' closing tick for hsc2hs
                                            #{offset VkSamplerCreateInfo, maxLod}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "borderColor" VkBorderColor 'False 
                                                            #{offset VkSamplerCreateInfo, borderColor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "unnormalizedCoordinates" VkBool32 'False 
                                                                   #{offset VkSamplerCreateInfo, unnormalizedCoordinates}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSamplerReductionModeCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSamplerReductionModeEXT reductionMode;
--   > } VkSamplerReductionModeCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerReductionModeCreateInfoEXT VkSamplerReductionModeCreateInfoEXT registry at www.khronos.org>
type VkSamplerReductionModeCreateInfoEXT =
     VkStruct VkSamplerReductionModeCreateInfoEXT' -- ' closing tick for hsc2hs

data VkSamplerReductionModeCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSamplerReductionModeCreateInfoEXT where
    type StructRep VkSamplerReductionModeCreateInfoEXT =
         'StructMeta "VkSamplerReductionModeCreateInfoEXT" -- ' closing tick for hsc2hs
           VkSamplerReductionModeCreateInfoEXT
           #{size VkSamplerReductionModeCreateInfoEXT}
           #{alignment VkSamplerReductionModeCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSamplerReductionModeCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSamplerReductionModeCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "reductionMode" VkSamplerReductionModeEXT 'False
                #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSamplerCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkSamplerYcbcrConversionCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkFormat                         format;
--   >     VkSamplerYcbcrModelConversion ycbcrModel;
--   >     VkSamplerYcbcrRange           ycbcrRange;
--   >     VkComponentMapping               components;
--   >     VkChromaLocation              xChromaOffset;
--   >     VkChromaLocation              yChromaOffset;
--   >     VkFilter                         chromaFilter;
--   >     VkBool32                         forceExplicitReconstruction;
--   > } VkSamplerYcbcrConversionCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerYcbcrConversionCreateInfo VkSamplerYcbcrConversionCreateInfo registry at www.khronos.org>
type VkSamplerYcbcrConversionCreateInfo =
     VkStruct VkSamplerYcbcrConversionCreateInfo' -- ' closing tick for hsc2hs

data VkSamplerYcbcrConversionCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSamplerYcbcrConversionCreateInfo where
    type StructRep VkSamplerYcbcrConversionCreateInfo =
         'StructMeta "VkSamplerYcbcrConversionCreateInfo" -- ' closing tick for hsc2hs
           VkSamplerYcbcrConversionCreateInfo
           #{size VkSamplerYcbcrConversionCreateInfo}
           #{alignment VkSamplerYcbcrConversionCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSamplerYcbcrConversionCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSamplerYcbcrConversionCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkFormat 'False 
                                                  #{offset VkSamplerYcbcrConversionCreateInfo, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ycbcrModel" VkSamplerYcbcrModelConversion 'False
                #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ycbcrRange" VkSamplerYcbcrRange 'False 
                                                                 #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "components" VkComponentMapping 'False 
                                                                #{offset VkSamplerYcbcrConversionCreateInfo, components}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "xChromaOffset" VkChromaLocation 'False 
                                                                 #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "yChromaOffset" VkChromaLocation 'False 
                                                                 #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "chromaFilter" VkFilter 'False 
                                                        #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "forceExplicitReconstruction" VkBool32 'False 
                                                                       #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkSamplerYcbcrConversionCreateInfo`
type VkSamplerYcbcrConversionCreateInfoKHR =
     VkSamplerYcbcrConversionCreateInfo

-- | > typedef struct VkSamplerYcbcrConversionImageFormatProperties {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     uint32_t                         combinedImageSamplerDescriptorCount;
--   > } VkSamplerYcbcrConversionImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerYcbcrConversionImageFormatProperties VkSamplerYcbcrConversionImageFormatProperties registry at www.khronos.org>
type VkSamplerYcbcrConversionImageFormatProperties =
     VkStruct VkSamplerYcbcrConversionImageFormatProperties' -- ' closing tick for hsc2hs

data VkSamplerYcbcrConversionImageFormatProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkSamplerYcbcrConversionImageFormatProperties
         where
    type StructRep VkSamplerYcbcrConversionImageFormatProperties =
         'StructMeta "VkSamplerYcbcrConversionImageFormatProperties" -- ' closing tick for hsc2hs
           VkSamplerYcbcrConversionImageFormatProperties
           #{size VkSamplerYcbcrConversionImageFormatProperties}
           #{alignment VkSamplerYcbcrConversionImageFormatProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSamplerYcbcrConversionImageFormatProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "combinedImageSamplerDescriptorCount" Word32 'False
                #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkImageFormatProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkSamplerYcbcrConversionImageFormatProperties`
type VkSamplerYcbcrConversionImageFormatPropertiesKHR =
     VkSamplerYcbcrConversionImageFormatProperties

-- | > typedef struct VkSamplerYcbcrConversionInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSamplerYcbcrConversion      conversion;
--   > } VkSamplerYcbcrConversionInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerYcbcrConversionInfo VkSamplerYcbcrConversionInfo registry at www.khronos.org>
type VkSamplerYcbcrConversionInfo =
     VkStruct VkSamplerYcbcrConversionInfo' -- ' closing tick for hsc2hs

data VkSamplerYcbcrConversionInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSamplerYcbcrConversionInfo where
    type StructRep VkSamplerYcbcrConversionInfo =
         'StructMeta "VkSamplerYcbcrConversionInfo" -- ' closing tick for hsc2hs
           VkSamplerYcbcrConversionInfo
           #{size VkSamplerYcbcrConversionInfo}
           #{alignment VkSamplerYcbcrConversionInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSamplerYcbcrConversionInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSamplerYcbcrConversionInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "conversion" VkSamplerYcbcrConversion 'False 
                                                                      #{offset VkSamplerYcbcrConversionInfo, conversion}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSamplerCreateInfo, VkImageViewCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkSamplerYcbcrConversionInfo`
type VkSamplerYcbcrConversionInfoKHR = VkSamplerYcbcrConversionInfo
