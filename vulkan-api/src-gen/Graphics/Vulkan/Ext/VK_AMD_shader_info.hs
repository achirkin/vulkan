{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_AMD_shader_info
       (-- * Vulkan extension: @VK_AMD_shader_info@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jaakko Konttinen @jaakko@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @43@
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkShaderInfoTypeAMD,
        module Graphics.Vulkan.Types.Struct.VkShaderResourceUsageAMD,
        module Graphics.Vulkan.Types.Enum.VkShaderStageFlags,
        module Graphics.Vulkan.Types.Struct.VkShaderStatisticsInfoAMD,
        -- > #include "vk_platform.h"
        VkGetShaderInfoAMD, pattern VkGetShaderInfoAMD,
        HS_vkGetShaderInfoAMD, PFN_vkGetShaderInfoAMD,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_AMD_SHADER_INFO_SPEC_VERSION,
        pattern VK_AMD_SHADER_INFO_SPEC_VERSION,
        VK_AMD_SHADER_INFO_EXTENSION_NAME,
        pattern VK_AMD_SHADER_INFO_EXTENSION_NAME)
       where
import           GHC.Ptr                                                (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                           (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkShaderInfoTypeAMD
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkShaderResourceUsageAMD
import           Graphics.Vulkan.Types.Struct.VkShaderStatisticsInfoAMD

pattern VkGetShaderInfoAMD :: CString

pattern VkGetShaderInfoAMD <- (is_VkGetShaderInfoAMD -> True)
  where VkGetShaderInfoAMD = _VkGetShaderInfoAMD

{-# INLINE _VkGetShaderInfoAMD #-}

_VkGetShaderInfoAMD :: CString
_VkGetShaderInfoAMD = Ptr "vkGetShaderInfoAMD\NUL"#

{-# INLINE is_VkGetShaderInfoAMD #-}

is_VkGetShaderInfoAMD :: CString -> Bool
is_VkGetShaderInfoAMD = (EQ ==) . cmpCStrings _VkGetShaderInfoAMD

type VkGetShaderInfoAMD = "vkGetShaderInfoAMD"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_FEATURE_NOT_PRESENT', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetShaderInfoAMD
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , VkShaderStageFlagBits shaderStage
--   >     , VkShaderInfoTypeAMD infoType
--   >     , size_t* pInfoSize
--   >     , void* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetShaderInfoAMD vkGetShaderInfoAMD registry at www.khronos.org>
type HS_vkGetShaderInfoAMD =
     VkDevice -- ^ device
              ->
       VkPipeline -- ^ pipeline
                  ->
         VkShaderStageFlagBits -- ^ shaderStage
                               ->
           VkShaderInfoTypeAMD -- ^ infoType
                               -> Ptr CSize -- ^ pInfoSize
                                            -> Ptr Void -- ^ pInfo
                                                        -> IO VkResult

type PFN_vkGetShaderInfoAMD = FunPtr HS_vkGetShaderInfoAMD

foreign import ccall "dynamic" unwrapVkGetShaderInfoAMD ::
               PFN_vkGetShaderInfoAMD -> HS_vkGetShaderInfoAMD

instance VulkanProc "vkGetShaderInfoAMD" where
        type VkProcType "vkGetShaderInfoAMD" = HS_vkGetShaderInfoAMD
        vkProcSymbol = _VkGetShaderInfoAMD

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetShaderInfoAMD

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_AMD_SHADER_INFO_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_AMD_SHADER_INFO_SPEC_VERSION = 1

type VK_AMD_SHADER_INFO_SPEC_VERSION = 1

pattern VK_AMD_SHADER_INFO_EXTENSION_NAME :: CString

pattern VK_AMD_SHADER_INFO_EXTENSION_NAME <-
        (is_VK_AMD_SHADER_INFO_EXTENSION_NAME -> True)
  where VK_AMD_SHADER_INFO_EXTENSION_NAME
          = _VK_AMD_SHADER_INFO_EXTENSION_NAME

{-# INLINE _VK_AMD_SHADER_INFO_EXTENSION_NAME #-}

_VK_AMD_SHADER_INFO_EXTENSION_NAME :: CString
_VK_AMD_SHADER_INFO_EXTENSION_NAME = Ptr "VK_AMD_shader_info\NUL"#

{-# INLINE is_VK_AMD_SHADER_INFO_EXTENSION_NAME #-}

is_VK_AMD_SHADER_INFO_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_SHADER_INFO_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_AMD_SHADER_INFO_EXTENSION_NAME

type VK_AMD_SHADER_INFO_EXTENSION_NAME = "VK_AMD_shader_info"
