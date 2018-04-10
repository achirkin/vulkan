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
module Graphics.Vulkan.Ext.VK_EXT_sample_locations
       (-- * Vulkan extension: @VK_EXT_sample_locations@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @aqnuep@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @144@
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkAccessFlags,
        module Graphics.Vulkan.Types.Struct.VkAttachmentSampleLocationsEXT,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkClearColorValue,
        module Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue,
        module Graphics.Vulkan.Types.Struct.VkClearValue,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Enum.VkImageAspectFlags,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Struct.VkImageMemoryBarrier,
        module Graphics.Vulkan.Types.Struct.VkImageSubresourceRange,
        module Graphics.Vulkan.Types.Struct.VkMultisamplePropertiesEXT,
        module Graphics.Vulkan.Types.Struct.VkOffset2D,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSampleLocationsPropertiesEXT,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineSampleLocationsStateCreateInfoEXT,
        module Graphics.Vulkan.Types.Struct.VkRect2D,
        module Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo,
        module Graphics.Vulkan.Types.Struct.VkRenderPassSampleLocationsBeginInfoEXT,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Struct.VkSampleLocationEXT,
        module Graphics.Vulkan.Types.Struct.VkSampleLocationsInfoEXT,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkSubpassSampleLocationsEXT,
        -- > #include "vk_platform.h"
        VkCmdSetSampleLocationsEXT, pattern VkCmdSetSampleLocationsEXT,
        HS_vkCmdSetSampleLocationsEXT, PFN_vkCmdSetSampleLocationsEXT,
        unwrapVkCmdSetSampleLocationsEXT,
        VkGetPhysicalDeviceMultisamplePropertiesEXT,
        pattern VkGetPhysicalDeviceMultisamplePropertiesEXT,
        HS_vkGetPhysicalDeviceMultisamplePropertiesEXT,
        PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT,
        unwrapVkGetPhysicalDeviceMultisamplePropertiesEXT,
        module Graphics.Vulkan.Types.Handles,
        VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION,
        pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION,
        VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME,
        pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME,
        pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT,
        pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT,
        pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT)
       where
import           GHC.Ptr
                                                                                            (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
                                                                                            (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkAccessFlags
import           Graphics.Vulkan.Types.Enum.VkDynamicState
                                                                                            (VkDynamicState (..))
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
                                                                                            (VkImageCreateBitmask (..),
                                                                                            VkImageCreateFlagBits)
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAttachmentSampleLocationsEXT
import           Graphics.Vulkan.Types.Struct.VkClearColorValue
import           Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue
import           Graphics.Vulkan.Types.Struct.VkClearValue
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkImageMemoryBarrier
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceRange
import           Graphics.Vulkan.Types.Struct.VkMultisamplePropertiesEXT
import           Graphics.Vulkan.Types.Struct.VkOffset2D
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSampleLocationsPropertiesEXT
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
import           Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineSampleLocationsStateCreateInfoEXT
import           Graphics.Vulkan.Types.Struct.VkRect2D
import           Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo
import           Graphics.Vulkan.Types.Struct.VkRenderPassSampleLocationsBeginInfoEXT
import           Graphics.Vulkan.Types.Struct.VkSampleLocationEXT
import           Graphics.Vulkan.Types.Struct.VkSampleLocationsInfoEXT
import           Graphics.Vulkan.Types.Struct.VkSubpassSampleLocationsEXT

pattern VkCmdSetSampleLocationsEXT :: CString

pattern VkCmdSetSampleLocationsEXT <-
        (is_VkCmdSetSampleLocationsEXT -> True)
  where VkCmdSetSampleLocationsEXT = _VkCmdSetSampleLocationsEXT

{-# INLINE _VkCmdSetSampleLocationsEXT #-}

_VkCmdSetSampleLocationsEXT :: CString
_VkCmdSetSampleLocationsEXT = Ptr "vkCmdSetSampleLocationsEXT\NUL"#

{-# INLINE is_VkCmdSetSampleLocationsEXT #-}

is_VkCmdSetSampleLocationsEXT :: CString -> Bool
is_VkCmdSetSampleLocationsEXT
  = (EQ ==) . cmpCStrings _VkCmdSetSampleLocationsEXT

type VkCmdSetSampleLocationsEXT = "vkCmdSetSampleLocationsEXT"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetSampleLocationsEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkSampleLocationsInfoEXT* pSampleLocationsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetSampleLocationsEXT vkCmdSetSampleLocationsEXT registry at www.khronos.org>
type HS_vkCmdSetSampleLocationsEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkSampleLocationsInfoEXT -- ^ pSampleLocationsInfo
                                                     -> IO ()

type PFN_vkCmdSetSampleLocationsEXT =
     FunPtr HS_vkCmdSetSampleLocationsEXT

foreign import ccall "dynamic" unwrapVkCmdSetSampleLocationsEXT ::
               PFN_vkCmdSetSampleLocationsEXT -> HS_vkCmdSetSampleLocationsEXT

instance VulkanProc "vkCmdSetSampleLocationsEXT" where
        type VkProcType "vkCmdSetSampleLocationsEXT" =
             HS_vkCmdSetSampleLocationsEXT
        vkProcSymbol = _VkCmdSetSampleLocationsEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetSampleLocationsEXT

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceMultisamplePropertiesEXT :: CString

pattern VkGetPhysicalDeviceMultisamplePropertiesEXT <-
        (is_VkGetPhysicalDeviceMultisamplePropertiesEXT -> True)
  where VkGetPhysicalDeviceMultisamplePropertiesEXT
          = _VkGetPhysicalDeviceMultisamplePropertiesEXT

{-# INLINE _VkGetPhysicalDeviceMultisamplePropertiesEXT #-}

_VkGetPhysicalDeviceMultisamplePropertiesEXT :: CString
_VkGetPhysicalDeviceMultisamplePropertiesEXT
  = Ptr "vkGetPhysicalDeviceMultisamplePropertiesEXT\NUL"#

{-# INLINE is_VkGetPhysicalDeviceMultisamplePropertiesEXT #-}

is_VkGetPhysicalDeviceMultisamplePropertiesEXT :: CString -> Bool
is_VkGetPhysicalDeviceMultisamplePropertiesEXT
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceMultisamplePropertiesEXT

type VkGetPhysicalDeviceMultisamplePropertiesEXT =
     "vkGetPhysicalDeviceMultisamplePropertiesEXT"

-- | > () vkGetPhysicalDeviceMultisamplePropertiesEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSampleCountFlagBits samples
--   >     , VkMultisamplePropertiesEXT* pMultisampleProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMultisamplePropertiesEXT vkGetPhysicalDeviceMultisamplePropertiesEXT registry at www.khronos.org>
type HS_vkGetPhysicalDeviceMultisamplePropertiesEXT =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkSampleCountFlagBits -- ^ samples
                             -> Ptr VkMultisamplePropertiesEXT -- ^ pMultisampleProperties
                                                               -> IO ()

type PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT =
     FunPtr HS_vkGetPhysicalDeviceMultisamplePropertiesEXT

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceMultisamplePropertiesEXT ::
               PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT ->
                 HS_vkGetPhysicalDeviceMultisamplePropertiesEXT

instance VulkanProc "vkGetPhysicalDeviceMultisamplePropertiesEXT"
         where
        type VkProcType "vkGetPhysicalDeviceMultisamplePropertiesEXT" =
             HS_vkGetPhysicalDeviceMultisamplePropertiesEXT
        vkProcSymbol = _VkGetPhysicalDeviceMultisamplePropertiesEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceMultisamplePropertiesEXT

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1

type VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1

pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString

pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME <-
        (is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME -> True)
  where VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
          = _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME

{-# INLINE _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME #-}

_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString
_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  = Ptr "VK_EXT_sample_locations\NUL"#

{-# INLINE is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME #-}

is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME

type VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME =
     "VK_EXT_sample_locations"

-- | bitpos = @12@
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
        :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT =
        VkImageCreateFlagBits 4096

pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT =
        VkStructureType 1000143000

pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
        = VkStructureType 1000143001

pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
        = VkStructureType 1000143002

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
        = VkStructureType 1000143003

pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT =
        VkStructureType 1000143004

pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT =
        VkDynamicState 1000143000
