{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE CPP                      #-}
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
        -- contact: @Daniel Rakos @drakos-amd@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @144@
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.AccessFlags,
        module Graphics.Vulkan.Types.Struct.Attachment,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.Clear,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Struct.Image,
        module Graphics.Vulkan.Types.Struct.MultisamplePropertiesEXT,
        module Graphics.Vulkan.Types.Struct.Offset,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Enum.PhysicalDeviceType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.Pipeline,
        module Graphics.Vulkan.Types.Struct.Rect,
        module Graphics.Vulkan.Types.Struct.RenderPass,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Struct.SampleLocation,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Struct.Subpass,
        -- > #include "vk_platform.h"
        VkCmdSetSampleLocationsEXT, pattern VkCmdSetSampleLocationsEXT,
        HS_vkCmdSetSampleLocationsEXT, PFN_vkCmdSetSampleLocationsEXT,
        VkGetPhysicalDeviceMultisamplePropertiesEXT,
        pattern VkGetPhysicalDeviceMultisamplePropertiesEXT,
        HS_vkGetPhysicalDeviceMultisamplePropertiesEXT,
        PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT,
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
import           GHC.Ptr                                               (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                          (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.AccessFlags
import           Graphics.Vulkan.Types.Enum.DynamicState               (VkDynamicState (..))
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Attachment
import           Graphics.Vulkan.Types.Struct.Clear
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Image
import           Graphics.Vulkan.Types.Struct.MultisamplePropertiesEXT
import           Graphics.Vulkan.Types.Struct.Offset
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.Pipeline
import           Graphics.Vulkan.Types.Struct.Rect
import           Graphics.Vulkan.Types.Struct.RenderPass
import           Graphics.Vulkan.Types.Struct.SampleLocation
import           Graphics.Vulkan.Types.Struct.Subpass

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

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetSampleLocationsEXT
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

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetSampleLocationsEXTUnsafe ::
               PFN_vkCmdSetSampleLocationsEXT -> HS_vkCmdSetSampleLocationsEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetSampleLocationsEXTSafe ::
               PFN_vkCmdSetSampleLocationsEXT -> HS_vkCmdSetSampleLocationsEXT

instance VulkanProc "vkCmdSetSampleLocationsEXT" where
        type VkProcType "vkCmdSetSampleLocationsEXT" =
             HS_vkCmdSetSampleLocationsEXT
        vkProcSymbol = _VkCmdSetSampleLocationsEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCmdSetSampleLocationsEXTUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetSampleLocationsEXTSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- | > void vkGetPhysicalDeviceMultisamplePropertiesEXT
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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceMultisamplePropertiesEXTUnsafe ::
               PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT ->
                 HS_vkGetPhysicalDeviceMultisamplePropertiesEXT

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceMultisamplePropertiesEXTSafe ::
               PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT ->
                 HS_vkGetPhysicalDeviceMultisamplePropertiesEXT

instance VulkanProc "vkGetPhysicalDeviceMultisamplePropertiesEXT"
         where
        type VkProcType "vkGetPhysicalDeviceMultisamplePropertiesEXT" =
             HS_vkGetPhysicalDeviceMultisamplePropertiesEXT
        vkProcSymbol = _VkGetPhysicalDeviceMultisamplePropertiesEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceMultisamplePropertiesEXTUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceMultisamplePropertiesEXTSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
