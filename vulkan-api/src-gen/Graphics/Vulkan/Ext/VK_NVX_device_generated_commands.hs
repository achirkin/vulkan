{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NVX_device_generated_commands
       (-- * Vulkan extension: @VK_NVX_device_generated_commands@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Christoph Kubisch @pixeljetstream@
        --
        -- author: @NVX@
        --
        -- type: @device@
        --
        -- Extension number: @87@
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkCmdProcessCommandsInfoNVX,
        module Graphics.Vulkan.Types.Struct.VkCmdReserveSpaceForCommandsInfoNVX,
        module Graphics.Vulkan.Types.Struct.VkDeviceGeneratedCommandsFeaturesNVX,
        module Graphics.Vulkan.Types.Struct.VkDeviceGeneratedCommandsLimitsNVX,
        module Graphics.Vulkan.Types.Enum.VkIndexType,
        module Graphics.Vulkan.Types.Struct.VkIndirectCommandsLayoutCreateInfoNVX,
        module Graphics.Vulkan.Types.Struct.VkIndirectCommandsLayoutTokenNVX,
        module Graphics.Vulkan.Types.Enum.VkIndirectCommandsLayoutUsageFlagsNVX,
        module Graphics.Vulkan.Types.Struct.VkIndirectCommandsTokenNVX,
        module Graphics.Vulkan.Types.Enum.VkIndirectCommandsTokenTypeNVX,
        module Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX,
        module Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX,
        module Graphics.Vulkan.Types.Struct.VkObjectTableCreateInfoNVX,
        module Graphics.Vulkan.Types.Struct.VkObjectTableDescriptorSetEntryNVX,
        module Graphics.Vulkan.Types.Struct.VkObjectTableEntryNVX,
        module Graphics.Vulkan.Types.Struct.VkObjectTableIndexBufferEntryNVX,
        module Graphics.Vulkan.Types.Struct.VkObjectTablePipelineEntryNVX,
        module Graphics.Vulkan.Types.Struct.VkObjectTablePushConstantEntryNVX,
        module Graphics.Vulkan.Types.Struct.VkObjectTableVertexBufferEntryNVX,
        module Graphics.Vulkan.Types.Enum.VkPipelineBindPoint,
        module Graphics.Vulkan.Types.Enum.VkShaderStageFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VkCmdProcessCommandsNVX, pattern VkCmdProcessCommandsNVX,
        HS_vkCmdProcessCommandsNVX, PFN_vkCmdProcessCommandsNVX,
        unwrapVkCmdProcessCommandsNVX, vkCmdProcessCommandsNVX,
        vkCmdProcessCommandsNVXSafe, VkCmdReserveSpaceForCommandsNVX,
        pattern VkCmdReserveSpaceForCommandsNVX,
        HS_vkCmdReserveSpaceForCommandsNVX,
        PFN_vkCmdReserveSpaceForCommandsNVX,
        unwrapVkCmdReserveSpaceForCommandsNVX,
        vkCmdReserveSpaceForCommandsNVX,
        vkCmdReserveSpaceForCommandsNVXSafe,
        VkCreateIndirectCommandsLayoutNVX,
        pattern VkCreateIndirectCommandsLayoutNVX,
        HS_vkCreateIndirectCommandsLayoutNVX,
        PFN_vkCreateIndirectCommandsLayoutNVX,
        unwrapVkCreateIndirectCommandsLayoutNVX,
        vkCreateIndirectCommandsLayoutNVX,
        vkCreateIndirectCommandsLayoutNVXSafe,
        VkDestroyIndirectCommandsLayoutNVX,
        pattern VkDestroyIndirectCommandsLayoutNVX,
        HS_vkDestroyIndirectCommandsLayoutNVX,
        PFN_vkDestroyIndirectCommandsLayoutNVX,
        unwrapVkDestroyIndirectCommandsLayoutNVX,
        vkDestroyIndirectCommandsLayoutNVX,
        vkDestroyIndirectCommandsLayoutNVXSafe, VkCreateObjectTableNVX,
        pattern VkCreateObjectTableNVX, HS_vkCreateObjectTableNVX,
        PFN_vkCreateObjectTableNVX, unwrapVkCreateObjectTableNVX,
        vkCreateObjectTableNVX, vkCreateObjectTableNVXSafe,
        VkDestroyObjectTableNVX, pattern VkDestroyObjectTableNVX,
        HS_vkDestroyObjectTableNVX, PFN_vkDestroyObjectTableNVX,
        unwrapVkDestroyObjectTableNVX, vkDestroyObjectTableNVX,
        vkDestroyObjectTableNVXSafe, VkRegisterObjectsNVX,
        pattern VkRegisterObjectsNVX, HS_vkRegisterObjectsNVX,
        PFN_vkRegisterObjectsNVX, unwrapVkRegisterObjectsNVX,
        vkRegisterObjectsNVX, vkRegisterObjectsNVXSafe,
        VkUnregisterObjectsNVX, pattern VkUnregisterObjectsNVX,
        HS_vkUnregisterObjectsNVX, PFN_vkUnregisterObjectsNVX,
        unwrapVkUnregisterObjectsNVX, vkUnregisterObjectsNVX,
        vkUnregisterObjectsNVXSafe,
        VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX,
        pattern VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX,
        HS_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX,
        PFN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX,
        unwrapVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX,
        vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX,
        vkGetPhysicalDeviceGeneratedCommandsPropertiesNVXSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION,
        pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION,
        VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME,
        pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX,
        pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX,
        pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX,
        pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX,
        pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX,
        pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX,
        pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX,
        pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX,
        pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX)
       where
import           GHC.Ptr
                                                                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.InstanceProc
                                                                                     (VulkanInstanceProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkAccessFlags
                                                                                     (VkAccessBitmask (..),
                                                                                     VkAccessFlagBits)
import           Graphics.Vulkan.Types.Enum.VkIndexType
import           Graphics.Vulkan.Types.Enum.VkIndirectCommandsLayoutUsageFlagsNVX
import           Graphics.Vulkan.Types.Enum.VkIndirectCommandsTokenTypeNVX
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX
import           Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX
import           Graphics.Vulkan.Types.Enum.VkObjectType
                                                                                     (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags
                                                                                     (VkPipelineStageBitmask (..),
                                                                                     VkPipelineStageFlagBits)
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkCmdProcessCommandsInfoNVX
import           Graphics.Vulkan.Types.Struct.VkCmdReserveSpaceForCommandsInfoNVX
import           Graphics.Vulkan.Types.Struct.VkDeviceGeneratedCommandsFeaturesNVX
import           Graphics.Vulkan.Types.Struct.VkDeviceGeneratedCommandsLimitsNVX
import           Graphics.Vulkan.Types.Struct.VkIndirectCommandsLayoutCreateInfoNVX
import           Graphics.Vulkan.Types.Struct.VkIndirectCommandsLayoutTokenNVX
import           Graphics.Vulkan.Types.Struct.VkIndirectCommandsTokenNVX
import           Graphics.Vulkan.Types.Struct.VkObjectTableCreateInfoNVX
import           Graphics.Vulkan.Types.Struct.VkObjectTableDescriptorSetEntryNVX
import           Graphics.Vulkan.Types.Struct.VkObjectTableEntryNVX
import           Graphics.Vulkan.Types.Struct.VkObjectTableIndexBufferEntryNVX
import           Graphics.Vulkan.Types.Struct.VkObjectTablePipelineEntryNVX
import           Graphics.Vulkan.Types.Struct.VkObjectTablePushConstantEntryNVX
import           Graphics.Vulkan.Types.Struct.VkObjectTableVertexBufferEntryNVX

pattern VkCmdProcessCommandsNVX :: CString

pattern VkCmdProcessCommandsNVX <-
        (is_VkCmdProcessCommandsNVX -> True)
  where VkCmdProcessCommandsNVX = _VkCmdProcessCommandsNVX

{-# INLINE _VkCmdProcessCommandsNVX #-}

_VkCmdProcessCommandsNVX :: CString
_VkCmdProcessCommandsNVX = Ptr "vkCmdProcessCommandsNVX\NUL"#

{-# INLINE is_VkCmdProcessCommandsNVX #-}

is_VkCmdProcessCommandsNVX :: CString -> Bool
is_VkCmdProcessCommandsNVX
  = (EQ ==) . cmpCStrings _VkCmdProcessCommandsNVX

type VkCmdProcessCommandsNVX = "vkCmdProcessCommandsNVX"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @inside@
--
--   > () vkCmdProcessCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdProcessCommandsInfoNVX* pProcessCommandsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdProcessCommandsNVX.html vkCmdProcessCommandsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdProcessCommandsNVX"
               vkCmdProcessCommandsNVX ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCmdProcessCommandsInfoNVX -- ^ pProcessCommandsInfo
                                                                  -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @inside@
--
--   > () vkCmdProcessCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdProcessCommandsInfoNVX* pProcessCommandsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdProcessCommandsNVX.html vkCmdProcessCommandsNVX registry at www.khronos.org>
foreign import ccall safe "vkCmdProcessCommandsNVX"
               vkCmdProcessCommandsNVXSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCmdProcessCommandsInfoNVX -- ^ pProcessCommandsInfo
                                                                  -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @inside@
--
--   > () vkCmdProcessCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdProcessCommandsInfoNVX* pProcessCommandsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdProcessCommandsNVX.html vkCmdProcessCommandsNVX registry at www.khronos.org>
type HS_vkCmdProcessCommandsNVX =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkCmdProcessCommandsInfoNVX -- ^ pProcessCommandsInfo
                                                        -> IO ()

type PFN_vkCmdProcessCommandsNVX =
     FunPtr HS_vkCmdProcessCommandsNVX

foreign import ccall "dynamic" unwrapVkCmdProcessCommandsNVX ::
               PFN_vkCmdProcessCommandsNVX -> HS_vkCmdProcessCommandsNVX

instance VulkanInstanceProc "vkCmdProcessCommandsNVX" where
        type VkInstanceProcType "vkCmdProcessCommandsNVX" =
             HS_vkCmdProcessCommandsNVX
        vkInstanceProcSymbol = _VkCmdProcessCommandsNVX

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkCmdProcessCommandsNVX

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkCmdReserveSpaceForCommandsNVX :: CString

pattern VkCmdReserveSpaceForCommandsNVX <-
        (is_VkCmdReserveSpaceForCommandsNVX -> True)
  where VkCmdReserveSpaceForCommandsNVX
          = _VkCmdReserveSpaceForCommandsNVX

{-# INLINE _VkCmdReserveSpaceForCommandsNVX #-}

_VkCmdReserveSpaceForCommandsNVX :: CString
_VkCmdReserveSpaceForCommandsNVX
  = Ptr "vkCmdReserveSpaceForCommandsNVX\NUL"#

{-# INLINE is_VkCmdReserveSpaceForCommandsNVX #-}

is_VkCmdReserveSpaceForCommandsNVX :: CString -> Bool
is_VkCmdReserveSpaceForCommandsNVX
  = (EQ ==) . cmpCStrings _VkCmdReserveSpaceForCommandsNVX

type VkCmdReserveSpaceForCommandsNVX =
     "vkCmdReserveSpaceForCommandsNVX"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @inside@
--
--   > () vkCmdReserveSpaceForCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdReserveSpaceForCommandsInfoNVX* pReserveSpaceInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdReserveSpaceForCommandsNVX.html vkCmdReserveSpaceForCommandsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdReserveSpaceForCommandsNVX"
               vkCmdReserveSpaceForCommandsNVX ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCmdReserveSpaceForCommandsInfoNVX -- ^ pReserveSpaceInfo
                                                                          -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @inside@
--
--   > () vkCmdReserveSpaceForCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdReserveSpaceForCommandsInfoNVX* pReserveSpaceInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdReserveSpaceForCommandsNVX.html vkCmdReserveSpaceForCommandsNVX registry at www.khronos.org>
foreign import ccall safe "vkCmdReserveSpaceForCommandsNVX"
               vkCmdReserveSpaceForCommandsNVXSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCmdReserveSpaceForCommandsInfoNVX -- ^ pReserveSpaceInfo
                                                                          -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @inside@
--
--   > () vkCmdReserveSpaceForCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdReserveSpaceForCommandsInfoNVX* pReserveSpaceInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdReserveSpaceForCommandsNVX.html vkCmdReserveSpaceForCommandsNVX registry at www.khronos.org>
type HS_vkCmdReserveSpaceForCommandsNVX =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkCmdReserveSpaceForCommandsInfoNVX -- ^ pReserveSpaceInfo
                                                                -> IO ()

type PFN_vkCmdReserveSpaceForCommandsNVX =
     FunPtr HS_vkCmdReserveSpaceForCommandsNVX

foreign import ccall "dynamic"
               unwrapVkCmdReserveSpaceForCommandsNVX ::
               PFN_vkCmdReserveSpaceForCommandsNVX ->
                 HS_vkCmdReserveSpaceForCommandsNVX

instance VulkanInstanceProc "vkCmdReserveSpaceForCommandsNVX" where
        type VkInstanceProcType "vkCmdReserveSpaceForCommandsNVX" =
             HS_vkCmdReserveSpaceForCommandsNVX
        vkInstanceProcSymbol = _VkCmdReserveSpaceForCommandsNVX

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkCmdReserveSpaceForCommandsNVX

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkCreateIndirectCommandsLayoutNVX :: CString

pattern VkCreateIndirectCommandsLayoutNVX <-
        (is_VkCreateIndirectCommandsLayoutNVX -> True)
  where VkCreateIndirectCommandsLayoutNVX
          = _VkCreateIndirectCommandsLayoutNVX

{-# INLINE _VkCreateIndirectCommandsLayoutNVX #-}

_VkCreateIndirectCommandsLayoutNVX :: CString
_VkCreateIndirectCommandsLayoutNVX
  = Ptr "vkCreateIndirectCommandsLayoutNVX\NUL"#

{-# INLINE is_VkCreateIndirectCommandsLayoutNVX #-}

is_VkCreateIndirectCommandsLayoutNVX :: CString -> Bool
is_VkCreateIndirectCommandsLayoutNVX
  = (EQ ==) . cmpCStrings _VkCreateIndirectCommandsLayoutNVX

type VkCreateIndirectCommandsLayoutNVX =
     "vkCreateIndirectCommandsLayoutNVX"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , const VkIndirectCommandsLayoutCreateInfoNVX* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkIndirectCommandsLayoutNVX* pIndirectCommandsLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateIndirectCommandsLayoutNVX.html vkCreateIndirectCommandsLayoutNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCreateIndirectCommandsLayoutNVX"
               vkCreateIndirectCommandsLayoutNVX ::
               VkDevice -- ^ device
                        ->
                 Ptr VkIndirectCommandsLayoutCreateInfoNVX -- ^ pCreateInfo
                                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkIndirectCommandsLayoutNVX -- ^ pIndirectCommandsLayout
                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , const VkIndirectCommandsLayoutCreateInfoNVX* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkIndirectCommandsLayoutNVX* pIndirectCommandsLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateIndirectCommandsLayoutNVX.html vkCreateIndirectCommandsLayoutNVX registry at www.khronos.org>
foreign import ccall safe "vkCreateIndirectCommandsLayoutNVX"
               vkCreateIndirectCommandsLayoutNVXSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkIndirectCommandsLayoutCreateInfoNVX -- ^ pCreateInfo
                                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkIndirectCommandsLayoutNVX -- ^ pIndirectCommandsLayout
                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , const VkIndirectCommandsLayoutCreateInfoNVX* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkIndirectCommandsLayoutNVX* pIndirectCommandsLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateIndirectCommandsLayoutNVX.html vkCreateIndirectCommandsLayoutNVX registry at www.khronos.org>
type HS_vkCreateIndirectCommandsLayoutNVX =
     VkDevice -- ^ device
              ->
       Ptr VkIndirectCommandsLayoutCreateInfoNVX -- ^ pCreateInfo
                                                 ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkIndirectCommandsLayoutNVX -- ^ pIndirectCommandsLayout
                                           -> IO VkResult

type PFN_vkCreateIndirectCommandsLayoutNVX =
     FunPtr HS_vkCreateIndirectCommandsLayoutNVX

foreign import ccall "dynamic"
               unwrapVkCreateIndirectCommandsLayoutNVX ::
               PFN_vkCreateIndirectCommandsLayoutNVX ->
                 HS_vkCreateIndirectCommandsLayoutNVX

instance VulkanInstanceProc "vkCreateIndirectCommandsLayoutNVX"
         where
        type VkInstanceProcType "vkCreateIndirectCommandsLayoutNVX" =
             HS_vkCreateIndirectCommandsLayoutNVX
        vkInstanceProcSymbol = _VkCreateIndirectCommandsLayoutNVX

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkCreateIndirectCommandsLayoutNVX

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkDestroyIndirectCommandsLayoutNVX :: CString

pattern VkDestroyIndirectCommandsLayoutNVX <-
        (is_VkDestroyIndirectCommandsLayoutNVX -> True)
  where VkDestroyIndirectCommandsLayoutNVX
          = _VkDestroyIndirectCommandsLayoutNVX

{-# INLINE _VkDestroyIndirectCommandsLayoutNVX #-}

_VkDestroyIndirectCommandsLayoutNVX :: CString
_VkDestroyIndirectCommandsLayoutNVX
  = Ptr "vkDestroyIndirectCommandsLayoutNVX\NUL"#

{-# INLINE is_VkDestroyIndirectCommandsLayoutNVX #-}

is_VkDestroyIndirectCommandsLayoutNVX :: CString -> Bool
is_VkDestroyIndirectCommandsLayoutNVX
  = (EQ ==) . cmpCStrings _VkDestroyIndirectCommandsLayoutNVX

type VkDestroyIndirectCommandsLayoutNVX =
     "vkDestroyIndirectCommandsLayoutNVX"

-- | > () vkDestroyIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , VkIndirectCommandsLayoutNVX indirectCommandsLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyIndirectCommandsLayoutNVX.html vkDestroyIndirectCommandsLayoutNVX registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyIndirectCommandsLayoutNVX"
               vkDestroyIndirectCommandsLayoutNVX ::
               VkDevice -- ^ device
                        ->
                 VkIndirectCommandsLayoutNVX -- ^ indirectCommandsLayout
                                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                          -> IO ()

-- | > () vkDestroyIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , VkIndirectCommandsLayoutNVX indirectCommandsLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyIndirectCommandsLayoutNVX.html vkDestroyIndirectCommandsLayoutNVX registry at www.khronos.org>
foreign import ccall safe "vkDestroyIndirectCommandsLayoutNVX"
               vkDestroyIndirectCommandsLayoutNVXSafe ::
               VkDevice -- ^ device
                        ->
                 VkIndirectCommandsLayoutNVX -- ^ indirectCommandsLayout
                                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                          -> IO ()

-- | > () vkDestroyIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , VkIndirectCommandsLayoutNVX indirectCommandsLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyIndirectCommandsLayoutNVX.html vkDestroyIndirectCommandsLayoutNVX registry at www.khronos.org>
type HS_vkDestroyIndirectCommandsLayoutNVX =
     VkDevice -- ^ device
              ->
       VkIndirectCommandsLayoutNVX -- ^ indirectCommandsLayout
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

type PFN_vkDestroyIndirectCommandsLayoutNVX =
     FunPtr HS_vkDestroyIndirectCommandsLayoutNVX

foreign import ccall "dynamic"
               unwrapVkDestroyIndirectCommandsLayoutNVX ::
               PFN_vkDestroyIndirectCommandsLayoutNVX ->
                 HS_vkDestroyIndirectCommandsLayoutNVX

instance VulkanInstanceProc "vkDestroyIndirectCommandsLayoutNVX"
         where
        type VkInstanceProcType "vkDestroyIndirectCommandsLayoutNVX" =
             HS_vkDestroyIndirectCommandsLayoutNVX
        vkInstanceProcSymbol = _VkDestroyIndirectCommandsLayoutNVX

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkDestroyIndirectCommandsLayoutNVX

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkCreateObjectTableNVX :: CString

pattern VkCreateObjectTableNVX <-
        (is_VkCreateObjectTableNVX -> True)
  where VkCreateObjectTableNVX = _VkCreateObjectTableNVX

{-# INLINE _VkCreateObjectTableNVX #-}

_VkCreateObjectTableNVX :: CString
_VkCreateObjectTableNVX = Ptr "vkCreateObjectTableNVX\NUL"#

{-# INLINE is_VkCreateObjectTableNVX #-}

is_VkCreateObjectTableNVX :: CString -> Bool
is_VkCreateObjectTableNVX
  = (EQ ==) . cmpCStrings _VkCreateObjectTableNVX

type VkCreateObjectTableNVX = "vkCreateObjectTableNVX"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateObjectTableNVX
--   >     ( VkDevice device
--   >     , const VkObjectTableCreateInfoNVX* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkObjectTableNVX* pObjectTable
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateObjectTableNVX.html vkCreateObjectTableNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCreateObjectTableNVX"
               vkCreateObjectTableNVX ::
               VkDevice -- ^ device
                        ->
                 Ptr VkObjectTableCreateInfoNVX -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkObjectTableNVX -- ^ pObjectTable
                                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateObjectTableNVX
--   >     ( VkDevice device
--   >     , const VkObjectTableCreateInfoNVX* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkObjectTableNVX* pObjectTable
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateObjectTableNVX.html vkCreateObjectTableNVX registry at www.khronos.org>
foreign import ccall safe "vkCreateObjectTableNVX"
               vkCreateObjectTableNVXSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkObjectTableCreateInfoNVX -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkObjectTableNVX -- ^ pObjectTable
                                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateObjectTableNVX
--   >     ( VkDevice device
--   >     , const VkObjectTableCreateInfoNVX* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkObjectTableNVX* pObjectTable
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateObjectTableNVX.html vkCreateObjectTableNVX registry at www.khronos.org>
type HS_vkCreateObjectTableNVX =
     VkDevice -- ^ device
              ->
       Ptr VkObjectTableCreateInfoNVX -- ^ pCreateInfo
                                      ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkObjectTableNVX -- ^ pObjectTable
                                                           -> IO VkResult

type PFN_vkCreateObjectTableNVX = FunPtr HS_vkCreateObjectTableNVX

foreign import ccall "dynamic" unwrapVkCreateObjectTableNVX ::
               PFN_vkCreateObjectTableNVX -> HS_vkCreateObjectTableNVX

instance VulkanInstanceProc "vkCreateObjectTableNVX" where
        type VkInstanceProcType "vkCreateObjectTableNVX" =
             HS_vkCreateObjectTableNVX
        vkInstanceProcSymbol = _VkCreateObjectTableNVX

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkCreateObjectTableNVX

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkDestroyObjectTableNVX :: CString

pattern VkDestroyObjectTableNVX <-
        (is_VkDestroyObjectTableNVX -> True)
  where VkDestroyObjectTableNVX = _VkDestroyObjectTableNVX

{-# INLINE _VkDestroyObjectTableNVX #-}

_VkDestroyObjectTableNVX :: CString
_VkDestroyObjectTableNVX = Ptr "vkDestroyObjectTableNVX\NUL"#

{-# INLINE is_VkDestroyObjectTableNVX #-}

is_VkDestroyObjectTableNVX :: CString -> Bool
is_VkDestroyObjectTableNVX
  = (EQ ==) . cmpCStrings _VkDestroyObjectTableNVX

type VkDestroyObjectTableNVX = "vkDestroyObjectTableNVX"

-- | > () vkDestroyObjectTableNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyObjectTableNVX.html vkDestroyObjectTableNVX registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyObjectTableNVX"
               vkDestroyObjectTableNVX ::
               VkDevice -- ^ device
                        -> VkObjectTableNVX -- ^ objectTable
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | > () vkDestroyObjectTableNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyObjectTableNVX.html vkDestroyObjectTableNVX registry at www.khronos.org>
foreign import ccall safe "vkDestroyObjectTableNVX"
               vkDestroyObjectTableNVXSafe ::
               VkDevice -- ^ device
                        -> VkObjectTableNVX -- ^ objectTable
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | > () vkDestroyObjectTableNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyObjectTableNVX.html vkDestroyObjectTableNVX registry at www.khronos.org>
type HS_vkDestroyObjectTableNVX =
     VkDevice -- ^ device
              -> VkObjectTableNVX -- ^ objectTable
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyObjectTableNVX =
     FunPtr HS_vkDestroyObjectTableNVX

foreign import ccall "dynamic" unwrapVkDestroyObjectTableNVX ::
               PFN_vkDestroyObjectTableNVX -> HS_vkDestroyObjectTableNVX

instance VulkanInstanceProc "vkDestroyObjectTableNVX" where
        type VkInstanceProcType "vkDestroyObjectTableNVX" =
             HS_vkDestroyObjectTableNVX
        vkInstanceProcSymbol = _VkDestroyObjectTableNVX

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkDestroyObjectTableNVX

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkRegisterObjectsNVX :: CString

pattern VkRegisterObjectsNVX <- (is_VkRegisterObjectsNVX -> True)
  where VkRegisterObjectsNVX = _VkRegisterObjectsNVX

{-# INLINE _VkRegisterObjectsNVX #-}

_VkRegisterObjectsNVX :: CString
_VkRegisterObjectsNVX = Ptr "vkRegisterObjectsNVX\NUL"#

{-# INLINE is_VkRegisterObjectsNVX #-}

is_VkRegisterObjectsNVX :: CString -> Bool
is_VkRegisterObjectsNVX
  = (EQ ==) . cmpCStrings _VkRegisterObjectsNVX

type VkRegisterObjectsNVX = "vkRegisterObjectsNVX"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkRegisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectTableEntryNVX* const*    ppObjectTableEntries
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkRegisterObjectsNVX.html vkRegisterObjectsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkRegisterObjectsNVX"
               vkRegisterObjectsNVX ::
               VkDevice -- ^ device
                        ->
                 VkObjectTableNVX -- ^ objectTable
                                  ->
                   Word32 -- ^ objectCount
                          ->
                     Ptr (Ptr VkObjectTableEntryNVX) -- ^ ppObjectTableEntries
                                                     -> Ptr Word32 -- ^ pObjectIndices
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkRegisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectTableEntryNVX* const*    ppObjectTableEntries
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkRegisterObjectsNVX.html vkRegisterObjectsNVX registry at www.khronos.org>
foreign import ccall safe "vkRegisterObjectsNVX"
               vkRegisterObjectsNVXSafe ::
               VkDevice -- ^ device
                        ->
                 VkObjectTableNVX -- ^ objectTable
                                  ->
                   Word32 -- ^ objectCount
                          ->
                     Ptr (Ptr VkObjectTableEntryNVX) -- ^ ppObjectTableEntries
                                                     -> Ptr Word32 -- ^ pObjectIndices
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkRegisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectTableEntryNVX* const*    ppObjectTableEntries
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkRegisterObjectsNVX.html vkRegisterObjectsNVX registry at www.khronos.org>
type HS_vkRegisterObjectsNVX =
     VkDevice -- ^ device
              ->
       VkObjectTableNVX -- ^ objectTable
                        ->
         Word32 -- ^ objectCount
                ->
           Ptr (Ptr VkObjectTableEntryNVX) -- ^ ppObjectTableEntries
                                           -> Ptr Word32 -- ^ pObjectIndices
                                                         -> IO VkResult

type PFN_vkRegisterObjectsNVX = FunPtr HS_vkRegisterObjectsNVX

foreign import ccall "dynamic" unwrapVkRegisterObjectsNVX ::
               PFN_vkRegisterObjectsNVX -> HS_vkRegisterObjectsNVX

instance VulkanInstanceProc "vkRegisterObjectsNVX" where
        type VkInstanceProcType "vkRegisterObjectsNVX" =
             HS_vkRegisterObjectsNVX
        vkInstanceProcSymbol = _VkRegisterObjectsNVX

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkRegisterObjectsNVX

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkUnregisterObjectsNVX :: CString

pattern VkUnregisterObjectsNVX <-
        (is_VkUnregisterObjectsNVX -> True)
  where VkUnregisterObjectsNVX = _VkUnregisterObjectsNVX

{-# INLINE _VkUnregisterObjectsNVX #-}

_VkUnregisterObjectsNVX :: CString
_VkUnregisterObjectsNVX = Ptr "vkUnregisterObjectsNVX\NUL"#

{-# INLINE is_VkUnregisterObjectsNVX #-}

is_VkUnregisterObjectsNVX :: CString -> Bool
is_VkUnregisterObjectsNVX
  = (EQ ==) . cmpCStrings _VkUnregisterObjectsNVX

type VkUnregisterObjectsNVX = "vkUnregisterObjectsNVX"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkUnregisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectEntryTypeNVX* pObjectEntryTypes
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUnregisterObjectsNVX.html vkUnregisterObjectsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkUnregisterObjectsNVX"
               vkUnregisterObjectsNVX ::
               VkDevice -- ^ device
                        ->
                 VkObjectTableNVX -- ^ objectTable
                                  ->
                   Word32 -- ^ objectCount
                          -> Ptr VkObjectEntryTypeNVX -- ^ pObjectEntryTypes
                                                      -> Ptr Word32 -- ^ pObjectIndices
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkUnregisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectEntryTypeNVX* pObjectEntryTypes
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUnregisterObjectsNVX.html vkUnregisterObjectsNVX registry at www.khronos.org>
foreign import ccall safe "vkUnregisterObjectsNVX"
               vkUnregisterObjectsNVXSafe ::
               VkDevice -- ^ device
                        ->
                 VkObjectTableNVX -- ^ objectTable
                                  ->
                   Word32 -- ^ objectCount
                          -> Ptr VkObjectEntryTypeNVX -- ^ pObjectEntryTypes
                                                      -> Ptr Word32 -- ^ pObjectIndices
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkUnregisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectEntryTypeNVX* pObjectEntryTypes
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUnregisterObjectsNVX.html vkUnregisterObjectsNVX registry at www.khronos.org>
type HS_vkUnregisterObjectsNVX =
     VkDevice -- ^ device
              ->
       VkObjectTableNVX -- ^ objectTable
                        ->
         Word32 -- ^ objectCount
                -> Ptr VkObjectEntryTypeNVX -- ^ pObjectEntryTypes
                                            -> Ptr Word32 -- ^ pObjectIndices
                                                          -> IO VkResult

type PFN_vkUnregisterObjectsNVX = FunPtr HS_vkUnregisterObjectsNVX

foreign import ccall "dynamic" unwrapVkUnregisterObjectsNVX ::
               PFN_vkUnregisterObjectsNVX -> HS_vkUnregisterObjectsNVX

instance VulkanInstanceProc "vkUnregisterObjectsNVX" where
        type VkInstanceProcType "vkUnregisterObjectsNVX" =
             HS_vkUnregisterObjectsNVX
        vkInstanceProcSymbol = _VkUnregisterObjectsNVX

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkUnregisterObjectsNVX

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX ::
        CString

pattern VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX <-
        (is_VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX -> True)
  where VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
          = _VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX

{-# INLINE _VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX #-}

_VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX :: CString
_VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  = Ptr "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX\NUL"#

{-# INLINE is_VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX #-}

is_VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX ::
                                                     CString -> Bool
is_VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX

type VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX =
     "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX"

-- | > () vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDeviceGeneratedCommandsFeaturesNVX* pFeatures
--   >     , VkDeviceGeneratedCommandsLimitsNVX* pLimits
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.html vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX"
               vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkDeviceGeneratedCommandsFeaturesNVX -- ^ pFeatures
                                                          ->
                   Ptr VkDeviceGeneratedCommandsLimitsNVX -- ^ pLimits
                                                          -> IO ()

-- | > () vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDeviceGeneratedCommandsFeaturesNVX* pFeatures
--   >     , VkDeviceGeneratedCommandsLimitsNVX* pLimits
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.html vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX"
               vkGetPhysicalDeviceGeneratedCommandsPropertiesNVXSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkDeviceGeneratedCommandsFeaturesNVX -- ^ pFeatures
                                                          ->
                   Ptr VkDeviceGeneratedCommandsLimitsNVX -- ^ pLimits
                                                          -> IO ()

-- | > () vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDeviceGeneratedCommandsFeaturesNVX* pFeatures
--   >     , VkDeviceGeneratedCommandsLimitsNVX* pLimits
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.html vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX registry at www.khronos.org>
type HS_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkDeviceGeneratedCommandsFeaturesNVX -- ^ pFeatures
                                                ->
         Ptr VkDeviceGeneratedCommandsLimitsNVX -- ^ pLimits
                                                -> IO ()

type PFN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX =
     FunPtr HS_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX ::
               PFN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX ->
                 HS_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX

instance VulkanInstanceProc
           "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX"
         where
        type VkInstanceProcType
               "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX"
             = HS_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
        vkInstanceProcSymbol
          = _VkGetPhysicalDeviceGeneratedCommandsPropertiesNVX

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc
          = unwrapVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX

        {-# INLINE unwrapVkInstanceProc #-}

pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

type VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: CString

pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME <-
        (is_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME -> True)
  where VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
          = _VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME

{-# INLINE _VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME #-}

_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: CString
_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  = Ptr "VK_NVX_device_generated_commands\NUL"#

{-# INLINE is_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME #-}

is_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME ::
                                                   CString -> Bool
is_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME

type VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME =
     "VK_NVX_device_generated_commands"

pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX =
        VkStructureType 1000086000

pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
        = VkStructureType 1000086001

pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX =
        VkStructureType 1000086002

pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX =
        VkStructureType 1000086003

pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX =
        VkStructureType 1000086004

pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX =
        VkStructureType 1000086005

-- | bitpos = @17@
pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX =
        VkPipelineStageFlagBits 131072

-- | bitpos = @17@
pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX :: VkAccessFlagBits

pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX =
        VkAccessFlagBits 131072

-- | bitpos = @18@
pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX :: VkAccessFlagBits

pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX =
        VkAccessFlagBits 262144

-- | VkobjectTableNVX
pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX :: VkObjectType

pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX = VkObjectType 1000086000

-- | VkIndirectCommandsLayoutNVX
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX :: VkObjectType

pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX =
        VkObjectType 1000086001
