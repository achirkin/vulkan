{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
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
        vkCmdProcessCommandsNVX, vkCmdProcessCommandsNVXSafe,
        vkCmdReserveSpaceForCommandsNVX,
        vkCmdReserveSpaceForCommandsNVXSafe,
        vkCreateIndirectCommandsLayoutNVX,
        vkCreateIndirectCommandsLayoutNVXSafe,
        vkDestroyIndirectCommandsLayoutNVX,
        vkDestroyIndirectCommandsLayoutNVXSafe, vkCreateObjectTableNVX,
        vkCreateObjectTableNVXSafe, vkDestroyObjectTableNVX,
        vkDestroyObjectTableNVXSafe, vkRegisterObjectsNVX,
        vkRegisterObjectsNVXSafe, vkUnregisterObjectsNVX,
        vkUnregisterObjectsNVXSafe,
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

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @inside@
--
--   > () vkCmdProcessCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdProcessCommandsInfoNVX* pProcessCommandsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCmdProcessCommandsNVX.html vkCmdProcessCommandsNVX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCmdProcessCommandsNVX.html vkCmdProcessCommandsNVX registry at www.khronos.org>
foreign import ccall safe "vkCmdProcessCommandsNVX"
               vkCmdProcessCommandsNVXSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCmdProcessCommandsInfoNVX -- ^ pProcessCommandsInfo
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCmdReserveSpaceForCommandsNVX.html vkCmdReserveSpaceForCommandsNVX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCmdReserveSpaceForCommandsNVX.html vkCmdReserveSpaceForCommandsNVX registry at www.khronos.org>
foreign import ccall safe "vkCmdReserveSpaceForCommandsNVX"
               vkCmdReserveSpaceForCommandsNVXSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCmdReserveSpaceForCommandsInfoNVX -- ^ pReserveSpaceInfo
                                                                          -> IO ()

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateIndirectCommandsLayoutNVX.html vkCreateIndirectCommandsLayoutNVX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateIndirectCommandsLayoutNVX.html vkCreateIndirectCommandsLayoutNVX registry at www.khronos.org>
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

-- | > () vkDestroyIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , VkIndirectCommandsLayoutNVX indirectCommandsLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDestroyIndirectCommandsLayoutNVX.html vkDestroyIndirectCommandsLayoutNVX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDestroyIndirectCommandsLayoutNVX.html vkDestroyIndirectCommandsLayoutNVX registry at www.khronos.org>
foreign import ccall safe "vkDestroyIndirectCommandsLayoutNVX"
               vkDestroyIndirectCommandsLayoutNVXSafe ::
               VkDevice -- ^ device
                        ->
                 VkIndirectCommandsLayoutNVX -- ^ indirectCommandsLayout
                                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                          -> IO ()

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateObjectTableNVX.html vkCreateObjectTableNVX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateObjectTableNVX.html vkCreateObjectTableNVX registry at www.khronos.org>
foreign import ccall safe "vkCreateObjectTableNVX"
               vkCreateObjectTableNVXSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkObjectTableCreateInfoNVX -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkObjectTableNVX -- ^ pObjectTable
                                                                     -> IO VkResult

-- | > () vkDestroyObjectTableNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDestroyObjectTableNVX.html vkDestroyObjectTableNVX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDestroyObjectTableNVX.html vkDestroyObjectTableNVX registry at www.khronos.org>
foreign import ccall safe "vkDestroyObjectTableNVX"
               vkDestroyObjectTableNVXSafe ::
               VkDevice -- ^ device
                        -> VkObjectTableNVX -- ^ objectTable
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkRegisterObjectsNVX.html vkRegisterObjectsNVX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkRegisterObjectsNVX.html vkRegisterObjectsNVX registry at www.khronos.org>
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
--   > VkResult vkUnregisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectEntryTypeNVX* pObjectEntryTypes
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkUnregisterObjectsNVX.html vkUnregisterObjectsNVX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkUnregisterObjectsNVX.html vkUnregisterObjectsNVX registry at www.khronos.org>
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

-- | > () vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDeviceGeneratedCommandsFeaturesNVX* pFeatures
--   >     , VkDeviceGeneratedCommandsLimitsNVX* pLimits
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.html vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.html vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX"
               vkGetPhysicalDeviceGeneratedCommandsPropertiesNVXSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkDeviceGeneratedCommandsFeaturesNVX -- ^ pFeatures
                                                          ->
                   Ptr VkDeviceGeneratedCommandsLimitsNVX -- ^ pLimits
                                                          -> IO ()

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
