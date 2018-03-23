{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Strict         #-}
module Graphics.Vulkan.Types.Handles
       (VkBuffer, VkBuffer_T(), VkBufferView, VkBufferView_T(),
        VkCommandBuffer, VkCommandBuffer_T(), VkCommandPool,
        VkCommandPool_T(), VkDebugReportCallbackEXT,
        VkDebugReportCallbackEXT_T(), VkDebugUtilsMessengerEXT,
        VkDebugUtilsMessengerEXT_T(), VkDescriptorPool,
        VkDescriptorPool_T(), VkDescriptorSet, VkDescriptorSet_T(),
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorUpdateTemplate, VkDescriptorUpdateTemplate_T(),
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDevice, VkDevice_T(), VkDeviceMemory, VkDeviceMemory_T(),
        VkDisplayKHR, VkDisplayKHR_T(), VkDisplayModeKHR,
        VkDisplayModeKHR_T(), VkEvent, VkEvent_T(), VkFence, VkFence_T(),
        VkFramebuffer, VkFramebuffer_T(), VkImage, VkImage_T(),
        VkImageView, VkImageView_T(), VkIndirectCommandsLayoutNVX,
        VkIndirectCommandsLayoutNVX_T(), VkInstance, VkInstance_T(),
        VkObjectTableNVX, VkObjectTableNVX_T(), VkPhysicalDevice,
        VkPhysicalDevice_T(), VkPipeline, VkPipeline_T(), VkPipelineCache,
        VkPipelineCache_T(), VkPipelineLayout, VkPipelineLayout_T(),
        VkQueryPool, VkQueryPool_T(), VkQueue, VkQueue_T(), VkRenderPass,
        VkRenderPass_T(), VkSampler, VkSampler_T(),
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversion_T(),
        VkSamplerYcbcrConversionKHR, VkSamplerYcbcrConversionKHR_T(),
        VkSemaphore, VkSemaphore_T(), VkShaderModule, VkShaderModule_T(),
        VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR, VkSwapchainKHR_T(),
        VkValidationCacheEXT, VkValidationCacheEXT_T())
       where
import           Graphics.Vulkan.Marshal (Ptr, VkPtr)

type VkBuffer = VkPtr VkBuffer_T

-- | Opaque data type referenced by VkBuffer
data VkBuffer_T

type VkBufferView = VkPtr VkBufferView_T

-- | Opaque data type referenced by VkBufferView
data VkBufferView_T

type VkCommandBuffer = Ptr VkCommandBuffer_T

-- | Opaque data type referenced by VkCommandBuffer
data VkCommandBuffer_T

type VkCommandPool = VkPtr VkCommandPool_T

-- | Opaque data type referenced by VkCommandPool
data VkCommandPool_T

type VkDebugReportCallbackEXT = VkPtr VkDebugReportCallbackEXT_T

-- | Opaque data type referenced by VkDebugReportCallbackEXT
data VkDebugReportCallbackEXT_T

type VkDebugUtilsMessengerEXT = VkPtr VkDebugUtilsMessengerEXT_T

-- | Opaque data type referenced by VkDebugUtilsMessengerEXT
data VkDebugUtilsMessengerEXT_T

type VkDescriptorPool = VkPtr VkDescriptorPool_T

-- | Opaque data type referenced by VkDescriptorPool
data VkDescriptorPool_T

type VkDescriptorSet = VkPtr VkDescriptorSet_T

-- | Opaque data type referenced by VkDescriptorSet
data VkDescriptorSet_T

type VkDescriptorSetLayout = VkPtr VkDescriptorSetLayout_T

-- | Opaque data type referenced by VkDescriptorSetLayout
data VkDescriptorSetLayout_T

type VkDescriptorUpdateTemplate =
     VkPtr VkDescriptorUpdateTemplate_T

-- | Opaque data type referenced by VkDescriptorUpdateTemplate
data VkDescriptorUpdateTemplate_T

type VkDescriptorUpdateTemplateKHR =
     VkPtr VkDescriptorUpdateTemplateKHR_T

-- | Opaque data type referenced by VkDescriptorUpdateTemplateKHR
data VkDescriptorUpdateTemplateKHR_T

type VkDevice = Ptr VkDevice_T

-- | Opaque data type referenced by VkDevice
data VkDevice_T

type VkDeviceMemory = VkPtr VkDeviceMemory_T

-- | Opaque data type referenced by VkDeviceMemory
data VkDeviceMemory_T

type VkDisplayKHR = VkPtr VkDisplayKHR_T

-- | Opaque data type referenced by VkDisplayKHR
data VkDisplayKHR_T

type VkDisplayModeKHR = VkPtr VkDisplayModeKHR_T

-- | Opaque data type referenced by VkDisplayModeKHR
data VkDisplayModeKHR_T

type VkEvent = VkPtr VkEvent_T

-- | Opaque data type referenced by VkEvent
data VkEvent_T

type VkFence = VkPtr VkFence_T

-- | Opaque data type referenced by VkFence
data VkFence_T

type VkFramebuffer = VkPtr VkFramebuffer_T

-- | Opaque data type referenced by VkFramebuffer
data VkFramebuffer_T

type VkImage = VkPtr VkImage_T

-- | Opaque data type referenced by VkImage
data VkImage_T

type VkImageView = VkPtr VkImageView_T

-- | Opaque data type referenced by VkImageView
data VkImageView_T

type VkIndirectCommandsLayoutNVX =
     VkPtr VkIndirectCommandsLayoutNVX_T

-- | Opaque data type referenced by VkIndirectCommandsLayoutNVX
data VkIndirectCommandsLayoutNVX_T

type VkInstance = Ptr VkInstance_T

-- | Opaque data type referenced by VkInstance
data VkInstance_T

type VkObjectTableNVX = VkPtr VkObjectTableNVX_T

-- | Opaque data type referenced by VkObjectTableNVX
data VkObjectTableNVX_T

type VkPhysicalDevice = Ptr VkPhysicalDevice_T

-- | Opaque data type referenced by VkPhysicalDevice
data VkPhysicalDevice_T

type VkPipeline = VkPtr VkPipeline_T

-- | Opaque data type referenced by VkPipeline
data VkPipeline_T

type VkPipelineCache = VkPtr VkPipelineCache_T

-- | Opaque data type referenced by VkPipelineCache
data VkPipelineCache_T

type VkPipelineLayout = VkPtr VkPipelineLayout_T

-- | Opaque data type referenced by VkPipelineLayout
data VkPipelineLayout_T

type VkQueryPool = VkPtr VkQueryPool_T

-- | Opaque data type referenced by VkQueryPool
data VkQueryPool_T

type VkQueue = Ptr VkQueue_T

-- | Opaque data type referenced by VkQueue
data VkQueue_T

type VkRenderPass = VkPtr VkRenderPass_T

-- | Opaque data type referenced by VkRenderPass
data VkRenderPass_T

type VkSampler = VkPtr VkSampler_T

-- | Opaque data type referenced by VkSampler
data VkSampler_T

type VkSamplerYcbcrConversion = VkPtr VkSamplerYcbcrConversion_T

-- | Opaque data type referenced by VkSamplerYcbcrConversion
data VkSamplerYcbcrConversion_T

type VkSamplerYcbcrConversionKHR =
     VkPtr VkSamplerYcbcrConversionKHR_T

-- | Opaque data type referenced by VkSamplerYcbcrConversionKHR
data VkSamplerYcbcrConversionKHR_T

type VkSemaphore = VkPtr VkSemaphore_T

-- | Opaque data type referenced by VkSemaphore
data VkSemaphore_T

type VkShaderModule = VkPtr VkShaderModule_T

-- | Opaque data type referenced by VkShaderModule
data VkShaderModule_T

type VkSurfaceKHR = VkPtr VkSurfaceKHR_T

-- | Opaque data type referenced by VkSurfaceKHR
data VkSurfaceKHR_T

type VkSwapchainKHR = VkPtr VkSwapchainKHR_T

-- | Opaque data type referenced by VkSwapchainKHR
data VkSwapchainKHR_T

type VkValidationCacheEXT = VkPtr VkValidationCacheEXT_T

-- | Opaque data type referenced by VkValidationCacheEXT
data VkValidationCacheEXT_T
