{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Bitmasks
       (VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
        VkBuildAccelerationStructureFlagsNV(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorBindingFlagsEXT(..), VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDirectFBSurfaceCreateFlagsEXT(..),
        VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkGeometryFlagsNV(..),
        VkGeometryInstanceFlagsNV(..), VkHeadlessSurfaceCreateFlagsEXT(..),
        VkIOSSurfaceCreateFlagsMVK(..),
        VkImagePipeSurfaceCreateFlagsFUCHSIA(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMetalSurfaceCreateFlagsEXT(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageReductionStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationDepthClipStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineRasterizationStateStreamCreateFlagsEXT(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkResolveModeFlagsKHR(..),
        VkSemaphoreCreateFlags(..), VkSemaphoreImportFlagsKHR(..),
        VkSemaphoreWaitFlagsKHR(..),
        VkStreamDescriptorSurfaceCreateFlagsGGP(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..))
       where
import Data.Bits                       (Bits, FiniteBits)
import Data.Coerce                     (coerce)
import Foreign.Storable                (Storable)
import Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkAndroidSurfaceCreateFlagsKHR = VkAndroidSurfaceCreateFlagsKHR VkFlags
                                         deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkAndroidSurfaceCreateFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkAndroidSurfaceCreateFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkBufferViewCreateFlags = VkBufferViewCreateFlags VkFlags
                                  deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkBufferViewCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkBufferViewCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkBuildAccelerationStructureFlagsNV = VkBuildAccelerationStructureFlagsNV VkFlags
                                              deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkBuildAccelerationStructureFlagsNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkBuildAccelerationStructureFlagsNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkCommandPoolTrimFlags = VkCommandPoolTrimFlags VkFlags
                                 deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkCommandPoolTrimFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkCommandPoolTrimFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkCommandPoolTrimFlagsKHR = VkCommandPoolTrimFlagsKHR VkFlags
                                    deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkCommandPoolTrimFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkCommandPoolTrimFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDebugUtilsMessengerCallbackDataFlagsEXT = VkDebugUtilsMessengerCallbackDataFlagsEXT VkFlags
                                                    deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                              Storable)

instance Show VkDebugUtilsMessengerCallbackDataFlagsEXT where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDebugUtilsMessengerCallbackDataFlagsEXT where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDebugUtilsMessengerCreateFlagsEXT = VkDebugUtilsMessengerCreateFlagsEXT VkFlags
                                              deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDebugUtilsMessengerCreateFlagsEXT where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDebugUtilsMessengerCreateFlagsEXT where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDescriptorBindingFlagsEXT = VkDescriptorBindingFlagsEXT VkFlags
                                      deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDescriptorBindingFlagsEXT where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDescriptorBindingFlagsEXT where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDescriptorPoolResetFlags = VkDescriptorPoolResetFlags VkFlags
                                     deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDescriptorPoolResetFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDescriptorPoolResetFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDescriptorUpdateTemplateCreateFlags = VkDescriptorUpdateTemplateCreateFlags VkFlags
                                                deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDescriptorUpdateTemplateCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDescriptorUpdateTemplateCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDescriptorUpdateTemplateCreateFlagsKHR = VkDescriptorUpdateTemplateCreateFlagsKHR VkFlags
                                                   deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                             Storable)

instance Show VkDescriptorUpdateTemplateCreateFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDescriptorUpdateTemplateCreateFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDeviceCreateFlags = VkDeviceCreateFlags VkFlags
                              deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDeviceCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDeviceCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDirectFBSurfaceCreateFlagsEXT = VkDirectFBSurfaceCreateFlagsEXT VkFlags
                                          deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDirectFBSurfaceCreateFlagsEXT where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDirectFBSurfaceCreateFlagsEXT where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR VkFlags
                                      deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDisplayModeCreateFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDisplayModeCreateFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR VkFlags
                                         deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDisplaySurfaceCreateFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDisplaySurfaceCreateFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkEventCreateFlags = VkEventCreateFlags VkFlags
                             deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkEventCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkEventCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalFenceFeatureFlagsKHR = VkExternalFenceFeatureFlagsKHR VkFlags
                                         deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkExternalFenceFeatureFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkExternalFenceFeatureFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalFenceHandleTypeFlagsKHR = VkExternalFenceHandleTypeFlagsKHR VkFlags
                                            deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkExternalFenceHandleTypeFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkExternalFenceHandleTypeFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalMemoryFeatureFlagsKHR = VkExternalMemoryFeatureFlagsKHR VkFlags
                                          deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkExternalMemoryFeatureFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkExternalMemoryFeatureFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalMemoryHandleTypeFlagsKHR = VkExternalMemoryHandleTypeFlagsKHR VkFlags
                                             deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkExternalMemoryHandleTypeFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkExternalMemoryHandleTypeFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalSemaphoreFeatureFlagsKHR = VkExternalSemaphoreFeatureFlagsKHR VkFlags
                                             deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkExternalSemaphoreFeatureFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkExternalSemaphoreFeatureFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalSemaphoreHandleTypeFlagsKHR = VkExternalSemaphoreHandleTypeFlagsKHR VkFlags
                                                deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkExternalSemaphoreHandleTypeFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkExternalSemaphoreHandleTypeFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkFenceImportFlagsKHR = VkFenceImportFlagsKHR VkFlags
                                deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkFenceImportFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkFenceImportFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkGeometryFlagsNV = VkGeometryFlagsNV VkFlags
                            deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkGeometryFlagsNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkGeometryFlagsNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkGeometryInstanceFlagsNV = VkGeometryInstanceFlagsNV VkFlags
                                    deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkGeometryInstanceFlagsNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkGeometryInstanceFlagsNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkHeadlessSurfaceCreateFlagsEXT = VkHeadlessSurfaceCreateFlagsEXT VkFlags
                                          deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkHeadlessSurfaceCreateFlagsEXT where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkHeadlessSurfaceCreateFlagsEXT where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkIOSSurfaceCreateFlagsMVK = VkIOSSurfaceCreateFlagsMVK VkFlags
                                     deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkIOSSurfaceCreateFlagsMVK where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkIOSSurfaceCreateFlagsMVK where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkImagePipeSurfaceCreateFlagsFUCHSIA = VkImagePipeSurfaceCreateFlagsFUCHSIA VkFlags
                                               deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkImagePipeSurfaceCreateFlagsFUCHSIA where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkImagePipeSurfaceCreateFlagsFUCHSIA where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkInstanceCreateFlags = VkInstanceCreateFlags VkFlags
                                deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkInstanceCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkInstanceCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMacOSSurfaceCreateFlagsMVK = VkMacOSSurfaceCreateFlagsMVK VkFlags
                                       deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkMacOSSurfaceCreateFlagsMVK where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkMacOSSurfaceCreateFlagsMVK where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMemoryAllocateFlagsKHR = VkMemoryAllocateFlagsKHR VkFlags
                                   deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkMemoryAllocateFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkMemoryAllocateFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMemoryMapFlags = VkMemoryMapFlags VkFlags
                           deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkMemoryMapFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkMemoryMapFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMetalSurfaceCreateFlagsEXT = VkMetalSurfaceCreateFlagsEXT VkFlags
                                       deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkMetalSurfaceCreateFlagsEXT where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkMetalSurfaceCreateFlagsEXT where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPeerMemoryFeatureFlagsKHR = VkPeerMemoryFeatureFlagsKHR VkFlags
                                      deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPeerMemoryFeatureFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPeerMemoryFeatureFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags VkFlags
                                               deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineColorBlendStateCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineColorBlendStateCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineCoverageModulationStateCreateFlagsNV = VkPipelineCoverageModulationStateCreateFlagsNV VkFlags
                                                         deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                                   Storable)

instance Show VkPipelineCoverageModulationStateCreateFlagsNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineCoverageModulationStateCreateFlagsNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineCoverageReductionStateCreateFlagsNV = VkPipelineCoverageReductionStateCreateFlagsNV VkFlags
                                                        deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                                  Storable)

instance Show VkPipelineCoverageReductionStateCreateFlagsNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineCoverageReductionStateCreateFlagsNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineCoverageToColorStateCreateFlagsNV = VkPipelineCoverageToColorStateCreateFlagsNV VkFlags
                                                      deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                                Storable)

instance Show VkPipelineCoverageToColorStateCreateFlagsNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineCoverageToColorStateCreateFlagsNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags VkFlags
                                                 deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                           Storable)

instance Show VkPipelineDepthStencilStateCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineDepthStencilStateCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDiscardRectangleStateCreateFlagsEXT = VkPipelineDiscardRectangleStateCreateFlagsEXT VkFlags
                                                        deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                                  Storable)

instance Show VkPipelineDiscardRectangleStateCreateFlagsEXT where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineDiscardRectangleStateCreateFlagsEXT where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags VkFlags
                                            deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineDynamicStateCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineDynamicStateCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags VkFlags
                                                  deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                            Storable)

instance Show VkPipelineInputAssemblyStateCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineInputAssemblyStateCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags VkFlags
                                      deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineLayoutCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineLayoutCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags VkFlags
                                                deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineMultisampleStateCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineMultisampleStateCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT VkFlags
                                                                 deriving (Eq, Ord, Enum, Bits,
                                                                           FiniteBits, Storable)

instance Show
           VkPipelineRasterizationConservativeStateCreateFlagsEXT
         where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read
           VkPipelineRasterizationConservativeStateCreateFlagsEXT
         where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineRasterizationDepthClipStateCreateFlagsEXT = VkPipelineRasterizationDepthClipStateCreateFlagsEXT VkFlags
                                                              deriving (Eq, Ord, Enum, Bits,
                                                                        FiniteBits, Storable)

instance Show VkPipelineRasterizationDepthClipStateCreateFlagsEXT
         where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineRasterizationDepthClipStateCreateFlagsEXT
         where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags VkFlags
                                                  deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                            Storable)

instance Show VkPipelineRasterizationStateCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineRasterizationStateCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineRasterizationStateStreamCreateFlagsEXT = VkPipelineRasterizationStateStreamCreateFlagsEXT VkFlags
                                                           deriving (Eq, Ord, Enum, Bits,
                                                                     FiniteBits, Storable)

instance Show VkPipelineRasterizationStateStreamCreateFlagsEXT
         where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineRasterizationStateStreamCreateFlagsEXT
         where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags VkFlags
                                                 deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                           Storable)

instance Show VkPipelineTessellationStateCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineTessellationStateCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags VkFlags
                                                deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineVertexInputStateCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineVertexInputStateCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags VkFlags
                                             deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineViewportStateCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineViewportStateCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineViewportSwizzleStateCreateFlagsNV = VkPipelineViewportSwizzleStateCreateFlagsNV VkFlags
                                                      deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                                Storable)

instance Show VkPipelineViewportSwizzleStateCreateFlagsNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineViewportSwizzleStateCreateFlagsNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkQueryPoolCreateFlags = VkQueryPoolCreateFlags VkFlags
                                 deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkQueryPoolCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkQueryPoolCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkResolveModeFlagsKHR = VkResolveModeFlagsKHR VkFlags
                                deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkResolveModeFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkResolveModeFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSemaphoreCreateFlags = VkSemaphoreCreateFlags VkFlags
                                 deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkSemaphoreCreateFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkSemaphoreCreateFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSemaphoreImportFlagsKHR = VkSemaphoreImportFlagsKHR VkFlags
                                    deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkSemaphoreImportFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkSemaphoreImportFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSemaphoreWaitFlagsKHR = VkSemaphoreWaitFlagsKHR VkFlags
                                  deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkSemaphoreWaitFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkSemaphoreWaitFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkStreamDescriptorSurfaceCreateFlagsGGP = VkStreamDescriptorSurfaceCreateFlagsGGP VkFlags
                                                  deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                            Storable)

instance Show VkStreamDescriptorSurfaceCreateFlagsGGP where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkStreamDescriptorSurfaceCreateFlagsGGP where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT VkFlags
                                          deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkValidationCacheCreateFlagsEXT where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkValidationCacheCreateFlagsEXT where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkViSurfaceCreateFlagsNN = VkViSurfaceCreateFlagsNN VkFlags
                                   deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkViSurfaceCreateFlagsNN where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkViSurfaceCreateFlagsNN where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkWaylandSurfaceCreateFlagsKHR = VkWaylandSurfaceCreateFlagsKHR VkFlags
                                         deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkWaylandSurfaceCreateFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkWaylandSurfaceCreateFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkWin32SurfaceCreateFlagsKHR = VkWin32SurfaceCreateFlagsKHR VkFlags
                                       deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkWin32SurfaceCreateFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkWin32SurfaceCreateFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkXcbSurfaceCreateFlagsKHR = VkXcbSurfaceCreateFlagsKHR VkFlags
                                     deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkXcbSurfaceCreateFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkXcbSurfaceCreateFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkXlibSurfaceCreateFlagsKHR = VkXlibSurfaceCreateFlagsKHR VkFlags
                                      deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkXlibSurfaceCreateFlagsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkXlibSurfaceCreateFlagsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
