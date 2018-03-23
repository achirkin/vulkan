{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Bitmasks
       (VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkFramebufferCreateFlags(..),
        VkIOSSurfaceCreateFlagsMVK(..), VkImageViewCreateFlags(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMirSurfaceCreateFlagsKHR(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineCacheCreateFlags(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineShaderStageCreateFlags(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkRenderPassCreateFlags(..),
        VkSamplerCreateFlags(..), VkSemaphoreCreateFlags(..),
        VkSemaphoreImportFlagsKHR(..), VkShaderModuleCreateFlags(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..))
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkAndroidSurfaceCreateFlagsKHR = VkAndroidSurfaceCreateFlagsKHR VkFlags
                                           deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                     FiniteBits, Storable, Real, Data, Generic)

instance Show VkAndroidSurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkAndroidSurfaceCreateFlagsKHR x) = show x

instance Read VkAndroidSurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkBufferViewCreateFlags = VkBufferViewCreateFlags VkFlags
                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                              FiniteBits, Storable, Real, Data, Generic)

instance Show VkBufferViewCreateFlags where
        {-# INLINE show #-}
        show (VkBufferViewCreateFlags x) = show x

instance Read VkBufferViewCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkCommandPoolTrimFlags = VkCommandPoolTrimFlags VkFlags
                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                             FiniteBits, Storable, Real, Data, Generic)

instance Show VkCommandPoolTrimFlags where
        {-# INLINE show #-}
        show (VkCommandPoolTrimFlags x) = show x

instance Read VkCommandPoolTrimFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkCommandPoolTrimFlagsKHR = VkCommandPoolTrimFlagsKHR VkFlags
                                      deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                FiniteBits, Storable, Real, Data, Generic)

instance Show VkCommandPoolTrimFlagsKHR where
        {-# INLINE show #-}
        show (VkCommandPoolTrimFlagsKHR x) = show x

instance Read VkCommandPoolTrimFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDebugUtilsMessengerCallbackDataFlagsEXT = VkDebugUtilsMessengerCallbackDataFlagsEXT VkFlags
                                                      deriving (Eq, Ord, Num, Bounded, Enum,
                                                                Integral, Bits, FiniteBits,
                                                                Storable, Real, Data, Generic)

instance Show VkDebugUtilsMessengerCallbackDataFlagsEXT where
        {-# INLINE show #-}
        show (VkDebugUtilsMessengerCallbackDataFlagsEXT x) = show x

instance Read VkDebugUtilsMessengerCallbackDataFlagsEXT where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDebugUtilsMessengerCreateFlagsEXT = VkDebugUtilsMessengerCreateFlagsEXT VkFlags
                                                deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                          Bits, FiniteBits, Storable, Real, Data,
                                                          Generic)

instance Show VkDebugUtilsMessengerCreateFlagsEXT where
        {-# INLINE show #-}
        show (VkDebugUtilsMessengerCreateFlagsEXT x) = show x

instance Read VkDebugUtilsMessengerCreateFlagsEXT where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDescriptorPoolResetFlags = VkDescriptorPoolResetFlags VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkDescriptorPoolResetFlags where
        {-# INLINE show #-}
        show (VkDescriptorPoolResetFlags x) = show x

instance Read VkDescriptorPoolResetFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDescriptorUpdateTemplateCreateFlags = VkDescriptorUpdateTemplateCreateFlags VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkDescriptorUpdateTemplateCreateFlags where
        {-# INLINE show #-}
        show (VkDescriptorUpdateTemplateCreateFlags x) = show x

instance Read VkDescriptorUpdateTemplateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDescriptorUpdateTemplateCreateFlagsKHR = VkDescriptorUpdateTemplateCreateFlagsKHR VkFlags
                                                     deriving (Eq, Ord, Num, Bounded, Enum,
                                                               Integral, Bits, FiniteBits, Storable,
                                                               Real, Data, Generic)

instance Show VkDescriptorUpdateTemplateCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkDescriptorUpdateTemplateCreateFlagsKHR x) = show x

instance Read VkDescriptorUpdateTemplateCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDeviceCreateFlags = VkDeviceCreateFlags VkFlags
                                deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                          Storable, Real, Data, Generic)

instance Show VkDeviceCreateFlags where
        {-# INLINE show #-}
        show (VkDeviceCreateFlags x) = show x

instance Read VkDeviceCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkDisplayModeCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkDisplayModeCreateFlagsKHR x) = show x

instance Read VkDisplayModeCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR VkFlags
                                           deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                     FiniteBits, Storable, Real, Data, Generic)

instance Show VkDisplaySurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkDisplaySurfaceCreateFlagsKHR x) = show x

instance Read VkDisplaySurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkEventCreateFlags = VkEventCreateFlags VkFlags
                               deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                         Storable, Real, Data, Generic)

instance Show VkEventCreateFlags where
        {-# INLINE show #-}
        show (VkEventCreateFlags x) = show x

instance Read VkEventCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalFenceFeatureFlagsKHR = VkExternalFenceFeatureFlagsKHR VkFlags
                                           deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                     FiniteBits, Storable, Real, Data, Generic)

instance Show VkExternalFenceFeatureFlagsKHR where
        {-# INLINE show #-}
        show (VkExternalFenceFeatureFlagsKHR x) = show x

instance Read VkExternalFenceFeatureFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalFenceHandleTypeFlagsKHR = VkExternalFenceHandleTypeFlagsKHR VkFlags
                                              deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                        FiniteBits, Storable, Real, Data, Generic)

instance Show VkExternalFenceHandleTypeFlagsKHR where
        {-# INLINE show #-}
        show (VkExternalFenceHandleTypeFlagsKHR x) = show x

instance Read VkExternalFenceHandleTypeFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalMemoryFeatureFlagsKHR = VkExternalMemoryFeatureFlagsKHR VkFlags
                                            deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                      FiniteBits, Storable, Real, Data, Generic)

instance Show VkExternalMemoryFeatureFlagsKHR where
        {-# INLINE show #-}
        show (VkExternalMemoryFeatureFlagsKHR x) = show x

instance Read VkExternalMemoryFeatureFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalMemoryHandleTypeFlagsKHR = VkExternalMemoryHandleTypeFlagsKHR VkFlags
                                               deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                         Bits, FiniteBits, Storable, Real, Data,
                                                         Generic)

instance Show VkExternalMemoryHandleTypeFlagsKHR where
        {-# INLINE show #-}
        show (VkExternalMemoryHandleTypeFlagsKHR x) = show x

instance Read VkExternalMemoryHandleTypeFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalSemaphoreFeatureFlagsKHR = VkExternalSemaphoreFeatureFlagsKHR VkFlags
                                               deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                         Bits, FiniteBits, Storable, Real, Data,
                                                         Generic)

instance Show VkExternalSemaphoreFeatureFlagsKHR where
        {-# INLINE show #-}
        show (VkExternalSemaphoreFeatureFlagsKHR x) = show x

instance Read VkExternalSemaphoreFeatureFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalSemaphoreHandleTypeFlagsKHR = VkExternalSemaphoreHandleTypeFlagsKHR VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkExternalSemaphoreHandleTypeFlagsKHR where
        {-# INLINE show #-}
        show (VkExternalSemaphoreHandleTypeFlagsKHR x) = show x

instance Read VkExternalSemaphoreHandleTypeFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkFenceImportFlagsKHR = VkFenceImportFlagsKHR VkFlags
                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                            Storable, Real, Data, Generic)

instance Show VkFenceImportFlagsKHR where
        {-# INLINE show #-}
        show (VkFenceImportFlagsKHR x) = show x

instance Read VkFenceImportFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkFramebufferCreateFlags = VkFramebufferCreateFlags VkFlags
                                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                               FiniteBits, Storable, Real, Data, Generic)

instance Show VkFramebufferCreateFlags where
        {-# INLINE show #-}
        show (VkFramebufferCreateFlags x) = show x

instance Read VkFramebufferCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkIOSSurfaceCreateFlagsMVK = VkIOSSurfaceCreateFlagsMVK VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkIOSSurfaceCreateFlagsMVK where
        {-# INLINE show #-}
        show (VkIOSSurfaceCreateFlagsMVK x) = show x

instance Read VkIOSSurfaceCreateFlagsMVK where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkImageViewCreateFlags = VkImageViewCreateFlags VkFlags
                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                             FiniteBits, Storable, Real, Data, Generic)

instance Show VkImageViewCreateFlags where
        {-# INLINE show #-}
        show (VkImageViewCreateFlags x) = show x

instance Read VkImageViewCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkInstanceCreateFlags = VkInstanceCreateFlags VkFlags
                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                            Storable, Real, Data, Generic)

instance Show VkInstanceCreateFlags where
        {-# INLINE show #-}
        show (VkInstanceCreateFlags x) = show x

instance Read VkInstanceCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMacOSSurfaceCreateFlagsMVK = VkMacOSSurfaceCreateFlagsMVK VkFlags
                                         deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                   FiniteBits, Storable, Real, Data, Generic)

instance Show VkMacOSSurfaceCreateFlagsMVK where
        {-# INLINE show #-}
        show (VkMacOSSurfaceCreateFlagsMVK x) = show x

instance Read VkMacOSSurfaceCreateFlagsMVK where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMemoryAllocateFlagsKHR = VkMemoryAllocateFlagsKHR VkFlags
                                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                               FiniteBits, Storable, Real, Data, Generic)

instance Show VkMemoryAllocateFlagsKHR where
        {-# INLINE show #-}
        show (VkMemoryAllocateFlagsKHR x) = show x

instance Read VkMemoryAllocateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMemoryMapFlags = VkMemoryMapFlags VkFlags
                             deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                       Storable, Real, Data, Generic)

instance Show VkMemoryMapFlags where
        {-# INLINE show #-}
        show (VkMemoryMapFlags x) = show x

instance Read VkMemoryMapFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMirSurfaceCreateFlagsKHR = VkMirSurfaceCreateFlagsKHR VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkMirSurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkMirSurfaceCreateFlagsKHR x) = show x

instance Read VkMirSurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPeerMemoryFeatureFlagsKHR = VkPeerMemoryFeatureFlagsKHR VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkPeerMemoryFeatureFlagsKHR where
        {-# INLINE show #-}
        show (VkPeerMemoryFeatureFlagsKHR x) = show x

instance Read VkPeerMemoryFeatureFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineCacheCreateFlags = VkPipelineCacheCreateFlags VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineCacheCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineCacheCreateFlags x) = show x

instance Read VkPipelineCacheCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags VkFlags
                                                 deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                           Bits, FiniteBits, Storable, Real, Data,
                                                           Generic)

instance Show VkPipelineColorBlendStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineColorBlendStateCreateFlags x) = show x

instance Read VkPipelineColorBlendStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineCoverageModulationStateCreateFlagsNV = VkPipelineCoverageModulationStateCreateFlagsNV VkFlags
                                                           deriving (Eq, Ord, Num, Bounded, Enum,
                                                                     Integral, Bits, FiniteBits,
                                                                     Storable, Real, Data, Generic)

instance Show VkPipelineCoverageModulationStateCreateFlagsNV where
        {-# INLINE show #-}
        show (VkPipelineCoverageModulationStateCreateFlagsNV x) = show x

instance Read VkPipelineCoverageModulationStateCreateFlagsNV where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineCoverageToColorStateCreateFlagsNV = VkPipelineCoverageToColorStateCreateFlagsNV VkFlags
                                                        deriving (Eq, Ord, Num, Bounded, Enum,
                                                                  Integral, Bits, FiniteBits,
                                                                  Storable, Real, Data, Generic)

instance Show VkPipelineCoverageToColorStateCreateFlagsNV where
        {-# INLINE show #-}
        show (VkPipelineCoverageToColorStateCreateFlagsNV x) = show x

instance Read VkPipelineCoverageToColorStateCreateFlagsNV where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags VkFlags
                                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                             Bits, FiniteBits, Storable, Real, Data,
                                                             Generic)

instance Show VkPipelineDepthStencilStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineDepthStencilStateCreateFlags x) = show x

instance Read VkPipelineDepthStencilStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDiscardRectangleStateCreateFlagsEXT = VkPipelineDiscardRectangleStateCreateFlagsEXT VkFlags
                                                          deriving (Eq, Ord, Num, Bounded, Enum,
                                                                    Integral, Bits, FiniteBits,
                                                                    Storable, Real, Data, Generic)

instance Show VkPipelineDiscardRectangleStateCreateFlagsEXT where
        {-# INLINE show #-}
        show (VkPipelineDiscardRectangleStateCreateFlagsEXT x) = show x

instance Read VkPipelineDiscardRectangleStateCreateFlagsEXT where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags VkFlags
                                              deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                        FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineDynamicStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineDynamicStateCreateFlags x) = show x

instance Read VkPipelineDynamicStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags VkFlags
                                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                              Bits, FiniteBits, Storable, Real,
                                                              Data, Generic)

instance Show VkPipelineInputAssemblyStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineInputAssemblyStateCreateFlags x) = show x

instance Read VkPipelineInputAssemblyStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineLayoutCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineLayoutCreateFlags x) = show x

instance Read VkPipelineLayoutCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkPipelineMultisampleStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineMultisampleStateCreateFlags x) = show x

instance Read VkPipelineMultisampleStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT VkFlags
                                                                   deriving (Eq, Ord, Num, Bounded,
                                                                             Enum, Integral, Bits,
                                                                             FiniteBits, Storable,
                                                                             Real, Data, Generic)

instance Show
           VkPipelineRasterizationConservativeStateCreateFlagsEXT
         where
        {-# INLINE show #-}
        show (VkPipelineRasterizationConservativeStateCreateFlagsEXT x)
          = show x

instance Read
           VkPipelineRasterizationConservativeStateCreateFlagsEXT
         where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags VkFlags
                                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                              Bits, FiniteBits, Storable, Real,
                                                              Data, Generic)

instance Show VkPipelineRasterizationStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineRasterizationStateCreateFlags x) = show x

instance Read VkPipelineRasterizationStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags VkFlags
                                             deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                       FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineShaderStageCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineShaderStageCreateFlags x) = show x

instance Read VkPipelineShaderStageCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags VkFlags
                                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                             Bits, FiniteBits, Storable, Real, Data,
                                                             Generic)

instance Show VkPipelineTessellationStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineTessellationStateCreateFlags x) = show x

instance Read VkPipelineTessellationStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkPipelineVertexInputStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineVertexInputStateCreateFlags x) = show x

instance Read VkPipelineVertexInputStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags VkFlags
                                               deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                         Bits, FiniteBits, Storable, Real, Data,
                                                         Generic)

instance Show VkPipelineViewportStateCreateFlags where
        {-# INLINE show #-}
        show (VkPipelineViewportStateCreateFlags x) = show x

instance Read VkPipelineViewportStateCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineViewportSwizzleStateCreateFlagsNV = VkPipelineViewportSwizzleStateCreateFlagsNV VkFlags
                                                        deriving (Eq, Ord, Num, Bounded, Enum,
                                                                  Integral, Bits, FiniteBits,
                                                                  Storable, Real, Data, Generic)

instance Show VkPipelineViewportSwizzleStateCreateFlagsNV where
        {-# INLINE show #-}
        show (VkPipelineViewportSwizzleStateCreateFlagsNV x) = show x

instance Read VkPipelineViewportSwizzleStateCreateFlagsNV where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkQueryPoolCreateFlags = VkQueryPoolCreateFlags VkFlags
                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                             FiniteBits, Storable, Real, Data, Generic)

instance Show VkQueryPoolCreateFlags where
        {-# INLINE show #-}
        show (VkQueryPoolCreateFlags x) = show x

instance Read VkQueryPoolCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags VkFlags
                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                              FiniteBits, Storable, Real, Data, Generic)

instance Show VkRenderPassCreateFlags where
        {-# INLINE show #-}
        show (VkRenderPassCreateFlags x) = show x

instance Read VkRenderPassCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSamplerCreateFlags = VkSamplerCreateFlags VkFlags
                                 deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                           Storable, Real, Data, Generic)

instance Show VkSamplerCreateFlags where
        {-# INLINE show #-}
        show (VkSamplerCreateFlags x) = show x

instance Read VkSamplerCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSemaphoreCreateFlags = VkSemaphoreCreateFlags VkFlags
                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                             FiniteBits, Storable, Real, Data, Generic)

instance Show VkSemaphoreCreateFlags where
        {-# INLINE show #-}
        show (VkSemaphoreCreateFlags x) = show x

instance Read VkSemaphoreCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSemaphoreImportFlagsKHR = VkSemaphoreImportFlagsKHR VkFlags
                                      deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                FiniteBits, Storable, Real, Data, Generic)

instance Show VkSemaphoreImportFlagsKHR where
        {-# INLINE show #-}
        show (VkSemaphoreImportFlagsKHR x) = show x

instance Read VkSemaphoreImportFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkShaderModuleCreateFlags = VkShaderModuleCreateFlags VkFlags
                                      deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                FiniteBits, Storable, Real, Data, Generic)

instance Show VkShaderModuleCreateFlags where
        {-# INLINE show #-}
        show (VkShaderModuleCreateFlags x) = show x

instance Read VkShaderModuleCreateFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT VkFlags
                                            deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                      FiniteBits, Storable, Real, Data, Generic)

instance Show VkValidationCacheCreateFlagsEXT where
        {-# INLINE show #-}
        show (VkValidationCacheCreateFlagsEXT x) = show x

instance Read VkValidationCacheCreateFlagsEXT where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkViSurfaceCreateFlagsNN = VkViSurfaceCreateFlagsNN VkFlags
                                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                               FiniteBits, Storable, Real, Data, Generic)

instance Show VkViSurfaceCreateFlagsNN where
        {-# INLINE show #-}
        show (VkViSurfaceCreateFlagsNN x) = show x

instance Read VkViSurfaceCreateFlagsNN where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkWaylandSurfaceCreateFlagsKHR = VkWaylandSurfaceCreateFlagsKHR VkFlags
                                           deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                     FiniteBits, Storable, Real, Data, Generic)

instance Show VkWaylandSurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkWaylandSurfaceCreateFlagsKHR x) = show x

instance Read VkWaylandSurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkWin32SurfaceCreateFlagsKHR = VkWin32SurfaceCreateFlagsKHR VkFlags
                                         deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                   FiniteBits, Storable, Real, Data, Generic)

instance Show VkWin32SurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkWin32SurfaceCreateFlagsKHR x) = show x

instance Read VkWin32SurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkXcbSurfaceCreateFlagsKHR = VkXcbSurfaceCreateFlagsKHR VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkXcbSurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkXcbSurfaceCreateFlagsKHR x) = show x

instance Read VkXcbSurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkXlibSurfaceCreateFlagsKHR = VkXlibSurfaceCreateFlagsKHR VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkXlibSurfaceCreateFlagsKHR where
        {-# INLINE show #-}
        show (VkXlibSurfaceCreateFlagsKHR x) = show x

instance Read VkXlibSurfaceCreateFlagsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
