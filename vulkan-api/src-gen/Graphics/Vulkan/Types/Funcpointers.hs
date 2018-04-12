{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict    #-}
module Graphics.Vulkan.Types.Funcpointers
       (PFN_vkAllocationFunction, HS_vkAllocationFunction,
        newVkAllocationFunction, unwrapVkAllocationFunction,
        PFN_vkDebugReportCallbackEXT, HS_vkDebugReportCallbackEXT,
        newVkDebugReportCallbackEXT, unwrapVkDebugReportCallbackEXT,
        PFN_vkDebugUtilsMessengerCallbackEXT,
        HS_vkDebugUtilsMessengerCallbackEXT,
        newVkDebugUtilsMessengerCallbackEXT,
        unwrapVkDebugUtilsMessengerCallbackEXT, PFN_vkFreeFunction,
        HS_vkFreeFunction, newVkFreeFunction, unwrapVkFreeFunction,
        PFN_vkInternalAllocationNotification,
        HS_vkInternalAllocationNotification,
        newVkInternalAllocationNotification,
        unwrapVkInternalAllocationNotification,
        PFN_vkInternalFreeNotification, HS_vkInternalFreeNotification,
        newVkInternalFreeNotification, unwrapVkInternalFreeNotification,
        PFN_vkReallocationFunction, HS_vkReallocationFunction,
        newVkReallocationFunction, unwrapVkReallocationFunction,
        PFN_vkVoidFunction, HS_vkVoidFunction, newVkVoidFunction,
        unwrapVkVoidFunction)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes                                 (VkBool32 (..),
                                                                                  VkFlags (..))
import           Graphics.Vulkan.Types.Enum.Debug                                (VkDebugReportBitmaskEXT (..),
                                                                                  VkDebugReportFlagsEXT,
                                                                                  VkDebugReportObjectTypeEXT (..),
                                                                                  VkDebugUtilsMessageSeverityBitmaskEXT (..),
                                                                                  VkDebugUtilsMessageSeverityFlagBitsEXT,
                                                                                  VkDebugUtilsMessageTypeBitmaskEXT (..),
                                                                                  VkDebugUtilsMessageTypeFlagsEXT)
import           Graphics.Vulkan.Types.Enum.InternalAllocationType               (VkInternalAllocationType (..))
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope                (VkSystemAllocationScope (..))
import           Graphics.Vulkan.Types.Struct.DebugUtilsMessengerCallbackDataEXT (VkDebugUtilsMessengerCallbackDataEXT (..))

type HS_vkAllocationFunction =
     Ptr Void ->
       CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void)

-- | > typedef void* (VKAPI_PTR *PFN_vkAllocationFunction)(
--   >     void*                                       pUserData,
--   >     size_t                                      size,
--   >     size_t                                      alignment,
--   >     VkSystemAllocationScope                     allocationScope);
type PFN_vkAllocationFunction = FunPtr HS_vkAllocationFunction

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkAllocationFunction ::
               HS_vkAllocationFunction -> IO PFN_vkAllocationFunction

foreign import ccall "dynamic" unwrapVkAllocationFunction ::
               PFN_vkAllocationFunction -> HS_vkAllocationFunction

type HS_vkDebugReportCallbackEXT =
     VkDebugReportFlagsEXT ->
       VkDebugReportObjectTypeEXT ->
         Word64 ->
           CSize -> Int32 -> CString -> CString -> Ptr Void -> IO VkBool32

-- | > typedef VkBool32 (VKAPI_PTR *PFN_vkDebugReportCallbackEXT)(
--   >     VkDebugReportFlagsEXT                       flags,
--   >     VkDebugReportObjectTypeEXT                  objectType,
--   >     uint64_t                                    object,
--   >     size_t                                      location,
--   >     int32_t                                     messageCode,
--   >     const char*                                 pLayerPrefix,
--   >     const char*                                 pMessage,
--   >     void*                                       pUserData);
type PFN_vkDebugReportCallbackEXT =
     FunPtr HS_vkDebugReportCallbackEXT

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkDebugReportCallbackEXT ::
               HS_vkDebugReportCallbackEXT -> IO PFN_vkDebugReportCallbackEXT

foreign import ccall "dynamic" unwrapVkDebugReportCallbackEXT ::
               PFN_vkDebugReportCallbackEXT -> HS_vkDebugReportCallbackEXT

type HS_vkDebugUtilsMessengerCallbackEXT =
     VkDebugUtilsMessageSeverityFlagBitsEXT ->
       VkDebugUtilsMessageTypeFlagsEXT ->
         Ptr VkDebugUtilsMessengerCallbackDataEXT -> Ptr Void -> IO VkBool32

-- | > typedef VkBool32 (VKAPI_PTR *PFN_vkDebugUtilsMessengerCallbackEXT)(
--   >     VkDebugUtilsMessageSeverityFlagBitsEXT           messageSeverity,
--   >     VkDebugUtilsMessageTypeFlagsEXT                  messageType,
--   >     const VkDebugUtilsMessengerCallbackDataEXT*      pCallbackData,
--   >     void*                                            pUserData);
type PFN_vkDebugUtilsMessengerCallbackEXT =
     FunPtr HS_vkDebugUtilsMessengerCallbackEXT

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkDebugUtilsMessengerCallbackEXT
               ::
               HS_vkDebugUtilsMessengerCallbackEXT ->
                 IO PFN_vkDebugUtilsMessengerCallbackEXT

foreign import ccall "dynamic"
               unwrapVkDebugUtilsMessengerCallbackEXT ::
               PFN_vkDebugUtilsMessengerCallbackEXT ->
                 HS_vkDebugUtilsMessengerCallbackEXT

type HS_vkFreeFunction = Ptr Void -> Ptr Void -> IO ()

-- | > typedef void (VKAPI_PTR *PFN_vkFreeFunction)(
--   >     void*                                       pUserData,
--   >     void*                                       pMemory);
type PFN_vkFreeFunction = FunPtr HS_vkFreeFunction

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkFreeFunction ::
               HS_vkFreeFunction -> IO PFN_vkFreeFunction

foreign import ccall "dynamic" unwrapVkFreeFunction ::
               PFN_vkFreeFunction -> HS_vkFreeFunction

type HS_vkInternalAllocationNotification =
     Ptr Void ->
       CSize ->
         VkInternalAllocationType -> VkSystemAllocationScope -> IO ()

-- | > typedef void (VKAPI_PTR *PFN_vkInternalAllocationNotification)(
--   >     void*                                       pUserData,
--   >     size_t                                      size,
--   >     VkInternalAllocationType                    allocationType,
--   >     VkSystemAllocationScope                     allocationScope);
type PFN_vkInternalAllocationNotification =
     FunPtr HS_vkInternalAllocationNotification

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkInternalAllocationNotification
               ::
               HS_vkInternalAllocationNotification ->
                 IO PFN_vkInternalAllocationNotification

foreign import ccall "dynamic"
               unwrapVkInternalAllocationNotification ::
               PFN_vkInternalAllocationNotification ->
                 HS_vkInternalAllocationNotification

type HS_vkInternalFreeNotification =
     Ptr Void ->
       CSize ->
         VkInternalAllocationType -> VkSystemAllocationScope -> IO ()

-- | > typedef void (VKAPI_PTR *PFN_vkInternalFreeNotification)(
--   >     void*                                       pUserData,
--   >     size_t                                      size,
--   >     VkInternalAllocationType                    allocationType,
--   >     VkSystemAllocationScope                     allocationScope);
type PFN_vkInternalFreeNotification =
     FunPtr HS_vkInternalFreeNotification

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkInternalFreeNotification ::
               HS_vkInternalFreeNotification -> IO PFN_vkInternalFreeNotification

foreign import ccall "dynamic" unwrapVkInternalFreeNotification ::
               PFN_vkInternalFreeNotification -> HS_vkInternalFreeNotification

type HS_vkReallocationFunction =
     Ptr Void ->
       Ptr Void ->
         CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void)

-- | > typedef void* (VKAPI_PTR *PFN_vkReallocationFunction)(
--   >     void*                                       pUserData,
--   >     void*                                       pOriginal,
--   >     size_t                                      size,
--   >     size_t                                      alignment,
--   >     VkSystemAllocationScope                     allocationScope);
type PFN_vkReallocationFunction = FunPtr HS_vkReallocationFunction

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkReallocationFunction ::
               HS_vkReallocationFunction -> IO PFN_vkReallocationFunction

foreign import ccall "dynamic" unwrapVkReallocationFunction ::
               PFN_vkReallocationFunction -> HS_vkReallocationFunction

type HS_vkVoidFunction = IO ()

-- | > typedef void (VKAPI_PTR *PFN_vkVoidFunction)(void);
type PFN_vkVoidFunction = FunPtr HS_vkVoidFunction

-- | Wrap haskell function into C-callable FunPtr.
--   Note, you need to free resources after using it.
foreign import ccall "wrapper" newVkVoidFunction ::
               HS_vkVoidFunction -> IO PFN_vkVoidFunction

foreign import ccall "dynamic" unwrapVkVoidFunction ::
               PFN_vkVoidFunction -> HS_vkVoidFunction
