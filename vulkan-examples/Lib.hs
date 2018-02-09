{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib
    ( VulkanException (..), handleAllErrors, throwingVK, throwVKMsg
    , withVulkanInstance
    ) where

import           Control.Exception
import           Control.Monad         (when)
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.Vulkan


-- | Use this to throw all exceptions in this project
data VulkanException
  = VulkanException
  { vkeCode    :: Maybe VkResult
  , vkeMessage :: String
  } deriving (Eq, Show, Read)

instance Exception VulkanException where
  displayException (VulkanException Nothing msg)
    = unlines
    [ ""
    , "Vulkan exception:"
    , "*** " ++ msg
    ]
  displayException (VulkanException (Just c) msg)
    = unlines
    [ ""
    , "Vulkan error: " ++ show c
    , "*** " ++ msg
    ]

-- | Handle any error and return default value
handleAllErrors :: a -> SomeException -> IO a
handleAllErrors a (SomeException e)
  = a <$ putStrLn (displayException e)

-- | Throw VulkanException if something goes wrong
throwingVK :: String
           -> IO VkResult
           -> IO ()
throwingVK msg f = do
  vkRez <- f
  when (vkRez < VK_SUCCESS) $ throwIO $ VulkanException (Just vkRez) msg

-- | Throw VulkanException without error code
throwVKMsg :: String -> IO a
throwVKMsg msg = throwIO $ VulkanException Nothing msg


-- | Run an action with vulkan instance
withVulkanInstance :: String -- ^ program name
                   -> [CString]
                      -- ^ required extensions
                      --   passed as a list of CStrings, because they are
                      --   available either via vulkan-api pattern synonyms,
                      --   or from GLFW
                   -> [String]
                      -- ^ required layer names
                   -> (VkInstance -> IO ()) -> IO ()
withVulkanInstance
  progName extensions layers action = do
  execRez <-
    -- allocate some strings - names
    withArrayLen extensions $ \extCount extNames ->
    withCStringList layers $ \layerCount layerNames ->
    withCString progName $ \progNamePtr ->
    withCString "My Perfect Haskell Engine" $ \engineNamePtr -> do

      -- write VkApplicationInfo
      appInfo <- newVkData $ \appInfoPtr -> do
        writeField @"sType"              appInfoPtr VK_STRUCTURE_TYPE_APPLICATION_INFO
        writeField @"pNext"              appInfoPtr VK_NULL_HANDLE
        writeField @"pApplicationName"   appInfoPtr progNamePtr
        writeField @"applicationVersion" appInfoPtr (_VK_MAKE_VERSION 1 0 0)
        writeField @"pEngineName"        appInfoPtr engineNamePtr
        writeField @"engineVersion"      appInfoPtr (_VK_MAKE_VERSION 1 0 0)
        writeField @"apiVersion"         appInfoPtr (_VK_MAKE_VERSION 1 0 68)

      -- write VkInstanceCreateInfo
      iCreateInfo <- newVkData $ \iCreateInfoPtr -> do
        writeField @"sType"
          iCreateInfoPtr VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
        writeField @"pNext"
          iCreateInfoPtr VK_NULL_HANDLE
        writeField @"flags"
          iCreateInfoPtr 0
        writeField @"pApplicationInfo"
          iCreateInfoPtr (unsafePtr appInfo)  -- must keep appInfo alive!
        writeField @"enabledLayerCount"
          iCreateInfoPtr (fromIntegral layerCount)
        writeField @"ppEnabledLayerNames"
          iCreateInfoPtr layerNames
        writeField @"enabledExtensionCount"
          iCreateInfoPtr (fromIntegral extCount)
        writeField @"ppEnabledExtensionNames"
          iCreateInfoPtr extNames

      -- execute createInstance
      (vkResult, vkInstance) <- alloca $ \vkInstPtr -> do
        vkRes <- vkCreateInstance
          (unsafePtr iCreateInfo) -- must keep iCreateInfo alive!
          VK_NULL_HANDLE vkInstPtr
        vkI <- peek vkInstPtr
        return (vkRes, vkI)

      -- run a supplied action if instance creation was successful
      if vkResult == VK_SUCCESS
      then do
          eerr <- try $ action vkInstance
          vkDestroyInstance vkInstance VK_NULL_HANDLE
          -- make sure our data structures exist until the end of the program.
          touchVkData appInfo
          touchVkData iCreateInfo
          pure eerr
      else pure $ Left
                $ SomeException
                $ VulkanException
                   (Just vkResult)
                   "vkCreateInstance: Failed to create vkInstance."

  case execRez of
    Right ()                 -> pure ()
    Left (SomeException err) -> throwIO err

-- | Use list of haskell strings as @Ptr CString@
withCStringList :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStringList [] f = f 0 nullPtr
withCStringList xs f = go xs [] 0
  where
    go [] pts n     = withArray (reverse pts) (f n)
    go (s:ss) pts n = withCString s (\p -> go ss (p:pts) (n+1))
