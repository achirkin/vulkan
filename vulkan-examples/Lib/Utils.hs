{-# LANGUAGE Strict #-}
module Lib.Utils
    ( VulkanException (..), handleAllErrors, throwingVK, throwVKMsg
    , withCStringList
    ) where

import           Control.Exception
import           Control.Monad         (when)
import           Foreign.C.String
import           Foreign.Marshal.Array
import           Foreign.Ptr
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



-- | Use list of haskell strings as @Ptr CString@
withCStringList :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStringList [] f = f 0 nullPtr
withCStringList xs f = go xs [] 0
  where
    go [] pts n     = withArray (reverse pts) (f n)
    go (s:ss) pts n = withCString s (\p -> go ss (p:pts) (n+1))
