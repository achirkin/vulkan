module Main (main) where

import Graphics.Vulkan
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.String

import System.Mem (performGC)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  threadDelay 100000 >> performGC >> putStrLn "GC1 (before createInstance)..."

  (vkRes, vulkanInstance) <- createInstance
  putStrLn $ "createInstance: " ++ show vkRes

  performGC >> putStrLn "GC2 (after createInstance)"

  threadDelay 100001 >> performGC >> putStrLn "GC3 (after createInstance and delay)"

  destroyInstance vulkanInstance >> putStrLn "Destroyed instance"

  performGC >> putStrLn "GC4 (after destroyInstance)"

  threadDelay 100002 >> performGC >> putStrLn "GC5 (after destroyInstance and delay)"


createInstance :: IO (VkResult, VkInstance)
createInstance = alloca $ \vkInstPtr -> do

    -- allocate some strings - names
    progNamePtr <- newCString "01-CreateInstance"
    engineNamePtr <- newCString "My Perfect Haskell Engine"

    -- write VkApplicationInfo
    appInfoMut <- newVkData
    writeVkEngineVersion      appInfoMut (_VK_MAKE_VERSION 1 0 0)
    writeVkPEngineName        appInfoMut engineNamePtr
    writeVkApplicationVersion appInfoMut (_VK_MAKE_VERSION 1 0 0)
    writeVkPApplicationName   appInfoMut progNamePtr
    writeVkPNext              appInfoMut vkNullPtr
    writeVkSType              appInfoMut VK_STRUCTURE_TYPE_APPLICATION_INFO
    writeVkApiVersion         appInfoMut (_VK_MAKE_VERSION 1 0 68)


    -- write VkInstanceCreateInfo
    iCreateInfoMut <- newVkData
    writeVkPApplicationInfo        iCreateInfoMut
      (unsafeRef appInfoMut)  -- must remember to keep appInfoMut alive!
    writeVkPpEnabledExtensionNames iCreateInfoMut vkNullPtr
    writeVkEnabledExtensionCount   iCreateInfoMut 0
    writeVkPpEnabledLayerNames     iCreateInfoMut vkNullPtr
    writeVkEnabledLayerCount       iCreateInfoMut 0
    writeVkFlags                   iCreateInfoMut 0
    writeVkPNext                   iCreateInfoMut vkNullPtr
    writeVkSType                   iCreateInfoMut VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO

    -- execute createInstance
    vkRes <- vkCreateInstance
        (unsafeRef iCreateInfoMut) -- must remember to keep appInfoMut alive!
        nullPtr vkInstPtr
    vkInst <- peek vkInstPtr

    -- add finalizers
    -- the first two are added just to check if they are working
    addMutableFinalizer appInfoMut $ putStrLn "Finalizing appInfoMut..."
    addMutableFinalizer iCreateInfoMut $ putStrLn "Finalizing iCreateInfoMut..."
    -- this finalizer is important, we need it to cleanup resources
    --   after VkInstance is no longer in use
    addVkDataFinalizer vkInst $ do
      -- managed objects are GCed on their own, we just need to make sure
      --  that they are alive while vkInst is alive
      touchMutable appInfoMut
      touchMutable iCreateInfoMut
      -- unmanaged objects must be released explicitly
      free progNamePtr
      free engineNamePtr
      putStrLn "Finalizing vkInstance..."

    -- .. and, finally, return.
    return (vkRes, vkInst)


destroyInstance :: VkInstance -> IO ()
destroyInstance inst = vkDestroyInstance inst nullPtr >> touchVkData inst
