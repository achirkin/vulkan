{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.Result
       (VkResult(VkResult, VK_SUCCESS, VK_NOT_READY, VK_TIMEOUT,
                 VK_EVENT_SET, VK_EVENT_RESET, VK_INCOMPLETE,
                 VK_ERROR_OUT_OF_HOST_MEMORY, VK_ERROR_OUT_OF_DEVICE_MEMORY,
                 VK_ERROR_INITIALIZATION_FAILED, VK_ERROR_DEVICE_LOST,
                 VK_ERROR_MEMORY_MAP_FAILED, VK_ERROR_LAYER_NOT_PRESENT,
                 VK_ERROR_EXTENSION_NOT_PRESENT, VK_ERROR_FEATURE_NOT_PRESENT,
                 VK_ERROR_INCOMPATIBLE_DRIVER, VK_ERROR_TOO_MANY_OBJECTS,
                 VK_ERROR_FORMAT_NOT_SUPPORTED, VK_ERROR_FRAGMENTED_POOL))
       where
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | API result codes
--
--   type = @enum@
--
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkResult VkResult registry at www.khronos.org>
newtype VkResult = VkResult Int32
                     deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkResult where
        showsPrec _ VK_SUCCESS = showString "VK_SUCCESS"
        showsPrec _ VK_NOT_READY = showString "VK_NOT_READY"
        showsPrec _ VK_TIMEOUT = showString "VK_TIMEOUT"
        showsPrec _ VK_EVENT_SET = showString "VK_EVENT_SET"
        showsPrec _ VK_EVENT_RESET = showString "VK_EVENT_RESET"
        showsPrec _ VK_INCOMPLETE = showString "VK_INCOMPLETE"
        showsPrec _ VK_ERROR_OUT_OF_HOST_MEMORY
          = showString "VK_ERROR_OUT_OF_HOST_MEMORY"
        showsPrec _ VK_ERROR_OUT_OF_DEVICE_MEMORY
          = showString "VK_ERROR_OUT_OF_DEVICE_MEMORY"
        showsPrec _ VK_ERROR_INITIALIZATION_FAILED
          = showString "VK_ERROR_INITIALIZATION_FAILED"
        showsPrec _ VK_ERROR_DEVICE_LOST
          = showString "VK_ERROR_DEVICE_LOST"
        showsPrec _ VK_ERROR_MEMORY_MAP_FAILED
          = showString "VK_ERROR_MEMORY_MAP_FAILED"
        showsPrec _ VK_ERROR_LAYER_NOT_PRESENT
          = showString "VK_ERROR_LAYER_NOT_PRESENT"
        showsPrec _ VK_ERROR_EXTENSION_NOT_PRESENT
          = showString "VK_ERROR_EXTENSION_NOT_PRESENT"
        showsPrec _ VK_ERROR_FEATURE_NOT_PRESENT
          = showString "VK_ERROR_FEATURE_NOT_PRESENT"
        showsPrec _ VK_ERROR_INCOMPATIBLE_DRIVER
          = showString "VK_ERROR_INCOMPATIBLE_DRIVER"
        showsPrec _ VK_ERROR_TOO_MANY_OBJECTS
          = showString "VK_ERROR_TOO_MANY_OBJECTS"
        showsPrec _ VK_ERROR_FORMAT_NOT_SUPPORTED
          = showString "VK_ERROR_FORMAT_NOT_SUPPORTED"
        showsPrec _ VK_ERROR_FRAGMENTED_POOL
          = showString "VK_ERROR_FRAGMENTED_POOL"
        showsPrec p (VkResult x)
          = showParen (p >= 11) (showString "VkResult " . showsPrec 11 x)

instance Read VkResult where
        readPrec
          = parens
              (choose
                 [("VK_SUCCESS", pure VK_SUCCESS),
                  ("VK_NOT_READY", pure VK_NOT_READY),
                  ("VK_TIMEOUT", pure VK_TIMEOUT),
                  ("VK_EVENT_SET", pure VK_EVENT_SET),
                  ("VK_EVENT_RESET", pure VK_EVENT_RESET),
                  ("VK_INCOMPLETE", pure VK_INCOMPLETE),
                  ("VK_ERROR_OUT_OF_HOST_MEMORY", pure VK_ERROR_OUT_OF_HOST_MEMORY),
                  ("VK_ERROR_OUT_OF_DEVICE_MEMORY",
                   pure VK_ERROR_OUT_OF_DEVICE_MEMORY),
                  ("VK_ERROR_INITIALIZATION_FAILED",
                   pure VK_ERROR_INITIALIZATION_FAILED),
                  ("VK_ERROR_DEVICE_LOST", pure VK_ERROR_DEVICE_LOST),
                  ("VK_ERROR_MEMORY_MAP_FAILED", pure VK_ERROR_MEMORY_MAP_FAILED),
                  ("VK_ERROR_LAYER_NOT_PRESENT", pure VK_ERROR_LAYER_NOT_PRESENT),
                  ("VK_ERROR_EXTENSION_NOT_PRESENT",
                   pure VK_ERROR_EXTENSION_NOT_PRESENT),
                  ("VK_ERROR_FEATURE_NOT_PRESENT",
                   pure VK_ERROR_FEATURE_NOT_PRESENT),
                  ("VK_ERROR_INCOMPATIBLE_DRIVER",
                   pure VK_ERROR_INCOMPATIBLE_DRIVER),
                  ("VK_ERROR_TOO_MANY_OBJECTS", pure VK_ERROR_TOO_MANY_OBJECTS),
                  ("VK_ERROR_FORMAT_NOT_SUPPORTED",
                   pure VK_ERROR_FORMAT_NOT_SUPPORTED),
                  ("VK_ERROR_FRAGMENTED_POOL", pure VK_ERROR_FRAGMENTED_POOL)]
                 +++
                 prec 10
                   (expectP (Ident "VkResult") >> (VkResult <$> step readPrec)))

-- | Command completed successfully
pattern VK_SUCCESS :: VkResult

pattern VK_SUCCESS = VkResult 0

-- | A fence or query has not yet completed
pattern VK_NOT_READY :: VkResult

pattern VK_NOT_READY = VkResult 1

-- | A wait operation has not completed in the specified time
pattern VK_TIMEOUT :: VkResult

pattern VK_TIMEOUT = VkResult 2

-- | An event is signaled
pattern VK_EVENT_SET :: VkResult

pattern VK_EVENT_SET = VkResult 3

-- | An event is unsignaled
pattern VK_EVENT_RESET :: VkResult

pattern VK_EVENT_RESET = VkResult 4

-- | A return array was too small for the result
pattern VK_INCOMPLETE :: VkResult

pattern VK_INCOMPLETE = VkResult 5

-- | A host memory allocation has failed
pattern VK_ERROR_OUT_OF_HOST_MEMORY :: VkResult

pattern VK_ERROR_OUT_OF_HOST_MEMORY = VkResult (-1)

-- | A device memory allocation has failed
pattern VK_ERROR_OUT_OF_DEVICE_MEMORY :: VkResult

pattern VK_ERROR_OUT_OF_DEVICE_MEMORY = VkResult (-2)

-- | Initialization of a object has failed
pattern VK_ERROR_INITIALIZATION_FAILED :: VkResult

pattern VK_ERROR_INITIALIZATION_FAILED = VkResult (-3)

-- | The logical device has been lost. See <<devsandqueues-lost-device>>
pattern VK_ERROR_DEVICE_LOST :: VkResult

pattern VK_ERROR_DEVICE_LOST = VkResult (-4)

-- | Mapping of a memory object has failed
pattern VK_ERROR_MEMORY_MAP_FAILED :: VkResult

pattern VK_ERROR_MEMORY_MAP_FAILED = VkResult (-5)

-- | Layer specified does not exist
pattern VK_ERROR_LAYER_NOT_PRESENT :: VkResult

pattern VK_ERROR_LAYER_NOT_PRESENT = VkResult (-6)

-- | Extension specified does not exist
pattern VK_ERROR_EXTENSION_NOT_PRESENT :: VkResult

pattern VK_ERROR_EXTENSION_NOT_PRESENT = VkResult (-7)

-- | Requested feature is not available on this device
pattern VK_ERROR_FEATURE_NOT_PRESENT :: VkResult

pattern VK_ERROR_FEATURE_NOT_PRESENT = VkResult (-8)

-- | Unable to find a Vulkan driver
pattern VK_ERROR_INCOMPATIBLE_DRIVER :: VkResult

pattern VK_ERROR_INCOMPATIBLE_DRIVER = VkResult (-9)

-- | Too many objects of the type have already been created
pattern VK_ERROR_TOO_MANY_OBJECTS :: VkResult

pattern VK_ERROR_TOO_MANY_OBJECTS = VkResult (-10)

-- | Requested format is not supported on this device
pattern VK_ERROR_FORMAT_NOT_SUPPORTED :: VkResult

pattern VK_ERROR_FORMAT_NOT_SUPPORTED = VkResult (-11)

-- | A requested pool allocation has failed due to fragmentation of the pool's memory
pattern VK_ERROR_FRAGMENTED_POOL :: VkResult

pattern VK_ERROR_FRAGMENTED_POOL = VkResult (-12)
