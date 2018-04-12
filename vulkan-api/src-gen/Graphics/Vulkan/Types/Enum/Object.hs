{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Object
       (VkObjectEntryTypeNVX(VkObjectEntryTypeNVX,
                             VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX,
                             VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX,
                             VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX,
                             VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX,
                             VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX),
        VkObjectEntryUsageBitmaskNVX(VkObjectEntryUsageBitmaskNVX,
                                     VkObjectEntryUsageFlagsNVX, VkObjectEntryUsageFlagBitsNVX,
                                     VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX,
                                     VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX),
        VkObjectEntryUsageFlagsNVX, VkObjectEntryUsageFlagBitsNVX,
        VkObjectType(VkObjectType, VK_OBJECT_TYPE_UNKNOWN,
                     VK_OBJECT_TYPE_INSTANCE, VK_OBJECT_TYPE_PHYSICAL_DEVICE,
                     VK_OBJECT_TYPE_DEVICE, VK_OBJECT_TYPE_QUEUE,
                     VK_OBJECT_TYPE_SEMAPHORE, VK_OBJECT_TYPE_COMMAND_BUFFER,
                     VK_OBJECT_TYPE_FENCE, VK_OBJECT_TYPE_DEVICE_MEMORY,
                     VK_OBJECT_TYPE_BUFFER, VK_OBJECT_TYPE_IMAGE, VK_OBJECT_TYPE_EVENT,
                     VK_OBJECT_TYPE_QUERY_POOL, VK_OBJECT_TYPE_BUFFER_VIEW,
                     VK_OBJECT_TYPE_IMAGE_VIEW, VK_OBJECT_TYPE_SHADER_MODULE,
                     VK_OBJECT_TYPE_PIPELINE_CACHE, VK_OBJECT_TYPE_PIPELINE_LAYOUT,
                     VK_OBJECT_TYPE_RENDER_PASS, VK_OBJECT_TYPE_PIPELINE,
                     VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT, VK_OBJECT_TYPE_SAMPLER,
                     VK_OBJECT_TYPE_DESCRIPTOR_POOL, VK_OBJECT_TYPE_DESCRIPTOR_SET,
                     VK_OBJECT_TYPE_FRAMEBUFFER, VK_OBJECT_TYPE_COMMAND_POOL))
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectEntryTypeNVX VkObjectEntryTypeNVX registry at www.khronos.org>
newtype VkObjectEntryTypeNVX = VkObjectEntryTypeNVX Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkObjectEntryTypeNVX where
        showsPrec _ VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX"
        showsPrec p (VkObjectEntryTypeNVX x)
          = showParen (p >= 11)
              (showString "VkObjectEntryTypeNVX " . showsPrec 11 x)

instance Read VkObjectEntryTypeNVX where
        readPrec
          = parens
              (choose
                 [("VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX)]
                 +++
                 prec 10
                   (expectP (Ident "VkObjectEntryTypeNVX") >>
                      (VkObjectEntryTypeNVX <$> step readPrec)))

pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX =
        VkObjectEntryTypeNVX 0

pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX :: VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = VkObjectEntryTypeNVX 1

pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX =
        VkObjectEntryTypeNVX 2

pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX =
        VkObjectEntryTypeNVX 3

pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX =
        VkObjectEntryTypeNVX 4

newtype VkObjectEntryUsageBitmaskNVX (a ::
                                        FlagType) = VkObjectEntryUsageBitmaskNVX VkFlags
                                                      deriving (Eq, Ord, Storable, Data, Generic)

type VkObjectEntryUsageFlagsNVX =
     VkObjectEntryUsageBitmaskNVX FlagMask

type VkObjectEntryUsageFlagBitsNVX =
     VkObjectEntryUsageBitmaskNVX FlagBit

pattern VkObjectEntryUsageFlagBitsNVX ::
        VkFlags -> VkObjectEntryUsageBitmaskNVX FlagBit

pattern VkObjectEntryUsageFlagBitsNVX n =
        VkObjectEntryUsageBitmaskNVX n

pattern VkObjectEntryUsageFlagsNVX ::
        VkFlags -> VkObjectEntryUsageBitmaskNVX FlagMask

pattern VkObjectEntryUsageFlagsNVX n =
        VkObjectEntryUsageBitmaskNVX n

deriving instance Bits (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance
         FiniteBits (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance Integral (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance Num (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance Bounded (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance Enum (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance Real (VkObjectEntryUsageBitmaskNVX FlagMask)

instance Show (VkObjectEntryUsageBitmaskNVX a) where
        showsPrec _ VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
          = showString "VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX"
        showsPrec _ VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
          = showString "VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX"
        showsPrec p (VkObjectEntryUsageBitmaskNVX x)
          = showParen (p >= 11)
              (showString "VkObjectEntryUsageBitmaskNVX " . showsPrec 11 x)

instance Read (VkObjectEntryUsageBitmaskNVX a) where
        readPrec
          = parens
              (choose
                 [("VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX",
                   pure VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX),
                  ("VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX",
                   pure VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX)]
                 +++
                 prec 10
                   (expectP (Ident "VkObjectEntryUsageBitmaskNVX") >>
                      (VkObjectEntryUsageBitmaskNVX <$> step readPrec)))

-- | bitpos = @0@
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX ::
        VkObjectEntryUsageBitmaskNVX a

pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX =
        VkObjectEntryUsageBitmaskNVX 1

-- | bitpos = @1@
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX ::
        VkObjectEntryUsageBitmaskNVX a

pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX =
        VkObjectEntryUsageBitmaskNVX 2

-- | Enums to track objects of various types
--
--   type = @enum@
--
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectType VkObjectType registry at www.khronos.org>
newtype VkObjectType = VkObjectType Int32
                         deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkObjectType where
        showsPrec _ VK_OBJECT_TYPE_UNKNOWN
          = showString "VK_OBJECT_TYPE_UNKNOWN"
        showsPrec _ VK_OBJECT_TYPE_INSTANCE
          = showString "VK_OBJECT_TYPE_INSTANCE"
        showsPrec _ VK_OBJECT_TYPE_PHYSICAL_DEVICE
          = showString "VK_OBJECT_TYPE_PHYSICAL_DEVICE"
        showsPrec _ VK_OBJECT_TYPE_DEVICE
          = showString "VK_OBJECT_TYPE_DEVICE"
        showsPrec _ VK_OBJECT_TYPE_QUEUE
          = showString "VK_OBJECT_TYPE_QUEUE"
        showsPrec _ VK_OBJECT_TYPE_SEMAPHORE
          = showString "VK_OBJECT_TYPE_SEMAPHORE"
        showsPrec _ VK_OBJECT_TYPE_COMMAND_BUFFER
          = showString "VK_OBJECT_TYPE_COMMAND_BUFFER"
        showsPrec _ VK_OBJECT_TYPE_FENCE
          = showString "VK_OBJECT_TYPE_FENCE"
        showsPrec _ VK_OBJECT_TYPE_DEVICE_MEMORY
          = showString "VK_OBJECT_TYPE_DEVICE_MEMORY"
        showsPrec _ VK_OBJECT_TYPE_BUFFER
          = showString "VK_OBJECT_TYPE_BUFFER"
        showsPrec _ VK_OBJECT_TYPE_IMAGE
          = showString "VK_OBJECT_TYPE_IMAGE"
        showsPrec _ VK_OBJECT_TYPE_EVENT
          = showString "VK_OBJECT_TYPE_EVENT"
        showsPrec _ VK_OBJECT_TYPE_QUERY_POOL
          = showString "VK_OBJECT_TYPE_QUERY_POOL"
        showsPrec _ VK_OBJECT_TYPE_BUFFER_VIEW
          = showString "VK_OBJECT_TYPE_BUFFER_VIEW"
        showsPrec _ VK_OBJECT_TYPE_IMAGE_VIEW
          = showString "VK_OBJECT_TYPE_IMAGE_VIEW"
        showsPrec _ VK_OBJECT_TYPE_SHADER_MODULE
          = showString "VK_OBJECT_TYPE_SHADER_MODULE"
        showsPrec _ VK_OBJECT_TYPE_PIPELINE_CACHE
          = showString "VK_OBJECT_TYPE_PIPELINE_CACHE"
        showsPrec _ VK_OBJECT_TYPE_PIPELINE_LAYOUT
          = showString "VK_OBJECT_TYPE_PIPELINE_LAYOUT"
        showsPrec _ VK_OBJECT_TYPE_RENDER_PASS
          = showString "VK_OBJECT_TYPE_RENDER_PASS"
        showsPrec _ VK_OBJECT_TYPE_PIPELINE
          = showString "VK_OBJECT_TYPE_PIPELINE"
        showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT
          = showString "VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
        showsPrec _ VK_OBJECT_TYPE_SAMPLER
          = showString "VK_OBJECT_TYPE_SAMPLER"
        showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_POOL
          = showString "VK_OBJECT_TYPE_DESCRIPTOR_POOL"
        showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_SET
          = showString "VK_OBJECT_TYPE_DESCRIPTOR_SET"
        showsPrec _ VK_OBJECT_TYPE_FRAMEBUFFER
          = showString "VK_OBJECT_TYPE_FRAMEBUFFER"
        showsPrec _ VK_OBJECT_TYPE_COMMAND_POOL
          = showString "VK_OBJECT_TYPE_COMMAND_POOL"
        showsPrec p (VkObjectType x)
          = showParen (p >= 11) (showString "VkObjectType " . showsPrec 11 x)

instance Read VkObjectType where
        readPrec
          = parens
              (choose
                 [("VK_OBJECT_TYPE_UNKNOWN", pure VK_OBJECT_TYPE_UNKNOWN),
                  ("VK_OBJECT_TYPE_INSTANCE", pure VK_OBJECT_TYPE_INSTANCE),
                  ("VK_OBJECT_TYPE_PHYSICAL_DEVICE",
                   pure VK_OBJECT_TYPE_PHYSICAL_DEVICE),
                  ("VK_OBJECT_TYPE_DEVICE", pure VK_OBJECT_TYPE_DEVICE),
                  ("VK_OBJECT_TYPE_QUEUE", pure VK_OBJECT_TYPE_QUEUE),
                  ("VK_OBJECT_TYPE_SEMAPHORE", pure VK_OBJECT_TYPE_SEMAPHORE),
                  ("VK_OBJECT_TYPE_COMMAND_BUFFER",
                   pure VK_OBJECT_TYPE_COMMAND_BUFFER),
                  ("VK_OBJECT_TYPE_FENCE", pure VK_OBJECT_TYPE_FENCE),
                  ("VK_OBJECT_TYPE_DEVICE_MEMORY",
                   pure VK_OBJECT_TYPE_DEVICE_MEMORY),
                  ("VK_OBJECT_TYPE_BUFFER", pure VK_OBJECT_TYPE_BUFFER),
                  ("VK_OBJECT_TYPE_IMAGE", pure VK_OBJECT_TYPE_IMAGE),
                  ("VK_OBJECT_TYPE_EVENT", pure VK_OBJECT_TYPE_EVENT),
                  ("VK_OBJECT_TYPE_QUERY_POOL", pure VK_OBJECT_TYPE_QUERY_POOL),
                  ("VK_OBJECT_TYPE_BUFFER_VIEW", pure VK_OBJECT_TYPE_BUFFER_VIEW),
                  ("VK_OBJECT_TYPE_IMAGE_VIEW", pure VK_OBJECT_TYPE_IMAGE_VIEW),
                  ("VK_OBJECT_TYPE_SHADER_MODULE",
                   pure VK_OBJECT_TYPE_SHADER_MODULE),
                  ("VK_OBJECT_TYPE_PIPELINE_CACHE",
                   pure VK_OBJECT_TYPE_PIPELINE_CACHE),
                  ("VK_OBJECT_TYPE_PIPELINE_LAYOUT",
                   pure VK_OBJECT_TYPE_PIPELINE_LAYOUT),
                  ("VK_OBJECT_TYPE_RENDER_PASS", pure VK_OBJECT_TYPE_RENDER_PASS),
                  ("VK_OBJECT_TYPE_PIPELINE", pure VK_OBJECT_TYPE_PIPELINE),
                  ("VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT",
                   pure VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT),
                  ("VK_OBJECT_TYPE_SAMPLER", pure VK_OBJECT_TYPE_SAMPLER),
                  ("VK_OBJECT_TYPE_DESCRIPTOR_POOL",
                   pure VK_OBJECT_TYPE_DESCRIPTOR_POOL),
                  ("VK_OBJECT_TYPE_DESCRIPTOR_SET",
                   pure VK_OBJECT_TYPE_DESCRIPTOR_SET),
                  ("VK_OBJECT_TYPE_FRAMEBUFFER", pure VK_OBJECT_TYPE_FRAMEBUFFER),
                  ("VK_OBJECT_TYPE_COMMAND_POOL", pure VK_OBJECT_TYPE_COMMAND_POOL)]
                 +++
                 prec 10
                   (expectP (Ident "VkObjectType") >>
                      (VkObjectType <$> step readPrec)))

pattern VK_OBJECT_TYPE_UNKNOWN :: VkObjectType

pattern VK_OBJECT_TYPE_UNKNOWN = VkObjectType 0

-- | VkInstance
pattern VK_OBJECT_TYPE_INSTANCE :: VkObjectType

pattern VK_OBJECT_TYPE_INSTANCE = VkObjectType 1

-- | VkPhysicalDevice
pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE :: VkObjectType

pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE = VkObjectType 2

-- | VkDevice
pattern VK_OBJECT_TYPE_DEVICE :: VkObjectType

pattern VK_OBJECT_TYPE_DEVICE = VkObjectType 3

-- | VkQueue
pattern VK_OBJECT_TYPE_QUEUE :: VkObjectType

pattern VK_OBJECT_TYPE_QUEUE = VkObjectType 4

-- | VkSemaphore
pattern VK_OBJECT_TYPE_SEMAPHORE :: VkObjectType

pattern VK_OBJECT_TYPE_SEMAPHORE = VkObjectType 5

-- | VkCommandBuffer
pattern VK_OBJECT_TYPE_COMMAND_BUFFER :: VkObjectType

pattern VK_OBJECT_TYPE_COMMAND_BUFFER = VkObjectType 6

-- | VkFence
pattern VK_OBJECT_TYPE_FENCE :: VkObjectType

pattern VK_OBJECT_TYPE_FENCE = VkObjectType 7

-- | VkDeviceMemory
pattern VK_OBJECT_TYPE_DEVICE_MEMORY :: VkObjectType

pattern VK_OBJECT_TYPE_DEVICE_MEMORY = VkObjectType 8

-- | VkBuffer
pattern VK_OBJECT_TYPE_BUFFER :: VkObjectType

pattern VK_OBJECT_TYPE_BUFFER = VkObjectType 9

-- | VkImage
pattern VK_OBJECT_TYPE_IMAGE :: VkObjectType

pattern VK_OBJECT_TYPE_IMAGE = VkObjectType 10

-- | VkEvent
pattern VK_OBJECT_TYPE_EVENT :: VkObjectType

pattern VK_OBJECT_TYPE_EVENT = VkObjectType 11

-- | VkQueryPool
pattern VK_OBJECT_TYPE_QUERY_POOL :: VkObjectType

pattern VK_OBJECT_TYPE_QUERY_POOL = VkObjectType 12

-- | VkBufferView
pattern VK_OBJECT_TYPE_BUFFER_VIEW :: VkObjectType

pattern VK_OBJECT_TYPE_BUFFER_VIEW = VkObjectType 13

-- | VkImageView
pattern VK_OBJECT_TYPE_IMAGE_VIEW :: VkObjectType

pattern VK_OBJECT_TYPE_IMAGE_VIEW = VkObjectType 14

-- | VkShaderModule
pattern VK_OBJECT_TYPE_SHADER_MODULE :: VkObjectType

pattern VK_OBJECT_TYPE_SHADER_MODULE = VkObjectType 15

-- | VkPipelineCache
pattern VK_OBJECT_TYPE_PIPELINE_CACHE :: VkObjectType

pattern VK_OBJECT_TYPE_PIPELINE_CACHE = VkObjectType 16

-- | VkPipelineLayout
pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT :: VkObjectType

pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT = VkObjectType 17

-- | VkRenderPass
pattern VK_OBJECT_TYPE_RENDER_PASS :: VkObjectType

pattern VK_OBJECT_TYPE_RENDER_PASS = VkObjectType 18

-- | VkPipeline
pattern VK_OBJECT_TYPE_PIPELINE :: VkObjectType

pattern VK_OBJECT_TYPE_PIPELINE = VkObjectType 19

-- | VkDescriptorSetLayout
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT :: VkObjectType

pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT = VkObjectType 20

-- | VkSampler
pattern VK_OBJECT_TYPE_SAMPLER :: VkObjectType

pattern VK_OBJECT_TYPE_SAMPLER = VkObjectType 21

-- | VkDescriptorPool
pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL :: VkObjectType

pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL = VkObjectType 22

-- | VkDescriptorSet
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET :: VkObjectType

pattern VK_OBJECT_TYPE_DESCRIPTOR_SET = VkObjectType 23

-- | VkFramebuffer
pattern VK_OBJECT_TYPE_FRAMEBUFFER :: VkObjectType

pattern VK_OBJECT_TYPE_FRAMEBUFFER = VkObjectType 24

-- | VkCommandPool
pattern VK_OBJECT_TYPE_COMMAND_POOL :: VkObjectType

pattern VK_OBJECT_TYPE_COMMAND_POOL = VkObjectType 25
