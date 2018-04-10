{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkQueueGlobalPriorityEXT
       (VkQueueGlobalPriorityEXT(VkQueueGlobalPriorityEXT,
                                 VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT,
                                 VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT,
                                 VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT,
                                 VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT))
       where
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkQueueGlobalPriorityEXT VkQueueGlobalPriorityEXT registry at www.khronos.org>
newtype VkQueueGlobalPriorityEXT = VkQueueGlobalPriorityEXT Int32
                                     deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkQueueGlobalPriorityEXT where
        showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT
          = showString "VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT"
        showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT
          = showString "VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT"
        showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT
          = showString "VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT"
        showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT
          = showString "VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT"
        showsPrec p (VkQueueGlobalPriorityEXT x)
          = showParen (p >= 11)
              (showString "VkQueueGlobalPriorityEXT " . showsPrec 11 x)

instance Read VkQueueGlobalPriorityEXT where
        readPrec
          = parens
              (choose
                 [("VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT",
                   pure VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT),
                  ("VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT",
                   pure VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT),
                  ("VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT",
                   pure VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT),
                  ("VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT",
                   pure VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueueGlobalPriorityEXT") >>
                      (VkQueueGlobalPriorityEXT <$> step readPrec)))

pattern VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT ::
        VkQueueGlobalPriorityEXT

pattern VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT =
        VkQueueGlobalPriorityEXT 128

pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT ::
        VkQueueGlobalPriorityEXT

pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT =
        VkQueueGlobalPriorityEXT 256

pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT ::
        VkQueueGlobalPriorityEXT

pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT =
        VkQueueGlobalPriorityEXT 512

pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT ::
        VkQueueGlobalPriorityEXT

pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT =
        VkQueueGlobalPriorityEXT 1024
