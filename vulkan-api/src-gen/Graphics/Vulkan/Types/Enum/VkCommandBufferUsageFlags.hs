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
module Graphics.Vulkan.Types.Enum.VkCommandBufferUsageFlags
       (VkCommandBufferUsageBitmask(VkCommandBufferUsageBitmask,
                                    VkCommandBufferUsageFlags, VkCommandBufferUsageFlagBits,
                                    VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
                                    VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT,
                                    VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT),
        VkCommandBufferUsageFlags, VkCommandBufferUsageFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkCommandBufferUsageBitmask (a ::
                                       FlagType) = VkCommandBufferUsageBitmask VkFlags
                                                     deriving (Eq, Ord, Storable, Data, Generic)

type VkCommandBufferUsageFlags =
     VkCommandBufferUsageBitmask FlagMask

type VkCommandBufferUsageFlagBits =
     VkCommandBufferUsageBitmask FlagBit

pattern VkCommandBufferUsageFlagBits ::
        VkFlags -> VkCommandBufferUsageBitmask FlagBit

pattern VkCommandBufferUsageFlagBits n =
        VkCommandBufferUsageBitmask n

pattern VkCommandBufferUsageFlags ::
        VkFlags -> VkCommandBufferUsageBitmask FlagMask

pattern VkCommandBufferUsageFlags n = VkCommandBufferUsageBitmask n

deriving instance Bits (VkCommandBufferUsageBitmask FlagMask)

deriving instance FiniteBits (VkCommandBufferUsageBitmask FlagMask)

deriving instance Integral (VkCommandBufferUsageBitmask FlagMask)

deriving instance Num (VkCommandBufferUsageBitmask FlagMask)

deriving instance Bounded (VkCommandBufferUsageBitmask FlagMask)

deriving instance Enum (VkCommandBufferUsageBitmask FlagMask)

deriving instance Real (VkCommandBufferUsageBitmask FlagMask)

instance Show (VkCommandBufferUsageBitmask a) where
        showsPrec _ VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
          = showString "VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
        showsPrec _ VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
          = showString "VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
        showsPrec _ VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
          = showString "VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
        showsPrec p (VkCommandBufferUsageBitmask x)
          = showParen (p >= 11)
              (showString "VkCommandBufferUsageBitmask " . showsPrec 11 x)

instance Read (VkCommandBufferUsageBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT",
                   pure VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT),
                  ("VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT",
                   pure VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT),
                  ("VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT",
                   pure VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandBufferUsageBitmask") >>
                      (VkCommandBufferUsageBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT ::
        VkCommandBufferUsageBitmask a

pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT =
        VkCommandBufferUsageBitmask 1

-- | bitpos = @1@
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT ::
        VkCommandBufferUsageBitmask a

pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT =
        VkCommandBufferUsageBitmask 2

-- | Command buffer may be submitted/executed more than once simultaneously
--
--   bitpos = @2@
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT ::
        VkCommandBufferUsageBitmask a

pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT =
        VkCommandBufferUsageBitmask 4
