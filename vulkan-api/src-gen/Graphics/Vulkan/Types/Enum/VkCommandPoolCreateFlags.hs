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
module Graphics.Vulkan.Types.Enum.VkCommandPoolCreateFlags
       (VkCommandPoolCreateBitmask(VkCommandPoolCreateBitmask,
                                   VkCommandPoolCreateFlags, VkCommandPoolCreateFlagBits,
                                   VK_COMMAND_POOL_CREATE_TRANSIENT_BIT,
                                   VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT),
        VkCommandPoolCreateFlags, VkCommandPoolCreateFlagBits)
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

newtype VkCommandPoolCreateBitmask (a ::
                                      FlagType) = VkCommandPoolCreateBitmask VkFlags
                                                    deriving (Eq, Ord, Storable, Data, Generic)

type VkCommandPoolCreateFlags = VkCommandPoolCreateBitmask FlagMask

type VkCommandPoolCreateFlagBits =
     VkCommandPoolCreateBitmask FlagBit

pattern VkCommandPoolCreateFlagBits ::
        VkFlags -> VkCommandPoolCreateBitmask FlagBit

pattern VkCommandPoolCreateFlagBits n =
        VkCommandPoolCreateBitmask n

pattern VkCommandPoolCreateFlags ::
        VkFlags -> VkCommandPoolCreateBitmask FlagMask

pattern VkCommandPoolCreateFlags n = VkCommandPoolCreateBitmask n

deriving instance Bits (VkCommandPoolCreateBitmask FlagMask)

deriving instance FiniteBits (VkCommandPoolCreateBitmask FlagMask)

deriving instance Integral (VkCommandPoolCreateBitmask FlagMask)

deriving instance Num (VkCommandPoolCreateBitmask FlagMask)

deriving instance Bounded (VkCommandPoolCreateBitmask FlagMask)

deriving instance Enum (VkCommandPoolCreateBitmask FlagMask)

deriving instance Real (VkCommandPoolCreateBitmask FlagMask)

instance Show (VkCommandPoolCreateBitmask a) where
        showsPrec _ VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
          = showString "VK_COMMAND_POOL_CREATE_TRANSIENT_BIT"
        showsPrec _ VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
          = showString "VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
        showsPrec p (VkCommandPoolCreateBitmask x)
          = showParen (p >= 11)
              (showString "VkCommandPoolCreateBitmask " . showsPrec 11 x)

instance Read (VkCommandPoolCreateBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_POOL_CREATE_TRANSIENT_BIT",
                   pure VK_COMMAND_POOL_CREATE_TRANSIENT_BIT),
                  ("VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT",
                   pure VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandPoolCreateBitmask") >>
                      (VkCommandPoolCreateBitmask <$> step readPrec)))

-- | Command buffers have a short lifetime
--
--   bitpos = @0@
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT ::
        VkCommandPoolCreateBitmask a

pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT =
        VkCommandPoolCreateBitmask 1

-- | Command buffers may release their memory individually
--
--   bitpos = @1@
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT ::
        VkCommandPoolCreateBitmask a

pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT =
        VkCommandPoolCreateBitmask 2
