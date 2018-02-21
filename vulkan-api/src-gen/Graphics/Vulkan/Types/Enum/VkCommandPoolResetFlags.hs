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
module Graphics.Vulkan.Types.Enum.VkCommandPoolResetFlags
       (VkCommandPoolResetBitmask(VkCommandPoolResetBitmask,
                                  VkCommandPoolResetFlags, VkCommandPoolResetFlagBits,
                                  VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT),
        VkCommandPoolResetFlags, VkCommandPoolResetFlagBits)
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

newtype VkCommandPoolResetBitmask (a ::
                                     FlagType) = VkCommandPoolResetBitmask VkFlags
                                                   deriving (Eq, Ord, Storable, Data, Generic)

type VkCommandPoolResetFlags = VkCommandPoolResetBitmask FlagMask

type VkCommandPoolResetFlagBits = VkCommandPoolResetBitmask FlagBit

pattern VkCommandPoolResetFlagBits ::
        VkFlags -> VkCommandPoolResetBitmask FlagBit

pattern VkCommandPoolResetFlagBits n = VkCommandPoolResetBitmask n

pattern VkCommandPoolResetFlags ::
        VkFlags -> VkCommandPoolResetBitmask FlagMask

pattern VkCommandPoolResetFlags n = VkCommandPoolResetBitmask n

deriving instance Bits (VkCommandPoolResetBitmask FlagMask)

deriving instance FiniteBits (VkCommandPoolResetBitmask FlagMask)

deriving instance Integral (VkCommandPoolResetBitmask FlagMask)

deriving instance Num (VkCommandPoolResetBitmask FlagMask)

deriving instance Bounded (VkCommandPoolResetBitmask FlagMask)

deriving instance Enum (VkCommandPoolResetBitmask FlagMask)

deriving instance Real (VkCommandPoolResetBitmask FlagMask)

instance Show (VkCommandPoolResetBitmask a) where
        showsPrec _ VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
          = showString "VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
        showsPrec p (VkCommandPoolResetBitmask x)
          = showParen (p >= 11)
              (showString "VkCommandPoolResetBitmask " . showsPrec 11 x)

instance Read (VkCommandPoolResetBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT",
                   pure VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandPoolResetBitmask") >>
                      (VkCommandPoolResetBitmask <$> step readPrec)))

-- | Release resources owned by the pool
--
--   bitpos = @0@
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT ::
        VkCommandPoolResetBitmask a

pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT =
        VkCommandPoolResetBitmask 1
