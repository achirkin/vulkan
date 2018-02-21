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
module Graphics.Vulkan.Types.Enum.VkQueryControlFlags
       (VkQueryControlBitmask(VkQueryControlBitmask, VkQueryControlFlags,
                              VkQueryControlFlagBits, VK_QUERY_CONTROL_PRECISE_BIT),
        VkQueryControlFlags, VkQueryControlFlagBits)
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

newtype VkQueryControlBitmask (a ::
                                 FlagType) = VkQueryControlBitmask VkFlags
                                               deriving (Eq, Ord, Storable, Data, Generic)

type VkQueryControlFlags = VkQueryControlBitmask FlagMask

type VkQueryControlFlagBits = VkQueryControlBitmask FlagBit

pattern VkQueryControlFlagBits ::
        VkFlags -> VkQueryControlBitmask FlagBit

pattern VkQueryControlFlagBits n = VkQueryControlBitmask n

pattern VkQueryControlFlags ::
        VkFlags -> VkQueryControlBitmask FlagMask

pattern VkQueryControlFlags n = VkQueryControlBitmask n

deriving instance Bits (VkQueryControlBitmask FlagMask)

deriving instance FiniteBits (VkQueryControlBitmask FlagMask)

deriving instance Integral (VkQueryControlBitmask FlagMask)

deriving instance Num (VkQueryControlBitmask FlagMask)

deriving instance Bounded (VkQueryControlBitmask FlagMask)

deriving instance Enum (VkQueryControlBitmask FlagMask)

deriving instance Real (VkQueryControlBitmask FlagMask)

instance Show (VkQueryControlBitmask a) where
        showsPrec _ VK_QUERY_CONTROL_PRECISE_BIT
          = showString "VK_QUERY_CONTROL_PRECISE_BIT"
        showsPrec p (VkQueryControlBitmask x)
          = showParen (p >= 11)
              (showString "VkQueryControlBitmask " . showsPrec 11 x)

instance Read (VkQueryControlBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_CONTROL_PRECISE_BIT",
                   pure VK_QUERY_CONTROL_PRECISE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryControlBitmask") >>
                      (VkQueryControlBitmask <$> step readPrec)))

-- | Require precise results to be collected by the query
--
--   bitpos = @0@
pattern VK_QUERY_CONTROL_PRECISE_BIT :: VkQueryControlBitmask a

pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlBitmask 1
