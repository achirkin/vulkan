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
module Graphics.Vulkan.Types.Enum.VkFenceCreateFlags
       (VkFenceCreateBitmask(VkFenceCreateBitmask, VkFenceCreateFlags,
                             VkFenceCreateFlagBits, VK_FENCE_CREATE_SIGNALED_BIT),
        VkFenceCreateFlags, VkFenceCreateFlagBits)
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

newtype VkFenceCreateBitmask (a ::
                                FlagType) = VkFenceCreateBitmask VkFlags
                                              deriving (Eq, Ord, Storable, Data, Generic)

type VkFenceCreateFlags = VkFenceCreateBitmask FlagMask

type VkFenceCreateFlagBits = VkFenceCreateBitmask FlagBit

pattern VkFenceCreateFlagBits ::
        VkFlags -> VkFenceCreateBitmask FlagBit

pattern VkFenceCreateFlagBits n = VkFenceCreateBitmask n

pattern VkFenceCreateFlags ::
        VkFlags -> VkFenceCreateBitmask FlagMask

pattern VkFenceCreateFlags n = VkFenceCreateBitmask n

deriving instance Bits (VkFenceCreateBitmask FlagMask)

deriving instance FiniteBits (VkFenceCreateBitmask FlagMask)

deriving instance Integral (VkFenceCreateBitmask FlagMask)

deriving instance Num (VkFenceCreateBitmask FlagMask)

deriving instance Bounded (VkFenceCreateBitmask FlagMask)

deriving instance Enum (VkFenceCreateBitmask FlagMask)

deriving instance Real (VkFenceCreateBitmask FlagMask)

instance Show (VkFenceCreateBitmask a) where
        showsPrec _ VK_FENCE_CREATE_SIGNALED_BIT
          = showString "VK_FENCE_CREATE_SIGNALED_BIT"
        showsPrec p (VkFenceCreateBitmask x)
          = showParen (p >= 11)
              (showString "VkFenceCreateBitmask " . showsPrec 11 x)

instance Read (VkFenceCreateBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_FENCE_CREATE_SIGNALED_BIT",
                   pure VK_FENCE_CREATE_SIGNALED_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkFenceCreateBitmask") >>
                      (VkFenceCreateBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_FENCE_CREATE_SIGNALED_BIT :: VkFenceCreateBitmask a

pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateBitmask 1
