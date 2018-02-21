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
module Graphics.Vulkan.Types.Enum.VkColorComponentFlags
       (VkColorComponentBitmask(VkColorComponentBitmask,
                                VkColorComponentFlags, VkColorComponentFlagBits,
                                VK_COLOR_COMPONENT_R_BIT, VK_COLOR_COMPONENT_G_BIT,
                                VK_COLOR_COMPONENT_B_BIT, VK_COLOR_COMPONENT_A_BIT),
        VkColorComponentFlags, VkColorComponentFlagBits)
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

newtype VkColorComponentBitmask (a ::
                                   FlagType) = VkColorComponentBitmask VkFlags
                                                 deriving (Eq, Ord, Storable, Data, Generic)

type VkColorComponentFlags = VkColorComponentBitmask FlagMask

type VkColorComponentFlagBits = VkColorComponentBitmask FlagBit

pattern VkColorComponentFlagBits ::
        VkFlags -> VkColorComponentBitmask FlagBit

pattern VkColorComponentFlagBits n = VkColorComponentBitmask n

pattern VkColorComponentFlags ::
        VkFlags -> VkColorComponentBitmask FlagMask

pattern VkColorComponentFlags n = VkColorComponentBitmask n

deriving instance Bits (VkColorComponentBitmask FlagMask)

deriving instance FiniteBits (VkColorComponentBitmask FlagMask)

deriving instance Integral (VkColorComponentBitmask FlagMask)

deriving instance Num (VkColorComponentBitmask FlagMask)

deriving instance Bounded (VkColorComponentBitmask FlagMask)

deriving instance Enum (VkColorComponentBitmask FlagMask)

deriving instance Real (VkColorComponentBitmask FlagMask)

instance Show (VkColorComponentBitmask a) where
        showsPrec _ VK_COLOR_COMPONENT_R_BIT
          = showString "VK_COLOR_COMPONENT_R_BIT"
        showsPrec _ VK_COLOR_COMPONENT_G_BIT
          = showString "VK_COLOR_COMPONENT_G_BIT"
        showsPrec _ VK_COLOR_COMPONENT_B_BIT
          = showString "VK_COLOR_COMPONENT_B_BIT"
        showsPrec _ VK_COLOR_COMPONENT_A_BIT
          = showString "VK_COLOR_COMPONENT_A_BIT"
        showsPrec p (VkColorComponentBitmask x)
          = showParen (p >= 11)
              (showString "VkColorComponentBitmask " . showsPrec 11 x)

instance Read (VkColorComponentBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_COLOR_COMPONENT_R_BIT", pure VK_COLOR_COMPONENT_R_BIT),
                  ("VK_COLOR_COMPONENT_G_BIT", pure VK_COLOR_COMPONENT_G_BIT),
                  ("VK_COLOR_COMPONENT_B_BIT", pure VK_COLOR_COMPONENT_B_BIT),
                  ("VK_COLOR_COMPONENT_A_BIT", pure VK_COLOR_COMPONENT_A_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkColorComponentBitmask") >>
                      (VkColorComponentBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_COLOR_COMPONENT_R_BIT :: VkColorComponentBitmask a

pattern VK_COLOR_COMPONENT_R_BIT = VkColorComponentBitmask 1

-- | bitpos = @1@
pattern VK_COLOR_COMPONENT_G_BIT :: VkColorComponentBitmask a

pattern VK_COLOR_COMPONENT_G_BIT = VkColorComponentBitmask 2

-- | bitpos = @2@
pattern VK_COLOR_COMPONENT_B_BIT :: VkColorComponentBitmask a

pattern VK_COLOR_COMPONENT_B_BIT = VkColorComponentBitmask 4

-- | bitpos = @3@
pattern VK_COLOR_COMPONENT_A_BIT :: VkColorComponentBitmask a

pattern VK_COLOR_COMPONENT_A_BIT = VkColorComponentBitmask 8
