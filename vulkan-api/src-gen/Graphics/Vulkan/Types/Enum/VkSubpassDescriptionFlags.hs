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
module Graphics.Vulkan.Types.Enum.VkSubpassDescriptionFlags
       (VkSubpassDescriptionBitmask(VkSubpassDescriptionBitmask,
                                    VkSubpassDescriptionFlags, VkSubpassDescriptionFlagBits),
        VkSubpassDescriptionFlags, VkSubpassDescriptionFlagBits)
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

newtype VkSubpassDescriptionBitmask (a ::
                                       FlagType) = VkSubpassDescriptionBitmask VkFlags
                                                     deriving (Eq, Ord, Storable, Data, Generic)

type VkSubpassDescriptionFlags =
     VkSubpassDescriptionBitmask FlagMask

type VkSubpassDescriptionFlagBits =
     VkSubpassDescriptionBitmask FlagBit

pattern VkSubpassDescriptionFlagBits ::
        VkFlags -> VkSubpassDescriptionBitmask FlagBit

pattern VkSubpassDescriptionFlagBits n =
        VkSubpassDescriptionBitmask n

pattern VkSubpassDescriptionFlags ::
        VkFlags -> VkSubpassDescriptionBitmask FlagMask

pattern VkSubpassDescriptionFlags n = VkSubpassDescriptionBitmask n

deriving instance Bits (VkSubpassDescriptionBitmask FlagMask)

deriving instance FiniteBits (VkSubpassDescriptionBitmask FlagMask)

deriving instance Integral (VkSubpassDescriptionBitmask FlagMask)

deriving instance Num (VkSubpassDescriptionBitmask FlagMask)

deriving instance Bounded (VkSubpassDescriptionBitmask FlagMask)

deriving instance Enum (VkSubpassDescriptionBitmask FlagMask)

deriving instance Real (VkSubpassDescriptionBitmask FlagMask)

instance Show (VkSubpassDescriptionBitmask a) where
        showsPrec p (VkSubpassDescriptionBitmask x)
          = showParen (p >= 11)
              (showString "VkSubpassDescriptionBitmask " . showsPrec 11 x)

instance Read (VkSubpassDescriptionBitmask a) where
        readPrec
          = parens
              (choose [] +++
                 prec 10
                   (expectP (Ident "VkSubpassDescriptionBitmask") >>
                      (VkSubpassDescriptionBitmask <$> step readPrec)))
