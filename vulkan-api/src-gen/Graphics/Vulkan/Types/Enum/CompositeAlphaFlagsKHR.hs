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
module Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
       (VkCompositeAlphaBitmaskKHR(VkCompositeAlphaBitmaskKHR,
                                   VkCompositeAlphaFlagsKHR, VkCompositeAlphaFlagBitsKHR,
                                   VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                   VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR,
                                   VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR,
                                   VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR),
        VkCompositeAlphaFlagsKHR, VkCompositeAlphaFlagBitsKHR)
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

newtype VkCompositeAlphaBitmaskKHR (a ::
                                      FlagType) = VkCompositeAlphaBitmaskKHR VkFlags
                                                    deriving (Eq, Ord, Storable, Data, Generic)

type VkCompositeAlphaFlagsKHR = VkCompositeAlphaBitmaskKHR FlagMask

type VkCompositeAlphaFlagBitsKHR =
     VkCompositeAlphaBitmaskKHR FlagBit

pattern VkCompositeAlphaFlagBitsKHR ::
        VkFlags -> VkCompositeAlphaBitmaskKHR FlagBit

pattern VkCompositeAlphaFlagBitsKHR n =
        VkCompositeAlphaBitmaskKHR n

pattern VkCompositeAlphaFlagsKHR ::
        VkFlags -> VkCompositeAlphaBitmaskKHR FlagMask

pattern VkCompositeAlphaFlagsKHR n = VkCompositeAlphaBitmaskKHR n

deriving instance Bits (VkCompositeAlphaBitmaskKHR FlagMask)

deriving instance FiniteBits (VkCompositeAlphaBitmaskKHR FlagMask)

deriving instance Integral (VkCompositeAlphaBitmaskKHR FlagMask)

deriving instance Num (VkCompositeAlphaBitmaskKHR FlagMask)

deriving instance Bounded (VkCompositeAlphaBitmaskKHR FlagMask)

deriving instance Enum (VkCompositeAlphaBitmaskKHR FlagMask)

deriving instance Real (VkCompositeAlphaBitmaskKHR FlagMask)

instance Show (VkCompositeAlphaBitmaskKHR a) where
        showsPrec _ VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
          = showString "VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
        showsPrec _ VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
          = showString "VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
        showsPrec _ VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
          = showString "VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
        showsPrec _ VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
          = showString "VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR"
        showsPrec p (VkCompositeAlphaBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkCompositeAlphaBitmaskKHR " . showsPrec 11 x)

instance Read (VkCompositeAlphaBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR",
                   pure VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR),
                  ("VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR",
                   pure VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR),
                  ("VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR",
                   pure VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR),
                  ("VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR",
                   pure VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkCompositeAlphaBitmaskKHR") >>
                      (VkCompositeAlphaBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR ::
        VkCompositeAlphaBitmaskKHR a

pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR =
        VkCompositeAlphaBitmaskKHR 1

-- | bitpos = @1@
pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR ::
        VkCompositeAlphaBitmaskKHR a

pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR =
        VkCompositeAlphaBitmaskKHR 2

-- | bitpos = @2@
pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR ::
        VkCompositeAlphaBitmaskKHR a

pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR =
        VkCompositeAlphaBitmaskKHR 4

-- | bitpos = @3@
pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR ::
        VkCompositeAlphaBitmaskKHR a

pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR =
        VkCompositeAlphaBitmaskKHR 8
