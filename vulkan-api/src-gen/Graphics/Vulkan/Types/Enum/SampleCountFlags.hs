{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.SampleCountFlags
       (VkSampleCountBitmask(VkSampleCountBitmask, VkSampleCountFlags,
                             VkSampleCountFlagBits, VK_SAMPLE_COUNT_1_BIT,
                             VK_SAMPLE_COUNT_2_BIT, VK_SAMPLE_COUNT_4_BIT,
                             VK_SAMPLE_COUNT_8_BIT, VK_SAMPLE_COUNT_16_BIT,
                             VK_SAMPLE_COUNT_32_BIT, VK_SAMPLE_COUNT_64_BIT),
        VkSampleCountFlags, VkSampleCountFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkSampleCountBitmask (a ::
                                FlagType) = VkSampleCountBitmask VkFlags
                                            deriving (Eq, Ord, Storable)

type VkSampleCountFlags = VkSampleCountBitmask FlagMask

type VkSampleCountFlagBits = VkSampleCountBitmask FlagBit

pattern VkSampleCountFlagBits ::
        VkFlags -> VkSampleCountBitmask FlagBit

pattern VkSampleCountFlagBits n = VkSampleCountBitmask n

pattern VkSampleCountFlags ::
        VkFlags -> VkSampleCountBitmask FlagMask

pattern VkSampleCountFlags n = VkSampleCountBitmask n

deriving instance Bits (VkSampleCountBitmask FlagMask)

deriving instance FiniteBits (VkSampleCountBitmask FlagMask)

instance Show (VkSampleCountBitmask a) where
    showsPrec _ VK_SAMPLE_COUNT_1_BIT
      = showString "VK_SAMPLE_COUNT_1_BIT"
    showsPrec _ VK_SAMPLE_COUNT_2_BIT
      = showString "VK_SAMPLE_COUNT_2_BIT"
    showsPrec _ VK_SAMPLE_COUNT_4_BIT
      = showString "VK_SAMPLE_COUNT_4_BIT"
    showsPrec _ VK_SAMPLE_COUNT_8_BIT
      = showString "VK_SAMPLE_COUNT_8_BIT"
    showsPrec _ VK_SAMPLE_COUNT_16_BIT
      = showString "VK_SAMPLE_COUNT_16_BIT"
    showsPrec _ VK_SAMPLE_COUNT_32_BIT
      = showString "VK_SAMPLE_COUNT_32_BIT"
    showsPrec _ VK_SAMPLE_COUNT_64_BIT
      = showString "VK_SAMPLE_COUNT_64_BIT"
    showsPrec p (VkSampleCountBitmask x)
      = showParen (p >= 11)
          (showString "VkSampleCountBitmask " . showsPrec 11 x)

instance Read (VkSampleCountBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_SAMPLE_COUNT_1_BIT", pure VK_SAMPLE_COUNT_1_BIT),
              ("VK_SAMPLE_COUNT_2_BIT", pure VK_SAMPLE_COUNT_2_BIT),
              ("VK_SAMPLE_COUNT_4_BIT", pure VK_SAMPLE_COUNT_4_BIT),
              ("VK_SAMPLE_COUNT_8_BIT", pure VK_SAMPLE_COUNT_8_BIT),
              ("VK_SAMPLE_COUNT_16_BIT", pure VK_SAMPLE_COUNT_16_BIT),
              ("VK_SAMPLE_COUNT_32_BIT", pure VK_SAMPLE_COUNT_32_BIT),
              ("VK_SAMPLE_COUNT_64_BIT", pure VK_SAMPLE_COUNT_64_BIT)]
             +++
             prec 10
               (expectP (Ident "VkSampleCountBitmask") >>
                  (VkSampleCountBitmask <$> step readPrec)))

-- | Sample count 1 supported
--
--   bitpos = @0@
pattern VK_SAMPLE_COUNT_1_BIT :: VkSampleCountBitmask a

pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountBitmask 1

-- | Sample count 2 supported
--
--   bitpos = @1@
pattern VK_SAMPLE_COUNT_2_BIT :: VkSampleCountBitmask a

pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountBitmask 2

-- | Sample count 4 supported
--
--   bitpos = @2@
pattern VK_SAMPLE_COUNT_4_BIT :: VkSampleCountBitmask a

pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountBitmask 4

-- | Sample count 8 supported
--
--   bitpos = @3@
pattern VK_SAMPLE_COUNT_8_BIT :: VkSampleCountBitmask a

pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountBitmask 8

-- | Sample count 16 supported
--
--   bitpos = @4@
pattern VK_SAMPLE_COUNT_16_BIT :: VkSampleCountBitmask a

pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountBitmask 16

-- | Sample count 32 supported
--
--   bitpos = @5@
pattern VK_SAMPLE_COUNT_32_BIT :: VkSampleCountBitmask a

pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountBitmask 32

-- | Sample count 64 supported
--
--   bitpos = @6@
pattern VK_SAMPLE_COUNT_64_BIT :: VkSampleCountBitmask a

pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountBitmask 64
