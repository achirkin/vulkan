{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.SubgroupFeatureFlags
       (VkSubgroupFeatureBitmask(VkSubgroupFeatureBitmask,
                                 VkSubgroupFeatureFlags, VkSubgroupFeatureFlagBits,
                                 VK_SUBGROUP_FEATURE_BASIC_BIT, VK_SUBGROUP_FEATURE_VOTE_BIT,
                                 VK_SUBGROUP_FEATURE_ARITHMETIC_BIT, VK_SUBGROUP_FEATURE_BALLOT_BIT,
                                 VK_SUBGROUP_FEATURE_SHUFFLE_BIT,
                                 VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT,
                                 VK_SUBGROUP_FEATURE_CLUSTERED_BIT, VK_SUBGROUP_FEATURE_QUAD_BIT),
        VkSubgroupFeatureFlags, VkSubgroupFeatureFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkSubgroupFeatureBitmask (a ::
                                    FlagType) = VkSubgroupFeatureBitmask VkFlags
                                                deriving (Eq, Ord, Storable)

type VkSubgroupFeatureFlags = VkSubgroupFeatureBitmask FlagMask

type VkSubgroupFeatureFlagBits = VkSubgroupFeatureBitmask FlagBit

pattern VkSubgroupFeatureFlagBits ::
        VkFlags -> VkSubgroupFeatureBitmask FlagBit

pattern VkSubgroupFeatureFlagBits n = VkSubgroupFeatureBitmask n

pattern VkSubgroupFeatureFlags ::
        VkFlags -> VkSubgroupFeatureBitmask FlagMask

pattern VkSubgroupFeatureFlags n = VkSubgroupFeatureBitmask n

deriving instance Bits (VkSubgroupFeatureBitmask FlagMask)

deriving instance FiniteBits (VkSubgroupFeatureBitmask FlagMask)

instance Show (VkSubgroupFeatureBitmask a) where
    showsPrec _ VK_SUBGROUP_FEATURE_BASIC_BIT
      = showString "VK_SUBGROUP_FEATURE_BASIC_BIT"
    showsPrec _ VK_SUBGROUP_FEATURE_VOTE_BIT
      = showString "VK_SUBGROUP_FEATURE_VOTE_BIT"
    showsPrec _ VK_SUBGROUP_FEATURE_ARITHMETIC_BIT
      = showString "VK_SUBGROUP_FEATURE_ARITHMETIC_BIT"
    showsPrec _ VK_SUBGROUP_FEATURE_BALLOT_BIT
      = showString "VK_SUBGROUP_FEATURE_BALLOT_BIT"
    showsPrec _ VK_SUBGROUP_FEATURE_SHUFFLE_BIT
      = showString "VK_SUBGROUP_FEATURE_SHUFFLE_BIT"
    showsPrec _ VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT
      = showString "VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT"
    showsPrec _ VK_SUBGROUP_FEATURE_CLUSTERED_BIT
      = showString "VK_SUBGROUP_FEATURE_CLUSTERED_BIT"
    showsPrec _ VK_SUBGROUP_FEATURE_QUAD_BIT
      = showString "VK_SUBGROUP_FEATURE_QUAD_BIT"
    showsPrec p (VkSubgroupFeatureBitmask x)
      = showParen (p >= 11)
          (showString "VkSubgroupFeatureBitmask " . showsPrec 11 x)

instance Read (VkSubgroupFeatureBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_SUBGROUP_FEATURE_BASIC_BIT",
               pure VK_SUBGROUP_FEATURE_BASIC_BIT),
              ("VK_SUBGROUP_FEATURE_VOTE_BIT",
               pure VK_SUBGROUP_FEATURE_VOTE_BIT),
              ("VK_SUBGROUP_FEATURE_ARITHMETIC_BIT",
               pure VK_SUBGROUP_FEATURE_ARITHMETIC_BIT),
              ("VK_SUBGROUP_FEATURE_BALLOT_BIT",
               pure VK_SUBGROUP_FEATURE_BALLOT_BIT),
              ("VK_SUBGROUP_FEATURE_SHUFFLE_BIT",
               pure VK_SUBGROUP_FEATURE_SHUFFLE_BIT),
              ("VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT",
               pure VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT),
              ("VK_SUBGROUP_FEATURE_CLUSTERED_BIT",
               pure VK_SUBGROUP_FEATURE_CLUSTERED_BIT),
              ("VK_SUBGROUP_FEATURE_QUAD_BIT",
               pure VK_SUBGROUP_FEATURE_QUAD_BIT)]
             +++
             prec 10
               (expectP (Ident "VkSubgroupFeatureBitmask") >>
                  (VkSubgroupFeatureBitmask <$> step readPrec)))

-- | Basic subgroup operations
--
--   bitpos = @0@
pattern VK_SUBGROUP_FEATURE_BASIC_BIT :: VkSubgroupFeatureBitmask a

pattern VK_SUBGROUP_FEATURE_BASIC_BIT = VkSubgroupFeatureBitmask 1

-- | Vote subgroup operations
--
--   bitpos = @1@
pattern VK_SUBGROUP_FEATURE_VOTE_BIT :: VkSubgroupFeatureBitmask a

pattern VK_SUBGROUP_FEATURE_VOTE_BIT = VkSubgroupFeatureBitmask 2

-- | Arithmetic subgroup operations
--
--   bitpos = @2@
pattern VK_SUBGROUP_FEATURE_ARITHMETIC_BIT ::
        VkSubgroupFeatureBitmask a

pattern VK_SUBGROUP_FEATURE_ARITHMETIC_BIT =
        VkSubgroupFeatureBitmask 4

-- | Ballot subgroup operations
--
--   bitpos = @3@
pattern VK_SUBGROUP_FEATURE_BALLOT_BIT ::
        VkSubgroupFeatureBitmask a

pattern VK_SUBGROUP_FEATURE_BALLOT_BIT = VkSubgroupFeatureBitmask 8

-- | Shuffle subgroup operations
--
--   bitpos = @4@
pattern VK_SUBGROUP_FEATURE_SHUFFLE_BIT ::
        VkSubgroupFeatureBitmask a

pattern VK_SUBGROUP_FEATURE_SHUFFLE_BIT =
        VkSubgroupFeatureBitmask 16

-- | Shuffle relative subgroup operations
--
--   bitpos = @5@
pattern VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT ::
        VkSubgroupFeatureBitmask a

pattern VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT =
        VkSubgroupFeatureBitmask 32

-- | Clustered subgroup operations
--
--   bitpos = @6@
pattern VK_SUBGROUP_FEATURE_CLUSTERED_BIT ::
        VkSubgroupFeatureBitmask a

pattern VK_SUBGROUP_FEATURE_CLUSTERED_BIT =
        VkSubgroupFeatureBitmask 64

-- | Quad subgroup operations
--
--   bitpos = @7@
pattern VK_SUBGROUP_FEATURE_QUAD_BIT :: VkSubgroupFeatureBitmask a

pattern VK_SUBGROUP_FEATURE_QUAD_BIT = VkSubgroupFeatureBitmask 128
