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
module Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlags
       (VkExternalFenceFeatureBitmask(VkExternalFenceFeatureBitmask,
                                      VkExternalFenceFeatureFlags, VkExternalFenceFeatureFlagBits,
                                      VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT,
                                      VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT),
        VkExternalFenceFeatureFlags, VkExternalFenceFeatureFlagBits)
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

newtype VkExternalFenceFeatureBitmask (a ::
                                         FlagType) = VkExternalFenceFeatureBitmask VkFlags
                                                       deriving (Eq, Ord, Storable, Data, Generic)

type VkExternalFenceFeatureFlags =
     VkExternalFenceFeatureBitmask FlagMask

type VkExternalFenceFeatureFlagBits =
     VkExternalFenceFeatureBitmask FlagBit

pattern VkExternalFenceFeatureFlagBits ::
        VkFlags -> VkExternalFenceFeatureBitmask FlagBit

pattern VkExternalFenceFeatureFlagBits n =
        VkExternalFenceFeatureBitmask n

pattern VkExternalFenceFeatureFlags ::
        VkFlags -> VkExternalFenceFeatureBitmask FlagMask

pattern VkExternalFenceFeatureFlags n =
        VkExternalFenceFeatureBitmask n

deriving instance Bits (VkExternalFenceFeatureBitmask FlagMask)

deriving instance
         FiniteBits (VkExternalFenceFeatureBitmask FlagMask)

deriving instance Integral (VkExternalFenceFeatureBitmask FlagMask)

deriving instance Num (VkExternalFenceFeatureBitmask FlagMask)

deriving instance Bounded (VkExternalFenceFeatureBitmask FlagMask)

deriving instance Enum (VkExternalFenceFeatureBitmask FlagMask)

deriving instance Real (VkExternalFenceFeatureBitmask FlagMask)

instance Show (VkExternalFenceFeatureBitmask a) where
        showsPrec _ VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
          = showString "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT"
        showsPrec _ VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
          = showString "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT"
        showsPrec p (VkExternalFenceFeatureBitmask x)
          = showParen (p >= 11)
              (showString "VkExternalFenceFeatureBitmask " . showsPrec 11 x)

instance Read (VkExternalFenceFeatureBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT",
                   pure VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT),
                  ("VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT",
                   pure VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalFenceFeatureBitmask") >>
                      (VkExternalFenceFeatureBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT ::
        VkExternalFenceFeatureBitmask a

pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT =
        VkExternalFenceFeatureBitmask 1

-- | bitpos = @1@
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT ::
        VkExternalFenceFeatureBitmask a

pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT =
        VkExternalFenceFeatureBitmask 2
