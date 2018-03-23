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
module Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlags
       (VkExternalMemoryFeatureBitmask(VkExternalMemoryFeatureBitmask,
                                       VkExternalMemoryFeatureFlags,
                                       VkExternalMemoryFeatureFlagBits,
                                       VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT,
                                       VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT,
                                       VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT),
        VkExternalMemoryFeatureFlags, VkExternalMemoryFeatureFlagBits)
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

newtype VkExternalMemoryFeatureBitmask (a ::
                                          FlagType) = VkExternalMemoryFeatureBitmask VkFlags
                                                        deriving (Eq, Ord, Storable, Data, Generic)

type VkExternalMemoryFeatureFlags =
     VkExternalMemoryFeatureBitmask FlagMask

type VkExternalMemoryFeatureFlagBits =
     VkExternalMemoryFeatureBitmask FlagBit

pattern VkExternalMemoryFeatureFlagBits ::
        VkFlags -> VkExternalMemoryFeatureBitmask FlagBit

pattern VkExternalMemoryFeatureFlagBits n =
        VkExternalMemoryFeatureBitmask n

pattern VkExternalMemoryFeatureFlags ::
        VkFlags -> VkExternalMemoryFeatureBitmask FlagMask

pattern VkExternalMemoryFeatureFlags n =
        VkExternalMemoryFeatureBitmask n

deriving instance Bits (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance
         FiniteBits (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance
         Integral (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance Num (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance Bounded (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance Enum (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance Real (VkExternalMemoryFeatureBitmask FlagMask)

instance Show (VkExternalMemoryFeatureBitmask a) where
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
          = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
          = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
          = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT"
        showsPrec p (VkExternalMemoryFeatureBitmask x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryFeatureBitmask " . showsPrec 11 x)

instance Read (VkExternalMemoryFeatureBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT",
                   pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT),
                  ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT",
                   pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT),
                  ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT",
                   pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryFeatureBitmask") >>
                      (VkExternalMemoryFeatureBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT ::
        VkExternalMemoryFeatureBitmask a

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT =
        VkExternalMemoryFeatureBitmask 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT ::
        VkExternalMemoryFeatureBitmask a

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT =
        VkExternalMemoryFeatureBitmask 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT ::
        VkExternalMemoryFeatureBitmask a

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT =
        VkExternalMemoryFeatureBitmask 4
