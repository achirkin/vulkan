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
module Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagsNV
       (VkExternalMemoryFeatureBitmaskNV(VkExternalMemoryFeatureBitmaskNV,
                                         VkExternalMemoryFeatureFlagsNV,
                                         VkExternalMemoryFeatureFlagBitsNV,
                                         VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV,
                                         VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV,
                                         VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV),
        VkExternalMemoryFeatureFlagsNV, VkExternalMemoryFeatureFlagBitsNV)
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

newtype VkExternalMemoryFeatureBitmaskNV (a ::
                                            FlagType) = VkExternalMemoryFeatureBitmaskNV VkFlags
                                                          deriving (Eq, Ord, Storable, Data,
                                                                    Generic)

type VkExternalMemoryFeatureFlagsNV =
     VkExternalMemoryFeatureBitmaskNV FlagMask

type VkExternalMemoryFeatureFlagBitsNV =
     VkExternalMemoryFeatureBitmaskNV FlagBit

pattern VkExternalMemoryFeatureFlagBitsNV ::
        VkFlags -> VkExternalMemoryFeatureBitmaskNV FlagBit

pattern VkExternalMemoryFeatureFlagBitsNV n =
        VkExternalMemoryFeatureBitmaskNV n

pattern VkExternalMemoryFeatureFlagsNV ::
        VkFlags -> VkExternalMemoryFeatureBitmaskNV FlagMask

pattern VkExternalMemoryFeatureFlagsNV n =
        VkExternalMemoryFeatureBitmaskNV n

deriving instance Bits (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance
         FiniteBits (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance
         Integral (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance Num (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance
         Bounded (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance Enum (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance Real (VkExternalMemoryFeatureBitmaskNV FlagMask)

instance Show (VkExternalMemoryFeatureBitmaskNV a) where
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV"
        showsPrec p (VkExternalMemoryFeatureBitmaskNV x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryFeatureBitmaskNV " . showsPrec 11 x)

instance Read (VkExternalMemoryFeatureBitmaskNV a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryFeatureBitmaskNV") >>
                      (VkExternalMemoryFeatureBitmaskNV <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV ::
        VkExternalMemoryFeatureBitmaskNV a

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV =
        VkExternalMemoryFeatureBitmaskNV 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV ::
        VkExternalMemoryFeatureBitmaskNV a

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV =
        VkExternalMemoryFeatureBitmaskNV 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV ::
        VkExternalMemoryFeatureBitmaskNV a

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV =
        VkExternalMemoryFeatureBitmaskNV 4
