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
module Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagsKHR
       (VkExternalMemoryFeatureBitmaskKHR(VkExternalMemoryFeatureBitmaskKHR,
                                          VkExternalMemoryFeatureFlagsKHR,
                                          VkExternalMemoryFeatureFlagBitsKHR,
                                          VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR,
                                          VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR,
                                          VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR),
        VkExternalMemoryFeatureFlagsKHR,
        VkExternalMemoryFeatureFlagBitsKHR)
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

newtype VkExternalMemoryFeatureBitmaskKHR (a ::
                                             FlagType) = VkExternalMemoryFeatureBitmaskKHR VkFlags
                                                           deriving (Eq, Ord, Storable, Data,
                                                                     Generic)

type VkExternalMemoryFeatureFlagsKHR =
     VkExternalMemoryFeatureBitmaskKHR FlagMask

type VkExternalMemoryFeatureFlagBitsKHR =
     VkExternalMemoryFeatureBitmaskKHR FlagBit

pattern VkExternalMemoryFeatureFlagBitsKHR ::
        VkFlags -> VkExternalMemoryFeatureBitmaskKHR FlagBit

pattern VkExternalMemoryFeatureFlagBitsKHR n =
        VkExternalMemoryFeatureBitmaskKHR n

pattern VkExternalMemoryFeatureFlagsKHR ::
        VkFlags -> VkExternalMemoryFeatureBitmaskKHR FlagMask

pattern VkExternalMemoryFeatureFlagsKHR n =
        VkExternalMemoryFeatureBitmaskKHR n

deriving instance Bits (VkExternalMemoryFeatureBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkExternalMemoryFeatureBitmaskKHR FlagMask)

deriving instance
         Integral (VkExternalMemoryFeatureBitmaskKHR FlagMask)

deriving instance Num (VkExternalMemoryFeatureBitmaskKHR FlagMask)

deriving instance
         Bounded (VkExternalMemoryFeatureBitmaskKHR FlagMask)

deriving instance Enum (VkExternalMemoryFeatureBitmaskKHR FlagMask)

deriving instance Real (VkExternalMemoryFeatureBitmaskKHR FlagMask)

instance Show (VkExternalMemoryFeatureBitmaskKHR a) where
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR"
        showsPrec p (VkExternalMemoryFeatureBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryFeatureBitmaskKHR " . showsPrec 11 x)

instance Read (VkExternalMemoryFeatureBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryFeatureBitmaskKHR") >>
                      (VkExternalMemoryFeatureBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR ::
        VkExternalMemoryFeatureBitmaskKHR a

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR =
        VkExternalMemoryFeatureBitmaskKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR ::
        VkExternalMemoryFeatureBitmaskKHR a

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR =
        VkExternalMemoryFeatureBitmaskKHR 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR ::
        VkExternalMemoryFeatureBitmaskKHR a

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR =
        VkExternalMemoryFeatureBitmaskKHR 4
