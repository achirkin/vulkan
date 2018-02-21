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
module Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlagsKHR
       (VkExternalFenceFeatureBitmaskKHR(VkExternalFenceFeatureBitmaskKHR,
                                         VkExternalFenceFeatureFlagsKHR,
                                         VkExternalFenceFeatureFlagBitsKHR,
                                         VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR,
                                         VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR),
        VkExternalFenceFeatureFlagsKHR, VkExternalFenceFeatureFlagBitsKHR)
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

newtype VkExternalFenceFeatureBitmaskKHR (a ::
                                            FlagType) = VkExternalFenceFeatureBitmaskKHR VkFlags
                                                          deriving (Eq, Ord, Storable, Data,
                                                                    Generic)

type VkExternalFenceFeatureFlagsKHR =
     VkExternalFenceFeatureBitmaskKHR FlagMask

type VkExternalFenceFeatureFlagBitsKHR =
     VkExternalFenceFeatureBitmaskKHR FlagBit

pattern VkExternalFenceFeatureFlagBitsKHR ::
        VkFlags -> VkExternalFenceFeatureBitmaskKHR FlagBit

pattern VkExternalFenceFeatureFlagBitsKHR n =
        VkExternalFenceFeatureBitmaskKHR n

pattern VkExternalFenceFeatureFlagsKHR ::
        VkFlags -> VkExternalFenceFeatureBitmaskKHR FlagMask

pattern VkExternalFenceFeatureFlagsKHR n =
        VkExternalFenceFeatureBitmaskKHR n

deriving instance Bits (VkExternalFenceFeatureBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkExternalFenceFeatureBitmaskKHR FlagMask)

deriving instance
         Integral (VkExternalFenceFeatureBitmaskKHR FlagMask)

deriving instance Num (VkExternalFenceFeatureBitmaskKHR FlagMask)

deriving instance
         Bounded (VkExternalFenceFeatureBitmaskKHR FlagMask)

deriving instance Enum (VkExternalFenceFeatureBitmaskKHR FlagMask)

deriving instance Real (VkExternalFenceFeatureBitmaskKHR FlagMask)

instance Show (VkExternalFenceFeatureBitmaskKHR a) where
        showsPrec _ VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR"
        showsPrec _ VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR"
        showsPrec p (VkExternalFenceFeatureBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkExternalFenceFeatureBitmaskKHR " . showsPrec 11 x)

instance Read (VkExternalFenceFeatureBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR),
                  ("VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalFenceFeatureBitmaskKHR") >>
                      (VkExternalFenceFeatureBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR ::
        VkExternalFenceFeatureBitmaskKHR a

pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR =
        VkExternalFenceFeatureBitmaskKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR ::
        VkExternalFenceFeatureBitmaskKHR a

pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR =
        VkExternalFenceFeatureBitmaskKHR 2
