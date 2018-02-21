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
module Graphics.Vulkan.Types.Enum.VkExternalSemaphoreFeatureFlagsKHR
       (VkExternalSemaphoreFeatureBitmaskKHR(VkExternalSemaphoreFeatureBitmaskKHR,
                                             VkExternalSemaphoreFeatureFlagsKHR,
                                             VkExternalSemaphoreFeatureFlagBitsKHR,
                                             VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR,
                                             VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR),
        VkExternalSemaphoreFeatureFlagsKHR,
        VkExternalSemaphoreFeatureFlagBitsKHR)
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

newtype VkExternalSemaphoreFeatureBitmaskKHR (a ::
                                                FlagType) = VkExternalSemaphoreFeatureBitmaskKHR VkFlags
                                                              deriving (Eq, Ord, Storable, Data,
                                                                        Generic)

type VkExternalSemaphoreFeatureFlagsKHR =
     VkExternalSemaphoreFeatureBitmaskKHR FlagMask

type VkExternalSemaphoreFeatureFlagBitsKHR =
     VkExternalSemaphoreFeatureBitmaskKHR FlagBit

pattern VkExternalSemaphoreFeatureFlagBitsKHR ::
        VkFlags -> VkExternalSemaphoreFeatureBitmaskKHR FlagBit

pattern VkExternalSemaphoreFeatureFlagBitsKHR n =
        VkExternalSemaphoreFeatureBitmaskKHR n

pattern VkExternalSemaphoreFeatureFlagsKHR ::
        VkFlags -> VkExternalSemaphoreFeatureBitmaskKHR FlagMask

pattern VkExternalSemaphoreFeatureFlagsKHR n =
        VkExternalSemaphoreFeatureBitmaskKHR n

deriving instance
         Bits (VkExternalSemaphoreFeatureBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkExternalSemaphoreFeatureBitmaskKHR FlagMask)

deriving instance
         Integral (VkExternalSemaphoreFeatureBitmaskKHR FlagMask)

deriving instance
         Num (VkExternalSemaphoreFeatureBitmaskKHR FlagMask)

deriving instance
         Bounded (VkExternalSemaphoreFeatureBitmaskKHR FlagMask)

deriving instance
         Enum (VkExternalSemaphoreFeatureBitmaskKHR FlagMask)

deriving instance
         Real (VkExternalSemaphoreFeatureBitmaskKHR FlagMask)

instance Show (VkExternalSemaphoreFeatureBitmaskKHR a) where
        showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR
          = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR"
        showsPrec p (VkExternalSemaphoreFeatureBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkExternalSemaphoreFeatureBitmaskKHR " .
                 showsPrec 11 x)

instance Read (VkExternalSemaphoreFeatureBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR),
                  ("VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalSemaphoreFeatureBitmaskKHR") >>
                      (VkExternalSemaphoreFeatureBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR ::
        VkExternalSemaphoreFeatureBitmaskKHR a

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR =
        VkExternalSemaphoreFeatureBitmaskKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR ::
        VkExternalSemaphoreFeatureBitmaskKHR a

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR =
        VkExternalSemaphoreFeatureBitmaskKHR 2
