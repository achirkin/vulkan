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
module Graphics.Vulkan.Types.Enum.VkExternalSemaphoreFeatureFlags
       (VkExternalSemaphoreFeatureBitmask(VkExternalSemaphoreFeatureBitmask,
                                          VkExternalSemaphoreFeatureFlags,
                                          VkExternalSemaphoreFeatureFlagBits,
                                          VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT,
                                          VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT),
        VkExternalSemaphoreFeatureFlags,
        VkExternalSemaphoreFeatureFlagBits)
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

newtype VkExternalSemaphoreFeatureBitmask (a ::
                                             FlagType) = VkExternalSemaphoreFeatureBitmask VkFlags
                                                           deriving (Eq, Ord, Storable, Data,
                                                                     Generic)

type VkExternalSemaphoreFeatureFlags =
     VkExternalSemaphoreFeatureBitmask FlagMask

type VkExternalSemaphoreFeatureFlagBits =
     VkExternalSemaphoreFeatureBitmask FlagBit

pattern VkExternalSemaphoreFeatureFlagBits ::
        VkFlags -> VkExternalSemaphoreFeatureBitmask FlagBit

pattern VkExternalSemaphoreFeatureFlagBits n =
        VkExternalSemaphoreFeatureBitmask n

pattern VkExternalSemaphoreFeatureFlags ::
        VkFlags -> VkExternalSemaphoreFeatureBitmask FlagMask

pattern VkExternalSemaphoreFeatureFlags n =
        VkExternalSemaphoreFeatureBitmask n

deriving instance Bits (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance
         FiniteBits (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance
         Integral (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance Num (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance
         Bounded (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance Enum (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance Real (VkExternalSemaphoreFeatureBitmask FlagMask)

instance Show (VkExternalSemaphoreFeatureBitmask a) where
        showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
          = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
          = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT"
        showsPrec p (VkExternalSemaphoreFeatureBitmask x)
          = showParen (p >= 11)
              (showString "VkExternalSemaphoreFeatureBitmask " . showsPrec 11 x)

instance Read (VkExternalSemaphoreFeatureBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT",
                   pure VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT),
                  ("VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT",
                   pure VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalSemaphoreFeatureBitmask") >>
                      (VkExternalSemaphoreFeatureBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT ::
        VkExternalSemaphoreFeatureBitmask a

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT =
        VkExternalSemaphoreFeatureBitmask 1

-- | bitpos = @1@
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT ::
        VkExternalSemaphoreFeatureBitmask a

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT =
        VkExternalSemaphoreFeatureBitmask 2
