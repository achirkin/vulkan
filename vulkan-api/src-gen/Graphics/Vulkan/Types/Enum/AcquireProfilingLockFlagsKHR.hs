{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.AcquireProfilingLockFlagsKHR
       (VkAcquireProfilingLockBitmaskKHR(VkAcquireProfilingLockBitmaskKHR,
                                         VkAcquireProfilingLockFlagsKHR,
                                         VkAcquireProfilingLockFlagBitsKHR),
        VkAcquireProfilingLockFlagsKHR, VkAcquireProfilingLockFlagBitsKHR)
       where
import Data.Bits                       (Bits, FiniteBits)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

newtype VkAcquireProfilingLockBitmaskKHR (a ::
                                            FlagType) = VkAcquireProfilingLockBitmaskKHR VkFlags
                                                        deriving (Eq, Ord, Storable)

type VkAcquireProfilingLockFlagsKHR =
     VkAcquireProfilingLockBitmaskKHR FlagMask

type VkAcquireProfilingLockFlagBitsKHR =
     VkAcquireProfilingLockBitmaskKHR FlagBit

pattern VkAcquireProfilingLockFlagBitsKHR ::
        VkFlags -> VkAcquireProfilingLockBitmaskKHR FlagBit

pattern VkAcquireProfilingLockFlagBitsKHR n =
        VkAcquireProfilingLockBitmaskKHR n

pattern VkAcquireProfilingLockFlagsKHR ::
        VkFlags -> VkAcquireProfilingLockBitmaskKHR FlagMask

pattern VkAcquireProfilingLockFlagsKHR n =
        VkAcquireProfilingLockBitmaskKHR n

deriving instance Bits (VkAcquireProfilingLockBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkAcquireProfilingLockBitmaskKHR FlagMask)

instance Show (VkAcquireProfilingLockBitmaskKHR a) where
    showsPrec p (VkAcquireProfilingLockBitmaskKHR x)
      = showParen (p >= 11)
          (showString "VkAcquireProfilingLockBitmaskKHR " . showsPrec 11 x)

instance Read (VkAcquireProfilingLockBitmaskKHR a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkAcquireProfilingLockBitmaskKHR") >>
                  (VkAcquireProfilingLockBitmaskKHR <$> step readPrec)))
