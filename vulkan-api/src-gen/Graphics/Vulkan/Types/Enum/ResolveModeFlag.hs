{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.ResolveModeFlag
       (VkResolveModeFlagBitsKHR(..),
        VkResolveModeBitmask(VkResolveModeBitmask, VkResolveModeFlags,
                             VkResolveModeFlagBits, VK_RESOLVE_MODE_NONE,
                             VK_RESOLVE_MODE_SAMPLE_ZERO_BIT, VK_RESOLVE_MODE_AVERAGE_BIT,
                             VK_RESOLVE_MODE_MIN_BIT, VK_RESOLVE_MODE_MAX_BIT),
        VkResolveModeFlags, VkResolveModeFlagBits)
       where
import Data.Bits                       (Bits, FiniteBits)
import Data.Coerce                     (coerce)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

newtype VkResolveModeFlagBitsKHR = VkResolveModeFlagBitsKHR VkFlags
                                   deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkResolveModeFlagBitsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkResolveModeFlagBitsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkResolveModeBitmask (a ::
                                FlagType) = VkResolveModeBitmask VkFlags
                                            deriving (Eq, Ord, Storable)

type VkResolveModeFlags = VkResolveModeBitmask FlagMask

type VkResolveModeFlagBits = VkResolveModeBitmask FlagBit

pattern VkResolveModeFlagBits ::
        VkFlags -> VkResolveModeBitmask FlagBit

pattern VkResolveModeFlagBits n = VkResolveModeBitmask n

pattern VkResolveModeFlags ::
        VkFlags -> VkResolveModeBitmask FlagMask

pattern VkResolveModeFlags n = VkResolveModeBitmask n

deriving instance Bits (VkResolveModeBitmask FlagMask)

deriving instance FiniteBits (VkResolveModeBitmask FlagMask)

instance Show (VkResolveModeBitmask a) where
    showsPrec _ VK_RESOLVE_MODE_NONE
      = showString "VK_RESOLVE_MODE_NONE"
    showsPrec _ VK_RESOLVE_MODE_SAMPLE_ZERO_BIT
      = showString "VK_RESOLVE_MODE_SAMPLE_ZERO_BIT"
    showsPrec _ VK_RESOLVE_MODE_AVERAGE_BIT
      = showString "VK_RESOLVE_MODE_AVERAGE_BIT"
    showsPrec _ VK_RESOLVE_MODE_MIN_BIT
      = showString "VK_RESOLVE_MODE_MIN_BIT"
    showsPrec _ VK_RESOLVE_MODE_MAX_BIT
      = showString "VK_RESOLVE_MODE_MAX_BIT"
    showsPrec p (VkResolveModeBitmask x)
      = showParen (p >= 11)
          (showString "VkResolveModeBitmask " . showsPrec 11 x)

instance Read (VkResolveModeBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_RESOLVE_MODE_NONE", pure VK_RESOLVE_MODE_NONE),
              ("VK_RESOLVE_MODE_SAMPLE_ZERO_BIT",
               pure VK_RESOLVE_MODE_SAMPLE_ZERO_BIT),
              ("VK_RESOLVE_MODE_AVERAGE_BIT", pure VK_RESOLVE_MODE_AVERAGE_BIT),
              ("VK_RESOLVE_MODE_MIN_BIT", pure VK_RESOLVE_MODE_MIN_BIT),
              ("VK_RESOLVE_MODE_MAX_BIT", pure VK_RESOLVE_MODE_MAX_BIT)]
             +++
             prec 10
               (expectP (Ident "VkResolveModeBitmask") >>
                  (VkResolveModeBitmask <$> step readPrec)))

pattern VK_RESOLVE_MODE_NONE :: VkResolveModeBitmask a

pattern VK_RESOLVE_MODE_NONE = VkResolveModeBitmask 0

-- | bitpos = @0@
pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT :: VkResolveModeBitmask a

pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT = VkResolveModeBitmask 1

-- | bitpos = @1@
pattern VK_RESOLVE_MODE_AVERAGE_BIT :: VkResolveModeBitmask a

pattern VK_RESOLVE_MODE_AVERAGE_BIT = VkResolveModeBitmask 2

-- | bitpos = @2@
pattern VK_RESOLVE_MODE_MIN_BIT :: VkResolveModeBitmask a

pattern VK_RESOLVE_MODE_MIN_BIT = VkResolveModeBitmask 4

-- | bitpos = @3@
pattern VK_RESOLVE_MODE_MAX_BIT :: VkResolveModeBitmask a

pattern VK_RESOLVE_MODE_MAX_BIT = VkResolveModeBitmask 8
