{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.PeerMemoryFeatureFlag
       (VkPeerMemoryFeatureFlagBitsKHR(..),
        VkPeerMemoryFeatureBitmask(VkPeerMemoryFeatureBitmask,
                                   VkPeerMemoryFeatureFlags, VkPeerMemoryFeatureFlagBits,
                                   VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT,
                                   VK_PEER_MEMORY_FEATURE_COPY_DST_BIT,
                                   VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT,
                                   VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT),
        VkPeerMemoryFeatureFlags, VkPeerMemoryFeatureFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkPeerMemoryFeatureFlagBitsKHR = VkPeerMemoryFeatureFlagBitsKHR VkFlags
                                         deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPeerMemoryFeatureFlagBitsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPeerMemoryFeatureFlagBitsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPeerMemoryFeatureBitmask (a ::
                                      FlagType) = VkPeerMemoryFeatureBitmask VkFlags
                                                  deriving (Eq, Ord, Storable)

type VkPeerMemoryFeatureFlags = VkPeerMemoryFeatureBitmask FlagMask

type VkPeerMemoryFeatureFlagBits =
     VkPeerMemoryFeatureBitmask FlagBit

pattern VkPeerMemoryFeatureFlagBits ::
        VkFlags -> VkPeerMemoryFeatureBitmask FlagBit

pattern VkPeerMemoryFeatureFlagBits n =
        VkPeerMemoryFeatureBitmask n

pattern VkPeerMemoryFeatureFlags ::
        VkFlags -> VkPeerMemoryFeatureBitmask FlagMask

pattern VkPeerMemoryFeatureFlags n = VkPeerMemoryFeatureBitmask n

deriving instance Bits (VkPeerMemoryFeatureBitmask FlagMask)

deriving instance FiniteBits (VkPeerMemoryFeatureBitmask FlagMask)

instance Show (VkPeerMemoryFeatureBitmask a) where
    showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
      = showString "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT"
    showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
      = showString "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT"
    showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
      = showString "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT"
    showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
      = showString "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT"
    showsPrec p (VkPeerMemoryFeatureBitmask x)
      = showParen (p >= 11)
          (showString "VkPeerMemoryFeatureBitmask " . showsPrec 11 x)

instance Read (VkPeerMemoryFeatureBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT",
               pure VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT),
              ("VK_PEER_MEMORY_FEATURE_COPY_DST_BIT",
               pure VK_PEER_MEMORY_FEATURE_COPY_DST_BIT),
              ("VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT",
               pure VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT),
              ("VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT",
               pure VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT)]
             +++
             prec 10
               (expectP (Ident "VkPeerMemoryFeatureBitmask") >>
                  (VkPeerMemoryFeatureBitmask <$> step readPrec)))

-- | Can read with vkCmdCopy commands
--
--   bitpos = @0@
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT ::
        VkPeerMemoryFeatureBitmask a

pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT =
        VkPeerMemoryFeatureBitmask 1

-- | Can write with vkCmdCopy commands
--
--   bitpos = @1@
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT ::
        VkPeerMemoryFeatureBitmask a

pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT =
        VkPeerMemoryFeatureBitmask 2

-- | Can read with any access type/command
--
--   bitpos = @2@
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT ::
        VkPeerMemoryFeatureBitmask a

pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT =
        VkPeerMemoryFeatureBitmask 4

-- | Can write with and access type/command
--
--   bitpos = @3@
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT ::
        VkPeerMemoryFeatureBitmask a

pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT =
        VkPeerMemoryFeatureBitmask 8
