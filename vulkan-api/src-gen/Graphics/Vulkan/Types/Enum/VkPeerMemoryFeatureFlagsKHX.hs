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
module Graphics.Vulkan.Types.Enum.VkPeerMemoryFeatureFlagsKHX
       (VkPeerMemoryFeatureBitmaskKHX(VkPeerMemoryFeatureBitmaskKHX,
                                      VkPeerMemoryFeatureFlagsKHX, VkPeerMemoryFeatureFlagBitsKHX,
                                      VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX,
                                      VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX,
                                      VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX,
                                      VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX),
        VkPeerMemoryFeatureFlagsKHX, VkPeerMemoryFeatureFlagBitsKHX)
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

newtype VkPeerMemoryFeatureBitmaskKHX (a ::
                                         FlagType) = VkPeerMemoryFeatureBitmaskKHX VkFlags
                                                       deriving (Eq, Ord, Storable, Data, Generic)

type VkPeerMemoryFeatureFlagsKHX =
     VkPeerMemoryFeatureBitmaskKHX FlagMask

type VkPeerMemoryFeatureFlagBitsKHX =
     VkPeerMemoryFeatureBitmaskKHX FlagBit

pattern VkPeerMemoryFeatureFlagBitsKHX ::
        VkFlags -> VkPeerMemoryFeatureBitmaskKHX FlagBit

pattern VkPeerMemoryFeatureFlagBitsKHX n =
        VkPeerMemoryFeatureBitmaskKHX n

pattern VkPeerMemoryFeatureFlagsKHX ::
        VkFlags -> VkPeerMemoryFeatureBitmaskKHX FlagMask

pattern VkPeerMemoryFeatureFlagsKHX n =
        VkPeerMemoryFeatureBitmaskKHX n

deriving instance Bits (VkPeerMemoryFeatureBitmaskKHX FlagMask)

deriving instance
         FiniteBits (VkPeerMemoryFeatureBitmaskKHX FlagMask)

deriving instance Integral (VkPeerMemoryFeatureBitmaskKHX FlagMask)

deriving instance Num (VkPeerMemoryFeatureBitmaskKHX FlagMask)

deriving instance Bounded (VkPeerMemoryFeatureBitmaskKHX FlagMask)

deriving instance Enum (VkPeerMemoryFeatureBitmaskKHX FlagMask)

deriving instance Real (VkPeerMemoryFeatureBitmaskKHX FlagMask)

instance Show (VkPeerMemoryFeatureBitmaskKHX a) where
        showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX
          = showString "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX"
        showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX
          = showString "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX"
        showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX
          = showString "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX"
        showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX
          = showString "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX"
        showsPrec p (VkPeerMemoryFeatureBitmaskKHX x)
          = showParen (p >= 11)
              (showString "VkPeerMemoryFeatureBitmaskKHX " . showsPrec 11 x)

instance Read (VkPeerMemoryFeatureBitmaskKHX a) where
        readPrec
          = parens
              (choose
                 [("VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX",
                   pure VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX),
                  ("VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX",
                   pure VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX),
                  ("VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX",
                   pure VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX),
                  ("VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX",
                   pure VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX)]
                 +++
                 prec 10
                   (expectP (Ident "VkPeerMemoryFeatureBitmaskKHX") >>
                      (VkPeerMemoryFeatureBitmaskKHX <$> step readPrec)))

-- | Can read with vkCmdCopy commands
--
--   bitpos = @0@
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX ::
        VkPeerMemoryFeatureBitmaskKHX a

pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX =
        VkPeerMemoryFeatureBitmaskKHX 1

-- | Can write with vkCmdCopy commands
--
--   bitpos = @1@
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX ::
        VkPeerMemoryFeatureBitmaskKHX a

pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX =
        VkPeerMemoryFeatureBitmaskKHX 2

-- | Can read with any access type/command
--
--   bitpos = @2@
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX ::
        VkPeerMemoryFeatureBitmaskKHX a

pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX =
        VkPeerMemoryFeatureBitmaskKHX 4

-- | Can write with and access type/command
--
--   bitpos = @3@
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX ::
        VkPeerMemoryFeatureBitmaskKHX a

pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX =
        VkPeerMemoryFeatureBitmaskKHX 8
