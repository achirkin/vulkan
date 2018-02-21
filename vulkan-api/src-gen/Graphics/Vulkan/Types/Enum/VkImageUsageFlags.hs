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
module Graphics.Vulkan.Types.Enum.VkImageUsageFlags
       (VkImageUsageBitmask(VkImageUsageBitmask, VkImageUsageFlags,
                            VkImageUsageFlagBits, VK_IMAGE_USAGE_TRANSFER_SRC_BIT,
                            VK_IMAGE_USAGE_TRANSFER_DST_BIT, VK_IMAGE_USAGE_SAMPLED_BIT,
                            VK_IMAGE_USAGE_STORAGE_BIT, VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                            VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
                            VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT,
                            VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
        VkImageUsageFlags, VkImageUsageFlagBits)
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

newtype VkImageUsageBitmask (a ::
                               FlagType) = VkImageUsageBitmask VkFlags
                                             deriving (Eq, Ord, Storable, Data, Generic)

type VkImageUsageFlags = VkImageUsageBitmask FlagMask

type VkImageUsageFlagBits = VkImageUsageBitmask FlagBit

pattern VkImageUsageFlagBits ::
        VkFlags -> VkImageUsageBitmask FlagBit

pattern VkImageUsageFlagBits n = VkImageUsageBitmask n

pattern VkImageUsageFlags ::
        VkFlags -> VkImageUsageBitmask FlagMask

pattern VkImageUsageFlags n = VkImageUsageBitmask n

deriving instance Bits (VkImageUsageBitmask FlagMask)

deriving instance FiniteBits (VkImageUsageBitmask FlagMask)

deriving instance Integral (VkImageUsageBitmask FlagMask)

deriving instance Num (VkImageUsageBitmask FlagMask)

deriving instance Bounded (VkImageUsageBitmask FlagMask)

deriving instance Enum (VkImageUsageBitmask FlagMask)

deriving instance Real (VkImageUsageBitmask FlagMask)

instance Show (VkImageUsageBitmask a) where
        showsPrec _ VK_IMAGE_USAGE_TRANSFER_SRC_BIT
          = showString "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
        showsPrec _ VK_IMAGE_USAGE_TRANSFER_DST_BIT
          = showString "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
        showsPrec _ VK_IMAGE_USAGE_SAMPLED_BIT
          = showString "VK_IMAGE_USAGE_SAMPLED_BIT"
        showsPrec _ VK_IMAGE_USAGE_STORAGE_BIT
          = showString "VK_IMAGE_USAGE_STORAGE_BIT"
        showsPrec _ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
        showsPrec _ VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
        showsPrec _ VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
        showsPrec _ VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
        showsPrec p (VkImageUsageBitmask x)
          = showParen (p >= 11)
              (showString "VkImageUsageBitmask " . showsPrec 11 x)

instance Read (VkImageUsageBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_USAGE_TRANSFER_SRC_BIT",
                   pure VK_IMAGE_USAGE_TRANSFER_SRC_BIT),
                  ("VK_IMAGE_USAGE_TRANSFER_DST_BIT",
                   pure VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                  ("VK_IMAGE_USAGE_SAMPLED_BIT", pure VK_IMAGE_USAGE_SAMPLED_BIT),
                  ("VK_IMAGE_USAGE_STORAGE_BIT", pure VK_IMAGE_USAGE_STORAGE_BIT),
                  ("VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
                  ("VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT),
                  ("VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT),
                  ("VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageUsageBitmask") >>
                      (VkImageUsageBitmask <$> step readPrec)))

-- | Can be used as a source of transfer operations
--
--   bitpos = @0@
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT :: VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageBitmask 1

-- | Can be used as a destination of transfer operations
--
--   bitpos = @1@
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT :: VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageBitmask 2

-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
--
--   bitpos = @2@
pattern VK_IMAGE_USAGE_SAMPLED_BIT :: VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageBitmask 4

-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
--
--   bitpos = @3@
pattern VK_IMAGE_USAGE_STORAGE_BIT :: VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageBitmask 8

-- | Can be used as framebuffer color attachment
--
--   bitpos = @4@
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ::
        VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT =
        VkImageUsageBitmask 16

-- | Can be used as framebuffer depth/stencil attachment
--
--   bitpos = @5@
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT ::
        VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT =
        VkImageUsageBitmask 32

-- | Image data not needed outside of rendering
--
--   bitpos = @6@
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT ::
        VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT =
        VkImageUsageBitmask 64

-- | Can be used as framebuffer input attachment
--
--   bitpos = @7@
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT ::
        VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT =
        VkImageUsageBitmask 128
