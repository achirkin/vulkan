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
module Graphics.Vulkan.Types.Enum.VkImageAspectFlags
       (VkImageAspectBitmask(VkImageAspectBitmask, VkImageAspectFlags,
                             VkImageAspectFlagBits, VK_IMAGE_ASPECT_COLOR_BIT,
                             VK_IMAGE_ASPECT_DEPTH_BIT, VK_IMAGE_ASPECT_STENCIL_BIT,
                             VK_IMAGE_ASPECT_METADATA_BIT),
        VkImageAspectFlags, VkImageAspectFlagBits)
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

newtype VkImageAspectBitmask (a ::
                                FlagType) = VkImageAspectBitmask VkFlags
                                              deriving (Eq, Ord, Storable, Data, Generic)

type VkImageAspectFlags = VkImageAspectBitmask FlagMask

type VkImageAspectFlagBits = VkImageAspectBitmask FlagBit

pattern VkImageAspectFlagBits ::
        VkFlags -> VkImageAspectBitmask FlagBit

pattern VkImageAspectFlagBits n = VkImageAspectBitmask n

pattern VkImageAspectFlags ::
        VkFlags -> VkImageAspectBitmask FlagMask

pattern VkImageAspectFlags n = VkImageAspectBitmask n

deriving instance Bits (VkImageAspectBitmask FlagMask)

deriving instance FiniteBits (VkImageAspectBitmask FlagMask)

deriving instance Integral (VkImageAspectBitmask FlagMask)

deriving instance Num (VkImageAspectBitmask FlagMask)

deriving instance Bounded (VkImageAspectBitmask FlagMask)

deriving instance Enum (VkImageAspectBitmask FlagMask)

deriving instance Real (VkImageAspectBitmask FlagMask)

instance Show (VkImageAspectBitmask a) where
        showsPrec _ VK_IMAGE_ASPECT_COLOR_BIT
          = showString "VK_IMAGE_ASPECT_COLOR_BIT"
        showsPrec _ VK_IMAGE_ASPECT_DEPTH_BIT
          = showString "VK_IMAGE_ASPECT_DEPTH_BIT"
        showsPrec _ VK_IMAGE_ASPECT_STENCIL_BIT
          = showString "VK_IMAGE_ASPECT_STENCIL_BIT"
        showsPrec _ VK_IMAGE_ASPECT_METADATA_BIT
          = showString "VK_IMAGE_ASPECT_METADATA_BIT"
        showsPrec p (VkImageAspectBitmask x)
          = showParen (p >= 11)
              (showString "VkImageAspectBitmask " . showsPrec 11 x)

instance Read (VkImageAspectBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_ASPECT_COLOR_BIT", pure VK_IMAGE_ASPECT_COLOR_BIT),
                  ("VK_IMAGE_ASPECT_DEPTH_BIT", pure VK_IMAGE_ASPECT_DEPTH_BIT),
                  ("VK_IMAGE_ASPECT_STENCIL_BIT", pure VK_IMAGE_ASPECT_STENCIL_BIT),
                  ("VK_IMAGE_ASPECT_METADATA_BIT",
                   pure VK_IMAGE_ASPECT_METADATA_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageAspectBitmask") >>
                      (VkImageAspectBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_IMAGE_ASPECT_COLOR_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_COLOR_BIT = VkImageAspectBitmask 1

-- | bitpos = @1@
pattern VK_IMAGE_ASPECT_DEPTH_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_DEPTH_BIT = VkImageAspectBitmask 2

-- | bitpos = @2@
pattern VK_IMAGE_ASPECT_STENCIL_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_STENCIL_BIT = VkImageAspectBitmask 4

-- | bitpos = @3@
pattern VK_IMAGE_ASPECT_METADATA_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_METADATA_BIT = VkImageAspectBitmask 8
