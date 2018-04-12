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
module Graphics.Vulkan.Types.Enum.Color
       (VkColorComponentBitmask(VkColorComponentBitmask,
                                VkColorComponentFlags, VkColorComponentFlagBits,
                                VK_COLOR_COMPONENT_R_BIT, VK_COLOR_COMPONENT_G_BIT,
                                VK_COLOR_COMPONENT_B_BIT, VK_COLOR_COMPONENT_A_BIT),
        VkColorComponentFlags, VkColorComponentFlagBits,
        VkColorSpaceKHR(VkColorSpaceKHR,
                        VK_COLOR_SPACE_SRGB_NONLINEAR_KHR))
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkColorComponentBitmask (a ::
                                   FlagType) = VkColorComponentBitmask VkFlags
                                                 deriving (Eq, Ord, Storable, Data, Generic)

type VkColorComponentFlags = VkColorComponentBitmask FlagMask

type VkColorComponentFlagBits = VkColorComponentBitmask FlagBit

pattern VkColorComponentFlagBits ::
        VkFlags -> VkColorComponentBitmask FlagBit

pattern VkColorComponentFlagBits n = VkColorComponentBitmask n

pattern VkColorComponentFlags ::
        VkFlags -> VkColorComponentBitmask FlagMask

pattern VkColorComponentFlags n = VkColorComponentBitmask n

deriving instance Bits (VkColorComponentBitmask FlagMask)

deriving instance FiniteBits (VkColorComponentBitmask FlagMask)

deriving instance Integral (VkColorComponentBitmask FlagMask)

deriving instance Num (VkColorComponentBitmask FlagMask)

deriving instance Bounded (VkColorComponentBitmask FlagMask)

deriving instance Enum (VkColorComponentBitmask FlagMask)

deriving instance Real (VkColorComponentBitmask FlagMask)

instance Show (VkColorComponentBitmask a) where
        showsPrec _ VK_COLOR_COMPONENT_R_BIT
          = showString "VK_COLOR_COMPONENT_R_BIT"
        showsPrec _ VK_COLOR_COMPONENT_G_BIT
          = showString "VK_COLOR_COMPONENT_G_BIT"
        showsPrec _ VK_COLOR_COMPONENT_B_BIT
          = showString "VK_COLOR_COMPONENT_B_BIT"
        showsPrec _ VK_COLOR_COMPONENT_A_BIT
          = showString "VK_COLOR_COMPONENT_A_BIT"
        showsPrec p (VkColorComponentBitmask x)
          = showParen (p >= 11)
              (showString "VkColorComponentBitmask " . showsPrec 11 x)

instance Read (VkColorComponentBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_COLOR_COMPONENT_R_BIT", pure VK_COLOR_COMPONENT_R_BIT),
                  ("VK_COLOR_COMPONENT_G_BIT", pure VK_COLOR_COMPONENT_G_BIT),
                  ("VK_COLOR_COMPONENT_B_BIT", pure VK_COLOR_COMPONENT_B_BIT),
                  ("VK_COLOR_COMPONENT_A_BIT", pure VK_COLOR_COMPONENT_A_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkColorComponentBitmask") >>
                      (VkColorComponentBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_COLOR_COMPONENT_R_BIT :: VkColorComponentBitmask a

pattern VK_COLOR_COMPONENT_R_BIT = VkColorComponentBitmask 1

-- | bitpos = @1@
pattern VK_COLOR_COMPONENT_G_BIT :: VkColorComponentBitmask a

pattern VK_COLOR_COMPONENT_G_BIT = VkColorComponentBitmask 2

-- | bitpos = @2@
pattern VK_COLOR_COMPONENT_B_BIT :: VkColorComponentBitmask a

pattern VK_COLOR_COMPONENT_B_BIT = VkColorComponentBitmask 4

-- | bitpos = @3@
pattern VK_COLOR_COMPONENT_A_BIT :: VkColorComponentBitmask a

pattern VK_COLOR_COMPONENT_A_BIT = VkColorComponentBitmask 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkColorSpaceKHR VkColorSpaceKHR registry at www.khronos.org>
newtype VkColorSpaceKHR = VkColorSpaceKHR Int32
                            deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkColorSpaceKHR where
        showsPrec _ VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
          = showString "VK_COLOR_SPACE_SRGB_NONLINEAR_KHR"
        showsPrec p (VkColorSpaceKHR x)
          = showParen (p >= 11)
              (showString "VkColorSpaceKHR " . showsPrec 11 x)

instance Read VkColorSpaceKHR where
        readPrec
          = parens
              (choose
                 [("VK_COLOR_SPACE_SRGB_NONLINEAR_KHR",
                   pure VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkColorSpaceKHR") >>
                      (VkColorSpaceKHR <$> step readPrec)))

pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0
