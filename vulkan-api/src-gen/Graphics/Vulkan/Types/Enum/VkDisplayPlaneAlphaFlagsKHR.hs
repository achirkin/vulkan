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
module Graphics.Vulkan.Types.Enum.VkDisplayPlaneAlphaFlagsKHR
       (VkDisplayPlaneAlphaBitmaskKHR(VkDisplayPlaneAlphaBitmaskKHR,
                                      VkDisplayPlaneAlphaFlagsKHR, VkDisplayPlaneAlphaFlagBitsKHR,
                                      VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR,
                                      VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR,
                                      VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR,
                                      VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR),
        VkDisplayPlaneAlphaFlagsKHR, VkDisplayPlaneAlphaFlagBitsKHR)
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

newtype VkDisplayPlaneAlphaBitmaskKHR (a ::
                                         FlagType) = VkDisplayPlaneAlphaBitmaskKHR VkFlags
                                                       deriving (Eq, Ord, Storable, Data, Generic)

type VkDisplayPlaneAlphaFlagsKHR =
     VkDisplayPlaneAlphaBitmaskKHR FlagMask

type VkDisplayPlaneAlphaFlagBitsKHR =
     VkDisplayPlaneAlphaBitmaskKHR FlagBit

pattern VkDisplayPlaneAlphaFlagBitsKHR ::
        VkFlags -> VkDisplayPlaneAlphaBitmaskKHR FlagBit

pattern VkDisplayPlaneAlphaFlagBitsKHR n =
        VkDisplayPlaneAlphaBitmaskKHR n

pattern VkDisplayPlaneAlphaFlagsKHR ::
        VkFlags -> VkDisplayPlaneAlphaBitmaskKHR FlagMask

pattern VkDisplayPlaneAlphaFlagsKHR n =
        VkDisplayPlaneAlphaBitmaskKHR n

deriving instance Bits (VkDisplayPlaneAlphaBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkDisplayPlaneAlphaBitmaskKHR FlagMask)

deriving instance Integral (VkDisplayPlaneAlphaBitmaskKHR FlagMask)

deriving instance Num (VkDisplayPlaneAlphaBitmaskKHR FlagMask)

deriving instance Bounded (VkDisplayPlaneAlphaBitmaskKHR FlagMask)

deriving instance Enum (VkDisplayPlaneAlphaBitmaskKHR FlagMask)

deriving instance Real (VkDisplayPlaneAlphaBitmaskKHR FlagMask)

instance Show (VkDisplayPlaneAlphaBitmaskKHR a) where
        showsPrec _ VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
          = showString "VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR"
        showsPrec _ VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
          = showString "VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR"
        showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
          = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR"
        showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
          = showString
              "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR"
        showsPrec p (VkDisplayPlaneAlphaBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkDisplayPlaneAlphaBitmaskKHR " . showsPrec 11 x)

instance Read (VkDisplayPlaneAlphaBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR",
                   pure VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR),
                  ("VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR",
                   pure VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR),
                  ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR",
                   pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR),
                  ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR",
                   pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkDisplayPlaneAlphaBitmaskKHR") >>
                      (VkDisplayPlaneAlphaBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR ::
        VkDisplayPlaneAlphaBitmaskKHR a

pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR =
        VkDisplayPlaneAlphaBitmaskKHR 1

-- | bitpos = @1@
pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR ::
        VkDisplayPlaneAlphaBitmaskKHR a

pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR =
        VkDisplayPlaneAlphaBitmaskKHR 2

-- | bitpos = @2@
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR ::
        VkDisplayPlaneAlphaBitmaskKHR a

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR =
        VkDisplayPlaneAlphaBitmaskKHR 4

-- | bitpos = @3@
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR ::
        VkDisplayPlaneAlphaBitmaskKHR a

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR =
        VkDisplayPlaneAlphaBitmaskKHR 8
