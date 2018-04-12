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
module Graphics.Vulkan.Types.Enum.Surface
       (VkSurfaceCounterBitmaskEXT(VkSurfaceCounterBitmaskEXT,
                                   VkSurfaceCounterFlagsEXT, VkSurfaceCounterFlagBitsEXT,
                                   VK_SURFACE_COUNTER_VBLANK_EXT),
        VkSurfaceCounterFlagsEXT, VkSurfaceCounterFlagBitsEXT,
        VkSurfaceTransformBitmaskKHR(VkSurfaceTransformBitmaskKHR,
                                     VkSurfaceTransformFlagsKHR, VkSurfaceTransformFlagBitsKHR,
                                     VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR,
                                     VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR,
                                     VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR,
                                     VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR,
                                     VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR,
                                     VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR,
                                     VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR,
                                     VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR,
                                     VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR),
        VkSurfaceTransformFlagsKHR, VkSurfaceTransformFlagBitsKHR)
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

newtype VkSurfaceCounterBitmaskEXT (a ::
                                      FlagType) = VkSurfaceCounterBitmaskEXT VkFlags
                                                    deriving (Eq, Ord, Storable, Data, Generic)

type VkSurfaceCounterFlagsEXT = VkSurfaceCounterBitmaskEXT FlagMask

type VkSurfaceCounterFlagBitsEXT =
     VkSurfaceCounterBitmaskEXT FlagBit

pattern VkSurfaceCounterFlagBitsEXT ::
        VkFlags -> VkSurfaceCounterBitmaskEXT FlagBit

pattern VkSurfaceCounterFlagBitsEXT n =
        VkSurfaceCounterBitmaskEXT n

pattern VkSurfaceCounterFlagsEXT ::
        VkFlags -> VkSurfaceCounterBitmaskEXT FlagMask

pattern VkSurfaceCounterFlagsEXT n = VkSurfaceCounterBitmaskEXT n

deriving instance Bits (VkSurfaceCounterBitmaskEXT FlagMask)

deriving instance FiniteBits (VkSurfaceCounterBitmaskEXT FlagMask)

deriving instance Integral (VkSurfaceCounterBitmaskEXT FlagMask)

deriving instance Num (VkSurfaceCounterBitmaskEXT FlagMask)

deriving instance Bounded (VkSurfaceCounterBitmaskEXT FlagMask)

deriving instance Enum (VkSurfaceCounterBitmaskEXT FlagMask)

deriving instance Real (VkSurfaceCounterBitmaskEXT FlagMask)

instance Show (VkSurfaceCounterBitmaskEXT a) where
        showsPrec _ VK_SURFACE_COUNTER_VBLANK_EXT
          = showString "VK_SURFACE_COUNTER_VBLANK_EXT"
        showsPrec p (VkSurfaceCounterBitmaskEXT x)
          = showParen (p >= 11)
              (showString "VkSurfaceCounterBitmaskEXT " . showsPrec 11 x)

instance Read (VkSurfaceCounterBitmaskEXT a) where
        readPrec
          = parens
              (choose
                 [("VK_SURFACE_COUNTER_VBLANK_EXT",
                   pure VK_SURFACE_COUNTER_VBLANK_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSurfaceCounterBitmaskEXT") >>
                      (VkSurfaceCounterBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_SURFACE_COUNTER_VBLANK_EXT ::
        VkSurfaceCounterBitmaskEXT a

pattern VK_SURFACE_COUNTER_VBLANK_EXT =
        VkSurfaceCounterBitmaskEXT 1

newtype VkSurfaceTransformBitmaskKHR (a ::
                                        FlagType) = VkSurfaceTransformBitmaskKHR VkFlags
                                                      deriving (Eq, Ord, Storable, Data, Generic)

type VkSurfaceTransformFlagsKHR =
     VkSurfaceTransformBitmaskKHR FlagMask

type VkSurfaceTransformFlagBitsKHR =
     VkSurfaceTransformBitmaskKHR FlagBit

pattern VkSurfaceTransformFlagBitsKHR ::
        VkFlags -> VkSurfaceTransformBitmaskKHR FlagBit

pattern VkSurfaceTransformFlagBitsKHR n =
        VkSurfaceTransformBitmaskKHR n

pattern VkSurfaceTransformFlagsKHR ::
        VkFlags -> VkSurfaceTransformBitmaskKHR FlagMask

pattern VkSurfaceTransformFlagsKHR n =
        VkSurfaceTransformBitmaskKHR n

deriving instance Bits (VkSurfaceTransformBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkSurfaceTransformBitmaskKHR FlagMask)

deriving instance Integral (VkSurfaceTransformBitmaskKHR FlagMask)

deriving instance Num (VkSurfaceTransformBitmaskKHR FlagMask)

deriving instance Bounded (VkSurfaceTransformBitmaskKHR FlagMask)

deriving instance Enum (VkSurfaceTransformBitmaskKHR FlagMask)

deriving instance Real (VkSurfaceTransformBitmaskKHR FlagMask)

instance Show (VkSurfaceTransformBitmaskKHR a) where
        showsPrec _ VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR"
        showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR"
        showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR"
        showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR"
        showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR"
        showsPrec _
          VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
          = showString
              "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR"
        showsPrec _
          VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
          = showString
              "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR"
        showsPrec _
          VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
          = showString
              "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR"
        showsPrec _ VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR
          = showString "VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR"
        showsPrec p (VkSurfaceTransformBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkSurfaceTransformBitmaskKHR " . showsPrec 11 x)

instance Read (VkSurfaceTransformBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR),
                  ("VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR",
                   pure VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkSurfaceTransformBitmaskKHR") >>
                      (VkSurfaceTransformBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR ::
        VkSurfaceTransformBitmaskKHR a

pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR =
        VkSurfaceTransformBitmaskKHR 1

-- | bitpos = @1@
pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR ::
        VkSurfaceTransformBitmaskKHR a

pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR =
        VkSurfaceTransformBitmaskKHR 2

-- | bitpos = @2@
pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR ::
        VkSurfaceTransformBitmaskKHR a

pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR =
        VkSurfaceTransformBitmaskKHR 4

-- | bitpos = @3@
pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR ::
        VkSurfaceTransformBitmaskKHR a

pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR =
        VkSurfaceTransformBitmaskKHR 8

-- | bitpos = @4@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR ::
        VkSurfaceTransformBitmaskKHR a

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR =
        VkSurfaceTransformBitmaskKHR 16

-- | bitpos = @5@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR ::
        VkSurfaceTransformBitmaskKHR a

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR =
        VkSurfaceTransformBitmaskKHR 32

-- | bitpos = @6@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
        :: VkSurfaceTransformBitmaskKHR a

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR =
        VkSurfaceTransformBitmaskKHR 64

-- | bitpos = @7@
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
        :: VkSurfaceTransformBitmaskKHR a

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR =
        VkSurfaceTransformBitmaskKHR 128

-- | bitpos = @8@
pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR ::
        VkSurfaceTransformBitmaskKHR a

pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR =
        VkSurfaceTransformBitmaskKHR 256
