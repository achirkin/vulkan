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
module Graphics.Vulkan.Types.Enum.VkSurfaceCounterFlagsEXT
       (VkSurfaceCounterBitmaskEXT(VkSurfaceCounterBitmaskEXT,
                                   VkSurfaceCounterFlagsEXT, VkSurfaceCounterFlagBitsEXT,
                                   VK_SURFACE_COUNTER_VBLANK_EXT),
        VkSurfaceCounterFlagsEXT, VkSurfaceCounterFlagBitsEXT)
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
