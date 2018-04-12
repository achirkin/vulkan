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
module Graphics.Vulkan.Types.Enum.Display
       (VkDisplayEventTypeEXT(VkDisplayEventTypeEXT,
                              VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT),
        VkDisplayPlaneAlphaBitmaskKHR(VkDisplayPlaneAlphaBitmaskKHR,
                                      VkDisplayPlaneAlphaFlagsKHR, VkDisplayPlaneAlphaFlagBitsKHR,
                                      VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR,
                                      VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR,
                                      VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR,
                                      VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR),
        VkDisplayPlaneAlphaFlagsKHR, VkDisplayPlaneAlphaFlagBitsKHR,
        VkDisplayPowerStateEXT(VkDisplayPowerStateEXT,
                               VK_DISPLAY_POWER_STATE_OFF_EXT, VK_DISPLAY_POWER_STATE_SUSPEND_EXT,
                               VK_DISPLAY_POWER_STATE_ON_EXT))
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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayEventTypeEXT VkDisplayEventTypeEXT registry at www.khronos.org>
newtype VkDisplayEventTypeEXT = VkDisplayEventTypeEXT Int32
                                  deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkDisplayEventTypeEXT where
        showsPrec _ VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT
          = showString "VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT"
        showsPrec p (VkDisplayEventTypeEXT x)
          = showParen (p >= 11)
              (showString "VkDisplayEventTypeEXT " . showsPrec 11 x)

instance Read VkDisplayEventTypeEXT where
        readPrec
          = parens
              (choose
                 [("VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT",
                   pure VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDisplayEventTypeEXT") >>
                      (VkDisplayEventTypeEXT <$> step readPrec)))

pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT ::
        VkDisplayEventTypeEXT

pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT =
        VkDisplayEventTypeEXT 0

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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPowerStateEXT VkDisplayPowerStateEXT registry at www.khronos.org>
newtype VkDisplayPowerStateEXT = VkDisplayPowerStateEXT Int32
                                   deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkDisplayPowerStateEXT where
        showsPrec _ VK_DISPLAY_POWER_STATE_OFF_EXT
          = showString "VK_DISPLAY_POWER_STATE_OFF_EXT"
        showsPrec _ VK_DISPLAY_POWER_STATE_SUSPEND_EXT
          = showString "VK_DISPLAY_POWER_STATE_SUSPEND_EXT"
        showsPrec _ VK_DISPLAY_POWER_STATE_ON_EXT
          = showString "VK_DISPLAY_POWER_STATE_ON_EXT"
        showsPrec p (VkDisplayPowerStateEXT x)
          = showParen (p >= 11)
              (showString "VkDisplayPowerStateEXT " . showsPrec 11 x)

instance Read VkDisplayPowerStateEXT where
        readPrec
          = parens
              (choose
                 [("VK_DISPLAY_POWER_STATE_OFF_EXT",
                   pure VK_DISPLAY_POWER_STATE_OFF_EXT),
                  ("VK_DISPLAY_POWER_STATE_SUSPEND_EXT",
                   pure VK_DISPLAY_POWER_STATE_SUSPEND_EXT),
                  ("VK_DISPLAY_POWER_STATE_ON_EXT",
                   pure VK_DISPLAY_POWER_STATE_ON_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDisplayPowerStateEXT") >>
                      (VkDisplayPowerStateEXT <$> step readPrec)))

pattern VK_DISPLAY_POWER_STATE_OFF_EXT :: VkDisplayPowerStateEXT

pattern VK_DISPLAY_POWER_STATE_OFF_EXT = VkDisplayPowerStateEXT 0

pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT ::
        VkDisplayPowerStateEXT

pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT =
        VkDisplayPowerStateEXT 1

pattern VK_DISPLAY_POWER_STATE_ON_EXT :: VkDisplayPowerStateEXT

pattern VK_DISPLAY_POWER_STATE_ON_EXT = VkDisplayPowerStateEXT 2
