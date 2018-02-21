{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkDisplayPowerStateEXT
       (VkDisplayPowerStateEXT(VkDisplayPowerStateEXT,
                               VK_DISPLAY_POWER_STATE_OFF_EXT, VK_DISPLAY_POWER_STATE_SUSPEND_EXT,
                               VK_DISPLAY_POWER_STATE_ON_EXT))
       where
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDisplayPowerStateEXT.html VkDisplayPowerStateEXT registry at www.khronos.org>
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
