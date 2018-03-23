{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkDeviceEventTypeEXT
       (VkDeviceEventTypeEXT(VkDeviceEventTypeEXT,
                             VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDeviceEventTypeEXT.html VkDeviceEventTypeEXT registry at www.khronos.org>
newtype VkDeviceEventTypeEXT = VkDeviceEventTypeEXT Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkDeviceEventTypeEXT where
        showsPrec _ VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT
          = showString "VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT"
        showsPrec p (VkDeviceEventTypeEXT x)
          = showParen (p >= 11)
              (showString "VkDeviceEventTypeEXT " . showsPrec 11 x)

instance Read VkDeviceEventTypeEXT where
        readPrec
          = parens
              (choose
                 [("VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT",
                   pure VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDeviceEventTypeEXT") >>
                      (VkDeviceEventTypeEXT <$> step readPrec)))

pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT ::
        VkDeviceEventTypeEXT

pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT =
        VkDeviceEventTypeEXT 0
