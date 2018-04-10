{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkDisplayEventTypeEXT
       (VkDisplayEventTypeEXT(VkDisplayEventTypeEXT,
                              VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT))
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
