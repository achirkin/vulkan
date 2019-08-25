{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.BorderColor
       (VkBorderColor(VkBorderColor,
                      VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK,
                      VK_BORDER_COLOR_INT_TRANSPARENT_BLACK,
                      VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                      VK_BORDER_COLOR_INT_OPAQUE_BLACK,
                      VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                      VK_BORDER_COLOR_INT_OPAQUE_WHITE))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBorderColor VkBorderColor registry at www.khronos.org>
newtype VkBorderColor = VkBorderColor Int32
                        deriving (Eq, Ord, Enum, Storable)

instance Show VkBorderColor where
    showsPrec _ VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
      = showString "VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK"
    showsPrec _ VK_BORDER_COLOR_INT_TRANSPARENT_BLACK
      = showString "VK_BORDER_COLOR_INT_TRANSPARENT_BLACK"
    showsPrec _ VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK
      = showString "VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK"
    showsPrec _ VK_BORDER_COLOR_INT_OPAQUE_BLACK
      = showString "VK_BORDER_COLOR_INT_OPAQUE_BLACK"
    showsPrec _ VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE
      = showString "VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE"
    showsPrec _ VK_BORDER_COLOR_INT_OPAQUE_WHITE
      = showString "VK_BORDER_COLOR_INT_OPAQUE_WHITE"
    showsPrec p (VkBorderColor x)
      = showParen (p >= 11)
          (showString "VkBorderColor " . showsPrec 11 x)

instance Read VkBorderColor where
    readPrec
      = parens
          (choose
             [("VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK",
               pure VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK),
              ("VK_BORDER_COLOR_INT_TRANSPARENT_BLACK",
               pure VK_BORDER_COLOR_INT_TRANSPARENT_BLACK),
              ("VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK",
               pure VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK),
              ("VK_BORDER_COLOR_INT_OPAQUE_BLACK",
               pure VK_BORDER_COLOR_INT_OPAQUE_BLACK),
              ("VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE",
               pure VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE),
              ("VK_BORDER_COLOR_INT_OPAQUE_WHITE",
               pure VK_BORDER_COLOR_INT_OPAQUE_WHITE)]
             +++
             prec 10
               (expectP (Ident "VkBorderColor") >>
                  (VkBorderColor <$> step readPrec)))

pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK :: VkBorderColor

pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VkBorderColor 0

pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK :: VkBorderColor

pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = VkBorderColor 1

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK :: VkBorderColor

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = VkBorderColor 2

pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK :: VkBorderColor

pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK = VkBorderColor 3

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE :: VkBorderColor

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = VkBorderColor 4

pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE :: VkBorderColor

pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE = VkBorderColor 5
