{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.PolygonMode
       (VkPolygonMode(VkPolygonMode, VK_POLYGON_MODE_FILL,
                      VK_POLYGON_MODE_LINE, VK_POLYGON_MODE_POINT))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPolygonMode VkPolygonMode registry at www.khronos.org>
newtype VkPolygonMode = VkPolygonMode Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkPolygonMode where
        showsPrec _ VK_POLYGON_MODE_FILL
          = showString "VK_POLYGON_MODE_FILL"
        showsPrec _ VK_POLYGON_MODE_LINE
          = showString "VK_POLYGON_MODE_LINE"
        showsPrec _ VK_POLYGON_MODE_POINT
          = showString "VK_POLYGON_MODE_POINT"
        showsPrec p (VkPolygonMode x)
          = showParen (p >= 11)
              (showString "VkPolygonMode " . showsPrec 11 x)

instance Read VkPolygonMode where
        readPrec
          = parens
              (choose
                 [("VK_POLYGON_MODE_FILL", pure VK_POLYGON_MODE_FILL),
                  ("VK_POLYGON_MODE_LINE", pure VK_POLYGON_MODE_LINE),
                  ("VK_POLYGON_MODE_POINT", pure VK_POLYGON_MODE_POINT)]
                 +++
                 prec 10
                   (expectP (Ident "VkPolygonMode") >>
                      (VkPolygonMode <$> step readPrec)))

pattern VK_POLYGON_MODE_FILL :: VkPolygonMode

pattern VK_POLYGON_MODE_FILL = VkPolygonMode 0

pattern VK_POLYGON_MODE_LINE :: VkPolygonMode

pattern VK_POLYGON_MODE_LINE = VkPolygonMode 1

pattern VK_POLYGON_MODE_POINT :: VkPolygonMode

pattern VK_POLYGON_MODE_POINT = VkPolygonMode 2
