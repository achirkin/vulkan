{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.DiscardRectangleModeEXT
       (VkDiscardRectangleModeEXT(VkDiscardRectangleModeEXT,
                                  VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT,
                                  VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDiscardRectangleModeEXT VkDiscardRectangleModeEXT registry at www.khronos.org>
newtype VkDiscardRectangleModeEXT = VkDiscardRectangleModeEXT Int32
                                      deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                Generic)

instance Show VkDiscardRectangleModeEXT where
        showsPrec _ VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT
          = showString "VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT"
        showsPrec _ VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT
          = showString "VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT"
        showsPrec p (VkDiscardRectangleModeEXT x)
          = showParen (p >= 11)
              (showString "VkDiscardRectangleModeEXT " . showsPrec 11 x)

instance Read VkDiscardRectangleModeEXT where
        readPrec
          = parens
              (choose
                 [("VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT",
                   pure VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT),
                  ("VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT",
                   pure VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDiscardRectangleModeEXT") >>
                      (VkDiscardRectangleModeEXT <$> step readPrec)))

pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT ::
        VkDiscardRectangleModeEXT

pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT =
        VkDiscardRectangleModeEXT 0

pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT ::
        VkDiscardRectangleModeEXT

pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT =
        VkDiscardRectangleModeEXT 1
