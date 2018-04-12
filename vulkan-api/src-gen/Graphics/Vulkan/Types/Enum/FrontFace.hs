{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.FrontFace
       (VkFrontFace(VkFrontFace, VK_FRONT_FACE_COUNTER_CLOCKWISE,
                    VK_FRONT_FACE_CLOCKWISE))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFrontFace VkFrontFace registry at www.khronos.org>
newtype VkFrontFace = VkFrontFace Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkFrontFace where
        showsPrec _ VK_FRONT_FACE_COUNTER_CLOCKWISE
          = showString "VK_FRONT_FACE_COUNTER_CLOCKWISE"
        showsPrec _ VK_FRONT_FACE_CLOCKWISE
          = showString "VK_FRONT_FACE_CLOCKWISE"
        showsPrec p (VkFrontFace x)
          = showParen (p >= 11) (showString "VkFrontFace " . showsPrec 11 x)

instance Read VkFrontFace where
        readPrec
          = parens
              (choose
                 [("VK_FRONT_FACE_COUNTER_CLOCKWISE",
                   pure VK_FRONT_FACE_COUNTER_CLOCKWISE),
                  ("VK_FRONT_FACE_CLOCKWISE", pure VK_FRONT_FACE_CLOCKWISE)]
                 +++
                 prec 10
                   (expectP (Ident "VkFrontFace") >> (VkFrontFace <$> step readPrec)))

pattern VK_FRONT_FACE_COUNTER_CLOCKWISE :: VkFrontFace

pattern VK_FRONT_FACE_COUNTER_CLOCKWISE = VkFrontFace 0

pattern VK_FRONT_FACE_CLOCKWISE :: VkFrontFace

pattern VK_FRONT_FACE_CLOCKWISE = VkFrontFace 1
