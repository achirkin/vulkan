{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.Filter
       (VkFilter(VkFilter, VK_FILTER_NEAREST, VK_FILTER_LINEAR)) where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFilter VkFilter registry at www.khronos.org>
newtype VkFilter = VkFilter Int32
                   deriving (Eq, Ord, Enum, Storable)

instance Show VkFilter where
    showsPrec _ VK_FILTER_NEAREST = showString "VK_FILTER_NEAREST"
    showsPrec _ VK_FILTER_LINEAR = showString "VK_FILTER_LINEAR"
    showsPrec p (VkFilter x)
      = showParen (p >= 11) (showString "VkFilter " . showsPrec 11 x)

instance Read VkFilter where
    readPrec
      = parens
          (choose
             [("VK_FILTER_NEAREST", pure VK_FILTER_NEAREST),
              ("VK_FILTER_LINEAR", pure VK_FILTER_LINEAR)]
             +++
             prec 10
               (expectP (Ident "VkFilter") >> (VkFilter <$> step readPrec)))

pattern VK_FILTER_NEAREST :: VkFilter

pattern VK_FILTER_NEAREST = VkFilter 0

pattern VK_FILTER_LINEAR :: VkFilter

pattern VK_FILTER_LINEAR = VkFilter 1
