{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VertexInputRate
       (VkVertexInputRate(VkVertexInputRate, VK_VERTEX_INPUT_RATE_VERTEX,
                          VK_VERTEX_INPUT_RATE_INSTANCE))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkVertexInputRate VkVertexInputRate registry at www.khronos.org>
newtype VkVertexInputRate = VkVertexInputRate Int32
                            deriving (Eq, Ord, Enum, Storable)

instance Show VkVertexInputRate where
    showsPrec _ VK_VERTEX_INPUT_RATE_VERTEX
      = showString "VK_VERTEX_INPUT_RATE_VERTEX"
    showsPrec _ VK_VERTEX_INPUT_RATE_INSTANCE
      = showString "VK_VERTEX_INPUT_RATE_INSTANCE"
    showsPrec p (VkVertexInputRate x)
      = showParen (p >= 11)
          (showString "VkVertexInputRate " . showsPrec 11 x)

instance Read VkVertexInputRate where
    readPrec
      = parens
          (choose
             [("VK_VERTEX_INPUT_RATE_VERTEX", pure VK_VERTEX_INPUT_RATE_VERTEX),
              ("VK_VERTEX_INPUT_RATE_INSTANCE",
               pure VK_VERTEX_INPUT_RATE_INSTANCE)]
             +++
             prec 10
               (expectP (Ident "VkVertexInputRate") >>
                  (VkVertexInputRate <$> step readPrec)))

pattern VK_VERTEX_INPUT_RATE_VERTEX :: VkVertexInputRate

pattern VK_VERTEX_INPUT_RATE_VERTEX = VkVertexInputRate 0

pattern VK_VERTEX_INPUT_RATE_INSTANCE :: VkVertexInputRate

pattern VK_VERTEX_INPUT_RATE_INSTANCE = VkVertexInputRate 1
