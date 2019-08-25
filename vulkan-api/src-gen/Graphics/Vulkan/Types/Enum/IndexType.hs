{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.IndexType
       (VkIndexType(VkIndexType, VK_INDEX_TYPE_UINT16,
                    VK_INDEX_TYPE_UINT32))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkIndexType VkIndexType registry at www.khronos.org>
newtype VkIndexType = VkIndexType Int32
                      deriving (Eq, Ord, Enum, Storable)

instance Show VkIndexType where
    showsPrec _ VK_INDEX_TYPE_UINT16
      = showString "VK_INDEX_TYPE_UINT16"
    showsPrec _ VK_INDEX_TYPE_UINT32
      = showString "VK_INDEX_TYPE_UINT32"
    showsPrec p (VkIndexType x)
      = showParen (p >= 11) (showString "VkIndexType " . showsPrec 11 x)

instance Read VkIndexType where
    readPrec
      = parens
          (choose
             [("VK_INDEX_TYPE_UINT16", pure VK_INDEX_TYPE_UINT16),
              ("VK_INDEX_TYPE_UINT32", pure VK_INDEX_TYPE_UINT32)]
             +++
             prec 10
               (expectP (Ident "VkIndexType") >> (VkIndexType <$> step readPrec)))

pattern VK_INDEX_TYPE_UINT16 :: VkIndexType

pattern VK_INDEX_TYPE_UINT16 = VkIndexType 0

pattern VK_INDEX_TYPE_UINT32 :: VkIndexType

pattern VK_INDEX_TYPE_UINT32 = VkIndexType 1
