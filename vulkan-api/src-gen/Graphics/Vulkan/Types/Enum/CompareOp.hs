{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.CompareOp
       (VkCompareOp(VkCompareOp, VK_COMPARE_OP_NEVER, VK_COMPARE_OP_LESS,
                    VK_COMPARE_OP_EQUAL, VK_COMPARE_OP_LESS_OR_EQUAL,
                    VK_COMPARE_OP_GREATER, VK_COMPARE_OP_NOT_EQUAL,
                    VK_COMPARE_OP_GREATER_OR_EQUAL, VK_COMPARE_OP_ALWAYS))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCompareOp VkCompareOp registry at www.khronos.org>
newtype VkCompareOp = VkCompareOp Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkCompareOp where
        showsPrec _ VK_COMPARE_OP_NEVER = showString "VK_COMPARE_OP_NEVER"
        showsPrec _ VK_COMPARE_OP_LESS = showString "VK_COMPARE_OP_LESS"
        showsPrec _ VK_COMPARE_OP_EQUAL = showString "VK_COMPARE_OP_EQUAL"
        showsPrec _ VK_COMPARE_OP_LESS_OR_EQUAL
          = showString "VK_COMPARE_OP_LESS_OR_EQUAL"
        showsPrec _ VK_COMPARE_OP_GREATER
          = showString "VK_COMPARE_OP_GREATER"
        showsPrec _ VK_COMPARE_OP_NOT_EQUAL
          = showString "VK_COMPARE_OP_NOT_EQUAL"
        showsPrec _ VK_COMPARE_OP_GREATER_OR_EQUAL
          = showString "VK_COMPARE_OP_GREATER_OR_EQUAL"
        showsPrec _ VK_COMPARE_OP_ALWAYS
          = showString "VK_COMPARE_OP_ALWAYS"
        showsPrec p (VkCompareOp x)
          = showParen (p >= 11) (showString "VkCompareOp " . showsPrec 11 x)

instance Read VkCompareOp where
        readPrec
          = parens
              (choose
                 [("VK_COMPARE_OP_NEVER", pure VK_COMPARE_OP_NEVER),
                  ("VK_COMPARE_OP_LESS", pure VK_COMPARE_OP_LESS),
                  ("VK_COMPARE_OP_EQUAL", pure VK_COMPARE_OP_EQUAL),
                  ("VK_COMPARE_OP_LESS_OR_EQUAL", pure VK_COMPARE_OP_LESS_OR_EQUAL),
                  ("VK_COMPARE_OP_GREATER", pure VK_COMPARE_OP_GREATER),
                  ("VK_COMPARE_OP_NOT_EQUAL", pure VK_COMPARE_OP_NOT_EQUAL),
                  ("VK_COMPARE_OP_GREATER_OR_EQUAL",
                   pure VK_COMPARE_OP_GREATER_OR_EQUAL),
                  ("VK_COMPARE_OP_ALWAYS", pure VK_COMPARE_OP_ALWAYS)]
                 +++
                 prec 10
                   (expectP (Ident "VkCompareOp") >> (VkCompareOp <$> step readPrec)))

pattern VK_COMPARE_OP_NEVER :: VkCompareOp

pattern VK_COMPARE_OP_NEVER = VkCompareOp 0

pattern VK_COMPARE_OP_LESS :: VkCompareOp

pattern VK_COMPARE_OP_LESS = VkCompareOp 1

pattern VK_COMPARE_OP_EQUAL :: VkCompareOp

pattern VK_COMPARE_OP_EQUAL = VkCompareOp 2

pattern VK_COMPARE_OP_LESS_OR_EQUAL :: VkCompareOp

pattern VK_COMPARE_OP_LESS_OR_EQUAL = VkCompareOp 3

pattern VK_COMPARE_OP_GREATER :: VkCompareOp

pattern VK_COMPARE_OP_GREATER = VkCompareOp 4

pattern VK_COMPARE_OP_NOT_EQUAL :: VkCompareOp

pattern VK_COMPARE_OP_NOT_EQUAL = VkCompareOp 5

pattern VK_COMPARE_OP_GREATER_OR_EQUAL :: VkCompareOp

pattern VK_COMPARE_OP_GREATER_OR_EQUAL = VkCompareOp 6

pattern VK_COMPARE_OP_ALWAYS :: VkCompareOp

pattern VK_COMPARE_OP_ALWAYS = VkCompareOp 7
