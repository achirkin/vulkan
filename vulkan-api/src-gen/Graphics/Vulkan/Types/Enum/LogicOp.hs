{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.LogicOp
       (VkLogicOp(VkLogicOp, VK_LOGIC_OP_CLEAR, VK_LOGIC_OP_AND,
                  VK_LOGIC_OP_AND_REVERSE, VK_LOGIC_OP_COPY,
                  VK_LOGIC_OP_AND_INVERTED, VK_LOGIC_OP_NO_OP, VK_LOGIC_OP_XOR,
                  VK_LOGIC_OP_OR, VK_LOGIC_OP_NOR, VK_LOGIC_OP_EQUIVALENT,
                  VK_LOGIC_OP_INVERT, VK_LOGIC_OP_OR_REVERSE,
                  VK_LOGIC_OP_COPY_INVERTED, VK_LOGIC_OP_OR_INVERTED,
                  VK_LOGIC_OP_NAND, VK_LOGIC_OP_SET))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkLogicOp VkLogicOp registry at www.khronos.org>
newtype VkLogicOp = VkLogicOp Int32
                      deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkLogicOp where
        showsPrec _ VK_LOGIC_OP_CLEAR = showString "VK_LOGIC_OP_CLEAR"
        showsPrec _ VK_LOGIC_OP_AND = showString "VK_LOGIC_OP_AND"
        showsPrec _ VK_LOGIC_OP_AND_REVERSE
          = showString "VK_LOGIC_OP_AND_REVERSE"
        showsPrec _ VK_LOGIC_OP_COPY = showString "VK_LOGIC_OP_COPY"
        showsPrec _ VK_LOGIC_OP_AND_INVERTED
          = showString "VK_LOGIC_OP_AND_INVERTED"
        showsPrec _ VK_LOGIC_OP_NO_OP = showString "VK_LOGIC_OP_NO_OP"
        showsPrec _ VK_LOGIC_OP_XOR = showString "VK_LOGIC_OP_XOR"
        showsPrec _ VK_LOGIC_OP_OR = showString "VK_LOGIC_OP_OR"
        showsPrec _ VK_LOGIC_OP_NOR = showString "VK_LOGIC_OP_NOR"
        showsPrec _ VK_LOGIC_OP_EQUIVALENT
          = showString "VK_LOGIC_OP_EQUIVALENT"
        showsPrec _ VK_LOGIC_OP_INVERT = showString "VK_LOGIC_OP_INVERT"
        showsPrec _ VK_LOGIC_OP_OR_REVERSE
          = showString "VK_LOGIC_OP_OR_REVERSE"
        showsPrec _ VK_LOGIC_OP_COPY_INVERTED
          = showString "VK_LOGIC_OP_COPY_INVERTED"
        showsPrec _ VK_LOGIC_OP_OR_INVERTED
          = showString "VK_LOGIC_OP_OR_INVERTED"
        showsPrec _ VK_LOGIC_OP_NAND = showString "VK_LOGIC_OP_NAND"
        showsPrec _ VK_LOGIC_OP_SET = showString "VK_LOGIC_OP_SET"
        showsPrec p (VkLogicOp x)
          = showParen (p >= 11) (showString "VkLogicOp " . showsPrec 11 x)

instance Read VkLogicOp where
        readPrec
          = parens
              (choose
                 [("VK_LOGIC_OP_CLEAR", pure VK_LOGIC_OP_CLEAR),
                  ("VK_LOGIC_OP_AND", pure VK_LOGIC_OP_AND),
                  ("VK_LOGIC_OP_AND_REVERSE", pure VK_LOGIC_OP_AND_REVERSE),
                  ("VK_LOGIC_OP_COPY", pure VK_LOGIC_OP_COPY),
                  ("VK_LOGIC_OP_AND_INVERTED", pure VK_LOGIC_OP_AND_INVERTED),
                  ("VK_LOGIC_OP_NO_OP", pure VK_LOGIC_OP_NO_OP),
                  ("VK_LOGIC_OP_XOR", pure VK_LOGIC_OP_XOR),
                  ("VK_LOGIC_OP_OR", pure VK_LOGIC_OP_OR),
                  ("VK_LOGIC_OP_NOR", pure VK_LOGIC_OP_NOR),
                  ("VK_LOGIC_OP_EQUIVALENT", pure VK_LOGIC_OP_EQUIVALENT),
                  ("VK_LOGIC_OP_INVERT", pure VK_LOGIC_OP_INVERT),
                  ("VK_LOGIC_OP_OR_REVERSE", pure VK_LOGIC_OP_OR_REVERSE),
                  ("VK_LOGIC_OP_COPY_INVERTED", pure VK_LOGIC_OP_COPY_INVERTED),
                  ("VK_LOGIC_OP_OR_INVERTED", pure VK_LOGIC_OP_OR_INVERTED),
                  ("VK_LOGIC_OP_NAND", pure VK_LOGIC_OP_NAND),
                  ("VK_LOGIC_OP_SET", pure VK_LOGIC_OP_SET)]
                 +++
                 prec 10
                   (expectP (Ident "VkLogicOp") >> (VkLogicOp <$> step readPrec)))

pattern VK_LOGIC_OP_CLEAR :: VkLogicOp

pattern VK_LOGIC_OP_CLEAR = VkLogicOp 0

pattern VK_LOGIC_OP_AND :: VkLogicOp

pattern VK_LOGIC_OP_AND = VkLogicOp 1

pattern VK_LOGIC_OP_AND_REVERSE :: VkLogicOp

pattern VK_LOGIC_OP_AND_REVERSE = VkLogicOp 2

pattern VK_LOGIC_OP_COPY :: VkLogicOp

pattern VK_LOGIC_OP_COPY = VkLogicOp 3

pattern VK_LOGIC_OP_AND_INVERTED :: VkLogicOp

pattern VK_LOGIC_OP_AND_INVERTED = VkLogicOp 4

pattern VK_LOGIC_OP_NO_OP :: VkLogicOp

pattern VK_LOGIC_OP_NO_OP = VkLogicOp 5

pattern VK_LOGIC_OP_XOR :: VkLogicOp

pattern VK_LOGIC_OP_XOR = VkLogicOp 6

pattern VK_LOGIC_OP_OR :: VkLogicOp

pattern VK_LOGIC_OP_OR = VkLogicOp 7

pattern VK_LOGIC_OP_NOR :: VkLogicOp

pattern VK_LOGIC_OP_NOR = VkLogicOp 8

pattern VK_LOGIC_OP_EQUIVALENT :: VkLogicOp

pattern VK_LOGIC_OP_EQUIVALENT = VkLogicOp 9

pattern VK_LOGIC_OP_INVERT :: VkLogicOp

pattern VK_LOGIC_OP_INVERT = VkLogicOp 10

pattern VK_LOGIC_OP_OR_REVERSE :: VkLogicOp

pattern VK_LOGIC_OP_OR_REVERSE = VkLogicOp 11

pattern VK_LOGIC_OP_COPY_INVERTED :: VkLogicOp

pattern VK_LOGIC_OP_COPY_INVERTED = VkLogicOp 12

pattern VK_LOGIC_OP_OR_INVERTED :: VkLogicOp

pattern VK_LOGIC_OP_OR_INVERTED = VkLogicOp 13

pattern VK_LOGIC_OP_NAND :: VkLogicOp

pattern VK_LOGIC_OP_NAND = VkLogicOp 14

pattern VK_LOGIC_OP_SET :: VkLogicOp

pattern VK_LOGIC_OP_SET = VkLogicOp 15
