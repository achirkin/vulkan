{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkBlendOp
       (VkBlendOp(VkBlendOp, VK_BLEND_OP_ADD, VK_BLEND_OP_SUBTRACT,
                  VK_BLEND_OP_REVERSE_SUBTRACT, VK_BLEND_OP_MIN, VK_BLEND_OP_MAX))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkBlendOp.html VkBlendOp registry at www.khronos.org>
newtype VkBlendOp = VkBlendOp Int32
                      deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkBlendOp where
        showsPrec _ VK_BLEND_OP_ADD = showString "VK_BLEND_OP_ADD"
        showsPrec _ VK_BLEND_OP_SUBTRACT
          = showString "VK_BLEND_OP_SUBTRACT"
        showsPrec _ VK_BLEND_OP_REVERSE_SUBTRACT
          = showString "VK_BLEND_OP_REVERSE_SUBTRACT"
        showsPrec _ VK_BLEND_OP_MIN = showString "VK_BLEND_OP_MIN"
        showsPrec _ VK_BLEND_OP_MAX = showString "VK_BLEND_OP_MAX"
        showsPrec p (VkBlendOp x)
          = showParen (p >= 11) (showString "VkBlendOp " . showsPrec 11 x)

instance Read VkBlendOp where
        readPrec
          = parens
              (choose
                 [("VK_BLEND_OP_ADD", pure VK_BLEND_OP_ADD),
                  ("VK_BLEND_OP_SUBTRACT", pure VK_BLEND_OP_SUBTRACT),
                  ("VK_BLEND_OP_REVERSE_SUBTRACT",
                   pure VK_BLEND_OP_REVERSE_SUBTRACT),
                  ("VK_BLEND_OP_MIN", pure VK_BLEND_OP_MIN),
                  ("VK_BLEND_OP_MAX", pure VK_BLEND_OP_MAX)]
                 +++
                 prec 10
                   (expectP (Ident "VkBlendOp") >> (VkBlendOp <$> step readPrec)))

pattern VK_BLEND_OP_ADD :: VkBlendOp

pattern VK_BLEND_OP_ADD = VkBlendOp 0

pattern VK_BLEND_OP_SUBTRACT :: VkBlendOp

pattern VK_BLEND_OP_SUBTRACT = VkBlendOp 1

pattern VK_BLEND_OP_REVERSE_SUBTRACT :: VkBlendOp

pattern VK_BLEND_OP_REVERSE_SUBTRACT = VkBlendOp 2

pattern VK_BLEND_OP_MIN :: VkBlendOp

pattern VK_BLEND_OP_MIN = VkBlendOp 3

pattern VK_BLEND_OP_MAX :: VkBlendOp

pattern VK_BLEND_OP_MAX = VkBlendOp 4
