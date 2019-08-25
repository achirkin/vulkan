{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.Blend
       (VkBlendFactor(VkBlendFactor, VK_BLEND_FACTOR_ZERO,
                      VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_SRC_COLOR,
                      VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR, VK_BLEND_FACTOR_DST_COLOR,
                      VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR, VK_BLEND_FACTOR_SRC_ALPHA,
                      VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA, VK_BLEND_FACTOR_DST_ALPHA,
                      VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA,
                      VK_BLEND_FACTOR_CONSTANT_COLOR,
                      VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR,
                      VK_BLEND_FACTOR_CONSTANT_ALPHA,
                      VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA,
                      VK_BLEND_FACTOR_SRC_ALPHA_SATURATE, VK_BLEND_FACTOR_SRC1_COLOR,
                      VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR, VK_BLEND_FACTOR_SRC1_ALPHA,
                      VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA),
        VkBlendOp(VkBlendOp, VK_BLEND_OP_ADD, VK_BLEND_OP_SUBTRACT,
                  VK_BLEND_OP_REVERSE_SUBTRACT, VK_BLEND_OP_MIN, VK_BLEND_OP_MAX),
        VkBlendOverlapEXT(VkBlendOverlapEXT,
                          VK_BLEND_OVERLAP_UNCORRELATED_EXT, VK_BLEND_OVERLAP_DISJOINT_EXT,
                          VK_BLEND_OVERLAP_CONJOINT_EXT))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBlendFactor VkBlendFactor registry at www.khronos.org>
newtype VkBlendFactor = VkBlendFactor Int32
                        deriving (Eq, Ord, Enum, Storable)

instance Show VkBlendFactor where
    showsPrec _ VK_BLEND_FACTOR_ZERO
      = showString "VK_BLEND_FACTOR_ZERO"
    showsPrec _ VK_BLEND_FACTOR_ONE = showString "VK_BLEND_FACTOR_ONE"
    showsPrec _ VK_BLEND_FACTOR_SRC_COLOR
      = showString "VK_BLEND_FACTOR_SRC_COLOR"
    showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR
      = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR"
    showsPrec _ VK_BLEND_FACTOR_DST_COLOR
      = showString "VK_BLEND_FACTOR_DST_COLOR"
    showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR
      = showString "VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR"
    showsPrec _ VK_BLEND_FACTOR_SRC_ALPHA
      = showString "VK_BLEND_FACTOR_SRC_ALPHA"
    showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
      = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA"
    showsPrec _ VK_BLEND_FACTOR_DST_ALPHA
      = showString "VK_BLEND_FACTOR_DST_ALPHA"
    showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA
      = showString "VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA"
    showsPrec _ VK_BLEND_FACTOR_CONSTANT_COLOR
      = showString "VK_BLEND_FACTOR_CONSTANT_COLOR"
    showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR
      = showString "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR"
    showsPrec _ VK_BLEND_FACTOR_CONSTANT_ALPHA
      = showString "VK_BLEND_FACTOR_CONSTANT_ALPHA"
    showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA
      = showString "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA"
    showsPrec _ VK_BLEND_FACTOR_SRC_ALPHA_SATURATE
      = showString "VK_BLEND_FACTOR_SRC_ALPHA_SATURATE"
    showsPrec _ VK_BLEND_FACTOR_SRC1_COLOR
      = showString "VK_BLEND_FACTOR_SRC1_COLOR"
    showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR
      = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR"
    showsPrec _ VK_BLEND_FACTOR_SRC1_ALPHA
      = showString "VK_BLEND_FACTOR_SRC1_ALPHA"
    showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
      = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA"
    showsPrec p (VkBlendFactor x)
      = showParen (p >= 11)
          (showString "VkBlendFactor " . showsPrec 11 x)

instance Read VkBlendFactor where
    readPrec
      = parens
          (choose
             [("VK_BLEND_FACTOR_ZERO", pure VK_BLEND_FACTOR_ZERO),
              ("VK_BLEND_FACTOR_ONE", pure VK_BLEND_FACTOR_ONE),
              ("VK_BLEND_FACTOR_SRC_COLOR", pure VK_BLEND_FACTOR_SRC_COLOR),
              ("VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR",
               pure VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR),
              ("VK_BLEND_FACTOR_DST_COLOR", pure VK_BLEND_FACTOR_DST_COLOR),
              ("VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR",
               pure VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR),
              ("VK_BLEND_FACTOR_SRC_ALPHA", pure VK_BLEND_FACTOR_SRC_ALPHA),
              ("VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA",
               pure VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA),
              ("VK_BLEND_FACTOR_DST_ALPHA", pure VK_BLEND_FACTOR_DST_ALPHA),
              ("VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA",
               pure VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA),
              ("VK_BLEND_FACTOR_CONSTANT_COLOR",
               pure VK_BLEND_FACTOR_CONSTANT_COLOR),
              ("VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR",
               pure VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR),
              ("VK_BLEND_FACTOR_CONSTANT_ALPHA",
               pure VK_BLEND_FACTOR_CONSTANT_ALPHA),
              ("VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA",
               pure VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA),
              ("VK_BLEND_FACTOR_SRC_ALPHA_SATURATE",
               pure VK_BLEND_FACTOR_SRC_ALPHA_SATURATE),
              ("VK_BLEND_FACTOR_SRC1_COLOR", pure VK_BLEND_FACTOR_SRC1_COLOR),
              ("VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR",
               pure VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR),
              ("VK_BLEND_FACTOR_SRC1_ALPHA", pure VK_BLEND_FACTOR_SRC1_ALPHA),
              ("VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA",
               pure VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA)]
             +++
             prec 10
               (expectP (Ident "VkBlendFactor") >>
                  (VkBlendFactor <$> step readPrec)))

pattern VK_BLEND_FACTOR_ZERO :: VkBlendFactor

pattern VK_BLEND_FACTOR_ZERO = VkBlendFactor 0

pattern VK_BLEND_FACTOR_ONE :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE = VkBlendFactor 1

pattern VK_BLEND_FACTOR_SRC_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_SRC_COLOR = VkBlendFactor 2

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = VkBlendFactor 3

pattern VK_BLEND_FACTOR_DST_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_DST_COLOR = VkBlendFactor 4

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR = VkBlendFactor 5

pattern VK_BLEND_FACTOR_SRC_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_SRC_ALPHA = VkBlendFactor 6

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = VkBlendFactor 7

pattern VK_BLEND_FACTOR_DST_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_DST_ALPHA = VkBlendFactor 8

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = VkBlendFactor 9

pattern VK_BLEND_FACTOR_CONSTANT_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_CONSTANT_COLOR = VkBlendFactor 10

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = VkBlendFactor 11

pattern VK_BLEND_FACTOR_CONSTANT_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_CONSTANT_ALPHA = VkBlendFactor 12

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = VkBlendFactor 13

pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE :: VkBlendFactor

pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE = VkBlendFactor 14

pattern VK_BLEND_FACTOR_SRC1_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_SRC1_COLOR = VkBlendFactor 15

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR = VkBlendFactor 16

pattern VK_BLEND_FACTOR_SRC1_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_SRC1_ALPHA = VkBlendFactor 17

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA :: VkBlendFactor

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA = VkBlendFactor 18

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBlendOp VkBlendOp registry at www.khronos.org>
newtype VkBlendOp = VkBlendOp Int32
                    deriving (Eq, Ord, Enum, Storable)

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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBlendOverlapEXT VkBlendOverlapEXT registry at www.khronos.org>
newtype VkBlendOverlapEXT = VkBlendOverlapEXT Int32
                            deriving (Eq, Ord, Enum, Storable)

instance Show VkBlendOverlapEXT where
    showsPrec _ VK_BLEND_OVERLAP_UNCORRELATED_EXT
      = showString "VK_BLEND_OVERLAP_UNCORRELATED_EXT"
    showsPrec _ VK_BLEND_OVERLAP_DISJOINT_EXT
      = showString "VK_BLEND_OVERLAP_DISJOINT_EXT"
    showsPrec _ VK_BLEND_OVERLAP_CONJOINT_EXT
      = showString "VK_BLEND_OVERLAP_CONJOINT_EXT"
    showsPrec p (VkBlendOverlapEXT x)
      = showParen (p >= 11)
          (showString "VkBlendOverlapEXT " . showsPrec 11 x)

instance Read VkBlendOverlapEXT where
    readPrec
      = parens
          (choose
             [("VK_BLEND_OVERLAP_UNCORRELATED_EXT",
               pure VK_BLEND_OVERLAP_UNCORRELATED_EXT),
              ("VK_BLEND_OVERLAP_DISJOINT_EXT",
               pure VK_BLEND_OVERLAP_DISJOINT_EXT),
              ("VK_BLEND_OVERLAP_CONJOINT_EXT",
               pure VK_BLEND_OVERLAP_CONJOINT_EXT)]
             +++
             prec 10
               (expectP (Ident "VkBlendOverlapEXT") >>
                  (VkBlendOverlapEXT <$> step readPrec)))

pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT = VkBlendOverlapEXT 0

pattern VK_BLEND_OVERLAP_DISJOINT_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_DISJOINT_EXT = VkBlendOverlapEXT 1

pattern VK_BLEND_OVERLAP_CONJOINT_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_CONJOINT_EXT = VkBlendOverlapEXT 2
