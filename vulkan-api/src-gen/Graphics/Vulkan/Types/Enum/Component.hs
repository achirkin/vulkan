{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.Component
       (VkComponentSwizzle(VkComponentSwizzle,
                           VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_ZERO,
                           VK_COMPONENT_SWIZZLE_ONE, VK_COMPONENT_SWIZZLE_R,
                           VK_COMPONENT_SWIZZLE_G, VK_COMPONENT_SWIZZLE_B,
                           VK_COMPONENT_SWIZZLE_A),
        VkComponentTypeNV(VkComponentTypeNV, VK_COMPONENT_TYPE_FLOAT16_NV,
                          VK_COMPONENT_TYPE_FLOAT32_NV, VK_COMPONENT_TYPE_FLOAT64_NV,
                          VK_COMPONENT_TYPE_SINT8_NV, VK_COMPONENT_TYPE_SINT16_NV,
                          VK_COMPONENT_TYPE_SINT32_NV, VK_COMPONENT_TYPE_SINT64_NV,
                          VK_COMPONENT_TYPE_UINT8_NV, VK_COMPONENT_TYPE_UINT16_NV,
                          VK_COMPONENT_TYPE_UINT32_NV, VK_COMPONENT_TYPE_UINT64_NV))
       where
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (Int32)
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkComponentSwizzle VkComponentSwizzle registry at www.khronos.org>
newtype VkComponentSwizzle = VkComponentSwizzle Int32
                             deriving (Eq, Ord, Enum, Storable)

instance Show VkComponentSwizzle where
    showsPrec _ VK_COMPONENT_SWIZZLE_IDENTITY
      = showString "VK_COMPONENT_SWIZZLE_IDENTITY"
    showsPrec _ VK_COMPONENT_SWIZZLE_ZERO
      = showString "VK_COMPONENT_SWIZZLE_ZERO"
    showsPrec _ VK_COMPONENT_SWIZZLE_ONE
      = showString "VK_COMPONENT_SWIZZLE_ONE"
    showsPrec _ VK_COMPONENT_SWIZZLE_R
      = showString "VK_COMPONENT_SWIZZLE_R"
    showsPrec _ VK_COMPONENT_SWIZZLE_G
      = showString "VK_COMPONENT_SWIZZLE_G"
    showsPrec _ VK_COMPONENT_SWIZZLE_B
      = showString "VK_COMPONENT_SWIZZLE_B"
    showsPrec _ VK_COMPONENT_SWIZZLE_A
      = showString "VK_COMPONENT_SWIZZLE_A"
    showsPrec p (VkComponentSwizzle x)
      = showParen (p >= 11)
          (showString "VkComponentSwizzle " . showsPrec 11 x)

instance Read VkComponentSwizzle where
    readPrec
      = parens
          (choose
             [("VK_COMPONENT_SWIZZLE_IDENTITY",
               pure VK_COMPONENT_SWIZZLE_IDENTITY),
              ("VK_COMPONENT_SWIZZLE_ZERO", pure VK_COMPONENT_SWIZZLE_ZERO),
              ("VK_COMPONENT_SWIZZLE_ONE", pure VK_COMPONENT_SWIZZLE_ONE),
              ("VK_COMPONENT_SWIZZLE_R", pure VK_COMPONENT_SWIZZLE_R),
              ("VK_COMPONENT_SWIZZLE_G", pure VK_COMPONENT_SWIZZLE_G),
              ("VK_COMPONENT_SWIZZLE_B", pure VK_COMPONENT_SWIZZLE_B),
              ("VK_COMPONENT_SWIZZLE_A", pure VK_COMPONENT_SWIZZLE_A)]
             +++
             prec 10
               (expectP (Ident "VkComponentSwizzle") >>
                  (VkComponentSwizzle <$> step readPrec)))

pattern VK_COMPONENT_SWIZZLE_IDENTITY :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_IDENTITY = VkComponentSwizzle 0

pattern VK_COMPONENT_SWIZZLE_ZERO :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_ZERO = VkComponentSwizzle 1

pattern VK_COMPONENT_SWIZZLE_ONE :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_ONE = VkComponentSwizzle 2

pattern VK_COMPONENT_SWIZZLE_R :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_R = VkComponentSwizzle 3

pattern VK_COMPONENT_SWIZZLE_G :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_G = VkComponentSwizzle 4

pattern VK_COMPONENT_SWIZZLE_B :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_B = VkComponentSwizzle 5

pattern VK_COMPONENT_SWIZZLE_A :: VkComponentSwizzle

pattern VK_COMPONENT_SWIZZLE_A = VkComponentSwizzle 6

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkComponentTypeNV VkComponentTypeNV registry at www.khronos.org>
newtype VkComponentTypeNV = VkComponentTypeNV Int32
                            deriving (Eq, Ord, Enum, Storable)

instance Show VkComponentTypeNV where
    showsPrec _ VK_COMPONENT_TYPE_FLOAT16_NV
      = showString "VK_COMPONENT_TYPE_FLOAT16_NV"
    showsPrec _ VK_COMPONENT_TYPE_FLOAT32_NV
      = showString "VK_COMPONENT_TYPE_FLOAT32_NV"
    showsPrec _ VK_COMPONENT_TYPE_FLOAT64_NV
      = showString "VK_COMPONENT_TYPE_FLOAT64_NV"
    showsPrec _ VK_COMPONENT_TYPE_SINT8_NV
      = showString "VK_COMPONENT_TYPE_SINT8_NV"
    showsPrec _ VK_COMPONENT_TYPE_SINT16_NV
      = showString "VK_COMPONENT_TYPE_SINT16_NV"
    showsPrec _ VK_COMPONENT_TYPE_SINT32_NV
      = showString "VK_COMPONENT_TYPE_SINT32_NV"
    showsPrec _ VK_COMPONENT_TYPE_SINT64_NV
      = showString "VK_COMPONENT_TYPE_SINT64_NV"
    showsPrec _ VK_COMPONENT_TYPE_UINT8_NV
      = showString "VK_COMPONENT_TYPE_UINT8_NV"
    showsPrec _ VK_COMPONENT_TYPE_UINT16_NV
      = showString "VK_COMPONENT_TYPE_UINT16_NV"
    showsPrec _ VK_COMPONENT_TYPE_UINT32_NV
      = showString "VK_COMPONENT_TYPE_UINT32_NV"
    showsPrec _ VK_COMPONENT_TYPE_UINT64_NV
      = showString "VK_COMPONENT_TYPE_UINT64_NV"
    showsPrec p (VkComponentTypeNV x)
      = showParen (p >= 11)
          (showString "VkComponentTypeNV " . showsPrec 11 x)

instance Read VkComponentTypeNV where
    readPrec
      = parens
          (choose
             [("VK_COMPONENT_TYPE_FLOAT16_NV",
               pure VK_COMPONENT_TYPE_FLOAT16_NV),
              ("VK_COMPONENT_TYPE_FLOAT32_NV",
               pure VK_COMPONENT_TYPE_FLOAT32_NV),
              ("VK_COMPONENT_TYPE_FLOAT64_NV",
               pure VK_COMPONENT_TYPE_FLOAT64_NV),
              ("VK_COMPONENT_TYPE_SINT8_NV", pure VK_COMPONENT_TYPE_SINT8_NV),
              ("VK_COMPONENT_TYPE_SINT16_NV", pure VK_COMPONENT_TYPE_SINT16_NV),
              ("VK_COMPONENT_TYPE_SINT32_NV", pure VK_COMPONENT_TYPE_SINT32_NV),
              ("VK_COMPONENT_TYPE_SINT64_NV", pure VK_COMPONENT_TYPE_SINT64_NV),
              ("VK_COMPONENT_TYPE_UINT8_NV", pure VK_COMPONENT_TYPE_UINT8_NV),
              ("VK_COMPONENT_TYPE_UINT16_NV", pure VK_COMPONENT_TYPE_UINT16_NV),
              ("VK_COMPONENT_TYPE_UINT32_NV", pure VK_COMPONENT_TYPE_UINT32_NV),
              ("VK_COMPONENT_TYPE_UINT64_NV", pure VK_COMPONENT_TYPE_UINT64_NV)]
             +++
             prec 10
               (expectP (Ident "VkComponentTypeNV") >>
                  (VkComponentTypeNV <$> step readPrec)))

pattern VK_COMPONENT_TYPE_FLOAT16_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_FLOAT16_NV = VkComponentTypeNV 0

pattern VK_COMPONENT_TYPE_FLOAT32_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_FLOAT32_NV = VkComponentTypeNV 1

pattern VK_COMPONENT_TYPE_FLOAT64_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_FLOAT64_NV = VkComponentTypeNV 2

pattern VK_COMPONENT_TYPE_SINT8_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_SINT8_NV = VkComponentTypeNV 3

pattern VK_COMPONENT_TYPE_SINT16_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_SINT16_NV = VkComponentTypeNV 4

pattern VK_COMPONENT_TYPE_SINT32_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_SINT32_NV = VkComponentTypeNV 5

pattern VK_COMPONENT_TYPE_SINT64_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_SINT64_NV = VkComponentTypeNV 6

pattern VK_COMPONENT_TYPE_UINT8_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_UINT8_NV = VkComponentTypeNV 7

pattern VK_COMPONENT_TYPE_UINT16_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_UINT16_NV = VkComponentTypeNV 8

pattern VK_COMPONENT_TYPE_UINT32_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_UINT32_NV = VkComponentTypeNV 9

pattern VK_COMPONENT_TYPE_UINT64_NV :: VkComponentTypeNV

pattern VK_COMPONENT_TYPE_UINT64_NV = VkComponentTypeNV 10
