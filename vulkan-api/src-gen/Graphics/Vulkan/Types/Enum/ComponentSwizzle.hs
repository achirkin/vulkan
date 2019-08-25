{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.ComponentSwizzle
       (VkComponentSwizzle(VkComponentSwizzle,
                           VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_ZERO,
                           VK_COMPONENT_SWIZZLE_ONE, VK_COMPONENT_SWIZZLE_R,
                           VK_COMPONENT_SWIZZLE_G, VK_COMPONENT_SWIZZLE_B,
                           VK_COMPONENT_SWIZZLE_A))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkComponentSwizzle VkComponentSwizzle registry at www.khronos.org>
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
