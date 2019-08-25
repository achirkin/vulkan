{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.ViewportCoordinateSwizzleNV
       (VkViewportCoordinateSwizzleNV(VkViewportCoordinateSwizzleNV,
                                      VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV,
                                      VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV,
                                      VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV,
                                      VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV,
                                      VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV,
                                      VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV,
                                      VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV,
                                      VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkViewportCoordinateSwizzleNV VkViewportCoordinateSwizzleNV registry at www.khronos.org>
newtype VkViewportCoordinateSwizzleNV = VkViewportCoordinateSwizzleNV Int32
                                        deriving (Eq, Ord, Enum, Storable)

instance Show VkViewportCoordinateSwizzleNV where
    showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV
      = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV"
    showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV
      = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV"
    showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV
      = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV"
    showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV
      = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV"
    showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV
      = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV"
    showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV
      = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV"
    showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV
      = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV"
    showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV
      = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV"
    showsPrec p (VkViewportCoordinateSwizzleNV x)
      = showParen (p >= 11)
          (showString "VkViewportCoordinateSwizzleNV " . showsPrec 11 x)

instance Read VkViewportCoordinateSwizzleNV where
    readPrec
      = parens
          (choose
             [("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV",
               pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV),
              ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV",
               pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV),
              ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV",
               pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV),
              ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV",
               pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV),
              ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV",
               pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV),
              ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV",
               pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV),
              ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV",
               pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV),
              ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV",
               pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV)]
             +++
             prec 10
               (expectP (Ident "VkViewportCoordinateSwizzleNV") >>
                  (VkViewportCoordinateSwizzleNV <$> step readPrec)))

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV =
        VkViewportCoordinateSwizzleNV 0

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV =
        VkViewportCoordinateSwizzleNV 1

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV =
        VkViewportCoordinateSwizzleNV 2

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV =
        VkViewportCoordinateSwizzleNV 3

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV =
        VkViewportCoordinateSwizzleNV 4

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV =
        VkViewportCoordinateSwizzleNV 5

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV =
        VkViewportCoordinateSwizzleNV 6

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV ::
        VkViewportCoordinateSwizzleNV

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV =
        VkViewportCoordinateSwizzleNV 7
