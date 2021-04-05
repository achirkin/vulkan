{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.LineRasterizationModeEXT
       (VkLineRasterizationModeEXT(VkLineRasterizationModeEXT,
                                   VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT,
                                   VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT,
                                   VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT,
                                   VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT))
       where
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (Int32)
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkLineRasterizationModeEXT VkLineRasterizationModeEXT registry at www.khronos.org>
newtype VkLineRasterizationModeEXT = VkLineRasterizationModeEXT Int32
                                     deriving (Eq, Ord, Enum, Storable)

instance Show VkLineRasterizationModeEXT where
    showsPrec _ VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT
      = showString "VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT"
    showsPrec _ VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT
      = showString "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT"
    showsPrec _ VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT
      = showString "VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT"
    showsPrec _ VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT
      = showString "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT"
    showsPrec p (VkLineRasterizationModeEXT x)
      = showParen (p >= 11)
          (showString "VkLineRasterizationModeEXT " . showsPrec 11 x)

instance Read VkLineRasterizationModeEXT where
    readPrec
      = parens
          (choose
             [("VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT",
               pure VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT),
              ("VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT",
               pure VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT),
              ("VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT",
               pure VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT),
              ("VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT",
               pure VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT)]
             +++
             prec 10
               (expectP (Ident "VkLineRasterizationModeEXT") >>
                  (VkLineRasterizationModeEXT <$> step readPrec)))

pattern VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT ::
        VkLineRasterizationModeEXT

pattern VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT =
        VkLineRasterizationModeEXT 0

pattern VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT ::
        VkLineRasterizationModeEXT

pattern VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT =
        VkLineRasterizationModeEXT 1

pattern VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT ::
        VkLineRasterizationModeEXT

pattern VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT =
        VkLineRasterizationModeEXT 2

pattern VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT ::
        VkLineRasterizationModeEXT

pattern VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT =
        VkLineRasterizationModeEXT 3
