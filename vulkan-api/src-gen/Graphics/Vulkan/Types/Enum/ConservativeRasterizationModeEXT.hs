{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.ConservativeRasterizationModeEXT
       (VkConservativeRasterizationModeEXT(VkConservativeRasterizationModeEXT,
                                           VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT,
                                           VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT,
                                           VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkConservativeRasterizationModeEXT VkConservativeRasterizationModeEXT registry at www.khronos.org>
newtype VkConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT Int32
                                             deriving (Eq, Ord, Enum, Storable)

instance Show VkConservativeRasterizationModeEXT where
    showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
      = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT"
    showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
      = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT"
    showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
      = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT"
    showsPrec p (VkConservativeRasterizationModeEXT x)
      = showParen (p >= 11)
          (showString "VkConservativeRasterizationModeEXT " . showsPrec 11 x)

instance Read VkConservativeRasterizationModeEXT where
    readPrec
      = parens
          (choose
             [("VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT",
               pure VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT),
              ("VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT",
               pure VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT),
              ("VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT",
               pure VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT)]
             +++
             prec 10
               (expectP (Ident "VkConservativeRasterizationModeEXT") >>
                  (VkConservativeRasterizationModeEXT <$> step readPrec)))

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT ::
        VkConservativeRasterizationModeEXT

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT =
        VkConservativeRasterizationModeEXT 0

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT ::
        VkConservativeRasterizationModeEXT

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT =
        VkConservativeRasterizationModeEXT 1

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT ::
        VkConservativeRasterizationModeEXT

pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT =
        VkConservativeRasterizationModeEXT 2
