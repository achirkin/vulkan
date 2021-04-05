{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.CoarseSampleOrderTypeNV
       (VkCoarseSampleOrderTypeNV(VkCoarseSampleOrderTypeNV,
                                  VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV,
                                  VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV,
                                  VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV,
                                  VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV))
       where
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (Int32)
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCoarseSampleOrderTypeNV VkCoarseSampleOrderTypeNV registry at www.khronos.org>
newtype VkCoarseSampleOrderTypeNV = VkCoarseSampleOrderTypeNV Int32
                                    deriving (Eq, Ord, Enum, Storable)

instance Show VkCoarseSampleOrderTypeNV where
    showsPrec _ VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV
      = showString "VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV"
    showsPrec _ VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV
      = showString "VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV"
    showsPrec _ VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV
      = showString "VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV"
    showsPrec _ VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV
      = showString "VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV"
    showsPrec p (VkCoarseSampleOrderTypeNV x)
      = showParen (p >= 11)
          (showString "VkCoarseSampleOrderTypeNV " . showsPrec 11 x)

instance Read VkCoarseSampleOrderTypeNV where
    readPrec
      = parens
          (choose
             [("VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV",
               pure VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV),
              ("VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV",
               pure VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV),
              ("VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV",
               pure VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV),
              ("VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV",
               pure VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV)]
             +++
             prec 10
               (expectP (Ident "VkCoarseSampleOrderTypeNV") >>
                  (VkCoarseSampleOrderTypeNV <$> step readPrec)))

pattern VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV ::
        VkCoarseSampleOrderTypeNV

pattern VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV =
        VkCoarseSampleOrderTypeNV 0

pattern VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV ::
        VkCoarseSampleOrderTypeNV

pattern VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV =
        VkCoarseSampleOrderTypeNV 1

pattern VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV ::
        VkCoarseSampleOrderTypeNV

pattern VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV =
        VkCoarseSampleOrderTypeNV 2

pattern VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV ::
        VkCoarseSampleOrderTypeNV

pattern VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV =
        VkCoarseSampleOrderTypeNV 3
