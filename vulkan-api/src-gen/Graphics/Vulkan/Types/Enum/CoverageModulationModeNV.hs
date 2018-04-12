{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.CoverageModulationModeNV
       (VkCoverageModulationModeNV(VkCoverageModulationModeNV,
                                   VK_COVERAGE_MODULATION_MODE_NONE_NV,
                                   VK_COVERAGE_MODULATION_MODE_RGB_NV,
                                   VK_COVERAGE_MODULATION_MODE_ALPHA_NV,
                                   VK_COVERAGE_MODULATION_MODE_RGBA_NV))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCoverageModulationModeNV VkCoverageModulationModeNV registry at www.khronos.org>
newtype VkCoverageModulationModeNV = VkCoverageModulationModeNV Int32
                                       deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                 Generic)

instance Show VkCoverageModulationModeNV where
        showsPrec _ VK_COVERAGE_MODULATION_MODE_NONE_NV
          = showString "VK_COVERAGE_MODULATION_MODE_NONE_NV"
        showsPrec _ VK_COVERAGE_MODULATION_MODE_RGB_NV
          = showString "VK_COVERAGE_MODULATION_MODE_RGB_NV"
        showsPrec _ VK_COVERAGE_MODULATION_MODE_ALPHA_NV
          = showString "VK_COVERAGE_MODULATION_MODE_ALPHA_NV"
        showsPrec _ VK_COVERAGE_MODULATION_MODE_RGBA_NV
          = showString "VK_COVERAGE_MODULATION_MODE_RGBA_NV"
        showsPrec p (VkCoverageModulationModeNV x)
          = showParen (p >= 11)
              (showString "VkCoverageModulationModeNV " . showsPrec 11 x)

instance Read VkCoverageModulationModeNV where
        readPrec
          = parens
              (choose
                 [("VK_COVERAGE_MODULATION_MODE_NONE_NV",
                   pure VK_COVERAGE_MODULATION_MODE_NONE_NV),
                  ("VK_COVERAGE_MODULATION_MODE_RGB_NV",
                   pure VK_COVERAGE_MODULATION_MODE_RGB_NV),
                  ("VK_COVERAGE_MODULATION_MODE_ALPHA_NV",
                   pure VK_COVERAGE_MODULATION_MODE_ALPHA_NV),
                  ("VK_COVERAGE_MODULATION_MODE_RGBA_NV",
                   pure VK_COVERAGE_MODULATION_MODE_RGBA_NV)]
                 +++
                 prec 10
                   (expectP (Ident "VkCoverageModulationModeNV") >>
                      (VkCoverageModulationModeNV <$> step readPrec)))

pattern VK_COVERAGE_MODULATION_MODE_NONE_NV ::
        VkCoverageModulationModeNV

pattern VK_COVERAGE_MODULATION_MODE_NONE_NV =
        VkCoverageModulationModeNV 0

pattern VK_COVERAGE_MODULATION_MODE_RGB_NV ::
        VkCoverageModulationModeNV

pattern VK_COVERAGE_MODULATION_MODE_RGB_NV =
        VkCoverageModulationModeNV 1

pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV ::
        VkCoverageModulationModeNV

pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV =
        VkCoverageModulationModeNV 2

pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV ::
        VkCoverageModulationModeNV

pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV =
        VkCoverageModulationModeNV 3
