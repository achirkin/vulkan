{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSamplerReductionModeEXT
       (VkSamplerReductionModeEXT(VkSamplerReductionModeEXT,
                                  VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT,
                                  VK_SAMPLER_REDUCTION_MODE_MIN_EXT,
                                  VK_SAMPLER_REDUCTION_MODE_MAX_EXT))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSamplerReductionModeEXT.html VkSamplerReductionModeEXT registry at www.khronos.org>
newtype VkSamplerReductionModeEXT = VkSamplerReductionModeEXT Int32
                                      deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                Generic)

instance Show VkSamplerReductionModeEXT where
        showsPrec _ VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT
          = showString "VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT"
        showsPrec _ VK_SAMPLER_REDUCTION_MODE_MIN_EXT
          = showString "VK_SAMPLER_REDUCTION_MODE_MIN_EXT"
        showsPrec _ VK_SAMPLER_REDUCTION_MODE_MAX_EXT
          = showString "VK_SAMPLER_REDUCTION_MODE_MAX_EXT"
        showsPrec p (VkSamplerReductionModeEXT x)
          = showParen (p >= 11)
              (showString "VkSamplerReductionModeEXT " . showsPrec 11 x)

instance Read VkSamplerReductionModeEXT where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT",
                   pure VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT),
                  ("VK_SAMPLER_REDUCTION_MODE_MIN_EXT",
                   pure VK_SAMPLER_REDUCTION_MODE_MIN_EXT),
                  ("VK_SAMPLER_REDUCTION_MODE_MAX_EXT",
                   pure VK_SAMPLER_REDUCTION_MODE_MAX_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerReductionModeEXT") >>
                      (VkSamplerReductionModeEXT <$> step readPrec)))

pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT ::
        VkSamplerReductionModeEXT

pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT =
        VkSamplerReductionModeEXT 0

pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT ::
        VkSamplerReductionModeEXT

pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT =
        VkSamplerReductionModeEXT 1

pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT ::
        VkSamplerReductionModeEXT

pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT =
        VkSamplerReductionModeEXT 2
