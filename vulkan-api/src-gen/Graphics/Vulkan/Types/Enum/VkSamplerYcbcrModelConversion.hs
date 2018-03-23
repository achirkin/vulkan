{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversion
       (VkSamplerYcbcrModelConversion(VkSamplerYcbcrModelConversion,
                                      VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY,
                                      VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY,
                                      VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709,
                                      VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601,
                                      VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSamplerYcbcrModelConversion.html VkSamplerYcbcrModelConversion registry at www.khronos.org>
newtype VkSamplerYcbcrModelConversion = VkSamplerYcbcrModelConversion Int32
                                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                    Generic)

instance Show VkSamplerYcbcrModelConversion where
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020"
        showsPrec p (VkSamplerYcbcrModelConversion x)
          = showParen (p >= 11)
              (showString "VkSamplerYcbcrModelConversion " . showsPrec 11 x)

instance Read VkSamplerYcbcrModelConversion where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerYcbcrModelConversion") >>
                      (VkSamplerYcbcrModelConversion <$> step readPrec)))

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY ::
        VkSamplerYcbcrModelConversion

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY =
        VkSamplerYcbcrModelConversion 0

-- | just range expansion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY ::
        VkSamplerYcbcrModelConversion

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY =
        VkSamplerYcbcrModelConversion 1

-- | aka HD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 ::
        VkSamplerYcbcrModelConversion

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 =
        VkSamplerYcbcrModelConversion 2

-- | aka SD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 ::
        VkSamplerYcbcrModelConversion

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 =
        VkSamplerYcbcrModelConversion 3

-- | aka UHD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 ::
        VkSamplerYcbcrModelConversion

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 =
        VkSamplerYcbcrModelConversion 4
