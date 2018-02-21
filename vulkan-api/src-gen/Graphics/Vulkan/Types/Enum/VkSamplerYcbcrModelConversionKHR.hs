{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversionKHR
       (VkSamplerYcbcrModelConversionKHR(VkSamplerYcbcrModelConversionKHR,
                                         VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR,
                                         VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR,
                                         VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR,
                                         VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR,
                                         VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSamplerYcbcrModelConversionKHR.html VkSamplerYcbcrModelConversionKHR registry at www.khronos.org>
newtype VkSamplerYcbcrModelConversionKHR = VkSamplerYcbcrModelConversionKHR Int32
                                             deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                       Generic)

instance Show VkSamplerYcbcrModelConversionKHR where
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR"
        showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR
          = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR"
        showsPrec p (VkSamplerYcbcrModelConversionKHR x)
          = showParen (p >= 11)
              (showString "VkSamplerYcbcrModelConversionKHR " . showsPrec 11 x)

instance Read VkSamplerYcbcrModelConversionKHR where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR),
                  ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR",
                   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerYcbcrModelConversionKHR") >>
                      (VkSamplerYcbcrModelConversionKHR <$> step readPrec)))

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR ::
        VkSamplerYcbcrModelConversionKHR

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR =
        VkSamplerYcbcrModelConversionKHR 0

-- | just range expansion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR ::
        VkSamplerYcbcrModelConversionKHR

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR =
        VkSamplerYcbcrModelConversionKHR 1

-- | aka HD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR ::
        VkSamplerYcbcrModelConversionKHR

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR =
        VkSamplerYcbcrModelConversionKHR 2

-- | aka SD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR ::
        VkSamplerYcbcrModelConversionKHR

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR =
        VkSamplerYcbcrModelConversionKHR 3

-- | aka UHD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR ::
        VkSamplerYcbcrModelConversionKHR

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR =
        VkSamplerYcbcrModelConversionKHR 4
