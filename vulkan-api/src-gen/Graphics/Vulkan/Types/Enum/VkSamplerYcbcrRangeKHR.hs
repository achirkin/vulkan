{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRangeKHR
       (VkSamplerYcbcrRangeKHR(VkSamplerYcbcrRangeKHR,
                               VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR,
                               VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSamplerYcbcrRangeKHR.html VkSamplerYcbcrRangeKHR registry at www.khronos.org>
newtype VkSamplerYcbcrRangeKHR = VkSamplerYcbcrRangeKHR Int32
                                   deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSamplerYcbcrRangeKHR where
        showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR
          = showString "VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR"
        showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR
          = showString "VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR"
        showsPrec p (VkSamplerYcbcrRangeKHR x)
          = showParen (p >= 11)
              (showString "VkSamplerYcbcrRangeKHR " . showsPrec 11 x)

instance Read VkSamplerYcbcrRangeKHR where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR",
                   pure VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR),
                  ("VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR",
                   pure VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerYcbcrRangeKHR") >>
                      (VkSamplerYcbcrRangeKHR <$> step readPrec)))

-- | Luma 0..1 maps to 0..255, chroma -0.5..0.5 to 1..255 (clamped)
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR ::
        VkSamplerYcbcrRangeKHR

pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR =
        VkSamplerYcbcrRangeKHR 0

-- | Luma 0..1 maps to 16..235, chroma -0.5..0.5 to 16..240
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR ::
        VkSamplerYcbcrRangeKHR

pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR =
        VkSamplerYcbcrRangeKHR 1
