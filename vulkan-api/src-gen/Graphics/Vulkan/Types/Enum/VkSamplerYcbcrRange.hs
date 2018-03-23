{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRange
       (VkSamplerYcbcrRange(VkSamplerYcbcrRange,
                            VK_SAMPLER_YCBCR_RANGE_ITU_FULL,
                            VK_SAMPLER_YCBCR_RANGE_ITU_NARROW))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSamplerYcbcrRange.html VkSamplerYcbcrRange registry at www.khronos.org>
newtype VkSamplerYcbcrRange = VkSamplerYcbcrRange Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSamplerYcbcrRange where
        showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_FULL
          = showString "VK_SAMPLER_YCBCR_RANGE_ITU_FULL"
        showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_NARROW
          = showString "VK_SAMPLER_YCBCR_RANGE_ITU_NARROW"
        showsPrec p (VkSamplerYcbcrRange x)
          = showParen (p >= 11)
              (showString "VkSamplerYcbcrRange " . showsPrec 11 x)

instance Read VkSamplerYcbcrRange where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_YCBCR_RANGE_ITU_FULL",
                   pure VK_SAMPLER_YCBCR_RANGE_ITU_FULL),
                  ("VK_SAMPLER_YCBCR_RANGE_ITU_NARROW",
                   pure VK_SAMPLER_YCBCR_RANGE_ITU_NARROW)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerYcbcrRange") >>
                      (VkSamplerYcbcrRange <$> step readPrec)))

-- | Luma 0..1 maps to 0..255, chroma -0.5..0.5 to 1..255 (clamped)
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL :: VkSamplerYcbcrRange

pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL = VkSamplerYcbcrRange 0

-- | Luma 0..1 maps to 16..235, chroma -0.5..0.5 to 16..240
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW :: VkSamplerYcbcrRange

pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW = VkSamplerYcbcrRange 1
