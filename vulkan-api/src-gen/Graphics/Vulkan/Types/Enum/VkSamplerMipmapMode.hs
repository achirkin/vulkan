{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSamplerMipmapMode
       (VkSamplerMipmapMode(VkSamplerMipmapMode,
                            VK_SAMPLER_MIPMAP_MODE_NEAREST, VK_SAMPLER_MIPMAP_MODE_LINEAR))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerMipmapModeVkSamplerMipmapMode registry at www.khronos.org>
newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSamplerMipmapMode where
        showsPrec _ VK_SAMPLER_MIPMAP_MODE_NEAREST
          = showString "VK_SAMPLER_MIPMAP_MODE_NEAREST"
        showsPrec _ VK_SAMPLER_MIPMAP_MODE_LINEAR
          = showString "VK_SAMPLER_MIPMAP_MODE_LINEAR"
        showsPrec p (VkSamplerMipmapMode x)
          = showParen (p >= 11)
              (showString "VkSamplerMipmapMode " . showsPrec 11 x)

instance Read VkSamplerMipmapMode where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_MIPMAP_MODE_NEAREST",
                   pure VK_SAMPLER_MIPMAP_MODE_NEAREST),
                  ("VK_SAMPLER_MIPMAP_MODE_LINEAR",
                   pure VK_SAMPLER_MIPMAP_MODE_LINEAR)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerMipmapMode") >>
                      (VkSamplerMipmapMode <$> step readPrec)))

-- | Choose nearest mip level
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST :: VkSamplerMipmapMode

pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0

-- | Linear filter between mip levels
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR :: VkSamplerMipmapMode

pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1
