{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSamplerAddressMode
       (VkSamplerAddressMode(VkSamplerAddressMode,
                             VK_SAMPLER_ADDRESS_MODE_REPEAT,
                             VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT,
                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerAddressModeVkSamplerAddressMode registry at www.khronos.org>
newtype VkSamplerAddressMode = VkSamplerAddressMode Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSamplerAddressMode where
        showsPrec _ VK_SAMPLER_ADDRESS_MODE_REPEAT
          = showString "VK_SAMPLER_ADDRESS_MODE_REPEAT"
        showsPrec _ VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
          = showString "VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT"
        showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
          = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE"
        showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
          = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER"
        showsPrec p (VkSamplerAddressMode x)
          = showParen (p >= 11)
              (showString "VkSamplerAddressMode " . showsPrec 11 x)

instance Read VkSamplerAddressMode where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_ADDRESS_MODE_REPEAT",
                   pure VK_SAMPLER_ADDRESS_MODE_REPEAT),
                  ("VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT",
                   pure VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT),
                  ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE",
                   pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE),
                  ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER",
                   pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerAddressMode") >>
                      (VkSamplerAddressMode <$> step readPrec)))

pattern VK_SAMPLER_ADDRESS_MODE_REPEAT :: VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = VkSamplerAddressMode 0

pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT =
        VkSamplerAddressMode 1

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE =
        VkSamplerAddressMode 2

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER =
        VkSamplerAddressMode 3
