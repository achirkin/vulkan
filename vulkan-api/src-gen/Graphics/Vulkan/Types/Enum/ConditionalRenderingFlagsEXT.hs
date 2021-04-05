{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.ConditionalRenderingFlagsEXT
       (VkConditionalRenderingBitmaskEXT(VkConditionalRenderingBitmaskEXT,
                                         VkConditionalRenderingFlagsEXT,
                                         VkConditionalRenderingFlagBitsEXT,
                                         VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT),
        VkConditionalRenderingFlagsEXT, VkConditionalRenderingFlagBitsEXT)
       where
import Data.Bits                       (Bits, FiniteBits)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

newtype VkConditionalRenderingBitmaskEXT (a ::
                                            FlagType) = VkConditionalRenderingBitmaskEXT VkFlags
                                                        deriving (Eq, Ord, Storable)

type VkConditionalRenderingFlagsEXT =
     VkConditionalRenderingBitmaskEXT FlagMask

type VkConditionalRenderingFlagBitsEXT =
     VkConditionalRenderingBitmaskEXT FlagBit

pattern VkConditionalRenderingFlagBitsEXT ::
        VkFlags -> VkConditionalRenderingBitmaskEXT FlagBit

pattern VkConditionalRenderingFlagBitsEXT n =
        VkConditionalRenderingBitmaskEXT n

pattern VkConditionalRenderingFlagsEXT ::
        VkFlags -> VkConditionalRenderingBitmaskEXT FlagMask

pattern VkConditionalRenderingFlagsEXT n =
        VkConditionalRenderingBitmaskEXT n

deriving instance Bits (VkConditionalRenderingBitmaskEXT FlagMask)

deriving instance
         FiniteBits (VkConditionalRenderingBitmaskEXT FlagMask)

instance Show (VkConditionalRenderingBitmaskEXT a) where
    showsPrec _ VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT
      = showString "VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT"
    showsPrec p (VkConditionalRenderingBitmaskEXT x)
      = showParen (p >= 11)
          (showString "VkConditionalRenderingBitmaskEXT " . showsPrec 11 x)

instance Read (VkConditionalRenderingBitmaskEXT a) where
    readPrec
      = parens
          (choose
             [("VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT",
               pure VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT)]
             +++
             prec 10
               (expectP (Ident "VkConditionalRenderingBitmaskEXT") >>
                  (VkConditionalRenderingBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT ::
        VkConditionalRenderingBitmaskEXT a

pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT =
        VkConditionalRenderingBitmaskEXT 1
