{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.RenderPassCreateFlags
       (VkRenderPassCreateBitmask(VkRenderPassCreateBitmask,
                                  VkRenderPassCreateFlags, VkRenderPassCreateFlagBits),
        VkRenderPassCreateFlags, VkRenderPassCreateFlagBits)
       where
import Data.Bits                       (Bits, FiniteBits)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

newtype VkRenderPassCreateBitmask (a ::
                                     FlagType) = VkRenderPassCreateBitmask VkFlags
                                                 deriving (Eq, Ord, Storable)

type VkRenderPassCreateFlags = VkRenderPassCreateBitmask FlagMask

type VkRenderPassCreateFlagBits = VkRenderPassCreateBitmask FlagBit

pattern VkRenderPassCreateFlagBits ::
        VkFlags -> VkRenderPassCreateBitmask FlagBit

pattern VkRenderPassCreateFlagBits n = VkRenderPassCreateBitmask n

pattern VkRenderPassCreateFlags ::
        VkFlags -> VkRenderPassCreateBitmask FlagMask

pattern VkRenderPassCreateFlags n = VkRenderPassCreateBitmask n

deriving instance Bits (VkRenderPassCreateBitmask FlagMask)

deriving instance FiniteBits (VkRenderPassCreateBitmask FlagMask)

instance Show (VkRenderPassCreateBitmask a) where
    showsPrec p (VkRenderPassCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkRenderPassCreateBitmask " . showsPrec 11 x)

instance Read (VkRenderPassCreateBitmask a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkRenderPassCreateBitmask") >>
                  (VkRenderPassCreateBitmask <$> step readPrec)))
