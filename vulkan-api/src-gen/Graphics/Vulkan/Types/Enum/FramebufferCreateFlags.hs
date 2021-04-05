{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.FramebufferCreateFlags
       (VkFramebufferCreateBitmask(VkFramebufferCreateBitmask,
                                   VkFramebufferCreateFlags, VkFramebufferCreateFlagBits),
        VkFramebufferCreateFlags, VkFramebufferCreateFlagBits)
       where
import Data.Bits                       (Bits, FiniteBits)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

newtype VkFramebufferCreateBitmask (a ::
                                      FlagType) = VkFramebufferCreateBitmask VkFlags
                                                  deriving (Eq, Ord, Storable)

type VkFramebufferCreateFlags = VkFramebufferCreateBitmask FlagMask

type VkFramebufferCreateFlagBits =
     VkFramebufferCreateBitmask FlagBit

pattern VkFramebufferCreateFlagBits ::
        VkFlags -> VkFramebufferCreateBitmask FlagBit

pattern VkFramebufferCreateFlagBits n =
        VkFramebufferCreateBitmask n

pattern VkFramebufferCreateFlags ::
        VkFlags -> VkFramebufferCreateBitmask FlagMask

pattern VkFramebufferCreateFlags n = VkFramebufferCreateBitmask n

deriving instance Bits (VkFramebufferCreateBitmask FlagMask)

deriving instance FiniteBits (VkFramebufferCreateBitmask FlagMask)

instance Show (VkFramebufferCreateBitmask a) where
    showsPrec p (VkFramebufferCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkFramebufferCreateBitmask " . showsPrec 11 x)

instance Read (VkFramebufferCreateBitmask a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkFramebufferCreateBitmask") >>
                  (VkFramebufferCreateBitmask <$> step readPrec)))
