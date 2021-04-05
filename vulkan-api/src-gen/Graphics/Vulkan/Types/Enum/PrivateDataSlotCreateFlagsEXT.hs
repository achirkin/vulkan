{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.PrivateDataSlotCreateFlagsEXT
       (VkPrivateDataSlotCreateBitmaskEXT(VkPrivateDataSlotCreateBitmaskEXT,
                                          VkPrivateDataSlotCreateFlagsEXT,
                                          VkPrivateDataSlotCreateFlagBitsEXT),
        VkPrivateDataSlotCreateFlagsEXT,
        VkPrivateDataSlotCreateFlagBitsEXT)
       where
import Data.Bits                       (Bits, FiniteBits)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

newtype VkPrivateDataSlotCreateBitmaskEXT (a ::
                                             FlagType) = VkPrivateDataSlotCreateBitmaskEXT VkFlags
                                                         deriving (Eq, Ord, Storable)

type VkPrivateDataSlotCreateFlagsEXT =
     VkPrivateDataSlotCreateBitmaskEXT FlagMask

type VkPrivateDataSlotCreateFlagBitsEXT =
     VkPrivateDataSlotCreateBitmaskEXT FlagBit

pattern VkPrivateDataSlotCreateFlagBitsEXT ::
        VkFlags -> VkPrivateDataSlotCreateBitmaskEXT FlagBit

pattern VkPrivateDataSlotCreateFlagBitsEXT n =
        VkPrivateDataSlotCreateBitmaskEXT n

pattern VkPrivateDataSlotCreateFlagsEXT ::
        VkFlags -> VkPrivateDataSlotCreateBitmaskEXT FlagMask

pattern VkPrivateDataSlotCreateFlagsEXT n =
        VkPrivateDataSlotCreateBitmaskEXT n

deriving instance Bits (VkPrivateDataSlotCreateBitmaskEXT FlagMask)

deriving instance
         FiniteBits (VkPrivateDataSlotCreateBitmaskEXT FlagMask)

instance Show (VkPrivateDataSlotCreateBitmaskEXT a) where
    showsPrec p (VkPrivateDataSlotCreateBitmaskEXT x)
      = showParen (p >= 11)
          (showString "VkPrivateDataSlotCreateBitmaskEXT " . showsPrec 11 x)

instance Read (VkPrivateDataSlotCreateBitmaskEXT a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkPrivateDataSlotCreateBitmaskEXT") >>
                  (VkPrivateDataSlotCreateBitmaskEXT <$> step readPrec)))
