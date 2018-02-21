{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.VkAttachmentDescriptionFlags
       (VkAttachmentDescriptionBitmask(VkAttachmentDescriptionBitmask,
                                       VkAttachmentDescriptionFlags,
                                       VkAttachmentDescriptionFlagBits,
                                       VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT),
        VkAttachmentDescriptionFlags, VkAttachmentDescriptionFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkAttachmentDescriptionBitmask (a ::
                                          FlagType) = VkAttachmentDescriptionBitmask VkFlags
                                                        deriving (Eq, Ord, Storable, Data, Generic)

type VkAttachmentDescriptionFlags =
     VkAttachmentDescriptionBitmask FlagMask

type VkAttachmentDescriptionFlagBits =
     VkAttachmentDescriptionBitmask FlagBit

pattern VkAttachmentDescriptionFlagBits ::
        VkFlags -> VkAttachmentDescriptionBitmask FlagBit

pattern VkAttachmentDescriptionFlagBits n =
        VkAttachmentDescriptionBitmask n

pattern VkAttachmentDescriptionFlags ::
        VkFlags -> VkAttachmentDescriptionBitmask FlagMask

pattern VkAttachmentDescriptionFlags n =
        VkAttachmentDescriptionBitmask n

deriving instance Bits (VkAttachmentDescriptionBitmask FlagMask)

deriving instance
         FiniteBits (VkAttachmentDescriptionBitmask FlagMask)

deriving instance
         Integral (VkAttachmentDescriptionBitmask FlagMask)

deriving instance Num (VkAttachmentDescriptionBitmask FlagMask)

deriving instance Bounded (VkAttachmentDescriptionBitmask FlagMask)

deriving instance Enum (VkAttachmentDescriptionBitmask FlagMask)

deriving instance Real (VkAttachmentDescriptionBitmask FlagMask)

instance Show (VkAttachmentDescriptionBitmask a) where
        showsPrec _ VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
          = showString "VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT"
        showsPrec p (VkAttachmentDescriptionBitmask x)
          = showParen (p >= 11)
              (showString "VkAttachmentDescriptionBitmask " . showsPrec 11 x)

instance Read (VkAttachmentDescriptionBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT",
                   pure VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkAttachmentDescriptionBitmask") >>
                      (VkAttachmentDescriptionBitmask <$> step readPrec)))

-- | The attachment may alias physical memory of another attachment in the same render pass
--
--   bitpos = @0@
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT ::
        VkAttachmentDescriptionBitmask a

pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT =
        VkAttachmentDescriptionBitmask 1
