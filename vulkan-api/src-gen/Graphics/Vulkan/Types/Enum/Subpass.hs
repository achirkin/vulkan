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
module Graphics.Vulkan.Types.Enum.Subpass
       (VkSubpassContents(VkSubpassContents, VK_SUBPASS_CONTENTS_INLINE,
                          VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS),
        VkSubpassDescriptionBitmask(VkSubpassDescriptionBitmask,
                                    VkSubpassDescriptionFlags, VkSubpassDescriptionFlagBits),
        VkSubpassDescriptionFlags, VkSubpassDescriptionFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubpassContents VkSubpassContents registry at www.khronos.org>
newtype VkSubpassContents = VkSubpassContents Int32
                              deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSubpassContents where
        showsPrec _ VK_SUBPASS_CONTENTS_INLINE
          = showString "VK_SUBPASS_CONTENTS_INLINE"
        showsPrec _ VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
          = showString "VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS"
        showsPrec p (VkSubpassContents x)
          = showParen (p >= 11)
              (showString "VkSubpassContents " . showsPrec 11 x)

instance Read VkSubpassContents where
        readPrec
          = parens
              (choose
                 [("VK_SUBPASS_CONTENTS_INLINE", pure VK_SUBPASS_CONTENTS_INLINE),
                  ("VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS",
                   pure VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS)]
                 +++
                 prec 10
                   (expectP (Ident "VkSubpassContents") >>
                      (VkSubpassContents <$> step readPrec)))

pattern VK_SUBPASS_CONTENTS_INLINE :: VkSubpassContents

pattern VK_SUBPASS_CONTENTS_INLINE = VkSubpassContents 0

pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS ::
        VkSubpassContents

pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS =
        VkSubpassContents 1

newtype VkSubpassDescriptionBitmask (a ::
                                       FlagType) = VkSubpassDescriptionBitmask VkFlags
                                                     deriving (Eq, Ord, Storable, Data, Generic)

type VkSubpassDescriptionFlags =
     VkSubpassDescriptionBitmask FlagMask

type VkSubpassDescriptionFlagBits =
     VkSubpassDescriptionBitmask FlagBit

pattern VkSubpassDescriptionFlagBits ::
        VkFlags -> VkSubpassDescriptionBitmask FlagBit

pattern VkSubpassDescriptionFlagBits n =
        VkSubpassDescriptionBitmask n

pattern VkSubpassDescriptionFlags ::
        VkFlags -> VkSubpassDescriptionBitmask FlagMask

pattern VkSubpassDescriptionFlags n = VkSubpassDescriptionBitmask n

deriving instance Bits (VkSubpassDescriptionBitmask FlagMask)

deriving instance FiniteBits (VkSubpassDescriptionBitmask FlagMask)

deriving instance Integral (VkSubpassDescriptionBitmask FlagMask)

deriving instance Num (VkSubpassDescriptionBitmask FlagMask)

deriving instance Bounded (VkSubpassDescriptionBitmask FlagMask)

deriving instance Enum (VkSubpassDescriptionBitmask FlagMask)

deriving instance Real (VkSubpassDescriptionBitmask FlagMask)

instance Show (VkSubpassDescriptionBitmask a) where
        showsPrec p (VkSubpassDescriptionBitmask x)
          = showParen (p >= 11)
              (showString "VkSubpassDescriptionBitmask " . showsPrec 11 x)

instance Read (VkSubpassDescriptionBitmask a) where
        readPrec
          = parens
              (choose [] +++
                 prec 10
                   (expectP (Ident "VkSubpassDescriptionBitmask") >>
                      (VkSubpassDescriptionBitmask <$> step readPrec)))
