{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSubpassContents
       (VkSubpassContents(VkSubpassContents, VK_SUBPASS_CONTENTS_INLINE,
                          VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSubpassContents.html VkSubpassContents registry at www.khronos.org>
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
