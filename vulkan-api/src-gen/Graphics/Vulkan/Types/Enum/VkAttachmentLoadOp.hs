{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkAttachmentLoadOp
       (VkAttachmentLoadOp(VkAttachmentLoadOp, VK_ATTACHMENT_LOAD_OP_LOAD,
                           VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_LOAD_OP_DONT_CARE))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkAttachmentLoadOp.html VkAttachmentLoadOp registry at www.khronos.org>
newtype VkAttachmentLoadOp = VkAttachmentLoadOp Int32
                               deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkAttachmentLoadOp where
        showsPrec _ VK_ATTACHMENT_LOAD_OP_LOAD
          = showString "VK_ATTACHMENT_LOAD_OP_LOAD"
        showsPrec _ VK_ATTACHMENT_LOAD_OP_CLEAR
          = showString "VK_ATTACHMENT_LOAD_OP_CLEAR"
        showsPrec _ VK_ATTACHMENT_LOAD_OP_DONT_CARE
          = showString "VK_ATTACHMENT_LOAD_OP_DONT_CARE"
        showsPrec p (VkAttachmentLoadOp x)
          = showParen (p >= 11)
              (showString "VkAttachmentLoadOp " . showsPrec 11 x)

instance Read VkAttachmentLoadOp where
        readPrec
          = parens
              (choose
                 [("VK_ATTACHMENT_LOAD_OP_LOAD", pure VK_ATTACHMENT_LOAD_OP_LOAD),
                  ("VK_ATTACHMENT_LOAD_OP_CLEAR", pure VK_ATTACHMENT_LOAD_OP_CLEAR),
                  ("VK_ATTACHMENT_LOAD_OP_DONT_CARE",
                   pure VK_ATTACHMENT_LOAD_OP_DONT_CARE)]
                 +++
                 prec 10
                   (expectP (Ident "VkAttachmentLoadOp") >>
                      (VkAttachmentLoadOp <$> step readPrec)))

pattern VK_ATTACHMENT_LOAD_OP_LOAD :: VkAttachmentLoadOp

pattern VK_ATTACHMENT_LOAD_OP_LOAD = VkAttachmentLoadOp 0

pattern VK_ATTACHMENT_LOAD_OP_CLEAR :: VkAttachmentLoadOp

pattern VK_ATTACHMENT_LOAD_OP_CLEAR = VkAttachmentLoadOp 1

pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE :: VkAttachmentLoadOp

pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE = VkAttachmentLoadOp 2
