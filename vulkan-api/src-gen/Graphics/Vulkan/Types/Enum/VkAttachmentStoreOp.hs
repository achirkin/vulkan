{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkAttachmentStoreOp
       (VkAttachmentStoreOp(VkAttachmentStoreOp,
                            VK_ATTACHMENT_STORE_OP_STORE, VK_ATTACHMENT_STORE_OP_DONT_CARE))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAttachmentStoreOpVkAttachmentStoreOp registry at www.khronos.org>
newtype VkAttachmentStoreOp = VkAttachmentStoreOp Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkAttachmentStoreOp where
        showsPrec _ VK_ATTACHMENT_STORE_OP_STORE
          = showString "VK_ATTACHMENT_STORE_OP_STORE"
        showsPrec _ VK_ATTACHMENT_STORE_OP_DONT_CARE
          = showString "VK_ATTACHMENT_STORE_OP_DONT_CARE"
        showsPrec p (VkAttachmentStoreOp x)
          = showParen (p >= 11)
              (showString "VkAttachmentStoreOp " . showsPrec 11 x)

instance Read VkAttachmentStoreOp where
        readPrec
          = parens
              (choose
                 [("VK_ATTACHMENT_STORE_OP_STORE",
                   pure VK_ATTACHMENT_STORE_OP_STORE),
                  ("VK_ATTACHMENT_STORE_OP_DONT_CARE",
                   pure VK_ATTACHMENT_STORE_OP_DONT_CARE)]
                 +++
                 prec 10
                   (expectP (Ident "VkAttachmentStoreOp") >>
                      (VkAttachmentStoreOp <$> step readPrec)))

pattern VK_ATTACHMENT_STORE_OP_STORE :: VkAttachmentStoreOp

pattern VK_ATTACHMENT_STORE_OP_STORE = VkAttachmentStoreOp 0

pattern VK_ATTACHMENT_STORE_OP_DONT_CARE :: VkAttachmentStoreOp

pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = VkAttachmentStoreOp 1
