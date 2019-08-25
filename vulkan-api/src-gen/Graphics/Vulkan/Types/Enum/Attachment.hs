{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Attachment
       (VkAttachmentDescriptionBitmask(VkAttachmentDescriptionBitmask,
                                       VkAttachmentDescriptionFlags,
                                       VkAttachmentDescriptionFlagBits,
                                       VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT),
        VkAttachmentDescriptionFlags, VkAttachmentDescriptionFlagBits,
        VkAttachmentLoadOp(VkAttachmentLoadOp, VK_ATTACHMENT_LOAD_OP_LOAD,
                           VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_LOAD_OP_DONT_CARE),
        VkAttachmentStoreOp(VkAttachmentStoreOp,
                            VK_ATTACHMENT_STORE_OP_STORE, VK_ATTACHMENT_STORE_OP_DONT_CARE))
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkAttachmentDescriptionBitmask (a ::
                                          FlagType) = VkAttachmentDescriptionBitmask VkFlags
                                                      deriving (Eq, Ord, Storable)

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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAttachmentLoadOp VkAttachmentLoadOp registry at www.khronos.org>
newtype VkAttachmentLoadOp = VkAttachmentLoadOp Int32
                             deriving (Eq, Ord, Enum, Storable)

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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAttachmentStoreOp VkAttachmentStoreOp registry at www.khronos.org>
newtype VkAttachmentStoreOp = VkAttachmentStoreOp Int32
                              deriving (Eq, Ord, Enum, Storable)

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
