{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateType
       (VkDescriptorUpdateTemplateType(VkDescriptorUpdateTemplateType,
                                       VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorUpdateTemplateType VkDescriptorUpdateTemplateType registry at www.khronos.org>
newtype VkDescriptorUpdateTemplateType = VkDescriptorUpdateTemplateType Int32
                                           deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                     Generic)

instance Show VkDescriptorUpdateTemplateType where
        showsPrec _ VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
          = showString "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET"
        showsPrec p (VkDescriptorUpdateTemplateType x)
          = showParen (p >= 11)
              (showString "VkDescriptorUpdateTemplateType " . showsPrec 11 x)

instance Read VkDescriptorUpdateTemplateType where
        readPrec
          = parens
              (choose
                 [("VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET",
                   pure VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET)]
                 +++
                 prec 10
                   (expectP (Ident "VkDescriptorUpdateTemplateType") >>
                      (VkDescriptorUpdateTemplateType <$> step readPrec)))

-- | Create descriptor update template for descriptor set updates
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET ::
        VkDescriptorUpdateTemplateType

pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET =
        VkDescriptorUpdateTemplateType 0
