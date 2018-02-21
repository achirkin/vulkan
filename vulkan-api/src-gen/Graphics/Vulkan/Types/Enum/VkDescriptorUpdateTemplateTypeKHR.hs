{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateTypeKHR
       (VkDescriptorUpdateTemplateTypeKHR(VkDescriptorUpdateTemplateTypeKHR,
                                          VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR,
                                          VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorUpdateTemplateTypeKHR.html VkDescriptorUpdateTemplateTypeKHR registry at www.khronos.org>
newtype VkDescriptorUpdateTemplateTypeKHR = VkDescriptorUpdateTemplateTypeKHR Int32
                                              deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                        Generic)

instance Show VkDescriptorUpdateTemplateTypeKHR where
        showsPrec _ VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR
          = showString
              "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR"
        showsPrec _ VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
          = showString
              "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR"
        showsPrec p (VkDescriptorUpdateTemplateTypeKHR x)
          = showParen (p >= 11)
              (showString "VkDescriptorUpdateTemplateTypeKHR " . showsPrec 11 x)

instance Read VkDescriptorUpdateTemplateTypeKHR where
        readPrec
          = parens
              (choose
                 [("VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR",
                   pure VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR),
                  ("VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR",
                   pure VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkDescriptorUpdateTemplateTypeKHR") >>
                      (VkDescriptorUpdateTemplateTypeKHR <$> step readPrec)))

-- | Create descriptor update template for descriptor set updates
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR ::
        VkDescriptorUpdateTemplateTypeKHR

pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR =
        VkDescriptorUpdateTemplateTypeKHR 0

-- | Create descriptor update template for pushed descriptor updates
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR ::
        VkDescriptorUpdateTemplateTypeKHR

pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR =
        VkDescriptorUpdateTemplateTypeKHR 1
