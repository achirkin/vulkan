{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkDescriptorType
       (VkDescriptorType(VkDescriptorType, VK_DESCRIPTOR_TYPE_SAMPLER,
                         VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                         VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE, VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                         VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER,
                         VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                         VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                         VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                         VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
                         VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC,
                         VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorType VkDescriptorType registry at www.khronos.org>
newtype VkDescriptorType = VkDescriptorType Int32
                             deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkDescriptorType where
        showsPrec _ VK_DESCRIPTOR_TYPE_SAMPLER
          = showString "VK_DESCRIPTOR_TYPE_SAMPLER"
        showsPrec _ VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          = showString "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
        showsPrec _ VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
          = showString "VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE"
        showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
          = showString "VK_DESCRIPTOR_TYPE_STORAGE_IMAGE"
        showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
          = showString "VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
        showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
          = showString "VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
        showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
          = showString "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
        showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
          = showString "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER"
        showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
          = showString "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
        showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
          = showString "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
        showsPrec _ VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
          = showString "VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
        showsPrec p (VkDescriptorType x)
          = showParen (p >= 11)
              (showString "VkDescriptorType " . showsPrec 11 x)

instance Read VkDescriptorType where
        readPrec
          = parens
              (choose
                 [("VK_DESCRIPTOR_TYPE_SAMPLER", pure VK_DESCRIPTOR_TYPE_SAMPLER),
                  ("VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER",
                   pure VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                  ("VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE",
                   pure VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                  ("VK_DESCRIPTOR_TYPE_STORAGE_IMAGE",
                   pure VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                  ("VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER",
                   pure VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER),
                  ("VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER",
                   pure VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER),
                  ("VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER",
                   pure VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                  ("VK_DESCRIPTOR_TYPE_STORAGE_BUFFER",
                   pure VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                  ("VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC",
                   pure VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC),
                  ("VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC",
                   pure VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC),
                  ("VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT",
                   pure VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDescriptorType") >>
                      (VkDescriptorType <$> step readPrec)))

pattern VK_DESCRIPTOR_TYPE_SAMPLER :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_SAMPLER = VkDescriptorType 0

pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER ::
        VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER =
        VkDescriptorType 1

pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = VkDescriptorType 2

pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = VkDescriptorType 3

pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER =
        VkDescriptorType 4

pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER =
        VkDescriptorType 5

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = VkDescriptorType 6

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = VkDescriptorType 7

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC ::
        VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC =
        VkDescriptorType 8

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC ::
        VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC =
        VkDescriptorType 9

pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT :: VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = VkDescriptorType 10
