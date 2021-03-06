{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Descriptor
       (VkDescriptorBindingFlagBitsEXT(..),
        VkDescriptorBindingBitmask(VkDescriptorBindingBitmask,
                                   VkDescriptorBindingFlags, VkDescriptorBindingFlagBits,
                                   VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT,
                                   VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT,
                                   VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT,
                                   VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT),
        VkDescriptorBindingFlags, VkDescriptorBindingFlagBits,
        VkDescriptorPoolCreateBitmask(VkDescriptorPoolCreateBitmask,
                                      VkDescriptorPoolCreateFlags, VkDescriptorPoolCreateFlagBits,
                                      VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
        VkDescriptorPoolCreateFlags, VkDescriptorPoolCreateFlagBits,
        VkDescriptorSetLayoutCreateBitmask(VkDescriptorSetLayoutCreateBitmask,
                                           VkDescriptorSetLayoutCreateFlags,
                                           VkDescriptorSetLayoutCreateFlagBits),
        VkDescriptorSetLayoutCreateFlags,
        VkDescriptorSetLayoutCreateFlagBits,
        VkDescriptorType(VkDescriptorType, VK_DESCRIPTOR_TYPE_SAMPLER,
                         VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                         VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE, VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                         VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER,
                         VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                         VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                         VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                         VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
                         VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC,
                         VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
        VkDescriptorUpdateTemplateType(VkDescriptorUpdateTemplateType,
                                       VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET),
        VkDescriptorUpdateTemplateTypeKHR(..))
       where
import Data.Bits                       (Bits, FiniteBits)
import Data.Coerce                     (coerce)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType, Int32)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

newtype VkDescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBitsEXT VkFlags
                                         deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDescriptorBindingFlagBitsEXT where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDescriptorBindingFlagBitsEXT where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkDescriptorBindingBitmask (a ::
                                      FlagType) = VkDescriptorBindingBitmask VkFlags
                                                  deriving (Eq, Ord, Storable)

type VkDescriptorBindingFlags = VkDescriptorBindingBitmask FlagMask

type VkDescriptorBindingFlagBits =
     VkDescriptorBindingBitmask FlagBit

pattern VkDescriptorBindingFlagBits ::
        VkFlags -> VkDescriptorBindingBitmask FlagBit

pattern VkDescriptorBindingFlagBits n =
        VkDescriptorBindingBitmask n

pattern VkDescriptorBindingFlags ::
        VkFlags -> VkDescriptorBindingBitmask FlagMask

pattern VkDescriptorBindingFlags n = VkDescriptorBindingBitmask n

deriving instance Bits (VkDescriptorBindingBitmask FlagMask)

deriving instance FiniteBits (VkDescriptorBindingBitmask FlagMask)

instance Show (VkDescriptorBindingBitmask a) where
    showsPrec _ VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT
      = showString "VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT"
    showsPrec _ VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT
      = showString
          "VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT"
    showsPrec _ VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT
      = showString "VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT"
    showsPrec _ VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT
      = showString "VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT"
    showsPrec p (VkDescriptorBindingBitmask x)
      = showParen (p >= 11)
          (showString "VkDescriptorBindingBitmask " . showsPrec 11 x)

instance Read (VkDescriptorBindingBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT",
               pure VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT),
              ("VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT",
               pure VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT),
              ("VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT",
               pure VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT),
              ("VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT",
               pure VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT)]
             +++
             prec 10
               (expectP (Ident "VkDescriptorBindingBitmask") >>
                  (VkDescriptorBindingBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT ::
        VkDescriptorBindingBitmask a

pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT =
        VkDescriptorBindingBitmask 1

-- | bitpos = @1@
pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT ::
        VkDescriptorBindingBitmask a

pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT =
        VkDescriptorBindingBitmask 2

-- | bitpos = @2@
pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT ::
        VkDescriptorBindingBitmask a

pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT =
        VkDescriptorBindingBitmask 4

-- | bitpos = @3@
pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT ::
        VkDescriptorBindingBitmask a

pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT =
        VkDescriptorBindingBitmask 8

newtype VkDescriptorPoolCreateBitmask (a ::
                                         FlagType) = VkDescriptorPoolCreateBitmask VkFlags
                                                     deriving (Eq, Ord, Storable)

type VkDescriptorPoolCreateFlags =
     VkDescriptorPoolCreateBitmask FlagMask

type VkDescriptorPoolCreateFlagBits =
     VkDescriptorPoolCreateBitmask FlagBit

pattern VkDescriptorPoolCreateFlagBits ::
        VkFlags -> VkDescriptorPoolCreateBitmask FlagBit

pattern VkDescriptorPoolCreateFlagBits n =
        VkDescriptorPoolCreateBitmask n

pattern VkDescriptorPoolCreateFlags ::
        VkFlags -> VkDescriptorPoolCreateBitmask FlagMask

pattern VkDescriptorPoolCreateFlags n =
        VkDescriptorPoolCreateBitmask n

deriving instance Bits (VkDescriptorPoolCreateBitmask FlagMask)

deriving instance
         FiniteBits (VkDescriptorPoolCreateBitmask FlagMask)

instance Show (VkDescriptorPoolCreateBitmask a) where
    showsPrec _ VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
      = showString "VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT"
    showsPrec p (VkDescriptorPoolCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkDescriptorPoolCreateBitmask " . showsPrec 11 x)

instance Read (VkDescriptorPoolCreateBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT",
               pure VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT)]
             +++
             prec 10
               (expectP (Ident "VkDescriptorPoolCreateBitmask") >>
                  (VkDescriptorPoolCreateBitmask <$> step readPrec)))

-- | Descriptor sets may be freed individually
--
--   bitpos = @0@
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT ::
        VkDescriptorPoolCreateBitmask a

pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT =
        VkDescriptorPoolCreateBitmask 1

newtype VkDescriptorSetLayoutCreateBitmask (a ::
                                              FlagType) = VkDescriptorSetLayoutCreateBitmask VkFlags
                                                          deriving (Eq, Ord, Storable)

type VkDescriptorSetLayoutCreateFlags =
     VkDescriptorSetLayoutCreateBitmask FlagMask

type VkDescriptorSetLayoutCreateFlagBits =
     VkDescriptorSetLayoutCreateBitmask FlagBit

pattern VkDescriptorSetLayoutCreateFlagBits ::
        VkFlags -> VkDescriptorSetLayoutCreateBitmask FlagBit

pattern VkDescriptorSetLayoutCreateFlagBits n =
        VkDescriptorSetLayoutCreateBitmask n

pattern VkDescriptorSetLayoutCreateFlags ::
        VkFlags -> VkDescriptorSetLayoutCreateBitmask FlagMask

pattern VkDescriptorSetLayoutCreateFlags n =
        VkDescriptorSetLayoutCreateBitmask n

deriving instance
         Bits (VkDescriptorSetLayoutCreateBitmask FlagMask)

deriving instance
         FiniteBits (VkDescriptorSetLayoutCreateBitmask FlagMask)

instance Show (VkDescriptorSetLayoutCreateBitmask a) where
    showsPrec p (VkDescriptorSetLayoutCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkDescriptorSetLayoutCreateBitmask " . showsPrec 11 x)

instance Read (VkDescriptorSetLayoutCreateBitmask a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkDescriptorSetLayoutCreateBitmask") >>
                  (VkDescriptorSetLayoutCreateBitmask <$> step readPrec)))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDescriptorType VkDescriptorType registry at www.khronos.org>
newtype VkDescriptorType = VkDescriptorType Int32
                           deriving (Eq, Ord, Enum, Storable)

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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDescriptorUpdateTemplateType VkDescriptorUpdateTemplateType registry at www.khronos.org>
newtype VkDescriptorUpdateTemplateType = VkDescriptorUpdateTemplateType Int32
                                         deriving (Eq, Ord, Enum, Storable)

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

newtype VkDescriptorUpdateTemplateTypeKHR = VkDescriptorUpdateTemplateTypeKHR VkFlags
                                            deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDescriptorUpdateTemplateTypeKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDescriptorUpdateTemplateTypeKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
