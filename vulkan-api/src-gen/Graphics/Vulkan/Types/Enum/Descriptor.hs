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
module Graphics.Vulkan.Types.Enum.Descriptor
       (VkDescriptorBindingBitmaskEXT(VkDescriptorBindingBitmaskEXT,
                                      VkDescriptorBindingFlagsEXT, VkDescriptorBindingFlagBitsEXT,
                                      VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT,
                                      VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT,
                                      VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT,
                                      VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT),
        VkDescriptorBindingFlagsEXT, VkDescriptorBindingFlagBitsEXT,
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
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
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

newtype VkDescriptorBindingBitmaskEXT (a ::
                                         FlagType) = VkDescriptorBindingBitmaskEXT VkFlags
                                                       deriving (Eq, Ord, Storable, Data, Generic)

type VkDescriptorBindingFlagsEXT =
     VkDescriptorBindingBitmaskEXT FlagMask

type VkDescriptorBindingFlagBitsEXT =
     VkDescriptorBindingBitmaskEXT FlagBit

pattern VkDescriptorBindingFlagBitsEXT ::
        VkFlags -> VkDescriptorBindingBitmaskEXT FlagBit

pattern VkDescriptorBindingFlagBitsEXT n =
        VkDescriptorBindingBitmaskEXT n

pattern VkDescriptorBindingFlagsEXT ::
        VkFlags -> VkDescriptorBindingBitmaskEXT FlagMask

pattern VkDescriptorBindingFlagsEXT n =
        VkDescriptorBindingBitmaskEXT n

deriving instance Bits (VkDescriptorBindingBitmaskEXT FlagMask)

deriving instance
         FiniteBits (VkDescriptorBindingBitmaskEXT FlagMask)

deriving instance Integral (VkDescriptorBindingBitmaskEXT FlagMask)

deriving instance Num (VkDescriptorBindingBitmaskEXT FlagMask)

deriving instance Bounded (VkDescriptorBindingBitmaskEXT FlagMask)

deriving instance Enum (VkDescriptorBindingBitmaskEXT FlagMask)

deriving instance Real (VkDescriptorBindingBitmaskEXT FlagMask)

instance Show (VkDescriptorBindingBitmaskEXT a) where
        showsPrec _ VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT
          = showString "VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT"
        showsPrec _
          VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT
          = showString
              "VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT"
        showsPrec _ VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT
          = showString "VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT"
        showsPrec _ VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT
          = showString
              "VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT"
        showsPrec p (VkDescriptorBindingBitmaskEXT x)
          = showParen (p >= 11)
              (showString "VkDescriptorBindingBitmaskEXT " . showsPrec 11 x)

instance Read (VkDescriptorBindingBitmaskEXT a) where
        readPrec
          = parens
              (choose
                 [("VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT",
                   pure VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT),
                  ("VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT",
                   pure VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT),
                  ("VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT",
                   pure VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT),
                  ("VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT",
                   pure VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDescriptorBindingBitmaskEXT") >>
                      (VkDescriptorBindingBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT ::
        VkDescriptorBindingBitmaskEXT a

pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT =
        VkDescriptorBindingBitmaskEXT 1

-- | bitpos = @1@
pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT
        :: VkDescriptorBindingBitmaskEXT a

pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT =
        VkDescriptorBindingBitmaskEXT 2

-- | bitpos = @2@
pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT ::
        VkDescriptorBindingBitmaskEXT a

pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT =
        VkDescriptorBindingBitmaskEXT 4

-- | bitpos = @3@
pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT ::
        VkDescriptorBindingBitmaskEXT a

pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT =
        VkDescriptorBindingBitmaskEXT 8

newtype VkDescriptorPoolCreateBitmask (a ::
                                         FlagType) = VkDescriptorPoolCreateBitmask VkFlags
                                                       deriving (Eq, Ord, Storable, Data, Generic)

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

deriving instance Integral (VkDescriptorPoolCreateBitmask FlagMask)

deriving instance Num (VkDescriptorPoolCreateBitmask FlagMask)

deriving instance Bounded (VkDescriptorPoolCreateBitmask FlagMask)

deriving instance Enum (VkDescriptorPoolCreateBitmask FlagMask)

deriving instance Real (VkDescriptorPoolCreateBitmask FlagMask)

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
                                                            deriving (Eq, Ord, Storable, Data,
                                                                      Generic)

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

deriving instance
         Integral (VkDescriptorSetLayoutCreateBitmask FlagMask)

deriving instance Num (VkDescriptorSetLayoutCreateBitmask FlagMask)

deriving instance
         Bounded (VkDescriptorSetLayoutCreateBitmask FlagMask)

deriving instance
         Enum (VkDescriptorSetLayoutCreateBitmask FlagMask)

deriving instance
         Real (VkDescriptorSetLayoutCreateBitmask FlagMask)

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

newtype VkDescriptorUpdateTemplateTypeKHR = VkDescriptorUpdateTemplateTypeKHR VkFlags
                                              deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                        FiniteBits, Storable, Real, Data, Generic)

instance Show VkDescriptorUpdateTemplateTypeKHR where
        {-# INLINE show #-}
        show (VkDescriptorUpdateTemplateTypeKHR x) = show x

instance Read VkDescriptorUpdateTemplateTypeKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
