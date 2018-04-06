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
module Graphics.Vulkan.Types.Enum.VkDescriptorBindingFlagsEXT
       (VkDescriptorBindingBitmaskEXT(VkDescriptorBindingBitmaskEXT,
                                      VkDescriptorBindingFlagsEXT, VkDescriptorBindingFlagBitsEXT,
                                      VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT,
                                      VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT,
                                      VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT,
                                      VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT),
        VkDescriptorBindingFlagsEXT, VkDescriptorBindingFlagBitsEXT)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
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
