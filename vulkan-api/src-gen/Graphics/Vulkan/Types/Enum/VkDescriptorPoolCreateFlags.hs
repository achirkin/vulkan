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
module Graphics.Vulkan.Types.Enum.VkDescriptorPoolCreateFlags
       (VkDescriptorPoolCreateBitmask(VkDescriptorPoolCreateBitmask,
                                      VkDescriptorPoolCreateFlags, VkDescriptorPoolCreateFlagBits,
                                      VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
        VkDescriptorPoolCreateFlags, VkDescriptorPoolCreateFlagBits)
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
