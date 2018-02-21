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
module Graphics.Vulkan.Types.Enum.VkDescriptorSetLayoutCreateFlags
       (VkDescriptorSetLayoutCreateBitmask(VkDescriptorSetLayoutCreateBitmask,
                                           VkDescriptorSetLayoutCreateFlags,
                                           VkDescriptorSetLayoutCreateFlagBits),
        VkDescriptorSetLayoutCreateFlags,
        VkDescriptorSetLayoutCreateFlagBits)
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
