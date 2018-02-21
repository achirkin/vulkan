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
module Graphics.Vulkan.Types.Enum.VkSparseMemoryBindFlags
       (VkSparseMemoryBindBitmask(VkSparseMemoryBindBitmask,
                                  VkSparseMemoryBindFlags, VkSparseMemoryBindFlagBits,
                                  VK_SPARSE_MEMORY_BIND_METADATA_BIT),
        VkSparseMemoryBindFlags, VkSparseMemoryBindFlagBits)
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

newtype VkSparseMemoryBindBitmask (a ::
                                     FlagType) = VkSparseMemoryBindBitmask VkFlags
                                                   deriving (Eq, Ord, Storable, Data, Generic)

type VkSparseMemoryBindFlags = VkSparseMemoryBindBitmask FlagMask

type VkSparseMemoryBindFlagBits = VkSparseMemoryBindBitmask FlagBit

pattern VkSparseMemoryBindFlagBits ::
        VkFlags -> VkSparseMemoryBindBitmask FlagBit

pattern VkSparseMemoryBindFlagBits n = VkSparseMemoryBindBitmask n

pattern VkSparseMemoryBindFlags ::
        VkFlags -> VkSparseMemoryBindBitmask FlagMask

pattern VkSparseMemoryBindFlags n = VkSparseMemoryBindBitmask n

deriving instance Bits (VkSparseMemoryBindBitmask FlagMask)

deriving instance FiniteBits (VkSparseMemoryBindBitmask FlagMask)

deriving instance Integral (VkSparseMemoryBindBitmask FlagMask)

deriving instance Num (VkSparseMemoryBindBitmask FlagMask)

deriving instance Bounded (VkSparseMemoryBindBitmask FlagMask)

deriving instance Enum (VkSparseMemoryBindBitmask FlagMask)

deriving instance Real (VkSparseMemoryBindBitmask FlagMask)

instance Show (VkSparseMemoryBindBitmask a) where
        showsPrec _ VK_SPARSE_MEMORY_BIND_METADATA_BIT
          = showString "VK_SPARSE_MEMORY_BIND_METADATA_BIT"
        showsPrec p (VkSparseMemoryBindBitmask x)
          = showParen (p >= 11)
              (showString "VkSparseMemoryBindBitmask " . showsPrec 11 x)

instance Read (VkSparseMemoryBindBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_SPARSE_MEMORY_BIND_METADATA_BIT",
                   pure VK_SPARSE_MEMORY_BIND_METADATA_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSparseMemoryBindBitmask") >>
                      (VkSparseMemoryBindBitmask <$> step readPrec)))

-- | Operation binds resource metadata to memory
--
--   bitpos = @0@
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT ::
        VkSparseMemoryBindBitmask a

pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT =
        VkSparseMemoryBindBitmask 1
