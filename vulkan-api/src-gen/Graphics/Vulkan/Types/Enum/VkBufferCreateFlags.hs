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
module Graphics.Vulkan.Types.Enum.VkBufferCreateFlags
       (VkBufferCreateBitmask(VkBufferCreateBitmask, VkBufferCreateFlags,
                              VkBufferCreateFlagBits, VK_BUFFER_CREATE_SPARSE_BINDING_BIT,
                              VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT,
                              VK_BUFFER_CREATE_SPARSE_ALIASED_BIT),
        VkBufferCreateFlags, VkBufferCreateFlagBits)
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

newtype VkBufferCreateBitmask (a ::
                                 FlagType) = VkBufferCreateBitmask VkFlags
                                               deriving (Eq, Ord, Storable, Data, Generic)

type VkBufferCreateFlags = VkBufferCreateBitmask FlagMask

type VkBufferCreateFlagBits = VkBufferCreateBitmask FlagBit

pattern VkBufferCreateFlagBits ::
        VkFlags -> VkBufferCreateBitmask FlagBit

pattern VkBufferCreateFlagBits n = VkBufferCreateBitmask n

pattern VkBufferCreateFlags ::
        VkFlags -> VkBufferCreateBitmask FlagMask

pattern VkBufferCreateFlags n = VkBufferCreateBitmask n

deriving instance Bits (VkBufferCreateBitmask FlagMask)

deriving instance FiniteBits (VkBufferCreateBitmask FlagMask)

deriving instance Integral (VkBufferCreateBitmask FlagMask)

deriving instance Num (VkBufferCreateBitmask FlagMask)

deriving instance Bounded (VkBufferCreateBitmask FlagMask)

deriving instance Enum (VkBufferCreateBitmask FlagMask)

deriving instance Real (VkBufferCreateBitmask FlagMask)

instance Show (VkBufferCreateBitmask a) where
        showsPrec _ VK_BUFFER_CREATE_SPARSE_BINDING_BIT
          = showString "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
        showsPrec _ VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
          = showString "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
        showsPrec _ VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
          = showString "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
        showsPrec p (VkBufferCreateBitmask x)
          = showParen (p >= 11)
              (showString "VkBufferCreateBitmask " . showsPrec 11 x)

instance Read (VkBufferCreateBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_BUFFER_CREATE_SPARSE_BINDING_BIT",
                   pure VK_BUFFER_CREATE_SPARSE_BINDING_BIT),
                  ("VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT",
                   pure VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT),
                  ("VK_BUFFER_CREATE_SPARSE_ALIASED_BIT",
                   pure VK_BUFFER_CREATE_SPARSE_ALIASED_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkBufferCreateBitmask") >>
                      (VkBufferCreateBitmask <$> step readPrec)))

-- | Buffer should support sparse backing
--
--   bitpos = @0@
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT ::
        VkBufferCreateBitmask a

pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT =
        VkBufferCreateBitmask 1

-- | Buffer should support sparse backing with partial residency
--
--   bitpos = @1@
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT ::
        VkBufferCreateBitmask a

pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT =
        VkBufferCreateBitmask 2

-- | Buffer should support constent data access to physical memory ranges mapped into multiple locations of sparse buffers
--
--   bitpos = @2@
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT ::
        VkBufferCreateBitmask a

pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT =
        VkBufferCreateBitmask 4
