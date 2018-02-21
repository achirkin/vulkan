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
module Graphics.Vulkan.Types.Enum.VkQueueFlags
       (VkQueueBitmask(VkQueueBitmask, VkQueueFlags, VkQueueFlagBits,
                       VK_QUEUE_GRAPHICS_BIT, VK_QUEUE_COMPUTE_BIT, VK_QUEUE_TRANSFER_BIT,
                       VK_QUEUE_SPARSE_BINDING_BIT),
        VkQueueFlags, VkQueueFlagBits)
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

newtype VkQueueBitmask (a :: FlagType) = VkQueueBitmask VkFlags
                                           deriving (Eq, Ord, Storable, Data, Generic)

type VkQueueFlags = VkQueueBitmask FlagMask

type VkQueueFlagBits = VkQueueBitmask FlagBit

pattern VkQueueFlagBits :: VkFlags -> VkQueueBitmask FlagBit

pattern VkQueueFlagBits n = VkQueueBitmask n

pattern VkQueueFlags :: VkFlags -> VkQueueBitmask FlagMask

pattern VkQueueFlags n = VkQueueBitmask n

deriving instance Bits (VkQueueBitmask FlagMask)

deriving instance FiniteBits (VkQueueBitmask FlagMask)

deriving instance Integral (VkQueueBitmask FlagMask)

deriving instance Num (VkQueueBitmask FlagMask)

deriving instance Bounded (VkQueueBitmask FlagMask)

deriving instance Enum (VkQueueBitmask FlagMask)

deriving instance Real (VkQueueBitmask FlagMask)

instance Show (VkQueueBitmask a) where
        showsPrec _ VK_QUEUE_GRAPHICS_BIT
          = showString "VK_QUEUE_GRAPHICS_BIT"
        showsPrec _ VK_QUEUE_COMPUTE_BIT
          = showString "VK_QUEUE_COMPUTE_BIT"
        showsPrec _ VK_QUEUE_TRANSFER_BIT
          = showString "VK_QUEUE_TRANSFER_BIT"
        showsPrec _ VK_QUEUE_SPARSE_BINDING_BIT
          = showString "VK_QUEUE_SPARSE_BINDING_BIT"
        showsPrec p (VkQueueBitmask x)
          = showParen (p >= 11)
              (showString "VkQueueBitmask " . showsPrec 11 x)

instance Read (VkQueueBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_QUEUE_GRAPHICS_BIT", pure VK_QUEUE_GRAPHICS_BIT),
                  ("VK_QUEUE_COMPUTE_BIT", pure VK_QUEUE_COMPUTE_BIT),
                  ("VK_QUEUE_TRANSFER_BIT", pure VK_QUEUE_TRANSFER_BIT),
                  ("VK_QUEUE_SPARSE_BINDING_BIT", pure VK_QUEUE_SPARSE_BINDING_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueueBitmask") >>
                      (VkQueueBitmask <$> step readPrec)))

-- | Queue supports graphics operations
--
--   bitpos = @0@
pattern VK_QUEUE_GRAPHICS_BIT :: VkQueueBitmask a

pattern VK_QUEUE_GRAPHICS_BIT = VkQueueBitmask 1

-- | Queue supports compute operations
--
--   bitpos = @1@
pattern VK_QUEUE_COMPUTE_BIT :: VkQueueBitmask a

pattern VK_QUEUE_COMPUTE_BIT = VkQueueBitmask 2

-- | Queue supports transfer operations
--
--   bitpos = @2@
pattern VK_QUEUE_TRANSFER_BIT :: VkQueueBitmask a

pattern VK_QUEUE_TRANSFER_BIT = VkQueueBitmask 4

-- | Queue supports sparse resource memory management operations
--
--   bitpos = @3@
pattern VK_QUEUE_SPARSE_BINDING_BIT :: VkQueueBitmask a

pattern VK_QUEUE_SPARSE_BINDING_BIT = VkQueueBitmask 8
