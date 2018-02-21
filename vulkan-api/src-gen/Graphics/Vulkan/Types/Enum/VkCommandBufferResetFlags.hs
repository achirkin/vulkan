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
module Graphics.Vulkan.Types.Enum.VkCommandBufferResetFlags
       (VkCommandBufferResetBitmask(VkCommandBufferResetBitmask,
                                    VkCommandBufferResetFlags, VkCommandBufferResetFlagBits,
                                    VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT),
        VkCommandBufferResetFlags, VkCommandBufferResetFlagBits)
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

newtype VkCommandBufferResetBitmask (a ::
                                       FlagType) = VkCommandBufferResetBitmask VkFlags
                                                     deriving (Eq, Ord, Storable, Data, Generic)

type VkCommandBufferResetFlags =
     VkCommandBufferResetBitmask FlagMask

type VkCommandBufferResetFlagBits =
     VkCommandBufferResetBitmask FlagBit

pattern VkCommandBufferResetFlagBits ::
        VkFlags -> VkCommandBufferResetBitmask FlagBit

pattern VkCommandBufferResetFlagBits n =
        VkCommandBufferResetBitmask n

pattern VkCommandBufferResetFlags ::
        VkFlags -> VkCommandBufferResetBitmask FlagMask

pattern VkCommandBufferResetFlags n = VkCommandBufferResetBitmask n

deriving instance Bits (VkCommandBufferResetBitmask FlagMask)

deriving instance FiniteBits (VkCommandBufferResetBitmask FlagMask)

deriving instance Integral (VkCommandBufferResetBitmask FlagMask)

deriving instance Num (VkCommandBufferResetBitmask FlagMask)

deriving instance Bounded (VkCommandBufferResetBitmask FlagMask)

deriving instance Enum (VkCommandBufferResetBitmask FlagMask)

deriving instance Real (VkCommandBufferResetBitmask FlagMask)

instance Show (VkCommandBufferResetBitmask a) where
        showsPrec _ VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
          = showString "VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
        showsPrec p (VkCommandBufferResetBitmask x)
          = showParen (p >= 11)
              (showString "VkCommandBufferResetBitmask " . showsPrec 11 x)

instance Read (VkCommandBufferResetBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT",
                   pure VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandBufferResetBitmask") >>
                      (VkCommandBufferResetBitmask <$> step readPrec)))

-- | Release resources owned by the buffer
--
--   bitpos = @0@
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT ::
        VkCommandBufferResetBitmask a

pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT =
        VkCommandBufferResetBitmask 1
