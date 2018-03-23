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
module Graphics.Vulkan.Types.Enum.VkDeviceQueueCreateFlags
       (VkDeviceQueueCreateBitmask(VkDeviceQueueCreateBitmask,
                                   VkDeviceQueueCreateFlags, VkDeviceQueueCreateFlagBits),
        VkDeviceQueueCreateFlags, VkDeviceQueueCreateFlagBits)
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

newtype VkDeviceQueueCreateBitmask (a ::
                                      FlagType) = VkDeviceQueueCreateBitmask VkFlags
                                                    deriving (Eq, Ord, Storable, Data, Generic)

type VkDeviceQueueCreateFlags = VkDeviceQueueCreateBitmask FlagMask

type VkDeviceQueueCreateFlagBits =
     VkDeviceQueueCreateBitmask FlagBit

pattern VkDeviceQueueCreateFlagBits ::
        VkFlags -> VkDeviceQueueCreateBitmask FlagBit

pattern VkDeviceQueueCreateFlagBits n =
        VkDeviceQueueCreateBitmask n

pattern VkDeviceQueueCreateFlags ::
        VkFlags -> VkDeviceQueueCreateBitmask FlagMask

pattern VkDeviceQueueCreateFlags n = VkDeviceQueueCreateBitmask n

deriving instance Bits (VkDeviceQueueCreateBitmask FlagMask)

deriving instance FiniteBits (VkDeviceQueueCreateBitmask FlagMask)

deriving instance Integral (VkDeviceQueueCreateBitmask FlagMask)

deriving instance Num (VkDeviceQueueCreateBitmask FlagMask)

deriving instance Bounded (VkDeviceQueueCreateBitmask FlagMask)

deriving instance Enum (VkDeviceQueueCreateBitmask FlagMask)

deriving instance Real (VkDeviceQueueCreateBitmask FlagMask)

instance Show (VkDeviceQueueCreateBitmask a) where
        showsPrec p (VkDeviceQueueCreateBitmask x)
          = showParen (p >= 11)
              (showString "VkDeviceQueueCreateBitmask " . showsPrec 11 x)

instance Read (VkDeviceQueueCreateBitmask a) where
        readPrec
          = parens
              (choose [] +++
                 prec 10
                   (expectP (Ident "VkDeviceQueueCreateBitmask") >>
                      (VkDeviceQueueCreateBitmask <$> step readPrec)))
