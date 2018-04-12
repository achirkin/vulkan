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
module Graphics.Vulkan.Types.Enum.DependencyFlags
       (VkDependencyBitmask(VkDependencyBitmask, VkDependencyFlags,
                            VkDependencyFlagBits, VK_DEPENDENCY_BY_REGION_BIT),
        VkDependencyFlags, VkDependencyFlagBits)
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

newtype VkDependencyBitmask (a ::
                               FlagType) = VkDependencyBitmask VkFlags
                                             deriving (Eq, Ord, Storable, Data, Generic)

type VkDependencyFlags = VkDependencyBitmask FlagMask

type VkDependencyFlagBits = VkDependencyBitmask FlagBit

pattern VkDependencyFlagBits ::
        VkFlags -> VkDependencyBitmask FlagBit

pattern VkDependencyFlagBits n = VkDependencyBitmask n

pattern VkDependencyFlags ::
        VkFlags -> VkDependencyBitmask FlagMask

pattern VkDependencyFlags n = VkDependencyBitmask n

deriving instance Bits (VkDependencyBitmask FlagMask)

deriving instance FiniteBits (VkDependencyBitmask FlagMask)

deriving instance Integral (VkDependencyBitmask FlagMask)

deriving instance Num (VkDependencyBitmask FlagMask)

deriving instance Bounded (VkDependencyBitmask FlagMask)

deriving instance Enum (VkDependencyBitmask FlagMask)

deriving instance Real (VkDependencyBitmask FlagMask)

instance Show (VkDependencyBitmask a) where
        showsPrec _ VK_DEPENDENCY_BY_REGION_BIT
          = showString "VK_DEPENDENCY_BY_REGION_BIT"
        showsPrec p (VkDependencyBitmask x)
          = showParen (p >= 11)
              (showString "VkDependencyBitmask " . showsPrec 11 x)

instance Read (VkDependencyBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_DEPENDENCY_BY_REGION_BIT", pure VK_DEPENDENCY_BY_REGION_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDependencyBitmask") >>
                      (VkDependencyBitmask <$> step readPrec)))

-- | Dependency is per pixel region
--
--   bitpos = @0@
pattern VK_DEPENDENCY_BY_REGION_BIT :: VkDependencyBitmask a

pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyBitmask 1
