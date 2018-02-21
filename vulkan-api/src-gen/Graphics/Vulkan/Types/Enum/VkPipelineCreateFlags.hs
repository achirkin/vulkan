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
module Graphics.Vulkan.Types.Enum.VkPipelineCreateFlags
       (VkPipelineCreateBitmask(VkPipelineCreateBitmask,
                                VkPipelineCreateFlags, VkPipelineCreateFlagBits,
                                VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT,
                                VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT,
                                VK_PIPELINE_CREATE_DERIVATIVE_BIT),
        VkPipelineCreateFlags, VkPipelineCreateFlagBits)
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

newtype VkPipelineCreateBitmask (a ::
                                   FlagType) = VkPipelineCreateBitmask VkFlags
                                                 deriving (Eq, Ord, Storable, Data, Generic)

type VkPipelineCreateFlags = VkPipelineCreateBitmask FlagMask

type VkPipelineCreateFlagBits = VkPipelineCreateBitmask FlagBit

pattern VkPipelineCreateFlagBits ::
        VkFlags -> VkPipelineCreateBitmask FlagBit

pattern VkPipelineCreateFlagBits n = VkPipelineCreateBitmask n

pattern VkPipelineCreateFlags ::
        VkFlags -> VkPipelineCreateBitmask FlagMask

pattern VkPipelineCreateFlags n = VkPipelineCreateBitmask n

deriving instance Bits (VkPipelineCreateBitmask FlagMask)

deriving instance FiniteBits (VkPipelineCreateBitmask FlagMask)

deriving instance Integral (VkPipelineCreateBitmask FlagMask)

deriving instance Num (VkPipelineCreateBitmask FlagMask)

deriving instance Bounded (VkPipelineCreateBitmask FlagMask)

deriving instance Enum (VkPipelineCreateBitmask FlagMask)

deriving instance Real (VkPipelineCreateBitmask FlagMask)

instance Show (VkPipelineCreateBitmask a) where
        showsPrec _ VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
          = showString "VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
        showsPrec _ VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
          = showString "VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
        showsPrec _ VK_PIPELINE_CREATE_DERIVATIVE_BIT
          = showString "VK_PIPELINE_CREATE_DERIVATIVE_BIT"
        showsPrec p (VkPipelineCreateBitmask x)
          = showParen (p >= 11)
              (showString "VkPipelineCreateBitmask " . showsPrec 11 x)

instance Read (VkPipelineCreateBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT",
                   pure VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT),
                  ("VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT",
                   pure VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT),
                  ("VK_PIPELINE_CREATE_DERIVATIVE_BIT",
                   pure VK_PIPELINE_CREATE_DERIVATIVE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkPipelineCreateBitmask") >>
                      (VkPipelineCreateBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT =
        VkPipelineCreateBitmask 1

-- | bitpos = @1@
pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT =
        VkPipelineCreateBitmask 2

-- | bitpos = @2@
pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT =
        VkPipelineCreateBitmask 4
