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
module Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX
       (VkObjectEntryUsageBitmaskNVX(VkObjectEntryUsageBitmaskNVX,
                                     VkObjectEntryUsageFlagsNVX, VkObjectEntryUsageFlagBitsNVX,
                                     VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX,
                                     VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX),
        VkObjectEntryUsageFlagsNVX, VkObjectEntryUsageFlagBitsNVX)
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

newtype VkObjectEntryUsageBitmaskNVX (a ::
                                        FlagType) = VkObjectEntryUsageBitmaskNVX VkFlags
                                                      deriving (Eq, Ord, Storable, Data, Generic)

type VkObjectEntryUsageFlagsNVX =
     VkObjectEntryUsageBitmaskNVX FlagMask

type VkObjectEntryUsageFlagBitsNVX =
     VkObjectEntryUsageBitmaskNVX FlagBit

pattern VkObjectEntryUsageFlagBitsNVX ::
        VkFlags -> VkObjectEntryUsageBitmaskNVX FlagBit

pattern VkObjectEntryUsageFlagBitsNVX n =
        VkObjectEntryUsageBitmaskNVX n

pattern VkObjectEntryUsageFlagsNVX ::
        VkFlags -> VkObjectEntryUsageBitmaskNVX FlagMask

pattern VkObjectEntryUsageFlagsNVX n =
        VkObjectEntryUsageBitmaskNVX n

deriving instance Bits (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance
         FiniteBits (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance Integral (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance Num (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance Bounded (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance Enum (VkObjectEntryUsageBitmaskNVX FlagMask)

deriving instance Real (VkObjectEntryUsageBitmaskNVX FlagMask)

instance Show (VkObjectEntryUsageBitmaskNVX a) where
        showsPrec _ VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
          = showString "VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX"
        showsPrec _ VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
          = showString "VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX"
        showsPrec p (VkObjectEntryUsageBitmaskNVX x)
          = showParen (p >= 11)
              (showString "VkObjectEntryUsageBitmaskNVX " . showsPrec 11 x)

instance Read (VkObjectEntryUsageBitmaskNVX a) where
        readPrec
          = parens
              (choose
                 [("VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX",
                   pure VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX),
                  ("VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX",
                   pure VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX)]
                 +++
                 prec 10
                   (expectP (Ident "VkObjectEntryUsageBitmaskNVX") >>
                      (VkObjectEntryUsageBitmaskNVX <$> step readPrec)))

-- | bitpos = @0@
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX ::
        VkObjectEntryUsageBitmaskNVX a

pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX =
        VkObjectEntryUsageBitmaskNVX 1

-- | bitpos = @1@
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX ::
        VkObjectEntryUsageBitmaskNVX a

pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX =
        VkObjectEntryUsageBitmaskNVX 2
