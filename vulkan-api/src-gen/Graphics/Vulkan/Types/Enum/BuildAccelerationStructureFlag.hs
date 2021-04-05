{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.BuildAccelerationStructureFlag
       (VkBuildAccelerationStructureFlagBitsNV(..),
        VkBuildAccelerationStructureBitmaskKHR(VkBuildAccelerationStructureBitmaskKHR,
                                               VkBuildAccelerationStructureFlagsKHR,
                                               VkBuildAccelerationStructureFlagBitsKHR,
                                               VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR,
                                               VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR,
                                               VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR,
                                               VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR,
                                               VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR),
        VkBuildAccelerationStructureFlagsKHR,
        VkBuildAccelerationStructureFlagBitsKHR)
       where
import Data.Bits                       (Bits, FiniteBits)
import Data.Coerce                     (coerce)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

newtype VkBuildAccelerationStructureFlagBitsNV = VkBuildAccelerationStructureFlagBitsNV VkFlags
                                                 deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                           Storable)

instance Show VkBuildAccelerationStructureFlagBitsNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkBuildAccelerationStructureFlagBitsNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkBuildAccelerationStructureBitmaskKHR (a ::
                                                  FlagType) = VkBuildAccelerationStructureBitmaskKHR VkFlags
                                                              deriving (Eq, Ord, Storable)

type VkBuildAccelerationStructureFlagsKHR =
     VkBuildAccelerationStructureBitmaskKHR FlagMask

type VkBuildAccelerationStructureFlagBitsKHR =
     VkBuildAccelerationStructureBitmaskKHR FlagBit

pattern VkBuildAccelerationStructureFlagBitsKHR ::
        VkFlags -> VkBuildAccelerationStructureBitmaskKHR FlagBit

pattern VkBuildAccelerationStructureFlagBitsKHR n =
        VkBuildAccelerationStructureBitmaskKHR n

pattern VkBuildAccelerationStructureFlagsKHR ::
        VkFlags -> VkBuildAccelerationStructureBitmaskKHR FlagMask

pattern VkBuildAccelerationStructureFlagsKHR n =
        VkBuildAccelerationStructureBitmaskKHR n

deriving instance
         Bits (VkBuildAccelerationStructureBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkBuildAccelerationStructureBitmaskKHR FlagMask)

instance Show (VkBuildAccelerationStructureBitmaskKHR a) where
    showsPrec _ VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR
      = showString "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR"
    showsPrec _
      VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR
      = showString
          "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR"
    showsPrec _
      VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR
      = showString
          "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR"
    showsPrec _
      VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR
      = showString
          "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR"
    showsPrec _ VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR
      = showString "VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR"
    showsPrec p (VkBuildAccelerationStructureBitmaskKHR x)
      = showParen (p >= 11)
          (showString "VkBuildAccelerationStructureBitmaskKHR " .
             showsPrec 11 x)

instance Read (VkBuildAccelerationStructureBitmaskKHR a) where
    readPrec
      = parens
          (choose
             [("VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR",
               pure VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR),
              ("VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR",
               pure VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR),
              ("VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR",
               pure VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR),
              ("VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR",
               pure VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR),
              ("VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR",
               pure VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR)]
             +++
             prec 10
               (expectP (Ident "VkBuildAccelerationStructureBitmaskKHR") >>
                  (VkBuildAccelerationStructureBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR ::
        VkBuildAccelerationStructureBitmaskKHR a

pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR =
        VkBuildAccelerationStructureBitmaskKHR 1

-- | bitpos = @1@
pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR ::
        VkBuildAccelerationStructureBitmaskKHR a

pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR =
        VkBuildAccelerationStructureBitmaskKHR 2

-- | bitpos = @2@
pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR
        :: VkBuildAccelerationStructureBitmaskKHR a

pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR =
        VkBuildAccelerationStructureBitmaskKHR 4

-- | bitpos = @3@
pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR
        :: VkBuildAccelerationStructureBitmaskKHR a

pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR =
        VkBuildAccelerationStructureBitmaskKHR 8

-- | bitpos = @4@
pattern VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR ::
        VkBuildAccelerationStructureBitmaskKHR a

pattern VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR =
        VkBuildAccelerationStructureBitmaskKHR 16
