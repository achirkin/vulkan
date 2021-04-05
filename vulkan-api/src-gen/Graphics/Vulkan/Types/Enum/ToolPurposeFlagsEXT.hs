{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.ToolPurposeFlagsEXT
       (VkToolPurposeBitmaskEXT(VkToolPurposeBitmaskEXT,
                                VkToolPurposeFlagsEXT, VkToolPurposeFlagBitsEXT,
                                VK_TOOL_PURPOSE_VALIDATION_BIT_EXT,
                                VK_TOOL_PURPOSE_PROFILING_BIT_EXT, VK_TOOL_PURPOSE_TRACING_BIT_EXT,
                                VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT,
                                VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT),
        VkToolPurposeFlagsEXT, VkToolPurposeFlagBitsEXT)
       where
import Data.Bits                       (Bits, FiniteBits)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

newtype VkToolPurposeBitmaskEXT (a ::
                                   FlagType) = VkToolPurposeBitmaskEXT VkFlags
                                               deriving (Eq, Ord, Storable)

type VkToolPurposeFlagsEXT = VkToolPurposeBitmaskEXT FlagMask

type VkToolPurposeFlagBitsEXT = VkToolPurposeBitmaskEXT FlagBit

pattern VkToolPurposeFlagBitsEXT ::
        VkFlags -> VkToolPurposeBitmaskEXT FlagBit

pattern VkToolPurposeFlagBitsEXT n = VkToolPurposeBitmaskEXT n

pattern VkToolPurposeFlagsEXT ::
        VkFlags -> VkToolPurposeBitmaskEXT FlagMask

pattern VkToolPurposeFlagsEXT n = VkToolPurposeBitmaskEXT n

deriving instance Bits (VkToolPurposeBitmaskEXT FlagMask)

deriving instance FiniteBits (VkToolPurposeBitmaskEXT FlagMask)

instance Show (VkToolPurposeBitmaskEXT a) where
    showsPrec _ VK_TOOL_PURPOSE_VALIDATION_BIT_EXT
      = showString "VK_TOOL_PURPOSE_VALIDATION_BIT_EXT"
    showsPrec _ VK_TOOL_PURPOSE_PROFILING_BIT_EXT
      = showString "VK_TOOL_PURPOSE_PROFILING_BIT_EXT"
    showsPrec _ VK_TOOL_PURPOSE_TRACING_BIT_EXT
      = showString "VK_TOOL_PURPOSE_TRACING_BIT_EXT"
    showsPrec _ VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT
      = showString "VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT"
    showsPrec _ VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT
      = showString "VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT"
    showsPrec p (VkToolPurposeBitmaskEXT x)
      = showParen (p >= 11)
          (showString "VkToolPurposeBitmaskEXT " . showsPrec 11 x)

instance Read (VkToolPurposeBitmaskEXT a) where
    readPrec
      = parens
          (choose
             [("VK_TOOL_PURPOSE_VALIDATION_BIT_EXT",
               pure VK_TOOL_PURPOSE_VALIDATION_BIT_EXT),
              ("VK_TOOL_PURPOSE_PROFILING_BIT_EXT",
               pure VK_TOOL_PURPOSE_PROFILING_BIT_EXT),
              ("VK_TOOL_PURPOSE_TRACING_BIT_EXT",
               pure VK_TOOL_PURPOSE_TRACING_BIT_EXT),
              ("VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT",
               pure VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT),
              ("VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT",
               pure VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT)]
             +++
             prec 10
               (expectP (Ident "VkToolPurposeBitmaskEXT") >>
                  (VkToolPurposeBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_TOOL_PURPOSE_VALIDATION_BIT_EXT ::
        VkToolPurposeBitmaskEXT a

pattern VK_TOOL_PURPOSE_VALIDATION_BIT_EXT =
        VkToolPurposeBitmaskEXT 1

-- | bitpos = @1@
pattern VK_TOOL_PURPOSE_PROFILING_BIT_EXT ::
        VkToolPurposeBitmaskEXT a

pattern VK_TOOL_PURPOSE_PROFILING_BIT_EXT =
        VkToolPurposeBitmaskEXT 2

-- | bitpos = @2@
pattern VK_TOOL_PURPOSE_TRACING_BIT_EXT ::
        VkToolPurposeBitmaskEXT a

pattern VK_TOOL_PURPOSE_TRACING_BIT_EXT = VkToolPurposeBitmaskEXT 4

-- | bitpos = @3@
pattern VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT ::
        VkToolPurposeBitmaskEXT a

pattern VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT =
        VkToolPurposeBitmaskEXT 8

-- | bitpos = @4@
pattern VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT ::
        VkToolPurposeBitmaskEXT a

pattern VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT =
        VkToolPurposeBitmaskEXT 16
