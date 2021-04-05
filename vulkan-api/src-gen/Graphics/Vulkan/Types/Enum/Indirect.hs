{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Indirect
       (VkIndirectCommandsLayoutUsageBitmaskNV(VkIndirectCommandsLayoutUsageBitmaskNV,
                                               VkIndirectCommandsLayoutUsageFlagsNV,
                                               VkIndirectCommandsLayoutUsageFlagBitsNV,
                                               VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV,
                                               VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV,
                                               VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV),
        VkIndirectCommandsLayoutUsageFlagsNV,
        VkIndirectCommandsLayoutUsageFlagBitsNV,
        VkIndirectCommandsTokenTypeNV(VkIndirectCommandsTokenTypeNV,
                                      VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV,
                                      VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV,
                                      VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV,
                                      VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV,
                                      VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV,
                                      VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV,
                                      VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV,
                                      VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV),
        VkIndirectStateBitmaskNV(VkIndirectStateBitmaskNV,
                                 VkIndirectStateFlagsNV, VkIndirectStateFlagBitsNV,
                                 VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV),
        VkIndirectStateFlagsNV, VkIndirectStateFlagBitsNV)
       where
import Data.Bits                       (Bits, FiniteBits)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType, Int32)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

newtype VkIndirectCommandsLayoutUsageBitmaskNV (a ::
                                                  FlagType) = VkIndirectCommandsLayoutUsageBitmaskNV VkFlags
                                                              deriving (Eq, Ord, Storable)

type VkIndirectCommandsLayoutUsageFlagsNV =
     VkIndirectCommandsLayoutUsageBitmaskNV FlagMask

type VkIndirectCommandsLayoutUsageFlagBitsNV =
     VkIndirectCommandsLayoutUsageBitmaskNV FlagBit

pattern VkIndirectCommandsLayoutUsageFlagBitsNV ::
        VkFlags -> VkIndirectCommandsLayoutUsageBitmaskNV FlagBit

pattern VkIndirectCommandsLayoutUsageFlagBitsNV n =
        VkIndirectCommandsLayoutUsageBitmaskNV n

pattern VkIndirectCommandsLayoutUsageFlagsNV ::
        VkFlags -> VkIndirectCommandsLayoutUsageBitmaskNV FlagMask

pattern VkIndirectCommandsLayoutUsageFlagsNV n =
        VkIndirectCommandsLayoutUsageBitmaskNV n

deriving instance
         Bits (VkIndirectCommandsLayoutUsageBitmaskNV FlagMask)

deriving instance
         FiniteBits (VkIndirectCommandsLayoutUsageBitmaskNV FlagMask)

instance Show (VkIndirectCommandsLayoutUsageBitmaskNV a) where
    showsPrec _
      VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV
      = showString
          "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV"
    showsPrec _
      VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV
      = showString
          "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV"
    showsPrec _
      VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV
      = showString
          "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV"
    showsPrec p (VkIndirectCommandsLayoutUsageBitmaskNV x)
      = showParen (p >= 11)
          (showString "VkIndirectCommandsLayoutUsageBitmaskNV " .
             showsPrec 11 x)

instance Read (VkIndirectCommandsLayoutUsageBitmaskNV a) where
    readPrec
      = parens
          (choose
             [("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV",
               pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV),
              ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV",
               pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV),
              ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV",
               pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV)]
             +++
             prec 10
               (expectP (Ident "VkIndirectCommandsLayoutUsageBitmaskNV") >>
                  (VkIndirectCommandsLayoutUsageBitmaskNV <$> step readPrec)))

-- | bitpos = @0@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV
        :: VkIndirectCommandsLayoutUsageBitmaskNV a

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV
        = VkIndirectCommandsLayoutUsageBitmaskNV 1

-- | bitpos = @1@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV
        :: VkIndirectCommandsLayoutUsageBitmaskNV a

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV
        = VkIndirectCommandsLayoutUsageBitmaskNV 2

-- | bitpos = @2@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV
        :: VkIndirectCommandsLayoutUsageBitmaskNV a

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV
        = VkIndirectCommandsLayoutUsageBitmaskNV 4

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkIndirectCommandsTokenTypeNV VkIndirectCommandsTokenTypeNV registry at www.khronos.org>
newtype VkIndirectCommandsTokenTypeNV = VkIndirectCommandsTokenTypeNV Int32
                                        deriving (Eq, Ord, Enum, Storable)

instance Show VkIndirectCommandsTokenTypeNV where
    showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV
      = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV"
    showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV
      = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV"
    showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV
      = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV"
    showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV
      = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV"
    showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV
      = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV"
    showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV
      = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV"
    showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV
      = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV"
    showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV
      = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV"
    showsPrec p (VkIndirectCommandsTokenTypeNV x)
      = showParen (p >= 11)
          (showString "VkIndirectCommandsTokenTypeNV " . showsPrec 11 x)

instance Read VkIndirectCommandsTokenTypeNV where
    readPrec
      = parens
          (choose
             [("VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV",
               pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV),
              ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV",
               pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV),
              ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV",
               pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV),
              ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV",
               pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV),
              ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV",
               pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV),
              ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV",
               pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV),
              ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV",
               pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV),
              ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV",
               pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV)]
             +++
             prec 10
               (expectP (Ident "VkIndirectCommandsTokenTypeNV") >>
                  (VkIndirectCommandsTokenTypeNV <$> step readPrec)))

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV ::
        VkIndirectCommandsTokenTypeNV

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV =
        VkIndirectCommandsTokenTypeNV 0

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV ::
        VkIndirectCommandsTokenTypeNV

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV =
        VkIndirectCommandsTokenTypeNV 1

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV ::
        VkIndirectCommandsTokenTypeNV

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV =
        VkIndirectCommandsTokenTypeNV 2

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV ::
        VkIndirectCommandsTokenTypeNV

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV =
        VkIndirectCommandsTokenTypeNV 3

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV ::
        VkIndirectCommandsTokenTypeNV

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV =
        VkIndirectCommandsTokenTypeNV 4

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV ::
        VkIndirectCommandsTokenTypeNV

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV =
        VkIndirectCommandsTokenTypeNV 5

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV ::
        VkIndirectCommandsTokenTypeNV

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV =
        VkIndirectCommandsTokenTypeNV 6

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV ::
        VkIndirectCommandsTokenTypeNV

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV =
        VkIndirectCommandsTokenTypeNV 7

newtype VkIndirectStateBitmaskNV (a ::
                                    FlagType) = VkIndirectStateBitmaskNV VkFlags
                                                deriving (Eq, Ord, Storable)

type VkIndirectStateFlagsNV = VkIndirectStateBitmaskNV FlagMask

type VkIndirectStateFlagBitsNV = VkIndirectStateBitmaskNV FlagBit

pattern VkIndirectStateFlagBitsNV ::
        VkFlags -> VkIndirectStateBitmaskNV FlagBit

pattern VkIndirectStateFlagBitsNV n = VkIndirectStateBitmaskNV n

pattern VkIndirectStateFlagsNV ::
        VkFlags -> VkIndirectStateBitmaskNV FlagMask

pattern VkIndirectStateFlagsNV n = VkIndirectStateBitmaskNV n

deriving instance Bits (VkIndirectStateBitmaskNV FlagMask)

deriving instance FiniteBits (VkIndirectStateBitmaskNV FlagMask)

instance Show (VkIndirectStateBitmaskNV a) where
    showsPrec _ VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV
      = showString "VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV"
    showsPrec p (VkIndirectStateBitmaskNV x)
      = showParen (p >= 11)
          (showString "VkIndirectStateBitmaskNV " . showsPrec 11 x)

instance Read (VkIndirectStateBitmaskNV a) where
    readPrec
      = parens
          (choose
             [("VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV",
               pure VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV)]
             +++
             prec 10
               (expectP (Ident "VkIndirectStateBitmaskNV") >>
                  (VkIndirectStateBitmaskNV <$> step readPrec)))

-- | bitpos = @0@
pattern VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV ::
        VkIndirectStateBitmaskNV a

pattern VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV =
        VkIndirectStateBitmaskNV 1
