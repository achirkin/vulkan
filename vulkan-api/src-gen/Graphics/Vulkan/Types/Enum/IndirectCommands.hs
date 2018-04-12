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
module Graphics.Vulkan.Types.Enum.IndirectCommands
       (VkIndirectCommandsLayoutUsageBitmaskNVX(VkIndirectCommandsLayoutUsageBitmaskNVX,
                                                VkIndirectCommandsLayoutUsageFlagsNVX,
                                                VkIndirectCommandsLayoutUsageFlagBitsNVX,
                                                VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX,
                                                VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX,
                                                VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX,
                                                VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX),
        VkIndirectCommandsLayoutUsageFlagsNVX,
        VkIndirectCommandsLayoutUsageFlagBitsNVX,
        VkIndirectCommandsTokenTypeNVX(VkIndirectCommandsTokenTypeNVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX))
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkIndirectCommandsLayoutUsageBitmaskNVX (a ::
                                                   FlagType) = VkIndirectCommandsLayoutUsageBitmaskNVX VkFlags
                                                                 deriving (Eq, Ord, Storable, Data,
                                                                           Generic)

type VkIndirectCommandsLayoutUsageFlagsNVX =
     VkIndirectCommandsLayoutUsageBitmaskNVX FlagMask

type VkIndirectCommandsLayoutUsageFlagBitsNVX =
     VkIndirectCommandsLayoutUsageBitmaskNVX FlagBit

pattern VkIndirectCommandsLayoutUsageFlagBitsNVX ::
        VkFlags -> VkIndirectCommandsLayoutUsageBitmaskNVX FlagBit

pattern VkIndirectCommandsLayoutUsageFlagBitsNVX n =
        VkIndirectCommandsLayoutUsageBitmaskNVX n

pattern VkIndirectCommandsLayoutUsageFlagsNVX ::
        VkFlags -> VkIndirectCommandsLayoutUsageBitmaskNVX FlagMask

pattern VkIndirectCommandsLayoutUsageFlagsNVX n =
        VkIndirectCommandsLayoutUsageBitmaskNVX n

deriving instance
         Bits (VkIndirectCommandsLayoutUsageBitmaskNVX FlagMask)

deriving instance
         FiniteBits (VkIndirectCommandsLayoutUsageBitmaskNVX FlagMask)

deriving instance
         Integral (VkIndirectCommandsLayoutUsageBitmaskNVX FlagMask)

deriving instance
         Num (VkIndirectCommandsLayoutUsageBitmaskNVX FlagMask)

deriving instance
         Bounded (VkIndirectCommandsLayoutUsageBitmaskNVX FlagMask)

deriving instance
         Enum (VkIndirectCommandsLayoutUsageBitmaskNVX FlagMask)

deriving instance
         Real (VkIndirectCommandsLayoutUsageBitmaskNVX FlagMask)

instance Show (VkIndirectCommandsLayoutUsageBitmaskNVX a) where
        showsPrec _
          VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
          = showString
              "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX"
        showsPrec _
          VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
          = showString
              "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX"
        showsPrec _
          VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
          = showString
              "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX"
        showsPrec _
          VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
          = showString
              "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX"
        showsPrec p (VkIndirectCommandsLayoutUsageBitmaskNVX x)
          = showParen (p >= 11)
              (showString "VkIndirectCommandsLayoutUsageBitmaskNVX " .
                 showsPrec 11 x)

instance Read (VkIndirectCommandsLayoutUsageBitmaskNVX a) where
        readPrec
          = parens
              (choose
                 [("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX",
                   pure
                     VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX),
                  ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX",
                   pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX),
                  ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX",
                   pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX),
                  ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX",
                   pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX)]
                 +++
                 prec 10
                   (expectP (Ident "VkIndirectCommandsLayoutUsageBitmaskNVX") >>
                      (VkIndirectCommandsLayoutUsageBitmaskNVX <$> step readPrec)))

-- | bitpos = @0@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
        :: VkIndirectCommandsLayoutUsageBitmaskNVX a

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
        = VkIndirectCommandsLayoutUsageBitmaskNVX 1

-- | bitpos = @1@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
        :: VkIndirectCommandsLayoutUsageBitmaskNVX a

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
        = VkIndirectCommandsLayoutUsageBitmaskNVX 2

-- | bitpos = @2@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
        :: VkIndirectCommandsLayoutUsageBitmaskNVX a

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
        = VkIndirectCommandsLayoutUsageBitmaskNVX 4

-- | bitpos = @3@
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
        :: VkIndirectCommandsLayoutUsageBitmaskNVX a

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
        = VkIndirectCommandsLayoutUsageBitmaskNVX 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkIndirectCommandsTokenTypeNVX VkIndirectCommandsTokenTypeNVX registry at www.khronos.org>
newtype VkIndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX Int32
                                           deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                     Generic)

instance Show VkIndirectCommandsTokenTypeNVX where
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX"
        showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX
          = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX"
        showsPrec p (VkIndirectCommandsTokenTypeNVX x)
          = showParen (p >= 11)
              (showString "VkIndirectCommandsTokenTypeNVX " . showsPrec 11 x)

instance Read VkIndirectCommandsTokenTypeNVX where
        readPrec
          = parens
              (choose
                 [("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX),
                  ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX",
                   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX)]
                 +++
                 prec 10
                   (expectP (Ident "VkIndirectCommandsTokenTypeNVX") >>
                      (VkIndirectCommandsTokenTypeNVX <$> step readPrec)))

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX =
        VkIndirectCommandsTokenTypeNVX 0

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX =
        VkIndirectCommandsTokenTypeNVX 1

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX =
        VkIndirectCommandsTokenTypeNVX 2

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX =
        VkIndirectCommandsTokenTypeNVX 3

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX =
        VkIndirectCommandsTokenTypeNVX 4

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX =
        VkIndirectCommandsTokenTypeNVX 5

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX =
        VkIndirectCommandsTokenTypeNVX 6

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX ::
        VkIndirectCommandsTokenTypeNVX

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX =
        VkIndirectCommandsTokenTypeNVX 7
