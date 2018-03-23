{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkIndirectCommandsTokenTypeNVX
       (VkIndirectCommandsTokenTypeNVX(VkIndirectCommandsTokenTypeNVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX,
                                       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX))
       where
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkIndirectCommandsTokenTypeNVX.html VkIndirectCommandsTokenTypeNVX registry at www.khronos.org>
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
