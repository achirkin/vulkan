{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkCommandBufferLevel
       (VkCommandBufferLevel(VkCommandBufferLevel,
                             VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                             VK_COMMAND_BUFFER_LEVEL_SECONDARY))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandBufferLevel VkCommandBufferLevel registry at www.khronos.org>
newtype VkCommandBufferLevel = VkCommandBufferLevel Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkCommandBufferLevel where
        showsPrec _ VK_COMMAND_BUFFER_LEVEL_PRIMARY
          = showString "VK_COMMAND_BUFFER_LEVEL_PRIMARY"
        showsPrec _ VK_COMMAND_BUFFER_LEVEL_SECONDARY
          = showString "VK_COMMAND_BUFFER_LEVEL_SECONDARY"
        showsPrec p (VkCommandBufferLevel x)
          = showParen (p >= 11)
              (showString "VkCommandBufferLevel " . showsPrec 11 x)

instance Read VkCommandBufferLevel where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_BUFFER_LEVEL_PRIMARY",
                   pure VK_COMMAND_BUFFER_LEVEL_PRIMARY),
                  ("VK_COMMAND_BUFFER_LEVEL_SECONDARY",
                   pure VK_COMMAND_BUFFER_LEVEL_SECONDARY)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandBufferLevel") >>
                      (VkCommandBufferLevel <$> step readPrec)))

pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY :: VkCommandBufferLevel

pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY :: VkCommandBufferLevel

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1
