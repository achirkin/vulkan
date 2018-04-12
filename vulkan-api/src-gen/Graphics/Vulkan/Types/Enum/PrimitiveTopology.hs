{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.PrimitiveTopology
       (VkPrimitiveTopology(VkPrimitiveTopology,
                            VK_PRIMITIVE_TOPOLOGY_POINT_LIST, VK_PRIMITIVE_TOPOLOGY_LINE_LIST,
                            VK_PRIMITIVE_TOPOLOGY_LINE_STRIP,
                            VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                            VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
                            VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN,
                            VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY,
                            VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY,
                            VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY,
                            VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY,
                            VK_PRIMITIVE_TOPOLOGY_PATCH_LIST))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPrimitiveTopology VkPrimitiveTopology registry at www.khronos.org>
newtype VkPrimitiveTopology = VkPrimitiveTopology Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkPrimitiveTopology where
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_POINT_LIST
          = showString "VK_PRIMITIVE_TOPOLOGY_POINT_LIST"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_LIST
          = showString "VK_PRIMITIVE_TOPOLOGY_LINE_LIST"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_STRIP
          = showString "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
          = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
          = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
          = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
          = showString "VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
          = showString "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
          = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
          = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY"
        showsPrec _ VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
          = showString "VK_PRIMITIVE_TOPOLOGY_PATCH_LIST"
        showsPrec p (VkPrimitiveTopology x)
          = showParen (p >= 11)
              (showString "VkPrimitiveTopology " . showsPrec 11 x)

instance Read VkPrimitiveTopology where
        readPrec
          = parens
              (choose
                 [("VK_PRIMITIVE_TOPOLOGY_POINT_LIST",
                   pure VK_PRIMITIVE_TOPOLOGY_POINT_LIST),
                  ("VK_PRIMITIVE_TOPOLOGY_LINE_LIST",
                   pure VK_PRIMITIVE_TOPOLOGY_LINE_LIST),
                  ("VK_PRIMITIVE_TOPOLOGY_LINE_STRIP",
                   pure VK_PRIMITIVE_TOPOLOGY_LINE_STRIP),
                  ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST",
                   pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST),
                  ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP",
                   pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP),
                  ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN",
                   pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN),
                  ("VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY",
                   pure VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY),
                  ("VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY",
                   pure VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY),
                  ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY",
                   pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY),
                  ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY",
                   pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY),
                  ("VK_PRIMITIVE_TOPOLOGY_PATCH_LIST",
                   pure VK_PRIMITIVE_TOPOLOGY_PATCH_LIST)]
                 +++
                 prec 10
                   (expectP (Ident "VkPrimitiveTopology") >>
                      (VkPrimitiveTopology <$> step readPrec)))

pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST = VkPrimitiveTopology 0

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST = VkPrimitiveTopology 1

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP = VkPrimitiveTopology 2

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = VkPrimitiveTopology 3

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP =
        VkPrimitiveTopology 4

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = VkPrimitiveTopology 5

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY ::
        VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY =
        VkPrimitiveTopology 6

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY ::
        VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY =
        VkPrimitiveTopology 7

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY ::
        VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY =
        VkPrimitiveTopology 8

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY ::
        VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY =
        VkPrimitiveTopology 9

pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST :: VkPrimitiveTopology

pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST = VkPrimitiveTopology 10
