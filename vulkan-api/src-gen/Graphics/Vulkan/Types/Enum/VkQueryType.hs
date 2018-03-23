{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkQueryType
       (VkQueryType(VkQueryType, VK_QUERY_TYPE_OCCLUSION,
                    VK_QUERY_TYPE_PIPELINE_STATISTICS, VK_QUERY_TYPE_TIMESTAMP))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkQueryType.html VkQueryType registry at www.khronos.org>
newtype VkQueryType = VkQueryType Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkQueryType where
        showsPrec _ VK_QUERY_TYPE_OCCLUSION
          = showString "VK_QUERY_TYPE_OCCLUSION"
        showsPrec _ VK_QUERY_TYPE_PIPELINE_STATISTICS
          = showString "VK_QUERY_TYPE_PIPELINE_STATISTICS"
        showsPrec _ VK_QUERY_TYPE_TIMESTAMP
          = showString "VK_QUERY_TYPE_TIMESTAMP"
        showsPrec p (VkQueryType x)
          = showParen (p >= 11) (showString "VkQueryType " . showsPrec 11 x)

instance Read VkQueryType where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_TYPE_OCCLUSION", pure VK_QUERY_TYPE_OCCLUSION),
                  ("VK_QUERY_TYPE_PIPELINE_STATISTICS",
                   pure VK_QUERY_TYPE_PIPELINE_STATISTICS),
                  ("VK_QUERY_TYPE_TIMESTAMP", pure VK_QUERY_TYPE_TIMESTAMP)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryType") >> (VkQueryType <$> step readPrec)))

pattern VK_QUERY_TYPE_OCCLUSION :: VkQueryType

pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0

-- | Optional
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS :: VkQueryType

pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1

pattern VK_QUERY_TYPE_TIMESTAMP :: VkQueryType

pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2
