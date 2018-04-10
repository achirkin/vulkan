{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineCacheHeaderVersion
       (VkPipelineCacheHeaderVersion(VkPipelineCacheHeaderVersion,
                                     VK_PIPELINE_CACHE_HEADER_VERSION_ONE))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineCacheHeaderVersion VkPipelineCacheHeaderVersion registry at www.khronos.org>
newtype VkPipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion Int32
                                         deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                   Generic)

instance Show VkPipelineCacheHeaderVersion where
        showsPrec _ VK_PIPELINE_CACHE_HEADER_VERSION_ONE
          = showString "VK_PIPELINE_CACHE_HEADER_VERSION_ONE"
        showsPrec p (VkPipelineCacheHeaderVersion x)
          = showParen (p >= 11)
              (showString "VkPipelineCacheHeaderVersion " . showsPrec 11 x)

instance Read VkPipelineCacheHeaderVersion where
        readPrec
          = parens
              (choose
                 [("VK_PIPELINE_CACHE_HEADER_VERSION_ONE",
                   pure VK_PIPELINE_CACHE_HEADER_VERSION_ONE)]
                 +++
                 prec 10
                   (expectP (Ident "VkPipelineCacheHeaderVersion") >>
                      (VkPipelineCacheHeaderVersion <$> step readPrec)))

pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE ::
        VkPipelineCacheHeaderVersion

pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE =
        VkPipelineCacheHeaderVersion 1
