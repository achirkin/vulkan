{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkBlendOverlapEXT
       (VkBlendOverlapEXT(VkBlendOverlapEXT,
                          VK_BLEND_OVERLAP_UNCORRELATED_EXT, VK_BLEND_OVERLAP_DISJOINT_EXT,
                          VK_BLEND_OVERLAP_CONJOINT_EXT))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBlendOverlapEXT.html VkBlendOverlapEXT registry at www.khronos.org>
newtype VkBlendOverlapEXT = VkBlendOverlapEXT Int32
                              deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkBlendOverlapEXT where
        showsPrec _ VK_BLEND_OVERLAP_UNCORRELATED_EXT
          = showString "VK_BLEND_OVERLAP_UNCORRELATED_EXT"
        showsPrec _ VK_BLEND_OVERLAP_DISJOINT_EXT
          = showString "VK_BLEND_OVERLAP_DISJOINT_EXT"
        showsPrec _ VK_BLEND_OVERLAP_CONJOINT_EXT
          = showString "VK_BLEND_OVERLAP_CONJOINT_EXT"
        showsPrec p (VkBlendOverlapEXT x)
          = showParen (p >= 11)
              (showString "VkBlendOverlapEXT " . showsPrec 11 x)

instance Read VkBlendOverlapEXT where
        readPrec
          = parens
              (choose
                 [("VK_BLEND_OVERLAP_UNCORRELATED_EXT",
                   pure VK_BLEND_OVERLAP_UNCORRELATED_EXT),
                  ("VK_BLEND_OVERLAP_DISJOINT_EXT",
                   pure VK_BLEND_OVERLAP_DISJOINT_EXT),
                  ("VK_BLEND_OVERLAP_CONJOINT_EXT",
                   pure VK_BLEND_OVERLAP_CONJOINT_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkBlendOverlapEXT") >>
                      (VkBlendOverlapEXT <$> step readPrec)))

pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT = VkBlendOverlapEXT 0

pattern VK_BLEND_OVERLAP_DISJOINT_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_DISJOINT_EXT = VkBlendOverlapEXT 1

pattern VK_BLEND_OVERLAP_CONJOINT_EXT :: VkBlendOverlapEXT

pattern VK_BLEND_OVERLAP_CONJOINT_EXT = VkBlendOverlapEXT 2
