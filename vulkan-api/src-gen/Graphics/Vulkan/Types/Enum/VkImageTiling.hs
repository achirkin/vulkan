{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkImageTiling
       (VkImageTiling(VkImageTiling, VK_IMAGE_TILING_OPTIMAL,
                      VK_IMAGE_TILING_LINEAR))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageTilingVkImageTiling registry at www.khronos.org>
newtype VkImageTiling = VkImageTiling Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageTiling where
        showsPrec _ VK_IMAGE_TILING_OPTIMAL
          = showString "VK_IMAGE_TILING_OPTIMAL"
        showsPrec _ VK_IMAGE_TILING_LINEAR
          = showString "VK_IMAGE_TILING_LINEAR"
        showsPrec p (VkImageTiling x)
          = showParen (p >= 11)
              (showString "VkImageTiling " . showsPrec 11 x)

instance Read VkImageTiling where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_TILING_OPTIMAL", pure VK_IMAGE_TILING_OPTIMAL),
                  ("VK_IMAGE_TILING_LINEAR", pure VK_IMAGE_TILING_LINEAR)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageTiling") >>
                      (VkImageTiling <$> step readPrec)))

pattern VK_IMAGE_TILING_OPTIMAL :: VkImageTiling

pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0

pattern VK_IMAGE_TILING_LINEAR :: VkImageTiling

pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1
