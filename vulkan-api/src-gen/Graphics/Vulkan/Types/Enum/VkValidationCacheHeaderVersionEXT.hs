{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkValidationCacheHeaderVersionEXT
       (VkValidationCacheHeaderVersionEXT(VkValidationCacheHeaderVersionEXT,
                                          VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkValidationCacheHeaderVersionEXTVkValidationCacheHeaderVersionEXT registry at www.khronos.org>
newtype VkValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT Int32
                                              deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                        Generic)

instance Show VkValidationCacheHeaderVersionEXT where
        showsPrec _ VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
          = showString "VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"
        showsPrec p (VkValidationCacheHeaderVersionEXT x)
          = showParen (p >= 11)
              (showString "VkValidationCacheHeaderVersionEXT " . showsPrec 11 x)

instance Read VkValidationCacheHeaderVersionEXT where
        readPrec
          = parens
              (choose
                 [("VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT",
                   pure VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkValidationCacheHeaderVersionEXT") >>
                      (VkValidationCacheHeaderVersionEXT <$> step readPrec)))

pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT ::
        VkValidationCacheHeaderVersionEXT

pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT =
        VkValidationCacheHeaderVersionEXT 1
