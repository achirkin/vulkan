{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.ValidationC
       (VkValidationCacheHeaderVersionEXT(VkValidationCacheHeaderVersionEXT,
                                          VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT),
        VkValidationCheckEXT(VkValidationCheckEXT,
                             VK_VALIDATION_CHECK_ALL_EXT, VK_VALIDATION_CHECK_SHADERS_EXT))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkValidationCacheHeaderVersionEXT VkValidationCacheHeaderVersionEXT registry at www.khronos.org>
newtype VkValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT Int32
                                            deriving (Eq, Ord, Enum, Storable)

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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkValidationCheckEXT VkValidationCheckEXT registry at www.khronos.org>
newtype VkValidationCheckEXT = VkValidationCheckEXT Int32
                               deriving (Eq, Ord, Enum, Storable)

instance Show VkValidationCheckEXT where
    showsPrec _ VK_VALIDATION_CHECK_ALL_EXT
      = showString "VK_VALIDATION_CHECK_ALL_EXT"
    showsPrec _ VK_VALIDATION_CHECK_SHADERS_EXT
      = showString "VK_VALIDATION_CHECK_SHADERS_EXT"
    showsPrec p (VkValidationCheckEXT x)
      = showParen (p >= 11)
          (showString "VkValidationCheckEXT " . showsPrec 11 x)

instance Read VkValidationCheckEXT where
    readPrec
      = parens
          (choose
             [("VK_VALIDATION_CHECK_ALL_EXT", pure VK_VALIDATION_CHECK_ALL_EXT),
              ("VK_VALIDATION_CHECK_SHADERS_EXT",
               pure VK_VALIDATION_CHECK_SHADERS_EXT)]
             +++
             prec 10
               (expectP (Ident "VkValidationCheckEXT") >>
                  (VkValidationCheckEXT <$> step readPrec)))

pattern VK_VALIDATION_CHECK_ALL_EXT :: VkValidationCheckEXT

pattern VK_VALIDATION_CHECK_ALL_EXT = VkValidationCheckEXT 0

pattern VK_VALIDATION_CHECK_SHADERS_EXT :: VkValidationCheckEXT

pattern VK_VALIDATION_CHECK_SHADERS_EXT = VkValidationCheckEXT 1
