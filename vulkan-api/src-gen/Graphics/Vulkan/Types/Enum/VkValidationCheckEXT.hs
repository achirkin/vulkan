{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkValidationCheckEXT
       (VkValidationCheckEXT(VkValidationCheckEXT,
                             VK_VALIDATION_CHECK_ALL_EXT, VK_VALIDATION_CHECK_SHADERS_EXT))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkValidationCheckEXT.html VkValidationCheckEXT registry at www.khronos.org>
newtype VkValidationCheckEXT = VkValidationCheckEXT Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

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
