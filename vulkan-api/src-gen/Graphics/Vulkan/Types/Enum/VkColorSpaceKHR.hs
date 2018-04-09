{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkColorSpaceKHR
       (VkColorSpaceKHR(VkColorSpaceKHR,
                        VK_COLOR_SPACE_SRGB_NONLINEAR_KHR))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkColorSpaceKHRVkColorSpaceKHR registry at www.khronos.org>
newtype VkColorSpaceKHR = VkColorSpaceKHR Int32
                            deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkColorSpaceKHR where
        showsPrec _ VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
          = showString "VK_COLOR_SPACE_SRGB_NONLINEAR_KHR"
        showsPrec p (VkColorSpaceKHR x)
          = showParen (p >= 11)
              (showString "VkColorSpaceKHR " . showsPrec 11 x)

instance Read VkColorSpaceKHR where
        readPrec
          = parens
              (choose
                 [("VK_COLOR_SPACE_SRGB_NONLINEAR_KHR",
                   pure VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkColorSpaceKHR") >>
                      (VkColorSpaceKHR <$> step readPrec)))

pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0
