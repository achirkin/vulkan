{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.SharingMode
       (VkSharingMode(VkSharingMode, VK_SHARING_MODE_EXCLUSIVE,
                      VK_SHARING_MODE_CONCURRENT))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSharingMode VkSharingMode registry at www.khronos.org>
newtype VkSharingMode = VkSharingMode Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkSharingMode where
        showsPrec _ VK_SHARING_MODE_EXCLUSIVE
          = showString "VK_SHARING_MODE_EXCLUSIVE"
        showsPrec _ VK_SHARING_MODE_CONCURRENT
          = showString "VK_SHARING_MODE_CONCURRENT"
        showsPrec p (VkSharingMode x)
          = showParen (p >= 11)
              (showString "VkSharingMode " . showsPrec 11 x)

instance Read VkSharingMode where
        readPrec
          = parens
              (choose
                 [("VK_SHARING_MODE_EXCLUSIVE", pure VK_SHARING_MODE_EXCLUSIVE),
                  ("VK_SHARING_MODE_CONCURRENT", pure VK_SHARING_MODE_CONCURRENT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSharingMode") >>
                      (VkSharingMode <$> step readPrec)))

pattern VK_SHARING_MODE_EXCLUSIVE :: VkSharingMode

pattern VK_SHARING_MODE_EXCLUSIVE = VkSharingMode 0

pattern VK_SHARING_MODE_CONCURRENT :: VkSharingMode

pattern VK_SHARING_MODE_CONCURRENT = VkSharingMode 1
