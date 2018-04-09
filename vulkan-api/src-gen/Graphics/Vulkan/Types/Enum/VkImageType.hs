{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkImageType
       (VkImageType(VkImageType, VK_IMAGE_TYPE_1D, VK_IMAGE_TYPE_2D,
                    VK_IMAGE_TYPE_3D))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageTypeVkImageType registry at www.khronos.org>
newtype VkImageType = VkImageType Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageType where
        showsPrec _ VK_IMAGE_TYPE_1D = showString "VK_IMAGE_TYPE_1D"
        showsPrec _ VK_IMAGE_TYPE_2D = showString "VK_IMAGE_TYPE_2D"
        showsPrec _ VK_IMAGE_TYPE_3D = showString "VK_IMAGE_TYPE_3D"
        showsPrec p (VkImageType x)
          = showParen (p >= 11) (showString "VkImageType " . showsPrec 11 x)

instance Read VkImageType where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_TYPE_1D", pure VK_IMAGE_TYPE_1D),
                  ("VK_IMAGE_TYPE_2D", pure VK_IMAGE_TYPE_2D),
                  ("VK_IMAGE_TYPE_3D", pure VK_IMAGE_TYPE_3D)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageType") >> (VkImageType <$> step readPrec)))

pattern VK_IMAGE_TYPE_1D :: VkImageType

pattern VK_IMAGE_TYPE_1D = VkImageType 0

pattern VK_IMAGE_TYPE_2D :: VkImageType

pattern VK_IMAGE_TYPE_2D = VkImageType 1

pattern VK_IMAGE_TYPE_3D :: VkImageType

pattern VK_IMAGE_TYPE_3D = VkImageType 2
