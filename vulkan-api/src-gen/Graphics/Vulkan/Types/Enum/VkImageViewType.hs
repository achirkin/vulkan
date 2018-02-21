{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkImageViewType
       (VkImageViewType(VkImageViewType, VK_IMAGE_VIEW_TYPE_1D,
                        VK_IMAGE_VIEW_TYPE_2D, VK_IMAGE_VIEW_TYPE_3D,
                        VK_IMAGE_VIEW_TYPE_CUBE, VK_IMAGE_VIEW_TYPE_1D_ARRAY,
                        VK_IMAGE_VIEW_TYPE_2D_ARRAY, VK_IMAGE_VIEW_TYPE_CUBE_ARRAY))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageViewType.html VkImageViewType registry at www.khronos.org>
newtype VkImageViewType = VkImageViewType Int32
                            deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageViewType where
        showsPrec _ VK_IMAGE_VIEW_TYPE_1D
          = showString "VK_IMAGE_VIEW_TYPE_1D"
        showsPrec _ VK_IMAGE_VIEW_TYPE_2D
          = showString "VK_IMAGE_VIEW_TYPE_2D"
        showsPrec _ VK_IMAGE_VIEW_TYPE_3D
          = showString "VK_IMAGE_VIEW_TYPE_3D"
        showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE
          = showString "VK_IMAGE_VIEW_TYPE_CUBE"
        showsPrec _ VK_IMAGE_VIEW_TYPE_1D_ARRAY
          = showString "VK_IMAGE_VIEW_TYPE_1D_ARRAY"
        showsPrec _ VK_IMAGE_VIEW_TYPE_2D_ARRAY
          = showString "VK_IMAGE_VIEW_TYPE_2D_ARRAY"
        showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
          = showString "VK_IMAGE_VIEW_TYPE_CUBE_ARRAY"
        showsPrec p (VkImageViewType x)
          = showParen (p >= 11)
              (showString "VkImageViewType " . showsPrec 11 x)

instance Read VkImageViewType where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_VIEW_TYPE_1D", pure VK_IMAGE_VIEW_TYPE_1D),
                  ("VK_IMAGE_VIEW_TYPE_2D", pure VK_IMAGE_VIEW_TYPE_2D),
                  ("VK_IMAGE_VIEW_TYPE_3D", pure VK_IMAGE_VIEW_TYPE_3D),
                  ("VK_IMAGE_VIEW_TYPE_CUBE", pure VK_IMAGE_VIEW_TYPE_CUBE),
                  ("VK_IMAGE_VIEW_TYPE_1D_ARRAY", pure VK_IMAGE_VIEW_TYPE_1D_ARRAY),
                  ("VK_IMAGE_VIEW_TYPE_2D_ARRAY", pure VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                  ("VK_IMAGE_VIEW_TYPE_CUBE_ARRAY",
                   pure VK_IMAGE_VIEW_TYPE_CUBE_ARRAY)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageViewType") >>
                      (VkImageViewType <$> step readPrec)))

pattern VK_IMAGE_VIEW_TYPE_1D :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_1D = VkImageViewType 0

pattern VK_IMAGE_VIEW_TYPE_2D :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_2D = VkImageViewType 1

pattern VK_IMAGE_VIEW_TYPE_3D :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_3D = VkImageViewType 2

pattern VK_IMAGE_VIEW_TYPE_CUBE :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_CUBE = VkImageViewType 3

pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY = VkImageViewType 4

pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY = VkImageViewType 5

pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = VkImageViewType 6
