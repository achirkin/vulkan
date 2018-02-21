{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkImageLayout
       (VkImageLayout(VkImageLayout, VK_IMAGE_LAYOUT_UNDEFINED,
                      VK_IMAGE_LAYOUT_GENERAL, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,
                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                      VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                      VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                      VK_IMAGE_LAYOUT_PREINITIALIZED))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageLayout.html VkImageLayout registry at www.khronos.org>
newtype VkImageLayout = VkImageLayout Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageLayout where
        showsPrec _ VK_IMAGE_LAYOUT_UNDEFINED
          = showString "VK_IMAGE_LAYOUT_UNDEFINED"
        showsPrec _ VK_IMAGE_LAYOUT_GENERAL
          = showString "VK_IMAGE_LAYOUT_GENERAL"
        showsPrec _ VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_PREINITIALIZED
          = showString "VK_IMAGE_LAYOUT_PREINITIALIZED"
        showsPrec p (VkImageLayout x)
          = showParen (p >= 11)
              (showString "VkImageLayout " . showsPrec 11 x)

instance Read VkImageLayout where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_LAYOUT_UNDEFINED", pure VK_IMAGE_LAYOUT_UNDEFINED),
                  ("VK_IMAGE_LAYOUT_GENERAL", pure VK_IMAGE_LAYOUT_GENERAL),
                  ("VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_PREINITIALIZED",
                   pure VK_IMAGE_LAYOUT_PREINITIALIZED)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageLayout") >>
                      (VkImageLayout <$> step readPrec)))

-- | Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
pattern VK_IMAGE_LAYOUT_UNDEFINED :: VkImageLayout

pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0

-- | General layout when image can be used for any kind of access
pattern VK_IMAGE_LAYOUT_GENERAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout 1

-- | Optimal layout when image is only used for color attachment read/write
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VkImageLayout 2

-- | Optimal layout when image is only used for depth/stencil attachment read/write
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL ::
        VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL =
        VkImageLayout 3

-- | Optimal layout when image is used for read only depth/stencil attachment and shader access
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL ::
        VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL =
        VkImageLayout 4

-- | Optimal layout when image is used for read only shader access
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VkImageLayout 5

-- | Optimal layout when image is used only as source of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VkImageLayout 6

-- | Optimal layout when image is used only as destination of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VkImageLayout 7

-- | Initial layout used when the data is populated by the CPU
pattern VK_IMAGE_LAYOUT_PREINITIALIZED :: VkImageLayout

pattern VK_IMAGE_LAYOUT_PREINITIALIZED = VkImageLayout 8
