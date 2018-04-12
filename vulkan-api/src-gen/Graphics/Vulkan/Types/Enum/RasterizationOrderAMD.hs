{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.RasterizationOrderAMD
       (VkRasterizationOrderAMD(VkRasterizationOrderAMD,
                                VK_RASTERIZATION_ORDER_STRICT_AMD,
                                VK_RASTERIZATION_ORDER_RELAXED_AMD))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRasterizationOrderAMD VkRasterizationOrderAMD registry at www.khronos.org>
newtype VkRasterizationOrderAMD = VkRasterizationOrderAMD Int32
                                    deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkRasterizationOrderAMD where
        showsPrec _ VK_RASTERIZATION_ORDER_STRICT_AMD
          = showString "VK_RASTERIZATION_ORDER_STRICT_AMD"
        showsPrec _ VK_RASTERIZATION_ORDER_RELAXED_AMD
          = showString "VK_RASTERIZATION_ORDER_RELAXED_AMD"
        showsPrec p (VkRasterizationOrderAMD x)
          = showParen (p >= 11)
              (showString "VkRasterizationOrderAMD " . showsPrec 11 x)

instance Read VkRasterizationOrderAMD where
        readPrec
          = parens
              (choose
                 [("VK_RASTERIZATION_ORDER_STRICT_AMD",
                   pure VK_RASTERIZATION_ORDER_STRICT_AMD),
                  ("VK_RASTERIZATION_ORDER_RELAXED_AMD",
                   pure VK_RASTERIZATION_ORDER_RELAXED_AMD)]
                 +++
                 prec 10
                   (expectP (Ident "VkRasterizationOrderAMD") >>
                      (VkRasterizationOrderAMD <$> step readPrec)))

pattern VK_RASTERIZATION_ORDER_STRICT_AMD ::
        VkRasterizationOrderAMD

pattern VK_RASTERIZATION_ORDER_STRICT_AMD =
        VkRasterizationOrderAMD 0

pattern VK_RASTERIZATION_ORDER_RELAXED_AMD ::
        VkRasterizationOrderAMD

pattern VK_RASTERIZATION_ORDER_RELAXED_AMD =
        VkRasterizationOrderAMD 1
