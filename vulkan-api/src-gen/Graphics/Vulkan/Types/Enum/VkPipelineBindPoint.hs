{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineBindPoint
       (VkPipelineBindPoint(VkPipelineBindPoint,
                            VK_PIPELINE_BIND_POINT_GRAPHICS, VK_PIPELINE_BIND_POINT_COMPUTE))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineBindPoint VkPipelineBindPoint registry at www.khronos.org>
newtype VkPipelineBindPoint = VkPipelineBindPoint Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkPipelineBindPoint where
        showsPrec _ VK_PIPELINE_BIND_POINT_GRAPHICS
          = showString "VK_PIPELINE_BIND_POINT_GRAPHICS"
        showsPrec _ VK_PIPELINE_BIND_POINT_COMPUTE
          = showString "VK_PIPELINE_BIND_POINT_COMPUTE"
        showsPrec p (VkPipelineBindPoint x)
          = showParen (p >= 11)
              (showString "VkPipelineBindPoint " . showsPrec 11 x)

instance Read VkPipelineBindPoint where
        readPrec
          = parens
              (choose
                 [("VK_PIPELINE_BIND_POINT_GRAPHICS",
                   pure VK_PIPELINE_BIND_POINT_GRAPHICS),
                  ("VK_PIPELINE_BIND_POINT_COMPUTE",
                   pure VK_PIPELINE_BIND_POINT_COMPUTE)]
                 +++
                 prec 10
                   (expectP (Ident "VkPipelineBindPoint") >>
                      (VkPipelineBindPoint <$> step readPrec)))

pattern VK_PIPELINE_BIND_POINT_GRAPHICS :: VkPipelineBindPoint

pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0

pattern VK_PIPELINE_BIND_POINT_COMPUTE :: VkPipelineBindPoint

pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1
