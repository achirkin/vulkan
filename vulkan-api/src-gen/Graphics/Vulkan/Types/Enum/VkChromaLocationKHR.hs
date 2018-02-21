{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkChromaLocationKHR
       (VkChromaLocationKHR(VkChromaLocationKHR,
                            VK_CHROMA_LOCATION_COSITED_EVEN_KHR,
                            VK_CHROMA_LOCATION_MIDPOINT_KHR))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkChromaLocationKHR.html VkChromaLocationKHR registry at www.khronos.org>
newtype VkChromaLocationKHR = VkChromaLocationKHR Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkChromaLocationKHR where
        showsPrec _ VK_CHROMA_LOCATION_COSITED_EVEN_KHR
          = showString "VK_CHROMA_LOCATION_COSITED_EVEN_KHR"
        showsPrec _ VK_CHROMA_LOCATION_MIDPOINT_KHR
          = showString "VK_CHROMA_LOCATION_MIDPOINT_KHR"
        showsPrec p (VkChromaLocationKHR x)
          = showParen (p >= 11)
              (showString "VkChromaLocationKHR " . showsPrec 11 x)

instance Read VkChromaLocationKHR where
        readPrec
          = parens
              (choose
                 [("VK_CHROMA_LOCATION_COSITED_EVEN_KHR",
                   pure VK_CHROMA_LOCATION_COSITED_EVEN_KHR),
                  ("VK_CHROMA_LOCATION_MIDPOINT_KHR",
                   pure VK_CHROMA_LOCATION_MIDPOINT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkChromaLocationKHR") >>
                      (VkChromaLocationKHR <$> step readPrec)))

pattern VK_CHROMA_LOCATION_COSITED_EVEN_KHR :: VkChromaLocationKHR

pattern VK_CHROMA_LOCATION_COSITED_EVEN_KHR = VkChromaLocationKHR 0

pattern VK_CHROMA_LOCATION_MIDPOINT_KHR :: VkChromaLocationKHR

pattern VK_CHROMA_LOCATION_MIDPOINT_KHR = VkChromaLocationKHR 1
