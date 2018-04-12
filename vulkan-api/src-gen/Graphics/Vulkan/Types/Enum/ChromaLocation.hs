{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.ChromaLocation
       (VkChromaLocation(VkChromaLocation,
                         VK_CHROMA_LOCATION_COSITED_EVEN, VK_CHROMA_LOCATION_MIDPOINT),
        VkChromaLocationKHR(..))
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkChromaLocation VkChromaLocation registry at www.khronos.org>
newtype VkChromaLocation = VkChromaLocation Int32
                             deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkChromaLocation where
        showsPrec _ VK_CHROMA_LOCATION_COSITED_EVEN
          = showString "VK_CHROMA_LOCATION_COSITED_EVEN"
        showsPrec _ VK_CHROMA_LOCATION_MIDPOINT
          = showString "VK_CHROMA_LOCATION_MIDPOINT"
        showsPrec p (VkChromaLocation x)
          = showParen (p >= 11)
              (showString "VkChromaLocation " . showsPrec 11 x)

instance Read VkChromaLocation where
        readPrec
          = parens
              (choose
                 [("VK_CHROMA_LOCATION_COSITED_EVEN",
                   pure VK_CHROMA_LOCATION_COSITED_EVEN),
                  ("VK_CHROMA_LOCATION_MIDPOINT", pure VK_CHROMA_LOCATION_MIDPOINT)]
                 +++
                 prec 10
                   (expectP (Ident "VkChromaLocation") >>
                      (VkChromaLocation <$> step readPrec)))

pattern VK_CHROMA_LOCATION_COSITED_EVEN :: VkChromaLocation

pattern VK_CHROMA_LOCATION_COSITED_EVEN = VkChromaLocation 0

pattern VK_CHROMA_LOCATION_MIDPOINT :: VkChromaLocation

pattern VK_CHROMA_LOCATION_MIDPOINT = VkChromaLocation 1

newtype VkChromaLocationKHR = VkChromaLocationKHR VkFlags
                                deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                          Storable, Real, Data, Generic)

instance Show VkChromaLocationKHR where
        {-# INLINE show #-}
        show (VkChromaLocationKHR x) = show x

instance Read VkChromaLocationKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
