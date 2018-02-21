{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.VkQueryResultFlags
       (VkQueryResultBitmask(VkQueryResultBitmask, VkQueryResultFlags,
                             VkQueryResultFlagBits, VK_QUERY_RESULT_64_BIT,
                             VK_QUERY_RESULT_WAIT_BIT, VK_QUERY_RESULT_WITH_AVAILABILITY_BIT,
                             VK_QUERY_RESULT_PARTIAL_BIT),
        VkQueryResultFlags, VkQueryResultFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkQueryResultBitmask (a ::
                                FlagType) = VkQueryResultBitmask VkFlags
                                              deriving (Eq, Ord, Storable, Data, Generic)

type VkQueryResultFlags = VkQueryResultBitmask FlagMask

type VkQueryResultFlagBits = VkQueryResultBitmask FlagBit

pattern VkQueryResultFlagBits ::
        VkFlags -> VkQueryResultBitmask FlagBit

pattern VkQueryResultFlagBits n = VkQueryResultBitmask n

pattern VkQueryResultFlags ::
        VkFlags -> VkQueryResultBitmask FlagMask

pattern VkQueryResultFlags n = VkQueryResultBitmask n

deriving instance Bits (VkQueryResultBitmask FlagMask)

deriving instance FiniteBits (VkQueryResultBitmask FlagMask)

deriving instance Integral (VkQueryResultBitmask FlagMask)

deriving instance Num (VkQueryResultBitmask FlagMask)

deriving instance Bounded (VkQueryResultBitmask FlagMask)

deriving instance Enum (VkQueryResultBitmask FlagMask)

deriving instance Real (VkQueryResultBitmask FlagMask)

instance Show (VkQueryResultBitmask a) where
        showsPrec _ VK_QUERY_RESULT_64_BIT
          = showString "VK_QUERY_RESULT_64_BIT"
        showsPrec _ VK_QUERY_RESULT_WAIT_BIT
          = showString "VK_QUERY_RESULT_WAIT_BIT"
        showsPrec _ VK_QUERY_RESULT_WITH_AVAILABILITY_BIT
          = showString "VK_QUERY_RESULT_WITH_AVAILABILITY_BIT"
        showsPrec _ VK_QUERY_RESULT_PARTIAL_BIT
          = showString "VK_QUERY_RESULT_PARTIAL_BIT"
        showsPrec p (VkQueryResultBitmask x)
          = showParen (p >= 11)
              (showString "VkQueryResultBitmask " . showsPrec 11 x)

instance Read (VkQueryResultBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_RESULT_64_BIT", pure VK_QUERY_RESULT_64_BIT),
                  ("VK_QUERY_RESULT_WAIT_BIT", pure VK_QUERY_RESULT_WAIT_BIT),
                  ("VK_QUERY_RESULT_WITH_AVAILABILITY_BIT",
                   pure VK_QUERY_RESULT_WITH_AVAILABILITY_BIT),
                  ("VK_QUERY_RESULT_PARTIAL_BIT", pure VK_QUERY_RESULT_PARTIAL_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryResultBitmask") >>
                      (VkQueryResultBitmask <$> step readPrec)))

-- | Results of the queries are written to the destination buffer as 64-bit values
--
--   bitpos = @0@
pattern VK_QUERY_RESULT_64_BIT :: VkQueryResultBitmask a

pattern VK_QUERY_RESULT_64_BIT = VkQueryResultBitmask 1

-- | Results of the queries are waited on before proceeding with the result copy
--
--   bitpos = @1@
pattern VK_QUERY_RESULT_WAIT_BIT :: VkQueryResultBitmask a

pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultBitmask 2

-- | Besides the results of the query, the availability of the results is also written
--
--   bitpos = @2@
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT ::
        VkQueryResultBitmask a

pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT =
        VkQueryResultBitmask 4

-- | Copy the partial results of the query even if the final results are not available
--
--   bitpos = @3@
pattern VK_QUERY_RESULT_PARTIAL_BIT :: VkQueryResultBitmask a

pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultBitmask 8
