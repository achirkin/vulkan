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
module Graphics.Vulkan.Types.Enum.VkFenceImportFlagsKHR
       (VkFenceImportBitmaskKHR(VkFenceImportBitmaskKHR,
                                VkFenceImportFlagsKHR, VkFenceImportFlagBitsKHR,
                                VK_FENCE_IMPORT_TEMPORARY_BIT_KHR),
        VkFenceImportFlagsKHR, VkFenceImportFlagBitsKHR)
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

newtype VkFenceImportBitmaskKHR (a ::
                                   FlagType) = VkFenceImportBitmaskKHR VkFlags
                                                 deriving (Eq, Ord, Storable, Data, Generic)

type VkFenceImportFlagsKHR = VkFenceImportBitmaskKHR FlagMask

type VkFenceImportFlagBitsKHR = VkFenceImportBitmaskKHR FlagBit

pattern VkFenceImportFlagBitsKHR ::
        VkFlags -> VkFenceImportBitmaskKHR FlagBit

pattern VkFenceImportFlagBitsKHR n = VkFenceImportBitmaskKHR n

pattern VkFenceImportFlagsKHR ::
        VkFlags -> VkFenceImportBitmaskKHR FlagMask

pattern VkFenceImportFlagsKHR n = VkFenceImportBitmaskKHR n

deriving instance Bits (VkFenceImportBitmaskKHR FlagMask)

deriving instance FiniteBits (VkFenceImportBitmaskKHR FlagMask)

deriving instance Integral (VkFenceImportBitmaskKHR FlagMask)

deriving instance Num (VkFenceImportBitmaskKHR FlagMask)

deriving instance Bounded (VkFenceImportBitmaskKHR FlagMask)

deriving instance Enum (VkFenceImportBitmaskKHR FlagMask)

deriving instance Real (VkFenceImportBitmaskKHR FlagMask)

instance Show (VkFenceImportBitmaskKHR a) where
        showsPrec _ VK_FENCE_IMPORT_TEMPORARY_BIT_KHR
          = showString "VK_FENCE_IMPORT_TEMPORARY_BIT_KHR"
        showsPrec p (VkFenceImportBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkFenceImportBitmaskKHR " . showsPrec 11 x)

instance Read (VkFenceImportBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_FENCE_IMPORT_TEMPORARY_BIT_KHR",
                   pure VK_FENCE_IMPORT_TEMPORARY_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkFenceImportBitmaskKHR") >>
                      (VkFenceImportBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR ::
        VkFenceImportBitmaskKHR a

pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR =
        VkFenceImportBitmaskKHR 1
