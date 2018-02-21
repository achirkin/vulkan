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
module Graphics.Vulkan.Types.Enum.VkSemaphoreImportFlagsKHR
       (VkSemaphoreImportBitmaskKHR(VkSemaphoreImportBitmaskKHR,
                                    VkSemaphoreImportFlagsKHR, VkSemaphoreImportFlagBitsKHR,
                                    VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR),
        VkSemaphoreImportFlagsKHR, VkSemaphoreImportFlagBitsKHR)
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

newtype VkSemaphoreImportBitmaskKHR (a ::
                                       FlagType) = VkSemaphoreImportBitmaskKHR VkFlags
                                                     deriving (Eq, Ord, Storable, Data, Generic)

type VkSemaphoreImportFlagsKHR =
     VkSemaphoreImportBitmaskKHR FlagMask

type VkSemaphoreImportFlagBitsKHR =
     VkSemaphoreImportBitmaskKHR FlagBit

pattern VkSemaphoreImportFlagBitsKHR ::
        VkFlags -> VkSemaphoreImportBitmaskKHR FlagBit

pattern VkSemaphoreImportFlagBitsKHR n =
        VkSemaphoreImportBitmaskKHR n

pattern VkSemaphoreImportFlagsKHR ::
        VkFlags -> VkSemaphoreImportBitmaskKHR FlagMask

pattern VkSemaphoreImportFlagsKHR n = VkSemaphoreImportBitmaskKHR n

deriving instance Bits (VkSemaphoreImportBitmaskKHR FlagMask)

deriving instance FiniteBits (VkSemaphoreImportBitmaskKHR FlagMask)

deriving instance Integral (VkSemaphoreImportBitmaskKHR FlagMask)

deriving instance Num (VkSemaphoreImportBitmaskKHR FlagMask)

deriving instance Bounded (VkSemaphoreImportBitmaskKHR FlagMask)

deriving instance Enum (VkSemaphoreImportBitmaskKHR FlagMask)

deriving instance Real (VkSemaphoreImportBitmaskKHR FlagMask)

instance Show (VkSemaphoreImportBitmaskKHR a) where
        showsPrec _ VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR
          = showString "VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR"
        showsPrec p (VkSemaphoreImportBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkSemaphoreImportBitmaskKHR " . showsPrec 11 x)

instance Read (VkSemaphoreImportBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR",
                   pure VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkSemaphoreImportBitmaskKHR") >>
                      (VkSemaphoreImportBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR ::
        VkSemaphoreImportBitmaskKHR a

pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR =
        VkSemaphoreImportBitmaskKHR 1
