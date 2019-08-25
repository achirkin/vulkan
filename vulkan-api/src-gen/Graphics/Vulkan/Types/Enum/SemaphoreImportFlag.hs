{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.SemaphoreImportFlag
       (VkSemaphoreImportFlagBitsKHR(..),
        VkSemaphoreImportBitmask(VkSemaphoreImportBitmask,
                                 VkSemaphoreImportFlags, VkSemaphoreImportFlagBits,
                                 VK_SEMAPHORE_IMPORT_TEMPORARY_BIT),
        VkSemaphoreImportFlags, VkSemaphoreImportFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkSemaphoreImportFlagBitsKHR = VkSemaphoreImportFlagBitsKHR VkFlags
                                       deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkSemaphoreImportFlagBitsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkSemaphoreImportFlagBitsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSemaphoreImportBitmask (a ::
                                    FlagType) = VkSemaphoreImportBitmask VkFlags
                                                deriving (Eq, Ord, Storable)

type VkSemaphoreImportFlags = VkSemaphoreImportBitmask FlagMask

type VkSemaphoreImportFlagBits = VkSemaphoreImportBitmask FlagBit

pattern VkSemaphoreImportFlagBits ::
        VkFlags -> VkSemaphoreImportBitmask FlagBit

pattern VkSemaphoreImportFlagBits n = VkSemaphoreImportBitmask n

pattern VkSemaphoreImportFlags ::
        VkFlags -> VkSemaphoreImportBitmask FlagMask

pattern VkSemaphoreImportFlags n = VkSemaphoreImportBitmask n

deriving instance Bits (VkSemaphoreImportBitmask FlagMask)

deriving instance FiniteBits (VkSemaphoreImportBitmask FlagMask)

instance Show (VkSemaphoreImportBitmask a) where
    showsPrec _ VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
      = showString "VK_SEMAPHORE_IMPORT_TEMPORARY_BIT"
    showsPrec p (VkSemaphoreImportBitmask x)
      = showParen (p >= 11)
          (showString "VkSemaphoreImportBitmask " . showsPrec 11 x)

instance Read (VkSemaphoreImportBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_SEMAPHORE_IMPORT_TEMPORARY_BIT",
               pure VK_SEMAPHORE_IMPORT_TEMPORARY_BIT)]
             +++
             prec 10
               (expectP (Ident "VkSemaphoreImportBitmask") >>
                  (VkSemaphoreImportBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT ::
        VkSemaphoreImportBitmask a

pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT =
        VkSemaphoreImportBitmask 1
