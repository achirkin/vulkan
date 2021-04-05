{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Semaphore
       (VkSemaphoreImportFlagBitsKHR(..),
        VkSemaphoreImportBitmask(VkSemaphoreImportBitmask,
                                 VkSemaphoreImportFlags, VkSemaphoreImportFlagBits,
                                 VK_SEMAPHORE_IMPORT_TEMPORARY_BIT),
        VkSemaphoreImportFlags, VkSemaphoreImportFlagBits,
        VkSemaphoreType(VkSemaphoreType, VK_SEMAPHORE_TYPE_BINARY,
                        VK_SEMAPHORE_TYPE_TIMELINE),
        VkSemaphoreTypeKHR(..), VkSemaphoreWaitFlagBitsKHR(..),
        VkSemaphoreWaitBitmask(VkSemaphoreWaitBitmask,
                               VkSemaphoreWaitFlags, VkSemaphoreWaitFlagBits,
                               VK_SEMAPHORE_WAIT_ANY_BIT),
        VkSemaphoreWaitFlags, VkSemaphoreWaitFlagBits)
       where
import Data.Bits                       (Bits, FiniteBits)
import Data.Coerce                     (coerce)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType, Int32)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSemaphoreType VkSemaphoreType registry at www.khronos.org>
newtype VkSemaphoreType = VkSemaphoreType Int32
                          deriving (Eq, Ord, Enum, Storable)

instance Show VkSemaphoreType where
    showsPrec _ VK_SEMAPHORE_TYPE_BINARY
      = showString "VK_SEMAPHORE_TYPE_BINARY"
    showsPrec _ VK_SEMAPHORE_TYPE_TIMELINE
      = showString "VK_SEMAPHORE_TYPE_TIMELINE"
    showsPrec p (VkSemaphoreType x)
      = showParen (p >= 11)
          (showString "VkSemaphoreType " . showsPrec 11 x)

instance Read VkSemaphoreType where
    readPrec
      = parens
          (choose
             [("VK_SEMAPHORE_TYPE_BINARY", pure VK_SEMAPHORE_TYPE_BINARY),
              ("VK_SEMAPHORE_TYPE_TIMELINE", pure VK_SEMAPHORE_TYPE_TIMELINE)]
             +++
             prec 10
               (expectP (Ident "VkSemaphoreType") >>
                  (VkSemaphoreType <$> step readPrec)))

pattern VK_SEMAPHORE_TYPE_BINARY :: VkSemaphoreType

pattern VK_SEMAPHORE_TYPE_BINARY = VkSemaphoreType 0

pattern VK_SEMAPHORE_TYPE_TIMELINE :: VkSemaphoreType

pattern VK_SEMAPHORE_TYPE_TIMELINE = VkSemaphoreType 1

newtype VkSemaphoreTypeKHR = VkSemaphoreTypeKHR VkFlags
                             deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkSemaphoreTypeKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkSemaphoreTypeKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSemaphoreWaitFlagBitsKHR = VkSemaphoreWaitFlagBitsKHR VkFlags
                                     deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkSemaphoreWaitFlagBitsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkSemaphoreWaitFlagBitsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkSemaphoreWaitBitmask (a ::
                                  FlagType) = VkSemaphoreWaitBitmask VkFlags
                                              deriving (Eq, Ord, Storable)

type VkSemaphoreWaitFlags = VkSemaphoreWaitBitmask FlagMask

type VkSemaphoreWaitFlagBits = VkSemaphoreWaitBitmask FlagBit

pattern VkSemaphoreWaitFlagBits ::
        VkFlags -> VkSemaphoreWaitBitmask FlagBit

pattern VkSemaphoreWaitFlagBits n = VkSemaphoreWaitBitmask n

pattern VkSemaphoreWaitFlags ::
        VkFlags -> VkSemaphoreWaitBitmask FlagMask

pattern VkSemaphoreWaitFlags n = VkSemaphoreWaitBitmask n

deriving instance Bits (VkSemaphoreWaitBitmask FlagMask)

deriving instance FiniteBits (VkSemaphoreWaitBitmask FlagMask)

instance Show (VkSemaphoreWaitBitmask a) where
    showsPrec _ VK_SEMAPHORE_WAIT_ANY_BIT
      = showString "VK_SEMAPHORE_WAIT_ANY_BIT"
    showsPrec p (VkSemaphoreWaitBitmask x)
      = showParen (p >= 11)
          (showString "VkSemaphoreWaitBitmask " . showsPrec 11 x)

instance Read (VkSemaphoreWaitBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_SEMAPHORE_WAIT_ANY_BIT", pure VK_SEMAPHORE_WAIT_ANY_BIT)]
             +++
             prec 10
               (expectP (Ident "VkSemaphoreWaitBitmask") >>
                  (VkSemaphoreWaitBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_SEMAPHORE_WAIT_ANY_BIT :: VkSemaphoreWaitBitmask a

pattern VK_SEMAPHORE_WAIT_ANY_BIT = VkSemaphoreWaitBitmask 1
