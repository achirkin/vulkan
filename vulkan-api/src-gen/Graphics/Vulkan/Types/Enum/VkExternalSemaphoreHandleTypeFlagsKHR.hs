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
module Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlagsKHR
       (VkExternalSemaphoreHandleTypeBitmaskKHR(VkExternalSemaphoreHandleTypeBitmaskKHR,
                                                VkExternalSemaphoreHandleTypeFlagsKHR,
                                                VkExternalSemaphoreHandleTypeFlagBitsKHR,
                                                VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR,
                                                VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR,
                                                VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR,
                                                VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR,
                                                VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR),
        VkExternalSemaphoreHandleTypeFlagsKHR,
        VkExternalSemaphoreHandleTypeFlagBitsKHR)
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

newtype VkExternalSemaphoreHandleTypeBitmaskKHR (a ::
                                                   FlagType) = VkExternalSemaphoreHandleTypeBitmaskKHR VkFlags
                                                                 deriving (Eq, Ord, Storable, Data,
                                                                           Generic)

type VkExternalSemaphoreHandleTypeFlagsKHR =
     VkExternalSemaphoreHandleTypeBitmaskKHR FlagMask

type VkExternalSemaphoreHandleTypeFlagBitsKHR =
     VkExternalSemaphoreHandleTypeBitmaskKHR FlagBit

pattern VkExternalSemaphoreHandleTypeFlagBitsKHR ::
        VkFlags -> VkExternalSemaphoreHandleTypeBitmaskKHR FlagBit

pattern VkExternalSemaphoreHandleTypeFlagBitsKHR n =
        VkExternalSemaphoreHandleTypeBitmaskKHR n

pattern VkExternalSemaphoreHandleTypeFlagsKHR ::
        VkFlags -> VkExternalSemaphoreHandleTypeBitmaskKHR FlagMask

pattern VkExternalSemaphoreHandleTypeFlagsKHR n =
        VkExternalSemaphoreHandleTypeBitmaskKHR n

deriving instance
         Bits (VkExternalSemaphoreHandleTypeBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkExternalSemaphoreHandleTypeBitmaskKHR FlagMask)

deriving instance
         Integral (VkExternalSemaphoreHandleTypeBitmaskKHR FlagMask)

deriving instance
         Num (VkExternalSemaphoreHandleTypeBitmaskKHR FlagMask)

deriving instance
         Bounded (VkExternalSemaphoreHandleTypeBitmaskKHR FlagMask)

deriving instance
         Enum (VkExternalSemaphoreHandleTypeBitmaskKHR FlagMask)

deriving instance
         Real (VkExternalSemaphoreHandleTypeBitmaskKHR FlagMask)

instance Show (VkExternalSemaphoreHandleTypeBitmaskKHR a) where
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
          = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
          = showString
              "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
        showsPrec _
          VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
          = showString
              "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR
          = showString
              "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR
          = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
        showsPrec p (VkExternalSemaphoreHandleTypeBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkExternalSemaphoreHandleTypeBitmaskKHR " .
                 showsPrec 11 x)

instance Read (VkExternalSemaphoreHandleTypeBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalSemaphoreHandleTypeBitmaskKHR") >>
                      (VkExternalSemaphoreHandleTypeBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR ::
        VkExternalSemaphoreHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VkExternalSemaphoreHandleTypeBitmaskKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR ::
        VkExternalSemaphoreHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VkExternalSemaphoreHandleTypeBitmaskKHR 2

-- | bitpos = @2@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
        :: VkExternalSemaphoreHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
        = VkExternalSemaphoreHandleTypeBitmaskKHR 4

-- | bitpos = @3@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR ::
        VkExternalSemaphoreHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR =
        VkExternalSemaphoreHandleTypeBitmaskKHR 8

-- | bitpos = @4@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR ::
        VkExternalSemaphoreHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR =
        VkExternalSemaphoreHandleTypeBitmaskKHR 16
