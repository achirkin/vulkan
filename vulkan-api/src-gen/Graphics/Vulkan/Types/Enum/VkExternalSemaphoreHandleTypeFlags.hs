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
module Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlags
       (VkExternalSemaphoreHandleTypeBitmask(VkExternalSemaphoreHandleTypeBitmask,
                                             VkExternalSemaphoreHandleTypeFlags,
                                             VkExternalSemaphoreHandleTypeFlagBits,
                                             VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT,
                                             VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT,
                                             VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT,
                                             VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT,
                                             VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT),
        VkExternalSemaphoreHandleTypeFlags,
        VkExternalSemaphoreHandleTypeFlagBits)
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

newtype VkExternalSemaphoreHandleTypeBitmask (a ::
                                                FlagType) = VkExternalSemaphoreHandleTypeBitmask VkFlags
                                                              deriving (Eq, Ord, Storable, Data,
                                                                        Generic)

type VkExternalSemaphoreHandleTypeFlags =
     VkExternalSemaphoreHandleTypeBitmask FlagMask

type VkExternalSemaphoreHandleTypeFlagBits =
     VkExternalSemaphoreHandleTypeBitmask FlagBit

pattern VkExternalSemaphoreHandleTypeFlagBits ::
        VkFlags -> VkExternalSemaphoreHandleTypeBitmask FlagBit

pattern VkExternalSemaphoreHandleTypeFlagBits n =
        VkExternalSemaphoreHandleTypeBitmask n

pattern VkExternalSemaphoreHandleTypeFlags ::
        VkFlags -> VkExternalSemaphoreHandleTypeBitmask FlagMask

pattern VkExternalSemaphoreHandleTypeFlags n =
        VkExternalSemaphoreHandleTypeBitmask n

deriving instance
         Bits (VkExternalSemaphoreHandleTypeBitmask FlagMask)

deriving instance
         FiniteBits (VkExternalSemaphoreHandleTypeBitmask FlagMask)

deriving instance
         Integral (VkExternalSemaphoreHandleTypeBitmask FlagMask)

deriving instance
         Num (VkExternalSemaphoreHandleTypeBitmask FlagMask)

deriving instance
         Bounded (VkExternalSemaphoreHandleTypeBitmask FlagMask)

deriving instance
         Enum (VkExternalSemaphoreHandleTypeBitmask FlagMask)

deriving instance
         Real (VkExternalSemaphoreHandleTypeBitmask FlagMask)

instance Show (VkExternalSemaphoreHandleTypeBitmask a) where
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
          = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
          = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
          = showString
              "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
          = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
          = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT"
        showsPrec p (VkExternalSemaphoreHandleTypeBitmask x)
          = showParen (p >= 11)
              (showString "VkExternalSemaphoreHandleTypeBitmask " .
                 showsPrec 11 x)

instance Read (VkExternalSemaphoreHandleTypeBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT),
                  ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT",
                   pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalSemaphoreHandleTypeBitmask") >>
                      (VkExternalSemaphoreHandleTypeBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT ::
        VkExternalSemaphoreHandleTypeBitmask a

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT =
        VkExternalSemaphoreHandleTypeBitmask 1

-- | bitpos = @1@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT ::
        VkExternalSemaphoreHandleTypeBitmask a

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT =
        VkExternalSemaphoreHandleTypeBitmask 2

-- | bitpos = @2@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT ::
        VkExternalSemaphoreHandleTypeBitmask a

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT =
        VkExternalSemaphoreHandleTypeBitmask 4

-- | bitpos = @3@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT ::
        VkExternalSemaphoreHandleTypeBitmask a

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT =
        VkExternalSemaphoreHandleTypeBitmask 8

-- | bitpos = @4@
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT ::
        VkExternalSemaphoreHandleTypeBitmask a

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT =
        VkExternalSemaphoreHandleTypeBitmask 16
