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
module Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags
       (VkExternalFenceHandleTypeBitmask(VkExternalFenceHandleTypeBitmask,
                                         VkExternalFenceHandleTypeFlags,
                                         VkExternalFenceHandleTypeFlagBits,
                                         VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT,
                                         VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT,
                                         VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT,
                                         VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT),
        VkExternalFenceHandleTypeFlags, VkExternalFenceHandleTypeFlagBits)
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

newtype VkExternalFenceHandleTypeBitmask (a ::
                                            FlagType) = VkExternalFenceHandleTypeBitmask VkFlags
                                                          deriving (Eq, Ord, Storable, Data,
                                                                    Generic)

type VkExternalFenceHandleTypeFlags =
     VkExternalFenceHandleTypeBitmask FlagMask

type VkExternalFenceHandleTypeFlagBits =
     VkExternalFenceHandleTypeBitmask FlagBit

pattern VkExternalFenceHandleTypeFlagBits ::
        VkFlags -> VkExternalFenceHandleTypeBitmask FlagBit

pattern VkExternalFenceHandleTypeFlagBits n =
        VkExternalFenceHandleTypeBitmask n

pattern VkExternalFenceHandleTypeFlags ::
        VkFlags -> VkExternalFenceHandleTypeBitmask FlagMask

pattern VkExternalFenceHandleTypeFlags n =
        VkExternalFenceHandleTypeBitmask n

deriving instance Bits (VkExternalFenceHandleTypeBitmask FlagMask)

deriving instance
         FiniteBits (VkExternalFenceHandleTypeBitmask FlagMask)

deriving instance
         Integral (VkExternalFenceHandleTypeBitmask FlagMask)

deriving instance Num (VkExternalFenceHandleTypeBitmask FlagMask)

deriving instance
         Bounded (VkExternalFenceHandleTypeBitmask FlagMask)

deriving instance Enum (VkExternalFenceHandleTypeBitmask FlagMask)

deriving instance Real (VkExternalFenceHandleTypeBitmask FlagMask)

instance Show (VkExternalFenceHandleTypeBitmask a) where
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
          = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT"
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
          = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
          = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
          = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT"
        showsPrec p (VkExternalFenceHandleTypeBitmask x)
          = showParen (p >= 11)
              (showString "VkExternalFenceHandleTypeBitmask " . showsPrec 11 x)

instance Read (VkExternalFenceHandleTypeBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT),
                  ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT),
                  ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT),
                  ("VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalFenceHandleTypeBitmask") >>
                      (VkExternalFenceHandleTypeBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT ::
        VkExternalFenceHandleTypeBitmask a

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT =
        VkExternalFenceHandleTypeBitmask 1

-- | bitpos = @1@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT ::
        VkExternalFenceHandleTypeBitmask a

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT =
        VkExternalFenceHandleTypeBitmask 2

-- | bitpos = @2@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT ::
        VkExternalFenceHandleTypeBitmask a

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT =
        VkExternalFenceHandleTypeBitmask 4

-- | bitpos = @3@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT ::
        VkExternalFenceHandleTypeBitmask a

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT =
        VkExternalFenceHandleTypeBitmask 8
