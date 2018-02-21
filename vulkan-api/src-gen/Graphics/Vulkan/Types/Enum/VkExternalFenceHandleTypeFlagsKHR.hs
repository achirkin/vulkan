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
module Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR
       (VkExternalFenceHandleTypeBitmaskKHR(VkExternalFenceHandleTypeBitmaskKHR,
                                            VkExternalFenceHandleTypeFlagsKHR,
                                            VkExternalFenceHandleTypeFlagBitsKHR,
                                            VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR,
                                            VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR,
                                            VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR,
                                            VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR),
        VkExternalFenceHandleTypeFlagsKHR,
        VkExternalFenceHandleTypeFlagBitsKHR)
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

newtype VkExternalFenceHandleTypeBitmaskKHR (a ::
                                               FlagType) = VkExternalFenceHandleTypeBitmaskKHR VkFlags
                                                             deriving (Eq, Ord, Storable, Data,
                                                                       Generic)

type VkExternalFenceHandleTypeFlagsKHR =
     VkExternalFenceHandleTypeBitmaskKHR FlagMask

type VkExternalFenceHandleTypeFlagBitsKHR =
     VkExternalFenceHandleTypeBitmaskKHR FlagBit

pattern VkExternalFenceHandleTypeFlagBitsKHR ::
        VkFlags -> VkExternalFenceHandleTypeBitmaskKHR FlagBit

pattern VkExternalFenceHandleTypeFlagBitsKHR n =
        VkExternalFenceHandleTypeBitmaskKHR n

pattern VkExternalFenceHandleTypeFlagsKHR ::
        VkFlags -> VkExternalFenceHandleTypeBitmaskKHR FlagMask

pattern VkExternalFenceHandleTypeFlagsKHR n =
        VkExternalFenceHandleTypeBitmaskKHR n

deriving instance
         Bits (VkExternalFenceHandleTypeBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkExternalFenceHandleTypeBitmaskKHR FlagMask)

deriving instance
         Integral (VkExternalFenceHandleTypeBitmaskKHR FlagMask)

deriving instance
         Num (VkExternalFenceHandleTypeBitmaskKHR FlagMask)

deriving instance
         Bounded (VkExternalFenceHandleTypeBitmaskKHR FlagMask)

deriving instance
         Enum (VkExternalFenceHandleTypeBitmaskKHR FlagMask)

deriving instance
         Real (VkExternalFenceHandleTypeBitmaskKHR FlagMask)

instance Show (VkExternalFenceHandleTypeBitmaskKHR a) where
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
          = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
          = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
          = showString
              "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
        showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR
          = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
        showsPrec p (VkExternalFenceHandleTypeBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkExternalFenceHandleTypeBitmaskKHR " .
                 showsPrec 11 x)

instance Read (VkExternalFenceHandleTypeBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR),
                  ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR),
                  ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR),
                  ("VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR",
                   pure VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalFenceHandleTypeBitmaskKHR") >>
                      (VkExternalFenceHandleTypeBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR ::
        VkExternalFenceHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VkExternalFenceHandleTypeBitmaskKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR ::
        VkExternalFenceHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VkExternalFenceHandleTypeBitmaskKHR 2

-- | bitpos = @2@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR ::
        VkExternalFenceHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR =
        VkExternalFenceHandleTypeBitmaskKHR 4

-- | bitpos = @3@
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR ::
        VkExternalFenceHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR =
        VkExternalFenceHandleTypeBitmaskKHR 8
