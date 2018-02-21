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
module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR
       (VkExternalMemoryHandleTypeBitmaskKHR(VkExternalMemoryHandleTypeBitmaskKHR,
                                             VkExternalMemoryHandleTypeFlagsKHR,
                                             VkExternalMemoryHandleTypeFlagBitsKHR,
                                             VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR,
                                             VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR,
                                             VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR,
                                             VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR,
                                             VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR,
                                             VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR,
                                             VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR),
        VkExternalMemoryHandleTypeFlagsKHR,
        VkExternalMemoryHandleTypeFlagBitsKHR)
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

newtype VkExternalMemoryHandleTypeBitmaskKHR (a ::
                                                FlagType) = VkExternalMemoryHandleTypeBitmaskKHR VkFlags
                                                              deriving (Eq, Ord, Storable, Data,
                                                                        Generic)

type VkExternalMemoryHandleTypeFlagsKHR =
     VkExternalMemoryHandleTypeBitmaskKHR FlagMask

type VkExternalMemoryHandleTypeFlagBitsKHR =
     VkExternalMemoryHandleTypeBitmaskKHR FlagBit

pattern VkExternalMemoryHandleTypeFlagBitsKHR ::
        VkFlags -> VkExternalMemoryHandleTypeBitmaskKHR FlagBit

pattern VkExternalMemoryHandleTypeFlagBitsKHR n =
        VkExternalMemoryHandleTypeBitmaskKHR n

pattern VkExternalMemoryHandleTypeFlagsKHR ::
        VkFlags -> VkExternalMemoryHandleTypeBitmaskKHR FlagMask

pattern VkExternalMemoryHandleTypeFlagsKHR n =
        VkExternalMemoryHandleTypeBitmaskKHR n

deriving instance
         Bits (VkExternalMemoryHandleTypeBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkExternalMemoryHandleTypeBitmaskKHR FlagMask)

deriving instance
         Integral (VkExternalMemoryHandleTypeBitmaskKHR FlagMask)

deriving instance
         Num (VkExternalMemoryHandleTypeBitmaskKHR FlagMask)

deriving instance
         Bounded (VkExternalMemoryHandleTypeBitmaskKHR FlagMask)

deriving instance
         Enum (VkExternalMemoryHandleTypeBitmaskKHR FlagMask)

deriving instance
         Real (VkExternalMemoryHandleTypeBitmaskKHR FlagMask)

instance Show (VkExternalMemoryHandleTypeBitmaskKHR a) where
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
          = showString
              "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR"
        showsPrec _
          VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR
          = showString
              "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR
          = showString
              "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR"
        showsPrec p (VkExternalMemoryHandleTypeBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryHandleTypeBitmaskKHR " .
                 showsPrec 11 x)

instance Read (VkExternalMemoryHandleTypeBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryHandleTypeBitmaskKHR") >>
                      (VkExternalMemoryHandleTypeBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR ::
        VkExternalMemoryHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VkExternalMemoryHandleTypeBitmaskKHR 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR ::
        VkExternalMemoryHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VkExternalMemoryHandleTypeBitmaskKHR 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR ::
        VkExternalMemoryHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR =
        VkExternalMemoryHandleTypeBitmaskKHR 4

-- | bitpos = @3@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR ::
        VkExternalMemoryHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR =
        VkExternalMemoryHandleTypeBitmaskKHR 8

-- | bitpos = @4@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR ::
        VkExternalMemoryHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR =
        VkExternalMemoryHandleTypeBitmaskKHR 16

-- | bitpos = @5@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR ::
        VkExternalMemoryHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR =
        VkExternalMemoryHandleTypeBitmaskKHR 32

-- | bitpos = @6@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR ::
        VkExternalMemoryHandleTypeBitmaskKHR a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR =
        VkExternalMemoryHandleTypeBitmaskKHR 64
