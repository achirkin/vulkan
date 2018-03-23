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
module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags
       (VkExternalMemoryHandleTypeBitmask(VkExternalMemoryHandleTypeBitmask,
                                          VkExternalMemoryHandleTypeFlags,
                                          VkExternalMemoryHandleTypeFlagBits,
                                          VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
                                          VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT,
                                          VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT,
                                          VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT,
                                          VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT,
                                          VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT,
                                          VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT),
        VkExternalMemoryHandleTypeFlags,
        VkExternalMemoryHandleTypeFlagBits)
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

newtype VkExternalMemoryHandleTypeBitmask (a ::
                                             FlagType) = VkExternalMemoryHandleTypeBitmask VkFlags
                                                           deriving (Eq, Ord, Storable, Data,
                                                                     Generic)

type VkExternalMemoryHandleTypeFlags =
     VkExternalMemoryHandleTypeBitmask FlagMask

type VkExternalMemoryHandleTypeFlagBits =
     VkExternalMemoryHandleTypeBitmask FlagBit

pattern VkExternalMemoryHandleTypeFlagBits ::
        VkFlags -> VkExternalMemoryHandleTypeBitmask FlagBit

pattern VkExternalMemoryHandleTypeFlagBits n =
        VkExternalMemoryHandleTypeBitmask n

pattern VkExternalMemoryHandleTypeFlags ::
        VkFlags -> VkExternalMemoryHandleTypeBitmask FlagMask

pattern VkExternalMemoryHandleTypeFlags n =
        VkExternalMemoryHandleTypeBitmask n

deriving instance Bits (VkExternalMemoryHandleTypeBitmask FlagMask)

deriving instance
         FiniteBits (VkExternalMemoryHandleTypeBitmask FlagMask)

deriving instance
         Integral (VkExternalMemoryHandleTypeBitmask FlagMask)

deriving instance Num (VkExternalMemoryHandleTypeBitmask FlagMask)

deriving instance
         Bounded (VkExternalMemoryHandleTypeBitmask FlagMask)

deriving instance Enum (VkExternalMemoryHandleTypeBitmask FlagMask)

deriving instance Real (VkExternalMemoryHandleTypeBitmask FlagMask)

instance Show (VkExternalMemoryHandleTypeBitmask a) where
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT"
        showsPrec p (VkExternalMemoryHandleTypeBitmask x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryHandleTypeBitmask " . showsPrec 11 x)

instance Read (VkExternalMemoryHandleTypeBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryHandleTypeBitmask") >>
                      (VkExternalMemoryHandleTypeBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT ::
        VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT =
        VkExternalMemoryHandleTypeBitmask 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT ::
        VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT =
        VkExternalMemoryHandleTypeBitmask 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT ::
        VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT =
        VkExternalMemoryHandleTypeBitmask 4

-- | bitpos = @3@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT ::
        VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT =
        VkExternalMemoryHandleTypeBitmask 8

-- | bitpos = @4@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT ::
        VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT =
        VkExternalMemoryHandleTypeBitmask 16

-- | bitpos = @5@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT ::
        VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT =
        VkExternalMemoryHandleTypeBitmask 32

-- | bitpos = @6@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT ::
        VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT =
        VkExternalMemoryHandleTypeBitmask 64
