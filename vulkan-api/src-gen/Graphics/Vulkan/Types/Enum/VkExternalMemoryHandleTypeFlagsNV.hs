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
module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV
       (VkExternalMemoryHandleTypeBitmaskNV(VkExternalMemoryHandleTypeBitmaskNV,
                                            VkExternalMemoryHandleTypeFlagsNV,
                                            VkExternalMemoryHandleTypeFlagBitsNV,
                                            VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV,
                                            VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV,
                                            VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV,
                                            VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV),
        VkExternalMemoryHandleTypeFlagsNV,
        VkExternalMemoryHandleTypeFlagBitsNV)
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

newtype VkExternalMemoryHandleTypeBitmaskNV (a ::
                                               FlagType) = VkExternalMemoryHandleTypeBitmaskNV VkFlags
                                                             deriving (Eq, Ord, Storable, Data,
                                                                       Generic)

type VkExternalMemoryHandleTypeFlagsNV =
     VkExternalMemoryHandleTypeBitmaskNV FlagMask

type VkExternalMemoryHandleTypeFlagBitsNV =
     VkExternalMemoryHandleTypeBitmaskNV FlagBit

pattern VkExternalMemoryHandleTypeFlagBitsNV ::
        VkFlags -> VkExternalMemoryHandleTypeBitmaskNV FlagBit

pattern VkExternalMemoryHandleTypeFlagBitsNV n =
        VkExternalMemoryHandleTypeBitmaskNV n

pattern VkExternalMemoryHandleTypeFlagsNV ::
        VkFlags -> VkExternalMemoryHandleTypeBitmaskNV FlagMask

pattern VkExternalMemoryHandleTypeFlagsNV n =
        VkExternalMemoryHandleTypeBitmaskNV n

deriving instance
         Bits (VkExternalMemoryHandleTypeBitmaskNV FlagMask)

deriving instance
         FiniteBits (VkExternalMemoryHandleTypeBitmaskNV FlagMask)

deriving instance
         Integral (VkExternalMemoryHandleTypeBitmaskNV FlagMask)

deriving instance
         Num (VkExternalMemoryHandleTypeBitmaskNV FlagMask)

deriving instance
         Bounded (VkExternalMemoryHandleTypeBitmaskNV FlagMask)

deriving instance
         Enum (VkExternalMemoryHandleTypeBitmaskNV FlagMask)

deriving instance
         Real (VkExternalMemoryHandleTypeBitmaskNV FlagMask)

instance Show (VkExternalMemoryHandleTypeBitmaskNV a) where
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
          = showString
              "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
          = showString
              "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV"
        showsPrec p (VkExternalMemoryHandleTypeBitmaskNV x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryHandleTypeBitmaskNV " .
                 showsPrec 11 x)

instance Read (VkExternalMemoryHandleTypeBitmaskNV a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryHandleTypeBitmaskNV") >>
                      (VkExternalMemoryHandleTypeBitmaskNV <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV ::
        VkExternalMemoryHandleTypeBitmaskNV a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV =
        VkExternalMemoryHandleTypeBitmaskNV 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV ::
        VkExternalMemoryHandleTypeBitmaskNV a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV =
        VkExternalMemoryHandleTypeBitmaskNV 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV ::
        VkExternalMemoryHandleTypeBitmaskNV a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV =
        VkExternalMemoryHandleTypeBitmaskNV 4

-- | bitpos = @3@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV ::
        VkExternalMemoryHandleTypeBitmaskNV a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV =
        VkExternalMemoryHandleTypeBitmaskNV 8
