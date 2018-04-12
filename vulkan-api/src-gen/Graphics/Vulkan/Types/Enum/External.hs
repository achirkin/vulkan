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
module Graphics.Vulkan.Types.Enum.External
       (VkExternalFenceFeatureFlagBitsKHR(..),
        VkExternalFenceFeatureBitmask(VkExternalFenceFeatureBitmask,
                                      VkExternalFenceFeatureFlags, VkExternalFenceFeatureFlagBits,
                                      VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT,
                                      VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT),
        VkExternalFenceFeatureFlags, VkExternalFenceFeatureFlagBits,
        VkExternalFenceHandleTypeFlagBitsKHR(..),
        VkExternalFenceHandleTypeBitmask(VkExternalFenceHandleTypeBitmask,
                                         VkExternalFenceHandleTypeFlags,
                                         VkExternalFenceHandleTypeFlagBits,
                                         VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT,
                                         VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT,
                                         VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT,
                                         VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT),
        VkExternalFenceHandleTypeFlags, VkExternalFenceHandleTypeFlagBits,
        VkExternalMemoryFeatureFlagBitsKHR(..),
        VkExternalMemoryFeatureBitmask(VkExternalMemoryFeatureBitmask,
                                       VkExternalMemoryFeatureFlags,
                                       VkExternalMemoryFeatureFlagBits,
                                       VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT,
                                       VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT,
                                       VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT),
        VkExternalMemoryFeatureFlags, VkExternalMemoryFeatureFlagBits,
        VkExternalMemoryFeatureBitmaskNV(VkExternalMemoryFeatureBitmaskNV,
                                         VkExternalMemoryFeatureFlagsNV,
                                         VkExternalMemoryFeatureFlagBitsNV,
                                         VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV,
                                         VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV,
                                         VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV),
        VkExternalMemoryFeatureFlagsNV, VkExternalMemoryFeatureFlagBitsNV,
        VkExternalMemoryHandleTypeFlagBitsKHR(..),
        VkExternalMemoryHandleTypeBitmask(VkExternalMemoryHandleTypeBitmask,
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
        VkExternalMemoryHandleTypeFlagBits,
        VkExternalMemoryHandleTypeBitmaskNV(VkExternalMemoryHandleTypeBitmaskNV,
                                            VkExternalMemoryHandleTypeFlagsNV,
                                            VkExternalMemoryHandleTypeFlagBitsNV,
                                            VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV,
                                            VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV,
                                            VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV,
                                            VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV),
        VkExternalMemoryHandleTypeFlagsNV,
        VkExternalMemoryHandleTypeFlagBitsNV,
        VkExternalSemaphoreFeatureFlagBitsKHR(..),
        VkExternalSemaphoreFeatureBitmask(VkExternalSemaphoreFeatureBitmask,
                                          VkExternalSemaphoreFeatureFlags,
                                          VkExternalSemaphoreFeatureFlagBits,
                                          VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT,
                                          VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT),
        VkExternalSemaphoreFeatureFlags,
        VkExternalSemaphoreFeatureFlagBits,
        VkExternalSemaphoreHandleTypeFlagBitsKHR(..),
        VkExternalSemaphoreHandleTypeBitmask(VkExternalSemaphoreHandleTypeBitmask,
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
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkExternalFenceFeatureFlagBitsKHR = VkExternalFenceFeatureFlagBitsKHR VkFlags
                                              deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                        FiniteBits, Storable, Real, Data, Generic)

instance Show VkExternalFenceFeatureFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalFenceFeatureFlagBitsKHR x) = show x

instance Read VkExternalFenceFeatureFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalFenceFeatureBitmask (a ::
                                         FlagType) = VkExternalFenceFeatureBitmask VkFlags
                                                       deriving (Eq, Ord, Storable, Data, Generic)

type VkExternalFenceFeatureFlags =
     VkExternalFenceFeatureBitmask FlagMask

type VkExternalFenceFeatureFlagBits =
     VkExternalFenceFeatureBitmask FlagBit

pattern VkExternalFenceFeatureFlagBits ::
        VkFlags -> VkExternalFenceFeatureBitmask FlagBit

pattern VkExternalFenceFeatureFlagBits n =
        VkExternalFenceFeatureBitmask n

pattern VkExternalFenceFeatureFlags ::
        VkFlags -> VkExternalFenceFeatureBitmask FlagMask

pattern VkExternalFenceFeatureFlags n =
        VkExternalFenceFeatureBitmask n

deriving instance Bits (VkExternalFenceFeatureBitmask FlagMask)

deriving instance
         FiniteBits (VkExternalFenceFeatureBitmask FlagMask)

deriving instance Integral (VkExternalFenceFeatureBitmask FlagMask)

deriving instance Num (VkExternalFenceFeatureBitmask FlagMask)

deriving instance Bounded (VkExternalFenceFeatureBitmask FlagMask)

deriving instance Enum (VkExternalFenceFeatureBitmask FlagMask)

deriving instance Real (VkExternalFenceFeatureBitmask FlagMask)

instance Show (VkExternalFenceFeatureBitmask a) where
        showsPrec _ VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
          = showString "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT"
        showsPrec _ VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
          = showString "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT"
        showsPrec p (VkExternalFenceFeatureBitmask x)
          = showParen (p >= 11)
              (showString "VkExternalFenceFeatureBitmask " . showsPrec 11 x)

instance Read (VkExternalFenceFeatureBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT",
                   pure VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT),
                  ("VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT",
                   pure VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalFenceFeatureBitmask") >>
                      (VkExternalFenceFeatureBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT ::
        VkExternalFenceFeatureBitmask a

pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT =
        VkExternalFenceFeatureBitmask 1

-- | bitpos = @1@
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT ::
        VkExternalFenceFeatureBitmask a

pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT =
        VkExternalFenceFeatureBitmask 2

newtype VkExternalFenceHandleTypeFlagBitsKHR = VkExternalFenceHandleTypeFlagBitsKHR VkFlags
                                                 deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                           Bits, FiniteBits, Storable, Real, Data,
                                                           Generic)

instance Show VkExternalFenceHandleTypeFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalFenceHandleTypeFlagBitsKHR x) = show x

instance Read VkExternalFenceHandleTypeFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

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

newtype VkExternalMemoryFeatureFlagBitsKHR = VkExternalMemoryFeatureFlagBitsKHR VkFlags
                                               deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                         Bits, FiniteBits, Storable, Real, Data,
                                                         Generic)

instance Show VkExternalMemoryFeatureFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalMemoryFeatureFlagBitsKHR x) = show x

instance Read VkExternalMemoryFeatureFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalMemoryFeatureBitmask (a ::
                                          FlagType) = VkExternalMemoryFeatureBitmask VkFlags
                                                        deriving (Eq, Ord, Storable, Data, Generic)

type VkExternalMemoryFeatureFlags =
     VkExternalMemoryFeatureBitmask FlagMask

type VkExternalMemoryFeatureFlagBits =
     VkExternalMemoryFeatureBitmask FlagBit

pattern VkExternalMemoryFeatureFlagBits ::
        VkFlags -> VkExternalMemoryFeatureBitmask FlagBit

pattern VkExternalMemoryFeatureFlagBits n =
        VkExternalMemoryFeatureBitmask n

pattern VkExternalMemoryFeatureFlags ::
        VkFlags -> VkExternalMemoryFeatureBitmask FlagMask

pattern VkExternalMemoryFeatureFlags n =
        VkExternalMemoryFeatureBitmask n

deriving instance Bits (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance
         FiniteBits (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance
         Integral (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance Num (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance Bounded (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance Enum (VkExternalMemoryFeatureBitmask FlagMask)

deriving instance Real (VkExternalMemoryFeatureBitmask FlagMask)

instance Show (VkExternalMemoryFeatureBitmask a) where
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
          = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
          = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
          = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT"
        showsPrec p (VkExternalMemoryFeatureBitmask x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryFeatureBitmask " . showsPrec 11 x)

instance Read (VkExternalMemoryFeatureBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT",
                   pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT),
                  ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT",
                   pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT),
                  ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT",
                   pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryFeatureBitmask") >>
                      (VkExternalMemoryFeatureBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT ::
        VkExternalMemoryFeatureBitmask a

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT =
        VkExternalMemoryFeatureBitmask 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT ::
        VkExternalMemoryFeatureBitmask a

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT =
        VkExternalMemoryFeatureBitmask 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT ::
        VkExternalMemoryFeatureBitmask a

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT =
        VkExternalMemoryFeatureBitmask 4

newtype VkExternalMemoryFeatureBitmaskNV (a ::
                                            FlagType) = VkExternalMemoryFeatureBitmaskNV VkFlags
                                                          deriving (Eq, Ord, Storable, Data,
                                                                    Generic)

type VkExternalMemoryFeatureFlagsNV =
     VkExternalMemoryFeatureBitmaskNV FlagMask

type VkExternalMemoryFeatureFlagBitsNV =
     VkExternalMemoryFeatureBitmaskNV FlagBit

pattern VkExternalMemoryFeatureFlagBitsNV ::
        VkFlags -> VkExternalMemoryFeatureBitmaskNV FlagBit

pattern VkExternalMemoryFeatureFlagBitsNV n =
        VkExternalMemoryFeatureBitmaskNV n

pattern VkExternalMemoryFeatureFlagsNV ::
        VkFlags -> VkExternalMemoryFeatureBitmaskNV FlagMask

pattern VkExternalMemoryFeatureFlagsNV n =
        VkExternalMemoryFeatureBitmaskNV n

deriving instance Bits (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance
         FiniteBits (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance
         Integral (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance Num (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance
         Bounded (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance Enum (VkExternalMemoryFeatureBitmaskNV FlagMask)

deriving instance Real (VkExternalMemoryFeatureBitmaskNV FlagMask)

instance Show (VkExternalMemoryFeatureBitmaskNV a) where
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV"
        showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
          = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV"
        showsPrec p (VkExternalMemoryFeatureBitmaskNV x)
          = showParen (p >= 11)
              (showString "VkExternalMemoryFeatureBitmaskNV " . showsPrec 11 x)

instance Read (VkExternalMemoryFeatureBitmaskNV a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV),
                  ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV",
                   pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalMemoryFeatureBitmaskNV") >>
                      (VkExternalMemoryFeatureBitmaskNV <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV ::
        VkExternalMemoryFeatureBitmaskNV a

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV =
        VkExternalMemoryFeatureBitmaskNV 1

-- | bitpos = @1@
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV ::
        VkExternalMemoryFeatureBitmaskNV a

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV =
        VkExternalMemoryFeatureBitmaskNV 2

-- | bitpos = @2@
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV ::
        VkExternalMemoryFeatureBitmaskNV a

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV =
        VkExternalMemoryFeatureBitmaskNV 4

newtype VkExternalMemoryHandleTypeFlagBitsKHR = VkExternalMemoryHandleTypeFlagBitsKHR VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkExternalMemoryHandleTypeFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalMemoryHandleTypeFlagBitsKHR x) = show x

instance Read VkExternalMemoryHandleTypeFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

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

newtype VkExternalSemaphoreFeatureFlagBitsKHR = VkExternalSemaphoreFeatureFlagBitsKHR VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkExternalSemaphoreFeatureFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalSemaphoreFeatureFlagBitsKHR x) = show x

instance Read VkExternalSemaphoreFeatureFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkExternalSemaphoreFeatureBitmask (a ::
                                             FlagType) = VkExternalSemaphoreFeatureBitmask VkFlags
                                                           deriving (Eq, Ord, Storable, Data,
                                                                     Generic)

type VkExternalSemaphoreFeatureFlags =
     VkExternalSemaphoreFeatureBitmask FlagMask

type VkExternalSemaphoreFeatureFlagBits =
     VkExternalSemaphoreFeatureBitmask FlagBit

pattern VkExternalSemaphoreFeatureFlagBits ::
        VkFlags -> VkExternalSemaphoreFeatureBitmask FlagBit

pattern VkExternalSemaphoreFeatureFlagBits n =
        VkExternalSemaphoreFeatureBitmask n

pattern VkExternalSemaphoreFeatureFlags ::
        VkFlags -> VkExternalSemaphoreFeatureBitmask FlagMask

pattern VkExternalSemaphoreFeatureFlags n =
        VkExternalSemaphoreFeatureBitmask n

deriving instance Bits (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance
         FiniteBits (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance
         Integral (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance Num (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance
         Bounded (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance Enum (VkExternalSemaphoreFeatureBitmask FlagMask)

deriving instance Real (VkExternalSemaphoreFeatureBitmask FlagMask)

instance Show (VkExternalSemaphoreFeatureBitmask a) where
        showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
          = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT"
        showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
          = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT"
        showsPrec p (VkExternalSemaphoreFeatureBitmask x)
          = showParen (p >= 11)
              (showString "VkExternalSemaphoreFeatureBitmask " . showsPrec 11 x)

instance Read (VkExternalSemaphoreFeatureBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT",
                   pure VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT),
                  ("VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT",
                   pure VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkExternalSemaphoreFeatureBitmask") >>
                      (VkExternalSemaphoreFeatureBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT ::
        VkExternalSemaphoreFeatureBitmask a

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT =
        VkExternalSemaphoreFeatureBitmask 1

-- | bitpos = @1@
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT ::
        VkExternalSemaphoreFeatureBitmask a

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT =
        VkExternalSemaphoreFeatureBitmask 2

newtype VkExternalSemaphoreHandleTypeFlagBitsKHR = VkExternalSemaphoreHandleTypeFlagBitsKHR VkFlags
                                                     deriving (Eq, Ord, Num, Bounded, Enum,
                                                               Integral, Bits, FiniteBits, Storable,
                                                               Real, Data, Generic)

instance Show VkExternalSemaphoreHandleTypeFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalSemaphoreHandleTypeFlagBitsKHR x) = show x

instance Read VkExternalSemaphoreHandleTypeFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

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
