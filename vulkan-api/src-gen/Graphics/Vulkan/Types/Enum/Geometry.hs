{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Geometry
       (VkGeometryFlagBitsNV(..),
        VkGeometryBitmaskKHR(VkGeometryBitmaskKHR, VkGeometryFlagsKHR,
                             VkGeometryFlagBitsKHR, VK_GEOMETRY_OPAQUE_BIT_KHR,
                             VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR),
        VkGeometryFlagsKHR, VkGeometryFlagBitsKHR,
        VkGeometryInstanceFlagBitsNV(..),
        VkGeometryInstanceBitmaskKHR(VkGeometryInstanceBitmaskKHR,
                                     VkGeometryInstanceFlagsKHR, VkGeometryInstanceFlagBitsKHR,
                                     VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR,
                                     VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR,
                                     VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR,
                                     VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR),
        VkGeometryInstanceFlagsKHR, VkGeometryInstanceFlagBitsKHR,
        VkGeometryTypeKHR(VkGeometryTypeKHR,
                          VK_GEOMETRY_TYPE_TRIANGLES_KHR, VK_GEOMETRY_TYPE_AABBS_KHR),
        VkGeometryTypeNV(..))
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

newtype VkGeometryFlagBitsNV = VkGeometryFlagBitsNV VkFlags
                               deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkGeometryFlagBitsNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkGeometryFlagBitsNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkGeometryBitmaskKHR (a ::
                                FlagType) = VkGeometryBitmaskKHR VkFlags
                                            deriving (Eq, Ord, Storable)

type VkGeometryFlagsKHR = VkGeometryBitmaskKHR FlagMask

type VkGeometryFlagBitsKHR = VkGeometryBitmaskKHR FlagBit

pattern VkGeometryFlagBitsKHR ::
        VkFlags -> VkGeometryBitmaskKHR FlagBit

pattern VkGeometryFlagBitsKHR n = VkGeometryBitmaskKHR n

pattern VkGeometryFlagsKHR ::
        VkFlags -> VkGeometryBitmaskKHR FlagMask

pattern VkGeometryFlagsKHR n = VkGeometryBitmaskKHR n

deriving instance Bits (VkGeometryBitmaskKHR FlagMask)

deriving instance FiniteBits (VkGeometryBitmaskKHR FlagMask)

instance Show (VkGeometryBitmaskKHR a) where
    showsPrec _ VK_GEOMETRY_OPAQUE_BIT_KHR
      = showString "VK_GEOMETRY_OPAQUE_BIT_KHR"
    showsPrec _ VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR
      = showString "VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR"
    showsPrec p (VkGeometryBitmaskKHR x)
      = showParen (p >= 11)
          (showString "VkGeometryBitmaskKHR " . showsPrec 11 x)

instance Read (VkGeometryBitmaskKHR a) where
    readPrec
      = parens
          (choose
             [("VK_GEOMETRY_OPAQUE_BIT_KHR", pure VK_GEOMETRY_OPAQUE_BIT_KHR),
              ("VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR",
               pure VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR)]
             +++
             prec 10
               (expectP (Ident "VkGeometryBitmaskKHR") >>
                  (VkGeometryBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_GEOMETRY_OPAQUE_BIT_KHR :: VkGeometryBitmaskKHR a

pattern VK_GEOMETRY_OPAQUE_BIT_KHR = VkGeometryBitmaskKHR 1

-- | bitpos = @1@
pattern VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR ::
        VkGeometryBitmaskKHR a

pattern VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR =
        VkGeometryBitmaskKHR 2

newtype VkGeometryInstanceFlagBitsNV = VkGeometryInstanceFlagBitsNV VkFlags
                                       deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkGeometryInstanceFlagBitsNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkGeometryInstanceFlagBitsNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkGeometryInstanceBitmaskKHR (a ::
                                        FlagType) = VkGeometryInstanceBitmaskKHR VkFlags
                                                    deriving (Eq, Ord, Storable)

type VkGeometryInstanceFlagsKHR =
     VkGeometryInstanceBitmaskKHR FlagMask

type VkGeometryInstanceFlagBitsKHR =
     VkGeometryInstanceBitmaskKHR FlagBit

pattern VkGeometryInstanceFlagBitsKHR ::
        VkFlags -> VkGeometryInstanceBitmaskKHR FlagBit

pattern VkGeometryInstanceFlagBitsKHR n =
        VkGeometryInstanceBitmaskKHR n

pattern VkGeometryInstanceFlagsKHR ::
        VkFlags -> VkGeometryInstanceBitmaskKHR FlagMask

pattern VkGeometryInstanceFlagsKHR n =
        VkGeometryInstanceBitmaskKHR n

deriving instance Bits (VkGeometryInstanceBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkGeometryInstanceBitmaskKHR FlagMask)

instance Show (VkGeometryInstanceBitmaskKHR a) where
    showsPrec _
      VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR
      = showString
          "VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR"
    showsPrec _
      VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR
      = showString
          "VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR"
    showsPrec _ VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR
      = showString "VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR"
    showsPrec _ VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR
      = showString "VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR"
    showsPrec p (VkGeometryInstanceBitmaskKHR x)
      = showParen (p >= 11)
          (showString "VkGeometryInstanceBitmaskKHR " . showsPrec 11 x)

instance Read (VkGeometryInstanceBitmaskKHR a) where
    readPrec
      = parens
          (choose
             [("VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR",
               pure VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR),
              ("VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR",
               pure VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR),
              ("VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR",
               pure VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR),
              ("VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR",
               pure VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR)]
             +++
             prec 10
               (expectP (Ident "VkGeometryInstanceBitmaskKHR") >>
                  (VkGeometryInstanceBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR
        :: VkGeometryInstanceBitmaskKHR a

pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR =
        VkGeometryInstanceBitmaskKHR 1

-- | bitpos = @1@
pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR
        :: VkGeometryInstanceBitmaskKHR a

pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR
        = VkGeometryInstanceBitmaskKHR 2

-- | bitpos = @2@
pattern VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR ::
        VkGeometryInstanceBitmaskKHR a

pattern VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR =
        VkGeometryInstanceBitmaskKHR 4

-- | bitpos = @3@
pattern VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR ::
        VkGeometryInstanceBitmaskKHR a

pattern VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR =
        VkGeometryInstanceBitmaskKHR 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkGeometryTypeKHR VkGeometryTypeKHR registry at www.khronos.org>
newtype VkGeometryTypeKHR = VkGeometryTypeKHR Int32
                            deriving (Eq, Ord, Enum, Storable)

instance Show VkGeometryTypeKHR where
    showsPrec _ VK_GEOMETRY_TYPE_TRIANGLES_KHR
      = showString "VK_GEOMETRY_TYPE_TRIANGLES_KHR"
    showsPrec _ VK_GEOMETRY_TYPE_AABBS_KHR
      = showString "VK_GEOMETRY_TYPE_AABBS_KHR"
    showsPrec p (VkGeometryTypeKHR x)
      = showParen (p >= 11)
          (showString "VkGeometryTypeKHR " . showsPrec 11 x)

instance Read VkGeometryTypeKHR where
    readPrec
      = parens
          (choose
             [("VK_GEOMETRY_TYPE_TRIANGLES_KHR",
               pure VK_GEOMETRY_TYPE_TRIANGLES_KHR),
              ("VK_GEOMETRY_TYPE_AABBS_KHR", pure VK_GEOMETRY_TYPE_AABBS_KHR)]
             +++
             prec 10
               (expectP (Ident "VkGeometryTypeKHR") >>
                  (VkGeometryTypeKHR <$> step readPrec)))

pattern VK_GEOMETRY_TYPE_TRIANGLES_KHR :: VkGeometryTypeKHR

pattern VK_GEOMETRY_TYPE_TRIANGLES_KHR = VkGeometryTypeKHR 0

pattern VK_GEOMETRY_TYPE_AABBS_KHR :: VkGeometryTypeKHR

pattern VK_GEOMETRY_TYPE_AABBS_KHR = VkGeometryTypeKHR 1

newtype VkGeometryTypeNV = VkGeometryTypeNV VkFlags
                           deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkGeometryTypeNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkGeometryTypeNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
