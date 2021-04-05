{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Sampler
       (VkSamplerAddressMode(VkSamplerAddressMode,
                             VK_SAMPLER_ADDRESS_MODE_REPEAT,
                             VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT,
                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER),
        VkSamplerCreateBitmask(VkSamplerCreateBitmask,
                               VkSamplerCreateFlags, VkSamplerCreateFlagBits),
        VkSamplerCreateFlags, VkSamplerCreateFlagBits,
        VkSamplerMipmapMode(VkSamplerMipmapMode,
                            VK_SAMPLER_MIPMAP_MODE_NEAREST, VK_SAMPLER_MIPMAP_MODE_LINEAR),
        VkSamplerReductionMode(VkSamplerReductionMode,
                               VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE,
                               VK_SAMPLER_REDUCTION_MODE_MIN, VK_SAMPLER_REDUCTION_MODE_MAX),
        VkSamplerReductionModeEXT(..),
        VkSamplerYcbcrModelConversion(VkSamplerYcbcrModelConversion,
                                      VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY,
                                      VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY,
                                      VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709,
                                      VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601,
                                      VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020),
        VkSamplerYcbcrModelConversionKHR(..),
        VkSamplerYcbcrRange(VkSamplerYcbcrRange,
                            VK_SAMPLER_YCBCR_RANGE_ITU_FULL,
                            VK_SAMPLER_YCBCR_RANGE_ITU_NARROW),
        VkSamplerYcbcrRangeKHR(..))
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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSamplerAddressMode VkSamplerAddressMode registry at www.khronos.org>
newtype VkSamplerAddressMode = VkSamplerAddressMode Int32
                               deriving (Eq, Ord, Enum, Storable)

instance Show VkSamplerAddressMode where
    showsPrec _ VK_SAMPLER_ADDRESS_MODE_REPEAT
      = showString "VK_SAMPLER_ADDRESS_MODE_REPEAT"
    showsPrec _ VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
      = showString "VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT"
    showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
      = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE"
    showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
      = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER"
    showsPrec p (VkSamplerAddressMode x)
      = showParen (p >= 11)
          (showString "VkSamplerAddressMode " . showsPrec 11 x)

instance Read VkSamplerAddressMode where
    readPrec
      = parens
          (choose
             [("VK_SAMPLER_ADDRESS_MODE_REPEAT",
               pure VK_SAMPLER_ADDRESS_MODE_REPEAT),
              ("VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT",
               pure VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT),
              ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE",
               pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE),
              ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER",
               pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER)]
             +++
             prec 10
               (expectP (Ident "VkSamplerAddressMode") >>
                  (VkSamplerAddressMode <$> step readPrec)))

pattern VK_SAMPLER_ADDRESS_MODE_REPEAT :: VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = VkSamplerAddressMode 0

pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT =
        VkSamplerAddressMode 1

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE =
        VkSamplerAddressMode 2

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER =
        VkSamplerAddressMode 3

newtype VkSamplerCreateBitmask (a ::
                                  FlagType) = VkSamplerCreateBitmask VkFlags
                                              deriving (Eq, Ord, Storable)

type VkSamplerCreateFlags = VkSamplerCreateBitmask FlagMask

type VkSamplerCreateFlagBits = VkSamplerCreateBitmask FlagBit

pattern VkSamplerCreateFlagBits ::
        VkFlags -> VkSamplerCreateBitmask FlagBit

pattern VkSamplerCreateFlagBits n = VkSamplerCreateBitmask n

pattern VkSamplerCreateFlags ::
        VkFlags -> VkSamplerCreateBitmask FlagMask

pattern VkSamplerCreateFlags n = VkSamplerCreateBitmask n

deriving instance Bits (VkSamplerCreateBitmask FlagMask)

deriving instance FiniteBits (VkSamplerCreateBitmask FlagMask)

instance Show (VkSamplerCreateBitmask a) where
    showsPrec p (VkSamplerCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkSamplerCreateBitmask " . showsPrec 11 x)

instance Read (VkSamplerCreateBitmask a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkSamplerCreateBitmask") >>
                  (VkSamplerCreateBitmask <$> step readPrec)))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSamplerMipmapMode VkSamplerMipmapMode registry at www.khronos.org>
newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int32
                              deriving (Eq, Ord, Enum, Storable)

instance Show VkSamplerMipmapMode where
    showsPrec _ VK_SAMPLER_MIPMAP_MODE_NEAREST
      = showString "VK_SAMPLER_MIPMAP_MODE_NEAREST"
    showsPrec _ VK_SAMPLER_MIPMAP_MODE_LINEAR
      = showString "VK_SAMPLER_MIPMAP_MODE_LINEAR"
    showsPrec p (VkSamplerMipmapMode x)
      = showParen (p >= 11)
          (showString "VkSamplerMipmapMode " . showsPrec 11 x)

instance Read VkSamplerMipmapMode where
    readPrec
      = parens
          (choose
             [("VK_SAMPLER_MIPMAP_MODE_NEAREST",
               pure VK_SAMPLER_MIPMAP_MODE_NEAREST),
              ("VK_SAMPLER_MIPMAP_MODE_LINEAR",
               pure VK_SAMPLER_MIPMAP_MODE_LINEAR)]
             +++
             prec 10
               (expectP (Ident "VkSamplerMipmapMode") >>
                  (VkSamplerMipmapMode <$> step readPrec)))

-- | Choose nearest mip level
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST :: VkSamplerMipmapMode

pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0

-- | Linear filter between mip levels
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR :: VkSamplerMipmapMode

pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSamplerReductionMode VkSamplerReductionMode registry at www.khronos.org>
newtype VkSamplerReductionMode = VkSamplerReductionMode Int32
                                 deriving (Eq, Ord, Enum, Storable)

instance Show VkSamplerReductionMode where
    showsPrec _ VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE
      = showString "VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE"
    showsPrec _ VK_SAMPLER_REDUCTION_MODE_MIN
      = showString "VK_SAMPLER_REDUCTION_MODE_MIN"
    showsPrec _ VK_SAMPLER_REDUCTION_MODE_MAX
      = showString "VK_SAMPLER_REDUCTION_MODE_MAX"
    showsPrec p (VkSamplerReductionMode x)
      = showParen (p >= 11)
          (showString "VkSamplerReductionMode " . showsPrec 11 x)

instance Read VkSamplerReductionMode where
    readPrec
      = parens
          (choose
             [("VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE",
               pure VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE),
              ("VK_SAMPLER_REDUCTION_MODE_MIN",
               pure VK_SAMPLER_REDUCTION_MODE_MIN),
              ("VK_SAMPLER_REDUCTION_MODE_MAX",
               pure VK_SAMPLER_REDUCTION_MODE_MAX)]
             +++
             prec 10
               (expectP (Ident "VkSamplerReductionMode") >>
                  (VkSamplerReductionMode <$> step readPrec)))

pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE ::
        VkSamplerReductionMode

pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE =
        VkSamplerReductionMode 0

pattern VK_SAMPLER_REDUCTION_MODE_MIN :: VkSamplerReductionMode

pattern VK_SAMPLER_REDUCTION_MODE_MIN = VkSamplerReductionMode 1

pattern VK_SAMPLER_REDUCTION_MODE_MAX :: VkSamplerReductionMode

pattern VK_SAMPLER_REDUCTION_MODE_MAX = VkSamplerReductionMode 2

newtype VkSamplerReductionModeEXT = VkSamplerReductionModeEXT VkFlags
                                    deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkSamplerReductionModeEXT where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkSamplerReductionModeEXT where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSamplerYcbcrModelConversion VkSamplerYcbcrModelConversion registry at www.khronos.org>
newtype VkSamplerYcbcrModelConversion = VkSamplerYcbcrModelConversion Int32
                                        deriving (Eq, Ord, Enum, Storable)

instance Show VkSamplerYcbcrModelConversion where
    showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
      = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY"
    showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
      = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY"
    showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
      = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709"
    showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
      = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601"
    showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020
      = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020"
    showsPrec p (VkSamplerYcbcrModelConversion x)
      = showParen (p >= 11)
          (showString "VkSamplerYcbcrModelConversion " . showsPrec 11 x)

instance Read VkSamplerYcbcrModelConversion where
    readPrec
      = parens
          (choose
             [("VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY",
               pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY),
              ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY",
               pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY),
              ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709",
               pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709),
              ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601",
               pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601),
              ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020",
               pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020)]
             +++
             prec 10
               (expectP (Ident "VkSamplerYcbcrModelConversion") >>
                  (VkSamplerYcbcrModelConversion <$> step readPrec)))

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY ::
        VkSamplerYcbcrModelConversion

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY =
        VkSamplerYcbcrModelConversion 0

-- | just range expansion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY ::
        VkSamplerYcbcrModelConversion

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY =
        VkSamplerYcbcrModelConversion 1

-- | aka HD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 ::
        VkSamplerYcbcrModelConversion

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 =
        VkSamplerYcbcrModelConversion 2

-- | aka SD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 ::
        VkSamplerYcbcrModelConversion

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 =
        VkSamplerYcbcrModelConversion 3

-- | aka UHD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 ::
        VkSamplerYcbcrModelConversion

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 =
        VkSamplerYcbcrModelConversion 4

newtype VkSamplerYcbcrModelConversionKHR = VkSamplerYcbcrModelConversionKHR VkFlags
                                           deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkSamplerYcbcrModelConversionKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkSamplerYcbcrModelConversionKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSamplerYcbcrRange VkSamplerYcbcrRange registry at www.khronos.org>
newtype VkSamplerYcbcrRange = VkSamplerYcbcrRange Int32
                              deriving (Eq, Ord, Enum, Storable)

instance Show VkSamplerYcbcrRange where
    showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_FULL
      = showString "VK_SAMPLER_YCBCR_RANGE_ITU_FULL"
    showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_NARROW
      = showString "VK_SAMPLER_YCBCR_RANGE_ITU_NARROW"
    showsPrec p (VkSamplerYcbcrRange x)
      = showParen (p >= 11)
          (showString "VkSamplerYcbcrRange " . showsPrec 11 x)

instance Read VkSamplerYcbcrRange where
    readPrec
      = parens
          (choose
             [("VK_SAMPLER_YCBCR_RANGE_ITU_FULL",
               pure VK_SAMPLER_YCBCR_RANGE_ITU_FULL),
              ("VK_SAMPLER_YCBCR_RANGE_ITU_NARROW",
               pure VK_SAMPLER_YCBCR_RANGE_ITU_NARROW)]
             +++
             prec 10
               (expectP (Ident "VkSamplerYcbcrRange") >>
                  (VkSamplerYcbcrRange <$> step readPrec)))

-- | Luma 0..1 maps to 0..255, chroma -0.5..0.5 to 1..255 (clamped)
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL :: VkSamplerYcbcrRange

pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL = VkSamplerYcbcrRange 0

-- | Luma 0..1 maps to 16..235, chroma -0.5..0.5 to 16..240
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW :: VkSamplerYcbcrRange

pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW = VkSamplerYcbcrRange 1

newtype VkSamplerYcbcrRangeKHR = VkSamplerYcbcrRangeKHR VkFlags
                                 deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkSamplerYcbcrRangeKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkSamplerYcbcrRangeKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
