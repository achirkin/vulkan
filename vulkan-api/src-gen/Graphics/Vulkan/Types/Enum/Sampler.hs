{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.Sampler
       (VkSamplerAddressMode(VkSamplerAddressMode,
                             VK_SAMPLER_ADDRESS_MODE_REPEAT,
                             VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT,
                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER),
        VkSamplerCreateFlagBits(..),
        VkSamplerMipmapMode(VkSamplerMipmapMode,
                            VK_SAMPLER_MIPMAP_MODE_NEAREST, VK_SAMPLER_MIPMAP_MODE_LINEAR),
        VkSamplerReductionModeEXT(VkSamplerReductionModeEXT,
                                  VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT,
                                  VK_SAMPLER_REDUCTION_MODE_MIN_EXT,
                                  VK_SAMPLER_REDUCTION_MODE_MAX_EXT),
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
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerAddressMode VkSamplerAddressMode registry at www.khronos.org>
newtype VkSamplerAddressMode = VkSamplerAddressMode Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

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

newtype VkSamplerCreateFlagBits = VkSamplerCreateFlagBits VkFlags
                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                              FiniteBits, Storable, Real, Data, Generic)

instance Show VkSamplerCreateFlagBits where
        {-# INLINE show #-}
        show (VkSamplerCreateFlagBits x) = show x

instance Read VkSamplerCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerMipmapMode VkSamplerMipmapMode registry at www.khronos.org>
newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerReductionModeEXT VkSamplerReductionModeEXT registry at www.khronos.org>
newtype VkSamplerReductionModeEXT = VkSamplerReductionModeEXT Int32
                                      deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                Generic)

instance Show VkSamplerReductionModeEXT where
        showsPrec _ VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT
          = showString "VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT"
        showsPrec _ VK_SAMPLER_REDUCTION_MODE_MIN_EXT
          = showString "VK_SAMPLER_REDUCTION_MODE_MIN_EXT"
        showsPrec _ VK_SAMPLER_REDUCTION_MODE_MAX_EXT
          = showString "VK_SAMPLER_REDUCTION_MODE_MAX_EXT"
        showsPrec p (VkSamplerReductionModeEXT x)
          = showParen (p >= 11)
              (showString "VkSamplerReductionModeEXT " . showsPrec 11 x)

instance Read VkSamplerReductionModeEXT where
        readPrec
          = parens
              (choose
                 [("VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT",
                   pure VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT),
                  ("VK_SAMPLER_REDUCTION_MODE_MIN_EXT",
                   pure VK_SAMPLER_REDUCTION_MODE_MIN_EXT),
                  ("VK_SAMPLER_REDUCTION_MODE_MAX_EXT",
                   pure VK_SAMPLER_REDUCTION_MODE_MAX_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkSamplerReductionModeEXT") >>
                      (VkSamplerReductionModeEXT <$> step readPrec)))

pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT ::
        VkSamplerReductionModeEXT

pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT =
        VkSamplerReductionModeEXT 0

pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT ::
        VkSamplerReductionModeEXT

pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT =
        VkSamplerReductionModeEXT 1

pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT ::
        VkSamplerReductionModeEXT

pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT =
        VkSamplerReductionModeEXT 2

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerYcbcrModelConversion VkSamplerYcbcrModelConversion registry at www.khronos.org>
newtype VkSamplerYcbcrModelConversion = VkSamplerYcbcrModelConversion Int32
                                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                    Generic)

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
                                             deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                       FiniteBits, Storable, Real, Data, Generic)

instance Show VkSamplerYcbcrModelConversionKHR where
        {-# INLINE show #-}
        show (VkSamplerYcbcrModelConversionKHR x) = show x

instance Read VkSamplerYcbcrModelConversionKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerYcbcrRange VkSamplerYcbcrRange registry at www.khronos.org>
newtype VkSamplerYcbcrRange = VkSamplerYcbcrRange Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

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
                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                             FiniteBits, Storable, Real, Data, Generic)

instance Show VkSamplerYcbcrRangeKHR where
        {-# INLINE show #-}
        show (VkSamplerYcbcrRangeKHR x) = show x

instance Read VkSamplerYcbcrRangeKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
