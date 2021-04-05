{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Shader
       (VkShaderCorePropertiesBitmaskAMD(VkShaderCorePropertiesBitmaskAMD,
                                         VkShaderCorePropertiesFlagsAMD,
                                         VkShaderCorePropertiesFlagBitsAMD),
        VkShaderCorePropertiesFlagsAMD, VkShaderCorePropertiesFlagBitsAMD,
        VkShaderFloatControlsIndependence(VkShaderFloatControlsIndependence,
                                          VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY,
                                          VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL,
                                          VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE),
        VkShaderFloatControlsIndependenceKHR(..),
        VkShaderInfoTypeAMD(VkShaderInfoTypeAMD,
                            VK_SHADER_INFO_TYPE_STATISTICS_AMD, VK_SHADER_INFO_TYPE_BINARY_AMD,
                            VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD),
        VkShaderModuleCreateBitmask(VkShaderModuleCreateBitmask,
                                    VkShaderModuleCreateFlags, VkShaderModuleCreateFlagBits),
        VkShaderModuleCreateFlags, VkShaderModuleCreateFlagBits,
        VkShaderStageBitmask(VkShaderStageBitmask, VkShaderStageFlags,
                             VkShaderStageFlagBits, VK_SHADER_STAGE_VERTEX_BIT,
                             VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT,
                             VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT,
                             VK_SHADER_STAGE_GEOMETRY_BIT, VK_SHADER_STAGE_FRAGMENT_BIT,
                             VK_SHADER_STAGE_COMPUTE_BIT, VK_SHADER_STAGE_ALL_GRAPHICS,
                             VK_SHADER_STAGE_ALL),
        VkShaderStageFlags, VkShaderStageFlagBits)
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

newtype VkShaderCorePropertiesBitmaskAMD (a ::
                                            FlagType) = VkShaderCorePropertiesBitmaskAMD VkFlags
                                                        deriving (Eq, Ord, Storable)

type VkShaderCorePropertiesFlagsAMD =
     VkShaderCorePropertiesBitmaskAMD FlagMask

type VkShaderCorePropertiesFlagBitsAMD =
     VkShaderCorePropertiesBitmaskAMD FlagBit

pattern VkShaderCorePropertiesFlagBitsAMD ::
        VkFlags -> VkShaderCorePropertiesBitmaskAMD FlagBit

pattern VkShaderCorePropertiesFlagBitsAMD n =
        VkShaderCorePropertiesBitmaskAMD n

pattern VkShaderCorePropertiesFlagsAMD ::
        VkFlags -> VkShaderCorePropertiesBitmaskAMD FlagMask

pattern VkShaderCorePropertiesFlagsAMD n =
        VkShaderCorePropertiesBitmaskAMD n

deriving instance Bits (VkShaderCorePropertiesBitmaskAMD FlagMask)

deriving instance
         FiniteBits (VkShaderCorePropertiesBitmaskAMD FlagMask)

instance Show (VkShaderCorePropertiesBitmaskAMD a) where
    showsPrec p (VkShaderCorePropertiesBitmaskAMD x)
      = showParen (p >= 11)
          (showString "VkShaderCorePropertiesBitmaskAMD " . showsPrec 11 x)

instance Read (VkShaderCorePropertiesBitmaskAMD a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkShaderCorePropertiesBitmaskAMD") >>
                  (VkShaderCorePropertiesBitmaskAMD <$> step readPrec)))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkShaderFloatControlsIndependence VkShaderFloatControlsIndependence registry at www.khronos.org>
newtype VkShaderFloatControlsIndependence = VkShaderFloatControlsIndependence Int32
                                            deriving (Eq, Ord, Enum, Storable)

instance Show VkShaderFloatControlsIndependence where
    showsPrec _ VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY
      = showString "VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY"
    showsPrec _ VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL
      = showString "VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL"
    showsPrec _ VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE
      = showString "VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE"
    showsPrec p (VkShaderFloatControlsIndependence x)
      = showParen (p >= 11)
          (showString "VkShaderFloatControlsIndependence " . showsPrec 11 x)

instance Read VkShaderFloatControlsIndependence where
    readPrec
      = parens
          (choose
             [("VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY",
               pure VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY),
              ("VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL",
               pure VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL),
              ("VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE",
               pure VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE)]
             +++
             prec 10
               (expectP (Ident "VkShaderFloatControlsIndependence") >>
                  (VkShaderFloatControlsIndependence <$> step readPrec)))

pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY ::
        VkShaderFloatControlsIndependence

pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY =
        VkShaderFloatControlsIndependence 0

pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL ::
        VkShaderFloatControlsIndependence

pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL =
        VkShaderFloatControlsIndependence 1

pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE ::
        VkShaderFloatControlsIndependence

pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE =
        VkShaderFloatControlsIndependence 2

newtype VkShaderFloatControlsIndependenceKHR = VkShaderFloatControlsIndependenceKHR VkFlags
                                               deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkShaderFloatControlsIndependenceKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkShaderFloatControlsIndependenceKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkShaderInfoTypeAMD VkShaderInfoTypeAMD registry at www.khronos.org>
newtype VkShaderInfoTypeAMD = VkShaderInfoTypeAMD Int32
                              deriving (Eq, Ord, Enum, Storable)

instance Show VkShaderInfoTypeAMD where
    showsPrec _ VK_SHADER_INFO_TYPE_STATISTICS_AMD
      = showString "VK_SHADER_INFO_TYPE_STATISTICS_AMD"
    showsPrec _ VK_SHADER_INFO_TYPE_BINARY_AMD
      = showString "VK_SHADER_INFO_TYPE_BINARY_AMD"
    showsPrec _ VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD
      = showString "VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD"
    showsPrec p (VkShaderInfoTypeAMD x)
      = showParen (p >= 11)
          (showString "VkShaderInfoTypeAMD " . showsPrec 11 x)

instance Read VkShaderInfoTypeAMD where
    readPrec
      = parens
          (choose
             [("VK_SHADER_INFO_TYPE_STATISTICS_AMD",
               pure VK_SHADER_INFO_TYPE_STATISTICS_AMD),
              ("VK_SHADER_INFO_TYPE_BINARY_AMD",
               pure VK_SHADER_INFO_TYPE_BINARY_AMD),
              ("VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD",
               pure VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD)]
             +++
             prec 10
               (expectP (Ident "VkShaderInfoTypeAMD") >>
                  (VkShaderInfoTypeAMD <$> step readPrec)))

pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD = VkShaderInfoTypeAMD 0

pattern VK_SHADER_INFO_TYPE_BINARY_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_BINARY_AMD = VkShaderInfoTypeAMD 1

pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD = VkShaderInfoTypeAMD 2

newtype VkShaderModuleCreateBitmask (a ::
                                       FlagType) = VkShaderModuleCreateBitmask VkFlags
                                                   deriving (Eq, Ord, Storable)

type VkShaderModuleCreateFlags =
     VkShaderModuleCreateBitmask FlagMask

type VkShaderModuleCreateFlagBits =
     VkShaderModuleCreateBitmask FlagBit

pattern VkShaderModuleCreateFlagBits ::
        VkFlags -> VkShaderModuleCreateBitmask FlagBit

pattern VkShaderModuleCreateFlagBits n =
        VkShaderModuleCreateBitmask n

pattern VkShaderModuleCreateFlags ::
        VkFlags -> VkShaderModuleCreateBitmask FlagMask

pattern VkShaderModuleCreateFlags n = VkShaderModuleCreateBitmask n

deriving instance Bits (VkShaderModuleCreateBitmask FlagMask)

deriving instance FiniteBits (VkShaderModuleCreateBitmask FlagMask)

instance Show (VkShaderModuleCreateBitmask a) where
    showsPrec p (VkShaderModuleCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkShaderModuleCreateBitmask " . showsPrec 11 x)

instance Read (VkShaderModuleCreateBitmask a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkShaderModuleCreateBitmask") >>
                  (VkShaderModuleCreateBitmask <$> step readPrec)))

newtype VkShaderStageBitmask (a ::
                                FlagType) = VkShaderStageBitmask VkFlags
                                            deriving (Eq, Ord, Storable)

type VkShaderStageFlags = VkShaderStageBitmask FlagMask

type VkShaderStageFlagBits = VkShaderStageBitmask FlagBit

pattern VkShaderStageFlagBits ::
        VkFlags -> VkShaderStageBitmask FlagBit

pattern VkShaderStageFlagBits n = VkShaderStageBitmask n

pattern VkShaderStageFlags ::
        VkFlags -> VkShaderStageBitmask FlagMask

pattern VkShaderStageFlags n = VkShaderStageBitmask n

deriving instance Bits (VkShaderStageBitmask FlagMask)

deriving instance FiniteBits (VkShaderStageBitmask FlagMask)

instance Show (VkShaderStageBitmask a) where
    showsPrec _ VK_SHADER_STAGE_VERTEX_BIT
      = showString "VK_SHADER_STAGE_VERTEX_BIT"
    showsPrec _ VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
      = showString "VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT"
    showsPrec _ VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
      = showString "VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT"
    showsPrec _ VK_SHADER_STAGE_GEOMETRY_BIT
      = showString "VK_SHADER_STAGE_GEOMETRY_BIT"
    showsPrec _ VK_SHADER_STAGE_FRAGMENT_BIT
      = showString "VK_SHADER_STAGE_FRAGMENT_BIT"
    showsPrec _ VK_SHADER_STAGE_COMPUTE_BIT
      = showString "VK_SHADER_STAGE_COMPUTE_BIT"
    showsPrec _ VK_SHADER_STAGE_ALL_GRAPHICS
      = showString "VK_SHADER_STAGE_ALL_GRAPHICS"
    showsPrec _ VK_SHADER_STAGE_ALL = showString "VK_SHADER_STAGE_ALL"
    showsPrec p (VkShaderStageBitmask x)
      = showParen (p >= 11)
          (showString "VkShaderStageBitmask " . showsPrec 11 x)

instance Read (VkShaderStageBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_SHADER_STAGE_VERTEX_BIT", pure VK_SHADER_STAGE_VERTEX_BIT),
              ("VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT",
               pure VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT),
              ("VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT",
               pure VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT),
              ("VK_SHADER_STAGE_GEOMETRY_BIT",
               pure VK_SHADER_STAGE_GEOMETRY_BIT),
              ("VK_SHADER_STAGE_FRAGMENT_BIT",
               pure VK_SHADER_STAGE_FRAGMENT_BIT),
              ("VK_SHADER_STAGE_COMPUTE_BIT", pure VK_SHADER_STAGE_COMPUTE_BIT),
              ("VK_SHADER_STAGE_ALL_GRAPHICS",
               pure VK_SHADER_STAGE_ALL_GRAPHICS),
              ("VK_SHADER_STAGE_ALL", pure VK_SHADER_STAGE_ALL)]
             +++
             prec 10
               (expectP (Ident "VkShaderStageBitmask") >>
                  (VkShaderStageBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_SHADER_STAGE_VERTEX_BIT :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_VERTEX_BIT = VkShaderStageBitmask 1

-- | bitpos = @1@
pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT ::
        VkShaderStageBitmask a

pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT =
        VkShaderStageBitmask 2

-- | bitpos = @2@
pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT ::
        VkShaderStageBitmask a

pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT =
        VkShaderStageBitmask 4

-- | bitpos = @3@
pattern VK_SHADER_STAGE_GEOMETRY_BIT :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_GEOMETRY_BIT = VkShaderStageBitmask 8

-- | bitpos = @4@
pattern VK_SHADER_STAGE_FRAGMENT_BIT :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_FRAGMENT_BIT = VkShaderStageBitmask 16

-- | bitpos = @5@
pattern VK_SHADER_STAGE_COMPUTE_BIT :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_COMPUTE_BIT = VkShaderStageBitmask 32

pattern VK_SHADER_STAGE_ALL_GRAPHICS :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_ALL_GRAPHICS = VkShaderStageBitmask 31

pattern VK_SHADER_STAGE_ALL :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_ALL = VkShaderStageBitmask 2147483647
