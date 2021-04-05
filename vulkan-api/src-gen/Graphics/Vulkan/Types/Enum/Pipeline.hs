{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Pipeline
       (VkPipelineBindPoint(VkPipelineBindPoint,
                            VK_PIPELINE_BIND_POINT_GRAPHICS, VK_PIPELINE_BIND_POINT_COMPUTE),
        VkPipelineCacheCreateBitmask(VkPipelineCacheCreateBitmask,
                                     VkPipelineCacheCreateFlags, VkPipelineCacheCreateFlagBits),
        VkPipelineCacheCreateFlags, VkPipelineCacheCreateFlagBits,
        VkPipelineCacheHeaderVersion(VkPipelineCacheHeaderVersion,
                                     VK_PIPELINE_CACHE_HEADER_VERSION_ONE),
        VkPipelineCompilerControlBitmaskAMD(VkPipelineCompilerControlBitmaskAMD,
                                            VkPipelineCompilerControlFlagsAMD,
                                            VkPipelineCompilerControlFlagBitsAMD),
        VkPipelineCompilerControlFlagsAMD,
        VkPipelineCompilerControlFlagBitsAMD,
        VkPipelineCreateBitmask(VkPipelineCreateBitmask,
                                VkPipelineCreateFlags, VkPipelineCreateFlagBits,
                                VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT,
                                VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT,
                                VK_PIPELINE_CREATE_DERIVATIVE_BIT),
        VkPipelineCreateFlags, VkPipelineCreateFlagBits,
        VkPipelineCreationFeedbackBitmaskEXT(VkPipelineCreationFeedbackBitmaskEXT,
                                             VkPipelineCreationFeedbackFlagsEXT,
                                             VkPipelineCreationFeedbackFlagBitsEXT,
                                             VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT,
                                             VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT,
                                             VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT),
        VkPipelineCreationFeedbackFlagsEXT,
        VkPipelineCreationFeedbackFlagBitsEXT,
        VkPipelineExecutableStatisticFormatKHR(VkPipelineExecutableStatisticFormatKHR,
                                               VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR,
                                               VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR,
                                               VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR,
                                               VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR),
        VkPipelineShaderStageCreateBitmask(VkPipelineShaderStageCreateBitmask,
                                           VkPipelineShaderStageCreateFlags,
                                           VkPipelineShaderStageCreateFlagBits),
        VkPipelineShaderStageCreateFlags,
        VkPipelineShaderStageCreateFlagBits,
        VkPipelineStageBitmask(VkPipelineStageBitmask,
                               VkPipelineStageFlags, VkPipelineStageFlagBits,
                               VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                               VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT,
                               VK_PIPELINE_STAGE_VERTEX_INPUT_BIT,
                               VK_PIPELINE_STAGE_VERTEX_SHADER_BIT,
                               VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT,
                               VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT,
                               VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT,
                               VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
                               VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT,
                               VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT,
                               VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                               VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
                               VK_PIPELINE_STAGE_TRANSFER_BIT,
                               VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, VK_PIPELINE_STAGE_HOST_BIT,
                               VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT,
                               VK_PIPELINE_STAGE_ALL_COMMANDS_BIT),
        VkPipelineStageFlags, VkPipelineStageFlagBits)
       where
import Data.Bits                       (Bits, FiniteBits)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType, Int32)
import Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineBindPoint VkPipelineBindPoint registry at www.khronos.org>
newtype VkPipelineBindPoint = VkPipelineBindPoint Int32
                              deriving (Eq, Ord, Enum, Storable)

instance Show VkPipelineBindPoint where
    showsPrec _ VK_PIPELINE_BIND_POINT_GRAPHICS
      = showString "VK_PIPELINE_BIND_POINT_GRAPHICS"
    showsPrec _ VK_PIPELINE_BIND_POINT_COMPUTE
      = showString "VK_PIPELINE_BIND_POINT_COMPUTE"
    showsPrec p (VkPipelineBindPoint x)
      = showParen (p >= 11)
          (showString "VkPipelineBindPoint " . showsPrec 11 x)

instance Read VkPipelineBindPoint where
    readPrec
      = parens
          (choose
             [("VK_PIPELINE_BIND_POINT_GRAPHICS",
               pure VK_PIPELINE_BIND_POINT_GRAPHICS),
              ("VK_PIPELINE_BIND_POINT_COMPUTE",
               pure VK_PIPELINE_BIND_POINT_COMPUTE)]
             +++
             prec 10
               (expectP (Ident "VkPipelineBindPoint") >>
                  (VkPipelineBindPoint <$> step readPrec)))

pattern VK_PIPELINE_BIND_POINT_GRAPHICS :: VkPipelineBindPoint

pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0

pattern VK_PIPELINE_BIND_POINT_COMPUTE :: VkPipelineBindPoint

pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1

newtype VkPipelineCacheCreateBitmask (a ::
                                        FlagType) = VkPipelineCacheCreateBitmask VkFlags
                                                    deriving (Eq, Ord, Storable)

type VkPipelineCacheCreateFlags =
     VkPipelineCacheCreateBitmask FlagMask

type VkPipelineCacheCreateFlagBits =
     VkPipelineCacheCreateBitmask FlagBit

pattern VkPipelineCacheCreateFlagBits ::
        VkFlags -> VkPipelineCacheCreateBitmask FlagBit

pattern VkPipelineCacheCreateFlagBits n =
        VkPipelineCacheCreateBitmask n

pattern VkPipelineCacheCreateFlags ::
        VkFlags -> VkPipelineCacheCreateBitmask FlagMask

pattern VkPipelineCacheCreateFlags n =
        VkPipelineCacheCreateBitmask n

deriving instance Bits (VkPipelineCacheCreateBitmask FlagMask)

deriving instance
         FiniteBits (VkPipelineCacheCreateBitmask FlagMask)

instance Show (VkPipelineCacheCreateBitmask a) where
    showsPrec p (VkPipelineCacheCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkPipelineCacheCreateBitmask " . showsPrec 11 x)

instance Read (VkPipelineCacheCreateBitmask a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkPipelineCacheCreateBitmask") >>
                  (VkPipelineCacheCreateBitmask <$> step readPrec)))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineCacheHeaderVersion VkPipelineCacheHeaderVersion registry at www.khronos.org>
newtype VkPipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion Int32
                                       deriving (Eq, Ord, Enum, Storable)

instance Show VkPipelineCacheHeaderVersion where
    showsPrec _ VK_PIPELINE_CACHE_HEADER_VERSION_ONE
      = showString "VK_PIPELINE_CACHE_HEADER_VERSION_ONE"
    showsPrec p (VkPipelineCacheHeaderVersion x)
      = showParen (p >= 11)
          (showString "VkPipelineCacheHeaderVersion " . showsPrec 11 x)

instance Read VkPipelineCacheHeaderVersion where
    readPrec
      = parens
          (choose
             [("VK_PIPELINE_CACHE_HEADER_VERSION_ONE",
               pure VK_PIPELINE_CACHE_HEADER_VERSION_ONE)]
             +++
             prec 10
               (expectP (Ident "VkPipelineCacheHeaderVersion") >>
                  (VkPipelineCacheHeaderVersion <$> step readPrec)))

pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE ::
        VkPipelineCacheHeaderVersion

pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE =
        VkPipelineCacheHeaderVersion 1

newtype VkPipelineCompilerControlBitmaskAMD (a ::
                                               FlagType) = VkPipelineCompilerControlBitmaskAMD VkFlags
                                                           deriving (Eq, Ord, Storable)

type VkPipelineCompilerControlFlagsAMD =
     VkPipelineCompilerControlBitmaskAMD FlagMask

type VkPipelineCompilerControlFlagBitsAMD =
     VkPipelineCompilerControlBitmaskAMD FlagBit

pattern VkPipelineCompilerControlFlagBitsAMD ::
        VkFlags -> VkPipelineCompilerControlBitmaskAMD FlagBit

pattern VkPipelineCompilerControlFlagBitsAMD n =
        VkPipelineCompilerControlBitmaskAMD n

pattern VkPipelineCompilerControlFlagsAMD ::
        VkFlags -> VkPipelineCompilerControlBitmaskAMD FlagMask

pattern VkPipelineCompilerControlFlagsAMD n =
        VkPipelineCompilerControlBitmaskAMD n

deriving instance
         Bits (VkPipelineCompilerControlBitmaskAMD FlagMask)

deriving instance
         FiniteBits (VkPipelineCompilerControlBitmaskAMD FlagMask)

instance Show (VkPipelineCompilerControlBitmaskAMD a) where
    showsPrec p (VkPipelineCompilerControlBitmaskAMD x)
      = showParen (p >= 11)
          (showString "VkPipelineCompilerControlBitmaskAMD " .
             showsPrec 11 x)

instance Read (VkPipelineCompilerControlBitmaskAMD a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkPipelineCompilerControlBitmaskAMD") >>
                  (VkPipelineCompilerControlBitmaskAMD <$> step readPrec)))

newtype VkPipelineCreateBitmask (a ::
                                   FlagType) = VkPipelineCreateBitmask VkFlags
                                               deriving (Eq, Ord, Storable)

type VkPipelineCreateFlags = VkPipelineCreateBitmask FlagMask

type VkPipelineCreateFlagBits = VkPipelineCreateBitmask FlagBit

pattern VkPipelineCreateFlagBits ::
        VkFlags -> VkPipelineCreateBitmask FlagBit

pattern VkPipelineCreateFlagBits n = VkPipelineCreateBitmask n

pattern VkPipelineCreateFlags ::
        VkFlags -> VkPipelineCreateBitmask FlagMask

pattern VkPipelineCreateFlags n = VkPipelineCreateBitmask n

deriving instance Bits (VkPipelineCreateBitmask FlagMask)

deriving instance FiniteBits (VkPipelineCreateBitmask FlagMask)

instance Show (VkPipelineCreateBitmask a) where
    showsPrec _ VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
      = showString "VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
    showsPrec _ VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
      = showString "VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
    showsPrec _ VK_PIPELINE_CREATE_DERIVATIVE_BIT
      = showString "VK_PIPELINE_CREATE_DERIVATIVE_BIT"
    showsPrec p (VkPipelineCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkPipelineCreateBitmask " . showsPrec 11 x)

instance Read (VkPipelineCreateBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT",
               pure VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT),
              ("VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT",
               pure VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT),
              ("VK_PIPELINE_CREATE_DERIVATIVE_BIT",
               pure VK_PIPELINE_CREATE_DERIVATIVE_BIT)]
             +++
             prec 10
               (expectP (Ident "VkPipelineCreateBitmask") >>
                  (VkPipelineCreateBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT =
        VkPipelineCreateBitmask 1

-- | bitpos = @1@
pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT =
        VkPipelineCreateBitmask 2

-- | bitpos = @2@
pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT =
        VkPipelineCreateBitmask 4

newtype VkPipelineCreationFeedbackBitmaskEXT (a ::
                                                FlagType) = VkPipelineCreationFeedbackBitmaskEXT VkFlags
                                                            deriving (Eq, Ord, Storable)

type VkPipelineCreationFeedbackFlagsEXT =
     VkPipelineCreationFeedbackBitmaskEXT FlagMask

type VkPipelineCreationFeedbackFlagBitsEXT =
     VkPipelineCreationFeedbackBitmaskEXT FlagBit

pattern VkPipelineCreationFeedbackFlagBitsEXT ::
        VkFlags -> VkPipelineCreationFeedbackBitmaskEXT FlagBit

pattern VkPipelineCreationFeedbackFlagBitsEXT n =
        VkPipelineCreationFeedbackBitmaskEXT n

pattern VkPipelineCreationFeedbackFlagsEXT ::
        VkFlags -> VkPipelineCreationFeedbackBitmaskEXT FlagMask

pattern VkPipelineCreationFeedbackFlagsEXT n =
        VkPipelineCreationFeedbackBitmaskEXT n

deriving instance
         Bits (VkPipelineCreationFeedbackBitmaskEXT FlagMask)

deriving instance
         FiniteBits (VkPipelineCreationFeedbackBitmaskEXT FlagMask)

instance Show (VkPipelineCreationFeedbackBitmaskEXT a) where
    showsPrec _ VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT
      = showString "VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT"
    showsPrec _
      VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
      = showString
          "VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT"
    showsPrec _
      VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
      = showString
          "VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT"
    showsPrec p (VkPipelineCreationFeedbackBitmaskEXT x)
      = showParen (p >= 11)
          (showString "VkPipelineCreationFeedbackBitmaskEXT " .
             showsPrec 11 x)

instance Read (VkPipelineCreationFeedbackBitmaskEXT a) where
    readPrec
      = parens
          (choose
             [("VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT",
               pure VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT),
              ("VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT",
               pure
                 VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT),
              ("VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT",
               pure
                 VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT)]
             +++
             prec 10
               (expectP (Ident "VkPipelineCreationFeedbackBitmaskEXT") >>
                  (VkPipelineCreationFeedbackBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT ::
        VkPipelineCreationFeedbackBitmaskEXT a

pattern VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT =
        VkPipelineCreationFeedbackBitmaskEXT 1

-- | bitpos = @1@
pattern VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
        :: VkPipelineCreationFeedbackBitmaskEXT a

pattern VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
        = VkPipelineCreationFeedbackBitmaskEXT 2

-- | bitpos = @2@
pattern VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
        :: VkPipelineCreationFeedbackBitmaskEXT a

pattern VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
        = VkPipelineCreationFeedbackBitmaskEXT 4

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineExecutableStatisticFormatKHR VkPipelineExecutableStatisticFormatKHR registry at www.khronos.org>
newtype VkPipelineExecutableStatisticFormatKHR = VkPipelineExecutableStatisticFormatKHR Int32
                                                 deriving (Eq, Ord, Enum, Storable)

instance Show VkPipelineExecutableStatisticFormatKHR where
    showsPrec _ VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR
      = showString "VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR"
    showsPrec _ VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR
      = showString "VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR"
    showsPrec _ VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR
      = showString "VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR"
    showsPrec _ VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR
      = showString "VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR"
    showsPrec p (VkPipelineExecutableStatisticFormatKHR x)
      = showParen (p >= 11)
          (showString "VkPipelineExecutableStatisticFormatKHR " .
             showsPrec 11 x)

instance Read VkPipelineExecutableStatisticFormatKHR where
    readPrec
      = parens
          (choose
             [("VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR",
               pure VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR),
              ("VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR",
               pure VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR),
              ("VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR",
               pure VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR),
              ("VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR",
               pure VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR)]
             +++
             prec 10
               (expectP (Ident "VkPipelineExecutableStatisticFormatKHR") >>
                  (VkPipelineExecutableStatisticFormatKHR <$> step readPrec)))

pattern VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR ::
        VkPipelineExecutableStatisticFormatKHR

pattern VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR =
        VkPipelineExecutableStatisticFormatKHR 0

pattern VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR ::
        VkPipelineExecutableStatisticFormatKHR

pattern VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR =
        VkPipelineExecutableStatisticFormatKHR 1

pattern VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR ::
        VkPipelineExecutableStatisticFormatKHR

pattern VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR =
        VkPipelineExecutableStatisticFormatKHR 2

pattern VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR ::
        VkPipelineExecutableStatisticFormatKHR

pattern VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR =
        VkPipelineExecutableStatisticFormatKHR 3

newtype VkPipelineShaderStageCreateBitmask (a ::
                                              FlagType) = VkPipelineShaderStageCreateBitmask VkFlags
                                                          deriving (Eq, Ord, Storable)

type VkPipelineShaderStageCreateFlags =
     VkPipelineShaderStageCreateBitmask FlagMask

type VkPipelineShaderStageCreateFlagBits =
     VkPipelineShaderStageCreateBitmask FlagBit

pattern VkPipelineShaderStageCreateFlagBits ::
        VkFlags -> VkPipelineShaderStageCreateBitmask FlagBit

pattern VkPipelineShaderStageCreateFlagBits n =
        VkPipelineShaderStageCreateBitmask n

pattern VkPipelineShaderStageCreateFlags ::
        VkFlags -> VkPipelineShaderStageCreateBitmask FlagMask

pattern VkPipelineShaderStageCreateFlags n =
        VkPipelineShaderStageCreateBitmask n

deriving instance
         Bits (VkPipelineShaderStageCreateBitmask FlagMask)

deriving instance
         FiniteBits (VkPipelineShaderStageCreateBitmask FlagMask)

instance Show (VkPipelineShaderStageCreateBitmask a) where
    showsPrec p (VkPipelineShaderStageCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkPipelineShaderStageCreateBitmask " . showsPrec 11 x)

instance Read (VkPipelineShaderStageCreateBitmask a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkPipelineShaderStageCreateBitmask") >>
                  (VkPipelineShaderStageCreateBitmask <$> step readPrec)))

newtype VkPipelineStageBitmask (a ::
                                  FlagType) = VkPipelineStageBitmask VkFlags
                                              deriving (Eq, Ord, Storable)

type VkPipelineStageFlags = VkPipelineStageBitmask FlagMask

type VkPipelineStageFlagBits = VkPipelineStageBitmask FlagBit

pattern VkPipelineStageFlagBits ::
        VkFlags -> VkPipelineStageBitmask FlagBit

pattern VkPipelineStageFlagBits n = VkPipelineStageBitmask n

pattern VkPipelineStageFlags ::
        VkFlags -> VkPipelineStageBitmask FlagMask

pattern VkPipelineStageFlags n = VkPipelineStageBitmask n

deriving instance Bits (VkPipelineStageBitmask FlagMask)

deriving instance FiniteBits (VkPipelineStageBitmask FlagMask)

instance Show (VkPipelineStageBitmask a) where
    showsPrec _ VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
      = showString "VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT"
    showsPrec _ VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT
      = showString "VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT"
    showsPrec _ VK_PIPELINE_STAGE_VERTEX_INPUT_BIT
      = showString "VK_PIPELINE_STAGE_VERTEX_INPUT_BIT"
    showsPrec _ VK_PIPELINE_STAGE_VERTEX_SHADER_BIT
      = showString "VK_PIPELINE_STAGE_VERTEX_SHADER_BIT"
    showsPrec _ VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
      = showString "VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT"
    showsPrec _ VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
      = showString "VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT"
    showsPrec _ VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
      = showString "VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT"
    showsPrec _ VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
      = showString "VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT"
    showsPrec _ VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
      = showString "VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT"
    showsPrec _ VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
      = showString "VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT"
    showsPrec _ VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      = showString "VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT"
    showsPrec _ VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT
      = showString "VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT"
    showsPrec _ VK_PIPELINE_STAGE_TRANSFER_BIT
      = showString "VK_PIPELINE_STAGE_TRANSFER_BIT"
    showsPrec _ VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
      = showString "VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT"
    showsPrec _ VK_PIPELINE_STAGE_HOST_BIT
      = showString "VK_PIPELINE_STAGE_HOST_BIT"
    showsPrec _ VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT
      = showString "VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT"
    showsPrec _ VK_PIPELINE_STAGE_ALL_COMMANDS_BIT
      = showString "VK_PIPELINE_STAGE_ALL_COMMANDS_BIT"
    showsPrec p (VkPipelineStageBitmask x)
      = showParen (p >= 11)
          (showString "VkPipelineStageBitmask " . showsPrec 11 x)

instance Read (VkPipelineStageBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT",
               pure VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
              ("VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT",
               pure VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
              ("VK_PIPELINE_STAGE_VERTEX_INPUT_BIT",
               pure VK_PIPELINE_STAGE_VERTEX_INPUT_BIT),
              ("VK_PIPELINE_STAGE_VERTEX_SHADER_BIT",
               pure VK_PIPELINE_STAGE_VERTEX_SHADER_BIT),
              ("VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT",
               pure VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT),
              ("VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT",
               pure VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT),
              ("VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT",
               pure VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT),
              ("VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT",
               pure VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
              ("VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT",
               pure VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT),
              ("VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT",
               pure VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),
              ("VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT",
               pure VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
              ("VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT",
               pure VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
              ("VK_PIPELINE_STAGE_TRANSFER_BIT",
               pure VK_PIPELINE_STAGE_TRANSFER_BIT),
              ("VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT",
               pure VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
              ("VK_PIPELINE_STAGE_HOST_BIT", pure VK_PIPELINE_STAGE_HOST_BIT),
              ("VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT",
               pure VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT),
              ("VK_PIPELINE_STAGE_ALL_COMMANDS_BIT",
               pure VK_PIPELINE_STAGE_ALL_COMMANDS_BIT)]
             +++
             prec 10
               (expectP (Ident "VkPipelineStageBitmask") >>
                  (VkPipelineStageBitmask <$> step readPrec)))

-- | Before subsequent commands are processed
--
--   bitpos = @0@
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT =
        VkPipelineStageBitmask 1

-- | Draw/DispatchIndirect command fetch
--
--   bitpos = @1@
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT =
        VkPipelineStageBitmask 2

-- | Vertex/index fetch
--
--   bitpos = @2@
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT =
        VkPipelineStageBitmask 4

-- | Vertex shading
--
--   bitpos = @3@
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT =
        VkPipelineStageBitmask 8

-- | Tessellation control shading
--
--   bitpos = @4@
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT =
        VkPipelineStageBitmask 16

-- | Tessellation evaluation shading
--
--   bitpos = @5@
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT =
        VkPipelineStageBitmask 32

-- | Geometry shading
--
--   bitpos = @6@
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT =
        VkPipelineStageBitmask 64

-- | Fragment shading
--
--   bitpos = @7@
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT =
        VkPipelineStageBitmask 128

-- | Early fragment (depth and stencil) tests
--
--   bitpos = @8@
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT =
        VkPipelineStageBitmask 256

-- | Late fragment (depth and stencil) tests
--
--   bitpos = @9@
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT =
        VkPipelineStageBitmask 512

-- | Color attachment writes
--
--   bitpos = @10@
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT =
        VkPipelineStageBitmask 1024

-- | Compute shading
--
--   bitpos = @11@
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT =
        VkPipelineStageBitmask 2048

-- | Transfer/copy operations
--
--   bitpos = @12@
pattern VK_PIPELINE_STAGE_TRANSFER_BIT :: VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_TRANSFER_BIT =
        VkPipelineStageBitmask 4096

-- | After previous commands have completed
--
--   bitpos = @13@
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT =
        VkPipelineStageBitmask 8192

-- | Indicates host (CPU) is a source/sink of the dependency
--
--   bitpos = @14@
pattern VK_PIPELINE_STAGE_HOST_BIT :: VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_HOST_BIT = VkPipelineStageBitmask 16384

-- | All stages of the graphics pipeline
--
--   bitpos = @15@
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT =
        VkPipelineStageBitmask 32768

-- | All stages supported on the queue
--
--   bitpos = @16@
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT =
        VkPipelineStageBitmask 65536
