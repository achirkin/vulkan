{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Performance
       (VkPerformanceConfigurationTypeINTEL(VkPerformanceConfigurationTypeINTEL,
                                            VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL),
        VkPerformanceCounterDescriptionBitmaskKHR(VkPerformanceCounterDescriptionBitmaskKHR,
                                                  VkPerformanceCounterDescriptionFlagsKHR,
                                                  VkPerformanceCounterDescriptionFlagBitsKHR,
                                                  VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR,
                                                  VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR),
        VkPerformanceCounterDescriptionFlagsKHR,
        VkPerformanceCounterDescriptionFlagBitsKHR,
        pattern VK_QUERY_SCOPE_COMMAND_BUFFER_KHR,
        pattern VK_QUERY_SCOPE_RENDER_PASS_KHR,
        pattern VK_QUERY_SCOPE_COMMAND_KHR,
        VkPerformanceCounterScopeKHR(VkPerformanceCounterScopeKHR,
                                     VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR,
                                     VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR,
                                     VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR),
        VkPerformanceCounterStorageKHR(VkPerformanceCounterStorageKHR,
                                       VK_PERFORMANCE_COUNTER_STORAGE_INT32_KHR,
                                       VK_PERFORMANCE_COUNTER_STORAGE_INT64_KHR,
                                       VK_PERFORMANCE_COUNTER_STORAGE_UINT32_KHR,
                                       VK_PERFORMANCE_COUNTER_STORAGE_UINT64_KHR,
                                       VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR,
                                       VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR),
        VkPerformanceCounterUnitKHR(VkPerformanceCounterUnitKHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_GENERIC_KHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_BYTES_KHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_KELVIN_KHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_WATTS_KHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_VOLTS_KHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_AMPS_KHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_HERTZ_KHR,
                                    VK_PERFORMANCE_COUNTER_UNIT_CYCLES_KHR),
        VkPerformanceOverrideTypeINTEL(VkPerformanceOverrideTypeINTEL,
                                       VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL,
                                       VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL),
        VkPerformanceParameterTypeINTEL(VkPerformanceParameterTypeINTEL,
                                        VK_PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL,
                                        VK_PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL),
        VkPerformanceValueTypeINTEL(VkPerformanceValueTypeINTEL,
                                    VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL,
                                    VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL,
                                    VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL,
                                    VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL,
                                    VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceConfigurationTypeINTEL VkPerformanceConfigurationTypeINTEL registry at www.khronos.org>
newtype VkPerformanceConfigurationTypeINTEL = VkPerformanceConfigurationTypeINTEL Int32
                                              deriving (Eq, Ord, Enum, Storable)

instance Show VkPerformanceConfigurationTypeINTEL where
    showsPrec _
      VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL
      = showString
          "VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL"
    showsPrec p (VkPerformanceConfigurationTypeINTEL x)
      = showParen (p >= 11)
          (showString "VkPerformanceConfigurationTypeINTEL " .
             showsPrec 11 x)

instance Read VkPerformanceConfigurationTypeINTEL where
    readPrec
      = parens
          (choose
             [("VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL",
               pure
                 VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL)]
             +++
             prec 10
               (expectP (Ident "VkPerformanceConfigurationTypeINTEL") >>
                  (VkPerformanceConfigurationTypeINTEL <$> step readPrec)))

pattern VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL
        :: VkPerformanceConfigurationTypeINTEL

pattern VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL
        = VkPerformanceConfigurationTypeINTEL 0

newtype VkPerformanceCounterDescriptionBitmaskKHR (a ::
                                                     FlagType) = VkPerformanceCounterDescriptionBitmaskKHR VkFlags
                                                                 deriving (Eq, Ord, Storable)

type VkPerformanceCounterDescriptionFlagsKHR =
     VkPerformanceCounterDescriptionBitmaskKHR FlagMask

type VkPerformanceCounterDescriptionFlagBitsKHR =
     VkPerformanceCounterDescriptionBitmaskKHR FlagBit

pattern VkPerformanceCounterDescriptionFlagBitsKHR ::
        VkFlags -> VkPerformanceCounterDescriptionBitmaskKHR FlagBit

pattern VkPerformanceCounterDescriptionFlagBitsKHR n =
        VkPerformanceCounterDescriptionBitmaskKHR n

pattern VkPerformanceCounterDescriptionFlagsKHR ::
        VkFlags -> VkPerformanceCounterDescriptionBitmaskKHR FlagMask

pattern VkPerformanceCounterDescriptionFlagsKHR n =
        VkPerformanceCounterDescriptionBitmaskKHR n

deriving instance
         Bits (VkPerformanceCounterDescriptionBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkPerformanceCounterDescriptionBitmaskKHR FlagMask)

instance Show (VkPerformanceCounterDescriptionBitmaskKHR a) where
    showsPrec _
      VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR
      = showString
          "VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR"
    showsPrec _
      VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR
      = showString
          "VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR"
    showsPrec p (VkPerformanceCounterDescriptionBitmaskKHR x)
      = showParen (p >= 11)
          (showString "VkPerformanceCounterDescriptionBitmaskKHR " .
             showsPrec 11 x)

instance Read (VkPerformanceCounterDescriptionBitmaskKHR a) where
    readPrec
      = parens
          (choose
             [("VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR",
               pure VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR),
              ("VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR",
               pure VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR)]
             +++
             prec 10
               (expectP (Ident "VkPerformanceCounterDescriptionBitmaskKHR") >>
                  (VkPerformanceCounterDescriptionBitmaskKHR <$> step readPrec)))

-- | bitpos = @0@
pattern VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR
        :: VkPerformanceCounterDescriptionBitmaskKHR a

pattern VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR
        = VkPerformanceCounterDescriptionBitmaskKHR 1

-- | bitpos = @1@
pattern VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR
        :: VkPerformanceCounterDescriptionBitmaskKHR a

pattern VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR
        = VkPerformanceCounterDescriptionBitmaskKHR 2

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceCounterScopeKHR VkPerformanceCounterScopeKHR registry at www.khronos.org>
newtype VkPerformanceCounterScopeKHR = VkPerformanceCounterScopeKHR Int32
                                       deriving (Eq, Ord, Enum, Storable)

instance Show VkPerformanceCounterScopeKHR where
    showsPrec _ VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR
      = showString "VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR
      = showString "VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR
      = showString "VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR"
    showsPrec _ VK_QUERY_SCOPE_COMMAND_BUFFER_KHR
      = showString "VK_QUERY_SCOPE_COMMAND_BUFFER_KHR"
    showsPrec _ VK_QUERY_SCOPE_RENDER_PASS_KHR
      = showString "VK_QUERY_SCOPE_RENDER_PASS_KHR"
    showsPrec _ VK_QUERY_SCOPE_COMMAND_KHR
      = showString "VK_QUERY_SCOPE_COMMAND_KHR"
    showsPrec p (VkPerformanceCounterScopeKHR x)
      = showParen (p >= 11)
          (showString "VkPerformanceCounterScopeKHR " . showsPrec 11 x)

instance Read VkPerformanceCounterScopeKHR where
    readPrec
      = parens
          (choose
             [("VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR",
               pure VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR),
              ("VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR",
               pure VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR),
              ("VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR",
               pure VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR),
              ("VK_QUERY_SCOPE_COMMAND_BUFFER_KHR",
               pure VK_QUERY_SCOPE_COMMAND_BUFFER_KHR),
              ("VK_QUERY_SCOPE_RENDER_PASS_KHR",
               pure VK_QUERY_SCOPE_RENDER_PASS_KHR),
              ("VK_QUERY_SCOPE_COMMAND_KHR", pure VK_QUERY_SCOPE_COMMAND_KHR)]
             +++
             prec 10
               (expectP (Ident "VkPerformanceCounterScopeKHR") >>
                  (VkPerformanceCounterScopeKHR <$> step readPrec)))

pattern VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR ::
        VkPerformanceCounterScopeKHR

pattern VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR =
        VkPerformanceCounterScopeKHR 0

pattern VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR ::
        VkPerformanceCounterScopeKHR

pattern VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR =
        VkPerformanceCounterScopeKHR 1

pattern VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR ::
        VkPerformanceCounterScopeKHR

pattern VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR =
        VkPerformanceCounterScopeKHR 2

pattern VK_QUERY_SCOPE_COMMAND_BUFFER_KHR =
        VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR

pattern VK_QUERY_SCOPE_RENDER_PASS_KHR =
        VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR

pattern VK_QUERY_SCOPE_COMMAND_KHR =
        VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceCounterStorageKHR VkPerformanceCounterStorageKHR registry at www.khronos.org>
newtype VkPerformanceCounterStorageKHR = VkPerformanceCounterStorageKHR Int32
                                         deriving (Eq, Ord, Enum, Storable)

instance Show VkPerformanceCounterStorageKHR where
    showsPrec _ VK_PERFORMANCE_COUNTER_STORAGE_INT32_KHR
      = showString "VK_PERFORMANCE_COUNTER_STORAGE_INT32_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_STORAGE_INT64_KHR
      = showString "VK_PERFORMANCE_COUNTER_STORAGE_INT64_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_STORAGE_UINT32_KHR
      = showString "VK_PERFORMANCE_COUNTER_STORAGE_UINT32_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_STORAGE_UINT64_KHR
      = showString "VK_PERFORMANCE_COUNTER_STORAGE_UINT64_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR
      = showString "VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR
      = showString "VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR"
    showsPrec p (VkPerformanceCounterStorageKHR x)
      = showParen (p >= 11)
          (showString "VkPerformanceCounterStorageKHR " . showsPrec 11 x)

instance Read VkPerformanceCounterStorageKHR where
    readPrec
      = parens
          (choose
             [("VK_PERFORMANCE_COUNTER_STORAGE_INT32_KHR",
               pure VK_PERFORMANCE_COUNTER_STORAGE_INT32_KHR),
              ("VK_PERFORMANCE_COUNTER_STORAGE_INT64_KHR",
               pure VK_PERFORMANCE_COUNTER_STORAGE_INT64_KHR),
              ("VK_PERFORMANCE_COUNTER_STORAGE_UINT32_KHR",
               pure VK_PERFORMANCE_COUNTER_STORAGE_UINT32_KHR),
              ("VK_PERFORMANCE_COUNTER_STORAGE_UINT64_KHR",
               pure VK_PERFORMANCE_COUNTER_STORAGE_UINT64_KHR),
              ("VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR",
               pure VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR),
              ("VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR",
               pure VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR)]
             +++
             prec 10
               (expectP (Ident "VkPerformanceCounterStorageKHR") >>
                  (VkPerformanceCounterStorageKHR <$> step readPrec)))

pattern VK_PERFORMANCE_COUNTER_STORAGE_INT32_KHR ::
        VkPerformanceCounterStorageKHR

pattern VK_PERFORMANCE_COUNTER_STORAGE_INT32_KHR =
        VkPerformanceCounterStorageKHR 0

pattern VK_PERFORMANCE_COUNTER_STORAGE_INT64_KHR ::
        VkPerformanceCounterStorageKHR

pattern VK_PERFORMANCE_COUNTER_STORAGE_INT64_KHR =
        VkPerformanceCounterStorageKHR 1

pattern VK_PERFORMANCE_COUNTER_STORAGE_UINT32_KHR ::
        VkPerformanceCounterStorageKHR

pattern VK_PERFORMANCE_COUNTER_STORAGE_UINT32_KHR =
        VkPerformanceCounterStorageKHR 2

pattern VK_PERFORMANCE_COUNTER_STORAGE_UINT64_KHR ::
        VkPerformanceCounterStorageKHR

pattern VK_PERFORMANCE_COUNTER_STORAGE_UINT64_KHR =
        VkPerformanceCounterStorageKHR 3

pattern VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR ::
        VkPerformanceCounterStorageKHR

pattern VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR =
        VkPerformanceCounterStorageKHR 4

pattern VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR ::
        VkPerformanceCounterStorageKHR

pattern VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR =
        VkPerformanceCounterStorageKHR 5

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceCounterUnitKHR VkPerformanceCounterUnitKHR registry at www.khronos.org>
newtype VkPerformanceCounterUnitKHR = VkPerformanceCounterUnitKHR Int32
                                      deriving (Eq, Ord, Enum, Storable)

instance Show VkPerformanceCounterUnitKHR where
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_GENERIC_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_GENERIC_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_BYTES_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_BYTES_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_KELVIN_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_KELVIN_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_WATTS_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_WATTS_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_VOLTS_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_VOLTS_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_AMPS_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_AMPS_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_HERTZ_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_HERTZ_KHR"
    showsPrec _ VK_PERFORMANCE_COUNTER_UNIT_CYCLES_KHR
      = showString "VK_PERFORMANCE_COUNTER_UNIT_CYCLES_KHR"
    showsPrec p (VkPerformanceCounterUnitKHR x)
      = showParen (p >= 11)
          (showString "VkPerformanceCounterUnitKHR " . showsPrec 11 x)

instance Read VkPerformanceCounterUnitKHR where
    readPrec
      = parens
          (choose
             [("VK_PERFORMANCE_COUNTER_UNIT_GENERIC_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_GENERIC_KHR),
              ("VK_PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR),
              ("VK_PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR),
              ("VK_PERFORMANCE_COUNTER_UNIT_BYTES_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_BYTES_KHR),
              ("VK_PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR),
              ("VK_PERFORMANCE_COUNTER_UNIT_KELVIN_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_KELVIN_KHR),
              ("VK_PERFORMANCE_COUNTER_UNIT_WATTS_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_WATTS_KHR),
              ("VK_PERFORMANCE_COUNTER_UNIT_VOLTS_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_VOLTS_KHR),
              ("VK_PERFORMANCE_COUNTER_UNIT_AMPS_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_AMPS_KHR),
              ("VK_PERFORMANCE_COUNTER_UNIT_HERTZ_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_HERTZ_KHR),
              ("VK_PERFORMANCE_COUNTER_UNIT_CYCLES_KHR",
               pure VK_PERFORMANCE_COUNTER_UNIT_CYCLES_KHR)]
             +++
             prec 10
               (expectP (Ident "VkPerformanceCounterUnitKHR") >>
                  (VkPerformanceCounterUnitKHR <$> step readPrec)))

pattern VK_PERFORMANCE_COUNTER_UNIT_GENERIC_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_GENERIC_KHR =
        VkPerformanceCounterUnitKHR 0

pattern VK_PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR =
        VkPerformanceCounterUnitKHR 1

pattern VK_PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR =
        VkPerformanceCounterUnitKHR 2

pattern VK_PERFORMANCE_COUNTER_UNIT_BYTES_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_BYTES_KHR =
        VkPerformanceCounterUnitKHR 3

pattern VK_PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR =
        VkPerformanceCounterUnitKHR 4

pattern VK_PERFORMANCE_COUNTER_UNIT_KELVIN_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_KELVIN_KHR =
        VkPerformanceCounterUnitKHR 5

pattern VK_PERFORMANCE_COUNTER_UNIT_WATTS_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_WATTS_KHR =
        VkPerformanceCounterUnitKHR 6

pattern VK_PERFORMANCE_COUNTER_UNIT_VOLTS_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_VOLTS_KHR =
        VkPerformanceCounterUnitKHR 7

pattern VK_PERFORMANCE_COUNTER_UNIT_AMPS_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_AMPS_KHR =
        VkPerformanceCounterUnitKHR 8

pattern VK_PERFORMANCE_COUNTER_UNIT_HERTZ_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_HERTZ_KHR =
        VkPerformanceCounterUnitKHR 9

pattern VK_PERFORMANCE_COUNTER_UNIT_CYCLES_KHR ::
        VkPerformanceCounterUnitKHR

pattern VK_PERFORMANCE_COUNTER_UNIT_CYCLES_KHR =
        VkPerformanceCounterUnitKHR 10

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceOverrideTypeINTEL VkPerformanceOverrideTypeINTEL registry at www.khronos.org>
newtype VkPerformanceOverrideTypeINTEL = VkPerformanceOverrideTypeINTEL Int32
                                         deriving (Eq, Ord, Enum, Storable)

instance Show VkPerformanceOverrideTypeINTEL where
    showsPrec _ VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL
      = showString "VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL"
    showsPrec _ VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL
      = showString "VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL"
    showsPrec p (VkPerformanceOverrideTypeINTEL x)
      = showParen (p >= 11)
          (showString "VkPerformanceOverrideTypeINTEL " . showsPrec 11 x)

instance Read VkPerformanceOverrideTypeINTEL where
    readPrec
      = parens
          (choose
             [("VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL",
               pure VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL),
              ("VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL",
               pure VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL)]
             +++
             prec 10
               (expectP (Ident "VkPerformanceOverrideTypeINTEL") >>
                  (VkPerformanceOverrideTypeINTEL <$> step readPrec)))

pattern VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL ::
        VkPerformanceOverrideTypeINTEL

pattern VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL =
        VkPerformanceOverrideTypeINTEL 0

pattern VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL ::
        VkPerformanceOverrideTypeINTEL

pattern VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL =
        VkPerformanceOverrideTypeINTEL 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceParameterTypeINTEL VkPerformanceParameterTypeINTEL registry at www.khronos.org>
newtype VkPerformanceParameterTypeINTEL = VkPerformanceParameterTypeINTEL Int32
                                          deriving (Eq, Ord, Enum, Storable)

instance Show VkPerformanceParameterTypeINTEL where
    showsPrec _
      VK_PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL
      = showString
          "VK_PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL"
    showsPrec _
      VK_PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL
      = showString
          "VK_PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL"
    showsPrec p (VkPerformanceParameterTypeINTEL x)
      = showParen (p >= 11)
          (showString "VkPerformanceParameterTypeINTEL " . showsPrec 11 x)

instance Read VkPerformanceParameterTypeINTEL where
    readPrec
      = parens
          (choose
             [("VK_PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL",
               pure VK_PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL),
              ("VK_PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL",
               pure VK_PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL)]
             +++
             prec 10
               (expectP (Ident "VkPerformanceParameterTypeINTEL") >>
                  (VkPerformanceParameterTypeINTEL <$> step readPrec)))

pattern VK_PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL
        :: VkPerformanceParameterTypeINTEL

pattern VK_PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL =
        VkPerformanceParameterTypeINTEL 0

pattern VK_PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL
        :: VkPerformanceParameterTypeINTEL

pattern VK_PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL
        = VkPerformanceParameterTypeINTEL 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPerformanceValueTypeINTEL VkPerformanceValueTypeINTEL registry at www.khronos.org>
newtype VkPerformanceValueTypeINTEL = VkPerformanceValueTypeINTEL Int32
                                      deriving (Eq, Ord, Enum, Storable)

instance Show VkPerformanceValueTypeINTEL where
    showsPrec _ VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL
      = showString "VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL"
    showsPrec _ VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL
      = showString "VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL"
    showsPrec _ VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL
      = showString "VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL"
    showsPrec _ VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL
      = showString "VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL"
    showsPrec _ VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL
      = showString "VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL"
    showsPrec p (VkPerformanceValueTypeINTEL x)
      = showParen (p >= 11)
          (showString "VkPerformanceValueTypeINTEL " . showsPrec 11 x)

instance Read VkPerformanceValueTypeINTEL where
    readPrec
      = parens
          (choose
             [("VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL",
               pure VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL),
              ("VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL",
               pure VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL),
              ("VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL",
               pure VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL),
              ("VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL",
               pure VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL),
              ("VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL",
               pure VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL)]
             +++
             prec 10
               (expectP (Ident "VkPerformanceValueTypeINTEL") >>
                  (VkPerformanceValueTypeINTEL <$> step readPrec)))

pattern VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL ::
        VkPerformanceValueTypeINTEL

pattern VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL =
        VkPerformanceValueTypeINTEL 0

pattern VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL ::
        VkPerformanceValueTypeINTEL

pattern VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL =
        VkPerformanceValueTypeINTEL 1

pattern VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL ::
        VkPerformanceValueTypeINTEL

pattern VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL =
        VkPerformanceValueTypeINTEL 2

pattern VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL ::
        VkPerformanceValueTypeINTEL

pattern VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL =
        VkPerformanceValueTypeINTEL 3

pattern VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL ::
        VkPerformanceValueTypeINTEL

pattern VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL =
        VkPerformanceValueTypeINTEL 4
