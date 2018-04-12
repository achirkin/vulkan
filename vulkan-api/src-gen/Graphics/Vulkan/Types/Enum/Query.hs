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
module Graphics.Vulkan.Types.Enum.Query
       (VkQueryControlBitmask(VkQueryControlBitmask, VkQueryControlFlags,
                              VkQueryControlFlagBits, VK_QUERY_CONTROL_PRECISE_BIT),
        VkQueryControlFlags, VkQueryControlFlagBits,
        VkQueryPipelineStatisticBitmask(VkQueryPipelineStatisticBitmask,
                                        VkQueryPipelineStatisticFlags,
                                        VkQueryPipelineStatisticFlagBits,
                                        VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT,
                                        VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT,
                                        VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT,
                                        VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT,
                                        VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT,
                                        VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT,
                                        VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT,
                                        VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT,
                                        VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT,
                                        VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT,
                                        VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT),
        VkQueryPipelineStatisticFlags, VkQueryPipelineStatisticFlagBits,
        VkQueryPoolCreateFlagBits(..),
        VkQueryResultBitmask(VkQueryResultBitmask, VkQueryResultFlags,
                             VkQueryResultFlagBits, VK_QUERY_RESULT_64_BIT,
                             VK_QUERY_RESULT_WAIT_BIT, VK_QUERY_RESULT_WITH_AVAILABILITY_BIT,
                             VK_QUERY_RESULT_PARTIAL_BIT),
        VkQueryResultFlags, VkQueryResultFlagBits,
        VkQueryType(VkQueryType, VK_QUERY_TYPE_OCCLUSION,
                    VK_QUERY_TYPE_PIPELINE_STATISTICS, VK_QUERY_TYPE_TIMESTAMP))
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkQueryControlBitmask (a ::
                                 FlagType) = VkQueryControlBitmask VkFlags
                                               deriving (Eq, Ord, Storable, Data, Generic)

type VkQueryControlFlags = VkQueryControlBitmask FlagMask

type VkQueryControlFlagBits = VkQueryControlBitmask FlagBit

pattern VkQueryControlFlagBits ::
        VkFlags -> VkQueryControlBitmask FlagBit

pattern VkQueryControlFlagBits n = VkQueryControlBitmask n

pattern VkQueryControlFlags ::
        VkFlags -> VkQueryControlBitmask FlagMask

pattern VkQueryControlFlags n = VkQueryControlBitmask n

deriving instance Bits (VkQueryControlBitmask FlagMask)

deriving instance FiniteBits (VkQueryControlBitmask FlagMask)

deriving instance Integral (VkQueryControlBitmask FlagMask)

deriving instance Num (VkQueryControlBitmask FlagMask)

deriving instance Bounded (VkQueryControlBitmask FlagMask)

deriving instance Enum (VkQueryControlBitmask FlagMask)

deriving instance Real (VkQueryControlBitmask FlagMask)

instance Show (VkQueryControlBitmask a) where
        showsPrec _ VK_QUERY_CONTROL_PRECISE_BIT
          = showString "VK_QUERY_CONTROL_PRECISE_BIT"
        showsPrec p (VkQueryControlBitmask x)
          = showParen (p >= 11)
              (showString "VkQueryControlBitmask " . showsPrec 11 x)

instance Read (VkQueryControlBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_CONTROL_PRECISE_BIT",
                   pure VK_QUERY_CONTROL_PRECISE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryControlBitmask") >>
                      (VkQueryControlBitmask <$> step readPrec)))

-- | Require precise results to be collected by the query
--
--   bitpos = @0@
pattern VK_QUERY_CONTROL_PRECISE_BIT :: VkQueryControlBitmask a

pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlBitmask 1

newtype VkQueryPipelineStatisticBitmask (a ::
                                           FlagType) = VkQueryPipelineStatisticBitmask VkFlags
                                                         deriving (Eq, Ord, Storable, Data, Generic)

type VkQueryPipelineStatisticFlags =
     VkQueryPipelineStatisticBitmask FlagMask

type VkQueryPipelineStatisticFlagBits =
     VkQueryPipelineStatisticBitmask FlagBit

pattern VkQueryPipelineStatisticFlagBits ::
        VkFlags -> VkQueryPipelineStatisticBitmask FlagBit

pattern VkQueryPipelineStatisticFlagBits n =
        VkQueryPipelineStatisticBitmask n

pattern VkQueryPipelineStatisticFlags ::
        VkFlags -> VkQueryPipelineStatisticBitmask FlagMask

pattern VkQueryPipelineStatisticFlags n =
        VkQueryPipelineStatisticBitmask n

deriving instance Bits (VkQueryPipelineStatisticBitmask FlagMask)

deriving instance
         FiniteBits (VkQueryPipelineStatisticBitmask FlagMask)

deriving instance
         Integral (VkQueryPipelineStatisticBitmask FlagMask)

deriving instance Num (VkQueryPipelineStatisticBitmask FlagMask)

deriving instance
         Bounded (VkQueryPipelineStatisticBitmask FlagMask)

deriving instance Enum (VkQueryPipelineStatisticBitmask FlagMask)

deriving instance Real (VkQueryPipelineStatisticBitmask FlagMask)

instance Show (VkQueryPipelineStatisticBitmask a) where
        showsPrec _ VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT"
        showsPrec _ VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT
          = showString "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT"
        showsPrec _ VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT
          = showString "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT"
        showsPrec _
          VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
          = showString
              "VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT"
        showsPrec p (VkQueryPipelineStatisticBitmask x)
          = showParen (p >= 11)
              (showString "VkQueryPipelineStatisticBitmask " . showsPrec 11 x)

instance Read (VkQueryPipelineStatisticBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT",
                   pure
                     VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT",
                   pure
                     VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT),
                  ("VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT",
                   pure VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryPipelineStatisticBitmask") >>
                      (VkQueryPipelineStatisticBitmask <$> step readPrec)))

-- | Optional
--
--   bitpos = @0@
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT ::
        VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT =
        VkQueryPipelineStatisticBitmask 1

-- | Optional
--
--   bitpos = @1@
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
        :: VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT =
        VkQueryPipelineStatisticBitmask 2

-- | Optional
--
--   bitpos = @2@
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT =
        VkQueryPipelineStatisticBitmask 4

-- | Optional
--
--   bitpos = @3@
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticBitmask 8

-- | Optional
--
--   bitpos = @4@
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
        :: VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
        = VkQueryPipelineStatisticBitmask 16

-- | Optional
--
--   bitpos = @5@
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT ::
        VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT =
        VkQueryPipelineStatisticBitmask 32

-- | Optional
--
--   bitpos = @6@
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT ::
        VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT =
        VkQueryPipelineStatisticBitmask 64

-- | Optional
--
--   bitpos = @7@
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticBitmask 128

-- | Optional
--
--   bitpos = @8@
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
        :: VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
        = VkQueryPipelineStatisticBitmask 256

-- | Optional
--
--   bitpos = @9@
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticBitmask 512

-- | Optional
--
--   bitpos = @10@
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
        :: VkQueryPipelineStatisticBitmask a

pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
        = VkQueryPipelineStatisticBitmask 1024

newtype VkQueryPoolCreateFlagBits = VkQueryPoolCreateFlagBits VkFlags
                                      deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                FiniteBits, Storable, Real, Data, Generic)

instance Show VkQueryPoolCreateFlagBits where
        {-# INLINE show #-}
        show (VkQueryPoolCreateFlagBits x) = show x

instance Read VkQueryPoolCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkQueryResultBitmask (a ::
                                FlagType) = VkQueryResultBitmask VkFlags
                                              deriving (Eq, Ord, Storable, Data, Generic)

type VkQueryResultFlags = VkQueryResultBitmask FlagMask

type VkQueryResultFlagBits = VkQueryResultBitmask FlagBit

pattern VkQueryResultFlagBits ::
        VkFlags -> VkQueryResultBitmask FlagBit

pattern VkQueryResultFlagBits n = VkQueryResultBitmask n

pattern VkQueryResultFlags ::
        VkFlags -> VkQueryResultBitmask FlagMask

pattern VkQueryResultFlags n = VkQueryResultBitmask n

deriving instance Bits (VkQueryResultBitmask FlagMask)

deriving instance FiniteBits (VkQueryResultBitmask FlagMask)

deriving instance Integral (VkQueryResultBitmask FlagMask)

deriving instance Num (VkQueryResultBitmask FlagMask)

deriving instance Bounded (VkQueryResultBitmask FlagMask)

deriving instance Enum (VkQueryResultBitmask FlagMask)

deriving instance Real (VkQueryResultBitmask FlagMask)

instance Show (VkQueryResultBitmask a) where
        showsPrec _ VK_QUERY_RESULT_64_BIT
          = showString "VK_QUERY_RESULT_64_BIT"
        showsPrec _ VK_QUERY_RESULT_WAIT_BIT
          = showString "VK_QUERY_RESULT_WAIT_BIT"
        showsPrec _ VK_QUERY_RESULT_WITH_AVAILABILITY_BIT
          = showString "VK_QUERY_RESULT_WITH_AVAILABILITY_BIT"
        showsPrec _ VK_QUERY_RESULT_PARTIAL_BIT
          = showString "VK_QUERY_RESULT_PARTIAL_BIT"
        showsPrec p (VkQueryResultBitmask x)
          = showParen (p >= 11)
              (showString "VkQueryResultBitmask " . showsPrec 11 x)

instance Read (VkQueryResultBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_RESULT_64_BIT", pure VK_QUERY_RESULT_64_BIT),
                  ("VK_QUERY_RESULT_WAIT_BIT", pure VK_QUERY_RESULT_WAIT_BIT),
                  ("VK_QUERY_RESULT_WITH_AVAILABILITY_BIT",
                   pure VK_QUERY_RESULT_WITH_AVAILABILITY_BIT),
                  ("VK_QUERY_RESULT_PARTIAL_BIT", pure VK_QUERY_RESULT_PARTIAL_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryResultBitmask") >>
                      (VkQueryResultBitmask <$> step readPrec)))

-- | Results of the queries are written to the destination buffer as 64-bit values
--
--   bitpos = @0@
pattern VK_QUERY_RESULT_64_BIT :: VkQueryResultBitmask a

pattern VK_QUERY_RESULT_64_BIT = VkQueryResultBitmask 1

-- | Results of the queries are waited on before proceeding with the result copy
--
--   bitpos = @1@
pattern VK_QUERY_RESULT_WAIT_BIT :: VkQueryResultBitmask a

pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultBitmask 2

-- | Besides the results of the query, the availability of the results is also written
--
--   bitpos = @2@
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT ::
        VkQueryResultBitmask a

pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT =
        VkQueryResultBitmask 4

-- | Copy the partial results of the query even if the final results are not available
--
--   bitpos = @3@
pattern VK_QUERY_RESULT_PARTIAL_BIT :: VkQueryResultBitmask a

pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultBitmask 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkQueryType VkQueryType registry at www.khronos.org>
newtype VkQueryType = VkQueryType Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkQueryType where
        showsPrec _ VK_QUERY_TYPE_OCCLUSION
          = showString "VK_QUERY_TYPE_OCCLUSION"
        showsPrec _ VK_QUERY_TYPE_PIPELINE_STATISTICS
          = showString "VK_QUERY_TYPE_PIPELINE_STATISTICS"
        showsPrec _ VK_QUERY_TYPE_TIMESTAMP
          = showString "VK_QUERY_TYPE_TIMESTAMP"
        showsPrec p (VkQueryType x)
          = showParen (p >= 11) (showString "VkQueryType " . showsPrec 11 x)

instance Read VkQueryType where
        readPrec
          = parens
              (choose
                 [("VK_QUERY_TYPE_OCCLUSION", pure VK_QUERY_TYPE_OCCLUSION),
                  ("VK_QUERY_TYPE_PIPELINE_STATISTICS",
                   pure VK_QUERY_TYPE_PIPELINE_STATISTICS),
                  ("VK_QUERY_TYPE_TIMESTAMP", pure VK_QUERY_TYPE_TIMESTAMP)]
                 +++
                 prec 10
                   (expectP (Ident "VkQueryType") >> (VkQueryType <$> step readPrec)))

pattern VK_QUERY_TYPE_OCCLUSION :: VkQueryType

pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0

-- | Optional
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS :: VkQueryType

pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1

pattern VK_QUERY_TYPE_TIMESTAMP :: VkQueryType

pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2
