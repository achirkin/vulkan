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
module Graphics.Vulkan.Types.Enum.VkQueryPipelineStatisticFlags
       (VkQueryPipelineStatisticBitmask(VkQueryPipelineStatisticBitmask,
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
        VkQueryPipelineStatisticFlags, VkQueryPipelineStatisticFlagBits)
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
