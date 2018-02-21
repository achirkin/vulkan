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
module Graphics.Vulkan.Types.Enum.VkPipelineStageFlags
       (VkPipelineStageBitmask(VkPipelineStageBitmask,
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

newtype VkPipelineStageBitmask (a ::
                                  FlagType) = VkPipelineStageBitmask VkFlags
                                                deriving (Eq, Ord, Storable, Data, Generic)

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

deriving instance Integral (VkPipelineStageBitmask FlagMask)

deriving instance Num (VkPipelineStageBitmask FlagMask)

deriving instance Bounded (VkPipelineStageBitmask FlagMask)

deriving instance Enum (VkPipelineStageBitmask FlagMask)

deriving instance Real (VkPipelineStageBitmask FlagMask)

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
