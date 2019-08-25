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
        VkPipelineCacheCreateFlagBits(..),
        VkPipelineCacheHeaderVersion(VkPipelineCacheHeaderVersion,
                                     VK_PIPELINE_CACHE_HEADER_VERSION_ONE),
        VkPipelineColorBlendStateCreateFlagBits(..),
        VkPipelineCreateBitmask(VkPipelineCreateBitmask,
                                VkPipelineCreateFlags, VkPipelineCreateFlagBits,
                                VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT,
                                VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT,
                                VK_PIPELINE_CREATE_DERIVATIVE_BIT),
        VkPipelineCreateFlags, VkPipelineCreateFlagBits,
        VkPipelineDepthStencilStateCreateFlagBits(..),
        VkPipelineDynamicStateCreateFlagBits(..),
        VkPipelineInputAssemblyStateCreateFlagBits(..),
        VkPipelineLayoutCreateFlagBits(..),
        VkPipelineMultisampleStateCreateFlagBits(..),
        VkPipelineRasterizationStateCreateFlagBits(..),
        VkPipelineShaderStageCreateFlagBits(..),
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
        VkPipelineStageFlags, VkPipelineStageFlagBits,
        VkPipelineTessellationStateCreateFlagBits(..),
        VkPipelineVertexInputStateCreateFlagBits(..),
        VkPipelineViewportStateCreateFlagBits(..))
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineBindPoint VkPipelineBindPoint registry at www.khronos.org>
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

newtype VkPipelineCacheCreateFlagBits = VkPipelineCacheCreateFlagBits VkFlags
                                        deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineCacheCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineCacheCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineCacheHeaderVersion VkPipelineCacheHeaderVersion registry at www.khronos.org>
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

newtype VkPipelineColorBlendStateCreateFlagBits = VkPipelineColorBlendStateCreateFlagBits VkFlags
                                                  deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                            Storable)

instance Show VkPipelineColorBlendStateCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineColorBlendStateCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

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

newtype VkPipelineDepthStencilStateCreateFlagBits = VkPipelineDepthStencilStateCreateFlagBits VkFlags
                                                    deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                              Storable)

instance Show VkPipelineDepthStencilStateCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineDepthStencilStateCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineDynamicStateCreateFlagBits = VkPipelineDynamicStateCreateFlagBits VkFlags
                                               deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineDynamicStateCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineDynamicStateCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineInputAssemblyStateCreateFlagBits = VkPipelineInputAssemblyStateCreateFlagBits VkFlags
                                                     deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                               Storable)

instance Show VkPipelineInputAssemblyStateCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineInputAssemblyStateCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineLayoutCreateFlagBits = VkPipelineLayoutCreateFlagBits VkFlags
                                         deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineLayoutCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineLayoutCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineMultisampleStateCreateFlagBits = VkPipelineMultisampleStateCreateFlagBits VkFlags
                                                   deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                             Storable)

instance Show VkPipelineMultisampleStateCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineMultisampleStateCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineRasterizationStateCreateFlagBits = VkPipelineRasterizationStateCreateFlagBits VkFlags
                                                     deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                               Storable)

instance Show VkPipelineRasterizationStateCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineRasterizationStateCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineShaderStageCreateFlagBits = VkPipelineShaderStageCreateFlagBits VkFlags
                                              deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineShaderStageCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineShaderStageCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

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

newtype VkPipelineTessellationStateCreateFlagBits = VkPipelineTessellationStateCreateFlagBits VkFlags
                                                    deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                              Storable)

instance Show VkPipelineTessellationStateCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineTessellationStateCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineVertexInputStateCreateFlagBits = VkPipelineVertexInputStateCreateFlagBits VkFlags
                                                   deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                             Storable)

instance Show VkPipelineVertexInputStateCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineVertexInputStateCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkPipelineViewportStateCreateFlagBits = VkPipelineViewportStateCreateFlagBits VkFlags
                                                deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkPipelineViewportStateCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkPipelineViewportStateCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
