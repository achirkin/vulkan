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
module Graphics.Vulkan.Types.Enum.Shader
       (VkShaderInfoTypeAMD(VkShaderInfoTypeAMD,
                            VK_SHADER_INFO_TYPE_STATISTICS_AMD, VK_SHADER_INFO_TYPE_BINARY_AMD,
                            VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD),
        VkShaderStageBitmask(VkShaderStageBitmask, VkShaderStageFlags,
                             VkShaderStageFlagBits, VK_SHADER_STAGE_VERTEX_BIT,
                             VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT,
                             VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT,
                             VK_SHADER_STAGE_GEOMETRY_BIT, VK_SHADER_STAGE_FRAGMENT_BIT,
                             VK_SHADER_STAGE_COMPUTE_BIT, VK_SHADER_STAGE_ALL_GRAPHICS,
                             VK_SHADER_STAGE_ALL),
        VkShaderStageFlags, VkShaderStageFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkShaderInfoTypeAMD VkShaderInfoTypeAMD registry at www.khronos.org>
newtype VkShaderInfoTypeAMD = VkShaderInfoTypeAMD Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

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

newtype VkShaderStageBitmask (a ::
                                FlagType) = VkShaderStageBitmask VkFlags
                                              deriving (Eq, Ord, Storable, Data, Generic)

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

deriving instance Integral (VkShaderStageBitmask FlagMask)

deriving instance Num (VkShaderStageBitmask FlagMask)

deriving instance Bounded (VkShaderStageBitmask FlagMask)

deriving instance Enum (VkShaderStageBitmask FlagMask)

deriving instance Real (VkShaderStageBitmask FlagMask)

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
