{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.AccelerationStructure
       (VkAccelerationStructureBuildTypeKHR(VkAccelerationStructureBuildTypeKHR,
                                            VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR,
                                            VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR,
                                            VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR),
        VkAccelerationStructureMemoryRequirementsTypeKHR(VkAccelerationStructureMemoryRequirementsTypeKHR,
                                                         VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR,
                                                         VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR,
                                                         VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR),
        VkAccelerationStructureMemoryRequirementsTypeNV(..),
        VkAccelerationStructureTypeKHR(VkAccelerationStructureTypeKHR,
                                       VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR,
                                       VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR),
        VkAccelerationStructureTypeNV(..))
       where
import Data.Bits                       (Bits, FiniteBits)
import Data.Coerce                     (coerce)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (Int32)
import Graphics.Vulkan.Types.BaseTypes (VkFlags)
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureBuildTypeKHR VkAccelerationStructureBuildTypeKHR registry at www.khronos.org>
newtype VkAccelerationStructureBuildTypeKHR = VkAccelerationStructureBuildTypeKHR Int32
                                              deriving (Eq, Ord, Enum, Storable)

instance Show VkAccelerationStructureBuildTypeKHR where
    showsPrec _ VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR
      = showString "VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR"
    showsPrec _ VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
      = showString "VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR"
    showsPrec _ VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR
      = showString
          "VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR"
    showsPrec p (VkAccelerationStructureBuildTypeKHR x)
      = showParen (p >= 11)
          (showString "VkAccelerationStructureBuildTypeKHR " .
             showsPrec 11 x)

instance Read VkAccelerationStructureBuildTypeKHR where
    readPrec
      = parens
          (choose
             [("VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR",
               pure VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR),
              ("VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR",
               pure VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR),
              ("VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR",
               pure VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR)]
             +++
             prec 10
               (expectP (Ident "VkAccelerationStructureBuildTypeKHR") >>
                  (VkAccelerationStructureBuildTypeKHR <$> step readPrec)))

pattern VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR ::
        VkAccelerationStructureBuildTypeKHR

pattern VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR =
        VkAccelerationStructureBuildTypeKHR 0

pattern VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR ::
        VkAccelerationStructureBuildTypeKHR

pattern VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR =
        VkAccelerationStructureBuildTypeKHR 1

pattern VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR ::
        VkAccelerationStructureBuildTypeKHR

pattern VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR =
        VkAccelerationStructureBuildTypeKHR 2

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureMemoryRequirementsTypeKHR VkAccelerationStructureMemoryRequirementsTypeKHR registry at www.khronos.org>
newtype VkAccelerationStructureMemoryRequirementsTypeKHR = VkAccelerationStructureMemoryRequirementsTypeKHR Int32
                                                           deriving (Eq, Ord, Enum, Storable)

instance Show VkAccelerationStructureMemoryRequirementsTypeKHR
         where
    showsPrec _
      VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR
      = showString
          "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR"
    showsPrec _
      VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR
      = showString
          "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR"
    showsPrec _
      VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR
      = showString
          "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR"
    showsPrec p (VkAccelerationStructureMemoryRequirementsTypeKHR x)
      = showParen (p >= 11)
          (showString "VkAccelerationStructureMemoryRequirementsTypeKHR " .
             showsPrec 11 x)

instance Read VkAccelerationStructureMemoryRequirementsTypeKHR
         where
    readPrec
      = parens
          (choose
             [("VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR",
               pure
                 VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR),
              ("VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR",
               pure
                 VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR),
              ("VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR",
               pure
                 VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR)]
             +++
             prec 10
               (expectP (Ident "VkAccelerationStructureMemoryRequirementsTypeKHR")
                  >>
                  (VkAccelerationStructureMemoryRequirementsTypeKHR <$>
                     step readPrec)))

pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR
        :: VkAccelerationStructureMemoryRequirementsTypeKHR

pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR
        = VkAccelerationStructureMemoryRequirementsTypeKHR 0

pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR
        :: VkAccelerationStructureMemoryRequirementsTypeKHR

pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR
        = VkAccelerationStructureMemoryRequirementsTypeKHR 1

pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR
        :: VkAccelerationStructureMemoryRequirementsTypeKHR

pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR
        = VkAccelerationStructureMemoryRequirementsTypeKHR 2

newtype VkAccelerationStructureMemoryRequirementsTypeNV = VkAccelerationStructureMemoryRequirementsTypeNV VkFlags
                                                          deriving (Eq, Ord, Enum, Bits, FiniteBits,
                                                                    Storable)

instance Show VkAccelerationStructureMemoryRequirementsTypeNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkAccelerationStructureMemoryRequirementsTypeNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureTypeKHR VkAccelerationStructureTypeKHR registry at www.khronos.org>
newtype VkAccelerationStructureTypeKHR = VkAccelerationStructureTypeKHR Int32
                                         deriving (Eq, Ord, Enum, Storable)

instance Show VkAccelerationStructureTypeKHR where
    showsPrec _ VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
      = showString "VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR"
    showsPrec _ VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
      = showString "VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR"
    showsPrec p (VkAccelerationStructureTypeKHR x)
      = showParen (p >= 11)
          (showString "VkAccelerationStructureTypeKHR " . showsPrec 11 x)

instance Read VkAccelerationStructureTypeKHR where
    readPrec
      = parens
          (choose
             [("VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR",
               pure VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR),
              ("VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR",
               pure VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR)]
             +++
             prec 10
               (expectP (Ident "VkAccelerationStructureTypeKHR") >>
                  (VkAccelerationStructureTypeKHR <$> step readPrec)))

pattern VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR ::
        VkAccelerationStructureTypeKHR

pattern VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR =
        VkAccelerationStructureTypeKHR 0

pattern VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR ::
        VkAccelerationStructureTypeKHR

pattern VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR =
        VkAccelerationStructureTypeKHR 1

newtype VkAccelerationStructureTypeNV = VkAccelerationStructureTypeNV VkFlags
                                        deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkAccelerationStructureTypeNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkAccelerationStructureTypeNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
