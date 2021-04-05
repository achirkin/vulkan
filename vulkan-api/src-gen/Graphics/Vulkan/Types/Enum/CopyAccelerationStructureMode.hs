{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.CopyAccelerationStructureMode
       (VkCopyAccelerationStructureModeKHR(VkCopyAccelerationStructureModeKHR,
                                           VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR,
                                           VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR,
                                           VK_COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR,
                                           VK_COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR),
        VkCopyAccelerationStructureModeNV(..))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCopyAccelerationStructureModeKHR VkCopyAccelerationStructureModeKHR registry at www.khronos.org>
newtype VkCopyAccelerationStructureModeKHR = VkCopyAccelerationStructureModeKHR Int32
                                             deriving (Eq, Ord, Enum, Storable)

instance Show VkCopyAccelerationStructureModeKHR where
    showsPrec _ VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR
      = showString "VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR"
    showsPrec _ VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR
      = showString "VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR"
    showsPrec _ VK_COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR
      = showString "VK_COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR"
    showsPrec _ VK_COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR
      = showString "VK_COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR"
    showsPrec p (VkCopyAccelerationStructureModeKHR x)
      = showParen (p >= 11)
          (showString "VkCopyAccelerationStructureModeKHR " . showsPrec 11 x)

instance Read VkCopyAccelerationStructureModeKHR where
    readPrec
      = parens
          (choose
             [("VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR",
               pure VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR),
              ("VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR",
               pure VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR),
              ("VK_COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR",
               pure VK_COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR),
              ("VK_COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR",
               pure VK_COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR)]
             +++
             prec 10
               (expectP (Ident "VkCopyAccelerationStructureModeKHR") >>
                  (VkCopyAccelerationStructureModeKHR <$> step readPrec)))

pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR ::
        VkCopyAccelerationStructureModeKHR

pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR =
        VkCopyAccelerationStructureModeKHR 0

pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR ::
        VkCopyAccelerationStructureModeKHR

pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR =
        VkCopyAccelerationStructureModeKHR 1

pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR ::
        VkCopyAccelerationStructureModeKHR

pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR =
        VkCopyAccelerationStructureModeKHR 2

pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR ::
        VkCopyAccelerationStructureModeKHR

pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR =
        VkCopyAccelerationStructureModeKHR 3

newtype VkCopyAccelerationStructureModeNV = VkCopyAccelerationStructureModeNV VkFlags
                                            deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkCopyAccelerationStructureModeNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkCopyAccelerationStructureModeNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
