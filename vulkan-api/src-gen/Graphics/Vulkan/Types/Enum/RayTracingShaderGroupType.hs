{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.RayTracingShaderGroupType
       (VkRayTracingShaderGroupTypeKHR(VkRayTracingShaderGroupTypeKHR,
                                       VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR,
                                       VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR,
                                       VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR),
        VkRayTracingShaderGroupTypeNV(..))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRayTracingShaderGroupTypeKHR VkRayTracingShaderGroupTypeKHR registry at www.khronos.org>
newtype VkRayTracingShaderGroupTypeKHR = VkRayTracingShaderGroupTypeKHR Int32
                                         deriving (Eq, Ord, Enum, Storable)

instance Show VkRayTracingShaderGroupTypeKHR where
    showsPrec _ VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR
      = showString "VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR"
    showsPrec _
      VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR
      = showString
          "VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR"
    showsPrec _
      VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR
      = showString
          "VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR"
    showsPrec p (VkRayTracingShaderGroupTypeKHR x)
      = showParen (p >= 11)
          (showString "VkRayTracingShaderGroupTypeKHR " . showsPrec 11 x)

instance Read VkRayTracingShaderGroupTypeKHR where
    readPrec
      = parens
          (choose
             [("VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR",
               pure VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR),
              ("VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR",
               pure VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR),
              ("VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR",
               pure VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR)]
             +++
             prec 10
               (expectP (Ident "VkRayTracingShaderGroupTypeKHR") >>
                  (VkRayTracingShaderGroupTypeKHR <$> step readPrec)))

pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR ::
        VkRayTracingShaderGroupTypeKHR

pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR =
        VkRayTracingShaderGroupTypeKHR 0

pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR ::
        VkRayTracingShaderGroupTypeKHR

pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR =
        VkRayTracingShaderGroupTypeKHR 1

pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR
        :: VkRayTracingShaderGroupTypeKHR

pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR =
        VkRayTracingShaderGroupTypeKHR 2

newtype VkRayTracingShaderGroupTypeNV = VkRayTracingShaderGroupTypeNV VkFlags
                                        deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkRayTracingShaderGroupTypeNV where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkRayTracingShaderGroupTypeNV where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
