{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.DynamicState
       (VkDynamicState(VkDynamicState, VK_DYNAMIC_STATE_VIEWPORT,
                       VK_DYNAMIC_STATE_SCISSOR, VK_DYNAMIC_STATE_LINE_WIDTH,
                       VK_DYNAMIC_STATE_DEPTH_BIAS, VK_DYNAMIC_STATE_BLEND_CONSTANTS,
                       VK_DYNAMIC_STATE_DEPTH_BOUNDS,
                       VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK,
                       VK_DYNAMIC_STATE_STENCIL_WRITE_MASK,
                       VK_DYNAMIC_STATE_STENCIL_REFERENCE))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDynamicState VkDynamicState registry at www.khronos.org>
newtype VkDynamicState = VkDynamicState Int32
                         deriving (Eq, Ord, Enum, Storable)

instance Show VkDynamicState where
    showsPrec _ VK_DYNAMIC_STATE_VIEWPORT
      = showString "VK_DYNAMIC_STATE_VIEWPORT"
    showsPrec _ VK_DYNAMIC_STATE_SCISSOR
      = showString "VK_DYNAMIC_STATE_SCISSOR"
    showsPrec _ VK_DYNAMIC_STATE_LINE_WIDTH
      = showString "VK_DYNAMIC_STATE_LINE_WIDTH"
    showsPrec _ VK_DYNAMIC_STATE_DEPTH_BIAS
      = showString "VK_DYNAMIC_STATE_DEPTH_BIAS"
    showsPrec _ VK_DYNAMIC_STATE_BLEND_CONSTANTS
      = showString "VK_DYNAMIC_STATE_BLEND_CONSTANTS"
    showsPrec _ VK_DYNAMIC_STATE_DEPTH_BOUNDS
      = showString "VK_DYNAMIC_STATE_DEPTH_BOUNDS"
    showsPrec _ VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK
      = showString "VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK"
    showsPrec _ VK_DYNAMIC_STATE_STENCIL_WRITE_MASK
      = showString "VK_DYNAMIC_STATE_STENCIL_WRITE_MASK"
    showsPrec _ VK_DYNAMIC_STATE_STENCIL_REFERENCE
      = showString "VK_DYNAMIC_STATE_STENCIL_REFERENCE"
    showsPrec p (VkDynamicState x)
      = showParen (p >= 11)
          (showString "VkDynamicState " . showsPrec 11 x)

instance Read VkDynamicState where
    readPrec
      = parens
          (choose
             [("VK_DYNAMIC_STATE_VIEWPORT", pure VK_DYNAMIC_STATE_VIEWPORT),
              ("VK_DYNAMIC_STATE_SCISSOR", pure VK_DYNAMIC_STATE_SCISSOR),
              ("VK_DYNAMIC_STATE_LINE_WIDTH", pure VK_DYNAMIC_STATE_LINE_WIDTH),
              ("VK_DYNAMIC_STATE_DEPTH_BIAS", pure VK_DYNAMIC_STATE_DEPTH_BIAS),
              ("VK_DYNAMIC_STATE_BLEND_CONSTANTS",
               pure VK_DYNAMIC_STATE_BLEND_CONSTANTS),
              ("VK_DYNAMIC_STATE_DEPTH_BOUNDS",
               pure VK_DYNAMIC_STATE_DEPTH_BOUNDS),
              ("VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK",
               pure VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK),
              ("VK_DYNAMIC_STATE_STENCIL_WRITE_MASK",
               pure VK_DYNAMIC_STATE_STENCIL_WRITE_MASK),
              ("VK_DYNAMIC_STATE_STENCIL_REFERENCE",
               pure VK_DYNAMIC_STATE_STENCIL_REFERENCE)]
             +++
             prec 10
               (expectP (Ident "VkDynamicState") >>
                  (VkDynamicState <$> step readPrec)))

pattern VK_DYNAMIC_STATE_VIEWPORT :: VkDynamicState

pattern VK_DYNAMIC_STATE_VIEWPORT = VkDynamicState 0

pattern VK_DYNAMIC_STATE_SCISSOR :: VkDynamicState

pattern VK_DYNAMIC_STATE_SCISSOR = VkDynamicState 1

pattern VK_DYNAMIC_STATE_LINE_WIDTH :: VkDynamicState

pattern VK_DYNAMIC_STATE_LINE_WIDTH = VkDynamicState 2

pattern VK_DYNAMIC_STATE_DEPTH_BIAS :: VkDynamicState

pattern VK_DYNAMIC_STATE_DEPTH_BIAS = VkDynamicState 3

pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS :: VkDynamicState

pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS = VkDynamicState 4

pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS :: VkDynamicState

pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS = VkDynamicState 5

pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK :: VkDynamicState

pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = VkDynamicState 6

pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK :: VkDynamicState

pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = VkDynamicState 7

pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE :: VkDynamicState

pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE = VkDynamicState 8
