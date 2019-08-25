{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.SystemAllocationScope
       (VkSystemAllocationScope(VkSystemAllocationScope,
                                VK_SYSTEM_ALLOCATION_SCOPE_COMMAND,
                                VK_SYSTEM_ALLOCATION_SCOPE_OBJECT,
                                VK_SYSTEM_ALLOCATION_SCOPE_CACHE,
                                VK_SYSTEM_ALLOCATION_SCOPE_DEVICE,
                                VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSystemAllocationScope VkSystemAllocationScope registry at www.khronos.org>
newtype VkSystemAllocationScope = VkSystemAllocationScope Int32
                                  deriving (Eq, Ord, Enum, Storable)

instance Show VkSystemAllocationScope where
    showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_COMMAND
      = showString "VK_SYSTEM_ALLOCATION_SCOPE_COMMAND"
    showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_OBJECT
      = showString "VK_SYSTEM_ALLOCATION_SCOPE_OBJECT"
    showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_CACHE
      = showString "VK_SYSTEM_ALLOCATION_SCOPE_CACHE"
    showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_DEVICE
      = showString "VK_SYSTEM_ALLOCATION_SCOPE_DEVICE"
    showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE
      = showString "VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE"
    showsPrec p (VkSystemAllocationScope x)
      = showParen (p >= 11)
          (showString "VkSystemAllocationScope " . showsPrec 11 x)

instance Read VkSystemAllocationScope where
    readPrec
      = parens
          (choose
             [("VK_SYSTEM_ALLOCATION_SCOPE_COMMAND",
               pure VK_SYSTEM_ALLOCATION_SCOPE_COMMAND),
              ("VK_SYSTEM_ALLOCATION_SCOPE_OBJECT",
               pure VK_SYSTEM_ALLOCATION_SCOPE_OBJECT),
              ("VK_SYSTEM_ALLOCATION_SCOPE_CACHE",
               pure VK_SYSTEM_ALLOCATION_SCOPE_CACHE),
              ("VK_SYSTEM_ALLOCATION_SCOPE_DEVICE",
               pure VK_SYSTEM_ALLOCATION_SCOPE_DEVICE),
              ("VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE",
               pure VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE)]
             +++
             prec 10
               (expectP (Ident "VkSystemAllocationScope") >>
                  (VkSystemAllocationScope <$> step readPrec)))

pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND ::
        VkSystemAllocationScope

pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND =
        VkSystemAllocationScope 0

pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT ::
        VkSystemAllocationScope

pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT =
        VkSystemAllocationScope 1

pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE :: VkSystemAllocationScope

pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE =
        VkSystemAllocationScope 2

pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE ::
        VkSystemAllocationScope

pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE =
        VkSystemAllocationScope 3

pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE ::
        VkSystemAllocationScope

pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE =
        VkSystemAllocationScope 4
