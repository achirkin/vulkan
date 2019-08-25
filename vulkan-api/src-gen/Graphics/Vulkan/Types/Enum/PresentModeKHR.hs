{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.PresentModeKHR
       (VkPresentModeKHR(VkPresentModeKHR, VK_PRESENT_MODE_IMMEDIATE_KHR,
                         VK_PRESENT_MODE_MAILBOX_KHR, VK_PRESENT_MODE_FIFO_KHR,
                         VK_PRESENT_MODE_FIFO_RELAXED_KHR))
       where
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentModeKHR VkPresentModeKHR registry at www.khronos.org>
newtype VkPresentModeKHR = VkPresentModeKHR Int32
                           deriving (Eq, Ord, Enum, Storable)

instance Show VkPresentModeKHR where
    showsPrec _ VK_PRESENT_MODE_IMMEDIATE_KHR
      = showString "VK_PRESENT_MODE_IMMEDIATE_KHR"
    showsPrec _ VK_PRESENT_MODE_MAILBOX_KHR
      = showString "VK_PRESENT_MODE_MAILBOX_KHR"
    showsPrec _ VK_PRESENT_MODE_FIFO_KHR
      = showString "VK_PRESENT_MODE_FIFO_KHR"
    showsPrec _ VK_PRESENT_MODE_FIFO_RELAXED_KHR
      = showString "VK_PRESENT_MODE_FIFO_RELAXED_KHR"
    showsPrec p (VkPresentModeKHR x)
      = showParen (p >= 11)
          (showString "VkPresentModeKHR " . showsPrec 11 x)

instance Read VkPresentModeKHR where
    readPrec
      = parens
          (choose
             [("VK_PRESENT_MODE_IMMEDIATE_KHR",
               pure VK_PRESENT_MODE_IMMEDIATE_KHR),
              ("VK_PRESENT_MODE_MAILBOX_KHR", pure VK_PRESENT_MODE_MAILBOX_KHR),
              ("VK_PRESENT_MODE_FIFO_KHR", pure VK_PRESENT_MODE_FIFO_KHR),
              ("VK_PRESENT_MODE_FIFO_RELAXED_KHR",
               pure VK_PRESENT_MODE_FIFO_RELAXED_KHR)]
             +++
             prec 10
               (expectP (Ident "VkPresentModeKHR") >>
                  (VkPresentModeKHR <$> step readPrec)))

pattern VK_PRESENT_MODE_IMMEDIATE_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_IMMEDIATE_KHR = VkPresentModeKHR 0

pattern VK_PRESENT_MODE_MAILBOX_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_MAILBOX_KHR = VkPresentModeKHR 1

pattern VK_PRESENT_MODE_FIFO_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_FIFO_KHR = VkPresentModeKHR 2

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR :: VkPresentModeKHR

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = VkPresentModeKHR 3
