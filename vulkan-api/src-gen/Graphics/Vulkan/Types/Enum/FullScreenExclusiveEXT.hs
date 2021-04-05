{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.FullScreenExclusiveEXT
       (VkFullScreenExclusiveEXT(VkFullScreenExclusiveEXT,
                                 VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT,
                                 VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT,
                                 VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT,
                                 VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT))
       where
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (Int32)
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFullScreenExclusiveEXT VkFullScreenExclusiveEXT registry at www.khronos.org>
newtype VkFullScreenExclusiveEXT = VkFullScreenExclusiveEXT Int32
                                   deriving (Eq, Ord, Enum, Storable)

instance Show VkFullScreenExclusiveEXT where
    showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT
      = showString "VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT"
    showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT
      = showString "VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT"
    showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT
      = showString "VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT"
    showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT
      = showString "VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT"
    showsPrec p (VkFullScreenExclusiveEXT x)
      = showParen (p >= 11)
          (showString "VkFullScreenExclusiveEXT " . showsPrec 11 x)

instance Read VkFullScreenExclusiveEXT where
    readPrec
      = parens
          (choose
             [("VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT",
               pure VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT),
              ("VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT",
               pure VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT),
              ("VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT",
               pure VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT),
              ("VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT",
               pure VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT)]
             +++
             prec 10
               (expectP (Ident "VkFullScreenExclusiveEXT") >>
                  (VkFullScreenExclusiveEXT <$> step readPrec)))

pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT ::
        VkFullScreenExclusiveEXT

pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT =
        VkFullScreenExclusiveEXT 0

pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT ::
        VkFullScreenExclusiveEXT

pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT =
        VkFullScreenExclusiveEXT 1

pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT ::
        VkFullScreenExclusiveEXT

pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT =
        VkFullScreenExclusiveEXT 2

pattern VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT ::
        VkFullScreenExclusiveEXT

pattern VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT =
        VkFullScreenExclusiveEXT 3
