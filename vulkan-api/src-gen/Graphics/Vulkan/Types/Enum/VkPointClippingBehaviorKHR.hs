{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPointClippingBehaviorKHR
       (VkPointClippingBehaviorKHR(VkPointClippingBehaviorKHR,
                                   VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR,
                                   VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR))
       where
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPointClippingBehaviorKHR.html VkPointClippingBehaviorKHR registry at www.khronos.org>
newtype VkPointClippingBehaviorKHR = VkPointClippingBehaviorKHR Int32
                                       deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                 Generic)

instance Show VkPointClippingBehaviorKHR where
        showsPrec _ VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR
          = showString "VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR"
        showsPrec _ VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR
          = showString "VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR"
        showsPrec p (VkPointClippingBehaviorKHR x)
          = showParen (p >= 11)
              (showString "VkPointClippingBehaviorKHR " . showsPrec 11 x)

instance Read VkPointClippingBehaviorKHR where
        readPrec
          = parens
              (choose
                 [("VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR",
                   pure VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR),
                  ("VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR",
                   pure VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkPointClippingBehaviorKHR") >>
                      (VkPointClippingBehaviorKHR <$> step readPrec)))

pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR ::
        VkPointClippingBehaviorKHR

pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR =
        VkPointClippingBehaviorKHR 0

pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR ::
        VkPointClippingBehaviorKHR

pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR =
        VkPointClippingBehaviorKHR 1
