{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.ScopeNV
       (VkScopeNV(VkScopeNV, VK_SCOPE_DEVICE_NV, VK_SCOPE_WORKGROUP_NV,
                  VK_SCOPE_SUBGROUP_NV, VK_SCOPE_QUEUE_FAMILY_NV))
       where
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (Int32)
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkScopeNV VkScopeNV registry at www.khronos.org>
newtype VkScopeNV = VkScopeNV Int32
                    deriving (Eq, Ord, Enum, Storable)

instance Show VkScopeNV where
    showsPrec _ VK_SCOPE_DEVICE_NV = showString "VK_SCOPE_DEVICE_NV"
    showsPrec _ VK_SCOPE_WORKGROUP_NV
      = showString "VK_SCOPE_WORKGROUP_NV"
    showsPrec _ VK_SCOPE_SUBGROUP_NV
      = showString "VK_SCOPE_SUBGROUP_NV"
    showsPrec _ VK_SCOPE_QUEUE_FAMILY_NV
      = showString "VK_SCOPE_QUEUE_FAMILY_NV"
    showsPrec p (VkScopeNV x)
      = showParen (p >= 11) (showString "VkScopeNV " . showsPrec 11 x)

instance Read VkScopeNV where
    readPrec
      = parens
          (choose
             [("VK_SCOPE_DEVICE_NV", pure VK_SCOPE_DEVICE_NV),
              ("VK_SCOPE_WORKGROUP_NV", pure VK_SCOPE_WORKGROUP_NV),
              ("VK_SCOPE_SUBGROUP_NV", pure VK_SCOPE_SUBGROUP_NV),
              ("VK_SCOPE_QUEUE_FAMILY_NV", pure VK_SCOPE_QUEUE_FAMILY_NV)]
             +++
             prec 10
               (expectP (Ident "VkScopeNV") >> (VkScopeNV <$> step readPrec)))

pattern VK_SCOPE_DEVICE_NV :: VkScopeNV

pattern VK_SCOPE_DEVICE_NV = VkScopeNV 1

pattern VK_SCOPE_WORKGROUP_NV :: VkScopeNV

pattern VK_SCOPE_WORKGROUP_NV = VkScopeNV 2

pattern VK_SCOPE_SUBGROUP_NV :: VkScopeNV

pattern VK_SCOPE_SUBGROUP_NV = VkScopeNV 3

pattern VK_SCOPE_QUEUE_FAMILY_NV :: VkScopeNV

pattern VK_SCOPE_QUEUE_FAMILY_NV = VkScopeNV 5
