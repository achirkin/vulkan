{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkTessellationDomainOrigin
       (VkTessellationDomainOrigin(VkTessellationDomainOrigin,
                                   VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT,
                                   VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkTessellationDomainOrigin VkTessellationDomainOrigin registry at www.khronos.org>
newtype VkTessellationDomainOrigin = VkTessellationDomainOrigin Int32
                                       deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                 Generic)

instance Show VkTessellationDomainOrigin where
        showsPrec _ VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
          = showString "VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT"
        showsPrec _ VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
          = showString "VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT"
        showsPrec p (VkTessellationDomainOrigin x)
          = showParen (p >= 11)
              (showString "VkTessellationDomainOrigin " . showsPrec 11 x)

instance Read VkTessellationDomainOrigin where
        readPrec
          = parens
              (choose
                 [("VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT",
                   pure VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT),
                  ("VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT",
                   pure VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT)]
                 +++
                 prec 10
                   (expectP (Ident "VkTessellationDomainOrigin") >>
                      (VkTessellationDomainOrigin <$> step readPrec)))

pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT ::
        VkTessellationDomainOrigin

pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT =
        VkTessellationDomainOrigin 0

pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT ::
        VkTessellationDomainOrigin

pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT =
        VkTessellationDomainOrigin 1
