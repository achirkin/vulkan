{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkTessellationDomainOriginKHR
       (VkTessellationDomainOriginKHR(VkTessellationDomainOriginKHR,
                                      VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR,
                                      VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkTessellationDomainOriginKHR.html VkTessellationDomainOriginKHR registry at www.khronos.org>
newtype VkTessellationDomainOriginKHR = VkTessellationDomainOriginKHR Int32
                                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data,
                                                    Generic)

instance Show VkTessellationDomainOriginKHR where
        showsPrec _ VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR
          = showString "VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR"
        showsPrec _ VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR
          = showString "VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR"
        showsPrec p (VkTessellationDomainOriginKHR x)
          = showParen (p >= 11)
              (showString "VkTessellationDomainOriginKHR " . showsPrec 11 x)

instance Read VkTessellationDomainOriginKHR where
        readPrec
          = parens
              (choose
                 [("VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR",
                   pure VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR),
                  ("VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR",
                   pure VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkTessellationDomainOriginKHR") >>
                      (VkTessellationDomainOriginKHR <$> step readPrec)))

pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR ::
        VkTessellationDomainOriginKHR

pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR =
        VkTessellationDomainOriginKHR 0

pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR ::
        VkTessellationDomainOriginKHR

pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR =
        VkTessellationDomainOriginKHR 1
