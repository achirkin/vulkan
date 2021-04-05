{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.TimeDomainEXT
       (VkTimeDomainEXT(VkTimeDomainEXT, VK_TIME_DOMAIN_DEVICE_EXT,
                        VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT,
                        VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT,
                        VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT))
       where
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (Int32)
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkTimeDomainEXT VkTimeDomainEXT registry at www.khronos.org>
newtype VkTimeDomainEXT = VkTimeDomainEXT Int32
                          deriving (Eq, Ord, Enum, Storable)

instance Show VkTimeDomainEXT where
    showsPrec _ VK_TIME_DOMAIN_DEVICE_EXT
      = showString "VK_TIME_DOMAIN_DEVICE_EXT"
    showsPrec _ VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT
      = showString "VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT"
    showsPrec _ VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT
      = showString "VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT"
    showsPrec _ VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT
      = showString "VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT"
    showsPrec p (VkTimeDomainEXT x)
      = showParen (p >= 11)
          (showString "VkTimeDomainEXT " . showsPrec 11 x)

instance Read VkTimeDomainEXT where
    readPrec
      = parens
          (choose
             [("VK_TIME_DOMAIN_DEVICE_EXT", pure VK_TIME_DOMAIN_DEVICE_EXT),
              ("VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT",
               pure VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT),
              ("VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT",
               pure VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT),
              ("VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT",
               pure VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT)]
             +++
             prec 10
               (expectP (Ident "VkTimeDomainEXT") >>
                  (VkTimeDomainEXT <$> step readPrec)))

pattern VK_TIME_DOMAIN_DEVICE_EXT :: VkTimeDomainEXT

pattern VK_TIME_DOMAIN_DEVICE_EXT = VkTimeDomainEXT 0

pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT :: VkTimeDomainEXT

pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT = VkTimeDomainEXT 1

pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT :: VkTimeDomainEXT

pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT = VkTimeDomainEXT 2

pattern VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT ::
        VkTimeDomainEXT

pattern VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT =
        VkTimeDomainEXT 3
