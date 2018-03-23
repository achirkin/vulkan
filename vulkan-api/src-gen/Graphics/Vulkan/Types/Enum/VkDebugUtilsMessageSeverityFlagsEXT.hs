{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.VkDebugUtilsMessageSeverityFlagsEXT
       (VkDebugUtilsMessageSeverityBitmaskEXT(VkDebugUtilsMessageSeverityBitmaskEXT,
                                              VkDebugUtilsMessageSeverityFlagsEXT,
                                              VkDebugUtilsMessageSeverityFlagBitsEXT,
                                              VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT,
                                              VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT,
                                              VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,
                                              VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT),
        VkDebugUtilsMessageSeverityFlagsEXT,
        VkDebugUtilsMessageSeverityFlagBitsEXT)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkDebugUtilsMessageSeverityBitmaskEXT (a ::
                                                 FlagType) = VkDebugUtilsMessageSeverityBitmaskEXT VkFlags
                                                               deriving (Eq, Ord, Storable, Data,
                                                                         Generic)

type VkDebugUtilsMessageSeverityFlagsEXT =
     VkDebugUtilsMessageSeverityBitmaskEXT FlagMask

type VkDebugUtilsMessageSeverityFlagBitsEXT =
     VkDebugUtilsMessageSeverityBitmaskEXT FlagBit

pattern VkDebugUtilsMessageSeverityFlagBitsEXT ::
        VkFlags -> VkDebugUtilsMessageSeverityBitmaskEXT FlagBit

pattern VkDebugUtilsMessageSeverityFlagBitsEXT n =
        VkDebugUtilsMessageSeverityBitmaskEXT n

pattern VkDebugUtilsMessageSeverityFlagsEXT ::
        VkFlags -> VkDebugUtilsMessageSeverityBitmaskEXT FlagMask

pattern VkDebugUtilsMessageSeverityFlagsEXT n =
        VkDebugUtilsMessageSeverityBitmaskEXT n

deriving instance
         Bits (VkDebugUtilsMessageSeverityBitmaskEXT FlagMask)

deriving instance
         FiniteBits (VkDebugUtilsMessageSeverityBitmaskEXT FlagMask)

deriving instance
         Integral (VkDebugUtilsMessageSeverityBitmaskEXT FlagMask)

deriving instance
         Num (VkDebugUtilsMessageSeverityBitmaskEXT FlagMask)

deriving instance
         Bounded (VkDebugUtilsMessageSeverityBitmaskEXT FlagMask)

deriving instance
         Enum (VkDebugUtilsMessageSeverityBitmaskEXT FlagMask)

deriving instance
         Real (VkDebugUtilsMessageSeverityBitmaskEXT FlagMask)

instance Show (VkDebugUtilsMessageSeverityBitmaskEXT a) where
        showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
          = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT"
        showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
          = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT"
        showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
          = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT"
        showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
          = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT"
        showsPrec p (VkDebugUtilsMessageSeverityBitmaskEXT x)
          = showParen (p >= 11)
              (showString "VkDebugUtilsMessageSeverityBitmaskEXT " .
                 showsPrec 11 x)

instance Read (VkDebugUtilsMessageSeverityBitmaskEXT a) where
        readPrec
          = parens
              (choose
                 [("VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT",
                   pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT),
                  ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT",
                   pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT),
                  ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT",
                   pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT),
                  ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT",
                   pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDebugUtilsMessageSeverityBitmaskEXT") >>
                      (VkDebugUtilsMessageSeverityBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT ::
        VkDebugUtilsMessageSeverityBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT =
        VkDebugUtilsMessageSeverityBitmaskEXT 1

-- | bitpos = @4@
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT ::
        VkDebugUtilsMessageSeverityBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT =
        VkDebugUtilsMessageSeverityBitmaskEXT 16

-- | bitpos = @8@
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT ::
        VkDebugUtilsMessageSeverityBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT =
        VkDebugUtilsMessageSeverityBitmaskEXT 256

-- | bitpos = @12@
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT ::
        VkDebugUtilsMessageSeverityBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT =
        VkDebugUtilsMessageSeverityBitmaskEXT 4096
