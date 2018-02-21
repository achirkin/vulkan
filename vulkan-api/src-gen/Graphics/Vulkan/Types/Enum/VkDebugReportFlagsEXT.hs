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
module Graphics.Vulkan.Types.Enum.VkDebugReportFlagsEXT
       (VkDebugReportBitmaskEXT(VkDebugReportBitmaskEXT,
                                VkDebugReportFlagsEXT, VkDebugReportFlagBitsEXT,
                                VK_DEBUG_REPORT_INFORMATION_BIT_EXT,
                                VK_DEBUG_REPORT_WARNING_BIT_EXT,
                                VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT,
                                VK_DEBUG_REPORT_ERROR_BIT_EXT, VK_DEBUG_REPORT_DEBUG_BIT_EXT),
        VkDebugReportFlagsEXT, VkDebugReportFlagBitsEXT)
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

newtype VkDebugReportBitmaskEXT (a ::
                                   FlagType) = VkDebugReportBitmaskEXT VkFlags
                                                 deriving (Eq, Ord, Storable, Data, Generic)

type VkDebugReportFlagsEXT = VkDebugReportBitmaskEXT FlagMask

type VkDebugReportFlagBitsEXT = VkDebugReportBitmaskEXT FlagBit

pattern VkDebugReportFlagBitsEXT ::
        VkFlags -> VkDebugReportBitmaskEXT FlagBit

pattern VkDebugReportFlagBitsEXT n = VkDebugReportBitmaskEXT n

pattern VkDebugReportFlagsEXT ::
        VkFlags -> VkDebugReportBitmaskEXT FlagMask

pattern VkDebugReportFlagsEXT n = VkDebugReportBitmaskEXT n

deriving instance Bits (VkDebugReportBitmaskEXT FlagMask)

deriving instance FiniteBits (VkDebugReportBitmaskEXT FlagMask)

deriving instance Integral (VkDebugReportBitmaskEXT FlagMask)

deriving instance Num (VkDebugReportBitmaskEXT FlagMask)

deriving instance Bounded (VkDebugReportBitmaskEXT FlagMask)

deriving instance Enum (VkDebugReportBitmaskEXT FlagMask)

deriving instance Real (VkDebugReportBitmaskEXT FlagMask)

instance Show (VkDebugReportBitmaskEXT a) where
        showsPrec _ VK_DEBUG_REPORT_INFORMATION_BIT_EXT
          = showString "VK_DEBUG_REPORT_INFORMATION_BIT_EXT"
        showsPrec _ VK_DEBUG_REPORT_WARNING_BIT_EXT
          = showString "VK_DEBUG_REPORT_WARNING_BIT_EXT"
        showsPrec _ VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
          = showString "VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT"
        showsPrec _ VK_DEBUG_REPORT_ERROR_BIT_EXT
          = showString "VK_DEBUG_REPORT_ERROR_BIT_EXT"
        showsPrec _ VK_DEBUG_REPORT_DEBUG_BIT_EXT
          = showString "VK_DEBUG_REPORT_DEBUG_BIT_EXT"
        showsPrec p (VkDebugReportBitmaskEXT x)
          = showParen (p >= 11)
              (showString "VkDebugReportBitmaskEXT " . showsPrec 11 x)

instance Read (VkDebugReportBitmaskEXT a) where
        readPrec
          = parens
              (choose
                 [("VK_DEBUG_REPORT_INFORMATION_BIT_EXT",
                   pure VK_DEBUG_REPORT_INFORMATION_BIT_EXT),
                  ("VK_DEBUG_REPORT_WARNING_BIT_EXT",
                   pure VK_DEBUG_REPORT_WARNING_BIT_EXT),
                  ("VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT",
                   pure VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT),
                  ("VK_DEBUG_REPORT_ERROR_BIT_EXT",
                   pure VK_DEBUG_REPORT_ERROR_BIT_EXT),
                  ("VK_DEBUG_REPORT_DEBUG_BIT_EXT",
                   pure VK_DEBUG_REPORT_DEBUG_BIT_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDebugReportBitmaskEXT") >>
                      (VkDebugReportBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT ::
        VkDebugReportBitmaskEXT a

pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT =
        VkDebugReportBitmaskEXT 1

-- | bitpos = @1@
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT ::
        VkDebugReportBitmaskEXT a

pattern VK_DEBUG_REPORT_WARNING_BIT_EXT = VkDebugReportBitmaskEXT 2

-- | bitpos = @2@
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT ::
        VkDebugReportBitmaskEXT a

pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT =
        VkDebugReportBitmaskEXT 4

-- | bitpos = @3@
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT :: VkDebugReportBitmaskEXT a

pattern VK_DEBUG_REPORT_ERROR_BIT_EXT = VkDebugReportBitmaskEXT 8

-- | bitpos = @4@
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT :: VkDebugReportBitmaskEXT a

pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT = VkDebugReportBitmaskEXT 16
