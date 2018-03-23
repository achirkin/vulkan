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
module Graphics.Vulkan.Types.Enum.VkDebugUtilsMessageTypeFlagsEXT
       (VkDebugUtilsMessageTypeBitmaskEXT(VkDebugUtilsMessageTypeBitmaskEXT,
                                          VkDebugUtilsMessageTypeFlagsEXT,
                                          VkDebugUtilsMessageTypeFlagBitsEXT,
                                          VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT,
                                          VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
                                          VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT),
        VkDebugUtilsMessageTypeFlagsEXT,
        VkDebugUtilsMessageTypeFlagBitsEXT)
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

newtype VkDebugUtilsMessageTypeBitmaskEXT (a ::
                                             FlagType) = VkDebugUtilsMessageTypeBitmaskEXT VkFlags
                                                           deriving (Eq, Ord, Storable, Data,
                                                                     Generic)

type VkDebugUtilsMessageTypeFlagsEXT =
     VkDebugUtilsMessageTypeBitmaskEXT FlagMask

type VkDebugUtilsMessageTypeFlagBitsEXT =
     VkDebugUtilsMessageTypeBitmaskEXT FlagBit

pattern VkDebugUtilsMessageTypeFlagBitsEXT ::
        VkFlags -> VkDebugUtilsMessageTypeBitmaskEXT FlagBit

pattern VkDebugUtilsMessageTypeFlagBitsEXT n =
        VkDebugUtilsMessageTypeBitmaskEXT n

pattern VkDebugUtilsMessageTypeFlagsEXT ::
        VkFlags -> VkDebugUtilsMessageTypeBitmaskEXT FlagMask

pattern VkDebugUtilsMessageTypeFlagsEXT n =
        VkDebugUtilsMessageTypeBitmaskEXT n

deriving instance Bits (VkDebugUtilsMessageTypeBitmaskEXT FlagMask)

deriving instance
         FiniteBits (VkDebugUtilsMessageTypeBitmaskEXT FlagMask)

deriving instance
         Integral (VkDebugUtilsMessageTypeBitmaskEXT FlagMask)

deriving instance Num (VkDebugUtilsMessageTypeBitmaskEXT FlagMask)

deriving instance
         Bounded (VkDebugUtilsMessageTypeBitmaskEXT FlagMask)

deriving instance Enum (VkDebugUtilsMessageTypeBitmaskEXT FlagMask)

deriving instance Real (VkDebugUtilsMessageTypeBitmaskEXT FlagMask)

instance Show (VkDebugUtilsMessageTypeBitmaskEXT a) where
        showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT"
        showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT"
        showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
          = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT"
        showsPrec p (VkDebugUtilsMessageTypeBitmaskEXT x)
          = showParen (p >= 11)
              (showString "VkDebugUtilsMessageTypeBitmaskEXT " . showsPrec 11 x)

instance Read (VkDebugUtilsMessageTypeBitmaskEXT a) where
        readPrec
          = parens
              (choose
                 [("VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT",
                   pure VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT),
                  ("VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT",
                   pure VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT),
                  ("VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT",
                   pure VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT)]
                 +++
                 prec 10
                   (expectP (Ident "VkDebugUtilsMessageTypeBitmaskEXT") >>
                      (VkDebugUtilsMessageTypeBitmaskEXT <$> step readPrec)))

-- | bitpos = @0@
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT ::
        VkDebugUtilsMessageTypeBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT =
        VkDebugUtilsMessageTypeBitmaskEXT 1

-- | bitpos = @1@
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT ::
        VkDebugUtilsMessageTypeBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT =
        VkDebugUtilsMessageTypeBitmaskEXT 2

-- | bitpos = @2@
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT ::
        VkDebugUtilsMessageTypeBitmaskEXT a

pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT =
        VkDebugUtilsMessageTypeBitmaskEXT 4
