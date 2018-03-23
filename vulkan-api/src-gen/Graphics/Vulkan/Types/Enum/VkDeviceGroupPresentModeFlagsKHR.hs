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
module Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHR
       (VkDeviceGroupPresentModeBitmaskKHR(VkDeviceGroupPresentModeBitmaskKHR,
                                           VkDeviceGroupPresentModeFlagsKHR,
                                           VkDeviceGroupPresentModeFlagBitsKHR,
                                           VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR,
                                           VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR,
                                           VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR,
                                           VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR),
        VkDeviceGroupPresentModeFlagsKHR,
        VkDeviceGroupPresentModeFlagBitsKHR)
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

newtype VkDeviceGroupPresentModeBitmaskKHR (a ::
                                              FlagType) = VkDeviceGroupPresentModeBitmaskKHR VkFlags
                                                            deriving (Eq, Ord, Storable, Data,
                                                                      Generic)

type VkDeviceGroupPresentModeFlagsKHR =
     VkDeviceGroupPresentModeBitmaskKHR FlagMask

type VkDeviceGroupPresentModeFlagBitsKHR =
     VkDeviceGroupPresentModeBitmaskKHR FlagBit

pattern VkDeviceGroupPresentModeFlagBitsKHR ::
        VkFlags -> VkDeviceGroupPresentModeBitmaskKHR FlagBit

pattern VkDeviceGroupPresentModeFlagBitsKHR n =
        VkDeviceGroupPresentModeBitmaskKHR n

pattern VkDeviceGroupPresentModeFlagsKHR ::
        VkFlags -> VkDeviceGroupPresentModeBitmaskKHR FlagMask

pattern VkDeviceGroupPresentModeFlagsKHR n =
        VkDeviceGroupPresentModeBitmaskKHR n

deriving instance
         Bits (VkDeviceGroupPresentModeBitmaskKHR FlagMask)

deriving instance
         FiniteBits (VkDeviceGroupPresentModeBitmaskKHR FlagMask)

deriving instance
         Integral (VkDeviceGroupPresentModeBitmaskKHR FlagMask)

deriving instance Num (VkDeviceGroupPresentModeBitmaskKHR FlagMask)

deriving instance
         Bounded (VkDeviceGroupPresentModeBitmaskKHR FlagMask)

deriving instance
         Enum (VkDeviceGroupPresentModeBitmaskKHR FlagMask)

deriving instance
         Real (VkDeviceGroupPresentModeBitmaskKHR FlagMask)

instance Show (VkDeviceGroupPresentModeBitmaskKHR a) where
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR
          = showString "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR"
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR
          = showString "VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR"
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR
          = showString "VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR"
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR
          = showString
              "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR"
        showsPrec p (VkDeviceGroupPresentModeBitmaskKHR x)
          = showParen (p >= 11)
              (showString "VkDeviceGroupPresentModeBitmaskKHR " . showsPrec 11 x)

instance Read (VkDeviceGroupPresentModeBitmaskKHR a) where
        readPrec
          = parens
              (choose
                 [("VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR),
                  ("VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR),
                  ("VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR),
                  ("VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR)]
                 +++
                 prec 10
                   (expectP (Ident "VkDeviceGroupPresentModeBitmaskKHR") >>
                      (VkDeviceGroupPresentModeBitmaskKHR <$> step readPrec)))

-- | Present from local memory
--
--   bitpos = @0@
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR ::
        VkDeviceGroupPresentModeBitmaskKHR a

pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR =
        VkDeviceGroupPresentModeBitmaskKHR 1

-- | Present from remote memory
--
--   bitpos = @1@
pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR ::
        VkDeviceGroupPresentModeBitmaskKHR a

pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR =
        VkDeviceGroupPresentModeBitmaskKHR 2

-- | Present sum of local and/or remote memory
--
--   bitpos = @2@
pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR ::
        VkDeviceGroupPresentModeBitmaskKHR a

pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR =
        VkDeviceGroupPresentModeBitmaskKHR 4

-- | Each physical device presents from local memory
--
--   bitpos = @3@
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR ::
        VkDeviceGroupPresentModeBitmaskKHR a

pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR =
        VkDeviceGroupPresentModeBitmaskKHR 8
