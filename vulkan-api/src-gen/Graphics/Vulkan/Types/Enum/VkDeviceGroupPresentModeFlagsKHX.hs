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
module Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHX
       (VkDeviceGroupPresentModeBitmaskKHX(VkDeviceGroupPresentModeBitmaskKHX,
                                           VkDeviceGroupPresentModeFlagsKHX,
                                           VkDeviceGroupPresentModeFlagBitsKHX,
                                           VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX,
                                           VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX,
                                           VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX,
                                           VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX),
        VkDeviceGroupPresentModeFlagsKHX,
        VkDeviceGroupPresentModeFlagBitsKHX)
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

newtype VkDeviceGroupPresentModeBitmaskKHX (a ::
                                              FlagType) = VkDeviceGroupPresentModeBitmaskKHX VkFlags
                                                            deriving (Eq, Ord, Storable, Data,
                                                                      Generic)

type VkDeviceGroupPresentModeFlagsKHX =
     VkDeviceGroupPresentModeBitmaskKHX FlagMask

type VkDeviceGroupPresentModeFlagBitsKHX =
     VkDeviceGroupPresentModeBitmaskKHX FlagBit

pattern VkDeviceGroupPresentModeFlagBitsKHX ::
        VkFlags -> VkDeviceGroupPresentModeBitmaskKHX FlagBit

pattern VkDeviceGroupPresentModeFlagBitsKHX n =
        VkDeviceGroupPresentModeBitmaskKHX n

pattern VkDeviceGroupPresentModeFlagsKHX ::
        VkFlags -> VkDeviceGroupPresentModeBitmaskKHX FlagMask

pattern VkDeviceGroupPresentModeFlagsKHX n =
        VkDeviceGroupPresentModeBitmaskKHX n

deriving instance
         Bits (VkDeviceGroupPresentModeBitmaskKHX FlagMask)

deriving instance
         FiniteBits (VkDeviceGroupPresentModeBitmaskKHX FlagMask)

deriving instance
         Integral (VkDeviceGroupPresentModeBitmaskKHX FlagMask)

deriving instance Num (VkDeviceGroupPresentModeBitmaskKHX FlagMask)

deriving instance
         Bounded (VkDeviceGroupPresentModeBitmaskKHX FlagMask)

deriving instance
         Enum (VkDeviceGroupPresentModeBitmaskKHX FlagMask)

deriving instance
         Real (VkDeviceGroupPresentModeBitmaskKHX FlagMask)

instance Show (VkDeviceGroupPresentModeBitmaskKHX a) where
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX
          = showString "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX"
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX
          = showString "VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX"
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX
          = showString "VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX"
        showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX
          = showString
              "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX"
        showsPrec p (VkDeviceGroupPresentModeBitmaskKHX x)
          = showParen (p >= 11)
              (showString "VkDeviceGroupPresentModeBitmaskKHX " . showsPrec 11 x)

instance Read (VkDeviceGroupPresentModeBitmaskKHX a) where
        readPrec
          = parens
              (choose
                 [("VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX),
                  ("VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX),
                  ("VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX),
                  ("VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX",
                   pure VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX)]
                 +++
                 prec 10
                   (expectP (Ident "VkDeviceGroupPresentModeBitmaskKHX") >>
                      (VkDeviceGroupPresentModeBitmaskKHX <$> step readPrec)))

-- | Present from local memory
--
--   bitpos = @0@
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX ::
        VkDeviceGroupPresentModeBitmaskKHX a

pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX =
        VkDeviceGroupPresentModeBitmaskKHX 1

-- | Present from remote memory
--
--   bitpos = @1@
pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX ::
        VkDeviceGroupPresentModeBitmaskKHX a

pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX =
        VkDeviceGroupPresentModeBitmaskKHX 2

-- | Present sum of local and/or remote memory
--
--   bitpos = @2@
pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX ::
        VkDeviceGroupPresentModeBitmaskKHX a

pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX =
        VkDeviceGroupPresentModeBitmaskKHX 4

-- | Each physical device presents from local memory
--
--   bitpos = @3@
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX ::
        VkDeviceGroupPresentModeBitmaskKHX a

pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX =
        VkDeviceGroupPresentModeBitmaskKHX 8
