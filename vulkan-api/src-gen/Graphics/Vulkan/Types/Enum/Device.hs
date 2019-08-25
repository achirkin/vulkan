{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Device
       (VkDeviceCreateFlagBits(..),
        VkDeviceEventTypeEXT(VkDeviceEventTypeEXT,
                             VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT),
        VkDeviceGroupPresentModeBitmaskKHR(VkDeviceGroupPresentModeBitmaskKHR,
                                           VkDeviceGroupPresentModeFlagsKHR,
                                           VkDeviceGroupPresentModeFlagBitsKHR,
                                           VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR,
                                           VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR,
                                           VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR,
                                           VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR),
        VkDeviceGroupPresentModeFlagsKHR,
        VkDeviceGroupPresentModeFlagBitsKHR,
        VkDeviceQueueCreateBitmask(VkDeviceQueueCreateBitmask,
                                   VkDeviceQueueCreateFlags, VkDeviceQueueCreateFlagBits),
        VkDeviceQueueCreateFlags, VkDeviceQueueCreateFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkDeviceCreateFlagBits = VkDeviceCreateFlagBits VkFlags
                                 deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDeviceCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDeviceCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceEventTypeEXT VkDeviceEventTypeEXT registry at www.khronos.org>
newtype VkDeviceEventTypeEXT = VkDeviceEventTypeEXT Int32
                               deriving (Eq, Ord, Enum, Storable)

instance Show VkDeviceEventTypeEXT where
    showsPrec _ VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT
      = showString "VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT"
    showsPrec p (VkDeviceEventTypeEXT x)
      = showParen (p >= 11)
          (showString "VkDeviceEventTypeEXT " . showsPrec 11 x)

instance Read VkDeviceEventTypeEXT where
    readPrec
      = parens
          (choose
             [("VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT",
               pure VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT)]
             +++
             prec 10
               (expectP (Ident "VkDeviceEventTypeEXT") >>
                  (VkDeviceEventTypeEXT <$> step readPrec)))

pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT ::
        VkDeviceEventTypeEXT

pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT =
        VkDeviceEventTypeEXT 0

newtype VkDeviceGroupPresentModeBitmaskKHR (a ::
                                              FlagType) = VkDeviceGroupPresentModeBitmaskKHR VkFlags
                                                          deriving (Eq, Ord, Storable)

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

newtype VkDeviceQueueCreateBitmask (a ::
                                      FlagType) = VkDeviceQueueCreateBitmask VkFlags
                                                  deriving (Eq, Ord, Storable)

type VkDeviceQueueCreateFlags = VkDeviceQueueCreateBitmask FlagMask

type VkDeviceQueueCreateFlagBits =
     VkDeviceQueueCreateBitmask FlagBit

pattern VkDeviceQueueCreateFlagBits ::
        VkFlags -> VkDeviceQueueCreateBitmask FlagBit

pattern VkDeviceQueueCreateFlagBits n =
        VkDeviceQueueCreateBitmask n

pattern VkDeviceQueueCreateFlags ::
        VkFlags -> VkDeviceQueueCreateBitmask FlagMask

pattern VkDeviceQueueCreateFlags n = VkDeviceQueueCreateBitmask n

deriving instance Bits (VkDeviceQueueCreateBitmask FlagMask)

deriving instance FiniteBits (VkDeviceQueueCreateBitmask FlagMask)

instance Show (VkDeviceQueueCreateBitmask a) where
    showsPrec p (VkDeviceQueueCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkDeviceQueueCreateBitmask " . showsPrec 11 x)

instance Read (VkDeviceQueueCreateBitmask a) where
    readPrec
      = parens
          (choose [] +++
             prec 10
               (expectP (Ident "VkDeviceQueueCreateBitmask") >>
                  (VkDeviceQueueCreateBitmask <$> step readPrec)))
