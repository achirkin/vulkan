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
module Graphics.Vulkan.Types.Enum.Command
       (VkCommandBufferLevel(VkCommandBufferLevel,
                             VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                             VK_COMMAND_BUFFER_LEVEL_SECONDARY),
        VkCommandBufferResetBitmask(VkCommandBufferResetBitmask,
                                    VkCommandBufferResetFlags, VkCommandBufferResetFlagBits,
                                    VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT),
        VkCommandBufferResetFlags, VkCommandBufferResetFlagBits,
        VkCommandBufferUsageBitmask(VkCommandBufferUsageBitmask,
                                    VkCommandBufferUsageFlags, VkCommandBufferUsageFlagBits,
                                    VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
                                    VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT,
                                    VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT),
        VkCommandBufferUsageFlags, VkCommandBufferUsageFlagBits,
        VkCommandPoolCreateBitmask(VkCommandPoolCreateBitmask,
                                   VkCommandPoolCreateFlags, VkCommandPoolCreateFlagBits,
                                   VK_COMMAND_POOL_CREATE_TRANSIENT_BIT,
                                   VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT),
        VkCommandPoolCreateFlags, VkCommandPoolCreateFlagBits,
        VkCommandPoolResetBitmask(VkCommandPoolResetBitmask,
                                  VkCommandPoolResetFlags, VkCommandPoolResetFlagBits,
                                  VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT),
        VkCommandPoolResetFlags, VkCommandPoolResetFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandBufferLevel VkCommandBufferLevel registry at www.khronos.org>
newtype VkCommandBufferLevel = VkCommandBufferLevel Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkCommandBufferLevel where
        showsPrec _ VK_COMMAND_BUFFER_LEVEL_PRIMARY
          = showString "VK_COMMAND_BUFFER_LEVEL_PRIMARY"
        showsPrec _ VK_COMMAND_BUFFER_LEVEL_SECONDARY
          = showString "VK_COMMAND_BUFFER_LEVEL_SECONDARY"
        showsPrec p (VkCommandBufferLevel x)
          = showParen (p >= 11)
              (showString "VkCommandBufferLevel " . showsPrec 11 x)

instance Read VkCommandBufferLevel where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_BUFFER_LEVEL_PRIMARY",
                   pure VK_COMMAND_BUFFER_LEVEL_PRIMARY),
                  ("VK_COMMAND_BUFFER_LEVEL_SECONDARY",
                   pure VK_COMMAND_BUFFER_LEVEL_SECONDARY)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandBufferLevel") >>
                      (VkCommandBufferLevel <$> step readPrec)))

pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY :: VkCommandBufferLevel

pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY :: VkCommandBufferLevel

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1

newtype VkCommandBufferResetBitmask (a ::
                                       FlagType) = VkCommandBufferResetBitmask VkFlags
                                                     deriving (Eq, Ord, Storable, Data, Generic)

type VkCommandBufferResetFlags =
     VkCommandBufferResetBitmask FlagMask

type VkCommandBufferResetFlagBits =
     VkCommandBufferResetBitmask FlagBit

pattern VkCommandBufferResetFlagBits ::
        VkFlags -> VkCommandBufferResetBitmask FlagBit

pattern VkCommandBufferResetFlagBits n =
        VkCommandBufferResetBitmask n

pattern VkCommandBufferResetFlags ::
        VkFlags -> VkCommandBufferResetBitmask FlagMask

pattern VkCommandBufferResetFlags n = VkCommandBufferResetBitmask n

deriving instance Bits (VkCommandBufferResetBitmask FlagMask)

deriving instance FiniteBits (VkCommandBufferResetBitmask FlagMask)

deriving instance Integral (VkCommandBufferResetBitmask FlagMask)

deriving instance Num (VkCommandBufferResetBitmask FlagMask)

deriving instance Bounded (VkCommandBufferResetBitmask FlagMask)

deriving instance Enum (VkCommandBufferResetBitmask FlagMask)

deriving instance Real (VkCommandBufferResetBitmask FlagMask)

instance Show (VkCommandBufferResetBitmask a) where
        showsPrec _ VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
          = showString "VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
        showsPrec p (VkCommandBufferResetBitmask x)
          = showParen (p >= 11)
              (showString "VkCommandBufferResetBitmask " . showsPrec 11 x)

instance Read (VkCommandBufferResetBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT",
                   pure VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandBufferResetBitmask") >>
                      (VkCommandBufferResetBitmask <$> step readPrec)))

-- | Release resources owned by the buffer
--
--   bitpos = @0@
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT ::
        VkCommandBufferResetBitmask a

pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT =
        VkCommandBufferResetBitmask 1

newtype VkCommandBufferUsageBitmask (a ::
                                       FlagType) = VkCommandBufferUsageBitmask VkFlags
                                                     deriving (Eq, Ord, Storable, Data, Generic)

type VkCommandBufferUsageFlags =
     VkCommandBufferUsageBitmask FlagMask

type VkCommandBufferUsageFlagBits =
     VkCommandBufferUsageBitmask FlagBit

pattern VkCommandBufferUsageFlagBits ::
        VkFlags -> VkCommandBufferUsageBitmask FlagBit

pattern VkCommandBufferUsageFlagBits n =
        VkCommandBufferUsageBitmask n

pattern VkCommandBufferUsageFlags ::
        VkFlags -> VkCommandBufferUsageBitmask FlagMask

pattern VkCommandBufferUsageFlags n = VkCommandBufferUsageBitmask n

deriving instance Bits (VkCommandBufferUsageBitmask FlagMask)

deriving instance FiniteBits (VkCommandBufferUsageBitmask FlagMask)

deriving instance Integral (VkCommandBufferUsageBitmask FlagMask)

deriving instance Num (VkCommandBufferUsageBitmask FlagMask)

deriving instance Bounded (VkCommandBufferUsageBitmask FlagMask)

deriving instance Enum (VkCommandBufferUsageBitmask FlagMask)

deriving instance Real (VkCommandBufferUsageBitmask FlagMask)

instance Show (VkCommandBufferUsageBitmask a) where
        showsPrec _ VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
          = showString "VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
        showsPrec _ VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
          = showString "VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
        showsPrec _ VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
          = showString "VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
        showsPrec p (VkCommandBufferUsageBitmask x)
          = showParen (p >= 11)
              (showString "VkCommandBufferUsageBitmask " . showsPrec 11 x)

instance Read (VkCommandBufferUsageBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT",
                   pure VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT),
                  ("VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT",
                   pure VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT),
                  ("VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT",
                   pure VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandBufferUsageBitmask") >>
                      (VkCommandBufferUsageBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT ::
        VkCommandBufferUsageBitmask a

pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT =
        VkCommandBufferUsageBitmask 1

-- | bitpos = @1@
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT ::
        VkCommandBufferUsageBitmask a

pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT =
        VkCommandBufferUsageBitmask 2

-- | Command buffer may be submitted/executed more than once simultaneously
--
--   bitpos = @2@
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT ::
        VkCommandBufferUsageBitmask a

pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT =
        VkCommandBufferUsageBitmask 4

newtype VkCommandPoolCreateBitmask (a ::
                                      FlagType) = VkCommandPoolCreateBitmask VkFlags
                                                    deriving (Eq, Ord, Storable, Data, Generic)

type VkCommandPoolCreateFlags = VkCommandPoolCreateBitmask FlagMask

type VkCommandPoolCreateFlagBits =
     VkCommandPoolCreateBitmask FlagBit

pattern VkCommandPoolCreateFlagBits ::
        VkFlags -> VkCommandPoolCreateBitmask FlagBit

pattern VkCommandPoolCreateFlagBits n =
        VkCommandPoolCreateBitmask n

pattern VkCommandPoolCreateFlags ::
        VkFlags -> VkCommandPoolCreateBitmask FlagMask

pattern VkCommandPoolCreateFlags n = VkCommandPoolCreateBitmask n

deriving instance Bits (VkCommandPoolCreateBitmask FlagMask)

deriving instance FiniteBits (VkCommandPoolCreateBitmask FlagMask)

deriving instance Integral (VkCommandPoolCreateBitmask FlagMask)

deriving instance Num (VkCommandPoolCreateBitmask FlagMask)

deriving instance Bounded (VkCommandPoolCreateBitmask FlagMask)

deriving instance Enum (VkCommandPoolCreateBitmask FlagMask)

deriving instance Real (VkCommandPoolCreateBitmask FlagMask)

instance Show (VkCommandPoolCreateBitmask a) where
        showsPrec _ VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
          = showString "VK_COMMAND_POOL_CREATE_TRANSIENT_BIT"
        showsPrec _ VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
          = showString "VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
        showsPrec p (VkCommandPoolCreateBitmask x)
          = showParen (p >= 11)
              (showString "VkCommandPoolCreateBitmask " . showsPrec 11 x)

instance Read (VkCommandPoolCreateBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_POOL_CREATE_TRANSIENT_BIT",
                   pure VK_COMMAND_POOL_CREATE_TRANSIENT_BIT),
                  ("VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT",
                   pure VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandPoolCreateBitmask") >>
                      (VkCommandPoolCreateBitmask <$> step readPrec)))

-- | Command buffers have a short lifetime
--
--   bitpos = @0@
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT ::
        VkCommandPoolCreateBitmask a

pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT =
        VkCommandPoolCreateBitmask 1

-- | Command buffers may release their memory individually
--
--   bitpos = @1@
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT ::
        VkCommandPoolCreateBitmask a

pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT =
        VkCommandPoolCreateBitmask 2

newtype VkCommandPoolResetBitmask (a ::
                                     FlagType) = VkCommandPoolResetBitmask VkFlags
                                                   deriving (Eq, Ord, Storable, Data, Generic)

type VkCommandPoolResetFlags = VkCommandPoolResetBitmask FlagMask

type VkCommandPoolResetFlagBits = VkCommandPoolResetBitmask FlagBit

pattern VkCommandPoolResetFlagBits ::
        VkFlags -> VkCommandPoolResetBitmask FlagBit

pattern VkCommandPoolResetFlagBits n = VkCommandPoolResetBitmask n

pattern VkCommandPoolResetFlags ::
        VkFlags -> VkCommandPoolResetBitmask FlagMask

pattern VkCommandPoolResetFlags n = VkCommandPoolResetBitmask n

deriving instance Bits (VkCommandPoolResetBitmask FlagMask)

deriving instance FiniteBits (VkCommandPoolResetBitmask FlagMask)

deriving instance Integral (VkCommandPoolResetBitmask FlagMask)

deriving instance Num (VkCommandPoolResetBitmask FlagMask)

deriving instance Bounded (VkCommandPoolResetBitmask FlagMask)

deriving instance Enum (VkCommandPoolResetBitmask FlagMask)

deriving instance Real (VkCommandPoolResetBitmask FlagMask)

instance Show (VkCommandPoolResetBitmask a) where
        showsPrec _ VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
          = showString "VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
        showsPrec p (VkCommandPoolResetBitmask x)
          = showParen (p >= 11)
              (showString "VkCommandPoolResetBitmask " . showsPrec 11 x)

instance Read (VkCommandPoolResetBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT",
                   pure VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkCommandPoolResetBitmask") >>
                      (VkCommandPoolResetBitmask <$> step readPrec)))

-- | Release resources owned by the pool
--
--   bitpos = @0@
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT ::
        VkCommandPoolResetBitmask a

pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT =
        VkCommandPoolResetBitmask 1
