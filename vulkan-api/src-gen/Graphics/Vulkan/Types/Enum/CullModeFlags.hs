{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.CullModeFlags
       (VkCullModeBitmask(VkCullModeBitmask, VkCullModeFlags,
                          VkCullModeFlagBits, VK_CULL_MODE_NONE, VK_CULL_MODE_FRONT_BIT,
                          VK_CULL_MODE_BACK_BIT, VK_CULL_MODE_FRONT_AND_BACK),
        VkCullModeFlags, VkCullModeFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkCullModeBitmask (a ::
                             FlagType) = VkCullModeBitmask VkFlags
                                         deriving (Eq, Ord, Storable)

type VkCullModeFlags = VkCullModeBitmask FlagMask

type VkCullModeFlagBits = VkCullModeBitmask FlagBit

pattern VkCullModeFlagBits :: VkFlags -> VkCullModeBitmask FlagBit

pattern VkCullModeFlagBits n = VkCullModeBitmask n

pattern VkCullModeFlags :: VkFlags -> VkCullModeBitmask FlagMask

pattern VkCullModeFlags n = VkCullModeBitmask n

deriving instance Bits (VkCullModeBitmask FlagMask)

deriving instance FiniteBits (VkCullModeBitmask FlagMask)

instance Show (VkCullModeBitmask a) where
    showsPrec _ VK_CULL_MODE_NONE = showString "VK_CULL_MODE_NONE"
    showsPrec _ VK_CULL_MODE_FRONT_BIT
      = showString "VK_CULL_MODE_FRONT_BIT"
    showsPrec _ VK_CULL_MODE_BACK_BIT
      = showString "VK_CULL_MODE_BACK_BIT"
    showsPrec _ VK_CULL_MODE_FRONT_AND_BACK
      = showString "VK_CULL_MODE_FRONT_AND_BACK"
    showsPrec p (VkCullModeBitmask x)
      = showParen (p >= 11)
          (showString "VkCullModeBitmask " . showsPrec 11 x)

instance Read (VkCullModeBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_CULL_MODE_NONE", pure VK_CULL_MODE_NONE),
              ("VK_CULL_MODE_FRONT_BIT", pure VK_CULL_MODE_FRONT_BIT),
              ("VK_CULL_MODE_BACK_BIT", pure VK_CULL_MODE_BACK_BIT),
              ("VK_CULL_MODE_FRONT_AND_BACK", pure VK_CULL_MODE_FRONT_AND_BACK)]
             +++
             prec 10
               (expectP (Ident "VkCullModeBitmask") >>
                  (VkCullModeBitmask <$> step readPrec)))

pattern VK_CULL_MODE_NONE :: VkCullModeBitmask a

pattern VK_CULL_MODE_NONE = VkCullModeBitmask 0

-- | bitpos = @0@
pattern VK_CULL_MODE_FRONT_BIT :: VkCullModeBitmask a

pattern VK_CULL_MODE_FRONT_BIT = VkCullModeBitmask 1

-- | bitpos = @1@
pattern VK_CULL_MODE_BACK_BIT :: VkCullModeBitmask a

pattern VK_CULL_MODE_BACK_BIT = VkCullModeBitmask 2

pattern VK_CULL_MODE_FRONT_AND_BACK :: VkCullModeBitmask a

pattern VK_CULL_MODE_FRONT_AND_BACK = VkCullModeBitmask 3
