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
module Graphics.Vulkan.Types.Enum.VkStencilFaceFlags
       (VkStencilFaceBitmask(VkStencilFaceBitmask, VkStencilFaceFlags,
                             VkStencilFaceFlagBits, VK_STENCIL_FACE_FRONT_BIT,
                             VK_STENCIL_FACE_BACK_BIT, VK_STENCIL_FRONT_AND_BACK),
        VkStencilFaceFlags, VkStencilFaceFlagBits)
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

newtype VkStencilFaceBitmask (a ::
                                FlagType) = VkStencilFaceBitmask VkFlags
                                              deriving (Eq, Ord, Storable, Data, Generic)

type VkStencilFaceFlags = VkStencilFaceBitmask FlagMask

type VkStencilFaceFlagBits = VkStencilFaceBitmask FlagBit

pattern VkStencilFaceFlagBits ::
        VkFlags -> VkStencilFaceBitmask FlagBit

pattern VkStencilFaceFlagBits n = VkStencilFaceBitmask n

pattern VkStencilFaceFlags ::
        VkFlags -> VkStencilFaceBitmask FlagMask

pattern VkStencilFaceFlags n = VkStencilFaceBitmask n

deriving instance Bits (VkStencilFaceBitmask FlagMask)

deriving instance FiniteBits (VkStencilFaceBitmask FlagMask)

deriving instance Integral (VkStencilFaceBitmask FlagMask)

deriving instance Num (VkStencilFaceBitmask FlagMask)

deriving instance Bounded (VkStencilFaceBitmask FlagMask)

deriving instance Enum (VkStencilFaceBitmask FlagMask)

deriving instance Real (VkStencilFaceBitmask FlagMask)

instance Show (VkStencilFaceBitmask a) where
        showsPrec _ VK_STENCIL_FACE_FRONT_BIT
          = showString "VK_STENCIL_FACE_FRONT_BIT"
        showsPrec _ VK_STENCIL_FACE_BACK_BIT
          = showString "VK_STENCIL_FACE_BACK_BIT"
        showsPrec _ VK_STENCIL_FRONT_AND_BACK
          = showString "VK_STENCIL_FRONT_AND_BACK"
        showsPrec p (VkStencilFaceBitmask x)
          = showParen (p >= 11)
              (showString "VkStencilFaceBitmask " . showsPrec 11 x)

instance Read (VkStencilFaceBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_STENCIL_FACE_FRONT_BIT", pure VK_STENCIL_FACE_FRONT_BIT),
                  ("VK_STENCIL_FACE_BACK_BIT", pure VK_STENCIL_FACE_BACK_BIT),
                  ("VK_STENCIL_FRONT_AND_BACK", pure VK_STENCIL_FRONT_AND_BACK)]
                 +++
                 prec 10
                   (expectP (Ident "VkStencilFaceBitmask") >>
                      (VkStencilFaceBitmask <$> step readPrec)))

-- | Front face
--
--   bitpos = @0@
pattern VK_STENCIL_FACE_FRONT_BIT :: VkStencilFaceBitmask a

pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceBitmask 1

-- | Back face
--
--   bitpos = @1@
pattern VK_STENCIL_FACE_BACK_BIT :: VkStencilFaceBitmask a

pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceBitmask 2

-- | Front and back faces
pattern VK_STENCIL_FRONT_AND_BACK :: VkStencilFaceBitmask a

pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceBitmask 3
