#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_viewport_swizzle
       (-- * Vulkan extension: @VK_NV_viewport_swizzle@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @99@
        VkViewportSwizzleNV(..),
        VkPipelineViewportSwizzleStateCreateInfoNV(..),
        VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION,
        pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION,
        VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME,
        pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkPipelineViewportSwizzleStateCreateFlagsNV,
                                                   VkStructureType,
                                                   VkStructureType (..),
                                                   VkViewportCoordinateSwizzleNV,
                                                   Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkViewportSwizzleNV = VkViewportSwizzleNV## ByteArray##

instance Eq VkViewportSwizzleNV where
        (VkViewportSwizzleNV## a) == (VkViewportSwizzleNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkViewportSwizzleNV where
        (VkViewportSwizzleNV## a) `compare` (VkViewportSwizzleNV## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkViewportSwizzleNV where
        sizeOf ~_ = #{size VkViewportSwizzleNV}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViewportSwizzleNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkViewportSwizzleNV),
            I## a <- alignment (undefined :: VkViewportSwizzleNV) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkViewportSwizzleNV## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkViewportSwizzleNV## ba)
          | I## n <- sizeOf (undefined :: VkViewportSwizzleNV) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkViewportSwizzleNV where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkViewportSwizzleNV),
            I## a <- alignment (undefined :: VkViewportSwizzleNV) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkViewportSwizzleNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkViewportSwizzleNV## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkViewportSwizzleNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkViewportSwizzleNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkViewportSwizzleNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkViewportSwizzleNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkX VkViewportSwizzleNV where
        type VkXMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkViewportSwizzleNV, x}

        {-# INLINE readVkX #-}
        readVkX p
          = peekByteOff p #{offset VkViewportSwizzleNV, x}

        {-# INLINE writeVkX #-}
        writeVkX p
          = pokeByteOff p #{offset VkViewportSwizzleNV, x}

instance {-# OVERLAPPING #-} HasVkY VkViewportSwizzleNV where
        type VkYMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkViewportSwizzleNV, y}

        {-# INLINE readVkY #-}
        readVkY p
          = peekByteOff p #{offset VkViewportSwizzleNV, y}

        {-# INLINE writeVkY #-}
        writeVkY p
          = pokeByteOff p #{offset VkViewportSwizzleNV, y}

instance {-# OVERLAPPING #-} HasVkZ VkViewportSwizzleNV where
        type VkZMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkZ #-}
        vkZ x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, z})

        {-# INLINE vkZByteOffset #-}
        vkZByteOffset ~_ = #{offset VkViewportSwizzleNV, z}

        {-# INLINE readVkZ #-}
        readVkZ p
          = peekByteOff p #{offset VkViewportSwizzleNV, z}

        {-# INLINE writeVkZ #-}
        writeVkZ p
          = pokeByteOff p #{offset VkViewportSwizzleNV, z}

instance {-# OVERLAPPING #-} HasVkW VkViewportSwizzleNV where
        type VkWMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkW #-}
        vkW x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, w})

        {-# INLINE vkWByteOffset #-}
        vkWByteOffset ~_ = #{offset VkViewportSwizzleNV, w}

        {-# INLINE readVkW #-}
        readVkW p
          = peekByteOff p #{offset VkViewportSwizzleNV, w}

        {-# INLINE writeVkW #-}
        writeVkW p
          = pokeByteOff p #{offset VkViewportSwizzleNV, w}

instance Show VkViewportSwizzleNV where
        showsPrec d x
          = showString "VkViewportSwizzleNV {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " .
                      showsPrec d (vkY x) .
                        showString ", " .
                          showString "vkZ = " .
                            showsPrec d (vkZ x) .
                              showString ", " .
                                showString "vkW = " . showsPrec d (vkW x) . showChar '}'

data VkPipelineViewportSwizzleStateCreateInfoNV = VkPipelineViewportSwizzleStateCreateInfoNV## ByteArray##

instance Eq VkPipelineViewportSwizzleStateCreateInfoNV where
        (VkPipelineViewportSwizzleStateCreateInfoNV## a) ==
          (VkPipelineViewportSwizzleStateCreateInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPipelineViewportSwizzleStateCreateInfoNV where
        (VkPipelineViewportSwizzleStateCreateInfoNV## a) `compare`
          (VkPipelineViewportSwizzleStateCreateInfoNV## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPipelineViewportSwizzleStateCreateInfoNV where
        sizeOf ~_
          = #{size VkPipelineViewportSwizzleStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineViewportSwizzleStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPipelineViewportSwizzleStateCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkPipelineViewportSwizzleStateCreateInfoNV)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPipelineViewportSwizzleStateCreateInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPipelineViewportSwizzleStateCreateInfoNV## ba)
          | I## n <- sizeOf
                      (undefined :: VkPipelineViewportSwizzleStateCreateInfoNV)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPipelineViewportSwizzleStateCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkPipelineViewportSwizzleStateCreateInfoNV)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPipelineViewportSwizzleStateCreateInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPipelineViewportSwizzleStateCreateInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPipelineViewportSwizzleStateCreateInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPipelineViewportSwizzleStateCreateInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPipelineViewportSwizzleStateCreateInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPipelineViewportSwizzleStateCreateInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineViewportSwizzleStateCreateInfoNV where
        type VkSTypeMType VkPipelineViewportSwizzleStateCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineViewportSwizzleStateCreateInfoNV where
        type VkPNextMType VkPipelineViewportSwizzleStateCreateInfoNV =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineViewportSwizzleStateCreateInfoNV where
        type VkFlagsMType VkPipelineViewportSwizzleStateCreateInfoNV =
             VkPipelineViewportSwizzleStateCreateFlagsNV

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         HasVkViewportCount VkPipelineViewportSwizzleStateCreateInfoNV where
        type VkViewportCountMType
               VkPipelineViewportSwizzleStateCreateInfoNV
             = Word32

        {-# NOINLINE vkViewportCount #-}
        vkViewportCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount})

        {-# INLINE vkViewportCountByteOffset #-}
        vkViewportCountByteOffset ~_
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

        {-# INLINE readVkViewportCount #-}
        readVkViewportCount p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

        {-# INLINE writeVkViewportCount #-}
        writeVkViewportCount p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         HasVkPViewportSwizzles VkPipelineViewportSwizzleStateCreateInfoNV
         where
        type VkPViewportSwizzlesMType
               VkPipelineViewportSwizzleStateCreateInfoNV
             = Ptr VkViewportSwizzleNV

        {-# NOINLINE vkPViewportSwizzles #-}
        vkPViewportSwizzles x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles})

        {-# INLINE vkPViewportSwizzlesByteOffset #-}
        vkPViewportSwizzlesByteOffset ~_
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

        {-# INLINE readVkPViewportSwizzles #-}
        readVkPViewportSwizzles p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

        {-# INLINE writeVkPViewportSwizzles #-}
        writeVkPViewportSwizzles p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

instance Show VkPipelineViewportSwizzleStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineViewportSwizzleStateCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkViewportCount = " .
                                  showsPrec d (vkViewportCount x) .
                                    showString ", " .
                                      showString "vkPViewportSwizzles = " .
                                        showsPrec d (vkPViewportSwizzles x) . showChar '}'

pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1

type VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1

pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: CString

pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME <-
        (is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME -> True)
  where VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
          = _VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME

_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: CString

{-# INLINE _VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME #-}
_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  = Ptr "VK_NV_viewport_swizzle\NUL"##

is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME #-}
is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  = (_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME ==)

type VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME =
     "VK_NV_viewport_swizzle"

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
        = VkStructureType 1000098000
