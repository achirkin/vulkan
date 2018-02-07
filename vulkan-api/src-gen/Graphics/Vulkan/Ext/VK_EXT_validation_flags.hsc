#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_validation_flags
       (-- * Vulkan extension: @VK_EXT_validation_flags@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobin Ehlis @tobine@
        --
        -- author: @GOOGLE@
        --
        -- type: @instance@
        --
        -- Extension number: @62@
        VkValidationFlagsEXT(..), VK_EXT_VALIDATION_FLAGS_SPEC_VERSION,
        pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION,
        VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME,
        pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkStructureType (..), Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkValidationFlagsEXT = VkValidationFlagsEXT## ByteArray##

instance Eq VkValidationFlagsEXT where
        (VkValidationFlagsEXT## a) == (VkValidationFlagsEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkValidationFlagsEXT where
        (VkValidationFlagsEXT## a) `compare` (VkValidationFlagsEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkValidationFlagsEXT where
        sizeOf ~_ = #{size VkValidationFlagsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkValidationFlagsEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkValidationFlagsEXT),
            I## a <- alignment (undefined :: VkValidationFlagsEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkValidationFlagsEXT## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkValidationFlagsEXT## ba)
          | I## n <- sizeOf (undefined :: VkValidationFlagsEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkValidationFlagsEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkValidationFlagsEXT),
            I## a <- alignment (undefined :: VkValidationFlagsEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkValidationFlagsEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkValidationFlagsEXT## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkValidationFlagsEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkValidationFlagsEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkValidationFlagsEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkValidationFlagsEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkValidationFlagsEXT where
        type VkSTypeMType VkValidationFlagsEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkValidationFlagsEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkValidationFlagsEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkValidationFlagsEXT, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkValidationFlagsEXT where
        type VkPNextMType VkValidationFlagsEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkValidationFlagsEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkValidationFlagsEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkValidationFlagsEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkDisabledValidationCheckCount VkValidationFlagsEXT where
        type VkDisabledValidationCheckCountMType VkValidationFlagsEXT =
             Word32

        {-# NOINLINE vkDisabledValidationCheckCount #-}
        vkDisabledValidationCheckCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, disabledValidationCheckCount})

        {-# INLINE vkDisabledValidationCheckCountByteOffset #-}
        vkDisabledValidationCheckCountByteOffset ~_
          = #{offset VkValidationFlagsEXT, disabledValidationCheckCount}

        {-# INLINE readVkDisabledValidationCheckCount #-}
        readVkDisabledValidationCheckCount p
          = peekByteOff p #{offset VkValidationFlagsEXT, disabledValidationCheckCount}

        {-# INLINE writeVkDisabledValidationCheckCount #-}
        writeVkDisabledValidationCheckCount p
          = pokeByteOff p #{offset VkValidationFlagsEXT, disabledValidationCheckCount}

instance {-# OVERLAPPING #-}
         HasVkPDisabledValidationChecks VkValidationFlagsEXT where
        type VkPDisabledValidationChecksMType VkValidationFlagsEXT =
             Ptr VkValidationCheckEXT

        {-# NOINLINE vkPDisabledValidationChecks #-}
        vkPDisabledValidationChecks x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, pDisabledValidationChecks})

        {-# INLINE vkPDisabledValidationChecksByteOffset #-}
        vkPDisabledValidationChecksByteOffset ~_
          = #{offset VkValidationFlagsEXT, pDisabledValidationChecks}

        {-# INLINE readVkPDisabledValidationChecks #-}
        readVkPDisabledValidationChecks p
          = peekByteOff p #{offset VkValidationFlagsEXT, pDisabledValidationChecks}

        {-# INLINE writeVkPDisabledValidationChecks #-}
        writeVkPDisabledValidationChecks p
          = pokeByteOff p #{offset VkValidationFlagsEXT, pDisabledValidationChecks}

instance Show VkValidationFlagsEXT where
        showsPrec d x
          = showString "VkValidationFlagsEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDisabledValidationCheckCount = " .
                            showsPrec d (vkDisabledValidationCheckCount x) .
                              showString ", " .
                                showString "vkPDisabledValidationChecks = " .
                                  showsPrec d (vkPDisabledValidationChecks x) . showChar '}'

pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION = 1

type VK_EXT_VALIDATION_FLAGS_SPEC_VERSION = 1

pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME :: CString

pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME <-
        (is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME -> True)
  where VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
          = _VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME

_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME #-}
_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
  = Ptr "VK_EXT_validation_flags\NUL"##

is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME #-}
is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
  = (_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME ==)

type VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME =
     "VK_EXT_validation_flags"

pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT =
        VkStructureType 1000061000
