#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_KHR_external_fence
       (-- * Vulkan extension: @VK_KHR_external_fence@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @114@
        --
        -- Required extensions: 'VK_KHR_external_fence_capabilities'.
        --

        -- ** Required extensions: 'VK_KHR_external_fence_capabilities'.
        VkExportFenceCreateInfoKHR(..),
        VK_KHR_EXTERNAL_FENCE_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkExternalFenceHandleTypeFlagsKHR,
                                                   VkStructureType,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExportFenceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalFenceHandleTypeFlagsKHR handleTypes;
--   > } VkExportFenceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExportFenceCreateInfoKHR.html VkExportFenceCreateInfoKHR registry at www.khronos.org>
data VkExportFenceCreateInfoKHR = VkExportFenceCreateInfoKHR## ByteArray##

instance Eq VkExportFenceCreateInfoKHR where
        (VkExportFenceCreateInfoKHR## a) == (VkExportFenceCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExportFenceCreateInfoKHR where
        (VkExportFenceCreateInfoKHR## a) `compare`
          (VkExportFenceCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExportFenceCreateInfoKHR where
        sizeOf ~_ = #{size VkExportFenceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExportFenceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExportFenceCreateInfoKHR),
            I## a <- alignment (undefined :: VkExportFenceCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExportFenceCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExportFenceCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkExportFenceCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExportFenceCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExportFenceCreateInfoKHR),
            I## a <- alignment (undefined :: VkExportFenceCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExportFenceCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExportFenceCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExportFenceCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExportFenceCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExportFenceCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExportFenceCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkExportFenceCreateInfoKHR
         where
        type VkSTypeMType VkExportFenceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportFenceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportFenceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportFenceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportFenceCreateInfoKHR where
        type FieldType "sType" VkExportFenceCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkExportFenceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkExportFenceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExportFenceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkExportFenceCreateInfoKHR
         where
        type VkPNextMType VkExportFenceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportFenceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportFenceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportFenceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportFenceCreateInfoKHR where
        type FieldType "pNext" VkExportFenceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportFenceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkExportFenceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExportFenceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExportFenceCreateInfoKHR where
        type VkHandleTypesMType VkExportFenceCreateInfoKHR =
             VkExternalFenceHandleTypeFlagsKHR

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfoKHR, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExportFenceCreateInfoKHR, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExportFenceCreateInfoKHR, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExportFenceCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportFenceCreateInfoKHR where
        type FieldType "handleTypes" VkExportFenceCreateInfoKHR =
             VkExternalFenceHandleTypeFlagsKHR
        type FieldOptional "handleTypes" VkExportFenceCreateInfoKHR = 'True -- ' closing tick for hsc2hs

instance CanReadField "handleTypes" VkExportFenceCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes" VkExportFenceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExportFenceCreateInfoKHR where
        showsPrec d x
          = showString "VkExportFenceCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_FENCE_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME

_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME #-}
_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  = Ptr "VK_KHR_external_fence\NUL"##

is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME #-}
is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME = "VK_KHR_external_fence"

pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR =
        VkStructureType 1000113000
