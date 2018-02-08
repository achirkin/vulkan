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
module Graphics.Vulkan.Ext.VK_KHR_external_semaphore
       (-- * Vulkan extension: @VK_KHR_external_semaphore@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @78@
        --
        -- Required extensions: 'VK_KHR_external_semaphore_capabilities'.
        --

        -- ** Required extensions: 'VK_KHR_external_semaphore_capabilities'.
        VkExportSemaphoreCreateInfoKHR(..),
        VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION,
        VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkExternalSemaphoreHandleTypeFlagsKHR,
                                                   VkStructureType,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExportSemaphoreCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalSemaphoreHandleTypeFlagsKHR handleTypes;
--   > } VkExportSemaphoreCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExportSemaphoreCreateInfoKHR.html VkExportSemaphoreCreateInfoKHR registry at www.khronos.org>
data VkExportSemaphoreCreateInfoKHR = VkExportSemaphoreCreateInfoKHR## ByteArray##

instance Eq VkExportSemaphoreCreateInfoKHR where
        (VkExportSemaphoreCreateInfoKHR## a) ==
          (VkExportSemaphoreCreateInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExportSemaphoreCreateInfoKHR where
        (VkExportSemaphoreCreateInfoKHR## a) `compare`
          (VkExportSemaphoreCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExportSemaphoreCreateInfoKHR where
        sizeOf ~_ = #{size VkExportSemaphoreCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportSemaphoreCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExportSemaphoreCreateInfoKHR),
            I## a <- alignment (undefined :: VkExportSemaphoreCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExportSemaphoreCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExportSemaphoreCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkExportSemaphoreCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExportSemaphoreCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExportSemaphoreCreateInfoKHR),
            I## a <- alignment (undefined :: VkExportSemaphoreCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExportSemaphoreCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExportSemaphoreCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExportSemaphoreCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExportSemaphoreCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExportSemaphoreCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExportSemaphoreCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExportSemaphoreCreateInfoKHR where
        type VkSTypeMType VkExportSemaphoreCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportSemaphoreCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportSemaphoreCreateInfoKHR where
        type FieldType "sType" VkExportSemaphoreCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportSemaphoreCreateInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkExportSemaphoreCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExportSemaphoreCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportSemaphoreCreateInfoKHR where
        type VkPNextMType VkExportSemaphoreCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportSemaphoreCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportSemaphoreCreateInfoKHR where
        type FieldType "pNext" VkExportSemaphoreCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportSemaphoreCreateInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkExportSemaphoreCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExportSemaphoreCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExportSemaphoreCreateInfoKHR where
        type VkHandleTypesMType VkExportSemaphoreCreateInfoKHR =
             VkExternalSemaphoreHandleTypeFlagsKHR

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfoKHR, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExportSemaphoreCreateInfoKHR, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfoKHR, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportSemaphoreCreateInfoKHR where
        type FieldType "handleTypes" VkExportSemaphoreCreateInfoKHR =
             VkExternalSemaphoreHandleTypeFlagsKHR
        type FieldOptional "handleTypes" VkExportSemaphoreCreateInfoKHR =
             'True -- ' closing tick for hsc2hs

instance CanReadField "handleTypes" VkExportSemaphoreCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes" VkExportSemaphoreCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExportSemaphoreCreateInfoKHR where
        showsPrec d x
          = showString "VkExportSemaphoreCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME

_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME #-}
_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
  = Ptr "VK_KHR_external_semaphore\NUL"##

is_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME #-}
is_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME =
     "VK_KHR_external_semaphore"

pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR =
        VkStructureType 1000077000
