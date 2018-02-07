#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_fence_capabilities
       (-- * Vulkan extension: @VK_KHR_external_fence_capabilities@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @113@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceExternalFenceInfoKHR(..),
        VkExternalFencePropertiesKHR(..),
        VkPhysicalDeviceIDPropertiesKHR(..),
        vkGetPhysicalDeviceExternalFencePropertiesKHR,
        VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR,
        pattern VK_LUID_SIZE_KHR)
       where
import           Data.Int
import           Data.Void                                                  (Void)
import           Data.Word
import           Foreign.C.String                                           (CString)
import           Foreign.C.Types                                            (CChar (..),
                                                                             CFloat (..),
                                                                             CInt (..),
                                                                             CSize (..),
                                                                             CULong (..))
import           Foreign.Storable                                           (Storable (..))
import           GHC.ForeignPtr                                             (ForeignPtr (..),
                                                                             ForeignPtrContents (..),
                                                                             newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                                                    (Ptr (..))
import           GHC.Types                                                  (IO (..),
                                                                             Int (..))
import           Graphics.Vulkan.Base
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Core
import           Graphics.Vulkan.Ext.VK_KHR_external_semaphore_capabilities (VkPhysicalDeviceIDPropertiesKHR (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

data VkPhysicalDeviceExternalFenceInfoKHR = VkPhysicalDeviceExternalFenceInfoKHR## ByteArray##

instance Eq VkPhysicalDeviceExternalFenceInfoKHR where
        (VkPhysicalDeviceExternalFenceInfoKHR## a) ==
          (VkPhysicalDeviceExternalFenceInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalFenceInfoKHR where
        (VkPhysicalDeviceExternalFenceInfoKHR## a) `compare`
          (VkPhysicalDeviceExternalFenceInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalFenceInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalFenceInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalFenceInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalFenceInfoKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceExternalFenceInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceExternalFenceInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceExternalFenceInfoKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalFenceInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceExternalFenceInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalFenceInfoKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceExternalFenceInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceExternalFenceInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceExternalFenceInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceExternalFenceInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceExternalFenceInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceExternalFenceInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceExternalFenceInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceExternalFenceInfoKHR where
        type VkSTypeMType VkPhysicalDeviceExternalFenceInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceExternalFenceInfoKHR where
        type VkPNextMType VkPhysicalDeviceExternalFenceInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkPhysicalDeviceExternalFenceInfoKHR where
        type VkHandleTypeMType VkPhysicalDeviceExternalFenceInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType}

instance Show VkPhysicalDeviceExternalFenceInfoKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalFenceInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleType = " .
                            showsPrec d (vkHandleType x) . showChar '}'

data VkExternalFencePropertiesKHR = VkExternalFencePropertiesKHR## ByteArray##

instance Eq VkExternalFencePropertiesKHR where
        (VkExternalFencePropertiesKHR## a) ==
          (VkExternalFencePropertiesKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExternalFencePropertiesKHR where
        (VkExternalFencePropertiesKHR## a) `compare`
          (VkExternalFencePropertiesKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExternalFencePropertiesKHR where
        sizeOf ~_ = #{size VkExternalFencePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalFencePropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExternalFencePropertiesKHR),
            I## a <- alignment (undefined :: VkExternalFencePropertiesKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExternalFencePropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExternalFencePropertiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkExternalFencePropertiesKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExternalFencePropertiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExternalFencePropertiesKHR),
            I## a <- alignment (undefined :: VkExternalFencePropertiesKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExternalFencePropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExternalFencePropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExternalFencePropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExternalFencePropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExternalFencePropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExternalFencePropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalFencePropertiesKHR where
        type VkSTypeMType VkExternalFencePropertiesKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFencePropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalFencePropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalFencePropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalFencePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalFencePropertiesKHR where
        type VkPNextMType VkExternalFencePropertiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFencePropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalFencePropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalFencePropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalFencePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkExportFromImportedHandleTypes VkExternalFencePropertiesKHR
         where
        type VkExportFromImportedHandleTypesMType
               VkExternalFencePropertiesKHR
             = VkExternalFenceHandleTypeFlagsKHR

        {-# NOINLINE vkExportFromImportedHandleTypes #-}
        vkExportFromImportedHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes})

        {-# INLINE vkExportFromImportedHandleTypesByteOffset #-}
        vkExportFromImportedHandleTypesByteOffset ~_
          = #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes}

        {-# INLINE readVkExportFromImportedHandleTypes #-}
        readVkExportFromImportedHandleTypes p
          = peekByteOff p #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes}

        {-# INLINE writeVkExportFromImportedHandleTypes #-}
        writeVkExportFromImportedHandleTypes p
          = pokeByteOff p #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasVkCompatibleHandleTypes VkExternalFencePropertiesKHR where
        type VkCompatibleHandleTypesMType VkExternalFencePropertiesKHR =
             VkExternalFenceHandleTypeFlagsKHR

        {-# NOINLINE vkCompatibleHandleTypes #-}
        vkCompatibleHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes})

        {-# INLINE vkCompatibleHandleTypesByteOffset #-}
        vkCompatibleHandleTypesByteOffset ~_
          = #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes}

        {-# INLINE readVkCompatibleHandleTypes #-}
        readVkCompatibleHandleTypes p
          = peekByteOff p #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes}

        {-# INLINE writeVkCompatibleHandleTypes #-}
        writeVkCompatibleHandleTypes p
          = pokeByteOff p #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         HasVkExternalFenceFeatures VkExternalFencePropertiesKHR where
        type VkExternalFenceFeaturesMType VkExternalFencePropertiesKHR =
             VkExternalFenceFeatureFlagsKHR

        {-# NOINLINE vkExternalFenceFeatures #-}
        vkExternalFenceFeatures x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFencePropertiesKHR, externalFenceFeatures})

        {-# INLINE vkExternalFenceFeaturesByteOffset #-}
        vkExternalFenceFeaturesByteOffset ~_
          = #{offset VkExternalFencePropertiesKHR, externalFenceFeatures}

        {-# INLINE readVkExternalFenceFeatures #-}
        readVkExternalFenceFeatures p
          = peekByteOff p #{offset VkExternalFencePropertiesKHR, externalFenceFeatures}

        {-# INLINE writeVkExternalFenceFeatures #-}
        writeVkExternalFenceFeatures p
          = pokeByteOff p #{offset VkExternalFencePropertiesKHR, externalFenceFeatures}

instance Show VkExternalFencePropertiesKHR where
        showsPrec d x
          = showString "VkExternalFencePropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkExportFromImportedHandleTypes = " .
                            showsPrec d (vkExportFromImportedHandleTypes x) .
                              showString ", " .
                                showString "vkCompatibleHandleTypes = " .
                                  showsPrec d (vkCompatibleHandleTypes x) .
                                    showString ", " .
                                      showString "vkExternalFenceFeatures = " .
                                        showsPrec d (vkExternalFenceFeatures x) . showChar '}'

-- | > void vkGetPhysicalDeviceExternalFencePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfoKHR* pExternalFenceInfo
--   >     , VkExternalFencePropertiesKHR* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceExternalFencePropertiesKHR.html vkGetPhysicalDeviceExternalFencePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalFencePropertiesKHR"
               vkGetPhysicalDeviceExternalFencePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalFenceInfoKHR -- ^ pExternalFenceInfo
                                                          ->
                   Ptr VkExternalFencePropertiesKHR -- ^ pExternalFenceProperties
                                                    -> IO ()

pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME ::
        CString

pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME

_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME #-}
_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_KHR_external_fence_capabilities\NUL"##

is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME ::
                                                     CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME #-}
is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME =
     "VK_KHR_external_fence_capabilities"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR =
        VkStructureType 1000112000

pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR =
        VkStructureType 1000112001
