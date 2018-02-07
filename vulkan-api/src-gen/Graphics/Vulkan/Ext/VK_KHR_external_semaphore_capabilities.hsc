#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_semaphore_capabilities
       (-- * Vulkan extension: @VK_KHR_external_semaphore_capabilities@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @77@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceExternalSemaphoreInfoKHR(..),
        VkExternalSemaphorePropertiesKHR(..),
        VkPhysicalDeviceIDPropertiesKHR(..),
        vkGetPhysicalDeviceExternalSemaphorePropertiesKHR,
        VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION,
        VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR,
        pattern VK_LUID_SIZE_KHR, VK_LUID_SIZE_KHR())
       where
import           Foreign.C.String                                        (CString)
import           Foreign.Storable                                        (Storable (..))
import           GHC.ForeignPtr                                          (ForeignPtr (..),
                                                                          ForeignPtrContents (..),
                                                                          newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                                                 (Ptr (..))
import           GHC.Types                                               (IO (..),
                                                                          Int (..))
import           Graphics.Vulkan.Common                                  (VkExternalSemaphoreFeatureFlagsKHR,
                                                                          VkExternalSemaphoreHandleTypeFlagBitsKHR,
                                                                          VkExternalSemaphoreHandleTypeFlagsKHR,
                                                                          VkPhysicalDevice,
                                                                          VkStructureType (..))
import           Graphics.Vulkan.Ext.VK_KHR_external_memory_capabilities (VkPhysicalDeviceIDPropertiesKHR (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

data VkPhysicalDeviceExternalSemaphoreInfoKHR = VkPhysicalDeviceExternalSemaphoreInfoKHR## ByteArray##

instance Eq VkPhysicalDeviceExternalSemaphoreInfoKHR where
        (VkPhysicalDeviceExternalSemaphoreInfoKHR## a) ==
          (VkPhysicalDeviceExternalSemaphoreInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalSemaphoreInfoKHR where
        (VkPhysicalDeviceExternalSemaphoreInfoKHR## a) `compare`
          (VkPhysicalDeviceExternalSemaphoreInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalSemaphoreInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalSemaphoreInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalSemaphoreInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalSemaphoreInfoKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceExternalSemaphoreInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceExternalSemaphoreInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceExternalSemaphoreInfoKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalSemaphoreInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalSemaphoreInfoKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceExternalSemaphoreInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceExternalSemaphoreInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceExternalSemaphoreInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceExternalSemaphoreInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceExternalSemaphoreInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceExternalSemaphoreInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceExternalSemaphoreInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceExternalSemaphoreInfoKHR where
        type VkSTypeMType VkPhysicalDeviceExternalSemaphoreInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceExternalSemaphoreInfoKHR where
        type VkPNextMType VkPhysicalDeviceExternalSemaphoreInfoKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkPhysicalDeviceExternalSemaphoreInfoKHR where
        type VkHandleTypeMType VkPhysicalDeviceExternalSemaphoreInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType}

instance Show VkPhysicalDeviceExternalSemaphoreInfoKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalSemaphoreInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleType = " .
                            showsPrec d (vkHandleType x) . showChar '}'

data VkExternalSemaphorePropertiesKHR = VkExternalSemaphorePropertiesKHR## ByteArray##

instance Eq VkExternalSemaphorePropertiesKHR where
        (VkExternalSemaphorePropertiesKHR## a) ==
          (VkExternalSemaphorePropertiesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExternalSemaphorePropertiesKHR where
        (VkExternalSemaphorePropertiesKHR## a) `compare`
          (VkExternalSemaphorePropertiesKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExternalSemaphorePropertiesKHR where
        sizeOf ~_ = #{size VkExternalSemaphorePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalSemaphorePropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExternalSemaphorePropertiesKHR),
            I## a <- alignment (undefined :: VkExternalSemaphorePropertiesKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExternalSemaphorePropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExternalSemaphorePropertiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkExternalSemaphorePropertiesKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExternalSemaphorePropertiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExternalSemaphorePropertiesKHR),
            I## a <- alignment (undefined :: VkExternalSemaphorePropertiesKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExternalSemaphorePropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExternalSemaphorePropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExternalSemaphorePropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExternalSemaphorePropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExternalSemaphorePropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExternalSemaphorePropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalSemaphorePropertiesKHR where
        type VkSTypeMType VkExternalSemaphorePropertiesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphorePropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalSemaphorePropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalSemaphorePropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalSemaphorePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalSemaphorePropertiesKHR where
        type VkPNextMType VkExternalSemaphorePropertiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphorePropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalSemaphorePropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalSemaphorePropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalSemaphorePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkExportFromImportedHandleTypes VkExternalSemaphorePropertiesKHR
         where
        type VkExportFromImportedHandleTypesMType
               VkExternalSemaphorePropertiesKHR
             = VkExternalSemaphoreHandleTypeFlagsKHR

        {-# NOINLINE vkExportFromImportedHandleTypes #-}
        vkExportFromImportedHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes})

        {-# INLINE vkExportFromImportedHandleTypesByteOffset #-}
        vkExportFromImportedHandleTypesByteOffset ~_
          = #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes}

        {-# INLINE readVkExportFromImportedHandleTypes #-}
        readVkExportFromImportedHandleTypes p
          = peekByteOff p #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes}

        {-# INLINE writeVkExportFromImportedHandleTypes #-}
        writeVkExportFromImportedHandleTypes p
          = pokeByteOff p #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasVkCompatibleHandleTypes VkExternalSemaphorePropertiesKHR where
        type VkCompatibleHandleTypesMType VkExternalSemaphorePropertiesKHR
             = VkExternalSemaphoreHandleTypeFlagsKHR

        {-# NOINLINE vkCompatibleHandleTypes #-}
        vkCompatibleHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes})

        {-# INLINE vkCompatibleHandleTypesByteOffset #-}
        vkCompatibleHandleTypesByteOffset ~_
          = #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes}

        {-# INLINE readVkCompatibleHandleTypes #-}
        readVkCompatibleHandleTypes p
          = peekByteOff p #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes}

        {-# INLINE writeVkCompatibleHandleTypes #-}
        writeVkCompatibleHandleTypes p
          = pokeByteOff p #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         HasVkExternalSemaphoreFeatures VkExternalSemaphorePropertiesKHR
         where
        type VkExternalSemaphoreFeaturesMType
               VkExternalSemaphorePropertiesKHR
             = VkExternalSemaphoreFeatureFlagsKHR

        {-# NOINLINE vkExternalSemaphoreFeatures #-}
        vkExternalSemaphoreFeatures x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures})

        {-# INLINE vkExternalSemaphoreFeaturesByteOffset #-}
        vkExternalSemaphoreFeaturesByteOffset ~_
          = #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures}

        {-# INLINE readVkExternalSemaphoreFeatures #-}
        readVkExternalSemaphoreFeatures p
          = peekByteOff p #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures}

        {-# INLINE writeVkExternalSemaphoreFeatures #-}
        writeVkExternalSemaphoreFeatures p
          = pokeByteOff p #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures}

instance Show VkExternalSemaphorePropertiesKHR where
        showsPrec d x
          = showString "VkExternalSemaphorePropertiesKHR {" .
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
                                      showString "vkExternalSemaphoreFeatures = " .
                                        showsPrec d (vkExternalSemaphoreFeatures x) . showChar '}'

-- | > void vkGetPhysicalDeviceExternalSemaphorePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalSemaphoreInfoKHR* pExternalSemaphoreInfo
--   >     , VkExternalSemaphorePropertiesKHR* pExternalSemaphoreProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceExternalSemaphorePropertiesKHR.html vkGetPhysicalDeviceExternalSemaphorePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR"
               vkGetPhysicalDeviceExternalSemaphorePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalSemaphoreInfoKHR -- ^ pExternalSemaphoreInfo
                                                              ->
                   Ptr VkExternalSemaphorePropertiesKHR -- ^ pExternalSemaphoreProperties
                                                        -> IO ()

pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME ::
        CString

pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME

_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
           #-}
_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_KHR_external_semaphore_capabilities\NUL"##

is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME ::
                                                         CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
           #-}
is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME =
     "VK_KHR_external_semaphore_capabilities"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR
        = VkStructureType 1000076000

pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR =
        VkStructureType 1000076001