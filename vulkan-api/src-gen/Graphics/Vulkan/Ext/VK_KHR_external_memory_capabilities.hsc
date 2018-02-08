#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_memory_capabilities
       (-- * Vulkan extension: @VK_KHR_external_memory_capabilities@
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
        -- Extension number: @72@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkExternalMemoryPropertiesKHR(..),
        VkPhysicalDeviceExternalImageFormatInfoKHR(..),
        VkExternalImageFormatPropertiesKHR(..),
        VkPhysicalDeviceExternalBufferInfoKHR(..),
        VkExternalBufferPropertiesKHR(..),
        VkPhysicalDeviceIDPropertiesKHR(..),
        vkGetPhysicalDeviceExternalBufferPropertiesKHR,
        VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR,
        pattern VK_LUID_SIZE_KHR, VK_LUID_SIZE_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.TypeLits                     (CmpNat, KnownNat, natVal') -- ' closing tick for hsc2hs
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryPropertiesKHR {
--   >     VkExternalMemoryFeatureFlagsKHR  externalMemoryFeatures;
--   >     VkExternalMemoryHandleTypeFlagsKHR exportFromImportedHandleTypes;
--   >     VkExternalMemoryHandleTypeFlagsKHR compatibleHandleTypes;
--   > } VkExternalMemoryPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryPropertiesKHR.html VkExternalMemoryPropertiesKHR registry at www.khronos.org>
data VkExternalMemoryPropertiesKHR = VkExternalMemoryPropertiesKHR## ByteArray##

instance Eq VkExternalMemoryPropertiesKHR where
        (VkExternalMemoryPropertiesKHR## a) ==
          (VkExternalMemoryPropertiesKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryPropertiesKHR where
        (VkExternalMemoryPropertiesKHR## a) `compare`
          (VkExternalMemoryPropertiesKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryPropertiesKHR where
        sizeOf ~_ = #{size VkExternalMemoryPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryPropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExternalMemoryPropertiesKHR),
            I## a <- alignment (undefined :: VkExternalMemoryPropertiesKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExternalMemoryPropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExternalMemoryPropertiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkExternalMemoryPropertiesKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExternalMemoryPropertiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExternalMemoryPropertiesKHR),
            I## a <- alignment (undefined :: VkExternalMemoryPropertiesKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExternalMemoryPropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExternalMemoryPropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExternalMemoryPropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExternalMemoryPropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExternalMemoryPropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExternalMemoryPropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkExternalMemoryFeatures VkExternalMemoryPropertiesKHR where
        type VkExternalMemoryFeaturesMType VkExternalMemoryPropertiesKHR =
             VkExternalMemoryFeatureFlagsKHR

        {-# NOINLINE vkExternalMemoryFeatures #-}
        vkExternalMemoryFeatures x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures})

        {-# INLINE vkExternalMemoryFeaturesByteOffset #-}
        vkExternalMemoryFeaturesByteOffset ~_
          = #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures}

        {-# INLINE readVkExternalMemoryFeatures #-}
        readVkExternalMemoryFeatures p
          = peekByteOff p #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures}

        {-# INLINE writeVkExternalMemoryFeatures #-}
        writeVkExternalMemoryFeatures p
          = pokeByteOff p #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryFeatures" VkExternalMemoryPropertiesKHR
         where
        type FieldType "externalMemoryFeatures"
               VkExternalMemoryPropertiesKHR
             = VkExternalMemoryFeatureFlagsKHR
        type FieldOptional "externalMemoryFeatures"
               VkExternalMemoryPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "externalMemoryFeatures"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExternalMemoryFeatures

        {-# INLINE readField #-}
        readField = readVkExternalMemoryFeatures

instance {-# OVERLAPPING #-}
         HasVkExportFromImportedHandleTypes VkExternalMemoryPropertiesKHR
         where
        type VkExportFromImportedHandleTypesMType
               VkExternalMemoryPropertiesKHR
             = VkExternalMemoryHandleTypeFlagsKHR

        {-# NOINLINE vkExportFromImportedHandleTypes #-}
        vkExportFromImportedHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes})

        {-# INLINE vkExportFromImportedHandleTypesByteOffset #-}
        vkExportFromImportedHandleTypesByteOffset ~_
          = #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes}

        {-# INLINE readVkExportFromImportedHandleTypes #-}
        readVkExportFromImportedHandleTypes p
          = peekByteOff p #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes}

        {-# INLINE writeVkExportFromImportedHandleTypes #-}
        writeVkExportFromImportedHandleTypes p
          = pokeByteOff p #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "exportFromImportedHandleTypes"
           VkExternalMemoryPropertiesKHR
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalMemoryPropertiesKHR
             = VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalMemoryPropertiesKHR
             = 'True -- ' closing tick for hsc2hs

instance CanReadField "exportFromImportedHandleTypes"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExportFromImportedHandleTypes

        {-# INLINE readField #-}
        readField = readVkExportFromImportedHandleTypes

instance {-# OVERLAPPING #-}
         HasVkCompatibleHandleTypes VkExternalMemoryPropertiesKHR where
        type VkCompatibleHandleTypesMType VkExternalMemoryPropertiesKHR =
             VkExternalMemoryHandleTypeFlagsKHR

        {-# NOINLINE vkCompatibleHandleTypes #-}
        vkCompatibleHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes})

        {-# INLINE vkCompatibleHandleTypesByteOffset #-}
        vkCompatibleHandleTypesByteOffset ~_
          = #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes}

        {-# INLINE readVkCompatibleHandleTypes #-}
        readVkCompatibleHandleTypes p
          = peekByteOff p #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes}

        {-# INLINE writeVkCompatibleHandleTypes #-}
        writeVkCompatibleHandleTypes p
          = pokeByteOff p #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "compatibleHandleTypes" VkExternalMemoryPropertiesKHR
         where
        type FieldType "compatibleHandleTypes"
               VkExternalMemoryPropertiesKHR
             = VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "compatibleHandleTypes"
               VkExternalMemoryPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "compatibleHandleTypes"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCompatibleHandleTypes

        {-# INLINE readField #-}
        readField = readVkCompatibleHandleTypes

instance Show VkExternalMemoryPropertiesKHR where
        showsPrec d x
          = showString "VkExternalMemoryPropertiesKHR {" .
              showString "vkExternalMemoryFeatures = " .
                showsPrec d (vkExternalMemoryFeatures x) .
                  showString ", " .
                    showString "vkExportFromImportedHandleTypes = " .
                      showsPrec d (vkExportFromImportedHandleTypes x) .
                        showString ", " .
                          showString "vkCompatibleHandleTypes = " .
                            showsPrec d (vkCompatibleHandleTypes x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceExternalImageFormatInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   > } VkPhysicalDeviceExternalImageFormatInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceExternalImageFormatInfoKHR.html VkPhysicalDeviceExternalImageFormatInfoKHR registry at www.khronos.org>
data VkPhysicalDeviceExternalImageFormatInfoKHR = VkPhysicalDeviceExternalImageFormatInfoKHR## ByteArray##

instance Eq VkPhysicalDeviceExternalImageFormatInfoKHR where
        (VkPhysicalDeviceExternalImageFormatInfoKHR## a) ==
          (VkPhysicalDeviceExternalImageFormatInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalImageFormatInfoKHR where
        (VkPhysicalDeviceExternalImageFormatInfoKHR## a) `compare`
          (VkPhysicalDeviceExternalImageFormatInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalImageFormatInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalImageFormatInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalImageFormatInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalImageFormatInfoKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceExternalImageFormatInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceExternalImageFormatInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceExternalImageFormatInfoKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalImageFormatInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalImageFormatInfoKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceExternalImageFormatInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceExternalImageFormatInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceExternalImageFormatInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceExternalImageFormatInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceExternalImageFormatInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceExternalImageFormatInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceExternalImageFormatInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceExternalImageFormatInfoKHR where
        type VkSTypeMType VkPhysicalDeviceExternalImageFormatInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalImageFormatInfoKHR where
        type FieldType "sType" VkPhysicalDeviceExternalImageFormatInfoKHR =
             VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType"
           VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceExternalImageFormatInfoKHR where
        type VkPNextMType VkPhysicalDeviceExternalImageFormatInfoKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalImageFormatInfoKHR where
        type FieldType "pNext" VkPhysicalDeviceExternalImageFormatInfoKHR =
             Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext"
           VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleType VkPhysicalDeviceExternalImageFormatInfoKHR where
        type VkHandleTypeMType VkPhysicalDeviceExternalImageFormatInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        type FieldType "handleType"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = 'True -- ' closing tick for hsc2hs

instance CanReadField "handleType"
           VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType"
           VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance Show VkPhysicalDeviceExternalImageFormatInfoKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalImageFormatInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleType = " .
                            showsPrec d (vkHandleType x) . showChar '}'

-- | > typedef struct VkExternalImageFormatPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryPropertiesKHR externalMemoryProperties;
--   > } VkExternalImageFormatPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalImageFormatPropertiesKHR.html VkExternalImageFormatPropertiesKHR registry at www.khronos.org>
data VkExternalImageFormatPropertiesKHR = VkExternalImageFormatPropertiesKHR## ByteArray##

instance Eq VkExternalImageFormatPropertiesKHR where
        (VkExternalImageFormatPropertiesKHR## a) ==
          (VkExternalImageFormatPropertiesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExternalImageFormatPropertiesKHR where
        (VkExternalImageFormatPropertiesKHR## a) `compare`
          (VkExternalImageFormatPropertiesKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExternalImageFormatPropertiesKHR where
        sizeOf ~_ = #{size VkExternalImageFormatPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalImageFormatPropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExternalImageFormatPropertiesKHR),
            I## a <- alignment (undefined :: VkExternalImageFormatPropertiesKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExternalImageFormatPropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExternalImageFormatPropertiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkExternalImageFormatPropertiesKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExternalImageFormatPropertiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExternalImageFormatPropertiesKHR),
            I## a <- alignment (undefined :: VkExternalImageFormatPropertiesKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExternalImageFormatPropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExternalImageFormatPropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkExternalImageFormatPropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExternalImageFormatPropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExternalImageFormatPropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExternalImageFormatPropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalImageFormatPropertiesKHR where
        type VkSTypeMType VkExternalImageFormatPropertiesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalImageFormatPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalImageFormatPropertiesKHR where
        type FieldType "sType" VkExternalImageFormatPropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkExternalImageFormatPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalImageFormatPropertiesKHR where
        type VkPNextMType VkExternalImageFormatPropertiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalImageFormatPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalImageFormatPropertiesKHR where
        type FieldType "pNext" VkExternalImageFormatPropertiesKHR =
             Ptr Void
        type FieldOptional "pNext" VkExternalImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkExternalImageFormatPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkExternalMemoryProperties VkExternalImageFormatPropertiesKHR
         where
        type VkExternalMemoryPropertiesMType
               VkExternalImageFormatPropertiesKHR
             = VkExternalMemoryPropertiesKHR

        {-# NOINLINE vkExternalMemoryProperties #-}
        vkExternalMemoryProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties})

        {-# INLINE vkExternalMemoryPropertiesByteOffset #-}
        vkExternalMemoryPropertiesByteOffset ~_
          = #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}

        {-# INLINE readVkExternalMemoryProperties #-}
        readVkExternalMemoryProperties p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}

        {-# INLINE writeVkExternalMemoryProperties #-}
        writeVkExternalMemoryProperties p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryProperties"
           VkExternalImageFormatPropertiesKHR
         where
        type FieldType "externalMemoryProperties"
               VkExternalImageFormatPropertiesKHR
             = VkExternalMemoryPropertiesKHR
        type FieldOptional "externalMemoryProperties"
               VkExternalImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "externalMemoryProperties"
           VkExternalImageFormatPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExternalMemoryProperties

        {-# INLINE readField #-}
        readField = readVkExternalMemoryProperties

instance Show VkExternalImageFormatPropertiesKHR where
        showsPrec d x
          = showString "VkExternalImageFormatPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkExternalMemoryProperties = " .
                            showsPrec d (vkExternalMemoryProperties x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceExternalBufferInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBufferCreateFlags flags;
--   >     VkBufferUsageFlags               usage;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   > } VkPhysicalDeviceExternalBufferInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceExternalBufferInfoKHR.html VkPhysicalDeviceExternalBufferInfoKHR registry at www.khronos.org>
data VkPhysicalDeviceExternalBufferInfoKHR = VkPhysicalDeviceExternalBufferInfoKHR## ByteArray##

instance Eq VkPhysicalDeviceExternalBufferInfoKHR where
        (VkPhysicalDeviceExternalBufferInfoKHR## a) ==
          (VkPhysicalDeviceExternalBufferInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalBufferInfoKHR where
        (VkPhysicalDeviceExternalBufferInfoKHR## a) `compare`
          (VkPhysicalDeviceExternalBufferInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalBufferInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalBufferInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalBufferInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalBufferInfoKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceExternalBufferInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceExternalBufferInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceExternalBufferInfoKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalBufferInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceExternalBufferInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalBufferInfoKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceExternalBufferInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceExternalBufferInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceExternalBufferInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceExternalBufferInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceExternalBufferInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceExternalBufferInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceExternalBufferInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceExternalBufferInfoKHR where
        type VkSTypeMType VkPhysicalDeviceExternalBufferInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "sType" VkPhysicalDeviceExternalBufferInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceExternalBufferInfoKHR where
        type VkPNextMType VkPhysicalDeviceExternalBufferInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "pNext" VkPhysicalDeviceExternalBufferInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPhysicalDeviceExternalBufferInfoKHR where
        type VkFlagsMType VkPhysicalDeviceExternalBufferInfoKHR =
             VkBufferCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "flags" VkPhysicalDeviceExternalBufferInfoKHR =
             VkBufferCreateFlags
        type FieldOptional "flags" VkPhysicalDeviceExternalBufferInfoKHR =
             'True -- ' closing tick for hsc2hs

instance CanReadField "flags" VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkUsage VkPhysicalDeviceExternalBufferInfoKHR where
        type VkUsageMType VkPhysicalDeviceExternalBufferInfoKHR =
             VkBufferUsageFlags

        {-# NOINLINE vkUsage #-}
        vkUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage})

        {-# INLINE vkUsageByteOffset #-}
        vkUsageByteOffset ~_
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}

        {-# INLINE readVkUsage #-}
        readVkUsage p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}

        {-# INLINE writeVkUsage #-}
        writeVkUsage p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "usage" VkPhysicalDeviceExternalBufferInfoKHR =
             VkBufferUsageFlags
        type FieldOptional "usage" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "usage" VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkUsage

        {-# INLINE readField #-}
        readField = readVkUsage

instance CanWriteField "usage"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkUsage

instance {-# OVERLAPPING #-}
         HasVkHandleType VkPhysicalDeviceExternalBufferInfoKHR where
        type VkHandleTypeMType VkPhysicalDeviceExternalBufferInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "handleType" VkPhysicalDeviceExternalBufferInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalBufferInfoKHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "handleType"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance Show VkPhysicalDeviceExternalBufferInfoKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalBufferInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkUsage = " .
                                  showsPrec d (vkUsage x) .
                                    showString ", " .
                                      showString "vkHandleType = " .
                                        showsPrec d (vkHandleType x) . showChar '}'

-- | > typedef struct VkExternalBufferPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryPropertiesKHR    externalMemoryProperties;
--   > } VkExternalBufferPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalBufferPropertiesKHR.html VkExternalBufferPropertiesKHR registry at www.khronos.org>
data VkExternalBufferPropertiesKHR = VkExternalBufferPropertiesKHR## ByteArray##

instance Eq VkExternalBufferPropertiesKHR where
        (VkExternalBufferPropertiesKHR## a) ==
          (VkExternalBufferPropertiesKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExternalBufferPropertiesKHR where
        (VkExternalBufferPropertiesKHR## a) `compare`
          (VkExternalBufferPropertiesKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExternalBufferPropertiesKHR where
        sizeOf ~_ = #{size VkExternalBufferPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalBufferPropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExternalBufferPropertiesKHR),
            I## a <- alignment (undefined :: VkExternalBufferPropertiesKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExternalBufferPropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExternalBufferPropertiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkExternalBufferPropertiesKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExternalBufferPropertiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExternalBufferPropertiesKHR),
            I## a <- alignment (undefined :: VkExternalBufferPropertiesKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExternalBufferPropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExternalBufferPropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExternalBufferPropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExternalBufferPropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExternalBufferPropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExternalBufferPropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalBufferPropertiesKHR where
        type VkSTypeMType VkExternalBufferPropertiesKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalBufferPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalBufferPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalBufferPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalBufferPropertiesKHR where
        type FieldType "sType" VkExternalBufferPropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalBufferPropertiesKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkExternalBufferPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalBufferPropertiesKHR where
        type VkPNextMType VkExternalBufferPropertiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalBufferPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalBufferPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalBufferPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalBufferPropertiesKHR where
        type FieldType "pNext" VkExternalBufferPropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkExternalBufferPropertiesKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkExternalBufferPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkExternalMemoryProperties VkExternalBufferPropertiesKHR where
        type VkExternalMemoryPropertiesMType VkExternalBufferPropertiesKHR
             = VkExternalMemoryPropertiesKHR

        {-# NOINLINE vkExternalMemoryProperties #-}
        vkExternalMemoryProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferPropertiesKHR, externalMemoryProperties})

        {-# INLINE vkExternalMemoryPropertiesByteOffset #-}
        vkExternalMemoryPropertiesByteOffset ~_
          = #{offset VkExternalBufferPropertiesKHR, externalMemoryProperties}

        {-# INLINE readVkExternalMemoryProperties #-}
        readVkExternalMemoryProperties p
          = peekByteOff p #{offset VkExternalBufferPropertiesKHR, externalMemoryProperties}

        {-# INLINE writeVkExternalMemoryProperties #-}
        writeVkExternalMemoryProperties p
          = pokeByteOff p #{offset VkExternalBufferPropertiesKHR, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryProperties" VkExternalBufferPropertiesKHR
         where
        type FieldType "externalMemoryProperties"
               VkExternalBufferPropertiesKHR
             = VkExternalMemoryPropertiesKHR
        type FieldOptional "externalMemoryProperties"
               VkExternalBufferPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "externalMemoryProperties"
           VkExternalBufferPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExternalMemoryProperties

        {-# INLINE readField #-}
        readField = readVkExternalMemoryProperties

instance Show VkExternalBufferPropertiesKHR where
        showsPrec d x
          = showString "VkExternalBufferPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkExternalMemoryProperties = " .
                            showsPrec d (vkExternalMemoryProperties x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceIDPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint8_t                          deviceUUID[VK_UUID_SIZE];
--   >     uint8_t                          driverUUID[VK_UUID_SIZE];
--   >     uint8_t                          deviceLUID[VK_LUID_SIZE_KHR];
--   >     uint32_t                         deviceNodeMask;
--   >     VkBool32                         deviceLUIDValid;
--   > } VkPhysicalDeviceIDPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceIDPropertiesKHR.html VkPhysicalDeviceIDPropertiesKHR registry at www.khronos.org>
data VkPhysicalDeviceIDPropertiesKHR = VkPhysicalDeviceIDPropertiesKHR## ByteArray##

instance Eq VkPhysicalDeviceIDPropertiesKHR where
        (VkPhysicalDeviceIDPropertiesKHR## a) ==
          (VkPhysicalDeviceIDPropertiesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceIDPropertiesKHR where
        (VkPhysicalDeviceIDPropertiesKHR## a) `compare`
          (VkPhysicalDeviceIDPropertiesKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceIDPropertiesKHR where
        sizeOf ~_ = #{size VkPhysicalDeviceIDPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceIDPropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceIDPropertiesKHR),
            I## a <- alignment (undefined :: VkPhysicalDeviceIDPropertiesKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceIDPropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceIDPropertiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceIDPropertiesKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceIDPropertiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceIDPropertiesKHR),
            I## a <- alignment (undefined :: VkPhysicalDeviceIDPropertiesKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceIDPropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceIDPropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkPhysicalDeviceIDPropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceIDPropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceIDPropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceIDPropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceIDPropertiesKHR where
        type VkSTypeMType VkPhysicalDeviceIDPropertiesKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "sType" VkPhysicalDeviceIDPropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceIDPropertiesKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkPhysicalDeviceIDPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceIDPropertiesKHR where
        type VkPNextMType VkPhysicalDeviceIDPropertiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "pNext" VkPhysicalDeviceIDPropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceIDPropertiesKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkPhysicalDeviceIDPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkDeviceUUIDArray VkPhysicalDeviceIDPropertiesKHR where
        type VkDeviceUUIDArrayMType VkPhysicalDeviceIDPropertiesKHR = Word8

        {-# NOINLINE vkDeviceUUIDArray #-}
        vkDeviceUUIDArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word8) +
                    #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID}))

        {-# INLINE vkDeviceUUIDArrayByteOffset #-}
        vkDeviceUUIDArrayByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID}

        {-# INLINE readVkDeviceUUIDArray #-}
        readVkDeviceUUIDArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID})

        {-# INLINE writeVkDeviceUUIDArray #-}
        writeVkDeviceUUIDArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, deviceUUID})

instance {-# OVERLAPPING #-}
         HasField "deviceUUID" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "deviceUUID" VkPhysicalDeviceIDPropertiesKHR = Word8
        type FieldOptional "deviceUUID" VkPhysicalDeviceIDPropertiesKHR =
             'False -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "deviceUUID" idx VkPhysicalDeviceIDPropertiesKHR) =>
         CanReadFieldArray "deviceUUID" idx VkPhysicalDeviceIDPropertiesKHR
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 0 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 1 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 2 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceUUID" 3 VkPhysicalDeviceIDPropertiesKHR
                       #-}
        type FieldArrayLength "deviceUUID" VkPhysicalDeviceIDPropertiesKHR
             = VK_UUID_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkDeviceUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkDeviceUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkDriverUUIDArray VkPhysicalDeviceIDPropertiesKHR where
        type VkDriverUUIDArrayMType VkPhysicalDeviceIDPropertiesKHR = Word8

        {-# NOINLINE vkDriverUUIDArray #-}
        vkDriverUUIDArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word8) +
                    #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID}))

        {-# INLINE vkDriverUUIDArrayByteOffset #-}
        vkDriverUUIDArrayByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID}

        {-# INLINE readVkDriverUUIDArray #-}
        readVkDriverUUIDArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID})

        {-# INLINE writeVkDriverUUIDArray #-}
        writeVkDriverUUIDArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, driverUUID})

instance {-# OVERLAPPING #-}
         HasField "driverUUID" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "driverUUID" VkPhysicalDeviceIDPropertiesKHR = Word8
        type FieldOptional "driverUUID" VkPhysicalDeviceIDPropertiesKHR =
             'False -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "driverUUID" idx VkPhysicalDeviceIDPropertiesKHR) =>
         CanReadFieldArray "driverUUID" idx VkPhysicalDeviceIDPropertiesKHR
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 0 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 1 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 2 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "driverUUID" 3 VkPhysicalDeviceIDPropertiesKHR
                       #-}
        type FieldArrayLength "driverUUID" VkPhysicalDeviceIDPropertiesKHR
             = VK_UUID_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkDriverUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkDriverUUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkDeviceLUIDArray VkPhysicalDeviceIDPropertiesKHR where
        type VkDeviceLUIDArrayMType VkPhysicalDeviceIDPropertiesKHR = Word8

        {-# NOINLINE vkDeviceLUIDArray #-}
        vkDeviceLUIDArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word8) +
                    #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID}))

        {-# INLINE vkDeviceLUIDArrayByteOffset #-}
        vkDeviceLUIDArrayByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID}

        {-# INLINE readVkDeviceLUIDArray #-}
        readVkDeviceLUIDArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID})

        {-# INLINE writeVkDeviceLUIDArray #-}
        writeVkDeviceLUIDArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word8) +
                 #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUID})

instance {-# OVERLAPPING #-}
         HasField "deviceLUID" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "deviceLUID" VkPhysicalDeviceIDPropertiesKHR = Word8
        type FieldOptional "deviceLUID" VkPhysicalDeviceIDPropertiesKHR =
             'False -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "deviceLUID" idx VkPhysicalDeviceIDPropertiesKHR) =>
         CanReadFieldArray "deviceLUID" idx VkPhysicalDeviceIDPropertiesKHR
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 0 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 1 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 2 VkPhysicalDeviceIDPropertiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "deviceLUID" 3 VkPhysicalDeviceIDPropertiesKHR
                       #-}
        type FieldArrayLength "deviceLUID" VkPhysicalDeviceIDPropertiesKHR
             = VK_LUID_SIZE_KHR

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkDeviceLUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkDeviceLUIDArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkDeviceNodeMask VkPhysicalDeviceIDPropertiesKHR where
        type VkDeviceNodeMaskMType VkPhysicalDeviceIDPropertiesKHR = Word32

        {-# NOINLINE vkDeviceNodeMask #-}
        vkDeviceNodeMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask})

        {-# INLINE vkDeviceNodeMaskByteOffset #-}
        vkDeviceNodeMaskByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask}

        {-# INLINE readVkDeviceNodeMask #-}
        readVkDeviceNodeMask p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask}

        {-# INLINE writeVkDeviceNodeMask #-}
        writeVkDeviceNodeMask p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceNodeMask}

instance {-# OVERLAPPING #-}
         HasField "deviceNodeMask" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "deviceNodeMask" VkPhysicalDeviceIDPropertiesKHR =
             Word32
        type FieldOptional "deviceNodeMask" VkPhysicalDeviceIDPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "deviceNodeMask"
           VkPhysicalDeviceIDPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkDeviceNodeMask

        {-# INLINE readField #-}
        readField = readVkDeviceNodeMask

instance {-# OVERLAPPING #-}
         HasVkDeviceLUIDValid VkPhysicalDeviceIDPropertiesKHR where
        type VkDeviceLUIDValidMType VkPhysicalDeviceIDPropertiesKHR =
             VkBool32

        {-# NOINLINE vkDeviceLUIDValid #-}
        vkDeviceLUIDValid x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid})

        {-# INLINE vkDeviceLUIDValidByteOffset #-}
        vkDeviceLUIDValidByteOffset ~_
          = #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid}

        {-# INLINE readVkDeviceLUIDValid #-}
        readVkDeviceLUIDValid p
          = peekByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid}

        {-# INLINE writeVkDeviceLUIDValid #-}
        writeVkDeviceLUIDValid p
          = pokeByteOff p #{offset VkPhysicalDeviceIDPropertiesKHR, deviceLUIDValid}

instance {-# OVERLAPPING #-}
         HasField "deviceLUIDValid" VkPhysicalDeviceIDPropertiesKHR where
        type FieldType "deviceLUIDValid" VkPhysicalDeviceIDPropertiesKHR =
             VkBool32
        type FieldOptional "deviceLUIDValid"
               VkPhysicalDeviceIDPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "deviceLUIDValid"
           VkPhysicalDeviceIDPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkDeviceLUIDValid

        {-# INLINE readField #-}
        readField = readVkDeviceLUIDValid

instance Show VkPhysicalDeviceIDPropertiesKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceIDPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceUUIDArray = [" .
                            showsPrec d (map (vkDeviceUUIDArray x) [1 .. VK_UUID_SIZE]) .
                              showChar ']' .
                                showString ", " .
                                  showString "vkDriverUUIDArray = [" .
                                    showsPrec d (map (vkDriverUUIDArray x) [1 .. VK_UUID_SIZE]) .
                                      showChar ']' .
                                        showString ", " .
                                          showString "vkDeviceLUIDArray = [" .
                                            showsPrec d
                                              (map (vkDeviceLUIDArray x) [1 .. VK_LUID_SIZE_KHR])
                                              .
                                              showChar ']' .
                                                showString ", " .
                                                  showString "vkDeviceNodeMask = " .
                                                    showsPrec d (vkDeviceNodeMask x) .
                                                      showString ", " .
                                                        showString "vkDeviceLUIDValid = " .
                                                          showsPrec d (vkDeviceLUIDValid x) .
                                                            showChar '}'

-- | > void vkGetPhysicalDeviceExternalBufferPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalBufferInfoKHR* pExternalBufferInfo
--   >     , VkExternalBufferPropertiesKHR* pExternalBufferProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceExternalBufferPropertiesKHR.html vkGetPhysicalDeviceExternalBufferPropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalBufferPropertiesKHR"
               vkGetPhysicalDeviceExternalBufferPropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalBufferInfoKHR -- ^ pExternalBufferInfo
                                                           ->
                   Ptr VkExternalBufferPropertiesKHR -- ^ pExternalBufferProperties
                                                     -> IO ()

pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME ::
        CString

pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME

_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME #-}
_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory_capabilities\NUL"##

is_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME ::
                                                      CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
           #-}
is_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME =
     "VK_KHR_external_memory_capabilities"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR
        = VkStructureType 1000071000

pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR =
        VkStructureType 1000071001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR
        = VkStructureType 1000071002

pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR =
        VkStructureType 1000071003

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR =
        VkStructureType 1000071004
