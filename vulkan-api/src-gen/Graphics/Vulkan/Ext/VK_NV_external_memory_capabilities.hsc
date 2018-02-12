#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NV_external_memory_capabilities
       (-- * Vulkan extension: @VK_NV_external_memory_capabilities@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @instance@
        --
        -- Extension number: @56@
        VkExternalImageFormatPropertiesNV(..),
        vkGetPhysicalDeviceExternalImageFormatPropertiesNV,
        VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME,
        pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkImageFormatProperties)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExternalImageFormatPropertiesNV {
--   >     VkImageFormatProperties          imageFormatProperties;
--   >     VkExternalMemoryFeatureFlagsNV   externalMemoryFeatures;
--   >     VkExternalMemoryHandleTypeFlagsNV exportFromImportedHandleTypes;
--   >     VkExternalMemoryHandleTypeFlagsNV compatibleHandleTypes;
--   > } VkExternalImageFormatPropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalImageFormatPropertiesNV.html VkExternalImageFormatPropertiesNV registry at www.khronos.org>
data VkExternalImageFormatPropertiesNV = VkExternalImageFormatPropertiesNV## ByteArray##

instance Eq VkExternalImageFormatPropertiesNV where
        (VkExternalImageFormatPropertiesNV## a) ==
          (VkExternalImageFormatPropertiesNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExternalImageFormatPropertiesNV where
        (VkExternalImageFormatPropertiesNV## a) `compare`
          (VkExternalImageFormatPropertiesNV## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExternalImageFormatPropertiesNV where
        sizeOf ~_ = #{size VkExternalImageFormatPropertiesNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalImageFormatPropertiesNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExternalImageFormatPropertiesNV),
            I## a <- alignment (undefined :: VkExternalImageFormatPropertiesNV)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExternalImageFormatPropertiesNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExternalImageFormatPropertiesNV## ba)
          | I## n <- sizeOf (undefined :: VkExternalImageFormatPropertiesNV) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExternalImageFormatPropertiesNV where
        type StructFields VkExternalImageFormatPropertiesNV =
             '["imageFormatProperties", "externalMemoryFeatures", -- ' closing tick for hsc2hs
               "exportFromImportedHandleTypes", "compatibleHandleTypes"]

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExternalImageFormatPropertiesNV),
            I## a <- alignment (undefined :: VkExternalImageFormatPropertiesNV)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExternalImageFormatPropertiesNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExternalImageFormatPropertiesNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExternalImageFormatPropertiesNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExternalImageFormatPropertiesNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExternalImageFormatPropertiesNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExternalImageFormatPropertiesNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkImageFormatProperties VkExternalImageFormatPropertiesNV where
        type VkImageFormatPropertiesMType VkExternalImageFormatPropertiesNV
             = VkImageFormatProperties

        {-# NOINLINE vkImageFormatProperties #-}
        vkImageFormatProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesNV, imageFormatProperties})

        {-# INLINE vkImageFormatPropertiesByteOffset #-}
        vkImageFormatPropertiesByteOffset ~_
          = #{offset VkExternalImageFormatPropertiesNV, imageFormatProperties}

        {-# INLINE readVkImageFormatProperties #-}
        readVkImageFormatProperties p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesNV, imageFormatProperties}

        {-# INLINE writeVkImageFormatProperties #-}
        writeVkImageFormatProperties p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesNV, imageFormatProperties}

instance {-# OVERLAPPING #-}
         HasField "imageFormatProperties" VkExternalImageFormatPropertiesNV
         where
        type FieldType "imageFormatProperties"
               VkExternalImageFormatPropertiesNV
             = VkImageFormatProperties
        type FieldOptional "imageFormatProperties"
               VkExternalImageFormatPropertiesNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageFormatProperties"
               VkExternalImageFormatPropertiesNV
             =
             #{offset VkExternalImageFormatPropertiesNV, imageFormatProperties}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatPropertiesNV, imageFormatProperties}

instance CanReadField "imageFormatProperties"
           VkExternalImageFormatPropertiesNV
         where
        {-# INLINE getField #-}
        getField = vkImageFormatProperties

        {-# INLINE readField #-}
        readField = readVkImageFormatProperties

instance {-# OVERLAPPING #-}
         HasVkExternalMemoryFeatures VkExternalImageFormatPropertiesNV where
        type VkExternalMemoryFeaturesMType
               VkExternalImageFormatPropertiesNV
             = VkExternalMemoryFeatureFlagsNV

        {-# NOINLINE vkExternalMemoryFeatures #-}
        vkExternalMemoryFeatures x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesNV, externalMemoryFeatures})

        {-# INLINE vkExternalMemoryFeaturesByteOffset #-}
        vkExternalMemoryFeaturesByteOffset ~_
          = #{offset VkExternalImageFormatPropertiesNV, externalMemoryFeatures}

        {-# INLINE readVkExternalMemoryFeatures #-}
        readVkExternalMemoryFeatures p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesNV, externalMemoryFeatures}

        {-# INLINE writeVkExternalMemoryFeatures #-}
        writeVkExternalMemoryFeatures p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesNV, externalMemoryFeatures}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryFeatures" VkExternalImageFormatPropertiesNV
         where
        type FieldType "externalMemoryFeatures"
               VkExternalImageFormatPropertiesNV
             = VkExternalMemoryFeatureFlagsNV
        type FieldOptional "externalMemoryFeatures"
               VkExternalImageFormatPropertiesNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "externalMemoryFeatures"
               VkExternalImageFormatPropertiesNV
             =
             #{offset VkExternalImageFormatPropertiesNV, externalMemoryFeatures}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatPropertiesNV, externalMemoryFeatures}

instance CanReadField "externalMemoryFeatures"
           VkExternalImageFormatPropertiesNV
         where
        {-# INLINE getField #-}
        getField = vkExternalMemoryFeatures

        {-# INLINE readField #-}
        readField = readVkExternalMemoryFeatures

instance {-# OVERLAPPING #-}
         HasVkExportFromImportedHandleTypes
           VkExternalImageFormatPropertiesNV
         where
        type VkExportFromImportedHandleTypesMType
               VkExternalImageFormatPropertiesNV
             = VkExternalMemoryHandleTypeFlagsNV

        {-# NOINLINE vkExportFromImportedHandleTypes #-}
        vkExportFromImportedHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesNV, exportFromImportedHandleTypes})

        {-# INLINE vkExportFromImportedHandleTypesByteOffset #-}
        vkExportFromImportedHandleTypesByteOffset ~_
          = #{offset VkExternalImageFormatPropertiesNV, exportFromImportedHandleTypes}

        {-# INLINE readVkExportFromImportedHandleTypes #-}
        readVkExportFromImportedHandleTypes p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesNV, exportFromImportedHandleTypes}

        {-# INLINE writeVkExportFromImportedHandleTypes #-}
        writeVkExportFromImportedHandleTypes p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesNV, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "exportFromImportedHandleTypes"
           VkExternalImageFormatPropertiesNV
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalImageFormatPropertiesNV
             = VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalImageFormatPropertiesNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalImageFormatPropertiesNV
             =
             #{offset VkExternalImageFormatPropertiesNV, exportFromImportedHandleTypes}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatPropertiesNV, exportFromImportedHandleTypes}

instance CanReadField "exportFromImportedHandleTypes"
           VkExternalImageFormatPropertiesNV
         where
        {-# INLINE getField #-}
        getField = vkExportFromImportedHandleTypes

        {-# INLINE readField #-}
        readField = readVkExportFromImportedHandleTypes

instance {-# OVERLAPPING #-}
         HasVkCompatibleHandleTypes VkExternalImageFormatPropertiesNV where
        type VkCompatibleHandleTypesMType VkExternalImageFormatPropertiesNV
             = VkExternalMemoryHandleTypeFlagsNV

        {-# NOINLINE vkCompatibleHandleTypes #-}
        vkCompatibleHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesNV, compatibleHandleTypes})

        {-# INLINE vkCompatibleHandleTypesByteOffset #-}
        vkCompatibleHandleTypesByteOffset ~_
          = #{offset VkExternalImageFormatPropertiesNV, compatibleHandleTypes}

        {-# INLINE readVkCompatibleHandleTypes #-}
        readVkCompatibleHandleTypes p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesNV, compatibleHandleTypes}

        {-# INLINE writeVkCompatibleHandleTypes #-}
        writeVkCompatibleHandleTypes p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesNV, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "compatibleHandleTypes" VkExternalImageFormatPropertiesNV
         where
        type FieldType "compatibleHandleTypes"
               VkExternalImageFormatPropertiesNV
             = VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "compatibleHandleTypes"
               VkExternalImageFormatPropertiesNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "compatibleHandleTypes"
               VkExternalImageFormatPropertiesNV
             =
             #{offset VkExternalImageFormatPropertiesNV, compatibleHandleTypes}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatPropertiesNV, compatibleHandleTypes}

instance CanReadField "compatibleHandleTypes"
           VkExternalImageFormatPropertiesNV
         where
        {-# INLINE getField #-}
        getField = vkCompatibleHandleTypes

        {-# INLINE readField #-}
        readField = readVkCompatibleHandleTypes

instance Show VkExternalImageFormatPropertiesNV where
        showsPrec d x
          = showString "VkExternalImageFormatPropertiesNV {" .
              showString "vkImageFormatProperties = " .
                showsPrec d (vkImageFormatProperties x) .
                  showString ", " .
                    showString "vkExternalMemoryFeatures = " .
                      showsPrec d (vkExternalMemoryFeatures x) .
                        showString ", " .
                          showString "vkExportFromImportedHandleTypes = " .
                            showsPrec d (vkExportFromImportedHandleTypes x) .
                              showString ", " .
                                showString "vkCompatibleHandleTypes = " .
                                  showsPrec d (vkCompatibleHandleTypes x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceExternalImageFormatPropertiesNV
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkImageTiling tiling
--   >     , VkImageUsageFlags usage
--   >     , VkImageCreateFlags flags
--   >     , VkExternalMemoryHandleTypeFlagsNV externalHandleType
--   >     , VkExternalImageFormatPropertiesNV* pExternalImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceExternalImageFormatPropertiesNV.html vkGetPhysicalDeviceExternalImageFormatPropertiesNV registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"
               vkGetPhysicalDeviceExternalImageFormatPropertiesNV ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          ->
                   VkImageType -- ^ type
                               ->
                     VkImageTiling -- ^ tiling
                                   ->
                       VkImageUsageFlags -- ^ usage
                                         ->
                         VkImageCreateFlags -- ^ flags
                                            ->
                           VkExternalMemoryHandleTypeFlagsNV -- ^ externalHandleType
                                                             ->
                             Ptr VkExternalImageFormatPropertiesNV -- ^ pExternalImageFormatProperties
                                                                   -> IO VkResult

pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

type VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME ::
        CString

pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME <-
        (is_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME -> True)
  where VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
          = _VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME

{-# INLINE _VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME #-}

_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: CString
_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_NV_external_memory_capabilities\NUL"##

{-# INLINE is_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME #-}

is_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME ::
                                                     CString -> Bool
is_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  = eqCStrings _VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME

type VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME =
     "VK_NV_external_memory_capabilities"
