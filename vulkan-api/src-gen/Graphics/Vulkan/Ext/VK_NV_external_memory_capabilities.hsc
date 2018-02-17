#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
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
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
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
data VkExternalImageFormatPropertiesNV = VkExternalImageFormatPropertiesNV## Addr##
                                                                            ByteArray##

instance Eq VkExternalImageFormatPropertiesNV where
        (VkExternalImageFormatPropertiesNV## a _) ==
          x@(VkExternalImageFormatPropertiesNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalImageFormatPropertiesNV where
        (VkExternalImageFormatPropertiesNV## a _) `compare`
          x@(VkExternalImageFormatPropertiesNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalImageFormatPropertiesNV where
        sizeOf ~_ = #{size VkExternalImageFormatPropertiesNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalImageFormatPropertiesNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalImageFormatPropertiesNV where
        unsafeAddr (VkExternalImageFormatPropertiesNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalImageFormatPropertiesNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalImageFormatPropertiesNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalImageFormatPropertiesNV where
        type StructFields VkExternalImageFormatPropertiesNV =
             '["imageFormatProperties", "externalMemoryFeatures", -- ' closing tick for hsc2hs
               "exportFromImportedHandleTypes", "compatibleHandleTypes"]
        type CUnionType VkExternalImageFormatPropertiesNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalImageFormatPropertiesNV = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalImageFormatPropertiesNV = '[] -- ' closing tick for hsc2hs

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
        type FieldIsArray "imageFormatProperties"
               VkExternalImageFormatPropertiesNV
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "externalMemoryFeatures"
               VkExternalImageFormatPropertiesNV
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalImageFormatPropertiesNV
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "compatibleHandleTypes"
               VkExternalImageFormatPropertiesNV
             = 'False -- ' closing tick for hsc2hs

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
