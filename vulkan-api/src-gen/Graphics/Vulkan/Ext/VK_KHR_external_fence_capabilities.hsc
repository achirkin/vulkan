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
        pattern VK_LUID_SIZE_KHR, VK_LUID_SIZE_KHR)
       where
import           Foreign.C.String                                        (CString)
import           Foreign.Storable                                        (Storable (..))
import           GHC.Prim
import           GHC.Ptr                                                 (Ptr (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Ext.VK_KHR_external_memory_capabilities (VkPhysicalDeviceIDPropertiesKHR (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalFenceInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalFenceHandleTypeFlagBitsKHR handleType;
--   > } VkPhysicalDeviceExternalFenceInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceExternalFenceInfoKHR.html VkPhysicalDeviceExternalFenceInfoKHR registry at www.khronos.org>
data VkPhysicalDeviceExternalFenceInfoKHR = VkPhysicalDeviceExternalFenceInfoKHR## Addr##
                                                                                  ByteArray##

instance Eq VkPhysicalDeviceExternalFenceInfoKHR where
        (VkPhysicalDeviceExternalFenceInfoKHR## a _) ==
          x@(VkPhysicalDeviceExternalFenceInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalFenceInfoKHR where
        (VkPhysicalDeviceExternalFenceInfoKHR## a _) `compare`
          x@(VkPhysicalDeviceExternalFenceInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalFenceInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalFenceInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalFenceInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceExternalFenceInfoKHR
         where
        unsafeAddr (VkPhysicalDeviceExternalFenceInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalFenceInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalFenceInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalFenceInfoKHR where
        type StructFields VkPhysicalDeviceExternalFenceInfoKHR =
             '["sType", "pNext", "handleType"] -- ' closing tick for hsc2hs

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
         HasField "sType" VkPhysicalDeviceExternalFenceInfoKHR where
        type FieldType "sType" VkPhysicalDeviceExternalFenceInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalFenceInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalFenceInfoKHR =
             #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType}

instance CanReadField "sType" VkPhysicalDeviceExternalFenceInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPhysicalDeviceExternalFenceInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkPhysicalDeviceExternalFenceInfoKHR where
        type FieldType "pNext" VkPhysicalDeviceExternalFenceInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalFenceInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalFenceInfoKHR =
             #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext}

instance CanReadField "pNext" VkPhysicalDeviceExternalFenceInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPhysicalDeviceExternalFenceInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalFenceInfoKHR where
        type FieldType "handleType" VkPhysicalDeviceExternalFenceInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalFenceInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkPhysicalDeviceExternalFenceInfoKHR
             =
             #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType}

instance CanReadField "handleType"
           VkPhysicalDeviceExternalFenceInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType"
           VkPhysicalDeviceExternalFenceInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

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

-- | > typedef struct VkExternalFencePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalFenceHandleTypeFlagsKHR exportFromImportedHandleTypes;
--   >     VkExternalFenceHandleTypeFlagsKHR compatibleHandleTypes;
--   >     VkExternalFenceFeatureFlagsKHR externalFenceFeatures;
--   > } VkExternalFencePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalFencePropertiesKHR.html VkExternalFencePropertiesKHR registry at www.khronos.org>
data VkExternalFencePropertiesKHR = VkExternalFencePropertiesKHR## Addr##
                                                                  ByteArray##

instance Eq VkExternalFencePropertiesKHR where
        (VkExternalFencePropertiesKHR## a _) ==
          x@(VkExternalFencePropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalFencePropertiesKHR where
        (VkExternalFencePropertiesKHR## a _) `compare`
          x@(VkExternalFencePropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalFencePropertiesKHR where
        sizeOf ~_ = #{size VkExternalFencePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalFencePropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalFencePropertiesKHR where
        unsafeAddr (VkExternalFencePropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalFencePropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalFencePropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalFencePropertiesKHR where
        type StructFields VkExternalFencePropertiesKHR =
             '["sType", "pNext", "exportFromImportedHandleTypes", -- ' closing tick for hsc2hs
               "compatibleHandleTypes", "externalFenceFeatures"]

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
         HasField "sType" VkExternalFencePropertiesKHR where
        type FieldType "sType" VkExternalFencePropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalFencePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalFencePropertiesKHR =
             #{offset VkExternalFencePropertiesKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFencePropertiesKHR, sType}

instance CanReadField "sType" VkExternalFencePropertiesKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

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
         HasField "pNext" VkExternalFencePropertiesKHR where
        type FieldType "pNext" VkExternalFencePropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkExternalFencePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalFencePropertiesKHR =
             #{offset VkExternalFencePropertiesKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFencePropertiesKHR, pNext}

instance CanReadField "pNext" VkExternalFencePropertiesKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

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
         HasField "exportFromImportedHandleTypes"
           VkExternalFencePropertiesKHR
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalFencePropertiesKHR
             = VkExternalFenceHandleTypeFlagsKHR
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalFencePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalFencePropertiesKHR
             =
             #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes}

instance CanReadField "exportFromImportedHandleTypes"
           VkExternalFencePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExportFromImportedHandleTypes

        {-# INLINE readField #-}
        readField = readVkExportFromImportedHandleTypes

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
         HasField "compatibleHandleTypes" VkExternalFencePropertiesKHR where
        type FieldType "compatibleHandleTypes" VkExternalFencePropertiesKHR
             = VkExternalFenceHandleTypeFlagsKHR
        type FieldOptional "compatibleHandleTypes"
               VkExternalFencePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compatibleHandleTypes"
               VkExternalFencePropertiesKHR
             =
             #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes}

instance CanReadField "compatibleHandleTypes"
           VkExternalFencePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCompatibleHandleTypes

        {-# INLINE readField #-}
        readField = readVkCompatibleHandleTypes

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

instance {-# OVERLAPPING #-}
         HasField "externalFenceFeatures" VkExternalFencePropertiesKHR where
        type FieldType "externalFenceFeatures" VkExternalFencePropertiesKHR
             = VkExternalFenceFeatureFlagsKHR
        type FieldOptional "externalFenceFeatures"
               VkExternalFencePropertiesKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "externalFenceFeatures"
               VkExternalFencePropertiesKHR
             =
             #{offset VkExternalFencePropertiesKHR, externalFenceFeatures}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFencePropertiesKHR, externalFenceFeatures}

instance CanReadField "externalFenceFeatures"
           VkExternalFencePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExternalFenceFeatures

        {-# INLINE readField #-}
        readField = readVkExternalFenceFeatures

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

{-# INLINE _VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_KHR_external_fence_capabilities\NUL"##

{-# INLINE is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME ::
                                                     CString -> Bool
is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME

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
