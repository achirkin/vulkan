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

-- | > typedef struct VkPhysicalDeviceExternalSemaphoreInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalSemaphoreHandleTypeFlagBitsKHR handleType;
--   > } VkPhysicalDeviceExternalSemaphoreInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceExternalSemaphoreInfoKHR.html VkPhysicalDeviceExternalSemaphoreInfoKHR registry at www.khronos.org>
data VkPhysicalDeviceExternalSemaphoreInfoKHR = VkPhysicalDeviceExternalSemaphoreInfoKHR## Addr##
                                                                                          ByteArray##

instance Eq VkPhysicalDeviceExternalSemaphoreInfoKHR where
        (VkPhysicalDeviceExternalSemaphoreInfoKHR## a _) ==
          x@(VkPhysicalDeviceExternalSemaphoreInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalSemaphoreInfoKHR where
        (VkPhysicalDeviceExternalSemaphoreInfoKHR## a _) `compare`
          x@(VkPhysicalDeviceExternalSemaphoreInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalSemaphoreInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalSemaphoreInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalSemaphoreInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        unsafeAddr (VkPhysicalDeviceExternalSemaphoreInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalSemaphoreInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalSemaphoreInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        type StructFields VkPhysicalDeviceExternalSemaphoreInfoKHR =
             '["sType", "pNext", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalSemaphoreInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalSemaphoreInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalSemaphoreInfoKHR = '[] -- ' closing tick for hsc2hs

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
         HasField "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR where
        type FieldType "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR =
             #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType}

instance CanReadField "sType"
           VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR where
        type FieldType "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR =
             #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        type FieldType "handleType"
               VkPhysicalDeviceExternalSemaphoreInfoKHR
             = VkExternalSemaphoreHandleTypeFlagBitsKHR
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalSemaphoreInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType"
               VkPhysicalDeviceExternalSemaphoreInfoKHR
             =
             #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType}

instance CanReadField "handleType"
           VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType"
           VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

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

-- | > typedef struct VkExternalSemaphorePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalSemaphoreHandleTypeFlagsKHR exportFromImportedHandleTypes;
--   >     VkExternalSemaphoreHandleTypeFlagsKHR compatibleHandleTypes;
--   >     VkExternalSemaphoreFeatureFlagsKHR externalSemaphoreFeatures;
--   > } VkExternalSemaphorePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalSemaphorePropertiesKHR.html VkExternalSemaphorePropertiesKHR registry at www.khronos.org>
data VkExternalSemaphorePropertiesKHR = VkExternalSemaphorePropertiesKHR## Addr##
                                                                          ByteArray##

instance Eq VkExternalSemaphorePropertiesKHR where
        (VkExternalSemaphorePropertiesKHR## a _) ==
          x@(VkExternalSemaphorePropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalSemaphorePropertiesKHR where
        (VkExternalSemaphorePropertiesKHR## a _) `compare`
          x@(VkExternalSemaphorePropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalSemaphorePropertiesKHR where
        sizeOf ~_ = #{size VkExternalSemaphorePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalSemaphorePropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalSemaphorePropertiesKHR where
        unsafeAddr (VkExternalSemaphorePropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalSemaphorePropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalSemaphorePropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalSemaphorePropertiesKHR where
        type StructFields VkExternalSemaphorePropertiesKHR =
             '["sType", "pNext", "exportFromImportedHandleTypes", -- ' closing tick for hsc2hs
               "compatibleHandleTypes", "externalSemaphoreFeatures"]
        type CUnionType VkExternalSemaphorePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalSemaphorePropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalSemaphorePropertiesKHR = '[] -- ' closing tick for hsc2hs

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
         HasField "sType" VkExternalSemaphorePropertiesKHR where
        type FieldType "sType" VkExternalSemaphorePropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalSemaphorePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalSemaphorePropertiesKHR =
             #{offset VkExternalSemaphorePropertiesKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphorePropertiesKHR, sType}

instance CanReadField "sType" VkExternalSemaphorePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

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
         HasField "pNext" VkExternalSemaphorePropertiesKHR where
        type FieldType "pNext" VkExternalSemaphorePropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkExternalSemaphorePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalSemaphorePropertiesKHR =
             #{offset VkExternalSemaphorePropertiesKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphorePropertiesKHR, pNext}

instance CanReadField "pNext" VkExternalSemaphorePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

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
         HasField "exportFromImportedHandleTypes"
           VkExternalSemaphorePropertiesKHR
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = VkExternalSemaphoreHandleTypeFlagsKHR
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalSemaphorePropertiesKHR
             =
             #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes}

instance CanReadField "exportFromImportedHandleTypes"
           VkExternalSemaphorePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExportFromImportedHandleTypes

        {-# INLINE readField #-}
        readField = readVkExportFromImportedHandleTypes

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
         HasField "compatibleHandleTypes" VkExternalSemaphorePropertiesKHR
         where
        type FieldType "compatibleHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = VkExternalSemaphoreHandleTypeFlagsKHR
        type FieldOptional "compatibleHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compatibleHandleTypes"
               VkExternalSemaphorePropertiesKHR
             =
             #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes}

instance CanReadField "compatibleHandleTypes"
           VkExternalSemaphorePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCompatibleHandleTypes

        {-# INLINE readField #-}
        readField = readVkCompatibleHandleTypes

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

instance {-# OVERLAPPING #-}
         HasField "externalSemaphoreFeatures"
           VkExternalSemaphorePropertiesKHR
         where
        type FieldType "externalSemaphoreFeatures"
               VkExternalSemaphorePropertiesKHR
             = VkExternalSemaphoreFeatureFlagsKHR
        type FieldOptional "externalSemaphoreFeatures"
               VkExternalSemaphorePropertiesKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "externalSemaphoreFeatures"
               VkExternalSemaphorePropertiesKHR
             =
             #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures}

instance CanReadField "externalSemaphoreFeatures"
           VkExternalSemaphorePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExternalSemaphoreFeatures

        {-# INLINE readField #-}
        readField = readVkExternalSemaphoreFeatures

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

{-# INLINE _VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
           #-}

_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_KHR_external_semaphore_capabilities\NUL"##

{-# INLINE is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
           #-}

is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME ::
                                                         CString -> Bool
is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME

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
