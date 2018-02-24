#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalFencePropertiesKHR
       (VkExternalFencePropertiesKHR(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlagsKHR    (VkExternalFenceFeatureFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR (VkExternalFenceHandleTypeFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkExternalFencePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalFenceHandleTypeFlagsKHR exportFromImportedHandleTypes;
--   >     VkExternalFenceHandleTypeFlagsKHR compatibleHandleTypes;
--   >     VkExternalFenceFeatureFlagsKHR externalFenceFeatures;
--   > } VkExternalFencePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalFencePropertiesKHR.html VkExternalFencePropertiesKHR registry at www.khronos.org>
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
        type CUnionType VkExternalFencePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalFencePropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalFencePropertiesKHR = '[] -- ' closing tick for hsc2hs

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
        type FieldIsArray "sType" VkExternalFencePropertiesKHR = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "sType" VkExternalFencePropertiesKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
        type FieldIsArray "pNext" VkExternalFencePropertiesKHR = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "pNext" VkExternalFencePropertiesKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalFencePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "exportFromImportedHandleTypes"
           VkExternalFencePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkExportFromImportedHandleTypes

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
        type FieldIsArray "compatibleHandleTypes"
               VkExternalFencePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "compatibleHandleTypes"
           VkExternalFencePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkCompatibleHandleTypes

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
        type FieldIsArray "externalFenceFeatures"
               VkExternalFencePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "externalFenceFeatures"
           VkExternalFencePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkExternalFenceFeatures

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
