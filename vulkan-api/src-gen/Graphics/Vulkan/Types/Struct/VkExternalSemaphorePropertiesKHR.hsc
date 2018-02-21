#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalSemaphorePropertiesKHR
       (VkExternalSemaphorePropertiesKHR(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreFeatureFlagsKHR
                                                                                   (VkExternalSemaphoreFeatureFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlagsKHR
                                                                                   (VkExternalSemaphoreHandleTypeFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                   (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkExternalSemaphorePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalSemaphoreHandleTypeFlagsKHR exportFromImportedHandleTypes;
--   >     VkExternalSemaphoreHandleTypeFlagsKHR compatibleHandleTypes;
--   >     VkExternalSemaphoreFeatureFlagsKHR externalSemaphoreFeatures;
--   > } VkExternalSemaphorePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalSemaphorePropertiesKHR.html VkExternalSemaphorePropertiesKHR registry at www.khronos.org>
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
        type FieldIsArray "sType" VkExternalSemaphorePropertiesKHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pNext" VkExternalSemaphorePropertiesKHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "compatibleHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "externalSemaphoreFeatures"
               VkExternalSemaphorePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

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
