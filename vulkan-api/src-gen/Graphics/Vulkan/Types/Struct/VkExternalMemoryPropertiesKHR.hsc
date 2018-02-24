#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalMemoryPropertiesKHR
       (VkExternalMemoryPropertiesKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagsKHR    (VkExternalMemoryFeatureFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagsKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryPropertiesKHR {
--   >     VkExternalMemoryFeatureFlagsKHR  externalMemoryFeatures;
--   >     VkExternalMemoryHandleTypeFlagsKHR exportFromImportedHandleTypes;
--   >     VkExternalMemoryHandleTypeFlagsKHR compatibleHandleTypes;
--   > } VkExternalMemoryPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalMemoryPropertiesKHR.html VkExternalMemoryPropertiesKHR registry at www.khronos.org>
data VkExternalMemoryPropertiesKHR = VkExternalMemoryPropertiesKHR## Addr##
                                                                    ByteArray##

instance Eq VkExternalMemoryPropertiesKHR where
        (VkExternalMemoryPropertiesKHR## a _) ==
          x@(VkExternalMemoryPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryPropertiesKHR where
        (VkExternalMemoryPropertiesKHR## a _) `compare`
          x@(VkExternalMemoryPropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryPropertiesKHR where
        sizeOf ~_ = #{size VkExternalMemoryPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryPropertiesKHR where
        unsafeAddr (VkExternalMemoryPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryPropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryPropertiesKHR where
        type StructFields VkExternalMemoryPropertiesKHR =
             '["externalMemoryFeatures", "exportFromImportedHandleTypes", -- ' closing tick for hsc2hs
               "compatibleHandleTypes"]
        type CUnionType VkExternalMemoryPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryPropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryPropertiesKHR = '[] -- ' closing tick for hsc2hs

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
        type FieldOffset "externalMemoryFeatures"
               VkExternalMemoryPropertiesKHR
             =
             #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures}
        type FieldIsArray "externalMemoryFeatures"
               VkExternalMemoryPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures}

instance CanReadField "externalMemoryFeatures"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExternalMemoryFeatures

        {-# INLINE readField #-}
        readField = readVkExternalMemoryFeatures

instance CanWriteField "externalMemoryFeatures"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkExternalMemoryFeatures

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
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalMemoryPropertiesKHR
             =
             #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes}
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalMemoryPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes}

instance CanReadField "exportFromImportedHandleTypes"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExportFromImportedHandleTypes

        {-# INLINE readField #-}
        readField = readVkExportFromImportedHandleTypes

instance CanWriteField "exportFromImportedHandleTypes"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkExportFromImportedHandleTypes

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
        type FieldOffset "compatibleHandleTypes"
               VkExternalMemoryPropertiesKHR
             =
             #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes}
        type FieldIsArray "compatibleHandleTypes"
               VkExternalMemoryPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes}

instance CanReadField "compatibleHandleTypes"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCompatibleHandleTypes

        {-# INLINE readField #-}
        readField = readVkCompatibleHandleTypes

instance CanWriteField "compatibleHandleTypes"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkCompatibleHandleTypes

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
