#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalFenceProperties
       (VkExternalFenceProperties(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Base                                                  (Addr##,
                                                                            ByteArray##,
                                                                            byteArrayContents##,
                                                                            plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlags    (VkExternalFenceFeatureFlags)
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags (VkExternalFenceHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkExternalFenceProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalFenceHandleTypeFlags exportFromImportedHandleTypes;
--   >     VkExternalFenceHandleTypeFlags compatibleHandleTypes;
--   >     VkExternalFenceFeatureFlags externalFenceFeatures;
--   > } VkExternalFenceProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkExternalFencePropertiesVkExternalFenceProperties registry at www.khronos.org>
data VkExternalFenceProperties = VkExternalFenceProperties## Addr##
                                                            ByteArray##

instance Eq VkExternalFenceProperties where
        (VkExternalFenceProperties## a _) ==
          x@(VkExternalFenceProperties## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalFenceProperties where
        (VkExternalFenceProperties## a _) `compare`
          x@(VkExternalFenceProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalFenceProperties where
        sizeOf ~_ = #{size VkExternalFenceProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExternalFenceProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalFenceProperties where
        unsafeAddr (VkExternalFenceProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalFenceProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalFenceProperties## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalFenceProperties where
        type StructFields VkExternalFenceProperties =
             '["sType", "pNext", "exportFromImportedHandleTypes", -- ' closing tick for hsc2hs
               "compatibleHandleTypes", "externalFenceFeatures"]
        type CUnionType VkExternalFenceProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalFenceProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalFenceProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalFenceProperties where
        type FieldType "sType" VkExternalFenceProperties = VkStructureType
        type FieldOptional "sType" VkExternalFenceProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalFenceProperties =
             #{offset VkExternalFenceProperties, sType}
        type FieldIsArray "sType" VkExternalFenceProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFenceProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalFenceProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFenceProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFenceProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalFenceProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFenceProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalFenceProperties where
        type FieldType "pNext" VkExternalFenceProperties = Ptr Void
        type FieldOptional "pNext" VkExternalFenceProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalFenceProperties =
             #{offset VkExternalFenceProperties, pNext}
        type FieldIsArray "pNext" VkExternalFenceProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFenceProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalFenceProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFenceProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFenceProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalFenceProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFenceProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "exportFromImportedHandleTypes" VkExternalFenceProperties
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalFenceProperties
             = VkExternalFenceHandleTypeFlags
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalFenceProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalFenceProperties
             =
             #{offset VkExternalFenceProperties, exportFromImportedHandleTypes}
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalFenceProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFenceProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "exportFromImportedHandleTypes"
           VkExternalFenceProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFenceProperties, exportFromImportedHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFenceProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "exportFromImportedHandleTypes"
           VkExternalFenceProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFenceProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "compatibleHandleTypes" VkExternalFenceProperties where
        type FieldType "compatibleHandleTypes" VkExternalFenceProperties =
             VkExternalFenceHandleTypeFlags
        type FieldOptional "compatibleHandleTypes"
               VkExternalFenceProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compatibleHandleTypes" VkExternalFenceProperties
             =
             #{offset VkExternalFenceProperties, compatibleHandleTypes}
        type FieldIsArray "compatibleHandleTypes" VkExternalFenceProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFenceProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "compatibleHandleTypes" VkExternalFenceProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFenceProperties, compatibleHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFenceProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "compatibleHandleTypes" VkExternalFenceProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFenceProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "externalFenceFeatures" VkExternalFenceProperties where
        type FieldType "externalFenceFeatures" VkExternalFenceProperties =
             VkExternalFenceFeatureFlags
        type FieldOptional "externalFenceFeatures"
               VkExternalFenceProperties
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "externalFenceFeatures" VkExternalFenceProperties
             =
             #{offset VkExternalFenceProperties, externalFenceFeatures}
        type FieldIsArray "externalFenceFeatures" VkExternalFenceProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFenceProperties, externalFenceFeatures}

instance {-# OVERLAPPING #-}
         CanReadField "externalFenceFeatures" VkExternalFenceProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFenceProperties, externalFenceFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFenceProperties, externalFenceFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "externalFenceFeatures" VkExternalFenceProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFenceProperties, externalFenceFeatures}

instance Show VkExternalFenceProperties where
        showsPrec d x
          = showString "VkExternalFenceProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "exportFromImportedHandleTypes = " .
                            showsPrec d (getField @"exportFromImportedHandleTypes" x) .
                              showString ", " .
                                showString "compatibleHandleTypes = " .
                                  showsPrec d (getField @"compatibleHandleTypes" x) .
                                    showString ", " .
                                      showString "externalFenceFeatures = " .
                                        showsPrec d (getField @"externalFenceFeatures" x) .
                                          showChar '}'
