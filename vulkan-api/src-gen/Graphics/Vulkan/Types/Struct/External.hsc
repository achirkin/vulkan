#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.External
       (VkExternalBufferProperties(..), VkExternalBufferPropertiesKHR,
        VkExternalFenceProperties(..), VkExternalFencePropertiesKHR,
        VkExternalImageFormatProperties(..),
        VkExternalImageFormatPropertiesKHR,
        VkExternalImageFormatPropertiesNV(..),
        VkExternalMemoryBufferCreateInfo(..),
        VkExternalMemoryBufferCreateInfoKHR,
        VkExternalMemoryImageCreateInfo(..),
        VkExternalMemoryImageCreateInfoKHR,
        VkExternalMemoryImageCreateInfoNV(..),
        VkExternalMemoryProperties(..), VkExternalMemoryPropertiesKHR,
        VkExternalSemaphoreProperties(..),
        VkExternalSemaphorePropertiesKHR)
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.External      (VkExternalFenceFeatureFlags,
                                                           VkExternalFenceHandleTypeFlags,
                                                           VkExternalMemoryFeatureFlags,
                                                           VkExternalMemoryFeatureFlagsNV,
                                                           VkExternalMemoryHandleTypeFlags,
                                                           VkExternalMemoryHandleTypeFlagsNV,
                                                           VkExternalSemaphoreFeatureFlags,
                                                           VkExternalSemaphoreHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Buffer      (VkBufferCreateInfo)
import           Graphics.Vulkan.Types.Struct.Image       (VkImageCreateInfo, VkImageFormatProperties,
                                                           VkImageFormatProperties2)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkExternalBufferProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryProperties    externalMemoryProperties;
--   > } VkExternalBufferProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalBufferProperties VkExternalBufferProperties registry at www.khronos.org>
data VkExternalBufferProperties = VkExternalBufferProperties## Addr##
                                                              ByteArray##

instance Eq VkExternalBufferProperties where
        (VkExternalBufferProperties## a _) ==
          x@(VkExternalBufferProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalBufferProperties where
        (VkExternalBufferProperties## a _) `compare`
          x@(VkExternalBufferProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalBufferProperties where
        sizeOf ~_ = #{size VkExternalBufferProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExternalBufferProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalBufferProperties where
        unsafeAddr (VkExternalBufferProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalBufferProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalBufferProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalBufferProperties where
        type StructFields VkExternalBufferProperties =
             '["sType", "pNext", "externalMemoryProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalBufferProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalBufferProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalBufferProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalBufferProperties where
        type FieldType "sType" VkExternalBufferProperties = VkStructureType
        type FieldOptional "sType" VkExternalBufferProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalBufferProperties =
             #{offset VkExternalBufferProperties, sType}
        type FieldIsArray "sType" VkExternalBufferProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalBufferProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalBufferProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalBufferProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalBufferProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalBufferProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalBufferProperties where
        type FieldType "pNext" VkExternalBufferProperties = Ptr Void
        type FieldOptional "pNext" VkExternalBufferProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalBufferProperties =
             #{offset VkExternalBufferProperties, pNext}
        type FieldIsArray "pNext" VkExternalBufferProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalBufferProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalBufferProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalBufferProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalBufferProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalBufferProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryProperties" VkExternalBufferProperties
         where
        type FieldType "externalMemoryProperties"
               VkExternalBufferProperties
             = VkExternalMemoryProperties
        type FieldOptional "externalMemoryProperties"
               VkExternalBufferProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "externalMemoryProperties"
               VkExternalBufferProperties
             =
             #{offset VkExternalBufferProperties, externalMemoryProperties}
        type FieldIsArray "externalMemoryProperties"
               VkExternalBufferProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalBufferProperties, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanReadField "externalMemoryProperties" VkExternalBufferProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferProperties, externalMemoryProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalBufferProperties, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "externalMemoryProperties" VkExternalBufferProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalBufferProperties, externalMemoryProperties}

instance Show VkExternalBufferProperties where
        showsPrec d x
          = showString "VkExternalBufferProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "externalMemoryProperties = " .
                            showsPrec d (getField @"externalMemoryProperties" x) . showChar '}'

-- | Alias for `VkExternalBufferProperties`
type VkExternalBufferPropertiesKHR = VkExternalBufferProperties

-- | > typedef struct VkExternalFenceProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalFenceHandleTypeFlags exportFromImportedHandleTypes;
--   >     VkExternalFenceHandleTypeFlags compatibleHandleTypes;
--   >     VkExternalFenceFeatureFlags externalFenceFeatures;
--   > } VkExternalFenceProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalFenceProperties VkExternalFenceProperties registry at www.khronos.org>
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

-- | Alias for `VkExternalFenceProperties`
type VkExternalFencePropertiesKHR = VkExternalFenceProperties

-- | > typedef struct VkExternalImageFormatProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryProperties externalMemoryProperties;
--   > } VkExternalImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalImageFormatProperties VkExternalImageFormatProperties registry at www.khronos.org>
data VkExternalImageFormatProperties = VkExternalImageFormatProperties## Addr##
                                                                        ByteArray##

instance Eq VkExternalImageFormatProperties where
        (VkExternalImageFormatProperties## a _) ==
          x@(VkExternalImageFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalImageFormatProperties where
        (VkExternalImageFormatProperties## a _) `compare`
          x@(VkExternalImageFormatProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalImageFormatProperties where
        sizeOf ~_ = #{size VkExternalImageFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalImageFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalImageFormatProperties where
        unsafeAddr (VkExternalImageFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalImageFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalImageFormatProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalImageFormatProperties where
        type StructFields VkExternalImageFormatProperties =
             '["sType", "pNext", "externalMemoryProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalImageFormatProperties =
             '[VkImageFormatProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalImageFormatProperties where
        type FieldType "sType" VkExternalImageFormatProperties =
             VkStructureType
        type FieldOptional "sType" VkExternalImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalImageFormatProperties =
             #{offset VkExternalImageFormatProperties, sType}
        type FieldIsArray "sType" VkExternalImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalImageFormatProperties where
        type FieldType "pNext" VkExternalImageFormatProperties = Ptr Void
        type FieldOptional "pNext" VkExternalImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalImageFormatProperties =
             #{offset VkExternalImageFormatProperties, pNext}
        type FieldIsArray "pNext" VkExternalImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryProperties" VkExternalImageFormatProperties
         where
        type FieldType "externalMemoryProperties"
               VkExternalImageFormatProperties
             = VkExternalMemoryProperties
        type FieldOptional "externalMemoryProperties"
               VkExternalImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "externalMemoryProperties"
               VkExternalImageFormatProperties
             =
             #{offset VkExternalImageFormatProperties, externalMemoryProperties}
        type FieldIsArray "externalMemoryProperties"
               VkExternalImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatProperties, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanReadField "externalMemoryProperties"
           VkExternalImageFormatProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatProperties, externalMemoryProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatProperties, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "externalMemoryProperties"
           VkExternalImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatProperties, externalMemoryProperties}

instance Show VkExternalImageFormatProperties where
        showsPrec d x
          = showString "VkExternalImageFormatProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "externalMemoryProperties = " .
                            showsPrec d (getField @"externalMemoryProperties" x) . showChar '}'

-- | Alias for `VkExternalImageFormatProperties`
type VkExternalImageFormatPropertiesKHR =
     VkExternalImageFormatProperties

-- | > typedef struct VkExternalImageFormatPropertiesNV {
--   >     VkImageFormatProperties          imageFormatProperties;
--   >     VkExternalMemoryFeatureFlagsNV   externalMemoryFeatures;
--   >     VkExternalMemoryHandleTypeFlagsNV exportFromImportedHandleTypes;
--   >     VkExternalMemoryHandleTypeFlagsNV compatibleHandleTypes;
--   > } VkExternalImageFormatPropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalImageFormatPropertiesNV VkExternalImageFormatPropertiesNV registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "imageFormatProperties"
           VkExternalImageFormatPropertiesNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesNV, imageFormatProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesNV, imageFormatProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "imageFormatProperties"
           VkExternalImageFormatPropertiesNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesNV, imageFormatProperties}

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

instance {-# OVERLAPPING #-}
         CanReadField "externalMemoryFeatures"
           VkExternalImageFormatPropertiesNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesNV, externalMemoryFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesNV, externalMemoryFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "externalMemoryFeatures"
           VkExternalImageFormatPropertiesNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesNV, externalMemoryFeatures}

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

instance {-# OVERLAPPING #-}
         CanReadField "exportFromImportedHandleTypes"
           VkExternalImageFormatPropertiesNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesNV, exportFromImportedHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesNV, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "exportFromImportedHandleTypes"
           VkExternalImageFormatPropertiesNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesNV, exportFromImportedHandleTypes}

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

instance {-# OVERLAPPING #-}
         CanReadField "compatibleHandleTypes"
           VkExternalImageFormatPropertiesNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesNV, compatibleHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesNV, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "compatibleHandleTypes"
           VkExternalImageFormatPropertiesNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesNV, compatibleHandleTypes}

instance Show VkExternalImageFormatPropertiesNV where
        showsPrec d x
          = showString "VkExternalImageFormatPropertiesNV {" .
              showString "imageFormatProperties = " .
                showsPrec d (getField @"imageFormatProperties" x) .
                  showString ", " .
                    showString "externalMemoryFeatures = " .
                      showsPrec d (getField @"externalMemoryFeatures" x) .
                        showString ", " .
                          showString "exportFromImportedHandleTypes = " .
                            showsPrec d (getField @"exportFromImportedHandleTypes" x) .
                              showString ", " .
                                showString "compatibleHandleTypes = " .
                                  showsPrec d (getField @"compatibleHandleTypes" x) . showChar '}'

-- | > typedef struct VkExternalMemoryBufferCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlags handleTypes;
--   > } VkExternalMemoryBufferCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalMemoryBufferCreateInfo VkExternalMemoryBufferCreateInfo registry at www.khronos.org>
data VkExternalMemoryBufferCreateInfo = VkExternalMemoryBufferCreateInfo## Addr##
                                                                          ByteArray##

instance Eq VkExternalMemoryBufferCreateInfo where
        (VkExternalMemoryBufferCreateInfo## a _) ==
          x@(VkExternalMemoryBufferCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryBufferCreateInfo where
        (VkExternalMemoryBufferCreateInfo## a _) `compare`
          x@(VkExternalMemoryBufferCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryBufferCreateInfo where
        sizeOf ~_ = #{size VkExternalMemoryBufferCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryBufferCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryBufferCreateInfo where
        unsafeAddr (VkExternalMemoryBufferCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryBufferCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryBufferCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryBufferCreateInfo where
        type StructFields VkExternalMemoryBufferCreateInfo =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryBufferCreateInfo =
             '[VkBufferCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryBufferCreateInfo where
        type FieldType "sType" VkExternalMemoryBufferCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryBufferCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryBufferCreateInfo =
             #{offset VkExternalMemoryBufferCreateInfo, sType}
        type FieldIsArray "sType" VkExternalMemoryBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalMemoryBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalMemoryBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryBufferCreateInfo where
        type FieldType "pNext" VkExternalMemoryBufferCreateInfo = Ptr Void
        type FieldOptional "pNext" VkExternalMemoryBufferCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryBufferCreateInfo =
             #{offset VkExternalMemoryBufferCreateInfo, pNext}
        type FieldIsArray "pNext" VkExternalMemoryBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalMemoryBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalMemoryBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryBufferCreateInfo where
        type FieldType "handleTypes" VkExternalMemoryBufferCreateInfo =
             VkExternalMemoryHandleTypeFlags
        type FieldOptional "handleTypes" VkExternalMemoryBufferCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryBufferCreateInfo =
             #{offset VkExternalMemoryBufferCreateInfo, handleTypes}
        type FieldIsArray "handleTypes" VkExternalMemoryBufferCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExternalMemoryBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfo, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExternalMemoryBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfo, handleTypes}

instance Show VkExternalMemoryBufferCreateInfo where
        showsPrec d x
          = showString "VkExternalMemoryBufferCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'

-- | Alias for `VkExternalMemoryBufferCreateInfo`
type VkExternalMemoryBufferCreateInfoKHR =
     VkExternalMemoryBufferCreateInfo

-- | > typedef struct VkExternalMemoryImageCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlags handleTypes;
--   > } VkExternalMemoryImageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalMemoryImageCreateInfo VkExternalMemoryImageCreateInfo registry at www.khronos.org>
data VkExternalMemoryImageCreateInfo = VkExternalMemoryImageCreateInfo## Addr##
                                                                        ByteArray##

instance Eq VkExternalMemoryImageCreateInfo where
        (VkExternalMemoryImageCreateInfo## a _) ==
          x@(VkExternalMemoryImageCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryImageCreateInfo where
        (VkExternalMemoryImageCreateInfo## a _) `compare`
          x@(VkExternalMemoryImageCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryImageCreateInfo where
        sizeOf ~_ = #{size VkExternalMemoryImageCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryImageCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryImageCreateInfo where
        unsafeAddr (VkExternalMemoryImageCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryImageCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryImageCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryImageCreateInfo where
        type StructFields VkExternalMemoryImageCreateInfo =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryImageCreateInfo =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryImageCreateInfo where
        type FieldType "sType" VkExternalMemoryImageCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryImageCreateInfo =
             #{offset VkExternalMemoryImageCreateInfo, sType}
        type FieldIsArray "sType" VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalMemoryImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalMemoryImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryImageCreateInfo where
        type FieldType "pNext" VkExternalMemoryImageCreateInfo = Ptr Void
        type FieldOptional "pNext" VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryImageCreateInfo =
             #{offset VkExternalMemoryImageCreateInfo, pNext}
        type FieldIsArray "pNext" VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalMemoryImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalMemoryImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryImageCreateInfo where
        type FieldType "handleTypes" VkExternalMemoryImageCreateInfo =
             VkExternalMemoryHandleTypeFlags
        type FieldOptional "handleTypes" VkExternalMemoryImageCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryImageCreateInfo =
             #{offset VkExternalMemoryImageCreateInfo, handleTypes}
        type FieldIsArray "handleTypes" VkExternalMemoryImageCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExternalMemoryImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfo, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExternalMemoryImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfo, handleTypes}

instance Show VkExternalMemoryImageCreateInfo where
        showsPrec d x
          = showString "VkExternalMemoryImageCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'

-- | Alias for `VkExternalMemoryImageCreateInfo`
type VkExternalMemoryImageCreateInfoKHR =
     VkExternalMemoryImageCreateInfo

-- | > typedef struct VkExternalMemoryImageCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleTypes;
--   > } VkExternalMemoryImageCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalMemoryImageCreateInfoNV VkExternalMemoryImageCreateInfoNV registry at www.khronos.org>
data VkExternalMemoryImageCreateInfoNV = VkExternalMemoryImageCreateInfoNV## Addr##
                                                                            ByteArray##

instance Eq VkExternalMemoryImageCreateInfoNV where
        (VkExternalMemoryImageCreateInfoNV## a _) ==
          x@(VkExternalMemoryImageCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryImageCreateInfoNV where
        (VkExternalMemoryImageCreateInfoNV## a _) `compare`
          x@(VkExternalMemoryImageCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryImageCreateInfoNV where
        sizeOf ~_ = #{size VkExternalMemoryImageCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryImageCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryImageCreateInfoNV where
        unsafeAddr (VkExternalMemoryImageCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryImageCreateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryImageCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryImageCreateInfoNV where
        type StructFields VkExternalMemoryImageCreateInfoNV =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryImageCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryImageCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryImageCreateInfoNV =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryImageCreateInfoNV where
        type FieldType "sType" VkExternalMemoryImageCreateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryImageCreateInfoNV =
             #{offset VkExternalMemoryImageCreateInfoNV, sType}
        type FieldIsArray "sType" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalMemoryImageCreateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalMemoryImageCreateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryImageCreateInfoNV where
        type FieldType "pNext" VkExternalMemoryImageCreateInfoNV = Ptr Void
        type FieldOptional "pNext" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryImageCreateInfoNV =
             #{offset VkExternalMemoryImageCreateInfoNV, pNext}
        type FieldIsArray "pNext" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalMemoryImageCreateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalMemoryImageCreateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryImageCreateInfoNV where
        type FieldType "handleTypes" VkExternalMemoryImageCreateInfoNV =
             VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "handleTypes" VkExternalMemoryImageCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryImageCreateInfoNV =
             #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}
        type FieldIsArray "handleTypes" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExternalMemoryImageCreateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExternalMemoryImageCreateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

instance Show VkExternalMemoryImageCreateInfoNV where
        showsPrec d x
          = showString "VkExternalMemoryImageCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'

-- | > typedef struct VkExternalMemoryProperties {
--   >     VkExternalMemoryFeatureFlags  externalMemoryFeatures;
--   >     VkExternalMemoryHandleTypeFlags exportFromImportedHandleTypes;
--   >     VkExternalMemoryHandleTypeFlags compatibleHandleTypes;
--   > } VkExternalMemoryProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalMemoryProperties VkExternalMemoryProperties registry at www.khronos.org>
data VkExternalMemoryProperties = VkExternalMemoryProperties## Addr##
                                                              ByteArray##

instance Eq VkExternalMemoryProperties where
        (VkExternalMemoryProperties## a _) ==
          x@(VkExternalMemoryProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryProperties where
        (VkExternalMemoryProperties## a _) `compare`
          x@(VkExternalMemoryProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryProperties where
        sizeOf ~_ = #{size VkExternalMemoryProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExternalMemoryProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryProperties where
        unsafeAddr (VkExternalMemoryProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryProperties where
        type StructFields VkExternalMemoryProperties =
             '["externalMemoryFeatures", "exportFromImportedHandleTypes", -- ' closing tick for hsc2hs
               "compatibleHandleTypes"]
        type CUnionType VkExternalMemoryProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "externalMemoryFeatures" VkExternalMemoryProperties where
        type FieldType "externalMemoryFeatures" VkExternalMemoryProperties
             = VkExternalMemoryFeatureFlags
        type FieldOptional "externalMemoryFeatures"
               VkExternalMemoryProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "externalMemoryFeatures"
               VkExternalMemoryProperties
             =
             #{offset VkExternalMemoryProperties, externalMemoryFeatures}
        type FieldIsArray "externalMemoryFeatures"
               VkExternalMemoryProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryProperties, externalMemoryFeatures}

instance {-# OVERLAPPING #-}
         CanReadField "externalMemoryFeatures" VkExternalMemoryProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryProperties, externalMemoryFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryProperties, externalMemoryFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "externalMemoryFeatures" VkExternalMemoryProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryProperties, externalMemoryFeatures}

instance {-# OVERLAPPING #-}
         HasField "exportFromImportedHandleTypes" VkExternalMemoryProperties
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalMemoryProperties
             = VkExternalMemoryHandleTypeFlags
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalMemoryProperties
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalMemoryProperties
             =
             #{offset VkExternalMemoryProperties, exportFromImportedHandleTypes}
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalMemoryProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "exportFromImportedHandleTypes"
           VkExternalMemoryProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryProperties, exportFromImportedHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "exportFromImportedHandleTypes"
           VkExternalMemoryProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "compatibleHandleTypes" VkExternalMemoryProperties where
        type FieldType "compatibleHandleTypes" VkExternalMemoryProperties =
             VkExternalMemoryHandleTypeFlags
        type FieldOptional "compatibleHandleTypes"
               VkExternalMemoryProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compatibleHandleTypes" VkExternalMemoryProperties
             =
             #{offset VkExternalMemoryProperties, compatibleHandleTypes}
        type FieldIsArray "compatibleHandleTypes"
               VkExternalMemoryProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "compatibleHandleTypes" VkExternalMemoryProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryProperties, compatibleHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "compatibleHandleTypes" VkExternalMemoryProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryProperties, compatibleHandleTypes}

instance Show VkExternalMemoryProperties where
        showsPrec d x
          = showString "VkExternalMemoryProperties {" .
              showString "externalMemoryFeatures = " .
                showsPrec d (getField @"externalMemoryFeatures" x) .
                  showString ", " .
                    showString "exportFromImportedHandleTypes = " .
                      showsPrec d (getField @"exportFromImportedHandleTypes" x) .
                        showString ", " .
                          showString "compatibleHandleTypes = " .
                            showsPrec d (getField @"compatibleHandleTypes" x) . showChar '}'

-- | Alias for `VkExternalMemoryProperties`
type VkExternalMemoryPropertiesKHR = VkExternalMemoryProperties

-- | > typedef struct VkExternalSemaphoreProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalSemaphoreHandleTypeFlags exportFromImportedHandleTypes;
--   >     VkExternalSemaphoreHandleTypeFlags compatibleHandleTypes;
--   >     VkExternalSemaphoreFeatureFlags externalSemaphoreFeatures;
--   > } VkExternalSemaphoreProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalSemaphoreProperties VkExternalSemaphoreProperties registry at www.khronos.org>
data VkExternalSemaphoreProperties = VkExternalSemaphoreProperties## Addr##
                                                                    ByteArray##

instance Eq VkExternalSemaphoreProperties where
        (VkExternalSemaphoreProperties## a _) ==
          x@(VkExternalSemaphoreProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalSemaphoreProperties where
        (VkExternalSemaphoreProperties## a _) `compare`
          x@(VkExternalSemaphoreProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalSemaphoreProperties where
        sizeOf ~_ = #{size VkExternalSemaphoreProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalSemaphoreProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalSemaphoreProperties where
        unsafeAddr (VkExternalSemaphoreProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalSemaphoreProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalSemaphoreProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalSemaphoreProperties where
        type StructFields VkExternalSemaphoreProperties =
             '["sType", "pNext", "exportFromImportedHandleTypes", -- ' closing tick for hsc2hs
               "compatibleHandleTypes", "externalSemaphoreFeatures"]
        type CUnionType VkExternalSemaphoreProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalSemaphoreProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalSemaphoreProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalSemaphoreProperties where
        type FieldType "sType" VkExternalSemaphoreProperties =
             VkStructureType
        type FieldOptional "sType" VkExternalSemaphoreProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalSemaphoreProperties =
             #{offset VkExternalSemaphoreProperties, sType}
        type FieldIsArray "sType" VkExternalSemaphoreProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphoreProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalSemaphoreProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphoreProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphoreProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalSemaphoreProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphoreProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalSemaphoreProperties where
        type FieldType "pNext" VkExternalSemaphoreProperties = Ptr Void
        type FieldOptional "pNext" VkExternalSemaphoreProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalSemaphoreProperties =
             #{offset VkExternalSemaphoreProperties, pNext}
        type FieldIsArray "pNext" VkExternalSemaphoreProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphoreProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalSemaphoreProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphoreProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphoreProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalSemaphoreProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphoreProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "exportFromImportedHandleTypes"
           VkExternalSemaphoreProperties
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalSemaphoreProperties
             = VkExternalSemaphoreHandleTypeFlags
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalSemaphoreProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalSemaphoreProperties
             =
             #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes}
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalSemaphoreProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "exportFromImportedHandleTypes"
           VkExternalSemaphoreProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "exportFromImportedHandleTypes"
           VkExternalSemaphoreProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "compatibleHandleTypes" VkExternalSemaphoreProperties
         where
        type FieldType "compatibleHandleTypes"
               VkExternalSemaphoreProperties
             = VkExternalSemaphoreHandleTypeFlags
        type FieldOptional "compatibleHandleTypes"
               VkExternalSemaphoreProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compatibleHandleTypes"
               VkExternalSemaphoreProperties
             =
             #{offset VkExternalSemaphoreProperties, compatibleHandleTypes}
        type FieldIsArray "compatibleHandleTypes"
               VkExternalSemaphoreProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphoreProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "compatibleHandleTypes" VkExternalSemaphoreProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphoreProperties, compatibleHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphoreProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "compatibleHandleTypes" VkExternalSemaphoreProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphoreProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "externalSemaphoreFeatures" VkExternalSemaphoreProperties
         where
        type FieldType "externalSemaphoreFeatures"
               VkExternalSemaphoreProperties
             = VkExternalSemaphoreFeatureFlags
        type FieldOptional "externalSemaphoreFeatures"
               VkExternalSemaphoreProperties
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "externalSemaphoreFeatures"
               VkExternalSemaphoreProperties
             =
             #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures}
        type FieldIsArray "externalSemaphoreFeatures"
               VkExternalSemaphoreProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures}

instance {-# OVERLAPPING #-}
         CanReadField "externalSemaphoreFeatures"
           VkExternalSemaphoreProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "externalSemaphoreFeatures"
           VkExternalSemaphoreProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures}

instance Show VkExternalSemaphoreProperties where
        showsPrec d x
          = showString "VkExternalSemaphoreProperties {" .
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
                                      showString "externalSemaphoreFeatures = " .
                                        showsPrec d (getField @"externalSemaphoreFeatures" x) .
                                          showChar '}'

-- | Alias for `VkExternalSemaphoreProperties`
type VkExternalSemaphorePropertiesKHR =
     VkExternalSemaphoreProperties
