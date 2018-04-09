#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalMemoryProperties
       (VkExternalMemoryProperties(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Base                                                   (Addr##,
                                                                             ByteArray##,
                                                                             byteArrayContents##,
                                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlags    (VkExternalMemoryFeatureFlags)
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags (VkExternalMemoryHandleTypeFlags)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryProperties {
--   >     VkExternalMemoryFeatureFlags  externalMemoryFeatures;
--   >     VkExternalMemoryHandleTypeFlags exportFromImportedHandleTypes;
--   >     VkExternalMemoryHandleTypeFlags compatibleHandleTypes;
--   > } VkExternalMemoryProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkExternalMemoryPropertiesVkExternalMemoryProperties registry at www.khronos.org>
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
