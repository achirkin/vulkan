#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalImageFormatPropertiesNV
       (VkExternalImageFormatPropertiesNV(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagsNV    (VkExternalMemoryFeatureFlagsNV)
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV (VkExternalMemoryHandleTypeFlagsNV)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties         (VkImageFormatProperties)
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkExternalImageFormatPropertiesNV {
--   >     VkImageFormatProperties          imageFormatProperties;
--   >     VkExternalMemoryFeatureFlagsNV   externalMemoryFeatures;
--   >     VkExternalMemoryHandleTypeFlagsNV exportFromImportedHandleTypes;
--   >     VkExternalMemoryHandleTypeFlagsNV compatibleHandleTypes;
--   > } VkExternalImageFormatPropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkExternalImageFormatPropertiesNV.html VkExternalImageFormatPropertiesNV registry at www.khronos.org>
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
