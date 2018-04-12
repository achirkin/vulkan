#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformAndroidKhr
       (VkAndroidHardwareBufferFormatPropertiesANDROID(..),
        VkAndroidHardwareBufferPropertiesANDROID(..),
        VkAndroidHardwareBufferUsageANDROID(..),
        VkAndroidSurfaceCreateInfoKHR(..), VkExternalFormatANDROID(..),
        VkImportAndroidHardwareBufferInfoANDROID(..),
        VkMemoryGetAndroidHardwareBufferInfoANDROID(..))
       where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkDeviceSize)
import           Graphics.Vulkan.Types.Bitmasks                (VkAndroidSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Defines                 (AHardwareBuffer,
                                                                ANativeWindow)
import           Graphics.Vulkan.Types.Enum.ChromaLocation     (VkChromaLocation)
import           Graphics.Vulkan.Types.Enum.Format             (VkFormat, VkFormatFeatureFlags)
import           Graphics.Vulkan.Types.Enum.Sampler            (VkSamplerYcbcrModelConversion,
                                                                VkSamplerYcbcrRange)
import           Graphics.Vulkan.Types.Enum.StructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                 (VkDeviceMemory)
import           Graphics.Vulkan.Types.Struct.ComponentMapping (VkComponentMapping)
import           Graphics.Vulkan.Types.Struct.Image            (VkImageCreateInfo,
                                                                VkImageFormatProperties2)
import           Graphics.Vulkan.Types.Struct.Memory           (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.Struct.Sampler          (VkSamplerYcbcrConversionCreateInfo)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkAndroidHardwareBufferFormatPropertiesANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     VkFormat                           format;
--   >     uint64_t                           externalFormat;
--   >     VkFormatFeatureFlags               formatFeatures;
--   >     VkComponentMapping                 samplerYcbcrConversionComponents;
--   >     VkSamplerYcbcrModelConversion      suggestedYcbcrModel;
--   >     VkSamplerYcbcrRange                suggestedYcbcrRange;
--   >     VkChromaLocation                   suggestedXChromaOffset;
--   >     VkChromaLocation                   suggestedYChromaOffset;
--   > } VkAndroidHardwareBufferFormatPropertiesANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAndroidHardwareBufferFormatPropertiesANDROID VkAndroidHardwareBufferFormatPropertiesANDROID registry at www.khronos.org>
data VkAndroidHardwareBufferFormatPropertiesANDROID = VkAndroidHardwareBufferFormatPropertiesANDROID## Addr##
                                                                                                      ByteArray##

instance Eq VkAndroidHardwareBufferFormatPropertiesANDROID where
        (VkAndroidHardwareBufferFormatPropertiesANDROID## a _) ==
          x@(VkAndroidHardwareBufferFormatPropertiesANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAndroidHardwareBufferFormatPropertiesANDROID where
        (VkAndroidHardwareBufferFormatPropertiesANDROID## a _) `compare`
          x@(VkAndroidHardwareBufferFormatPropertiesANDROID## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        sizeOf ~_
          = #{size VkAndroidHardwareBufferFormatPropertiesANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAndroidHardwareBufferFormatPropertiesANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        unsafeAddr (VkAndroidHardwareBufferFormatPropertiesANDROID## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkAndroidHardwareBufferFormatPropertiesANDROID## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAndroidHardwareBufferFormatPropertiesANDROID##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type StructFields VkAndroidHardwareBufferFormatPropertiesANDROID =
             '["sType", "pNext", "format", "externalFormat", "formatFeatures", -- ' closing tick for hsc2hs
               "samplerYcbcrConversionComponents", "suggestedYcbcrModel",
               "suggestedYcbcrRange", "suggestedXChromaOffset",
               "suggestedYChromaOffset"]
        type CUnionType VkAndroidHardwareBufferFormatPropertiesANDROID =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAndroidHardwareBufferFormatPropertiesANDROID =
             'True -- ' closing tick for hsc2hs
        type StructExtends VkAndroidHardwareBufferFormatPropertiesANDROID =
             '[VkAndroidHardwareBufferPropertiesANDROID] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type FieldType "sType"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = VkStructureType
        type FieldOptional "sType"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, sType}
        type FieldIsArray "sType"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type FieldType "pNext"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = Ptr Void
        type FieldOptional "pNext"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, pNext}
        type FieldIsArray "pNext"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "format" VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type FieldType "format"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = VkFormat
        type FieldOptional "format"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, format}
        type FieldIsArray "format"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, format}

instance {-# OVERLAPPING #-}
         CanReadField "format"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, format}

instance {-# OVERLAPPING #-}
         HasField "externalFormat"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type FieldType "externalFormat"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = Word64
        type FieldOptional "externalFormat"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "externalFormat"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, externalFormat}
        type FieldIsArray "externalFormat"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, externalFormat}

instance {-# OVERLAPPING #-}
         CanReadField "externalFormat"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, externalFormat})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, externalFormat}

instance {-# OVERLAPPING #-}
         CanWriteField "externalFormat"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, externalFormat}

instance {-# OVERLAPPING #-}
         HasField "formatFeatures"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type FieldType "formatFeatures"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = VkFormatFeatureFlags
        type FieldOptional "formatFeatures"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "formatFeatures"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, formatFeatures}
        type FieldIsArray "formatFeatures"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, formatFeatures}

instance {-# OVERLAPPING #-}
         CanReadField "formatFeatures"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, formatFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, formatFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "formatFeatures"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, formatFeatures}

instance {-# OVERLAPPING #-}
         HasField "samplerYcbcrConversionComponents"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type FieldType "samplerYcbcrConversionComponents"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = VkComponentMapping
        type FieldOptional "samplerYcbcrConversionComponents"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "samplerYcbcrConversionComponents"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, samplerYcbcrConversionComponents}
        type FieldIsArray "samplerYcbcrConversionComponents"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, samplerYcbcrConversionComponents}

instance {-# OVERLAPPING #-}
         CanReadField "samplerYcbcrConversionComponents"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, samplerYcbcrConversionComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, samplerYcbcrConversionComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "samplerYcbcrConversionComponents"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, samplerYcbcrConversionComponents}

instance {-# OVERLAPPING #-}
         HasField "suggestedYcbcrModel"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type FieldType "suggestedYcbcrModel"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = VkSamplerYcbcrModelConversion
        type FieldOptional "suggestedYcbcrModel"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "suggestedYcbcrModel"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrModel}
        type FieldIsArray "suggestedYcbcrModel"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrModel}

instance {-# OVERLAPPING #-}
         CanReadField "suggestedYcbcrModel"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrModel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrModel}

instance {-# OVERLAPPING #-}
         CanWriteField "suggestedYcbcrModel"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrModel}

instance {-# OVERLAPPING #-}
         HasField "suggestedYcbcrRange"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type FieldType "suggestedYcbcrRange"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = VkSamplerYcbcrRange
        type FieldOptional "suggestedYcbcrRange"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "suggestedYcbcrRange"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrRange}
        type FieldIsArray "suggestedYcbcrRange"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrRange}

instance {-# OVERLAPPING #-}
         CanReadField "suggestedYcbcrRange"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrRange})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrRange}

instance {-# OVERLAPPING #-}
         CanWriteField "suggestedYcbcrRange"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrRange}

instance {-# OVERLAPPING #-}
         HasField "suggestedXChromaOffset"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type FieldType "suggestedXChromaOffset"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = VkChromaLocation
        type FieldOptional "suggestedXChromaOffset"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "suggestedXChromaOffset"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedXChromaOffset}
        type FieldIsArray "suggestedXChromaOffset"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedXChromaOffset}

instance {-# OVERLAPPING #-}
         CanReadField "suggestedXChromaOffset"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedXChromaOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedXChromaOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "suggestedXChromaOffset"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedXChromaOffset}

instance {-# OVERLAPPING #-}
         HasField "suggestedYChromaOffset"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        type FieldType "suggestedYChromaOffset"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = VkChromaLocation
        type FieldOptional "suggestedYChromaOffset"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "suggestedYChromaOffset"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYChromaOffset}
        type FieldIsArray "suggestedYChromaOffset"
               VkAndroidHardwareBufferFormatPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYChromaOffset}

instance {-# OVERLAPPING #-}
         CanReadField "suggestedYChromaOffset"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYChromaOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYChromaOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "suggestedYChromaOffset"
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYChromaOffset}

instance Show VkAndroidHardwareBufferFormatPropertiesANDROID where
        showsPrec d x
          = showString "VkAndroidHardwareBufferFormatPropertiesANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "format = " .
                            showsPrec d (getField @"format" x) .
                              showString ", " .
                                showString "externalFormat = " .
                                  showsPrec d (getField @"externalFormat" x) .
                                    showString ", " .
                                      showString "formatFeatures = " .
                                        showsPrec d (getField @"formatFeatures" x) .
                                          showString ", " .
                                            showString "samplerYcbcrConversionComponents = " .
                                              showsPrec d
                                                (getField @"samplerYcbcrConversionComponents" x)
                                                .
                                                showString ", " .
                                                  showString "suggestedYcbcrModel = " .
                                                    showsPrec d (getField @"suggestedYcbcrModel" x)
                                                      .
                                                      showString ", " .
                                                        showString "suggestedYcbcrRange = " .
                                                          showsPrec d
                                                            (getField @"suggestedYcbcrRange" x)
                                                            .
                                                            showString ", " .
                                                              showString "suggestedXChromaOffset = "
                                                                .
                                                                showsPrec d
                                                                  (getField
                                                                     @"suggestedXChromaOffset"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "suggestedYChromaOffset = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"suggestedYChromaOffset"
                                                                           x)
                                                                        . showChar '}'

-- | > typedef struct VkAndroidHardwareBufferPropertiesANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     VkDeviceSize                       allocationSize;
--   >     uint32_t                           memoryTypeBits;
--   > } VkAndroidHardwareBufferPropertiesANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAndroidHardwareBufferPropertiesANDROID VkAndroidHardwareBufferPropertiesANDROID registry at www.khronos.org>
data VkAndroidHardwareBufferPropertiesANDROID = VkAndroidHardwareBufferPropertiesANDROID## Addr##
                                                                                          ByteArray##

instance Eq VkAndroidHardwareBufferPropertiesANDROID where
        (VkAndroidHardwareBufferPropertiesANDROID## a _) ==
          x@(VkAndroidHardwareBufferPropertiesANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAndroidHardwareBufferPropertiesANDROID where
        (VkAndroidHardwareBufferPropertiesANDROID## a _) `compare`
          x@(VkAndroidHardwareBufferPropertiesANDROID## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAndroidHardwareBufferPropertiesANDROID where
        sizeOf ~_
          = #{size VkAndroidHardwareBufferPropertiesANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAndroidHardwareBufferPropertiesANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAndroidHardwareBufferPropertiesANDROID
         where
        unsafeAddr (VkAndroidHardwareBufferPropertiesANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAndroidHardwareBufferPropertiesANDROID## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAndroidHardwareBufferPropertiesANDROID##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAndroidHardwareBufferPropertiesANDROID
         where
        type StructFields VkAndroidHardwareBufferPropertiesANDROID =
             '["sType", "pNext", "allocationSize", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkAndroidHardwareBufferPropertiesANDROID = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAndroidHardwareBufferPropertiesANDROID = 'True -- ' closing tick for hsc2hs
        type StructExtends VkAndroidHardwareBufferPropertiesANDROID = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkAndroidHardwareBufferPropertiesANDROID where
        type FieldType "sType" VkAndroidHardwareBufferPropertiesANDROID =
             VkStructureType
        type FieldOptional "sType" VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkAndroidHardwareBufferPropertiesANDROID =
             #{offset VkAndroidHardwareBufferPropertiesANDROID, sType}
        type FieldIsArray "sType" VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferPropertiesANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkAndroidHardwareBufferPropertiesANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferPropertiesANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkAndroidHardwareBufferPropertiesANDROID where
        type FieldType "pNext" VkAndroidHardwareBufferPropertiesANDROID =
             Ptr Void
        type FieldOptional "pNext" VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkAndroidHardwareBufferPropertiesANDROID =
             #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext}
        type FieldIsArray "pNext" VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkAndroidHardwareBufferPropertiesANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "allocationSize" VkAndroidHardwareBufferPropertiesANDROID
         where
        type FieldType "allocationSize"
               VkAndroidHardwareBufferPropertiesANDROID
             = VkDeviceSize
        type FieldOptional "allocationSize"
               VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "allocationSize"
               VkAndroidHardwareBufferPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize}
        type FieldIsArray "allocationSize"
               VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize}

instance {-# OVERLAPPING #-}
         CanReadField "allocationSize"
           VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize}

instance {-# OVERLAPPING #-}
         CanWriteField "allocationSize"
           VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkAndroidHardwareBufferPropertiesANDROID
         where
        type FieldType "memoryTypeBits"
               VkAndroidHardwareBufferPropertiesANDROID
             = Word32
        type FieldOptional "memoryTypeBits"
               VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits"
               VkAndroidHardwareBufferPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits}
        type FieldIsArray "memoryTypeBits"
               VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeBits"
           VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeBits"
           VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits}

instance Show VkAndroidHardwareBufferPropertiesANDROID where
        showsPrec d x
          = showString "VkAndroidHardwareBufferPropertiesANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "allocationSize = " .
                            showsPrec d (getField @"allocationSize" x) .
                              showString ", " .
                                showString "memoryTypeBits = " .
                                  showsPrec d (getField @"memoryTypeBits" x) . showChar '}'

-- | > typedef struct VkAndroidHardwareBufferUsageANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     uint64_t                           androidHardwareBufferUsage;
--   > } VkAndroidHardwareBufferUsageANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAndroidHardwareBufferUsageANDROID VkAndroidHardwareBufferUsageANDROID registry at www.khronos.org>
data VkAndroidHardwareBufferUsageANDROID = VkAndroidHardwareBufferUsageANDROID## Addr##
                                                                                ByteArray##

instance Eq VkAndroidHardwareBufferUsageANDROID where
        (VkAndroidHardwareBufferUsageANDROID## a _) ==
          x@(VkAndroidHardwareBufferUsageANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAndroidHardwareBufferUsageANDROID where
        (VkAndroidHardwareBufferUsageANDROID## a _) `compare`
          x@(VkAndroidHardwareBufferUsageANDROID## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAndroidHardwareBufferUsageANDROID where
        sizeOf ~_ = #{size VkAndroidHardwareBufferUsageANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAndroidHardwareBufferUsageANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAndroidHardwareBufferUsageANDROID
         where
        unsafeAddr (VkAndroidHardwareBufferUsageANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAndroidHardwareBufferUsageANDROID## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAndroidHardwareBufferUsageANDROID##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAndroidHardwareBufferUsageANDROID where
        type StructFields VkAndroidHardwareBufferUsageANDROID =
             '["sType", "pNext", "androidHardwareBufferUsage"] -- ' closing tick for hsc2hs
        type CUnionType VkAndroidHardwareBufferUsageANDROID = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAndroidHardwareBufferUsageANDROID = 'True -- ' closing tick for hsc2hs
        type StructExtends VkAndroidHardwareBufferUsageANDROID =
             '[VkImageFormatProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkAndroidHardwareBufferUsageANDROID where
        type FieldType "sType" VkAndroidHardwareBufferUsageANDROID =
             VkStructureType
        type FieldOptional "sType" VkAndroidHardwareBufferUsageANDROID =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkAndroidHardwareBufferUsageANDROID =
             #{offset VkAndroidHardwareBufferUsageANDROID, sType}
        type FieldIsArray "sType" VkAndroidHardwareBufferUsageANDROID =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferUsageANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkAndroidHardwareBufferUsageANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferUsageANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkAndroidHardwareBufferUsageANDROID where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkAndroidHardwareBufferUsageANDROID where
        type FieldType "pNext" VkAndroidHardwareBufferUsageANDROID =
             Ptr Void
        type FieldOptional "pNext" VkAndroidHardwareBufferUsageANDROID =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkAndroidHardwareBufferUsageANDROID =
             #{offset VkAndroidHardwareBufferUsageANDROID, pNext}
        type FieldIsArray "pNext" VkAndroidHardwareBufferUsageANDROID =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferUsageANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkAndroidHardwareBufferUsageANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferUsageANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkAndroidHardwareBufferUsageANDROID where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "androidHardwareBufferUsage"
           VkAndroidHardwareBufferUsageANDROID
         where
        type FieldType "androidHardwareBufferUsage"
               VkAndroidHardwareBufferUsageANDROID
             = Word64
        type FieldOptional "androidHardwareBufferUsage"
               VkAndroidHardwareBufferUsageANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "androidHardwareBufferUsage"
               VkAndroidHardwareBufferUsageANDROID
             =
             #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage}
        type FieldIsArray "androidHardwareBufferUsage"
               VkAndroidHardwareBufferUsageANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage}

instance {-# OVERLAPPING #-}
         CanReadField "androidHardwareBufferUsage"
           VkAndroidHardwareBufferUsageANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage}

instance {-# OVERLAPPING #-}
         CanWriteField "androidHardwareBufferUsage"
           VkAndroidHardwareBufferUsageANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage}

instance Show VkAndroidHardwareBufferUsageANDROID where
        showsPrec d x
          = showString "VkAndroidHardwareBufferUsageANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "androidHardwareBufferUsage = " .
                            showsPrec d (getField @"androidHardwareBufferUsage" x) .
                              showChar '}'

-- | > typedef struct VkAndroidSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                    pNext;
--   >     VkAndroidSurfaceCreateFlagsKHR flags;
--   >     struct ANativeWindow*    window;
--   > } VkAndroidSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAndroidSurfaceCreateInfoKHR VkAndroidSurfaceCreateInfoKHR registry at www.khronos.org>
data VkAndroidSurfaceCreateInfoKHR = VkAndroidSurfaceCreateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkAndroidSurfaceCreateInfoKHR where
        (VkAndroidSurfaceCreateInfoKHR## a _) ==
          x@(VkAndroidSurfaceCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAndroidSurfaceCreateInfoKHR where
        (VkAndroidSurfaceCreateInfoKHR## a _) `compare`
          x@(VkAndroidSurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAndroidSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkAndroidSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAndroidSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAndroidSurfaceCreateInfoKHR where
        unsafeAddr (VkAndroidSurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAndroidSurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAndroidSurfaceCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAndroidSurfaceCreateInfoKHR where
        type StructFields VkAndroidSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "window"] -- ' closing tick for hsc2hs
        type CUnionType VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAndroidSurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "sType" VkAndroidSurfaceCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkAndroidSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "pNext" VkAndroidSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkAndroidSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "flags" VkAndroidSurfaceCreateInfoKHR =
             VkAndroidSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkAndroidSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkAndroidSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "window" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "window" VkAndroidSurfaceCreateInfoKHR =
             Ptr ANativeWindow
        type FieldOptional "window" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "window" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, window}
        type FieldIsArray "window" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         CanReadField "window" VkAndroidSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, window})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         CanWriteField "window" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, window}

instance Show VkAndroidSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkAndroidSurfaceCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "window = " .
                                  showsPrec d (getField @"window" x) . showChar '}'

-- | > typedef struct VkExternalFormatANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     uint64_t                           externalFormat;
--   > } VkExternalFormatANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalFormatANDROID VkExternalFormatANDROID registry at www.khronos.org>
data VkExternalFormatANDROID = VkExternalFormatANDROID## Addr##
                                                        ByteArray##

instance Eq VkExternalFormatANDROID where
        (VkExternalFormatANDROID## a _) == x@(VkExternalFormatANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalFormatANDROID where
        (VkExternalFormatANDROID## a _) `compare`
          x@(VkExternalFormatANDROID## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalFormatANDROID where
        sizeOf ~_ = #{size VkExternalFormatANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExternalFormatANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalFormatANDROID where
        unsafeAddr (VkExternalFormatANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalFormatANDROID## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalFormatANDROID## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalFormatANDROID where
        type StructFields VkExternalFormatANDROID =
             '["sType", "pNext", "externalFormat"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalFormatANDROID =
             '[VkImageCreateInfo, VkSamplerYcbcrConversionCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalFormatANDROID where
        type FieldType "sType" VkExternalFormatANDROID = VkStructureType
        type FieldOptional "sType" VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalFormatANDROID =
             #{offset VkExternalFormatANDROID, sType}
        type FieldIsArray "sType" VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExternalFormatANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalFormatANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFormatANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFormatANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalFormatANDROID where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFormatANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalFormatANDROID where
        type FieldType "pNext" VkExternalFormatANDROID = Ptr Void
        type FieldOptional "pNext" VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalFormatANDROID =
             #{offset VkExternalFormatANDROID, pNext}
        type FieldIsArray "pNext" VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExternalFormatANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalFormatANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFormatANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFormatANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalFormatANDROID where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFormatANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "externalFormat" VkExternalFormatANDROID where
        type FieldType "externalFormat" VkExternalFormatANDROID = Word64
        type FieldOptional "externalFormat" VkExternalFormatANDROID =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "externalFormat" VkExternalFormatANDROID =
             #{offset VkExternalFormatANDROID, externalFormat}
        type FieldIsArray "externalFormat" VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFormatANDROID, externalFormat}

instance {-# OVERLAPPING #-}
         CanReadField "externalFormat" VkExternalFormatANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFormatANDROID, externalFormat})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFormatANDROID, externalFormat}

instance {-# OVERLAPPING #-}
         CanWriteField "externalFormat" VkExternalFormatANDROID where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFormatANDROID, externalFormat}

instance Show VkExternalFormatANDROID where
        showsPrec d x
          = showString "VkExternalFormatANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "externalFormat = " .
                            showsPrec d (getField @"externalFormat" x) . showChar '}'

-- | > typedef struct VkImportAndroidHardwareBufferInfoANDROID {
--   >     VkStructureType sType;
--   >     const void*                        pNext;
--   >     struct AHardwareBuffer*            buffer;
--   > } VkImportAndroidHardwareBufferInfoANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportAndroidHardwareBufferInfoANDROID VkImportAndroidHardwareBufferInfoANDROID registry at www.khronos.org>
data VkImportAndroidHardwareBufferInfoANDROID = VkImportAndroidHardwareBufferInfoANDROID## Addr##
                                                                                          ByteArray##

instance Eq VkImportAndroidHardwareBufferInfoANDROID where
        (VkImportAndroidHardwareBufferInfoANDROID## a _) ==
          x@(VkImportAndroidHardwareBufferInfoANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportAndroidHardwareBufferInfoANDROID where
        (VkImportAndroidHardwareBufferInfoANDROID## a _) `compare`
          x@(VkImportAndroidHardwareBufferInfoANDROID## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportAndroidHardwareBufferInfoANDROID where
        sizeOf ~_
          = #{size VkImportAndroidHardwareBufferInfoANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportAndroidHardwareBufferInfoANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportAndroidHardwareBufferInfoANDROID
         where
        unsafeAddr (VkImportAndroidHardwareBufferInfoANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportAndroidHardwareBufferInfoANDROID## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportAndroidHardwareBufferInfoANDROID##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportAndroidHardwareBufferInfoANDROID
         where
        type StructFields VkImportAndroidHardwareBufferInfoANDROID =
             '["sType", "pNext", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkImportAndroidHardwareBufferInfoANDROID = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportAndroidHardwareBufferInfoANDROID = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportAndroidHardwareBufferInfoANDROID =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportAndroidHardwareBufferInfoANDROID where
        type FieldType "sType" VkImportAndroidHardwareBufferInfoANDROID =
             VkStructureType
        type FieldOptional "sType" VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportAndroidHardwareBufferInfoANDROID =
             #{offset VkImportAndroidHardwareBufferInfoANDROID, sType}
        type FieldIsArray "sType" VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportAndroidHardwareBufferInfoANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportAndroidHardwareBufferInfoANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportAndroidHardwareBufferInfoANDROID where
        type FieldType "pNext" VkImportAndroidHardwareBufferInfoANDROID =
             Ptr Void
        type FieldOptional "pNext" VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportAndroidHardwareBufferInfoANDROID =
             #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext}
        type FieldIsArray "pNext" VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportAndroidHardwareBufferInfoANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkImportAndroidHardwareBufferInfoANDROID where
        type FieldType "buffer" VkImportAndroidHardwareBufferInfoANDROID =
             Ptr AHardwareBuffer
        type FieldOptional "buffer"
               VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkImportAndroidHardwareBufferInfoANDROID
             =
             #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer}
        type FieldIsArray "buffer" VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkImportAndroidHardwareBufferInfoANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkImportAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer}

instance Show VkImportAndroidHardwareBufferInfoANDROID where
        showsPrec d x
          = showString "VkImportAndroidHardwareBufferInfoANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) . showChar '}'

-- | > typedef struct VkMemoryGetAndroidHardwareBufferInfoANDROID {
--   >     VkStructureType sType;
--   >     const void*                        pNext;
--   >     VkDeviceMemory                     memory;
--   > } VkMemoryGetAndroidHardwareBufferInfoANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryGetAndroidHardwareBufferInfoANDROID VkMemoryGetAndroidHardwareBufferInfoANDROID registry at www.khronos.org>
data VkMemoryGetAndroidHardwareBufferInfoANDROID = VkMemoryGetAndroidHardwareBufferInfoANDROID## Addr##
                                                                                                ByteArray##

instance Eq VkMemoryGetAndroidHardwareBufferInfoANDROID where
        (VkMemoryGetAndroidHardwareBufferInfoANDROID## a _) ==
          x@(VkMemoryGetAndroidHardwareBufferInfoANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryGetAndroidHardwareBufferInfoANDROID where
        (VkMemoryGetAndroidHardwareBufferInfoANDROID## a _) `compare`
          x@(VkMemoryGetAndroidHardwareBufferInfoANDROID## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryGetAndroidHardwareBufferInfoANDROID where
        sizeOf ~_
          = #{size VkMemoryGetAndroidHardwareBufferInfoANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryGetAndroidHardwareBufferInfoANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        unsafeAddr (VkMemoryGetAndroidHardwareBufferInfoANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryGetAndroidHardwareBufferInfoANDROID## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryGetAndroidHardwareBufferInfoANDROID##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        type StructFields VkMemoryGetAndroidHardwareBufferInfoANDROID =
             '["sType", "pNext", "memory"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryGetAndroidHardwareBufferInfoANDROID =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryGetAndroidHardwareBufferInfoANDROID =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryGetAndroidHardwareBufferInfoANDROID =
             '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryGetAndroidHardwareBufferInfoANDROID where
        type FieldType "sType" VkMemoryGetAndroidHardwareBufferInfoANDROID
             = VkStructureType
        type FieldOptional "sType"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             =
             #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType}
        type FieldIsArray "sType"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryGetAndroidHardwareBufferInfoANDROID where
        type FieldType "pNext" VkMemoryGetAndroidHardwareBufferInfoANDROID
             = Ptr Void
        type FieldOptional "pNext"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             =
             #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext}
        type FieldIsArray "pNext"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "memory" VkMemoryGetAndroidHardwareBufferInfoANDROID where
        type FieldType "memory" VkMemoryGetAndroidHardwareBufferInfoANDROID
             = VkDeviceMemory
        type FieldOptional "memory"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             =
             #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory}
        type FieldIsArray "memory"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory}

instance Show VkMemoryGetAndroidHardwareBufferInfoANDROID where
        showsPrec d x
          = showString "VkMemoryGetAndroidHardwareBufferInfoANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memory = " .
                            showsPrec d (getField @"memory" x) . showChar '}'
