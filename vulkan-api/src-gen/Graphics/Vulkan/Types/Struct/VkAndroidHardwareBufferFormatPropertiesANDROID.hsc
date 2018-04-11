#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAndroidHardwareBufferFormatPropertiesANDROID
       (VkAndroidHardwareBufferFormatPropertiesANDROID(..)) where
import           Foreign.Storable
                                                                                        (Storable (..))
import           GHC.Base
                                                                                        (Addr##,
                                                                                        ByteArray##,
                                                                                        byteArrayContents##,
                                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkChromaLocation
                                                                                        (VkChromaLocation)
import           Graphics.Vulkan.Types.Enum.VkFormat
                                                                                        (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags
                                                                                        (VkFormatFeatureFlags)
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversion
                                                                                        (VkSamplerYcbcrModelConversion)
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRange
                                                                                        (VkSamplerYcbcrRange)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                        (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkAndroidHardwareBufferPropertiesANDROID
                                                                                        (VkAndroidHardwareBufferPropertiesANDROID)
import           Graphics.Vulkan.Types.Struct.VkComponentMapping
                                                                                        (VkComponentMapping)
import           System.IO.Unsafe
                                                                                        (unsafeDupablePerformIO)

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
