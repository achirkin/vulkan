#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionCreateInfo
       (VkSamplerYcbcrConversionCreateInfo(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkChromaLocation              (VkChromaLocation)
import           Graphics.Vulkan.Types.Enum.VkFilter                      (VkFilter)
import           Graphics.Vulkan.Types.Enum.VkFormat                      (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversion (VkSamplerYcbcrModelConversion)
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRange           (VkSamplerYcbcrRange)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkComponentMapping          (VkComponentMapping)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkSamplerYcbcrConversionCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkFormat                         format;
--   >     VkSamplerYcbcrModelConversion ycbcrModel;
--   >     VkSamplerYcbcrRange           ycbcrRange;
--   >     VkComponentMapping               components;
--   >     VkChromaLocation              xChromaOffset;
--   >     VkChromaLocation              yChromaOffset;
--   >     VkFilter                         chromaFilter;
--   >     VkBool32                         forceExplicitReconstruction;
--   > } VkSamplerYcbcrConversionCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSamplerYcbcrConversionCreateInfo VkSamplerYcbcrConversionCreateInfo registry at www.khronos.org>
data VkSamplerYcbcrConversionCreateInfo = VkSamplerYcbcrConversionCreateInfo## Addr##
                                                                              ByteArray##

instance Eq VkSamplerYcbcrConversionCreateInfo where
        (VkSamplerYcbcrConversionCreateInfo## a _) ==
          x@(VkSamplerYcbcrConversionCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionCreateInfo where
        (VkSamplerYcbcrConversionCreateInfo## a _) `compare`
          x@(VkSamplerYcbcrConversionCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionCreateInfo where
        sizeOf ~_ = #{size VkSamplerYcbcrConversionCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSamplerYcbcrConversionCreateInfo where
        unsafeAddr (VkSamplerYcbcrConversionCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSamplerYcbcrConversionCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerYcbcrConversionCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSamplerYcbcrConversionCreateInfo where
        type StructFields VkSamplerYcbcrConversionCreateInfo =
             '["sType", "pNext", "format", "ycbcrModel", "ycbcrRange", -- ' closing tick for hsc2hs
               "components", "xChromaOffset", "yChromaOffset", "chromaFilter",
               "forceExplicitReconstruction"]
        type CUnionType VkSamplerYcbcrConversionCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerYcbcrConversionCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSamplerYcbcrConversionCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "sType" VkSamplerYcbcrConversionCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, sType}
        type FieldIsArray "sType" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "pNext" VkSamplerYcbcrConversionCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, pNext}
        type FieldIsArray "pNext" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "format" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "format" VkSamplerYcbcrConversionCreateInfo =
             VkFormat
        type FieldOptional "format" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, format}
        type FieldIsArray "format" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, format}

instance {-# OVERLAPPING #-}
         HasField "ycbcrModel" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "ycbcrModel" VkSamplerYcbcrConversionCreateInfo =
             VkSamplerYcbcrModelConversion
        type FieldOptional "ycbcrModel" VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ycbcrModel" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel}
        type FieldIsArray "ycbcrModel" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel}

instance {-# OVERLAPPING #-}
         CanReadField "ycbcrModel" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel}

instance {-# OVERLAPPING #-}
         CanWriteField "ycbcrModel" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrModel}

instance {-# OVERLAPPING #-}
         HasField "ycbcrRange" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "ycbcrRange" VkSamplerYcbcrConversionCreateInfo =
             VkSamplerYcbcrRange
        type FieldOptional "ycbcrRange" VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ycbcrRange" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange}
        type FieldIsArray "ycbcrRange" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange}

instance {-# OVERLAPPING #-}
         CanReadField "ycbcrRange" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange}

instance {-# OVERLAPPING #-}
         CanWriteField "ycbcrRange" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, ycbcrRange}

instance {-# OVERLAPPING #-}
         HasField "components" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "components" VkSamplerYcbcrConversionCreateInfo =
             VkComponentMapping
        type FieldOptional "components" VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "components" VkSamplerYcbcrConversionCreateInfo =
             #{offset VkSamplerYcbcrConversionCreateInfo, components}
        type FieldIsArray "components" VkSamplerYcbcrConversionCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, components}

instance {-# OVERLAPPING #-}
         CanReadField "components" VkSamplerYcbcrConversionCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, components})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, components}

instance {-# OVERLAPPING #-}
         CanWriteField "components" VkSamplerYcbcrConversionCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, components}

instance {-# OVERLAPPING #-}
         HasField "xChromaOffset" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "xChromaOffset" VkSamplerYcbcrConversionCreateInfo =
             VkChromaLocation
        type FieldOptional "xChromaOffset"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "xChromaOffset" VkSamplerYcbcrConversionCreateInfo
             =
             #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset}
        type FieldIsArray "xChromaOffset"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset}

instance {-# OVERLAPPING #-}
         CanReadField "xChromaOffset" VkSamplerYcbcrConversionCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "xChromaOffset" VkSamplerYcbcrConversionCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, xChromaOffset}

instance {-# OVERLAPPING #-}
         HasField "yChromaOffset" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "yChromaOffset" VkSamplerYcbcrConversionCreateInfo =
             VkChromaLocation
        type FieldOptional "yChromaOffset"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "yChromaOffset" VkSamplerYcbcrConversionCreateInfo
             =
             #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset}
        type FieldIsArray "yChromaOffset"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset}

instance {-# OVERLAPPING #-}
         CanReadField "yChromaOffset" VkSamplerYcbcrConversionCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "yChromaOffset" VkSamplerYcbcrConversionCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, yChromaOffset}

instance {-# OVERLAPPING #-}
         HasField "chromaFilter" VkSamplerYcbcrConversionCreateInfo where
        type FieldType "chromaFilter" VkSamplerYcbcrConversionCreateInfo =
             VkFilter
        type FieldOptional "chromaFilter"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "chromaFilter" VkSamplerYcbcrConversionCreateInfo
             =
             #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter}
        type FieldIsArray "chromaFilter" VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter}

instance {-# OVERLAPPING #-}
         CanReadField "chromaFilter" VkSamplerYcbcrConversionCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter}

instance {-# OVERLAPPING #-}
         CanWriteField "chromaFilter" VkSamplerYcbcrConversionCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, chromaFilter}

instance {-# OVERLAPPING #-}
         HasField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfo
         where
        type FieldType "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfo
             = VkBool32
        type FieldOptional "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfo
             =
             #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction}
        type FieldIsArray "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction}

instance {-# OVERLAPPING #-}
         CanReadField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction}

instance {-# OVERLAPPING #-}
         CanWriteField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfo, forceExplicitReconstruction}

instance Show VkSamplerYcbcrConversionCreateInfo where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "format = " .
                            showsPrec d (getField @"format" x) .
                              showString ", " .
                                showString "ycbcrModel = " .
                                  showsPrec d (getField @"ycbcrModel" x) .
                                    showString ", " .
                                      showString "ycbcrRange = " .
                                        showsPrec d (getField @"ycbcrRange" x) .
                                          showString ", " .
                                            showString "components = " .
                                              showsPrec d (getField @"components" x) .
                                                showString ", " .
                                                  showString "xChromaOffset = " .
                                                    showsPrec d (getField @"xChromaOffset" x) .
                                                      showString ", " .
                                                        showString "yChromaOffset = " .
                                                          showsPrec d (getField @"yChromaOffset" x)
                                                            .
                                                            showString ", " .
                                                              showString "chromaFilter = " .
                                                                showsPrec d
                                                                  (getField @"chromaFilter" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "forceExplicitReconstruction = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"forceExplicitReconstruction"
                                                                           x)
                                                                        . showChar '}'
