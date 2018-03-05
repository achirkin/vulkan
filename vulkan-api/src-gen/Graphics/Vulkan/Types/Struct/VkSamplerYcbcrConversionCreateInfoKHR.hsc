#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionCreateInfoKHR
       (VkSamplerYcbcrConversionCreateInfoKHR(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                             (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkChromaLocationKHR              (VkChromaLocationKHR)
import           Graphics.Vulkan.Types.Enum.VkFilter                         (VkFilter)
import           Graphics.Vulkan.Types.Enum.VkFormat                         (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversionKHR (VkSamplerYcbcrModelConversionKHR)
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRangeKHR           (VkSamplerYcbcrRangeKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkComponentMapping             (VkComponentMapping)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkSamplerYcbcrConversionCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkFormat                         format;
--   >     VkSamplerYcbcrModelConversionKHR ycbcrModel;
--   >     VkSamplerYcbcrRangeKHR           ycbcrRange;
--   >     VkComponentMapping               components;
--   >     VkChromaLocationKHR              xChromaOffset;
--   >     VkChromaLocationKHR              yChromaOffset;
--   >     VkFilter                         chromaFilter;
--   >     VkBool32                         forceExplicitReconstruction;
--   > } VkSamplerYcbcrConversionCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSamplerYcbcrConversionCreateInfoKHR.html VkSamplerYcbcrConversionCreateInfoKHR registry at www.khronos.org>
data VkSamplerYcbcrConversionCreateInfoKHR = VkSamplerYcbcrConversionCreateInfoKHR## Addr##
                                                                                    ByteArray##

instance Eq VkSamplerYcbcrConversionCreateInfoKHR where
        (VkSamplerYcbcrConversionCreateInfoKHR## a _) ==
          x@(VkSamplerYcbcrConversionCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionCreateInfoKHR where
        (VkSamplerYcbcrConversionCreateInfoKHR## a _) `compare`
          x@(VkSamplerYcbcrConversionCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionCreateInfoKHR where
        sizeOf ~_
          = #{size VkSamplerYcbcrConversionCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSamplerYcbcrConversionCreateInfoKHR
         where
        unsafeAddr (VkSamplerYcbcrConversionCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSamplerYcbcrConversionCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerYcbcrConversionCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSamplerYcbcrConversionCreateInfoKHR where
        type StructFields VkSamplerYcbcrConversionCreateInfoKHR =
             '["sType", "pNext", "format", "ycbcrModel", "ycbcrRange", -- ' closing tick for hsc2hs
               "components", "xChromaOffset", "yChromaOffset", "chromaFilter",
               "forceExplicitReconstruction"]
        type CUnionType VkSamplerYcbcrConversionCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerYcbcrConversionCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSamplerYcbcrConversionCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerYcbcrConversionCreateInfoKHR where
        type FieldType "sType" VkSamplerYcbcrConversionCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkSamplerYcbcrConversionCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSamplerYcbcrConversionCreateInfoKHR =
             #{offset VkSamplerYcbcrConversionCreateInfoKHR, sType}
        type FieldIsArray "sType" VkSamplerYcbcrConversionCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSamplerYcbcrConversionCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSamplerYcbcrConversionCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerYcbcrConversionCreateInfoKHR where
        type FieldType "pNext" VkSamplerYcbcrConversionCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkSamplerYcbcrConversionCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSamplerYcbcrConversionCreateInfoKHR =
             #{offset VkSamplerYcbcrConversionCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkSamplerYcbcrConversionCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSamplerYcbcrConversionCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSamplerYcbcrConversionCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "format" VkSamplerYcbcrConversionCreateInfoKHR where
        type FieldType "format" VkSamplerYcbcrConversionCreateInfoKHR =
             VkFormat
        type FieldOptional "format" VkSamplerYcbcrConversionCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkSamplerYcbcrConversionCreateInfoKHR =
             #{offset VkSamplerYcbcrConversionCreateInfoKHR, format}
        type FieldIsArray "format" VkSamplerYcbcrConversionCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkSamplerYcbcrConversionCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkSamplerYcbcrConversionCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, format}

instance {-# OVERLAPPING #-}
         HasField "ycbcrModel" VkSamplerYcbcrConversionCreateInfoKHR where
        type FieldType "ycbcrModel" VkSamplerYcbcrConversionCreateInfoKHR =
             VkSamplerYcbcrModelConversionKHR
        type FieldOptional "ycbcrModel"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ycbcrModel" VkSamplerYcbcrConversionCreateInfoKHR
             =
             #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrModel}
        type FieldIsArray "ycbcrModel"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrModel}

instance {-# OVERLAPPING #-}
         CanReadField "ycbcrModel" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrModel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrModel}

instance {-# OVERLAPPING #-}
         CanWriteField "ycbcrModel" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrModel}

instance {-# OVERLAPPING #-}
         HasField "ycbcrRange" VkSamplerYcbcrConversionCreateInfoKHR where
        type FieldType "ycbcrRange" VkSamplerYcbcrConversionCreateInfoKHR =
             VkSamplerYcbcrRangeKHR
        type FieldOptional "ycbcrRange"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ycbcrRange" VkSamplerYcbcrConversionCreateInfoKHR
             =
             #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrRange}
        type FieldIsArray "ycbcrRange"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrRange}

instance {-# OVERLAPPING #-}
         CanReadField "ycbcrRange" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrRange})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrRange}

instance {-# OVERLAPPING #-}
         CanWriteField "ycbcrRange" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrRange}

instance {-# OVERLAPPING #-}
         HasField "components" VkSamplerYcbcrConversionCreateInfoKHR where
        type FieldType "components" VkSamplerYcbcrConversionCreateInfoKHR =
             VkComponentMapping
        type FieldOptional "components"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "components" VkSamplerYcbcrConversionCreateInfoKHR
             =
             #{offset VkSamplerYcbcrConversionCreateInfoKHR, components}
        type FieldIsArray "components"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, components}

instance {-# OVERLAPPING #-}
         CanReadField "components" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, components})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, components}

instance {-# OVERLAPPING #-}
         CanWriteField "components" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, components}

instance {-# OVERLAPPING #-}
         HasField "xChromaOffset" VkSamplerYcbcrConversionCreateInfoKHR
         where
        type FieldType "xChromaOffset"
               VkSamplerYcbcrConversionCreateInfoKHR
             = VkChromaLocationKHR
        type FieldOptional "xChromaOffset"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "xChromaOffset"
               VkSamplerYcbcrConversionCreateInfoKHR
             =
             #{offset VkSamplerYcbcrConversionCreateInfoKHR, xChromaOffset}
        type FieldIsArray "xChromaOffset"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, xChromaOffset}

instance {-# OVERLAPPING #-}
         CanReadField "xChromaOffset" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, xChromaOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, xChromaOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "xChromaOffset" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, xChromaOffset}

instance {-# OVERLAPPING #-}
         HasField "yChromaOffset" VkSamplerYcbcrConversionCreateInfoKHR
         where
        type FieldType "yChromaOffset"
               VkSamplerYcbcrConversionCreateInfoKHR
             = VkChromaLocationKHR
        type FieldOptional "yChromaOffset"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "yChromaOffset"
               VkSamplerYcbcrConversionCreateInfoKHR
             =
             #{offset VkSamplerYcbcrConversionCreateInfoKHR, yChromaOffset}
        type FieldIsArray "yChromaOffset"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, yChromaOffset}

instance {-# OVERLAPPING #-}
         CanReadField "yChromaOffset" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, yChromaOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, yChromaOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "yChromaOffset" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, yChromaOffset}

instance {-# OVERLAPPING #-}
         HasField "chromaFilter" VkSamplerYcbcrConversionCreateInfoKHR where
        type FieldType "chromaFilter" VkSamplerYcbcrConversionCreateInfoKHR
             = VkFilter
        type FieldOptional "chromaFilter"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "chromaFilter"
               VkSamplerYcbcrConversionCreateInfoKHR
             =
             #{offset VkSamplerYcbcrConversionCreateInfoKHR, chromaFilter}
        type FieldIsArray "chromaFilter"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, chromaFilter}

instance {-# OVERLAPPING #-}
         CanReadField "chromaFilter" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, chromaFilter})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, chromaFilter}

instance {-# OVERLAPPING #-}
         CanWriteField "chromaFilter" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, chromaFilter}

instance {-# OVERLAPPING #-}
         HasField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        type FieldType "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfoKHR
             = VkBool32
        type FieldOptional "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfoKHR
             =
             #{offset VkSamplerYcbcrConversionCreateInfoKHR, forceExplicitReconstruction}
        type FieldIsArray "forceExplicitReconstruction"
               VkSamplerYcbcrConversionCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, forceExplicitReconstruction}

instance {-# OVERLAPPING #-}
         CanReadField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, forceExplicitReconstruction})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, forceExplicitReconstruction}

instance {-# OVERLAPPING #-}
         CanWriteField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, forceExplicitReconstruction}

instance Show VkSamplerYcbcrConversionCreateInfoKHR where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionCreateInfoKHR {" .
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
