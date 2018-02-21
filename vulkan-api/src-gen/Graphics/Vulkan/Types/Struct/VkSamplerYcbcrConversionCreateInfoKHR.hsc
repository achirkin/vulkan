#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkSamplerYcbcrConversionCreateInfoKHR where
        type VkSTypeMType VkSamplerYcbcrConversionCreateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, sType}

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

instance CanReadField "sType" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSamplerYcbcrConversionCreateInfoKHR where
        type VkPNextMType VkSamplerYcbcrConversionCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, pNext}

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

instance CanReadField "pNext" VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFormat VkSamplerYcbcrConversionCreateInfoKHR where
        type VkFormatMType VkSamplerYcbcrConversionCreateInfoKHR = VkFormat

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, format}

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

instance CanReadField "format"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-}
         HasVkYcbcrModel VkSamplerYcbcrConversionCreateInfoKHR where
        type VkYcbcrModelMType VkSamplerYcbcrConversionCreateInfoKHR =
             VkSamplerYcbcrModelConversionKHR

        {-# NOINLINE vkYcbcrModel #-}
        vkYcbcrModel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrModel})

        {-# INLINE vkYcbcrModelByteOffset #-}
        vkYcbcrModelByteOffset ~_
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrModel}

        {-# INLINE readVkYcbcrModel #-}
        readVkYcbcrModel p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrModel}

        {-# INLINE writeVkYcbcrModel #-}
        writeVkYcbcrModel p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrModel}

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

instance CanReadField "ycbcrModel"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkYcbcrModel

        {-# INLINE readField #-}
        readField = readVkYcbcrModel

instance CanWriteField "ycbcrModel"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkYcbcrModel

instance {-# OVERLAPPING #-}
         HasVkYcbcrRange VkSamplerYcbcrConversionCreateInfoKHR where
        type VkYcbcrRangeMType VkSamplerYcbcrConversionCreateInfoKHR =
             VkSamplerYcbcrRangeKHR

        {-# NOINLINE vkYcbcrRange #-}
        vkYcbcrRange x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrRange})

        {-# INLINE vkYcbcrRangeByteOffset #-}
        vkYcbcrRangeByteOffset ~_
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrRange}

        {-# INLINE readVkYcbcrRange #-}
        readVkYcbcrRange p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrRange}

        {-# INLINE writeVkYcbcrRange #-}
        writeVkYcbcrRange p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, ycbcrRange}

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

instance CanReadField "ycbcrRange"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkYcbcrRange

        {-# INLINE readField #-}
        readField = readVkYcbcrRange

instance CanWriteField "ycbcrRange"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkYcbcrRange

instance {-# OVERLAPPING #-}
         HasVkComponents VkSamplerYcbcrConversionCreateInfoKHR where
        type VkComponentsMType VkSamplerYcbcrConversionCreateInfoKHR =
             VkComponentMapping

        {-# NOINLINE vkComponents #-}
        vkComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, components})

        {-# INLINE vkComponentsByteOffset #-}
        vkComponentsByteOffset ~_
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, components}

        {-# INLINE readVkComponents #-}
        readVkComponents p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, components}

        {-# INLINE writeVkComponents #-}
        writeVkComponents p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, components}

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

instance CanReadField "components"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkComponents

        {-# INLINE readField #-}
        readField = readVkComponents

instance CanWriteField "components"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkComponents

instance {-# OVERLAPPING #-}
         HasVkXChromaOffset VkSamplerYcbcrConversionCreateInfoKHR where
        type VkXChromaOffsetMType VkSamplerYcbcrConversionCreateInfoKHR =
             VkChromaLocationKHR

        {-# NOINLINE vkXChromaOffset #-}
        vkXChromaOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, xChromaOffset})

        {-# INLINE vkXChromaOffsetByteOffset #-}
        vkXChromaOffsetByteOffset ~_
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, xChromaOffset}

        {-# INLINE readVkXChromaOffset #-}
        readVkXChromaOffset p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, xChromaOffset}

        {-# INLINE writeVkXChromaOffset #-}
        writeVkXChromaOffset p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, xChromaOffset}

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

instance CanReadField "xChromaOffset"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkXChromaOffset

        {-# INLINE readField #-}
        readField = readVkXChromaOffset

instance CanWriteField "xChromaOffset"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkXChromaOffset

instance {-# OVERLAPPING #-}
         HasVkYChromaOffset VkSamplerYcbcrConversionCreateInfoKHR where
        type VkYChromaOffsetMType VkSamplerYcbcrConversionCreateInfoKHR =
             VkChromaLocationKHR

        {-# NOINLINE vkYChromaOffset #-}
        vkYChromaOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, yChromaOffset})

        {-# INLINE vkYChromaOffsetByteOffset #-}
        vkYChromaOffsetByteOffset ~_
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, yChromaOffset}

        {-# INLINE readVkYChromaOffset #-}
        readVkYChromaOffset p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, yChromaOffset}

        {-# INLINE writeVkYChromaOffset #-}
        writeVkYChromaOffset p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, yChromaOffset}

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

instance CanReadField "yChromaOffset"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkYChromaOffset

        {-# INLINE readField #-}
        readField = readVkYChromaOffset

instance CanWriteField "yChromaOffset"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkYChromaOffset

instance {-# OVERLAPPING #-}
         HasVkChromaFilter VkSamplerYcbcrConversionCreateInfoKHR where
        type VkChromaFilterMType VkSamplerYcbcrConversionCreateInfoKHR =
             VkFilter

        {-# NOINLINE vkChromaFilter #-}
        vkChromaFilter x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, chromaFilter})

        {-# INLINE vkChromaFilterByteOffset #-}
        vkChromaFilterByteOffset ~_
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, chromaFilter}

        {-# INLINE readVkChromaFilter #-}
        readVkChromaFilter p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, chromaFilter}

        {-# INLINE writeVkChromaFilter #-}
        writeVkChromaFilter p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, chromaFilter}

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

instance CanReadField "chromaFilter"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkChromaFilter

        {-# INLINE readField #-}
        readField = readVkChromaFilter

instance CanWriteField "chromaFilter"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkChromaFilter

instance {-# OVERLAPPING #-}
         HasVkForceExplicitReconstruction
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        type VkForceExplicitReconstructionMType
               VkSamplerYcbcrConversionCreateInfoKHR
             = VkBool32

        {-# NOINLINE vkForceExplicitReconstruction #-}
        vkForceExplicitReconstruction x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionCreateInfoKHR, forceExplicitReconstruction})

        {-# INLINE vkForceExplicitReconstructionByteOffset #-}
        vkForceExplicitReconstructionByteOffset ~_
          = #{offset VkSamplerYcbcrConversionCreateInfoKHR, forceExplicitReconstruction}

        {-# INLINE readVkForceExplicitReconstruction #-}
        readVkForceExplicitReconstruction p
          = peekByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, forceExplicitReconstruction}

        {-# INLINE writeVkForceExplicitReconstruction #-}
        writeVkForceExplicitReconstruction p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionCreateInfoKHR, forceExplicitReconstruction}

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

instance CanReadField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkForceExplicitReconstruction

        {-# INLINE readField #-}
        readField = readVkForceExplicitReconstruction

instance CanWriteField "forceExplicitReconstruction"
           VkSamplerYcbcrConversionCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkForceExplicitReconstruction

instance Show VkSamplerYcbcrConversionCreateInfoKHR where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFormat = " .
                            showsPrec d (vkFormat x) .
                              showString ", " .
                                showString "vkYcbcrModel = " .
                                  showsPrec d (vkYcbcrModel x) .
                                    showString ", " .
                                      showString "vkYcbcrRange = " .
                                        showsPrec d (vkYcbcrRange x) .
                                          showString ", " .
                                            showString "vkComponents = " .
                                              showsPrec d (vkComponents x) .
                                                showString ", " .
                                                  showString "vkXChromaOffset = " .
                                                    showsPrec d (vkXChromaOffset x) .
                                                      showString ", " .
                                                        showString "vkYChromaOffset = " .
                                                          showsPrec d (vkYChromaOffset x) .
                                                            showString ", " .
                                                              showString "vkChromaFilter = " .
                                                                showsPrec d (vkChromaFilter x) .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkForceExplicitReconstruction = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkForceExplicitReconstruction
                                                                           x)
                                                                        . showChar '}'
