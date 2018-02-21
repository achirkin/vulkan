#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceConservativeRasterizationPropertiesEXT
       (VkPhysicalDeviceConservativeRasterizationPropertiesEXT(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                             (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceConservativeRasterizationPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     float                  primitiveOverestimationSize;
--   >     float                  maxExtraPrimitiveOverestimationSize;
--   >     float                  extraPrimitiveOverestimationSizeGranularity;
--   >     VkBool32               primitiveUnderestimation;
--   >     VkBool32               conservativePointAndLineRasterization;
--   >     VkBool32               degenerateTrianglesRasterized;
--   >     VkBool32               degenerateLinesRasterized;
--   >     VkBool32               fullyCoveredFragmentShaderInputVariable;
--   >     VkBool32               conservativeRasterizationPostDepthCoverage;
--   > } VkPhysicalDeviceConservativeRasterizationPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceConservativeRasterizationPropertiesEXT.html VkPhysicalDeviceConservativeRasterizationPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceConservativeRasterizationPropertiesEXT = VkPhysicalDeviceConservativeRasterizationPropertiesEXT## Addr##
                                                                                                                      ByteArray##

instance Eq VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## a _) ==
          x@(VkPhysicalDeviceConservativeRasterizationPropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## a _)
          `compare`
          x@(VkPhysicalDeviceConservativeRasterizationPropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceConservativeRasterizationPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceConservativeRasterizationPropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        unsafeAddr
          (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceConservativeRasterizationPropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type StructFields
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             '["sType", "pNext", "primitiveOverestimationSize", -- ' closing tick for hsc2hs
               "maxExtraPrimitiveOverestimationSize",
               "extraPrimitiveOverestimationSizeGranularity",
               "primitiveUnderestimation",
               "conservativePointAndLineRasterization",
               "degenerateTrianglesRasterized", "degenerateLinesRasterized",
               "fullyCoveredFragmentShaderInputVariable",
               "conservativeRasterizationPostDepthCoverage"]
        type CUnionType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkSTypeMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, sType}

instance CanReadField "sType"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkPNextMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkPrimitiveOverestimationSize
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkPrimitiveOverestimationSizeMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = #{type float}

        {-# NOINLINE vkPrimitiveOverestimationSize #-}
        vkPrimitiveOverestimationSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveOverestimationSize})

        {-# INLINE vkPrimitiveOverestimationSizeByteOffset #-}
        vkPrimitiveOverestimationSizeByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveOverestimationSize}

        {-# INLINE readVkPrimitiveOverestimationSize #-}
        readVkPrimitiveOverestimationSize p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveOverestimationSize}

        {-# INLINE writeVkPrimitiveOverestimationSize #-}
        writeVkPrimitiveOverestimationSize p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveOverestimationSize}

instance {-# OVERLAPPING #-}
         HasField "primitiveOverestimationSize"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "primitiveOverestimationSize"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = #{type float}
        type FieldOptional "primitiveOverestimationSize"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "primitiveOverestimationSize"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveOverestimationSize}
        type FieldIsArray "primitiveOverestimationSize"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveOverestimationSize}

instance CanReadField "primitiveOverestimationSize"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPrimitiveOverestimationSize

        {-# INLINE readField #-}
        readField = readVkPrimitiveOverestimationSize

instance CanWriteField "primitiveOverestimationSize"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPrimitiveOverestimationSize

instance {-# OVERLAPPING #-}
         HasVkMaxExtraPrimitiveOverestimationSize
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkMaxExtraPrimitiveOverestimationSizeMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = #{type float}

        {-# NOINLINE vkMaxExtraPrimitiveOverestimationSize #-}
        vkMaxExtraPrimitiveOverestimationSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, maxExtraPrimitiveOverestimationSize})

        {-# INLINE vkMaxExtraPrimitiveOverestimationSizeByteOffset #-}
        vkMaxExtraPrimitiveOverestimationSizeByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, maxExtraPrimitiveOverestimationSize}

        {-# INLINE readVkMaxExtraPrimitiveOverestimationSize #-}
        readVkMaxExtraPrimitiveOverestimationSize p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, maxExtraPrimitiveOverestimationSize}

        {-# INLINE writeVkMaxExtraPrimitiveOverestimationSize #-}
        writeVkMaxExtraPrimitiveOverestimationSize p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, maxExtraPrimitiveOverestimationSize}

instance {-# OVERLAPPING #-}
         HasField "maxExtraPrimitiveOverestimationSize"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "maxExtraPrimitiveOverestimationSize"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = #{type float}
        type FieldOptional "maxExtraPrimitiveOverestimationSize"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxExtraPrimitiveOverestimationSize"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, maxExtraPrimitiveOverestimationSize}
        type FieldIsArray "maxExtraPrimitiveOverestimationSize"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, maxExtraPrimitiveOverestimationSize}

instance CanReadField "maxExtraPrimitiveOverestimationSize"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkMaxExtraPrimitiveOverestimationSize

        {-# INLINE readField #-}
        readField = readVkMaxExtraPrimitiveOverestimationSize

instance CanWriteField "maxExtraPrimitiveOverestimationSize"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxExtraPrimitiveOverestimationSize

instance {-# OVERLAPPING #-}
         HasVkExtraPrimitiveOverestimationSizeGranularity
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkExtraPrimitiveOverestimationSizeGranularityMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = #{type float}

        {-# NOINLINE vkExtraPrimitiveOverestimationSizeGranularity #-}
        vkExtraPrimitiveOverestimationSizeGranularity x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, extraPrimitiveOverestimationSizeGranularity})

        {-# INLINE vkExtraPrimitiveOverestimationSizeGranularityByteOffset
                   #-}
        vkExtraPrimitiveOverestimationSizeGranularityByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, extraPrimitiveOverestimationSizeGranularity}

        {-# INLINE readVkExtraPrimitiveOverestimationSizeGranularity #-}
        readVkExtraPrimitiveOverestimationSizeGranularity p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, extraPrimitiveOverestimationSizeGranularity}

        {-# INLINE writeVkExtraPrimitiveOverestimationSizeGranularity #-}
        writeVkExtraPrimitiveOverestimationSizeGranularity p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, extraPrimitiveOverestimationSizeGranularity}

instance {-# OVERLAPPING #-}
         HasField "extraPrimitiveOverestimationSizeGranularity"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "extraPrimitiveOverestimationSizeGranularity"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = #{type float}
        type FieldOptional "extraPrimitiveOverestimationSizeGranularity"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extraPrimitiveOverestimationSizeGranularity"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, extraPrimitiveOverestimationSizeGranularity}
        type FieldIsArray "extraPrimitiveOverestimationSizeGranularity"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, extraPrimitiveOverestimationSizeGranularity}

instance CanReadField "extraPrimitiveOverestimationSizeGranularity"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkExtraPrimitiveOverestimationSizeGranularity

        {-# INLINE readField #-}
        readField = readVkExtraPrimitiveOverestimationSizeGranularity

instance CanWriteField
           "extraPrimitiveOverestimationSizeGranularity"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkExtraPrimitiveOverestimationSizeGranularity

instance {-# OVERLAPPING #-}
         HasVkPrimitiveUnderestimation
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkPrimitiveUnderestimationMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32

        {-# NOINLINE vkPrimitiveUnderestimation #-}
        vkPrimitiveUnderestimation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveUnderestimation})

        {-# INLINE vkPrimitiveUnderestimationByteOffset #-}
        vkPrimitiveUnderestimationByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveUnderestimation}

        {-# INLINE readVkPrimitiveUnderestimation #-}
        readVkPrimitiveUnderestimation p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveUnderestimation}

        {-# INLINE writeVkPrimitiveUnderestimation #-}
        writeVkPrimitiveUnderestimation p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveUnderestimation}

instance {-# OVERLAPPING #-}
         HasField "primitiveUnderestimation"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "primitiveUnderestimation"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32
        type FieldOptional "primitiveUnderestimation"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "primitiveUnderestimation"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveUnderestimation}
        type FieldIsArray "primitiveUnderestimation"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveUnderestimation}

instance CanReadField "primitiveUnderestimation"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPrimitiveUnderestimation

        {-# INLINE readField #-}
        readField = readVkPrimitiveUnderestimation

instance CanWriteField "primitiveUnderestimation"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPrimitiveUnderestimation

instance {-# OVERLAPPING #-}
         HasVkConservativePointAndLineRasterization
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkConservativePointAndLineRasterizationMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32

        {-# NOINLINE vkConservativePointAndLineRasterization #-}
        vkConservativePointAndLineRasterization x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativePointAndLineRasterization})

        {-# INLINE vkConservativePointAndLineRasterizationByteOffset #-}
        vkConservativePointAndLineRasterizationByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativePointAndLineRasterization}

        {-# INLINE readVkConservativePointAndLineRasterization #-}
        readVkConservativePointAndLineRasterization p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativePointAndLineRasterization}

        {-# INLINE writeVkConservativePointAndLineRasterization #-}
        writeVkConservativePointAndLineRasterization p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativePointAndLineRasterization}

instance {-# OVERLAPPING #-}
         HasField "conservativePointAndLineRasterization"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "conservativePointAndLineRasterization"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32
        type FieldOptional "conservativePointAndLineRasterization"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "conservativePointAndLineRasterization"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativePointAndLineRasterization}
        type FieldIsArray "conservativePointAndLineRasterization"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativePointAndLineRasterization}

instance CanReadField "conservativePointAndLineRasterization"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkConservativePointAndLineRasterization

        {-# INLINE readField #-}
        readField = readVkConservativePointAndLineRasterization

instance CanWriteField "conservativePointAndLineRasterization"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkConservativePointAndLineRasterization

instance {-# OVERLAPPING #-}
         HasVkDegenerateTrianglesRasterized
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkDegenerateTrianglesRasterizedMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32

        {-# NOINLINE vkDegenerateTrianglesRasterized #-}
        vkDegenerateTrianglesRasterized x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateTrianglesRasterized})

        {-# INLINE vkDegenerateTrianglesRasterizedByteOffset #-}
        vkDegenerateTrianglesRasterizedByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateTrianglesRasterized}

        {-# INLINE readVkDegenerateTrianglesRasterized #-}
        readVkDegenerateTrianglesRasterized p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateTrianglesRasterized}

        {-# INLINE writeVkDegenerateTrianglesRasterized #-}
        writeVkDegenerateTrianglesRasterized p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateTrianglesRasterized}

instance {-# OVERLAPPING #-}
         HasField "degenerateTrianglesRasterized"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "degenerateTrianglesRasterized"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32
        type FieldOptional "degenerateTrianglesRasterized"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "degenerateTrianglesRasterized"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateTrianglesRasterized}
        type FieldIsArray "degenerateTrianglesRasterized"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateTrianglesRasterized}

instance CanReadField "degenerateTrianglesRasterized"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkDegenerateTrianglesRasterized

        {-# INLINE readField #-}
        readField = readVkDegenerateTrianglesRasterized

instance CanWriteField "degenerateTrianglesRasterized"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkDegenerateTrianglesRasterized

instance {-# OVERLAPPING #-}
         HasVkDegenerateLinesRasterized
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkDegenerateLinesRasterizedMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32

        {-# NOINLINE vkDegenerateLinesRasterized #-}
        vkDegenerateLinesRasterized x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateLinesRasterized})

        {-# INLINE vkDegenerateLinesRasterizedByteOffset #-}
        vkDegenerateLinesRasterizedByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateLinesRasterized}

        {-# INLINE readVkDegenerateLinesRasterized #-}
        readVkDegenerateLinesRasterized p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateLinesRasterized}

        {-# INLINE writeVkDegenerateLinesRasterized #-}
        writeVkDegenerateLinesRasterized p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateLinesRasterized}

instance {-# OVERLAPPING #-}
         HasField "degenerateLinesRasterized"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "degenerateLinesRasterized"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32
        type FieldOptional "degenerateLinesRasterized"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "degenerateLinesRasterized"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateLinesRasterized}
        type FieldIsArray "degenerateLinesRasterized"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateLinesRasterized}

instance CanReadField "degenerateLinesRasterized"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkDegenerateLinesRasterized

        {-# INLINE readField #-}
        readField = readVkDegenerateLinesRasterized

instance CanWriteField "degenerateLinesRasterized"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkDegenerateLinesRasterized

instance {-# OVERLAPPING #-}
         HasVkFullyCoveredFragmentShaderInputVariable
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkFullyCoveredFragmentShaderInputVariableMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32

        {-# NOINLINE vkFullyCoveredFragmentShaderInputVariable #-}
        vkFullyCoveredFragmentShaderInputVariable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, fullyCoveredFragmentShaderInputVariable})

        {-# INLINE vkFullyCoveredFragmentShaderInputVariableByteOffset #-}
        vkFullyCoveredFragmentShaderInputVariableByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, fullyCoveredFragmentShaderInputVariable}

        {-# INLINE readVkFullyCoveredFragmentShaderInputVariable #-}
        readVkFullyCoveredFragmentShaderInputVariable p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, fullyCoveredFragmentShaderInputVariable}

        {-# INLINE writeVkFullyCoveredFragmentShaderInputVariable #-}
        writeVkFullyCoveredFragmentShaderInputVariable p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, fullyCoveredFragmentShaderInputVariable}

instance {-# OVERLAPPING #-}
         HasField "fullyCoveredFragmentShaderInputVariable"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "fullyCoveredFragmentShaderInputVariable"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32
        type FieldOptional "fullyCoveredFragmentShaderInputVariable"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fullyCoveredFragmentShaderInputVariable"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, fullyCoveredFragmentShaderInputVariable}
        type FieldIsArray "fullyCoveredFragmentShaderInputVariable"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, fullyCoveredFragmentShaderInputVariable}

instance CanReadField "fullyCoveredFragmentShaderInputVariable"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkFullyCoveredFragmentShaderInputVariable

        {-# INLINE readField #-}
        readField = readVkFullyCoveredFragmentShaderInputVariable

instance CanWriteField "fullyCoveredFragmentShaderInputVariable"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkFullyCoveredFragmentShaderInputVariable

instance {-# OVERLAPPING #-}
         HasVkConservativeRasterizationPostDepthCoverage
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type VkConservativeRasterizationPostDepthCoverageMType
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32

        {-# NOINLINE vkConservativeRasterizationPostDepthCoverage #-}
        vkConservativeRasterizationPostDepthCoverage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativeRasterizationPostDepthCoverage})

        {-# INLINE vkConservativeRasterizationPostDepthCoverageByteOffset
                   #-}
        vkConservativeRasterizationPostDepthCoverageByteOffset ~_
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativeRasterizationPostDepthCoverage}

        {-# INLINE readVkConservativeRasterizationPostDepthCoverage #-}
        readVkConservativeRasterizationPostDepthCoverage p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativeRasterizationPostDepthCoverage}

        {-# INLINE writeVkConservativeRasterizationPostDepthCoverage #-}
        writeVkConservativeRasterizationPostDepthCoverage p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativeRasterizationPostDepthCoverage}

instance {-# OVERLAPPING #-}
         HasField "conservativeRasterizationPostDepthCoverage"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        type FieldType "conservativeRasterizationPostDepthCoverage"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = VkBool32
        type FieldOptional "conservativeRasterizationPostDepthCoverage"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "conservativeRasterizationPostDepthCoverage"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             =
             #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativeRasterizationPostDepthCoverage}
        type FieldIsArray "conservativeRasterizationPostDepthCoverage"
               VkPhysicalDeviceConservativeRasterizationPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativeRasterizationPostDepthCoverage}

instance CanReadField "conservativeRasterizationPostDepthCoverage"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkConservativeRasterizationPostDepthCoverage

        {-# INLINE readField #-}
        readField = readVkConservativeRasterizationPostDepthCoverage

instance CanWriteField "conservativeRasterizationPostDepthCoverage"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkConservativeRasterizationPostDepthCoverage

instance Show
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        showsPrec d x
          = showString
              "VkPhysicalDeviceConservativeRasterizationPropertiesEXT {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPrimitiveOverestimationSize = " .
                            showsPrec d (vkPrimitiveOverestimationSize x) .
                              showString ", " .
                                showString "vkMaxExtraPrimitiveOverestimationSize = " .
                                  showsPrec d (vkMaxExtraPrimitiveOverestimationSize x) .
                                    showString ", " .
                                      showString "vkExtraPrimitiveOverestimationSizeGranularity = "
                                        .
                                        showsPrec d
                                          (vkExtraPrimitiveOverestimationSizeGranularity x)
                                          .
                                          showString ", " .
                                            showString "vkPrimitiveUnderestimation = " .
                                              showsPrec d (vkPrimitiveUnderestimation x) .
                                                showString ", " .
                                                  showString
                                                    "vkConservativePointAndLineRasterization = "
                                                    .
                                                    showsPrec d
                                                      (vkConservativePointAndLineRasterization x)
                                                      .
                                                      showString ", " .
                                                        showString
                                                          "vkDegenerateTrianglesRasterized = "
                                                          .
                                                          showsPrec d
                                                            (vkDegenerateTrianglesRasterized x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "vkDegenerateLinesRasterized = "
                                                                .
                                                                showsPrec d
                                                                  (vkDegenerateLinesRasterized x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkFullyCoveredFragmentShaderInputVariable = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkFullyCoveredFragmentShaderInputVariable
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkConservativeRasterizationPostDepthCoverage = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkConservativeRasterizationPostDepthCoverage
                                                                                 x)
                                                                              . showChar '}'
