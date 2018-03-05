#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "primitiveOverestimationSize"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveOverestimationSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveOverestimationSize}

instance {-# OVERLAPPING #-}
         CanWriteField "primitiveOverestimationSize"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveOverestimationSize}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxExtraPrimitiveOverestimationSize"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, maxExtraPrimitiveOverestimationSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, maxExtraPrimitiveOverestimationSize}

instance {-# OVERLAPPING #-}
         CanWriteField "maxExtraPrimitiveOverestimationSize"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, maxExtraPrimitiveOverestimationSize}

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

instance {-# OVERLAPPING #-}
         CanReadField "extraPrimitiveOverestimationSizeGranularity"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, extraPrimitiveOverestimationSizeGranularity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, extraPrimitiveOverestimationSizeGranularity}

instance {-# OVERLAPPING #-}
         CanWriteField "extraPrimitiveOverestimationSizeGranularity"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, extraPrimitiveOverestimationSizeGranularity}

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

instance {-# OVERLAPPING #-}
         CanReadField "primitiveUnderestimation"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveUnderestimation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveUnderestimation}

instance {-# OVERLAPPING #-}
         CanWriteField "primitiveUnderestimation"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveUnderestimation}

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

instance {-# OVERLAPPING #-}
         CanReadField "conservativePointAndLineRasterization"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativePointAndLineRasterization})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativePointAndLineRasterization}

instance {-# OVERLAPPING #-}
         CanWriteField "conservativePointAndLineRasterization"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativePointAndLineRasterization}

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

instance {-# OVERLAPPING #-}
         CanReadField "degenerateTrianglesRasterized"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateTrianglesRasterized})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateTrianglesRasterized}

instance {-# OVERLAPPING #-}
         CanWriteField "degenerateTrianglesRasterized"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateTrianglesRasterized}

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

instance {-# OVERLAPPING #-}
         CanReadField "degenerateLinesRasterized"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateLinesRasterized})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateLinesRasterized}

instance {-# OVERLAPPING #-}
         CanWriteField "degenerateLinesRasterized"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateLinesRasterized}

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

instance {-# OVERLAPPING #-}
         CanReadField "fullyCoveredFragmentShaderInputVariable"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, fullyCoveredFragmentShaderInputVariable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, fullyCoveredFragmentShaderInputVariable}

instance {-# OVERLAPPING #-}
         CanWriteField "fullyCoveredFragmentShaderInputVariable"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, fullyCoveredFragmentShaderInputVariable}

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

instance {-# OVERLAPPING #-}
         CanReadField "conservativeRasterizationPostDepthCoverage"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativeRasterizationPostDepthCoverage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativeRasterizationPostDepthCoverage}

instance {-# OVERLAPPING #-}
         CanWriteField "conservativeRasterizationPostDepthCoverage"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativeRasterizationPostDepthCoverage}

instance Show
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        showsPrec d x
          = showString
              "VkPhysicalDeviceConservativeRasterizationPropertiesEXT {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "primitiveOverestimationSize = " .
                            showsPrec d (getField @"primitiveOverestimationSize" x) .
                              showString ", " .
                                showString "maxExtraPrimitiveOverestimationSize = " .
                                  showsPrec d (getField @"maxExtraPrimitiveOverestimationSize" x) .
                                    showString ", " .
                                      showString "extraPrimitiveOverestimationSizeGranularity = " .
                                        showsPrec d
                                          (getField @"extraPrimitiveOverestimationSizeGranularity"
                                             x)
                                          .
                                          showString ", " .
                                            showString "primitiveUnderestimation = " .
                                              showsPrec d (getField @"primitiveUnderestimation" x) .
                                                showString ", " .
                                                  showString
                                                    "conservativePointAndLineRasterization = "
                                                    .
                                                    showsPrec d
                                                      (getField
                                                         @"conservativePointAndLineRasterization"
                                                         x)
                                                      .
                                                      showString ", " .
                                                        showString
                                                          "degenerateTrianglesRasterized = "
                                                          .
                                                          showsPrec d
                                                            (getField
                                                               @"degenerateTrianglesRasterized"
                                                               x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "degenerateLinesRasterized = "
                                                                .
                                                                showsPrec d
                                                                  (getField
                                                                     @"degenerateLinesRasterized"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "fullyCoveredFragmentShaderInputVariable = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"fullyCoveredFragmentShaderInputVariable"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "conservativeRasterizationPostDepthCoverage = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"conservativeRasterizationPostDepthCoverage"
                                                                                 x)
                                                                              . showChar '}'
