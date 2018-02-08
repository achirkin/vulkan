#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_EXT_conservative_rasterization
       (-- * Vulkan extension: @VK_EXT_conservative_rasterization@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @102@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceConservativeRasterizationPropertiesEXT(..),
        VkPipelineRasterizationConservativeStateCreateInfoEXT(..),
        VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION,
        pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION,
        VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME,
        pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkBool32, VkConservativeRasterizationModeEXT,
                                                   VkPipelineRasterizationConservativeStateCreateFlagsEXT,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceConservativeRasterizationPropertiesEXT.html VkPhysicalDeviceConservativeRasterizationPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceConservativeRasterizationPropertiesEXT = VkPhysicalDeviceConservativeRasterizationPropertiesEXT## ByteArray##

instance Eq VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## a) ==
          (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## a)
          `compare`
          (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## b)
          = cmpImmutableContent a b

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
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined ::
                         VkPhysicalDeviceConservativeRasterizationPropertiesEXT),
            I## a <- alignment
                      (undefined ::
                         VkPhysicalDeviceConservativeRasterizationPropertiesEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceConservativeRasterizationPropertiesEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## ba)
          | I## n <- sizeOf
                      (undefined ::
                         VkPhysicalDeviceConservativeRasterizationPropertiesEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined ::
                         VkPhysicalDeviceConservativeRasterizationPropertiesEXT),
            I## a <- alignment
                      (undefined ::
                         VkPhysicalDeviceConservativeRasterizationPropertiesEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceConservativeRasterizationPropertiesEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr
          (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr##
              VkPhysicalDeviceConservativeRasterizationPropertiesEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr
          (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPhysicalDeviceConservativeRasterizationPropertiesEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData
          x@(VkPhysicalDeviceConservativeRasterizationPropertiesEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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

-- | > typedef struct VkPipelineRasterizationConservativeStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineRasterizationConservativeStateCreateFlagsEXT           flags;
--   >     VkConservativeRasterizationModeEXT                                               conservativeRasterizationMode;
--   >     float                                                                            extraPrimitiveOverestimationSize;
--   > } VkPipelineRasterizationConservativeStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineRasterizationConservativeStateCreateInfoEXT.html VkPipelineRasterizationConservativeStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineRasterizationConservativeStateCreateInfoEXT = VkPipelineRasterizationConservativeStateCreateInfoEXT## ByteArray##

instance Eq VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        (VkPipelineRasterizationConservativeStateCreateInfoEXT## a) ==
          (VkPipelineRasterizationConservativeStateCreateInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        (VkPipelineRasterizationConservativeStateCreateInfoEXT## a)
          `compare`
          (VkPipelineRasterizationConservativeStateCreateInfoEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineRasterizationConservativeStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineRasterizationConservativeStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined ::
                         VkPipelineRasterizationConservativeStateCreateInfoEXT),
            I## a <- alignment
                      (undefined ::
                         VkPipelineRasterizationConservativeStateCreateInfoEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPipelineRasterizationConservativeStateCreateInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkPipelineRasterizationConservativeStateCreateInfoEXT## ba)
          | I## n <- sizeOf
                      (undefined ::
                         VkPipelineRasterizationConservativeStateCreateInfoEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined ::
                         VkPipelineRasterizationConservativeStateCreateInfoEXT),
            I## a <- alignment
                      (undefined ::
                         VkPipelineRasterizationConservativeStateCreateInfoEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPipelineRasterizationConservativeStateCreateInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr
          (VkPipelineRasterizationConservativeStateCreateInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr##
              VkPipelineRasterizationConservativeStateCreateInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr
          (VkPipelineRasterizationConservativeStateCreateInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPipelineRasterizationConservativeStateCreateInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData
          x@(VkPipelineRasterizationConservativeStateCreateInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type VkSTypeMType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "sType"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type VkPNextMType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "pNext"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type VkFlagsMType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkPipelineRasterizationConservativeStateCreateFlagsEXT

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "flags"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkPipelineRasterizationConservativeStateCreateFlagsEXT
        type FieldOptional "flags"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs

instance CanReadField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkConservativeRasterizationMode
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type VkConservativeRasterizationModeMType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkConservativeRasterizationModeEXT

        {-# NOINLINE vkConservativeRasterizationMode #-}
        vkConservativeRasterizationMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode})

        {-# INLINE vkConservativeRasterizationModeByteOffset #-}
        vkConservativeRasterizationModeByteOffset ~_
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

        {-# INLINE readVkConservativeRasterizationMode #-}
        readVkConservativeRasterizationMode p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

        {-# INLINE writeVkConservativeRasterizationMode #-}
        writeVkConservativeRasterizationMode p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

instance {-# OVERLAPPING #-}
         HasField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "conservativeRasterizationMode"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkConservativeRasterizationModeEXT
        type FieldOptional "conservativeRasterizationMode"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkConservativeRasterizationMode

        {-# INLINE readField #-}
        readField = readVkConservativeRasterizationMode

instance CanWriteField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkConservativeRasterizationMode

instance {-# OVERLAPPING #-}
         HasVkExtraPrimitiveOverestimationSize
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type VkExtraPrimitiveOverestimationSizeMType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = #{type float}

        {-# NOINLINE vkExtraPrimitiveOverestimationSize #-}
        vkExtraPrimitiveOverestimationSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize})

        {-# INLINE vkExtraPrimitiveOverestimationSizeByteOffset #-}
        vkExtraPrimitiveOverestimationSizeByteOffset ~_
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

        {-# INLINE readVkExtraPrimitiveOverestimationSize #-}
        readVkExtraPrimitiveOverestimationSize p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

        {-# INLINE writeVkExtraPrimitiveOverestimationSize #-}
        writeVkExtraPrimitiveOverestimationSize p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

instance {-# OVERLAPPING #-}
         HasField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "extraPrimitiveOverestimationSize"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = #{type float}
        type FieldOptional "extraPrimitiveOverestimationSize"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkExtraPrimitiveOverestimationSize

        {-# INLINE readField #-}
        readField = readVkExtraPrimitiveOverestimationSize

instance CanWriteField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkExtraPrimitiveOverestimationSize

instance Show VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        showsPrec d x
          = showString
              "VkPipelineRasterizationConservativeStateCreateInfoEXT {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkConservativeRasterizationMode = " .
                                  showsPrec d (vkConservativeRasterizationMode x) .
                                    showString ", " .
                                      showString "vkExtraPrimitiveOverestimationSize = " .
                                        showsPrec d (vkExtraPrimitiveOverestimationSize x) .
                                          showChar '}'

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

type VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: CString

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME <-
        (is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME -> True)
  where VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
          = _VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME

_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME #-}
_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  = Ptr "VK_EXT_conservative_rasterization\NUL"##

is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME ::
                                                    CString -> Bool

{-# INLINE is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME #-}
is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  = (_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME ==)

type VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME =
     "VK_EXT_conservative_rasterization"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
        = VkStructureType 1000101000

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
        = VkStructureType 1000101001
