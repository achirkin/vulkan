#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_sampler_ycbcr_conversion
       (-- * Vulkan extension: @VK_KHR_sampler_ycbcr_conversion@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Andrew Garrard @fluppeteer@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @157@
        --
        -- Required extensions: 'VK_KHR_maintenance1', 'VK_KHR_bind_memory2', 'VK_KHR_get_memory_requirements2', 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_maintenance1', 'VK_KHR_bind_memory2', 'VK_KHR_get_memory_requirements2', 'VK_KHR_get_physical_device_properties2'.
        VkSamplerYcbcrConversionCreateInfoKHR(..),
        VkSamplerYcbcrConversionInfoKHR(..),
        VkBindImagePlaneMemoryInfoKHR(..),
        VkImagePlaneMemoryRequirementsInfoKHR(..),
        VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR(..),
        VkSamplerYcbcrConversionImageFormatPropertiesKHR(..),
        vkCreateSamplerYcbcrConversionKHR,
        vkDestroySamplerYcbcrConversionKHR,
        VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION,
        pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION,
        VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME,
        pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR,
        pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT,
        pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR,
        pattern VK_FORMAT_G8B8G8R8_422_UNORM_KHR,
        pattern VK_FORMAT_B8G8R8G8_422_UNORM_KHR,
        pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR,
        pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR,
        pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR,
        pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR,
        pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR,
        pattern VK_FORMAT_R10X6_UNORM_PACK16_KHR,
        pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR,
        pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_R12X4_UNORM_PACK16_KHR,
        pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR,
        pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G16B16G16R16_422_UNORM_KHR,
        pattern VK_FORMAT_B16G16R16G16_422_UNORM_KHR,
        pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR,
        pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR,
        pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR,
        pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR,
        pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR,
        pattern VK_IMAGE_ASPECT_PLANE_0_BIT_KHR,
        pattern VK_IMAGE_ASPECT_PLANE_1_BIT_KHR,
        pattern VK_IMAGE_ASPECT_PLANE_2_BIT_KHR,
        pattern VK_IMAGE_CREATE_DISJOINT_BIT_KHR,
        pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR,
        pattern VK_FORMAT_FEATURE_DISJOINT_BIT_KHR,
        pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR)
       where
import           Data.Int
import           Data.Void                        (Void)
import           Data.Word
import           Foreign.C.String                 (CString)
import           Foreign.C.Types                  (CChar (..), CFloat (..),
                                                   CInt (..), CSize (..),
                                                   CULong (..))
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Core
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkSamplerYcbcrConversionCreateInfoKHR = VkSamplerYcbcrConversionCreateInfoKHR## ByteArray##

instance Eq VkSamplerYcbcrConversionCreateInfoKHR where
        (VkSamplerYcbcrConversionCreateInfoKHR## a) ==
          (VkSamplerYcbcrConversionCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionCreateInfoKHR where
        (VkSamplerYcbcrConversionCreateInfoKHR## a) `compare`
          (VkSamplerYcbcrConversionCreateInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionCreateInfoKHR where
        sizeOf ~_
          = #{size VkSamplerYcbcrConversionCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkSamplerYcbcrConversionCreateInfoKHR),
            I## a <- alignment
                      (undefined :: VkSamplerYcbcrConversionCreateInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSamplerYcbcrConversionCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSamplerYcbcrConversionCreateInfoKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkSamplerYcbcrConversionCreateInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSamplerYcbcrConversionCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkSamplerYcbcrConversionCreateInfoKHR),
            I## a <- alignment
                      (undefined :: VkSamplerYcbcrConversionCreateInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSamplerYcbcrConversionCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSamplerYcbcrConversionCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkSamplerYcbcrConversionCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSamplerYcbcrConversionCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSamplerYcbcrConversionCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSamplerYcbcrConversionCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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

data VkSamplerYcbcrConversionInfoKHR = VkSamplerYcbcrConversionInfoKHR## ByteArray##

instance Eq VkSamplerYcbcrConversionInfoKHR where
        (VkSamplerYcbcrConversionInfoKHR## a) ==
          (VkSamplerYcbcrConversionInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionInfoKHR where
        (VkSamplerYcbcrConversionInfoKHR## a) `compare`
          (VkSamplerYcbcrConversionInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionInfoKHR where
        sizeOf ~_ = #{size VkSamplerYcbcrConversionInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkSamplerYcbcrConversionInfoKHR),
            I## a <- alignment (undefined :: VkSamplerYcbcrConversionInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSamplerYcbcrConversionInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSamplerYcbcrConversionInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkSamplerYcbcrConversionInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSamplerYcbcrConversionInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkSamplerYcbcrConversionInfoKHR),
            I## a <- alignment (undefined :: VkSamplerYcbcrConversionInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSamplerYcbcrConversionInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSamplerYcbcrConversionInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkSamplerYcbcrConversionInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSamplerYcbcrConversionInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSamplerYcbcrConversionInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSamplerYcbcrConversionInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkSamplerYcbcrConversionInfoKHR where
        type VkSTypeMType VkSamplerYcbcrConversionInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSamplerYcbcrConversionInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkSamplerYcbcrConversionInfoKHR where
        type VkPNextMType VkSamplerYcbcrConversionInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSamplerYcbcrConversionInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkConversion VkSamplerYcbcrConversionInfoKHR where
        type VkConversionMType VkSamplerYcbcrConversionInfoKHR =
             VkSamplerYcbcrConversionKHR

        {-# NOINLINE vkConversion #-}
        vkConversion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfoKHR, conversion})

        {-# INLINE vkConversionByteOffset #-}
        vkConversionByteOffset ~_
          = #{offset VkSamplerYcbcrConversionInfoKHR, conversion}

        {-# INLINE readVkConversion #-}
        readVkConversion p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, conversion}

        {-# INLINE writeVkConversion #-}
        writeVkConversion p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, conversion}

instance Show VkSamplerYcbcrConversionInfoKHR where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkConversion = " .
                            showsPrec d (vkConversion x) . showChar '}'

data VkBindImagePlaneMemoryInfoKHR = VkBindImagePlaneMemoryInfoKHR## ByteArray##

instance Eq VkBindImagePlaneMemoryInfoKHR where
        (VkBindImagePlaneMemoryInfoKHR## a) ==
          (VkBindImagePlaneMemoryInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkBindImagePlaneMemoryInfoKHR where
        (VkBindImagePlaneMemoryInfoKHR## a) `compare`
          (VkBindImagePlaneMemoryInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkBindImagePlaneMemoryInfoKHR where
        sizeOf ~_ = #{size VkBindImagePlaneMemoryInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindImagePlaneMemoryInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkBindImagePlaneMemoryInfoKHR),
            I## a <- alignment (undefined :: VkBindImagePlaneMemoryInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkBindImagePlaneMemoryInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkBindImagePlaneMemoryInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkBindImagePlaneMemoryInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkBindImagePlaneMemoryInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkBindImagePlaneMemoryInfoKHR),
            I## a <- alignment (undefined :: VkBindImagePlaneMemoryInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkBindImagePlaneMemoryInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkBindImagePlaneMemoryInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkBindImagePlaneMemoryInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkBindImagePlaneMemoryInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkBindImagePlaneMemoryInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkBindImagePlaneMemoryInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkBindImagePlaneMemoryInfoKHR where
        type VkSTypeMType VkBindImagePlaneMemoryInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindImagePlaneMemoryInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkBindImagePlaneMemoryInfoKHR where
        type VkPNextMType VkBindImagePlaneMemoryInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindImagePlaneMemoryInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkPlaneAspect VkBindImagePlaneMemoryInfoKHR where
        type VkPlaneAspectMType VkBindImagePlaneMemoryInfoKHR =
             VkImageAspectFlagBits

        {-# NOINLINE vkPlaneAspect #-}
        vkPlaneAspect x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect})

        {-# INLINE vkPlaneAspectByteOffset #-}
        vkPlaneAspectByteOffset ~_
          = #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect}

        {-# INLINE readVkPlaneAspect #-}
        readVkPlaneAspect p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect}

        {-# INLINE writeVkPlaneAspect #-}
        writeVkPlaneAspect p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect}

instance Show VkBindImagePlaneMemoryInfoKHR where
        showsPrec d x
          = showString "VkBindImagePlaneMemoryInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPlaneAspect = " .
                            showsPrec d (vkPlaneAspect x) . showChar '}'

data VkImagePlaneMemoryRequirementsInfoKHR = VkImagePlaneMemoryRequirementsInfoKHR## ByteArray##

instance Eq VkImagePlaneMemoryRequirementsInfoKHR where
        (VkImagePlaneMemoryRequirementsInfoKHR## a) ==
          (VkImagePlaneMemoryRequirementsInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImagePlaneMemoryRequirementsInfoKHR where
        (VkImagePlaneMemoryRequirementsInfoKHR## a) `compare`
          (VkImagePlaneMemoryRequirementsInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImagePlaneMemoryRequirementsInfoKHR where
        sizeOf ~_
          = #{size VkImagePlaneMemoryRequirementsInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImagePlaneMemoryRequirementsInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkImagePlaneMemoryRequirementsInfoKHR),
            I## a <- alignment
                      (undefined :: VkImagePlaneMemoryRequirementsInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImagePlaneMemoryRequirementsInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImagePlaneMemoryRequirementsInfoKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkImagePlaneMemoryRequirementsInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImagePlaneMemoryRequirementsInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkImagePlaneMemoryRequirementsInfoKHR),
            I## a <- alignment
                      (undefined :: VkImagePlaneMemoryRequirementsInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImagePlaneMemoryRequirementsInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImagePlaneMemoryRequirementsInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkImagePlaneMemoryRequirementsInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImagePlaneMemoryRequirementsInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImagePlaneMemoryRequirementsInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImagePlaneMemoryRequirementsInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkImagePlaneMemoryRequirementsInfoKHR where
        type VkSTypeMType VkImagePlaneMemoryRequirementsInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImagePlaneMemoryRequirementsInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkImagePlaneMemoryRequirementsInfoKHR where
        type VkPNextMType VkImagePlaneMemoryRequirementsInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImagePlaneMemoryRequirementsInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkPlaneAspect VkImagePlaneMemoryRequirementsInfoKHR where
        type VkPlaneAspectMType VkImagePlaneMemoryRequirementsInfoKHR =
             VkImageAspectFlagBits

        {-# NOINLINE vkPlaneAspect #-}
        vkPlaneAspect x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfoKHR, planeAspect})

        {-# INLINE vkPlaneAspectByteOffset #-}
        vkPlaneAspectByteOffset ~_
          = #{offset VkImagePlaneMemoryRequirementsInfoKHR, planeAspect}

        {-# INLINE readVkPlaneAspect #-}
        readVkPlaneAspect p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, planeAspect}

        {-# INLINE writeVkPlaneAspect #-}
        writeVkPlaneAspect p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, planeAspect}

instance Show VkImagePlaneMemoryRequirementsInfoKHR where
        showsPrec d x
          = showString "VkImagePlaneMemoryRequirementsInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPlaneAspect = " .
                            showsPrec d (vkPlaneAspect x) . showChar '}'

data VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR = VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## ByteArray##

instance Eq VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR where
        (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## a) ==
          (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## a) `compare`
          (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr##
              VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr
          (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData
          x@(VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR where
        type VkSTypeMType VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR where
        type VkPNextMType VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkSamplerYcbcrConversion
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        type VkSamplerYcbcrConversionMType
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = VkBool32

        {-# NOINLINE vkSamplerYcbcrConversion #-}
        vkSamplerYcbcrConversion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion})

        {-# INLINE vkSamplerYcbcrConversionByteOffset #-}
        vkSamplerYcbcrConversionByteOffset ~_
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion}

        {-# INLINE readVkSamplerYcbcrConversion #-}
        readVkSamplerYcbcrConversion p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion}

        {-# INLINE writeVkSamplerYcbcrConversion #-}
        writeVkSamplerYcbcrConversion p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion}

instance Show VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        showsPrec d x
          = showString "VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSamplerYcbcrConversion = " .
                            showsPrec d (vkSamplerYcbcrConversion x) . showChar '}'

data VkSamplerYcbcrConversionImageFormatPropertiesKHR = VkSamplerYcbcrConversionImageFormatPropertiesKHR## ByteArray##

instance Eq VkSamplerYcbcrConversionImageFormatPropertiesKHR where
        (VkSamplerYcbcrConversionImageFormatPropertiesKHR## a) ==
          (VkSamplerYcbcrConversionImageFormatPropertiesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionImageFormatPropertiesKHR where
        (VkSamplerYcbcrConversionImageFormatPropertiesKHR## a) `compare`
          (VkSamplerYcbcrConversionImageFormatPropertiesKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        sizeOf ~_
          = #{size VkSamplerYcbcrConversionImageFormatPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionImageFormatPropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkSamplerYcbcrConversionImageFormatPropertiesKHR),
            I## a <- alignment
                      (undefined :: VkSamplerYcbcrConversionImageFormatPropertiesKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSamplerYcbcrConversionImageFormatPropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkSamplerYcbcrConversionImageFormatPropertiesKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkSamplerYcbcrConversionImageFormatPropertiesKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkSamplerYcbcrConversionImageFormatPropertiesKHR),
            I## a <- alignment
                      (undefined :: VkSamplerYcbcrConversionImageFormatPropertiesKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSamplerYcbcrConversionImageFormatPropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSamplerYcbcrConversionImageFormatPropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkSamplerYcbcrConversionImageFormatPropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSamplerYcbcrConversionImageFormatPropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkSamplerYcbcrConversionImageFormatPropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData
          x@(VkSamplerYcbcrConversionImageFormatPropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkSamplerYcbcrConversionImageFormatPropertiesKHR where
        type VkSTypeMType VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkSamplerYcbcrConversionImageFormatPropertiesKHR where
        type VkPNextMType VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkCombinedImageSamplerDescriptorCount
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        type VkCombinedImageSamplerDescriptorCountMType
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = Word32

        {-# NOINLINE vkCombinedImageSamplerDescriptorCount #-}
        vkCombinedImageSamplerDescriptorCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount})

        {-# INLINE vkCombinedImageSamplerDescriptorCountByteOffset #-}
        vkCombinedImageSamplerDescriptorCountByteOffset ~_
          = #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount}

        {-# INLINE readVkCombinedImageSamplerDescriptorCount #-}
        readVkCombinedImageSamplerDescriptorCount p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount}

        {-# INLINE writeVkCombinedImageSamplerDescriptorCount #-}
        writeVkCombinedImageSamplerDescriptorCount p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount}

instance Show VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionImageFormatPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkCombinedImageSamplerDescriptorCount = " .
                            showsPrec d (vkCombinedImageSamplerDescriptorCount x) .
                              showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateSamplerYcbcrConversionKHR
--   >     ( VkDevice device
--   >     , const VkSamplerYcbcrConversionCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSamplerYcbcrConversionKHR* pYcbcrConversion
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSamplerYcbcrConversionKHR.html vkCreateSamplerYcbcrConversionKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSamplerYcbcrConversionKHR"
               vkCreateSamplerYcbcrConversionKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSamplerYcbcrConversionCreateInfoKHR -- ^ pCreateInfo
                                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkSamplerYcbcrConversionKHR -- ^ pYcbcrConversion
                                                     -> IO VkResult

-- | > void vkDestroySamplerYcbcrConversionKHR
--   >     ( VkDevice device
--   >     , VkSamplerYcbcrConversionKHR ycbcrConversion
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroySamplerYcbcrConversionKHR.html vkDestroySamplerYcbcrConversionKHR registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySamplerYcbcrConversionKHR"
               vkDestroySamplerYcbcrConversionKHR ::
               VkDevice -- ^ device
                        ->
                 VkSamplerYcbcrConversionKHR -- ^ ycbcrConversion
                                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                          -> IO ()

pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION = 1

type VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION = 1

pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME :: CString

pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME <-
        (is_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME -> True)
  where VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME
          = _VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME

_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME #-}
_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME
  = Ptr "VK_KHR_sampler_ycbcr_conversion\NUL"##

is_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME ::
                                                  CString -> Bool

{-# INLINE is_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME #-}
is_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME
  = (_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME ==)

type VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME =
     "VK_KHR_sampler_ycbcr_conversion"

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR
        = VkStructureType 1000156000

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR =
        VkStructureType 1000156001

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR =
        VkStructureType 1000156002

pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR
        = VkStructureType 1000156003

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR
        = VkStructureType 1000156004

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR
        = VkStructureType 1000156005

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT
        :: VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT
        = VkDebugReportObjectTypeEXT 1000156000

pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR =
        VkObjectType 1000156000

pattern VK_FORMAT_G8B8G8R8_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8B8G8R8_422_UNORM_KHR = VkFormat 1000156000

pattern VK_FORMAT_B8G8R8G8_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_B8G8R8G8_422_UNORM_KHR = VkFormat 1000156001

pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR =
        VkFormat 1000156002

pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR =
        VkFormat 1000156003

pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR =
        VkFormat 1000156004

pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR =
        VkFormat 1000156005

pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR =
        VkFormat 1000156006

pattern VK_FORMAT_R10X6_UNORM_PACK16_KHR :: VkFormat

pattern VK_FORMAT_R10X6_UNORM_PACK16_KHR = VkFormat 1000156007

pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR :: VkFormat

pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR =
        VkFormat 1000156008

pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR =
        VkFormat 1000156009

pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR =
        VkFormat 1000156010

pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR =
        VkFormat 1000156011

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR =
        VkFormat 1000156012

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR =
        VkFormat 1000156013

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR =
        VkFormat 1000156014

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR =
        VkFormat 1000156015

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR =
        VkFormat 1000156016

pattern VK_FORMAT_R12X4_UNORM_PACK16_KHR :: VkFormat

pattern VK_FORMAT_R12X4_UNORM_PACK16_KHR = VkFormat 1000156017

pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR :: VkFormat

pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR =
        VkFormat 1000156018

pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR =
        VkFormat 1000156019

pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR =
        VkFormat 1000156020

pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR =
        VkFormat 1000156021

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR =
        VkFormat 1000156022

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR =
        VkFormat 1000156023

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR =
        VkFormat 1000156024

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR =
        VkFormat 1000156025

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR =
        VkFormat 1000156026

pattern VK_FORMAT_G16B16G16R16_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16B16G16R16_422_UNORM_KHR = VkFormat 1000156027

pattern VK_FORMAT_B16G16R16G16_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_B16G16R16G16_422_UNORM_KHR = VkFormat 1000156028

pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR =
        VkFormat 1000156029

pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR =
        VkFormat 1000156030

pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR =
        VkFormat 1000156031

pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR =
        VkFormat 1000156032

pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR =
        VkFormat 1000156033

-- | bitpos = @4@
pattern VK_IMAGE_ASPECT_PLANE_0_BIT_KHR :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_PLANE_0_BIT_KHR = VkImageAspectFlagBits 16

-- | bitpos = @5@
pattern VK_IMAGE_ASPECT_PLANE_1_BIT_KHR :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_PLANE_1_BIT_KHR = VkImageAspectFlagBits 32

-- | bitpos = @6@
pattern VK_IMAGE_ASPECT_PLANE_2_BIT_KHR :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_PLANE_2_BIT_KHR = VkImageAspectFlagBits 64

-- | bitpos = @9@
pattern VK_IMAGE_CREATE_DISJOINT_BIT_KHR :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_DISJOINT_BIT_KHR =
        VkImageCreateFlagBits 512

-- | Format can have midpoint rather than cosited chroma samples
--
--   bitpos = @17@
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR =
        VkFormatFeatureFlagBits 131072

-- | Format can be used with linear filtering whilst color conversion is enabled
--
--   bitpos = @18@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR
        = VkFormatFeatureFlagBits 262144

-- | Format can have different chroma, min and mag filters
--
--   bitpos = @19@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR
        = VkFormatFeatureFlagBits 524288

-- | bitpos = @20@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR
        = VkFormatFeatureFlagBits 1048576

-- | bitpos = @21@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR
        = VkFormatFeatureFlagBits 2097152

-- | Format supports disjoint planes
--
--   bitpos = @22@
pattern VK_FORMAT_FEATURE_DISJOINT_BIT_KHR ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_DISJOINT_BIT_KHR =
        VkFormatFeatureFlagBits 4194304

-- | Format can have cosited rather than midpoint chroma samples
--
--   bitpos = @23@
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR =
        VkFormatFeatureFlagBits 8388608
