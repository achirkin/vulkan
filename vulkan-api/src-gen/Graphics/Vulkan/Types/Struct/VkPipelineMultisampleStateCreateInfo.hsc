#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo
       (VkPipelineMultisampleStateCreateInfo(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkBool32,
                                                                VkSampleMask)
import           Graphics.Vulkan.Types.Bitmasks                (VkPipelineMultisampleStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineMultisampleStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineMultisampleStateCreateFlags    flags;
--   >     VkSampleCountFlagBits  rasterizationSamples;
--   >     VkBool32               sampleShadingEnable;
--   >     float                  minSampleShading;
--   >     const VkSampleMask*    pSampleMask;
--   >     VkBool32               alphaToCoverageEnable;
--   >     VkBool32               alphaToOneEnable;
--   > } VkPipelineMultisampleStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineMultisampleStateCreateInfo.html VkPipelineMultisampleStateCreateInfo registry at www.khronos.org>
data VkPipelineMultisampleStateCreateInfo = VkPipelineMultisampleStateCreateInfo## Addr##
                                                                                  ByteArray##

instance Eq VkPipelineMultisampleStateCreateInfo where
        (VkPipelineMultisampleStateCreateInfo## a _) ==
          x@(VkPipelineMultisampleStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineMultisampleStateCreateInfo where
        (VkPipelineMultisampleStateCreateInfo## a _) `compare`
          x@(VkPipelineMultisampleStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineMultisampleStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineMultisampleStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineMultisampleStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineMultisampleStateCreateInfo
         where
        unsafeAddr (VkPipelineMultisampleStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineMultisampleStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineMultisampleStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineMultisampleStateCreateInfo where
        type StructFields VkPipelineMultisampleStateCreateInfo =
             '["sType", "pNext", "flags", "rasterizationSamples", -- ' closing tick for hsc2hs
               "sampleShadingEnable", "minSampleShading", "pSampleMask",
               "alphaToCoverageEnable", "alphaToOneEnable"]
        type CUnionType VkPipelineMultisampleStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineMultisampleStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineMultisampleStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineMultisampleStateCreateInfo where
        type VkSTypeMType VkPipelineMultisampleStateCreateInfo =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineMultisampleStateCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineMultisampleStateCreateInfo where
        type FieldType "sType" VkPipelineMultisampleStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineMultisampleStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineMultisampleStateCreateInfo =
             #{offset VkPipelineMultisampleStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineMultisampleStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, sType}

instance CanReadField "sType" VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineMultisampleStateCreateInfo where
        type VkPNextMType VkPipelineMultisampleStateCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineMultisampleStateCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineMultisampleStateCreateInfo where
        type FieldType "pNext" VkPipelineMultisampleStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineMultisampleStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineMultisampleStateCreateInfo =
             #{offset VkPipelineMultisampleStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineMultisampleStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, pNext}

instance CanReadField "pNext" VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineMultisampleStateCreateInfo where
        type VkFlagsMType VkPipelineMultisampleStateCreateInfo =
             VkPipelineMultisampleStateCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineMultisampleStateCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineMultisampleStateCreateInfo where
        type FieldType "flags" VkPipelineMultisampleStateCreateInfo =
             VkPipelineMultisampleStateCreateFlags
        type FieldOptional "flags" VkPipelineMultisampleStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineMultisampleStateCreateInfo =
             #{offset VkPipelineMultisampleStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineMultisampleStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, flags}

instance CanReadField "flags" VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkRasterizationSamples VkPipelineMultisampleStateCreateInfo
         where
        type VkRasterizationSamplesMType
               VkPipelineMultisampleStateCreateInfo
             = VkSampleCountFlagBits

        {-# NOINLINE vkRasterizationSamples #-}
        vkRasterizationSamples x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples})

        {-# INLINE vkRasterizationSamplesByteOffset #-}
        vkRasterizationSamplesByteOffset ~_
          = #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}

        {-# INLINE readVkRasterizationSamples #-}
        readVkRasterizationSamples p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}

        {-# INLINE writeVkRasterizationSamples #-}
        writeVkRasterizationSamples p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}

instance {-# OVERLAPPING #-}
         HasField "rasterizationSamples"
           VkPipelineMultisampleStateCreateInfo
         where
        type FieldType "rasterizationSamples"
               VkPipelineMultisampleStateCreateInfo
             = VkSampleCountFlagBits
        type FieldOptional "rasterizationSamples"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "rasterizationSamples"
               VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}
        type FieldIsArray "rasterizationSamples"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}

instance CanReadField "rasterizationSamples"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkRasterizationSamples

        {-# INLINE readField #-}
        readField = readVkRasterizationSamples

instance CanWriteField "rasterizationSamples"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkRasterizationSamples

instance {-# OVERLAPPING #-}
         HasVkSampleShadingEnable VkPipelineMultisampleStateCreateInfo where
        type VkSampleShadingEnableMType
               VkPipelineMultisampleStateCreateInfo
             = VkBool32

        {-# NOINLINE vkSampleShadingEnable #-}
        vkSampleShadingEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable})

        {-# INLINE vkSampleShadingEnableByteOffset #-}
        vkSampleShadingEnableByteOffset ~_
          = #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}

        {-# INLINE readVkSampleShadingEnable #-}
        readVkSampleShadingEnable p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}

        {-# INLINE writeVkSampleShadingEnable #-}
        writeVkSampleShadingEnable p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}

instance {-# OVERLAPPING #-}
         HasField "sampleShadingEnable" VkPipelineMultisampleStateCreateInfo
         where
        type FieldType "sampleShadingEnable"
               VkPipelineMultisampleStateCreateInfo
             = VkBool32
        type FieldOptional "sampleShadingEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleShadingEnable"
               VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}
        type FieldIsArray "sampleShadingEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}

instance CanReadField "sampleShadingEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSampleShadingEnable

        {-# INLINE readField #-}
        readField = readVkSampleShadingEnable

instance CanWriteField "sampleShadingEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleShadingEnable

instance {-# OVERLAPPING #-}
         HasVkMinSampleShading VkPipelineMultisampleStateCreateInfo where
        type VkMinSampleShadingMType VkPipelineMultisampleStateCreateInfo =
             #{type float}

        {-# NOINLINE vkMinSampleShading #-}
        vkMinSampleShading x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading})

        {-# INLINE vkMinSampleShadingByteOffset #-}
        vkMinSampleShadingByteOffset ~_
          = #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}

        {-# INLINE readVkMinSampleShading #-}
        readVkMinSampleShading p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}

        {-# INLINE writeVkMinSampleShading #-}
        writeVkMinSampleShading p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}

instance {-# OVERLAPPING #-}
         HasField "minSampleShading" VkPipelineMultisampleStateCreateInfo
         where
        type FieldType "minSampleShading"
               VkPipelineMultisampleStateCreateInfo
             = #{type float}
        type FieldOptional "minSampleShading"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minSampleShading"
               VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}
        type FieldIsArray "minSampleShading"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}

instance CanReadField "minSampleShading"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkMinSampleShading

        {-# INLINE readField #-}
        readField = readVkMinSampleShading

instance CanWriteField "minSampleShading"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinSampleShading

instance {-# OVERLAPPING #-}
         HasVkPSampleMask VkPipelineMultisampleStateCreateInfo where
        type VkPSampleMaskMType VkPipelineMultisampleStateCreateInfo =
             Ptr VkSampleMask

        {-# NOINLINE vkPSampleMask #-}
        vkPSampleMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask})

        {-# INLINE vkPSampleMaskByteOffset #-}
        vkPSampleMaskByteOffset ~_
          = #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}

        {-# INLINE readVkPSampleMask #-}
        readVkPSampleMask p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}

        {-# INLINE writeVkPSampleMask #-}
        writeVkPSampleMask p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}

instance {-# OVERLAPPING #-}
         HasField "pSampleMask" VkPipelineMultisampleStateCreateInfo where
        type FieldType "pSampleMask" VkPipelineMultisampleStateCreateInfo =
             Ptr VkSampleMask
        type FieldOptional "pSampleMask"
               VkPipelineMultisampleStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pSampleMask" VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}
        type FieldIsArray "pSampleMask"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}

instance CanReadField "pSampleMask"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPSampleMask

        {-# INLINE readField #-}
        readField = readVkPSampleMask

instance CanWriteField "pSampleMask"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPSampleMask

instance {-# OVERLAPPING #-}
         HasVkAlphaToCoverageEnable VkPipelineMultisampleStateCreateInfo
         where
        type VkAlphaToCoverageEnableMType
               VkPipelineMultisampleStateCreateInfo
             = VkBool32

        {-# NOINLINE vkAlphaToCoverageEnable #-}
        vkAlphaToCoverageEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable})

        {-# INLINE vkAlphaToCoverageEnableByteOffset #-}
        vkAlphaToCoverageEnableByteOffset ~_
          = #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}

        {-# INLINE readVkAlphaToCoverageEnable #-}
        readVkAlphaToCoverageEnable p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}

        {-# INLINE writeVkAlphaToCoverageEnable #-}
        writeVkAlphaToCoverageEnable p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}

instance {-# OVERLAPPING #-}
         HasField "alphaToCoverageEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        type FieldType "alphaToCoverageEnable"
               VkPipelineMultisampleStateCreateInfo
             = VkBool32
        type FieldOptional "alphaToCoverageEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "alphaToCoverageEnable"
               VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}
        type FieldIsArray "alphaToCoverageEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}

instance CanReadField "alphaToCoverageEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkAlphaToCoverageEnable

        {-# INLINE readField #-}
        readField = readVkAlphaToCoverageEnable

instance CanWriteField "alphaToCoverageEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkAlphaToCoverageEnable

instance {-# OVERLAPPING #-}
         HasVkAlphaToOneEnable VkPipelineMultisampleStateCreateInfo where
        type VkAlphaToOneEnableMType VkPipelineMultisampleStateCreateInfo =
             VkBool32

        {-# NOINLINE vkAlphaToOneEnable #-}
        vkAlphaToOneEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable})

        {-# INLINE vkAlphaToOneEnableByteOffset #-}
        vkAlphaToOneEnableByteOffset ~_
          = #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}

        {-# INLINE readVkAlphaToOneEnable #-}
        readVkAlphaToOneEnable p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}

        {-# INLINE writeVkAlphaToOneEnable #-}
        writeVkAlphaToOneEnable p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}

instance {-# OVERLAPPING #-}
         HasField "alphaToOneEnable" VkPipelineMultisampleStateCreateInfo
         where
        type FieldType "alphaToOneEnable"
               VkPipelineMultisampleStateCreateInfo
             = VkBool32
        type FieldOptional "alphaToOneEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "alphaToOneEnable"
               VkPipelineMultisampleStateCreateInfo
             =
             #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}
        type FieldIsArray "alphaToOneEnable"
               VkPipelineMultisampleStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}

instance CanReadField "alphaToOneEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkAlphaToOneEnable

        {-# INLINE readField #-}
        readField = readVkAlphaToOneEnable

instance CanWriteField "alphaToOneEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkAlphaToOneEnable

instance Show VkPipelineMultisampleStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineMultisampleStateCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkRasterizationSamples = " .
                                  showsPrec d (vkRasterizationSamples x) .
                                    showString ", " .
                                      showString "vkSampleShadingEnable = " .
                                        showsPrec d (vkSampleShadingEnable x) .
                                          showString ", " .
                                            showString "vkMinSampleShading = " .
                                              showsPrec d (vkMinSampleShading x) .
                                                showString ", " .
                                                  showString "vkPSampleMask = " .
                                                    showsPrec d (vkPSampleMask x) .
                                                      showString ", " .
                                                        showString "vkAlphaToCoverageEnable = " .
                                                          showsPrec d (vkAlphaToCoverageEnable x) .
                                                            showString ", " .
                                                              showString "vkAlphaToOneEnable = " .
                                                                showsPrec d (vkAlphaToOneEnable x) .
                                                                  showChar '}'
