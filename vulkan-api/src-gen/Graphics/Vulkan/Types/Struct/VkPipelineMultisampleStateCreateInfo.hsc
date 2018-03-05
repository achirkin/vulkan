#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineMultisampleStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineMultisampleStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineMultisampleStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineMultisampleStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineMultisampleStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineMultisampleStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "rasterizationSamples"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}

instance {-# OVERLAPPING #-}
         CanWriteField "rasterizationSamples"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, rasterizationSamples}

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

instance {-# OVERLAPPING #-}
         CanReadField "sampleShadingEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleShadingEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, sampleShadingEnable}

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

instance {-# OVERLAPPING #-}
         CanReadField "minSampleShading"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}

instance {-# OVERLAPPING #-}
         CanWriteField "minSampleShading"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, minSampleShading}

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

instance {-# OVERLAPPING #-}
         CanReadField "pSampleMask" VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}

instance {-# OVERLAPPING #-}
         CanWriteField "pSampleMask" VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, pSampleMask}

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

instance {-# OVERLAPPING #-}
         CanReadField "alphaToCoverageEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "alphaToCoverageEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToCoverageEnable}

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

instance {-# OVERLAPPING #-}
         CanReadField "alphaToOneEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "alphaToOneEnable"
           VkPipelineMultisampleStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineMultisampleStateCreateInfo, alphaToOneEnable}

instance Show VkPipelineMultisampleStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineMultisampleStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "rasterizationSamples = " .
                                  showsPrec d (getField @"rasterizationSamples" x) .
                                    showString ", " .
                                      showString "sampleShadingEnable = " .
                                        showsPrec d (getField @"sampleShadingEnable" x) .
                                          showString ", " .
                                            showString "minSampleShading = " .
                                              showsPrec d (getField @"minSampleShading" x) .
                                                showString ", " .
                                                  showString "pSampleMask = " .
                                                    showsPrec d (getField @"pSampleMask" x) .
                                                      showString ", " .
                                                        showString "alphaToCoverageEnable = " .
                                                          showsPrec d
                                                            (getField @"alphaToCoverageEnable" x)
                                                            .
                                                            showString ", " .
                                                              showString "alphaToOneEnable = " .
                                                                showsPrec d
                                                                  (getField @"alphaToOneEnable" x)
                                                                  . showChar '}'
