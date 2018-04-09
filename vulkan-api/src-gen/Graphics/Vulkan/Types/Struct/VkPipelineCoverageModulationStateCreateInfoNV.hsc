#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineCoverageModulationStateCreateInfoNV
       (VkPipelineCoverageModulationStateCreateInfoNV(..)) where
import           Foreign.Storable
                                                                                    (Storable (..))
import           GHC.Base
                                                                                    (Addr##,
                                                                                    ByteArray##,
                                                                                    byteArrayContents##,
                                                                                    plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes
                                                                                    (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks
                                                                                    (VkPipelineCoverageModulationStateCreateFlagsNV)
import           Graphics.Vulkan.Types.Enum.VkCoverageModulationModeNV
                                                                                    (VkCoverageModulationModeNV)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo
                                                                                    (VkPipelineMultisampleStateCreateInfo)
import           System.IO.Unsafe
                                                                                    (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineCoverageModulationStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineCoverageModulationStateCreateFlagsNV                   flags;
--   >     VkCoverageModulationModeNV                                                       coverageModulationMode;
--   >     VkBool32                                                                         coverageModulationTableEnable;
--   >     uint32_t                                                                         coverageModulationTableCount;
--   >     const float* pCoverageModulationTable;
--   > } VkPipelineCoverageModulationStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPipelineCoverageModulationStateCreateInfoNVVkPipelineCoverageModulationStateCreateInfoNV registry at www.khronos.org>
data VkPipelineCoverageModulationStateCreateInfoNV = VkPipelineCoverageModulationStateCreateInfoNV## Addr##
                                                                                                    ByteArray##

instance Eq VkPipelineCoverageModulationStateCreateInfoNV where
        (VkPipelineCoverageModulationStateCreateInfoNV## a _) ==
          x@(VkPipelineCoverageModulationStateCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineCoverageModulationStateCreateInfoNV where
        (VkPipelineCoverageModulationStateCreateInfoNV## a _) `compare`
          x@(VkPipelineCoverageModulationStateCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineCoverageModulationStateCreateInfoNV
         where
        sizeOf ~_
          = #{size VkPipelineCoverageModulationStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineCoverageModulationStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        unsafeAddr (VkPipelineCoverageModulationStateCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineCoverageModulationStateCreateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineCoverageModulationStateCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type StructFields VkPipelineCoverageModulationStateCreateInfoNV =
             '["sType", "pNext", "flags", "coverageModulationMode", -- ' closing tick for hsc2hs
               "coverageModulationTableEnable", "coverageModulationTableCount",
               "pCoverageModulationTable"]
        type CUnionType VkPipelineCoverageModulationStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineCoverageModulationStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineCoverageModulationStateCreateInfoNV =
             '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "sType"
               VkPipelineCoverageModulationStateCreateInfoNV
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}
        type FieldIsArray "sType"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "pNext"
               VkPipelineCoverageModulationStateCreateInfoNV
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}
        type FieldIsArray "pNext"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "flags"
               VkPipelineCoverageModulationStateCreateInfoNV
             = VkPipelineCoverageModulationStateCreateFlagsNV
        type FieldOptional "flags"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}
        type FieldIsArray "flags"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         HasField "coverageModulationMode"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "coverageModulationMode"
               VkPipelineCoverageModulationStateCreateInfoNV
             = VkCoverageModulationModeNV
        type FieldOptional "coverageModulationMode"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "coverageModulationMode"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}
        type FieldIsArray "coverageModulationMode"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}

instance {-# OVERLAPPING #-}
         CanReadField "coverageModulationMode"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}

instance {-# OVERLAPPING #-}
         CanWriteField "coverageModulationMode"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}

instance {-# OVERLAPPING #-}
         HasField "coverageModulationTableEnable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "coverageModulationTableEnable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = VkBool32
        type FieldOptional "coverageModulationTableEnable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "coverageModulationTableEnable"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}
        type FieldIsArray "coverageModulationTableEnable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}

instance {-# OVERLAPPING #-}
         CanReadField "coverageModulationTableEnable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "coverageModulationTableEnable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}

instance {-# OVERLAPPING #-}
         HasField "coverageModulationTableCount"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "coverageModulationTableCount"
               VkPipelineCoverageModulationStateCreateInfoNV
             = Word32
        type FieldOptional "coverageModulationTableCount"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "coverageModulationTableCount"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}
        type FieldIsArray "coverageModulationTableCount"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}

instance {-# OVERLAPPING #-}
         CanReadField "coverageModulationTableCount"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}

instance {-# OVERLAPPING #-}
         CanWriteField "coverageModulationTableCount"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}

instance {-# OVERLAPPING #-}
         HasField "pCoverageModulationTable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type FieldType "pCoverageModulationTable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = Ptr #{type float}
        type FieldOptional "pCoverageModulationTable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pCoverageModulationTable"
               VkPipelineCoverageModulationStateCreateInfoNV
             =
             #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}
        type FieldIsArray "pCoverageModulationTable"
               VkPipelineCoverageModulationStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}

instance {-# OVERLAPPING #-}
         CanReadField "pCoverageModulationTable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}

instance {-# OVERLAPPING #-}
         CanWriteField "pCoverageModulationTable"
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}

instance Show VkPipelineCoverageModulationStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineCoverageModulationStateCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "coverageModulationMode = " .
                                  showsPrec d (getField @"coverageModulationMode" x) .
                                    showString ", " .
                                      showString "coverageModulationTableEnable = " .
                                        showsPrec d (getField @"coverageModulationTableEnable" x) .
                                          showString ", " .
                                            showString "coverageModulationTableCount = " .
                                              showsPrec d
                                                (getField @"coverageModulationTableCount" x)
                                                .
                                                showString ", " .
                                                  showString "pCoverageModulationTable = " .
                                                    showsPrec d
                                                      (getField @"pCoverageModulationTable" x)
                                                      . showChar '}'
