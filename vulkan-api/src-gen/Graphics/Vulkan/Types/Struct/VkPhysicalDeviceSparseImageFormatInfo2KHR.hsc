#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2KHR
       (VkPhysicalDeviceSparseImageFormatInfo2KHR(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormat           (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageTiling      (VkImageTiling)
import           Graphics.Vulkan.Types.Enum.VkImageType        (VkImageType)
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags  (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSparseImageFormatInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkFormat                         format;
--   >     VkImageType                      type;
--   >     VkSampleCountFlagBits            samples;
--   >     VkImageUsageFlags                usage;
--   >     VkImageTiling                    tiling;
--   > } VkPhysicalDeviceSparseImageFormatInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceSparseImageFormatInfo2KHR.html VkPhysicalDeviceSparseImageFormatInfo2KHR registry at www.khronos.org>
data VkPhysicalDeviceSparseImageFormatInfo2KHR = VkPhysicalDeviceSparseImageFormatInfo2KHR## Addr##
                                                                                            ByteArray##

instance Eq VkPhysicalDeviceSparseImageFormatInfo2KHR where
        (VkPhysicalDeviceSparseImageFormatInfo2KHR## a _) ==
          x@(VkPhysicalDeviceSparseImageFormatInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSparseImageFormatInfo2KHR where
        (VkPhysicalDeviceSparseImageFormatInfo2KHR## a _) `compare`
          x@(VkPhysicalDeviceSparseImageFormatInfo2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSparseImageFormatInfo2KHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceSparseImageFormatInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSparseImageFormatInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        unsafeAddr (VkPhysicalDeviceSparseImageFormatInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceSparseImageFormatInfo2KHR## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceSparseImageFormatInfo2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        type StructFields VkPhysicalDeviceSparseImageFormatInfo2KHR =
             '["sType", "pNext", "format", "type", "samples", "usage", "tiling"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceSparseImageFormatInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceSparseImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceSparseImageFormatInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkSTypeMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}

instance CanReadField "sType"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkPNextMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFormat VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkFormatMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkFormat

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}

instance {-# OVERLAPPING #-}
         HasField "format" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "format" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkFormat
        type FieldOptional "format"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}
        type FieldIsArray "format"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}

instance CanReadField "format"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-}
         HasVkType VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkTypeMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageType

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}

instance {-# OVERLAPPING #-}
         HasField "type" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "type" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageType
        type FieldOptional "type" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}
        type FieldIsArray "type" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}

instance CanReadField "type"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkType

        {-# INLINE readField #-}
        readField = readVkType

instance CanWriteField "type"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkType

instance {-# OVERLAPPING #-}
         HasVkSamples VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkSamplesMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkSampleCountFlagBits

        {-# NOINLINE vkSamples #-}
        vkSamples x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples})

        {-# INLINE vkSamplesByteOffset #-}
        vkSamplesByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}

        {-# INLINE readVkSamples #-}
        readVkSamples p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}

        {-# INLINE writeVkSamples #-}
        writeVkSamples p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}

instance {-# OVERLAPPING #-}
         HasField "samples" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "samples" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = VkSampleCountFlagBits
        type FieldOptional "samples"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "samples"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}
        type FieldIsArray "samples"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}

instance CanReadField "samples"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSamples

        {-# INLINE readField #-}
        readField = readVkSamples

instance CanWriteField "samples"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSamples

instance {-# OVERLAPPING #-}
         HasVkUsage VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkUsageMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageUsageFlags

        {-# NOINLINE vkUsage #-}
        vkUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage})

        {-# INLINE vkUsageByteOffset #-}
        vkUsageByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}

        {-# INLINE readVkUsage #-}
        readVkUsage p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}

        {-# INLINE writeVkUsage #-}
        writeVkUsage p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageUsageFlags
        type FieldOptional "usage"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}
        type FieldIsArray "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}

instance CanReadField "usage"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkUsage

        {-# INLINE readField #-}
        readField = readVkUsage

instance CanWriteField "usage"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkUsage

instance {-# OVERLAPPING #-}
         HasVkTiling VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkTilingMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageTiling

        {-# NOINLINE vkTiling #-}
        vkTiling x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling})

        {-# INLINE vkTilingByteOffset #-}
        vkTilingByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}

        {-# INLINE readVkTiling #-}
        readVkTiling p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}

        {-# INLINE writeVkTiling #-}
        writeVkTiling p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}

instance {-# OVERLAPPING #-}
         HasField "tiling" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "tiling" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageTiling
        type FieldOptional "tiling"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tiling" VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}
        type FieldIsArray "tiling"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}

instance CanReadField "tiling"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkTiling

        {-# INLINE readField #-}
        readField = readVkTiling

instance CanWriteField "tiling"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkTiling

instance Show VkPhysicalDeviceSparseImageFormatInfo2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceSparseImageFormatInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFormat = " .
                            showsPrec d (vkFormat x) .
                              showString ", " .
                                showString "vkType = " .
                                  showsPrec d (vkType x) .
                                    showString ", " .
                                      showString "vkSamples = " .
                                        showsPrec d (vkSamples x) .
                                          showString ", " .
                                            showString "vkUsage = " .
                                              showsPrec d (vkUsage x) .
                                                showString ", " .
                                                  showString "vkTiling = " .
                                                    showsPrec d (vkTiling x) . showChar '}'
