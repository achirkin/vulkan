#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSampleLocationsPropertiesEXT
       (VkPhysicalDeviceSampleLocationsPropertiesEXT(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                                                (KnownNat,
                                                                              natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                             (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags               (VkSampleCountFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkExtent2D                     (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSampleLocationsPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkSampleCountFlags               sampleLocationSampleCounts;
--   >     VkExtent2D                       maxSampleLocationGridSize;
--   >     float                            sampleLocationCoordinateRange[2];
--   >     uint32_t                         sampleLocationSubPixelBits;
--   >     VkBool32                         variableSampleLocations;
--   > } VkPhysicalDeviceSampleLocationsPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceSampleLocationsPropertiesEXT.html VkPhysicalDeviceSampleLocationsPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceSampleLocationsPropertiesEXT = VkPhysicalDeviceSampleLocationsPropertiesEXT## Addr##
                                                                                                  ByteArray##

instance Eq VkPhysicalDeviceSampleLocationsPropertiesEXT where
        (VkPhysicalDeviceSampleLocationsPropertiesEXT## a _) ==
          x@(VkPhysicalDeviceSampleLocationsPropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSampleLocationsPropertiesEXT where
        (VkPhysicalDeviceSampleLocationsPropertiesEXT## a _) `compare`
          x@(VkPhysicalDeviceSampleLocationsPropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceSampleLocationsPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSampleLocationsPropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        unsafeAddr (VkPhysicalDeviceSampleLocationsPropertiesEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceSampleLocationsPropertiesEXT## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceSampleLocationsPropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type StructFields VkPhysicalDeviceSampleLocationsPropertiesEXT =
             '["sType", "pNext", "sampleLocationSampleCounts", -- ' closing tick for hsc2hs
               "maxSampleLocationGridSize", "sampleLocationCoordinateRange",
               "sampleLocationSubPixelBits", "variableSampleLocations"]
        type CUnionType VkPhysicalDeviceSampleLocationsPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceSampleLocationsPropertiesEXT =
             'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceSampleLocationsPropertiesEXT =
             '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceSampleLocationsPropertiesEXT where
        type VkSTypeMType VkPhysicalDeviceSampleLocationsPropertiesEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceSampleLocationsPropertiesEXT where
        type FieldType "sType" VkPhysicalDeviceSampleLocationsPropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             =
             #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}

instance CanReadField "sType"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceSampleLocationsPropertiesEXT where
        type VkPNextMType VkPhysicalDeviceSampleLocationsPropertiesEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceSampleLocationsPropertiesEXT where
        type FieldType "pNext" VkPhysicalDeviceSampleLocationsPropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             =
             #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSampleLocationSampleCounts
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type VkSampleLocationSampleCountsMType
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = VkSampleCountFlags

        {-# NOINLINE vkSampleLocationSampleCounts #-}
        vkSampleLocationSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts})

        {-# INLINE vkSampleLocationSampleCountsByteOffset #-}
        vkSampleLocationSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}

        {-# INLINE readVkSampleLocationSampleCounts #-}
        readVkSampleLocationSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}

        {-# INLINE writeVkSampleLocationSampleCounts #-}
        writeVkSampleLocationSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationSampleCounts"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type FieldType "sampleLocationSampleCounts"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = VkSampleCountFlags
        type FieldOptional "sampleLocationSampleCounts"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationSampleCounts"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             =
             #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}
        type FieldIsArray "sampleLocationSampleCounts"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}

instance CanReadField "sampleLocationSampleCounts"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationSampleCounts

        {-# INLINE readField #-}
        readField = readVkSampleLocationSampleCounts

instance CanWriteField "sampleLocationSampleCounts"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationSampleCounts

instance {-# OVERLAPPING #-}
         HasVkMaxSampleLocationGridSize
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type VkMaxSampleLocationGridSizeMType
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = VkExtent2D

        {-# NOINLINE vkMaxSampleLocationGridSize #-}
        vkMaxSampleLocationGridSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize})

        {-# INLINE vkMaxSampleLocationGridSizeByteOffset #-}
        vkMaxSampleLocationGridSizeByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}

        {-# INLINE readVkMaxSampleLocationGridSize #-}
        readVkMaxSampleLocationGridSize p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}

        {-# INLINE writeVkMaxSampleLocationGridSize #-}
        writeVkMaxSampleLocationGridSize p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}

instance {-# OVERLAPPING #-}
         HasField "maxSampleLocationGridSize"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type FieldType "maxSampleLocationGridSize"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = VkExtent2D
        type FieldOptional "maxSampleLocationGridSize"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSampleLocationGridSize"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             =
             #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}
        type FieldIsArray "maxSampleLocationGridSize"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}

instance CanReadField "maxSampleLocationGridSize"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkMaxSampleLocationGridSize

        {-# INLINE readField #-}
        readField = readVkMaxSampleLocationGridSize

instance CanWriteField "maxSampleLocationGridSize"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxSampleLocationGridSize

instance {-# OVERLAPPING #-}
         HasVkSampleLocationCoordinateRangeArray
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type VkSampleLocationCoordinateRangeArrayMType
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = #{type float}

        {-# NOINLINE vkSampleLocationCoordinateRangeArray #-}
        vkSampleLocationCoordinateRangeArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: #{type float}) +
                    #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange}))

        {-# INLINE vkSampleLocationCoordinateRangeArrayByteOffset #-}
        vkSampleLocationCoordinateRangeArrayByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange}

        {-# INLINE readVkSampleLocationCoordinateRangeArray #-}
        readVkSampleLocationCoordinateRangeArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange})

        {-# INLINE writeVkSampleLocationCoordinateRangeArray #-}
        writeVkSampleLocationCoordinateRangeArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange})

instance {-# OVERLAPPING #-}
         HasField "sampleLocationCoordinateRange"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type FieldType "sampleLocationCoordinateRange"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = #{type float}
        type FieldOptional "sampleLocationCoordinateRange"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationCoordinateRange"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             =
             #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange}
        type FieldIsArray "sampleLocationCoordinateRange"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange}

instance (KnownNat idx,
          IndexInBounds "sampleLocationCoordinateRange" idx
            VkPhysicalDeviceSampleLocationsPropertiesEXT) =>
         CanReadFieldArray "sampleLocationCoordinateRange" idx
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "sampleLocationCoordinateRange" 0
                         VkPhysicalDeviceSampleLocationsPropertiesEXT
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "sampleLocationCoordinateRange" 1
                         VkPhysicalDeviceSampleLocationsPropertiesEXT
                       #-}
        type FieldArrayLength "sampleLocationCoordinateRange"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 2

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 2

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkSampleLocationCoordinateRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkSampleLocationCoordinateRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "sampleLocationCoordinateRange" idx
            VkPhysicalDeviceSampleLocationsPropertiesEXT) =>
         CanWriteFieldArray "sampleLocationCoordinateRange" idx
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "sampleLocationCoordinateRange" 0
                         VkPhysicalDeviceSampleLocationsPropertiesEXT
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "sampleLocationCoordinateRange" 1
                         VkPhysicalDeviceSampleLocationsPropertiesEXT
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkSampleLocationCoordinateRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSampleLocationSubPixelBits
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type VkSampleLocationSubPixelBitsMType
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = Word32

        {-# NOINLINE vkSampleLocationSubPixelBits #-}
        vkSampleLocationSubPixelBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits})

        {-# INLINE vkSampleLocationSubPixelBitsByteOffset #-}
        vkSampleLocationSubPixelBitsByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}

        {-# INLINE readVkSampleLocationSubPixelBits #-}
        readVkSampleLocationSubPixelBits p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}

        {-# INLINE writeVkSampleLocationSubPixelBits #-}
        writeVkSampleLocationSubPixelBits p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationSubPixelBits"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type FieldType "sampleLocationSubPixelBits"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = Word32
        type FieldOptional "sampleLocationSubPixelBits"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationSubPixelBits"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             =
             #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}
        type FieldIsArray "sampleLocationSubPixelBits"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}

instance CanReadField "sampleLocationSubPixelBits"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationSubPixelBits

        {-# INLINE readField #-}
        readField = readVkSampleLocationSubPixelBits

instance CanWriteField "sampleLocationSubPixelBits"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationSubPixelBits

instance {-# OVERLAPPING #-}
         HasVkVariableSampleLocations
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type VkVariableSampleLocationsMType
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = VkBool32

        {-# NOINLINE vkVariableSampleLocations #-}
        vkVariableSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations})

        {-# INLINE vkVariableSampleLocationsByteOffset #-}
        vkVariableSampleLocationsByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}

        {-# INLINE readVkVariableSampleLocations #-}
        readVkVariableSampleLocations p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}

        {-# INLINE writeVkVariableSampleLocations #-}
        writeVkVariableSampleLocations p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}

instance {-# OVERLAPPING #-}
         HasField "variableSampleLocations"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type FieldType "variableSampleLocations"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = VkBool32
        type FieldOptional "variableSampleLocations"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "variableSampleLocations"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             =
             #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}
        type FieldIsArray "variableSampleLocations"
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}

instance CanReadField "variableSampleLocations"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkVariableSampleLocations

        {-# INLINE readField #-}
        readField = readVkVariableSampleLocations

instance CanWriteField "variableSampleLocations"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkVariableSampleLocations

instance Show VkPhysicalDeviceSampleLocationsPropertiesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceSampleLocationsPropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSampleLocationSampleCounts = " .
                            showsPrec d (vkSampleLocationSampleCounts x) .
                              showString ", " .
                                showString "vkMaxSampleLocationGridSize = " .
                                  showsPrec d (vkMaxSampleLocationGridSize x) .
                                    showString ", " .
                                      showString "vkSampleLocationCoordinateRangeArray = [" .
                                        showsPrec d
                                          (map (vkSampleLocationCoordinateRangeArray x) [1 .. 2])
                                          .
                                          showChar ']' .
                                            showString ", " .
                                              showString "vkSampleLocationSubPixelBits = " .
                                                showsPrec d (vkSampleLocationSubPixelBits x) .
                                                  showString ", " .
                                                    showString "vkVariableSampleLocations = " .
                                                      showsPrec d (vkVariableSampleLocations x) .
                                                        showChar '}'
