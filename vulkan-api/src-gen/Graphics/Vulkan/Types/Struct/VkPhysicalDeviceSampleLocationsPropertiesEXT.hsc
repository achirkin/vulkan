#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSampleLocationsPropertiesEXT
       (VkPhysicalDeviceSampleLocationsPropertiesEXT(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           Proxy##,
                                                                           byteArrayContents##,
                                                                           plusAddr##,
                                                                           proxy##)
import           GHC.TypeLits                                             (KnownNat,
                                                                           natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags            (VkSampleCountFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkExtent2D                  (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDeviceSampleLocationsPropertiesEXTVkPhysicalDeviceSampleLocationsPropertiesEXT registry at www.khronos.org>
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
             '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationSampleCounts"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationSampleCounts"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxSampleLocationGridSize"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSampleLocationGridSize"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange}
                      +
                      sizeOf (undefined :: #{type float}) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange}
                 +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange}
                 +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationSubPixelBits"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationSubPixelBits"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}

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

instance {-# OVERLAPPING #-}
         CanReadField "variableSampleLocations"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}

instance {-# OVERLAPPING #-}
         CanWriteField "variableSampleLocations"
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}

instance Show VkPhysicalDeviceSampleLocationsPropertiesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceSampleLocationsPropertiesEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "sampleLocationSampleCounts = " .
                            showsPrec d (getField @"sampleLocationSampleCounts" x) .
                              showString ", " .
                                showString "maxSampleLocationGridSize = " .
                                  showsPrec d (getField @"maxSampleLocationGridSize" x) .
                                    showString ", " .
                                      (showString "sampleLocationCoordinateRange = [" .
                                         showsPrec d
                                           (let s = sizeOf
                                                      (undefined ::
                                                         FieldType "sampleLocationCoordinateRange"
                                                           VkPhysicalDeviceSampleLocationsPropertiesEXT)
                                                o = fieldOffset @"sampleLocationCoordinateRange"
                                                      @VkPhysicalDeviceSampleLocationsPropertiesEXT
                                                f i
                                                  = peekByteOff (unsafePtr x) i ::
                                                      IO
                                                        (FieldType "sampleLocationCoordinateRange"
                                                           VkPhysicalDeviceSampleLocationsPropertiesEXT)
                                              in
                                              unsafeDupablePerformIO . mapM f $
                                                map (\ i -> o + i * s) [0 .. 2 - 1])
                                           . showChar ']')
                                        .
                                        showString ", " .
                                          showString "sampleLocationSubPixelBits = " .
                                            showsPrec d (getField @"sampleLocationSubPixelBits" x) .
                                              showString ", " .
                                                showString "variableSampleLocations = " .
                                                  showsPrec d
                                                    (getField @"variableSampleLocations" x)
                                                    . showChar '}'
