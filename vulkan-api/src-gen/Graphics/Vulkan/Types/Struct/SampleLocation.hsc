#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SampleLocation
       (VkSampleLocationEXT(..), VkSampleLocationsInfoEXT(..)) where
import           Foreign.Storable                            (Storable (..))
import           GHC.Base                                    (Addr##, ByteArray##,
                                                              byteArrayContents##,
                                                              plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.SampleCountFlags (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.Enum.StructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Extent         (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.Image          (VkImageMemoryBarrier)
import           System.IO.Unsafe                            (unsafeDupablePerformIO)

-- | > typedef struct VkSampleLocationEXT {
--   >     float                            x;
--   >     float                            y;
--   > } VkSampleLocationEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSampleLocationEXT VkSampleLocationEXT registry at www.khronos.org>
data VkSampleLocationEXT = VkSampleLocationEXT## Addr## ByteArray##

instance Eq VkSampleLocationEXT where
        (VkSampleLocationEXT## a _) == x@(VkSampleLocationEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSampleLocationEXT where
        (VkSampleLocationEXT## a _) `compare` x@(VkSampleLocationEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSampleLocationEXT where
        sizeOf ~_ = #{size VkSampleLocationEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSampleLocationEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSampleLocationEXT where
        unsafeAddr (VkSampleLocationEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSampleLocationEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSampleLocationEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSampleLocationEXT where
        type StructFields VkSampleLocationEXT = '["x", "y"] -- ' closing tick for hsc2hs
        type CUnionType VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSampleLocationEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "x" VkSampleLocationEXT where
        type FieldType "x" VkSampleLocationEXT = #{type float}
        type FieldOptional "x" VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "x" VkSampleLocationEXT =
             #{offset VkSampleLocationEXT, x}
        type FieldIsArray "x" VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSampleLocationEXT, x}

instance {-# OVERLAPPING #-} CanReadField "x" VkSampleLocationEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationEXT, x})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSampleLocationEXT, x}

instance {-# OVERLAPPING #-} CanWriteField "x" VkSampleLocationEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSampleLocationEXT, x}

instance {-# OVERLAPPING #-} HasField "y" VkSampleLocationEXT where
        type FieldType "y" VkSampleLocationEXT = #{type float}
        type FieldOptional "y" VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "y" VkSampleLocationEXT =
             #{offset VkSampleLocationEXT, y}
        type FieldIsArray "y" VkSampleLocationEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSampleLocationEXT, y}

instance {-# OVERLAPPING #-} CanReadField "y" VkSampleLocationEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationEXT, y})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSampleLocationEXT, y}

instance {-# OVERLAPPING #-} CanWriteField "y" VkSampleLocationEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSampleLocationEXT, y}

instance Show VkSampleLocationEXT where
        showsPrec d x
          = showString "VkSampleLocationEXT {" .
              showString "x = " .
                showsPrec d (getField @"x" x) .
                  showString ", " .
                    showString "y = " . showsPrec d (getField @"y" x) . showChar '}'

-- | > typedef struct VkSampleLocationsInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSampleCountFlagBits            sampleLocationsPerPixel;
--   >     VkExtent2D                       sampleLocationGridSize;
--   >     uint32_t                         sampleLocationsCount;
--   >     const VkSampleLocationEXT* pSampleLocations;
--   > } VkSampleLocationsInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSampleLocationsInfoEXT VkSampleLocationsInfoEXT registry at www.khronos.org>
data VkSampleLocationsInfoEXT = VkSampleLocationsInfoEXT## Addr##
                                                          ByteArray##

instance Eq VkSampleLocationsInfoEXT where
        (VkSampleLocationsInfoEXT## a _) ==
          x@(VkSampleLocationsInfoEXT## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSampleLocationsInfoEXT where
        (VkSampleLocationsInfoEXT## a _) `compare`
          x@(VkSampleLocationsInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSampleLocationsInfoEXT where
        sizeOf ~_ = #{size VkSampleLocationsInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSampleLocationsInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSampleLocationsInfoEXT where
        unsafeAddr (VkSampleLocationsInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSampleLocationsInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSampleLocationsInfoEXT## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSampleLocationsInfoEXT where
        type StructFields VkSampleLocationsInfoEXT =
             '["sType", "pNext", "sampleLocationsPerPixel", -- ' closing tick for hsc2hs
               "sampleLocationGridSize", "sampleLocationsCount",
               "pSampleLocations"]
        type CUnionType VkSampleLocationsInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSampleLocationsInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSampleLocationsInfoEXT =
             '[VkImageMemoryBarrier] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSampleLocationsInfoEXT where
        type FieldType "sType" VkSampleLocationsInfoEXT = VkStructureType
        type FieldOptional "sType" VkSampleLocationsInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSampleLocationsInfoEXT =
             #{offset VkSampleLocationsInfoEXT, sType}
        type FieldIsArray "sType" VkSampleLocationsInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSampleLocationsInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSampleLocationsInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSampleLocationsInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSampleLocationsInfoEXT where
        type FieldType "pNext" VkSampleLocationsInfoEXT = Ptr Void
        type FieldOptional "pNext" VkSampleLocationsInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSampleLocationsInfoEXT =
             #{offset VkSampleLocationsInfoEXT, pNext}
        type FieldIsArray "pNext" VkSampleLocationsInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSampleLocationsInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSampleLocationsInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSampleLocationsInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsPerPixel" VkSampleLocationsInfoEXT where
        type FieldType "sampleLocationsPerPixel" VkSampleLocationsInfoEXT =
             VkSampleCountFlagBits
        type FieldOptional "sampleLocationsPerPixel"
               VkSampleLocationsInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsPerPixel" VkSampleLocationsInfoEXT
             =
             #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}
        type FieldIsArray "sampleLocationsPerPixel"
               VkSampleLocationsInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationsPerPixel" VkSampleLocationsInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationsPerPixel" VkSampleLocationsInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationGridSize" VkSampleLocationsInfoEXT where
        type FieldType "sampleLocationGridSize" VkSampleLocationsInfoEXT =
             VkExtent2D
        type FieldOptional "sampleLocationGridSize"
               VkSampleLocationsInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationGridSize" VkSampleLocationsInfoEXT
             =
             #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}
        type FieldIsArray "sampleLocationGridSize" VkSampleLocationsInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationGridSize" VkSampleLocationsInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationGridSize" VkSampleLocationsInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsCount" VkSampleLocationsInfoEXT where
        type FieldType "sampleLocationsCount" VkSampleLocationsInfoEXT =
             Word32
        type FieldOptional "sampleLocationsCount" VkSampleLocationsInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsCount" VkSampleLocationsInfoEXT =
             #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}
        type FieldIsArray "sampleLocationsCount" VkSampleLocationsInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationsCount" VkSampleLocationsInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sampleLocationsCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationsCount" VkSampleLocationsInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasField "pSampleLocations" VkSampleLocationsInfoEXT where
        type FieldType "pSampleLocations" VkSampleLocationsInfoEXT =
             Ptr VkSampleLocationEXT
        type FieldOptional "pSampleLocations" VkSampleLocationsInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pSampleLocations" VkSampleLocationsInfoEXT =
             #{offset VkSampleLocationsInfoEXT, pSampleLocations}
        type FieldIsArray "pSampleLocations" VkSampleLocationsInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSampleLocationsInfoEXT, pSampleLocations}

instance {-# OVERLAPPING #-}
         CanReadField "pSampleLocations" VkSampleLocationsInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, pSampleLocations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, pSampleLocations}

instance {-# OVERLAPPING #-}
         CanWriteField "pSampleLocations" VkSampleLocationsInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, pSampleLocations}

instance Show VkSampleLocationsInfoEXT where
        showsPrec d x
          = showString "VkSampleLocationsInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "sampleLocationsPerPixel = " .
                            showsPrec d (getField @"sampleLocationsPerPixel" x) .
                              showString ", " .
                                showString "sampleLocationGridSize = " .
                                  showsPrec d (getField @"sampleLocationGridSize" x) .
                                    showString ", " .
                                      showString "sampleLocationsCount = " .
                                        showsPrec d (getField @"sampleLocationsCount" x) .
                                          showString ", " .
                                            showString "pSampleLocations = " .
                                              showsPrec d (getField @"pSampleLocations" x) .
                                                showChar '}'
