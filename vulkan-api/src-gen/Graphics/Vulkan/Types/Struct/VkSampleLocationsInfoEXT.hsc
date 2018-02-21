#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSampleLocationsInfoEXT
       (VkSampleLocationsInfoEXT(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags     (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkExtent2D           (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.VkImageMemoryBarrier (VkImageMemoryBarrier)
import           Graphics.Vulkan.Types.Struct.VkSampleLocationEXT  (VkSampleLocationEXT)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkSampleLocationsInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSampleCountFlagBits            sampleLocationsPerPixel;
--   >     VkExtent2D                       sampleLocationGridSize;
--   >     uint32_t                         sampleLocationsCount;
--   >     const VkSampleLocationEXT* pSampleLocations;
--   > } VkSampleLocationsInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSampleLocationsInfoEXT.html VkSampleLocationsInfoEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkSampleLocationsInfoEXT
         where
        type VkSTypeMType VkSampleLocationsInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sType}

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

instance CanReadField "sType" VkSampleLocationsInfoEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSampleLocationsInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSampleLocationsInfoEXT
         where
        type VkPNextMType VkSampleLocationsInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, pNext}

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

instance CanReadField "pNext" VkSampleLocationsInfoEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSampleLocationsInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsPerPixel VkSampleLocationsInfoEXT where
        type VkSampleLocationsPerPixelMType VkSampleLocationsInfoEXT =
             VkSampleCountFlagBits

        {-# NOINLINE vkSampleLocationsPerPixel #-}
        vkSampleLocationsPerPixel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel})

        {-# INLINE vkSampleLocationsPerPixelByteOffset #-}
        vkSampleLocationsPerPixelByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}

        {-# INLINE readVkSampleLocationsPerPixel #-}
        readVkSampleLocationsPerPixel p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}

        {-# INLINE writeVkSampleLocationsPerPixel #-}
        writeVkSampleLocationsPerPixel p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}

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

instance CanReadField "sampleLocationsPerPixel"
           VkSampleLocationsInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationsPerPixel

        {-# INLINE readField #-}
        readField = readVkSampleLocationsPerPixel

instance CanWriteField "sampleLocationsPerPixel"
           VkSampleLocationsInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationsPerPixel

instance {-# OVERLAPPING #-}
         HasVkSampleLocationGridSize VkSampleLocationsInfoEXT where
        type VkSampleLocationGridSizeMType VkSampleLocationsInfoEXT =
             VkExtent2D

        {-# NOINLINE vkSampleLocationGridSize #-}
        vkSampleLocationGridSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize})

        {-# INLINE vkSampleLocationGridSizeByteOffset #-}
        vkSampleLocationGridSizeByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}

        {-# INLINE readVkSampleLocationGridSize #-}
        readVkSampleLocationGridSize p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}

        {-# INLINE writeVkSampleLocationGridSize #-}
        writeVkSampleLocationGridSize p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}

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

instance CanReadField "sampleLocationGridSize"
           VkSampleLocationsInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationGridSize

        {-# INLINE readField #-}
        readField = readVkSampleLocationGridSize

instance CanWriteField "sampleLocationGridSize"
           VkSampleLocationsInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationGridSize

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsCount VkSampleLocationsInfoEXT where
        type VkSampleLocationsCountMType VkSampleLocationsInfoEXT = Word32

        {-# NOINLINE vkSampleLocationsCount #-}
        vkSampleLocationsCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sampleLocationsCount})

        {-# INLINE vkSampleLocationsCountByteOffset #-}
        vkSampleLocationsCountByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}

        {-# INLINE readVkSampleLocationsCount #-}
        readVkSampleLocationsCount p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}

        {-# INLINE writeVkSampleLocationsCount #-}
        writeVkSampleLocationsCount p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}

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

instance CanReadField "sampleLocationsCount"
           VkSampleLocationsInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationsCount

        {-# INLINE readField #-}
        readField = readVkSampleLocationsCount

instance CanWriteField "sampleLocationsCount"
           VkSampleLocationsInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationsCount

instance {-# OVERLAPPING #-}
         HasVkPSampleLocations VkSampleLocationsInfoEXT where
        type VkPSampleLocationsMType VkSampleLocationsInfoEXT =
             Ptr VkSampleLocationEXT

        {-# NOINLINE vkPSampleLocations #-}
        vkPSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, pSampleLocations})

        {-# INLINE vkPSampleLocationsByteOffset #-}
        vkPSampleLocationsByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, pSampleLocations}

        {-# INLINE readVkPSampleLocations #-}
        readVkPSampleLocations p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, pSampleLocations}

        {-# INLINE writeVkPSampleLocations #-}
        writeVkPSampleLocations p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, pSampleLocations}

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

instance CanReadField "pSampleLocations" VkSampleLocationsInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPSampleLocations

        {-# INLINE readField #-}
        readField = readVkPSampleLocations

instance CanWriteField "pSampleLocations" VkSampleLocationsInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPSampleLocations

instance Show VkSampleLocationsInfoEXT where
        showsPrec d x
          = showString "VkSampleLocationsInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSampleLocationsPerPixel = " .
                            showsPrec d (vkSampleLocationsPerPixel x) .
                              showString ", " .
                                showString "vkSampleLocationGridSize = " .
                                  showsPrec d (vkSampleLocationGridSize x) .
                                    showString ", " .
                                      showString "vkSampleLocationsCount = " .
                                        showsPrec d (vkSampleLocationsCount x) .
                                          showString ", " .
                                            showString "vkPSampleLocations = " .
                                              showsPrec d (vkPSampleLocations x) . showChar '}'
