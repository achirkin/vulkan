#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_sample_locations
       (-- * Vulkan extension: @VK_EXT_sample_locations@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @aqnuep@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @144@
        VkSampleLocationEXT(..), VkSampleLocationsInfoEXT(..),
        VkAttachmentSampleLocationsEXT(..),
        VkSubpassSampleLocationsEXT(..),
        VkRenderPassSampleLocationsBeginInfoEXT(..),
        VkPipelineSampleLocationsStateCreateInfoEXT(..),
        VkPhysicalDeviceSampleLocationsPropertiesEXT(..),
        VkMultisamplePropertiesEXT(..), vkCmdSetSampleLocationsEXT,
        vkGetPhysicalDeviceMultisamplePropertiesEXT,
        VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION,
        pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION,
        VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME,
        pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME,
        pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT,
        pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT,
        pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT)
       where
import           Foreign.C.String                                           (CString)
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           GHC.Ptr                                                    (Ptr (..))
import           GHC.TypeLits                                               (KnownNat,
                                                                             natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Base                                       (VkExtent2D,
                                                                             VkPipelineMultisampleStateCreateInfo,
                                                                             VkRenderPassBeginInfo)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Core                                       (VkImageMemoryBarrier)
import           Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2 (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkSampleLocationEXT {
--   >     float                            x;
--   >     float                            y;
--   > } VkSampleLocationEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSampleLocationEXT.html VkSampleLocationEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkX VkSampleLocationEXT where
        type VkXMType VkSampleLocationEXT = #{type float}

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationEXT, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkSampleLocationEXT, x}

        {-# INLINE readVkX #-}
        readVkX p
          = peekByteOff p #{offset VkSampleLocationEXT, x}

        {-# INLINE writeVkX #-}
        writeVkX p
          = pokeByteOff p #{offset VkSampleLocationEXT, x}

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

instance CanReadField "x" VkSampleLocationEXT where
        {-# INLINE getField #-}
        getField = vkX

        {-# INLINE readField #-}
        readField = readVkX

instance CanWriteField "x" VkSampleLocationEXT where
        {-# INLINE writeField #-}
        writeField = writeVkX

instance {-# OVERLAPPING #-} HasVkY VkSampleLocationEXT where
        type VkYMType VkSampleLocationEXT = #{type float}

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationEXT, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkSampleLocationEXT, y}

        {-# INLINE readVkY #-}
        readVkY p
          = peekByteOff p #{offset VkSampleLocationEXT, y}

        {-# INLINE writeVkY #-}
        writeVkY p
          = pokeByteOff p #{offset VkSampleLocationEXT, y}

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

instance CanReadField "y" VkSampleLocationEXT where
        {-# INLINE getField #-}
        getField = vkY

        {-# INLINE readField #-}
        readField = readVkY

instance CanWriteField "y" VkSampleLocationEXT where
        {-# INLINE writeField #-}
        writeField = writeVkY

instance Show VkSampleLocationEXT where
        showsPrec d x
          = showString "VkSampleLocationEXT {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " . showsPrec d (vkY x) . showChar '}'

-- | > typedef struct VkSampleLocationsInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSampleCountFlagBits            sampleLocationsPerPixel;
--   >     VkExtent2D                       sampleLocationGridSize;
--   >     uint32_t                         sampleLocationsCount;
--   >     const VkSampleLocationEXT* pSampleLocations;
--   > } VkSampleLocationsInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSampleLocationsInfoEXT.html VkSampleLocationsInfoEXT registry at www.khronos.org>
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

-- | > typedef struct VkAttachmentSampleLocationsEXT {
--   >     uint32_t                         attachmentIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkAttachmentSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentSampleLocationsEXT.html VkAttachmentSampleLocationsEXT registry at www.khronos.org>
data VkAttachmentSampleLocationsEXT = VkAttachmentSampleLocationsEXT## Addr##
                                                                      ByteArray##

instance Eq VkAttachmentSampleLocationsEXT where
        (VkAttachmentSampleLocationsEXT## a _) ==
          x@(VkAttachmentSampleLocationsEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAttachmentSampleLocationsEXT where
        (VkAttachmentSampleLocationsEXT## a _) `compare`
          x@(VkAttachmentSampleLocationsEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAttachmentSampleLocationsEXT where
        sizeOf ~_ = #{size VkAttachmentSampleLocationsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAttachmentSampleLocationsEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAttachmentSampleLocationsEXT where
        unsafeAddr (VkAttachmentSampleLocationsEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAttachmentSampleLocationsEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAttachmentSampleLocationsEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAttachmentSampleLocationsEXT where
        type StructFields VkAttachmentSampleLocationsEXT =
             '["attachmentIndex", "sampleLocationsInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkAttachmentSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAttachmentSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAttachmentSampleLocationsEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkAttachmentIndex VkAttachmentSampleLocationsEXT where
        type VkAttachmentIndexMType VkAttachmentSampleLocationsEXT = Word32

        {-# NOINLINE vkAttachmentIndex #-}
        vkAttachmentIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentSampleLocationsEXT, attachmentIndex})

        {-# INLINE vkAttachmentIndexByteOffset #-}
        vkAttachmentIndexByteOffset ~_
          = #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

        {-# INLINE readVkAttachmentIndex #-}
        readVkAttachmentIndex p
          = peekByteOff p #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

        {-# INLINE writeVkAttachmentIndex #-}
        writeVkAttachmentIndex p
          = pokeByteOff p #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

instance {-# OVERLAPPING #-}
         HasField "attachmentIndex" VkAttachmentSampleLocationsEXT where
        type FieldType "attachmentIndex" VkAttachmentSampleLocationsEXT =
             Word32
        type FieldOptional "attachmentIndex" VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "attachmentIndex" VkAttachmentSampleLocationsEXT =
             #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}
        type FieldIsArray "attachmentIndex" VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

instance CanReadField "attachmentIndex"
           VkAttachmentSampleLocationsEXT
         where
        {-# INLINE getField #-}
        getField = vkAttachmentIndex

        {-# INLINE readField #-}
        readField = readVkAttachmentIndex

instance CanWriteField "attachmentIndex"
           VkAttachmentSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAttachmentIndex

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsInfo VkAttachmentSampleLocationsEXT where
        type VkSampleLocationsInfoMType VkAttachmentSampleLocationsEXT =
             VkSampleLocationsInfoEXT

        {-# NOINLINE vkSampleLocationsInfo #-}
        vkSampleLocationsInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo})

        {-# INLINE vkSampleLocationsInfoByteOffset #-}
        vkSampleLocationsInfoByteOffset ~_
          = #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE readVkSampleLocationsInfo #-}
        readVkSampleLocationsInfo p
          = peekByteOff p #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE writeVkSampleLocationsInfo #-}
        writeVkSampleLocationsInfo p
          = pokeByteOff p #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsInfo" VkAttachmentSampleLocationsEXT where
        type FieldType "sampleLocationsInfo" VkAttachmentSampleLocationsEXT
             = VkSampleLocationsInfoEXT
        type FieldOptional "sampleLocationsInfo"
               VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsInfo"
               VkAttachmentSampleLocationsEXT
             =
             #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}
        type FieldIsArray "sampleLocationsInfo"
               VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

instance CanReadField "sampleLocationsInfo"
           VkAttachmentSampleLocationsEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationsInfo

        {-# INLINE readField #-}
        readField = readVkSampleLocationsInfo

instance CanWriteField "sampleLocationsInfo"
           VkAttachmentSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationsInfo

instance Show VkAttachmentSampleLocationsEXT where
        showsPrec d x
          = showString "VkAttachmentSampleLocationsEXT {" .
              showString "vkAttachmentIndex = " .
                showsPrec d (vkAttachmentIndex x) .
                  showString ", " .
                    showString "vkSampleLocationsInfo = " .
                      showsPrec d (vkSampleLocationsInfo x) . showChar '}'

-- | > typedef struct VkSubpassSampleLocationsEXT {
--   >     uint32_t                         subpassIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkSubpassSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubpassSampleLocationsEXT.html VkSubpassSampleLocationsEXT registry at www.khronos.org>
data VkSubpassSampleLocationsEXT = VkSubpassSampleLocationsEXT## Addr##
                                                                ByteArray##

instance Eq VkSubpassSampleLocationsEXT where
        (VkSubpassSampleLocationsEXT## a _) ==
          x@(VkSubpassSampleLocationsEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSubpassSampleLocationsEXT where
        (VkSubpassSampleLocationsEXT## a _) `compare`
          x@(VkSubpassSampleLocationsEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSubpassSampleLocationsEXT where
        sizeOf ~_ = #{size VkSubpassSampleLocationsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSubpassSampleLocationsEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSubpassSampleLocationsEXT where
        unsafeAddr (VkSubpassSampleLocationsEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSubpassSampleLocationsEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSubpassSampleLocationsEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSubpassSampleLocationsEXT where
        type StructFields VkSubpassSampleLocationsEXT =
             '["subpassIndex", "sampleLocationsInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkSubpassSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSubpassSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSubpassSampleLocationsEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSubpassIndex VkSubpassSampleLocationsEXT where
        type VkSubpassIndexMType VkSubpassSampleLocationsEXT = Word32

        {-# NOINLINE vkSubpassIndex #-}
        vkSubpassIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassSampleLocationsEXT, subpassIndex})

        {-# INLINE vkSubpassIndexByteOffset #-}
        vkSubpassIndexByteOffset ~_
          = #{offset VkSubpassSampleLocationsEXT, subpassIndex}

        {-# INLINE readVkSubpassIndex #-}
        readVkSubpassIndex p
          = peekByteOff p #{offset VkSubpassSampleLocationsEXT, subpassIndex}

        {-# INLINE writeVkSubpassIndex #-}
        writeVkSubpassIndex p
          = pokeByteOff p #{offset VkSubpassSampleLocationsEXT, subpassIndex}

instance {-# OVERLAPPING #-}
         HasField "subpassIndex" VkSubpassSampleLocationsEXT where
        type FieldType "subpassIndex" VkSubpassSampleLocationsEXT = Word32
        type FieldOptional "subpassIndex" VkSubpassSampleLocationsEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subpassIndex" VkSubpassSampleLocationsEXT =
             #{offset VkSubpassSampleLocationsEXT, subpassIndex}
        type FieldIsArray "subpassIndex" VkSubpassSampleLocationsEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassSampleLocationsEXT, subpassIndex}

instance CanReadField "subpassIndex" VkSubpassSampleLocationsEXT
         where
        {-# INLINE getField #-}
        getField = vkSubpassIndex

        {-# INLINE readField #-}
        readField = readVkSubpassIndex

instance CanWriteField "subpassIndex" VkSubpassSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubpassIndex

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsInfo VkSubpassSampleLocationsEXT where
        type VkSampleLocationsInfoMType VkSubpassSampleLocationsEXT =
             VkSampleLocationsInfoEXT

        {-# NOINLINE vkSampleLocationsInfo #-}
        vkSampleLocationsInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo})

        {-# INLINE vkSampleLocationsInfoByteOffset #-}
        vkSampleLocationsInfoByteOffset ~_
          = #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE readVkSampleLocationsInfo #-}
        readVkSampleLocationsInfo p
          = peekByteOff p #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE writeVkSampleLocationsInfo #-}
        writeVkSampleLocationsInfo p
          = pokeByteOff p #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsInfo" VkSubpassSampleLocationsEXT where
        type FieldType "sampleLocationsInfo" VkSubpassSampleLocationsEXT =
             VkSampleLocationsInfoEXT
        type FieldOptional "sampleLocationsInfo"
               VkSubpassSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsInfo" VkSubpassSampleLocationsEXT
             =
             #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}
        type FieldIsArray "sampleLocationsInfo" VkSubpassSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

instance CanReadField "sampleLocationsInfo"
           VkSubpassSampleLocationsEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationsInfo

        {-# INLINE readField #-}
        readField = readVkSampleLocationsInfo

instance CanWriteField "sampleLocationsInfo"
           VkSubpassSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationsInfo

instance Show VkSubpassSampleLocationsEXT where
        showsPrec d x
          = showString "VkSubpassSampleLocationsEXT {" .
              showString "vkSubpassIndex = " .
                showsPrec d (vkSubpassIndex x) .
                  showString ", " .
                    showString "vkSampleLocationsInfo = " .
                      showsPrec d (vkSampleLocationsInfo x) . showChar '}'

-- | > typedef struct VkRenderPassSampleLocationsBeginInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         attachmentInitialSampleLocationsCount;
--   >     const VkAttachmentSampleLocationsEXT* pAttachmentInitialSampleLocations;
--   >     uint32_t         postSubpassSampleLocationsCount;
--   >     const VkSubpassSampleLocationsEXT* pPostSubpassSampleLocations;
--   > } VkRenderPassSampleLocationsBeginInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkRenderPassSampleLocationsBeginInfoEXT.html VkRenderPassSampleLocationsBeginInfoEXT registry at www.khronos.org>
data VkRenderPassSampleLocationsBeginInfoEXT = VkRenderPassSampleLocationsBeginInfoEXT## Addr##
                                                                                        ByteArray##

instance Eq VkRenderPassSampleLocationsBeginInfoEXT where
        (VkRenderPassSampleLocationsBeginInfoEXT## a _) ==
          x@(VkRenderPassSampleLocationsBeginInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassSampleLocationsBeginInfoEXT where
        (VkRenderPassSampleLocationsBeginInfoEXT## a _) `compare`
          x@(VkRenderPassSampleLocationsBeginInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassSampleLocationsBeginInfoEXT where
        sizeOf ~_
          = #{size VkRenderPassSampleLocationsBeginInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassSampleLocationsBeginInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassSampleLocationsBeginInfoEXT
         where
        unsafeAddr (VkRenderPassSampleLocationsBeginInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassSampleLocationsBeginInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassSampleLocationsBeginInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassSampleLocationsBeginInfoEXT
         where
        type StructFields VkRenderPassSampleLocationsBeginInfoEXT =
             '["sType", "pNext", "attachmentInitialSampleLocationsCount", -- ' closing tick for hsc2hs
               "pAttachmentInitialSampleLocations",
               "postSubpassSampleLocationsCount", "pPostSubpassSampleLocations"]
        type CUnionType VkRenderPassSampleLocationsBeginInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassSampleLocationsBeginInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassSampleLocationsBeginInfoEXT =
             '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkRenderPassSampleLocationsBeginInfoEXT where
        type VkSTypeMType VkRenderPassSampleLocationsBeginInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassSampleLocationsBeginInfoEXT where
        type FieldType "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}
        type FieldIsArray "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance CanReadField "sType"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkRenderPassSampleLocationsBeginInfoEXT where
        type VkPNextMType VkRenderPassSampleLocationsBeginInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassSampleLocationsBeginInfoEXT where
        type FieldType "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}
        type FieldIsArray "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance CanReadField "pNext"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkAttachmentInitialSampleLocationsCount
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkAttachmentInitialSampleLocationsCountMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32

        {-# NOINLINE vkAttachmentInitialSampleLocationsCount #-}
        vkAttachmentInitialSampleLocationsCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount})

        {-# INLINE vkAttachmentInitialSampleLocationsCountByteOffset #-}
        vkAttachmentInitialSampleLocationsCountByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

        {-# INLINE readVkAttachmentInitialSampleLocationsCount #-}
        readVkAttachmentInitialSampleLocationsCount p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

        {-# INLINE writeVkAttachmentInitialSampleLocationsCount #-}
        writeVkAttachmentInitialSampleLocationsCount p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32
        type FieldOptional "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}
        type FieldIsArray "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance CanReadField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkAttachmentInitialSampleLocationsCount

        {-# INLINE readField #-}
        readField = readVkAttachmentInitialSampleLocationsCount

instance CanWriteField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAttachmentInitialSampleLocationsCount

instance {-# OVERLAPPING #-}
         HasVkPAttachmentInitialSampleLocations
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkPAttachmentInitialSampleLocationsMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkAttachmentSampleLocationsEXT

        {-# NOINLINE vkPAttachmentInitialSampleLocations #-}
        vkPAttachmentInitialSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations})

        {-# INLINE vkPAttachmentInitialSampleLocationsByteOffset #-}
        vkPAttachmentInitialSampleLocationsByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

        {-# INLINE readVkPAttachmentInitialSampleLocations #-}
        readVkPAttachmentInitialSampleLocations p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

        {-# INLINE writeVkPAttachmentInitialSampleLocations #-}
        writeVkPAttachmentInitialSampleLocations p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance {-# OVERLAPPING #-}
         HasField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkAttachmentSampleLocationsEXT
        type FieldOptional "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}
        type FieldIsArray "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance CanReadField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPAttachmentInitialSampleLocations

        {-# INLINE readField #-}
        readField = readVkPAttachmentInitialSampleLocations

instance CanWriteField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPAttachmentInitialSampleLocations

instance {-# OVERLAPPING #-}
         HasVkPostSubpassSampleLocationsCount
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkPostSubpassSampleLocationsCountMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32

        {-# NOINLINE vkPostSubpassSampleLocationsCount #-}
        vkPostSubpassSampleLocationsCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount})

        {-# INLINE vkPostSubpassSampleLocationsCountByteOffset #-}
        vkPostSubpassSampleLocationsCountByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

        {-# INLINE readVkPostSubpassSampleLocationsCount #-}
        readVkPostSubpassSampleLocationsCount p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

        {-# INLINE writeVkPostSubpassSampleLocationsCount #-}
        writeVkPostSubpassSampleLocationsCount p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32
        type FieldOptional "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}
        type FieldIsArray "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance CanReadField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPostSubpassSampleLocationsCount

        {-# INLINE readField #-}
        readField = readVkPostSubpassSampleLocationsCount

instance CanWriteField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPostSubpassSampleLocationsCount

instance {-# OVERLAPPING #-}
         HasVkPPostSubpassSampleLocations
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkPPostSubpassSampleLocationsMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkSubpassSampleLocationsEXT

        {-# NOINLINE vkPPostSubpassSampleLocations #-}
        vkPPostSubpassSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations})

        {-# INLINE vkPPostSubpassSampleLocationsByteOffset #-}
        vkPPostSubpassSampleLocationsByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

        {-# INLINE readVkPPostSubpassSampleLocations #-}
        readVkPPostSubpassSampleLocations p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

        {-# INLINE writeVkPPostSubpassSampleLocations #-}
        writeVkPPostSubpassSampleLocations p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance {-# OVERLAPPING #-}
         HasField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkSubpassSampleLocationsEXT
        type FieldOptional "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}
        type FieldIsArray "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance CanReadField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPPostSubpassSampleLocations

        {-# INLINE readField #-}
        readField = readVkPPostSubpassSampleLocations

instance CanWriteField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPPostSubpassSampleLocations

instance Show VkRenderPassSampleLocationsBeginInfoEXT where
        showsPrec d x
          = showString "VkRenderPassSampleLocationsBeginInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAttachmentInitialSampleLocationsCount = " .
                            showsPrec d (vkAttachmentInitialSampleLocationsCount x) .
                              showString ", " .
                                showString "vkPAttachmentInitialSampleLocations = " .
                                  showsPrec d (vkPAttachmentInitialSampleLocations x) .
                                    showString ", " .
                                      showString "vkPostSubpassSampleLocationsCount = " .
                                        showsPrec d (vkPostSubpassSampleLocationsCount x) .
                                          showString ", " .
                                            showString "vkPPostSubpassSampleLocations = " .
                                              showsPrec d (vkPPostSubpassSampleLocations x) .
                                                showChar '}'

-- | > typedef struct VkPipelineSampleLocationsStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         sampleLocationsEnable;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkPipelineSampleLocationsStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineSampleLocationsStateCreateInfoEXT.html VkPipelineSampleLocationsStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineSampleLocationsStateCreateInfoEXT = VkPipelineSampleLocationsStateCreateInfoEXT## Addr##
                                                                                                ByteArray##

instance Eq VkPipelineSampleLocationsStateCreateInfoEXT where
        (VkPipelineSampleLocationsStateCreateInfoEXT## a _) ==
          x@(VkPipelineSampleLocationsStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineSampleLocationsStateCreateInfoEXT where
        (VkPipelineSampleLocationsStateCreateInfoEXT## a _) `compare`
          x@(VkPipelineSampleLocationsStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineSampleLocationsStateCreateInfoEXT where
        sizeOf ~_
          = #{size VkPipelineSampleLocationsStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineSampleLocationsStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        unsafeAddr (VkPipelineSampleLocationsStateCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineSampleLocationsStateCreateInfoEXT## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineSampleLocationsStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type StructFields VkPipelineSampleLocationsStateCreateInfoEXT =
             '["sType", "pNext", "sampleLocationsEnable", "sampleLocationsInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineSampleLocationsStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineSampleLocationsStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineSampleLocationsStateCreateInfoEXT =
             '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineSampleLocationsStateCreateInfoEXT where
        type VkSTypeMType VkPipelineSampleLocationsStateCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineSampleLocationsStateCreateInfoEXT where
        type FieldType "sType" VkPipelineSampleLocationsStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

instance CanReadField "sType"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineSampleLocationsStateCreateInfoEXT where
        type VkPNextMType VkPipelineSampleLocationsStateCreateInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineSampleLocationsStateCreateInfoEXT where
        type FieldType "pNext" VkPipelineSampleLocationsStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

instance CanReadField "pNext"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsEnable
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type VkSampleLocationsEnableMType
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkBool32

        {-# NOINLINE vkSampleLocationsEnable #-}
        vkSampleLocationsEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable})

        {-# INLINE vkSampleLocationsEnableByteOffset #-}
        vkSampleLocationsEnableByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

        {-# INLINE readVkSampleLocationsEnable #-}
        readVkSampleLocationsEnable p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

        {-# INLINE writeVkSampleLocationsEnable #-}
        writeVkSampleLocationsEnable p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type FieldType "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkBool32
        type FieldOptional "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}
        type FieldIsArray "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

instance CanReadField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationsEnable

        {-# INLINE readField #-}
        readField = readVkSampleLocationsEnable

instance CanWriteField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationsEnable

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsInfo
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type VkSampleLocationsInfoMType
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkSampleLocationsInfoEXT

        {-# NOINLINE vkSampleLocationsInfo #-}
        vkSampleLocationsInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo})

        {-# INLINE vkSampleLocationsInfoByteOffset #-}
        vkSampleLocationsInfoByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

        {-# INLINE readVkSampleLocationsInfo #-}
        readVkSampleLocationsInfo p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

        {-# INLINE writeVkSampleLocationsInfo #-}
        writeVkSampleLocationsInfo p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type FieldType "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkSampleLocationsInfoEXT
        type FieldOptional "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}
        type FieldIsArray "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

instance CanReadField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationsInfo

        {-# INLINE readField #-}
        readField = readVkSampleLocationsInfo

instance CanWriteField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationsInfo

instance Show VkPipelineSampleLocationsStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineSampleLocationsStateCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSampleLocationsEnable = " .
                            showsPrec d (vkSampleLocationsEnable x) .
                              showString ", " .
                                showString "vkSampleLocationsInfo = " .
                                  showsPrec d (vkSampleLocationsInfo x) . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceSampleLocationsPropertiesEXT.html VkPhysicalDeviceSampleLocationsPropertiesEXT registry at www.khronos.org>
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

-- | > typedef struct VkMultisamplePropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExtent2D                       maxSampleLocationGridSize;
--   > } VkMultisamplePropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMultisamplePropertiesEXT.html VkMultisamplePropertiesEXT registry at www.khronos.org>
data VkMultisamplePropertiesEXT = VkMultisamplePropertiesEXT## Addr##
                                                              ByteArray##

instance Eq VkMultisamplePropertiesEXT where
        (VkMultisamplePropertiesEXT## a _) ==
          x@(VkMultisamplePropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMultisamplePropertiesEXT where
        (VkMultisamplePropertiesEXT## a _) `compare`
          x@(VkMultisamplePropertiesEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMultisamplePropertiesEXT where
        sizeOf ~_ = #{size VkMultisamplePropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMultisamplePropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMultisamplePropertiesEXT where
        unsafeAddr (VkMultisamplePropertiesEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMultisamplePropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMultisamplePropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMultisamplePropertiesEXT where
        type StructFields VkMultisamplePropertiesEXT =
             '["sType", "pNext", "maxSampleLocationGridSize"] -- ' closing tick for hsc2hs
        type CUnionType VkMultisamplePropertiesEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMultisamplePropertiesEXT = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMultisamplePropertiesEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkMultisamplePropertiesEXT
         where
        type VkSTypeMType VkMultisamplePropertiesEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMultisamplePropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMultisamplePropertiesEXT where
        type FieldType "sType" VkMultisamplePropertiesEXT = VkStructureType
        type FieldOptional "sType" VkMultisamplePropertiesEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMultisamplePropertiesEXT =
             #{offset VkMultisamplePropertiesEXT, sType}
        type FieldIsArray "sType" VkMultisamplePropertiesEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMultisamplePropertiesEXT, sType}

instance CanReadField "sType" VkMultisamplePropertiesEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMultisamplePropertiesEXT
         where
        type VkPNextMType VkMultisamplePropertiesEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMultisamplePropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMultisamplePropertiesEXT where
        type FieldType "pNext" VkMultisamplePropertiesEXT = Ptr Void
        type FieldOptional "pNext" VkMultisamplePropertiesEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMultisamplePropertiesEXT =
             #{offset VkMultisamplePropertiesEXT, pNext}
        type FieldIsArray "pNext" VkMultisamplePropertiesEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMultisamplePropertiesEXT, pNext}

instance CanReadField "pNext" VkMultisamplePropertiesEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkMaxSampleLocationGridSize VkMultisamplePropertiesEXT where
        type VkMaxSampleLocationGridSizeMType VkMultisamplePropertiesEXT =
             VkExtent2D

        {-# NOINLINE vkMaxSampleLocationGridSize #-}
        vkMaxSampleLocationGridSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize})

        {-# INLINE vkMaxSampleLocationGridSizeByteOffset #-}
        vkMaxSampleLocationGridSizeByteOffset ~_
          = #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

        {-# INLINE readVkMaxSampleLocationGridSize #-}
        readVkMaxSampleLocationGridSize p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

        {-# INLINE writeVkMaxSampleLocationGridSize #-}
        writeVkMaxSampleLocationGridSize p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

instance {-# OVERLAPPING #-}
         HasField "maxSampleLocationGridSize" VkMultisamplePropertiesEXT
         where
        type FieldType "maxSampleLocationGridSize"
               VkMultisamplePropertiesEXT
             = VkExtent2D
        type FieldOptional "maxSampleLocationGridSize"
               VkMultisamplePropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSampleLocationGridSize"
               VkMultisamplePropertiesEXT
             =
             #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}
        type FieldIsArray "maxSampleLocationGridSize"
               VkMultisamplePropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

instance CanReadField "maxSampleLocationGridSize"
           VkMultisamplePropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkMaxSampleLocationGridSize

        {-# INLINE readField #-}
        readField = readVkMaxSampleLocationGridSize

instance Show VkMultisamplePropertiesEXT where
        showsPrec d x
          = showString "VkMultisamplePropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMaxSampleLocationGridSize = " .
                            showsPrec d (vkMaxSampleLocationGridSize x) . showChar '}'

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetSampleLocationsEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkSampleLocationsInfoEXT* pSampleLocationsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetSampleLocationsEXT.html vkCmdSetSampleLocationsEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetSampleLocationsEXT"
               vkCmdSetSampleLocationsEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkSampleLocationsInfoEXT -- ^ pSampleLocationsInfo
                                                               -> IO ()

-- | > void vkGetPhysicalDeviceMultisamplePropertiesEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSampleCountFlagBits samples
--   >     , VkMultisamplePropertiesEXT* pMultisampleProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceMultisamplePropertiesEXT.html vkGetPhysicalDeviceMultisamplePropertiesEXT registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceMultisamplePropertiesEXT"
               vkGetPhysicalDeviceMultisamplePropertiesEXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSampleCountFlagBits -- ^ samples
                                       -> Ptr VkMultisamplePropertiesEXT -- ^ pMultisampleProperties
                                                                         -> IO ()

pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1

type VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1

pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString

pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME <-
        (is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME -> True)
  where VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
          = _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME

{-# INLINE _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME #-}

_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString
_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  = Ptr "VK_EXT_sample_locations\NUL"##

{-# INLINE is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME #-}

is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  = eqCStrings _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME

type VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME =
     "VK_EXT_sample_locations"

-- | bitpos = @12@
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
        :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT =
        VkImageCreateFlagBits 4096

pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT =
        VkStructureType 1000143000

pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
        = VkStructureType 1000143001

pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
        = VkStructureType 1000143002

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
        = VkStructureType 1000143003

pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT =
        VkStructureType 1000143004

pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT =
        VkDynamicState 1000143000
