#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_hdr_metadata
       (-- * Vulkan extension: @VK_EXT_hdr_metadata@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Courtney Goeltzenleuchter @courtneygo@
        --
        -- author: @GOOGLE@
        --
        -- type: @device@
        --
        -- Extension number: @106@
        --
        -- Required extensions: 'VK_KHR_swapchain'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain'.
        VkHdrMetadataEXT(..), VkXYColorEXT(..), vkSetHdrMetadataEXT,
        VK_EXT_HDR_METADATA_SPEC_VERSION,
        pattern VK_EXT_HDR_METADATA_SPEC_VERSION,
        VK_EXT_HDR_METADATA_EXTENSION_NAME,
        pattern VK_EXT_HDR_METADATA_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkHdrMetadataEXT {
--   >     VkStructureType sType;
--   >     const void*    pNext;
--   >     VkXYColorEXT   displayPrimaryRed;
--   >     VkXYColorEXT   displayPrimaryGreen;
--   >     VkXYColorEXT   displayPrimaryBlue;
--   >     VkXYColorEXT   whitePoint;
--   >     float          maxLuminance;
--   >     float          minLuminance;
--   >     float          maxContentLightLevel;
--   >     float          maxFrameAverageLightLevel;
--   > } VkHdrMetadataEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkHdrMetadataEXT.html VkHdrMetadataEXT registry at www.khronos.org>
data VkHdrMetadataEXT = VkHdrMetadataEXT## Addr## ByteArray##

instance Eq VkHdrMetadataEXT where
        (VkHdrMetadataEXT## a _) == x@(VkHdrMetadataEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkHdrMetadataEXT where
        (VkHdrMetadataEXT## a _) `compare` x@(VkHdrMetadataEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkHdrMetadataEXT where
        sizeOf ~_ = #{size VkHdrMetadataEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkHdrMetadataEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkHdrMetadataEXT where
        unsafeAddr (VkHdrMetadataEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkHdrMetadataEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkHdrMetadataEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkHdrMetadataEXT where
        type StructFields VkHdrMetadataEXT =
             '["sType", "pNext", "displayPrimaryRed", "displayPrimaryGreen", -- ' closing tick for hsc2hs
               "displayPrimaryBlue", "whitePoint", "maxLuminance", "minLuminance",
               "maxContentLightLevel", "maxFrameAverageLightLevel"]
        type CUnionType VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkHdrMetadataEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkHdrMetadataEXT where
        type VkSTypeMType VkHdrMetadataEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkHdrMetadataEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkHdrMetadataEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkHdrMetadataEXT, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkHdrMetadataEXT
         where
        type FieldType "sType" VkHdrMetadataEXT = VkStructureType
        type FieldOptional "sType" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkHdrMetadataEXT =
             #{offset VkHdrMetadataEXT, sType}
        type FieldIsArray "sType" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkHdrMetadataEXT, sType}

instance CanReadField "sType" VkHdrMetadataEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkHdrMetadataEXT where
        type VkPNextMType VkHdrMetadataEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkHdrMetadataEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkHdrMetadataEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkHdrMetadataEXT, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkHdrMetadataEXT
         where
        type FieldType "pNext" VkHdrMetadataEXT = Ptr Void
        type FieldOptional "pNext" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkHdrMetadataEXT =
             #{offset VkHdrMetadataEXT, pNext}
        type FieldIsArray "pNext" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkHdrMetadataEXT, pNext}

instance CanReadField "pNext" VkHdrMetadataEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDisplayPrimaryRed VkHdrMetadataEXT where
        type VkDisplayPrimaryRedMType VkHdrMetadataEXT = VkXYColorEXT

        {-# NOINLINE vkDisplayPrimaryRed #-}
        vkDisplayPrimaryRed x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, displayPrimaryRed})

        {-# INLINE vkDisplayPrimaryRedByteOffset #-}
        vkDisplayPrimaryRedByteOffset ~_
          = #{offset VkHdrMetadataEXT, displayPrimaryRed}

        {-# INLINE readVkDisplayPrimaryRed #-}
        readVkDisplayPrimaryRed p
          = peekByteOff p #{offset VkHdrMetadataEXT, displayPrimaryRed}

        {-# INLINE writeVkDisplayPrimaryRed #-}
        writeVkDisplayPrimaryRed p
          = pokeByteOff p #{offset VkHdrMetadataEXT, displayPrimaryRed}

instance {-# OVERLAPPING #-}
         HasField "displayPrimaryRed" VkHdrMetadataEXT where
        type FieldType "displayPrimaryRed" VkHdrMetadataEXT = VkXYColorEXT
        type FieldOptional "displayPrimaryRed" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "displayPrimaryRed" VkHdrMetadataEXT =
             #{offset VkHdrMetadataEXT, displayPrimaryRed}
        type FieldIsArray "displayPrimaryRed" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkHdrMetadataEXT, displayPrimaryRed}

instance CanReadField "displayPrimaryRed" VkHdrMetadataEXT where
        {-# INLINE getField #-}
        getField = vkDisplayPrimaryRed

        {-# INLINE readField #-}
        readField = readVkDisplayPrimaryRed

instance CanWriteField "displayPrimaryRed" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField = writeVkDisplayPrimaryRed

instance {-# OVERLAPPING #-}
         HasVkDisplayPrimaryGreen VkHdrMetadataEXT where
        type VkDisplayPrimaryGreenMType VkHdrMetadataEXT = VkXYColorEXT

        {-# NOINLINE vkDisplayPrimaryGreen #-}
        vkDisplayPrimaryGreen x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, displayPrimaryGreen})

        {-# INLINE vkDisplayPrimaryGreenByteOffset #-}
        vkDisplayPrimaryGreenByteOffset ~_
          = #{offset VkHdrMetadataEXT, displayPrimaryGreen}

        {-# INLINE readVkDisplayPrimaryGreen #-}
        readVkDisplayPrimaryGreen p
          = peekByteOff p #{offset VkHdrMetadataEXT, displayPrimaryGreen}

        {-# INLINE writeVkDisplayPrimaryGreen #-}
        writeVkDisplayPrimaryGreen p
          = pokeByteOff p #{offset VkHdrMetadataEXT, displayPrimaryGreen}

instance {-# OVERLAPPING #-}
         HasField "displayPrimaryGreen" VkHdrMetadataEXT where
        type FieldType "displayPrimaryGreen" VkHdrMetadataEXT =
             VkXYColorEXT
        type FieldOptional "displayPrimaryGreen" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "displayPrimaryGreen" VkHdrMetadataEXT =
             #{offset VkHdrMetadataEXT, displayPrimaryGreen}
        type FieldIsArray "displayPrimaryGreen" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkHdrMetadataEXT, displayPrimaryGreen}

instance CanReadField "displayPrimaryGreen" VkHdrMetadataEXT where
        {-# INLINE getField #-}
        getField = vkDisplayPrimaryGreen

        {-# INLINE readField #-}
        readField = readVkDisplayPrimaryGreen

instance CanWriteField "displayPrimaryGreen" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField = writeVkDisplayPrimaryGreen

instance {-# OVERLAPPING #-}
         HasVkDisplayPrimaryBlue VkHdrMetadataEXT where
        type VkDisplayPrimaryBlueMType VkHdrMetadataEXT = VkXYColorEXT

        {-# NOINLINE vkDisplayPrimaryBlue #-}
        vkDisplayPrimaryBlue x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, displayPrimaryBlue})

        {-# INLINE vkDisplayPrimaryBlueByteOffset #-}
        vkDisplayPrimaryBlueByteOffset ~_
          = #{offset VkHdrMetadataEXT, displayPrimaryBlue}

        {-# INLINE readVkDisplayPrimaryBlue #-}
        readVkDisplayPrimaryBlue p
          = peekByteOff p #{offset VkHdrMetadataEXT, displayPrimaryBlue}

        {-# INLINE writeVkDisplayPrimaryBlue #-}
        writeVkDisplayPrimaryBlue p
          = pokeByteOff p #{offset VkHdrMetadataEXT, displayPrimaryBlue}

instance {-# OVERLAPPING #-}
         HasField "displayPrimaryBlue" VkHdrMetadataEXT where
        type FieldType "displayPrimaryBlue" VkHdrMetadataEXT = VkXYColorEXT
        type FieldOptional "displayPrimaryBlue" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "displayPrimaryBlue" VkHdrMetadataEXT =
             #{offset VkHdrMetadataEXT, displayPrimaryBlue}
        type FieldIsArray "displayPrimaryBlue" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkHdrMetadataEXT, displayPrimaryBlue}

instance CanReadField "displayPrimaryBlue" VkHdrMetadataEXT where
        {-# INLINE getField #-}
        getField = vkDisplayPrimaryBlue

        {-# INLINE readField #-}
        readField = readVkDisplayPrimaryBlue

instance CanWriteField "displayPrimaryBlue" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField = writeVkDisplayPrimaryBlue

instance {-# OVERLAPPING #-} HasVkWhitePoint VkHdrMetadataEXT where
        type VkWhitePointMType VkHdrMetadataEXT = VkXYColorEXT

        {-# NOINLINE vkWhitePoint #-}
        vkWhitePoint x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, whitePoint})

        {-# INLINE vkWhitePointByteOffset #-}
        vkWhitePointByteOffset ~_
          = #{offset VkHdrMetadataEXT, whitePoint}

        {-# INLINE readVkWhitePoint #-}
        readVkWhitePoint p
          = peekByteOff p #{offset VkHdrMetadataEXT, whitePoint}

        {-# INLINE writeVkWhitePoint #-}
        writeVkWhitePoint p
          = pokeByteOff p #{offset VkHdrMetadataEXT, whitePoint}

instance {-# OVERLAPPING #-} HasField "whitePoint" VkHdrMetadataEXT
         where
        type FieldType "whitePoint" VkHdrMetadataEXT = VkXYColorEXT
        type FieldOptional "whitePoint" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "whitePoint" VkHdrMetadataEXT =
             #{offset VkHdrMetadataEXT, whitePoint}
        type FieldIsArray "whitePoint" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkHdrMetadataEXT, whitePoint}

instance CanReadField "whitePoint" VkHdrMetadataEXT where
        {-# INLINE getField #-}
        getField = vkWhitePoint

        {-# INLINE readField #-}
        readField = readVkWhitePoint

instance CanWriteField "whitePoint" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField = writeVkWhitePoint

instance {-# OVERLAPPING #-} HasVkMaxLuminance VkHdrMetadataEXT
         where
        type VkMaxLuminanceMType VkHdrMetadataEXT =
             #{type float}

        {-# NOINLINE vkMaxLuminance #-}
        vkMaxLuminance x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, maxLuminance})

        {-# INLINE vkMaxLuminanceByteOffset #-}
        vkMaxLuminanceByteOffset ~_
          = #{offset VkHdrMetadataEXT, maxLuminance}

        {-# INLINE readVkMaxLuminance #-}
        readVkMaxLuminance p
          = peekByteOff p #{offset VkHdrMetadataEXT, maxLuminance}

        {-# INLINE writeVkMaxLuminance #-}
        writeVkMaxLuminance p
          = pokeByteOff p #{offset VkHdrMetadataEXT, maxLuminance}

instance {-# OVERLAPPING #-}
         HasField "maxLuminance" VkHdrMetadataEXT where
        type FieldType "maxLuminance" VkHdrMetadataEXT =
             #{type float}
        type FieldOptional "maxLuminance" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxLuminance" VkHdrMetadataEXT =
             #{offset VkHdrMetadataEXT, maxLuminance}
        type FieldIsArray "maxLuminance" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkHdrMetadataEXT, maxLuminance}

instance CanReadField "maxLuminance" VkHdrMetadataEXT where
        {-# INLINE getField #-}
        getField = vkMaxLuminance

        {-# INLINE readField #-}
        readField = readVkMaxLuminance

instance CanWriteField "maxLuminance" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField = writeVkMaxLuminance

instance {-# OVERLAPPING #-} HasVkMinLuminance VkHdrMetadataEXT
         where
        type VkMinLuminanceMType VkHdrMetadataEXT =
             #{type float}

        {-# NOINLINE vkMinLuminance #-}
        vkMinLuminance x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, minLuminance})

        {-# INLINE vkMinLuminanceByteOffset #-}
        vkMinLuminanceByteOffset ~_
          = #{offset VkHdrMetadataEXT, minLuminance}

        {-# INLINE readVkMinLuminance #-}
        readVkMinLuminance p
          = peekByteOff p #{offset VkHdrMetadataEXT, minLuminance}

        {-# INLINE writeVkMinLuminance #-}
        writeVkMinLuminance p
          = pokeByteOff p #{offset VkHdrMetadataEXT, minLuminance}

instance {-# OVERLAPPING #-}
         HasField "minLuminance" VkHdrMetadataEXT where
        type FieldType "minLuminance" VkHdrMetadataEXT =
             #{type float}
        type FieldOptional "minLuminance" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minLuminance" VkHdrMetadataEXT =
             #{offset VkHdrMetadataEXT, minLuminance}
        type FieldIsArray "minLuminance" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkHdrMetadataEXT, minLuminance}

instance CanReadField "minLuminance" VkHdrMetadataEXT where
        {-# INLINE getField #-}
        getField = vkMinLuminance

        {-# INLINE readField #-}
        readField = readVkMinLuminance

instance CanWriteField "minLuminance" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField = writeVkMinLuminance

instance {-# OVERLAPPING #-}
         HasVkMaxContentLightLevel VkHdrMetadataEXT where
        type VkMaxContentLightLevelMType VkHdrMetadataEXT =
             #{type float}

        {-# NOINLINE vkMaxContentLightLevel #-}
        vkMaxContentLightLevel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, maxContentLightLevel})

        {-# INLINE vkMaxContentLightLevelByteOffset #-}
        vkMaxContentLightLevelByteOffset ~_
          = #{offset VkHdrMetadataEXT, maxContentLightLevel}

        {-# INLINE readVkMaxContentLightLevel #-}
        readVkMaxContentLightLevel p
          = peekByteOff p #{offset VkHdrMetadataEXT, maxContentLightLevel}

        {-# INLINE writeVkMaxContentLightLevel #-}
        writeVkMaxContentLightLevel p
          = pokeByteOff p #{offset VkHdrMetadataEXT, maxContentLightLevel}

instance {-# OVERLAPPING #-}
         HasField "maxContentLightLevel" VkHdrMetadataEXT where
        type FieldType "maxContentLightLevel" VkHdrMetadataEXT =
             #{type float}
        type FieldOptional "maxContentLightLevel" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxContentLightLevel" VkHdrMetadataEXT =
             #{offset VkHdrMetadataEXT, maxContentLightLevel}
        type FieldIsArray "maxContentLightLevel" VkHdrMetadataEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkHdrMetadataEXT, maxContentLightLevel}

instance CanReadField "maxContentLightLevel" VkHdrMetadataEXT where
        {-# INLINE getField #-}
        getField = vkMaxContentLightLevel

        {-# INLINE readField #-}
        readField = readVkMaxContentLightLevel

instance CanWriteField "maxContentLightLevel" VkHdrMetadataEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxContentLightLevel

instance {-# OVERLAPPING #-}
         HasVkMaxFrameAverageLightLevel VkHdrMetadataEXT where
        type VkMaxFrameAverageLightLevelMType VkHdrMetadataEXT =
             #{type float}

        {-# NOINLINE vkMaxFrameAverageLightLevel #-}
        vkMaxFrameAverageLightLevel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel})

        {-# INLINE vkMaxFrameAverageLightLevelByteOffset #-}
        vkMaxFrameAverageLightLevelByteOffset ~_
          = #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}

        {-# INLINE readVkMaxFrameAverageLightLevel #-}
        readVkMaxFrameAverageLightLevel p
          = peekByteOff p #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}

        {-# INLINE writeVkMaxFrameAverageLightLevel #-}
        writeVkMaxFrameAverageLightLevel p
          = pokeByteOff p #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}

instance {-# OVERLAPPING #-}
         HasField "maxFrameAverageLightLevel" VkHdrMetadataEXT where
        type FieldType "maxFrameAverageLightLevel" VkHdrMetadataEXT =
             #{type float}
        type FieldOptional "maxFrameAverageLightLevel" VkHdrMetadataEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxFrameAverageLightLevel" VkHdrMetadataEXT =
             #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}
        type FieldIsArray "maxFrameAverageLightLevel" VkHdrMetadataEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}

instance CanReadField "maxFrameAverageLightLevel" VkHdrMetadataEXT
         where
        {-# INLINE getField #-}
        getField = vkMaxFrameAverageLightLevel

        {-# INLINE readField #-}
        readField = readVkMaxFrameAverageLightLevel

instance CanWriteField "maxFrameAverageLightLevel" VkHdrMetadataEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxFrameAverageLightLevel

instance Show VkHdrMetadataEXT where
        showsPrec d x
          = showString "VkHdrMetadataEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDisplayPrimaryRed = " .
                            showsPrec d (vkDisplayPrimaryRed x) .
                              showString ", " .
                                showString "vkDisplayPrimaryGreen = " .
                                  showsPrec d (vkDisplayPrimaryGreen x) .
                                    showString ", " .
                                      showString "vkDisplayPrimaryBlue = " .
                                        showsPrec d (vkDisplayPrimaryBlue x) .
                                          showString ", " .
                                            showString "vkWhitePoint = " .
                                              showsPrec d (vkWhitePoint x) .
                                                showString ", " .
                                                  showString "vkMaxLuminance = " .
                                                    showsPrec d (vkMaxLuminance x) .
                                                      showString ", " .
                                                        showString "vkMinLuminance = " .
                                                          showsPrec d (vkMinLuminance x) .
                                                            showString ", " .
                                                              showString "vkMaxContentLightLevel = "
                                                                .
                                                                showsPrec d
                                                                  (vkMaxContentLightLevel x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkMaxFrameAverageLightLevel = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkMaxFrameAverageLightLevel
                                                                           x)
                                                                        . showChar '}'

-- | Chromaticity coordinate
--
--   > typedef struct VkXYColorEXT {
--   >     float   x;
--   >     float   y;
--   > } VkXYColorEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkXYColorEXT.html VkXYColorEXT registry at www.khronos.org>
data VkXYColorEXT = VkXYColorEXT## Addr## ByteArray##

instance Eq VkXYColorEXT where
        (VkXYColorEXT## a _) == x@(VkXYColorEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkXYColorEXT where
        (VkXYColorEXT## a _) `compare` x@(VkXYColorEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkXYColorEXT where
        sizeOf ~_ = #{size VkXYColorEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkXYColorEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkXYColorEXT where
        unsafeAddr (VkXYColorEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkXYColorEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkXYColorEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkXYColorEXT where
        type StructFields VkXYColorEXT = '["x", "y"] -- ' closing tick for hsc2hs
        type CUnionType VkXYColorEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkXYColorEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkXYColorEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkX VkXYColorEXT where
        type VkXMType VkXYColorEXT = #{type float}

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXYColorEXT, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkXYColorEXT, x}

        {-# INLINE readVkX #-}
        readVkX p = peekByteOff p #{offset VkXYColorEXT, x}

        {-# INLINE writeVkX #-}
        writeVkX p = pokeByteOff p #{offset VkXYColorEXT, x}

instance {-# OVERLAPPING #-} HasField "x" VkXYColorEXT where
        type FieldType "x" VkXYColorEXT = #{type float}
        type FieldOptional "x" VkXYColorEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "x" VkXYColorEXT =
             #{offset VkXYColorEXT, x}
        type FieldIsArray "x" VkXYColorEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkXYColorEXT, x}

instance CanReadField "x" VkXYColorEXT where
        {-# INLINE getField #-}
        getField = vkX

        {-# INLINE readField #-}
        readField = readVkX

instance CanWriteField "x" VkXYColorEXT where
        {-# INLINE writeField #-}
        writeField = writeVkX

instance {-# OVERLAPPING #-} HasVkY VkXYColorEXT where
        type VkYMType VkXYColorEXT = #{type float}

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXYColorEXT, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkXYColorEXT, y}

        {-# INLINE readVkY #-}
        readVkY p = peekByteOff p #{offset VkXYColorEXT, y}

        {-# INLINE writeVkY #-}
        writeVkY p = pokeByteOff p #{offset VkXYColorEXT, y}

instance {-# OVERLAPPING #-} HasField "y" VkXYColorEXT where
        type FieldType "y" VkXYColorEXT = #{type float}
        type FieldOptional "y" VkXYColorEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "y" VkXYColorEXT =
             #{offset VkXYColorEXT, y}
        type FieldIsArray "y" VkXYColorEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkXYColorEXT, y}

instance CanReadField "y" VkXYColorEXT where
        {-# INLINE getField #-}
        getField = vkY

        {-# INLINE readField #-}
        readField = readVkY

instance CanWriteField "y" VkXYColorEXT where
        {-# INLINE writeField #-}
        writeField = writeVkY

instance Show VkXYColorEXT where
        showsPrec d x
          = showString "VkXYColorEXT {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " . showsPrec d (vkY x) . showChar '}'

-- | > void vkSetHdrMetadataEXT
--   >     ( VkDevice device
--   >     , uint32_t swapchainCount
--   >     , const VkSwapchainKHR* pSwapchains
--   >     , const VkHdrMetadataEXT* pMetadata
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkSetHdrMetadataEXT.html vkSetHdrMetadataEXT registry at www.khronos.org>
foreign import ccall unsafe "vkSetHdrMetadataEXT"
               vkSetHdrMetadataEXT ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ swapchainCount
                        -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                              -> Ptr VkHdrMetadataEXT -- ^ pMetadata
                                                                      -> IO ()

pattern VK_EXT_HDR_METADATA_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_HDR_METADATA_SPEC_VERSION = 1

type VK_EXT_HDR_METADATA_SPEC_VERSION = 1

pattern VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString

pattern VK_EXT_HDR_METADATA_EXTENSION_NAME <-
        (is_VK_EXT_HDR_METADATA_EXTENSION_NAME -> True)
  where VK_EXT_HDR_METADATA_EXTENSION_NAME
          = _VK_EXT_HDR_METADATA_EXTENSION_NAME

{-# INLINE _VK_EXT_HDR_METADATA_EXTENSION_NAME #-}

_VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString
_VK_EXT_HDR_METADATA_EXTENSION_NAME
  = Ptr "VK_EXT_hdr_metadata\NUL"##

{-# INLINE is_VK_EXT_HDR_METADATA_EXTENSION_NAME #-}

is_VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_HDR_METADATA_EXTENSION_NAME
  = eqCStrings _VK_EXT_HDR_METADATA_EXTENSION_NAME

type VK_EXT_HDR_METADATA_EXTENSION_NAME = "VK_EXT_hdr_metadata"

pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT =
        VkStructureType 1000105000
