#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.HdrMetadataEXT
       (VkHdrMetadataEXT(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.XYColorEXT  (VkXYColorEXT)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkHdrMetadataEXT VkHdrMetadataEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} CanReadField "sType" VkHdrMetadataEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkHdrMetadataEXT, sType}

instance {-# OVERLAPPING #-} CanWriteField "sType" VkHdrMetadataEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkHdrMetadataEXT, sType}

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

instance {-# OVERLAPPING #-} CanReadField "pNext" VkHdrMetadataEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkHdrMetadataEXT, pNext}

instance {-# OVERLAPPING #-} CanWriteField "pNext" VkHdrMetadataEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkHdrMetadataEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "displayPrimaryRed" VkHdrMetadataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, displayPrimaryRed})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkHdrMetadataEXT, displayPrimaryRed}

instance {-# OVERLAPPING #-}
         CanWriteField "displayPrimaryRed" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkHdrMetadataEXT, displayPrimaryRed}

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

instance {-# OVERLAPPING #-}
         CanReadField "displayPrimaryGreen" VkHdrMetadataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, displayPrimaryGreen})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkHdrMetadataEXT, displayPrimaryGreen}

instance {-# OVERLAPPING #-}
         CanWriteField "displayPrimaryGreen" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkHdrMetadataEXT, displayPrimaryGreen}

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

instance {-# OVERLAPPING #-}
         CanReadField "displayPrimaryBlue" VkHdrMetadataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, displayPrimaryBlue})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkHdrMetadataEXT, displayPrimaryBlue}

instance {-# OVERLAPPING #-}
         CanWriteField "displayPrimaryBlue" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkHdrMetadataEXT, displayPrimaryBlue}

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

instance {-# OVERLAPPING #-}
         CanReadField "whitePoint" VkHdrMetadataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, whitePoint})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkHdrMetadataEXT, whitePoint}

instance {-# OVERLAPPING #-}
         CanWriteField "whitePoint" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkHdrMetadataEXT, whitePoint}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxLuminance" VkHdrMetadataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, maxLuminance})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkHdrMetadataEXT, maxLuminance}

instance {-# OVERLAPPING #-}
         CanWriteField "maxLuminance" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkHdrMetadataEXT, maxLuminance}

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

instance {-# OVERLAPPING #-}
         CanReadField "minLuminance" VkHdrMetadataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, minLuminance})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkHdrMetadataEXT, minLuminance}

instance {-# OVERLAPPING #-}
         CanWriteField "minLuminance" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkHdrMetadataEXT, minLuminance}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxContentLightLevel" VkHdrMetadataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, maxContentLightLevel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkHdrMetadataEXT, maxContentLightLevel}

instance {-# OVERLAPPING #-}
         CanWriteField "maxContentLightLevel" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkHdrMetadataEXT, maxContentLightLevel}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxFrameAverageLightLevel" VkHdrMetadataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}

instance {-# OVERLAPPING #-}
         CanWriteField "maxFrameAverageLightLevel" VkHdrMetadataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}

instance Show VkHdrMetadataEXT where
        showsPrec d x
          = showString "VkHdrMetadataEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "displayPrimaryRed = " .
                            showsPrec d (getField @"displayPrimaryRed" x) .
                              showString ", " .
                                showString "displayPrimaryGreen = " .
                                  showsPrec d (getField @"displayPrimaryGreen" x) .
                                    showString ", " .
                                      showString "displayPrimaryBlue = " .
                                        showsPrec d (getField @"displayPrimaryBlue" x) .
                                          showString ", " .
                                            showString "whitePoint = " .
                                              showsPrec d (getField @"whitePoint" x) .
                                                showString ", " .
                                                  showString "maxLuminance = " .
                                                    showsPrec d (getField @"maxLuminance" x) .
                                                      showString ", " .
                                                        showString "minLuminance = " .
                                                          showsPrec d (getField @"minLuminance" x) .
                                                            showString ", " .
                                                              showString "maxContentLightLevel = " .
                                                                showsPrec d
                                                                  (getField @"maxContentLightLevel"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "maxFrameAverageLightLevel = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"maxFrameAverageLightLevel"
                                                                           x)
                                                                        . showChar '}'
