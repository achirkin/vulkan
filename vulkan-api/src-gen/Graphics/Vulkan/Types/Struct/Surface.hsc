#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Surface
       (VkSurfaceCapabilities2EXT(..), VkSurfaceCapabilities2KHR(..),
        VkSurfaceCapabilitiesKHR(..), VkSurfaceFormat2KHR(..),
        VkSurfaceFormatKHR(..))
       where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Base                                          (Addr##,
                                                                    ByteArray##,
                                                                    byteArrayContents##,
                                                                    plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Color                  (VkColorSpaceKHR)
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR (VkCompositeAlphaFlagsKHR)
import           Graphics.Vulkan.Types.Enum.Format                 (VkFormat)
import           Graphics.Vulkan.Types.Enum.Image                  (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType          (VkStructureType)
import           Graphics.Vulkan.Types.Enum.Surface                (VkSurfaceCounterFlagsEXT,
                                                                    VkSurfaceTransformFlagBitsKHR,
                                                                    VkSurfaceTransformFlagsKHR)
import           Graphics.Vulkan.Types.Struct.Extent               (VkExtent2D)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkSurfaceCapabilities2EXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         minImageCount;
--   >     uint32_t                         maxImageCount;
--   >     VkExtent2D                       currentExtent;
--   >     VkExtent2D                       minImageExtent;
--   >     VkExtent2D                       maxImageExtent;
--   >     uint32_t                         maxImageArrayLayers;
--   >     VkSurfaceTransformFlagsKHR       supportedTransforms;
--   >     VkSurfaceTransformFlagBitsKHR    currentTransform;
--   >     VkCompositeAlphaFlagsKHR         supportedCompositeAlpha;
--   >     VkImageUsageFlags                supportedUsageFlags;
--   >     VkSurfaceCounterFlagsEXT supportedSurfaceCounters;
--   > } VkSurfaceCapabilities2EXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceCapabilities2EXT VkSurfaceCapabilities2EXT registry at www.khronos.org>
data VkSurfaceCapabilities2EXT = VkSurfaceCapabilities2EXT## Addr##
                                                            ByteArray##

instance Eq VkSurfaceCapabilities2EXT where
        (VkSurfaceCapabilities2EXT## a _) ==
          x@(VkSurfaceCapabilities2EXT## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceCapabilities2EXT where
        (VkSurfaceCapabilities2EXT## a _) `compare`
          x@(VkSurfaceCapabilities2EXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceCapabilities2EXT where
        sizeOf ~_ = #{size VkSurfaceCapabilities2EXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceCapabilities2EXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceCapabilities2EXT where
        unsafeAddr (VkSurfaceCapabilities2EXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceCapabilities2EXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceCapabilities2EXT## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceCapabilities2EXT where
        type StructFields VkSurfaceCapabilities2EXT =
             '["sType", "pNext", "minImageCount", "maxImageCount", -- ' closing tick for hsc2hs
               "currentExtent", "minImageExtent", "maxImageExtent",
               "maxImageArrayLayers", "supportedTransforms", "currentTransform",
               "supportedCompositeAlpha", "supportedUsageFlags",
               "supportedSurfaceCounters"]
        type CUnionType VkSurfaceCapabilities2EXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceCapabilities2EXT = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceCapabilities2EXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSurfaceCapabilities2EXT where
        type FieldType "sType" VkSurfaceCapabilities2EXT = VkStructureType
        type FieldOptional "sType" VkSurfaceCapabilities2EXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, sType}
        type FieldIsArray "sType" VkSurfaceCapabilities2EXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSurfaceCapabilities2EXT where
        type FieldType "pNext" VkSurfaceCapabilities2EXT = Ptr Void
        type FieldOptional "pNext" VkSurfaceCapabilities2EXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, pNext}
        type FieldIsArray "pNext" VkSurfaceCapabilities2EXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "minImageCount" VkSurfaceCapabilities2EXT where
        type FieldType "minImageCount" VkSurfaceCapabilities2EXT = Word32
        type FieldOptional "minImageCount" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minImageCount" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, minImageCount}
        type FieldIsArray "minImageCount" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, minImageCount}

instance {-# OVERLAPPING #-}
         CanReadField "minImageCount" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, minImageCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, minImageCount}

instance {-# OVERLAPPING #-}
         CanWriteField "minImageCount" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, minImageCount}

instance {-# OVERLAPPING #-}
         HasField "maxImageCount" VkSurfaceCapabilities2EXT where
        type FieldType "maxImageCount" VkSurfaceCapabilities2EXT = Word32
        type FieldOptional "maxImageCount" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageCount" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, maxImageCount}
        type FieldIsArray "maxImageCount" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, maxImageCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxImageCount" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, maxImageCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageCount" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageCount}

instance {-# OVERLAPPING #-}
         HasField "currentExtent" VkSurfaceCapabilities2EXT where
        type FieldType "currentExtent" VkSurfaceCapabilities2EXT =
             VkExtent2D
        type FieldOptional "currentExtent" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "currentExtent" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, currentExtent}
        type FieldIsArray "currentExtent" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, currentExtent}

instance {-# OVERLAPPING #-}
         CanReadField "currentExtent" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, currentExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, currentExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "currentExtent" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, currentExtent}

instance {-# OVERLAPPING #-}
         HasField "minImageExtent" VkSurfaceCapabilities2EXT where
        type FieldType "minImageExtent" VkSurfaceCapabilities2EXT =
             VkExtent2D
        type FieldOptional "minImageExtent" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minImageExtent" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, minImageExtent}
        type FieldIsArray "minImageExtent" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, minImageExtent}

instance {-# OVERLAPPING #-}
         CanReadField "minImageExtent" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, minImageExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, minImageExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "minImageExtent" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, minImageExtent}

instance {-# OVERLAPPING #-}
         HasField "maxImageExtent" VkSurfaceCapabilities2EXT where
        type FieldType "maxImageExtent" VkSurfaceCapabilities2EXT =
             VkExtent2D
        type FieldOptional "maxImageExtent" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageExtent" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, maxImageExtent}
        type FieldIsArray "maxImageExtent" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, maxImageExtent}

instance {-# OVERLAPPING #-}
         CanReadField "maxImageExtent" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, maxImageExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageExtent" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageExtent}

instance {-# OVERLAPPING #-}
         HasField "maxImageArrayLayers" VkSurfaceCapabilities2EXT where
        type FieldType "maxImageArrayLayers" VkSurfaceCapabilities2EXT =
             Word32
        type FieldOptional "maxImageArrayLayers" VkSurfaceCapabilities2EXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageArrayLayers" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}
        type FieldIsArray "maxImageArrayLayers" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         CanReadField "maxImageArrayLayers" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageArrayLayers" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         HasField "supportedTransforms" VkSurfaceCapabilities2EXT where
        type FieldType "supportedTransforms" VkSurfaceCapabilities2EXT =
             VkSurfaceTransformFlagsKHR
        type FieldOptional "supportedTransforms" VkSurfaceCapabilities2EXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedTransforms" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, supportedTransforms}
        type FieldIsArray "supportedTransforms" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, supportedTransforms}

instance {-# OVERLAPPING #-}
         CanReadField "supportedTransforms" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedTransforms})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedTransforms}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedTransforms" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedTransforms}

instance {-# OVERLAPPING #-}
         HasField "currentTransform" VkSurfaceCapabilities2EXT where
        type FieldType "currentTransform" VkSurfaceCapabilities2EXT =
             VkSurfaceTransformFlagBitsKHR
        type FieldOptional "currentTransform" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "currentTransform" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, currentTransform}
        type FieldIsArray "currentTransform" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, currentTransform}

instance {-# OVERLAPPING #-}
         CanReadField "currentTransform" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, currentTransform})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, currentTransform}

instance {-# OVERLAPPING #-}
         CanWriteField "currentTransform" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, currentTransform}

instance {-# OVERLAPPING #-}
         HasField "supportedCompositeAlpha" VkSurfaceCapabilities2EXT where
        type FieldType "supportedCompositeAlpha" VkSurfaceCapabilities2EXT
             = VkCompositeAlphaFlagsKHR
        type FieldOptional "supportedCompositeAlpha"
               VkSurfaceCapabilities2EXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedCompositeAlpha"
               VkSurfaceCapabilities2EXT
             =
             #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}
        type FieldIsArray "supportedCompositeAlpha"
               VkSurfaceCapabilities2EXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}

instance {-# OVERLAPPING #-}
         CanReadField "supportedCompositeAlpha" VkSurfaceCapabilities2EXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedCompositeAlpha" VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}

instance {-# OVERLAPPING #-}
         HasField "supportedUsageFlags" VkSurfaceCapabilities2EXT where
        type FieldType "supportedUsageFlags" VkSurfaceCapabilities2EXT =
             VkImageUsageFlags
        type FieldOptional "supportedUsageFlags" VkSurfaceCapabilities2EXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedUsageFlags" VkSurfaceCapabilities2EXT =
             #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}
        type FieldIsArray "supportedUsageFlags" VkSurfaceCapabilities2EXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}

instance {-# OVERLAPPING #-}
         CanReadField "supportedUsageFlags" VkSurfaceCapabilities2EXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedUsageFlags" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}

instance {-# OVERLAPPING #-}
         HasField "supportedSurfaceCounters" VkSurfaceCapabilities2EXT where
        type FieldType "supportedSurfaceCounters" VkSurfaceCapabilities2EXT
             = VkSurfaceCounterFlagsEXT
        type FieldOptional "supportedSurfaceCounters"
               VkSurfaceCapabilities2EXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedSurfaceCounters"
               VkSurfaceCapabilities2EXT
             =
             #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}
        type FieldIsArray "supportedSurfaceCounters"
               VkSurfaceCapabilities2EXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}

instance {-# OVERLAPPING #-}
         CanReadField "supportedSurfaceCounters" VkSurfaceCapabilities2EXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedSurfaceCounters" VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}

instance Show VkSurfaceCapabilities2EXT where
        showsPrec d x
          = showString "VkSurfaceCapabilities2EXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "minImageCount = " .
                            showsPrec d (getField @"minImageCount" x) .
                              showString ", " .
                                showString "maxImageCount = " .
                                  showsPrec d (getField @"maxImageCount" x) .
                                    showString ", " .
                                      showString "currentExtent = " .
                                        showsPrec d (getField @"currentExtent" x) .
                                          showString ", " .
                                            showString "minImageExtent = " .
                                              showsPrec d (getField @"minImageExtent" x) .
                                                showString ", " .
                                                  showString "maxImageExtent = " .
                                                    showsPrec d (getField @"maxImageExtent" x) .
                                                      showString ", " .
                                                        showString "maxImageArrayLayers = " .
                                                          showsPrec d
                                                            (getField @"maxImageArrayLayers" x)
                                                            .
                                                            showString ", " .
                                                              showString "supportedTransforms = " .
                                                                showsPrec d
                                                                  (getField @"supportedTransforms"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "currentTransform = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"currentTransform"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "supportedCompositeAlpha = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"supportedCompositeAlpha"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "supportedUsageFlags = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"supportedUsageFlags"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "supportedSurfaceCounters = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"supportedSurfaceCounters"
                                                                                             x)
                                                                                          .
                                                                                          showChar
                                                                                            '}'

-- | > typedef struct VkSurfaceCapabilities2KHR {
--   >     VkStructureType sType;
--   >     void*   pNext;
--   >     VkSurfaceCapabilitiesKHR surfaceCapabilities;
--   > } VkSurfaceCapabilities2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceCapabilities2KHR VkSurfaceCapabilities2KHR registry at www.khronos.org>
data VkSurfaceCapabilities2KHR = VkSurfaceCapabilities2KHR## Addr##
                                                            ByteArray##

instance Eq VkSurfaceCapabilities2KHR where
        (VkSurfaceCapabilities2KHR## a _) ==
          x@(VkSurfaceCapabilities2KHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceCapabilities2KHR where
        (VkSurfaceCapabilities2KHR## a _) `compare`
          x@(VkSurfaceCapabilities2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceCapabilities2KHR where
        sizeOf ~_ = #{size VkSurfaceCapabilities2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceCapabilities2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceCapabilities2KHR where
        unsafeAddr (VkSurfaceCapabilities2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceCapabilities2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceCapabilities2KHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceCapabilities2KHR where
        type StructFields VkSurfaceCapabilities2KHR =
             '["sType", "pNext", "surfaceCapabilities"] -- ' closing tick for hsc2hs
        type CUnionType VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceCapabilities2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceCapabilities2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSurfaceCapabilities2KHR where
        type FieldType "sType" VkSurfaceCapabilities2KHR = VkStructureType
        type FieldOptional "sType" VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSurfaceCapabilities2KHR =
             #{offset VkSurfaceCapabilities2KHR, sType}
        type FieldIsArray "sType" VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSurfaceCapabilities2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSurfaceCapabilities2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSurfaceCapabilities2KHR where
        type FieldType "pNext" VkSurfaceCapabilities2KHR = Ptr Void
        type FieldOptional "pNext" VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSurfaceCapabilities2KHR =
             #{offset VkSurfaceCapabilities2KHR, pNext}
        type FieldIsArray "pNext" VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSurfaceCapabilities2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSurfaceCapabilities2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "surfaceCapabilities" VkSurfaceCapabilities2KHR where
        type FieldType "surfaceCapabilities" VkSurfaceCapabilities2KHR =
             VkSurfaceCapabilitiesKHR
        type FieldOptional "surfaceCapabilities" VkSurfaceCapabilities2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "surfaceCapabilities" VkSurfaceCapabilities2KHR =
             #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}
        type FieldIsArray "surfaceCapabilities" VkSurfaceCapabilities2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

instance {-# OVERLAPPING #-}
         CanReadField "surfaceCapabilities" VkSurfaceCapabilities2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

instance {-# OVERLAPPING #-}
         CanWriteField "surfaceCapabilities" VkSurfaceCapabilities2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

instance Show VkSurfaceCapabilities2KHR where
        showsPrec d x
          = showString "VkSurfaceCapabilities2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "surfaceCapabilities = " .
                            showsPrec d (getField @"surfaceCapabilities" x) . showChar '}'

-- | > typedef struct VkSurfaceCapabilitiesKHR {
--   >     uint32_t                         minImageCount;
--   >     uint32_t                         maxImageCount;
--   >     VkExtent2D                       currentExtent;
--   >     VkExtent2D                       minImageExtent;
--   >     VkExtent2D                       maxImageExtent;
--   >     uint32_t                         maxImageArrayLayers;
--   >     VkSurfaceTransformFlagsKHR       supportedTransforms;
--   >     VkSurfaceTransformFlagBitsKHR    currentTransform;
--   >     VkCompositeAlphaFlagsKHR         supportedCompositeAlpha;
--   >     VkImageUsageFlags                supportedUsageFlags;
--   > } VkSurfaceCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceCapabilitiesKHR VkSurfaceCapabilitiesKHR registry at www.khronos.org>
data VkSurfaceCapabilitiesKHR = VkSurfaceCapabilitiesKHR## Addr##
                                                          ByteArray##

instance Eq VkSurfaceCapabilitiesKHR where
        (VkSurfaceCapabilitiesKHR## a _) ==
          x@(VkSurfaceCapabilitiesKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceCapabilitiesKHR where
        (VkSurfaceCapabilitiesKHR## a _) `compare`
          x@(VkSurfaceCapabilitiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceCapabilitiesKHR where
        sizeOf ~_ = #{size VkSurfaceCapabilitiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceCapabilitiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceCapabilitiesKHR where
        unsafeAddr (VkSurfaceCapabilitiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceCapabilitiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceCapabilitiesKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceCapabilitiesKHR where
        type StructFields VkSurfaceCapabilitiesKHR =
             '["minImageCount", "maxImageCount", "currentExtent", -- ' closing tick for hsc2hs
               "minImageExtent", "maxImageExtent", "maxImageArrayLayers",
               "supportedTransforms", "currentTransform",
               "supportedCompositeAlpha", "supportedUsageFlags"]
        type CUnionType VkSurfaceCapabilitiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceCapabilitiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceCapabilitiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "minImageCount" VkSurfaceCapabilitiesKHR where
        type FieldType "minImageCount" VkSurfaceCapabilitiesKHR = Word32
        type FieldOptional "minImageCount" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minImageCount" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, minImageCount}
        type FieldIsArray "minImageCount" VkSurfaceCapabilitiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, minImageCount}

instance {-# OVERLAPPING #-}
         CanReadField "minImageCount" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, minImageCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageCount}

instance {-# OVERLAPPING #-}
         CanWriteField "minImageCount" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageCount}

instance {-# OVERLAPPING #-}
         HasField "maxImageCount" VkSurfaceCapabilitiesKHR where
        type FieldType "maxImageCount" VkSurfaceCapabilitiesKHR = Word32
        type FieldOptional "maxImageCount" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageCount" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, maxImageCount}
        type FieldIsArray "maxImageCount" VkSurfaceCapabilitiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, maxImageCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxImageCount" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, maxImageCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageCount" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageCount}

instance {-# OVERLAPPING #-}
         HasField "currentExtent" VkSurfaceCapabilitiesKHR where
        type FieldType "currentExtent" VkSurfaceCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "currentExtent" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "currentExtent" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, currentExtent}
        type FieldIsArray "currentExtent" VkSurfaceCapabilitiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, currentExtent}

instance {-# OVERLAPPING #-}
         CanReadField "currentExtent" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, currentExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, currentExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "currentExtent" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, currentExtent}

instance {-# OVERLAPPING #-}
         HasField "minImageExtent" VkSurfaceCapabilitiesKHR where
        type FieldType "minImageExtent" VkSurfaceCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "minImageExtent" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minImageExtent" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, minImageExtent}
        type FieldIsArray "minImageExtent" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, minImageExtent}

instance {-# OVERLAPPING #-}
         CanReadField "minImageExtent" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, minImageExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "minImageExtent" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageExtent}

instance {-# OVERLAPPING #-}
         HasField "maxImageExtent" VkSurfaceCapabilitiesKHR where
        type FieldType "maxImageExtent" VkSurfaceCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "maxImageExtent" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageExtent" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}
        type FieldIsArray "maxImageExtent" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}

instance {-# OVERLAPPING #-}
         CanReadField "maxImageExtent" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, maxImageExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageExtent" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}

instance {-# OVERLAPPING #-}
         HasField "maxImageArrayLayers" VkSurfaceCapabilitiesKHR where
        type FieldType "maxImageArrayLayers" VkSurfaceCapabilitiesKHR =
             Word32
        type FieldOptional "maxImageArrayLayers" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageArrayLayers" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}
        type FieldIsArray "maxImageArrayLayers" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         CanReadField "maxImageArrayLayers" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageArrayLayers" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         HasField "supportedTransforms" VkSurfaceCapabilitiesKHR where
        type FieldType "supportedTransforms" VkSurfaceCapabilitiesKHR =
             VkSurfaceTransformFlagsKHR
        type FieldOptional "supportedTransforms" VkSurfaceCapabilitiesKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedTransforms" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}
        type FieldIsArray "supportedTransforms" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         CanReadField "supportedTransforms" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, supportedTransforms})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedTransforms" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         HasField "currentTransform" VkSurfaceCapabilitiesKHR where
        type FieldType "currentTransform" VkSurfaceCapabilitiesKHR =
             VkSurfaceTransformFlagBitsKHR
        type FieldOptional "currentTransform" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "currentTransform" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, currentTransform}
        type FieldIsArray "currentTransform" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, currentTransform}

instance {-# OVERLAPPING #-}
         CanReadField "currentTransform" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, currentTransform})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, currentTransform}

instance {-# OVERLAPPING #-}
         CanWriteField "currentTransform" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, currentTransform}

instance {-# OVERLAPPING #-}
         HasField "supportedCompositeAlpha" VkSurfaceCapabilitiesKHR where
        type FieldType "supportedCompositeAlpha" VkSurfaceCapabilitiesKHR =
             VkCompositeAlphaFlagsKHR
        type FieldOptional "supportedCompositeAlpha"
               VkSurfaceCapabilitiesKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedCompositeAlpha" VkSurfaceCapabilitiesKHR
             =
             #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}
        type FieldIsArray "supportedCompositeAlpha"
               VkSurfaceCapabilitiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}

instance {-# OVERLAPPING #-}
         CanReadField "supportedCompositeAlpha" VkSurfaceCapabilitiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedCompositeAlpha" VkSurfaceCapabilitiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}

instance {-# OVERLAPPING #-}
         HasField "supportedUsageFlags" VkSurfaceCapabilitiesKHR where
        type FieldType "supportedUsageFlags" VkSurfaceCapabilitiesKHR =
             VkImageUsageFlags
        type FieldOptional "supportedUsageFlags" VkSurfaceCapabilitiesKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedUsageFlags" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}
        type FieldIsArray "supportedUsageFlags" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}

instance {-# OVERLAPPING #-}
         CanReadField "supportedUsageFlags" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedUsageFlags" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}

instance Show VkSurfaceCapabilitiesKHR where
        showsPrec d x
          = showString "VkSurfaceCapabilitiesKHR {" .
              showString "minImageCount = " .
                showsPrec d (getField @"minImageCount" x) .
                  showString ", " .
                    showString "maxImageCount = " .
                      showsPrec d (getField @"maxImageCount" x) .
                        showString ", " .
                          showString "currentExtent = " .
                            showsPrec d (getField @"currentExtent" x) .
                              showString ", " .
                                showString "minImageExtent = " .
                                  showsPrec d (getField @"minImageExtent" x) .
                                    showString ", " .
                                      showString "maxImageExtent = " .
                                        showsPrec d (getField @"maxImageExtent" x) .
                                          showString ", " .
                                            showString "maxImageArrayLayers = " .
                                              showsPrec d (getField @"maxImageArrayLayers" x) .
                                                showString ", " .
                                                  showString "supportedTransforms = " .
                                                    showsPrec d (getField @"supportedTransforms" x)
                                                      .
                                                      showString ", " .
                                                        showString "currentTransform = " .
                                                          showsPrec d
                                                            (getField @"currentTransform" x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "supportedCompositeAlpha = "
                                                                .
                                                                showsPrec d
                                                                  (getField
                                                                     @"supportedCompositeAlpha"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "supportedUsageFlags = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"supportedUsageFlags"
                                                                           x)
                                                                        . showChar '}'

-- | > typedef struct VkSurfaceFormat2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkSurfaceFormatKHR surfaceFormat;
--   > } VkSurfaceFormat2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceFormat2KHR VkSurfaceFormat2KHR registry at www.khronos.org>
data VkSurfaceFormat2KHR = VkSurfaceFormat2KHR## Addr## ByteArray##

instance Eq VkSurfaceFormat2KHR where
        (VkSurfaceFormat2KHR## a _) == x@(VkSurfaceFormat2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceFormat2KHR where
        (VkSurfaceFormat2KHR## a _) `compare` x@(VkSurfaceFormat2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceFormat2KHR where
        sizeOf ~_ = #{size VkSurfaceFormat2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceFormat2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceFormat2KHR where
        unsafeAddr (VkSurfaceFormat2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceFormat2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceFormat2KHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceFormat2KHR where
        type StructFields VkSurfaceFormat2KHR =
             '["sType", "pNext", "surfaceFormat"] -- ' closing tick for hsc2hs
        type CUnionType VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceFormat2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceFormat2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkSurfaceFormat2KHR
         where
        type FieldType "sType" VkSurfaceFormat2KHR = VkStructureType
        type FieldOptional "sType" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSurfaceFormat2KHR =
             #{offset VkSurfaceFormat2KHR, sType}
        type FieldIsArray "sType" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSurfaceFormat2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSurfaceFormat2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSurfaceFormat2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkSurfaceFormat2KHR
         where
        type FieldType "pNext" VkSurfaceFormat2KHR = Ptr Void
        type FieldOptional "pNext" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSurfaceFormat2KHR =
             #{offset VkSurfaceFormat2KHR, pNext}
        type FieldIsArray "pNext" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSurfaceFormat2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSurfaceFormat2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSurfaceFormat2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "surfaceFormat" VkSurfaceFormat2KHR where
        type FieldType "surfaceFormat" VkSurfaceFormat2KHR =
             VkSurfaceFormatKHR
        type FieldOptional "surfaceFormat" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "surfaceFormat" VkSurfaceFormat2KHR =
             #{offset VkSurfaceFormat2KHR, surfaceFormat}
        type FieldIsArray "surfaceFormat" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceFormat2KHR, surfaceFormat}

instance {-# OVERLAPPING #-}
         CanReadField "surfaceFormat" VkSurfaceFormat2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, surfaceFormat})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, surfaceFormat}

instance {-# OVERLAPPING #-}
         CanWriteField "surfaceFormat" VkSurfaceFormat2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, surfaceFormat}

instance Show VkSurfaceFormat2KHR where
        showsPrec d x
          = showString "VkSurfaceFormat2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "surfaceFormat = " .
                            showsPrec d (getField @"surfaceFormat" x) . showChar '}'

-- | > typedef struct VkSurfaceFormatKHR {
--   >     VkFormat                         format;
--   >     VkColorSpaceKHR                  colorSpace;
--   > } VkSurfaceFormatKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceFormatKHR VkSurfaceFormatKHR registry at www.khronos.org>
data VkSurfaceFormatKHR = VkSurfaceFormatKHR## Addr## ByteArray##

instance Eq VkSurfaceFormatKHR where
        (VkSurfaceFormatKHR## a _) == x@(VkSurfaceFormatKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceFormatKHR where
        (VkSurfaceFormatKHR## a _) `compare` x@(VkSurfaceFormatKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceFormatKHR where
        sizeOf ~_ = #{size VkSurfaceFormatKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceFormatKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceFormatKHR where
        unsafeAddr (VkSurfaceFormatKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceFormatKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceFormatKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceFormatKHR where
        type StructFields VkSurfaceFormatKHR = '["format", "colorSpace"] -- ' closing tick for hsc2hs
        type CUnionType VkSurfaceFormatKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceFormatKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceFormatKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "format" VkSurfaceFormatKHR
         where
        type FieldType "format" VkSurfaceFormatKHR = VkFormat
        type FieldOptional "format" VkSurfaceFormatKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkSurfaceFormatKHR =
             #{offset VkSurfaceFormatKHR, format}
        type FieldIsArray "format" VkSurfaceFormatKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSurfaceFormatKHR, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkSurfaceFormatKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormatKHR, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceFormatKHR, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkSurfaceFormatKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceFormatKHR, format}

instance {-# OVERLAPPING #-}
         HasField "colorSpace" VkSurfaceFormatKHR where
        type FieldType "colorSpace" VkSurfaceFormatKHR = VkColorSpaceKHR
        type FieldOptional "colorSpace" VkSurfaceFormatKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "colorSpace" VkSurfaceFormatKHR =
             #{offset VkSurfaceFormatKHR, colorSpace}
        type FieldIsArray "colorSpace" VkSurfaceFormatKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSurfaceFormatKHR, colorSpace}

instance {-# OVERLAPPING #-}
         CanReadField "colorSpace" VkSurfaceFormatKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormatKHR, colorSpace})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceFormatKHR, colorSpace}

instance {-# OVERLAPPING #-}
         CanWriteField "colorSpace" VkSurfaceFormatKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceFormatKHR, colorSpace}

instance Show VkSurfaceFormatKHR where
        showsPrec d x
          = showString "VkSurfaceFormatKHR {" .
              showString "format = " .
                showsPrec d (getField @"format" x) .
                  showString ", " .
                    showString "colorSpace = " .
                      showsPrec d (getField @"colorSpace" x) . showChar '}'
