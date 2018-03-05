#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSurfaceCapabilities2EXT
       (VkSurfaceCapabilities2EXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR   (VkCompositeAlphaFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags          (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Enum.VkSurfaceCounterFlagsEXT   (VkSurfaceCounterFlagsEXT)
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR (VkSurfaceTransformFlagBitsKHR,
                                                                        VkSurfaceTransformFlagsKHR)
import           Graphics.Vulkan.Types.Struct.VkExtent2D               (VkExtent2D)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSurfaceCapabilities2EXT.html VkSurfaceCapabilities2EXT registry at www.khronos.org>
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
