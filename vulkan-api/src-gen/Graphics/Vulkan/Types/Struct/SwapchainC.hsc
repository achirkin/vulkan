#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SwapchainC
       (VkSwapchainCounterCreateInfoEXT(..), VkSwapchainCreateInfoKHR(..))
       where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Base                                           (Addr##,
                                                                     ByteArray##,
                                                                     byteArrayContents##,
                                                                     plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                    (VkBool32)
import           Graphics.Vulkan.Types.Enum.Color                   (VkColorSpaceKHR)
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR  (VkCompositeAlphaFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.Format                  (VkFormat)
import           Graphics.Vulkan.Types.Enum.Image                   (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.PresentModeKHR          (VkPresentModeKHR)
import           Graphics.Vulkan.Types.Enum.SharingMode             (VkSharingMode)
import           Graphics.Vulkan.Types.Enum.StructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Enum.Surface                 (VkSurfaceCounterFlagsEXT,
                                                                     VkSurfaceTransformFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.SwapchainCreateFlagsKHR (VkSwapchainCreateFlagsKHR)
import           Graphics.Vulkan.Types.Handles                      (VkSurfaceKHR,
                                                                     VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.Extent                (VkExtent2D)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkSwapchainCounterCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSurfaceCounterFlagsEXT         surfaceCounters;
--   > } VkSwapchainCounterCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSwapchainCounterCreateInfoEXT VkSwapchainCounterCreateInfoEXT registry at www.khronos.org>
data VkSwapchainCounterCreateInfoEXT = VkSwapchainCounterCreateInfoEXT## Addr##
                                                                        ByteArray##

instance Eq VkSwapchainCounterCreateInfoEXT where
        (VkSwapchainCounterCreateInfoEXT## a _) ==
          x@(VkSwapchainCounterCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSwapchainCounterCreateInfoEXT where
        (VkSwapchainCounterCreateInfoEXT## a _) `compare`
          x@(VkSwapchainCounterCreateInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSwapchainCounterCreateInfoEXT where
        sizeOf ~_ = #{size VkSwapchainCounterCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSwapchainCounterCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSwapchainCounterCreateInfoEXT where
        unsafeAddr (VkSwapchainCounterCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSwapchainCounterCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSwapchainCounterCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSwapchainCounterCreateInfoEXT where
        type StructFields VkSwapchainCounterCreateInfoEXT =
             '["sType", "pNext", "surfaceCounters"] -- ' closing tick for hsc2hs
        type CUnionType VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSwapchainCounterCreateInfoEXT =
             '[VkSwapchainCreateInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSwapchainCounterCreateInfoEXT where
        type FieldType "sType" VkSwapchainCounterCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSwapchainCounterCreateInfoEXT =
             #{offset VkSwapchainCounterCreateInfoEXT, sType}
        type FieldIsArray "sType" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCounterCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSwapchainCounterCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSwapchainCounterCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSwapchainCounterCreateInfoEXT where
        type FieldType "pNext" VkSwapchainCounterCreateInfoEXT = Ptr Void
        type FieldOptional "pNext" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSwapchainCounterCreateInfoEXT =
             #{offset VkSwapchainCounterCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCounterCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSwapchainCounterCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSwapchainCounterCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "surfaceCounters" VkSwapchainCounterCreateInfoEXT where
        type FieldType "surfaceCounters" VkSwapchainCounterCreateInfoEXT =
             VkSurfaceCounterFlagsEXT
        type FieldOptional "surfaceCounters"
               VkSwapchainCounterCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "surfaceCounters" VkSwapchainCounterCreateInfoEXT
             =
             #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}
        type FieldIsArray "surfaceCounters" VkSwapchainCounterCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

instance {-# OVERLAPPING #-}
         CanReadField "surfaceCounters" VkSwapchainCounterCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

instance {-# OVERLAPPING #-}
         CanWriteField "surfaceCounters" VkSwapchainCounterCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

instance Show VkSwapchainCounterCreateInfoEXT where
        showsPrec d x
          = showString "VkSwapchainCounterCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "surfaceCounters = " .
                            showsPrec d (getField @"surfaceCounters" x) . showChar '}'

-- | > typedef struct VkSwapchainCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainCreateFlagsKHR        flags;
--   >     VkSurfaceKHR                     surface;
--   >     uint32_t                         minImageCount;
--   >     VkFormat                         imageFormat;
--   >     VkColorSpaceKHR                  imageColorSpace;
--   >     VkExtent2D                       imageExtent;
--   >     uint32_t                         imageArrayLayers;
--   >     VkImageUsageFlags                imageUsage;
--   >     VkSharingMode                    imageSharingMode;
--   >     uint32_t         queueFamilyIndexCount;
--   >     const uint32_t*                  pQueueFamilyIndices;
--   >     VkSurfaceTransformFlagBitsKHR    preTransform;
--   >     VkCompositeAlphaFlagBitsKHR      compositeAlpha;
--   >     VkPresentModeKHR                 presentMode;
--   >     VkBool32                         clipped;
--   >     VkSwapchainKHR   oldSwapchain;
--   > } VkSwapchainCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSwapchainCreateInfoKHR VkSwapchainCreateInfoKHR registry at www.khronos.org>
data VkSwapchainCreateInfoKHR = VkSwapchainCreateInfoKHR## Addr##
                                                          ByteArray##

instance Eq VkSwapchainCreateInfoKHR where
        (VkSwapchainCreateInfoKHR## a _) ==
          x@(VkSwapchainCreateInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSwapchainCreateInfoKHR where
        (VkSwapchainCreateInfoKHR## a _) `compare`
          x@(VkSwapchainCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSwapchainCreateInfoKHR where
        sizeOf ~_ = #{size VkSwapchainCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSwapchainCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSwapchainCreateInfoKHR where
        unsafeAddr (VkSwapchainCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSwapchainCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSwapchainCreateInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSwapchainCreateInfoKHR where
        type StructFields VkSwapchainCreateInfoKHR =
             '["sType", "pNext", "flags", "surface", "minImageCount", -- ' closing tick for hsc2hs
               "imageFormat", "imageColorSpace", "imageExtent",
               "imageArrayLayers", "imageUsage", "imageSharingMode",
               "queueFamilyIndexCount", "pQueueFamilyIndices", "preTransform",
               "compositeAlpha", "presentMode", "clipped", "oldSwapchain"]
        type CUnionType VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSwapchainCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSwapchainCreateInfoKHR where
        type FieldType "sType" VkSwapchainCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, sType}
        type FieldIsArray "sType" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSwapchainCreateInfoKHR where
        type FieldType "pNext" VkSwapchainCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkSwapchainCreateInfoKHR where
        type FieldType "flags" VkSwapchainCreateInfoKHR =
             VkSwapchainCreateFlagsKHR
        type FieldOptional "flags" VkSwapchainCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, flags}
        type FieldIsArray "flags" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSwapchainCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "surface" VkSwapchainCreateInfoKHR where
        type FieldType "surface" VkSwapchainCreateInfoKHR = VkSurfaceKHR
        type FieldOptional "surface" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "surface" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, surface}
        type FieldIsArray "surface" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, surface}

instance {-# OVERLAPPING #-}
         CanReadField "surface" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, surface})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, surface}

instance {-# OVERLAPPING #-}
         CanWriteField "surface" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, surface}

instance {-# OVERLAPPING #-}
         HasField "minImageCount" VkSwapchainCreateInfoKHR where
        type FieldType "minImageCount" VkSwapchainCreateInfoKHR = Word32
        type FieldOptional "minImageCount" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minImageCount" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, minImageCount}
        type FieldIsArray "minImageCount" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, minImageCount}

instance {-# OVERLAPPING #-}
         CanReadField "minImageCount" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, minImageCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, minImageCount}

instance {-# OVERLAPPING #-}
         CanWriteField "minImageCount" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, minImageCount}

instance {-# OVERLAPPING #-}
         HasField "imageFormat" VkSwapchainCreateInfoKHR where
        type FieldType "imageFormat" VkSwapchainCreateInfoKHR = VkFormat
        type FieldOptional "imageFormat" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageFormat" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, imageFormat}
        type FieldIsArray "imageFormat" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, imageFormat}

instance {-# OVERLAPPING #-}
         CanReadField "imageFormat" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageFormat})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageFormat}

instance {-# OVERLAPPING #-}
         CanWriteField "imageFormat" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageFormat}

instance {-# OVERLAPPING #-}
         HasField "imageColorSpace" VkSwapchainCreateInfoKHR where
        type FieldType "imageColorSpace" VkSwapchainCreateInfoKHR =
             VkColorSpaceKHR
        type FieldOptional "imageColorSpace" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "imageColorSpace" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, imageColorSpace}
        type FieldIsArray "imageColorSpace" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, imageColorSpace}

instance {-# OVERLAPPING #-}
         CanReadField "imageColorSpace" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageColorSpace})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageColorSpace}

instance {-# OVERLAPPING #-}
         CanWriteField "imageColorSpace" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageColorSpace}

instance {-# OVERLAPPING #-}
         HasField "imageExtent" VkSwapchainCreateInfoKHR where
        type FieldType "imageExtent" VkSwapchainCreateInfoKHR = VkExtent2D
        type FieldOptional "imageExtent" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageExtent" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, imageExtent}
        type FieldIsArray "imageExtent" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, imageExtent}

instance {-# OVERLAPPING #-}
         CanReadField "imageExtent" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "imageExtent" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageExtent}

instance {-# OVERLAPPING #-}
         HasField "imageArrayLayers" VkSwapchainCreateInfoKHR where
        type FieldType "imageArrayLayers" VkSwapchainCreateInfoKHR = Word32
        type FieldOptional "imageArrayLayers" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "imageArrayLayers" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, imageArrayLayers}
        type FieldIsArray "imageArrayLayers" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, imageArrayLayers}

instance {-# OVERLAPPING #-}
         CanReadField "imageArrayLayers" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageArrayLayers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageArrayLayers}

instance {-# OVERLAPPING #-}
         CanWriteField "imageArrayLayers" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageArrayLayers}

instance {-# OVERLAPPING #-}
         HasField "imageUsage" VkSwapchainCreateInfoKHR where
        type FieldType "imageUsage" VkSwapchainCreateInfoKHR =
             VkImageUsageFlags
        type FieldOptional "imageUsage" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageUsage" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, imageUsage}
        type FieldIsArray "imageUsage" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, imageUsage}

instance {-# OVERLAPPING #-}
         CanReadField "imageUsage" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageUsage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageUsage}

instance {-# OVERLAPPING #-}
         CanWriteField "imageUsage" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageUsage}

instance {-# OVERLAPPING #-}
         HasField "imageSharingMode" VkSwapchainCreateInfoKHR where
        type FieldType "imageSharingMode" VkSwapchainCreateInfoKHR =
             VkSharingMode
        type FieldOptional "imageSharingMode" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "imageSharingMode" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, imageSharingMode}
        type FieldIsArray "imageSharingMode" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, imageSharingMode}

instance {-# OVERLAPPING #-}
         CanReadField "imageSharingMode" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageSharingMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageSharingMode}

instance {-# OVERLAPPING #-}
         CanWriteField "imageSharingMode" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageSharingMode}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndexCount" VkSwapchainCreateInfoKHR where
        type FieldType "queueFamilyIndexCount" VkSwapchainCreateInfoKHR =
             Word32
        type FieldOptional "queueFamilyIndexCount" VkSwapchainCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndexCount" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, queueFamilyIndexCount}
        type FieldIsArray "queueFamilyIndexCount" VkSwapchainCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyIndexCount" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, queueFamilyIndexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyIndexCount" VkSwapchainCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         HasField "pQueueFamilyIndices" VkSwapchainCreateInfoKHR where
        type FieldType "pQueueFamilyIndices" VkSwapchainCreateInfoKHR =
             Ptr Word32
        type FieldOptional "pQueueFamilyIndices" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pQueueFamilyIndices" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, pQueueFamilyIndices}
        type FieldIsArray "pQueueFamilyIndices" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pQueueFamilyIndices" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, pQueueFamilyIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pQueueFamilyIndices" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         HasField "preTransform" VkSwapchainCreateInfoKHR where
        type FieldType "preTransform" VkSwapchainCreateInfoKHR =
             VkSurfaceTransformFlagBitsKHR
        type FieldOptional "preTransform" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "preTransform" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, preTransform}
        type FieldIsArray "preTransform" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, preTransform}

instance {-# OVERLAPPING #-}
         CanReadField "preTransform" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, preTransform})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, preTransform}

instance {-# OVERLAPPING #-}
         CanWriteField "preTransform" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, preTransform}

instance {-# OVERLAPPING #-}
         HasField "compositeAlpha" VkSwapchainCreateInfoKHR where
        type FieldType "compositeAlpha" VkSwapchainCreateInfoKHR =
             VkCompositeAlphaFlagBitsKHR
        type FieldOptional "compositeAlpha" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "compositeAlpha" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, compositeAlpha}
        type FieldIsArray "compositeAlpha" VkSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, compositeAlpha}

instance {-# OVERLAPPING #-}
         CanReadField "compositeAlpha" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, compositeAlpha})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, compositeAlpha}

instance {-# OVERLAPPING #-}
         CanWriteField "compositeAlpha" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, compositeAlpha}

instance {-# OVERLAPPING #-}
         HasField "presentMode" VkSwapchainCreateInfoKHR where
        type FieldType "presentMode" VkSwapchainCreateInfoKHR =
             VkPresentModeKHR
        type FieldOptional "presentMode" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "presentMode" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, presentMode}
        type FieldIsArray "presentMode" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, presentMode}

instance {-# OVERLAPPING #-}
         CanReadField "presentMode" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, presentMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, presentMode}

instance {-# OVERLAPPING #-}
         CanWriteField "presentMode" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, presentMode}

instance {-# OVERLAPPING #-}
         HasField "clipped" VkSwapchainCreateInfoKHR where
        type FieldType "clipped" VkSwapchainCreateInfoKHR = VkBool32
        type FieldOptional "clipped" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "clipped" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, clipped}
        type FieldIsArray "clipped" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, clipped}

instance {-# OVERLAPPING #-}
         CanReadField "clipped" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, clipped})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, clipped}

instance {-# OVERLAPPING #-}
         CanWriteField "clipped" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, clipped}

instance {-# OVERLAPPING #-}
         HasField "oldSwapchain" VkSwapchainCreateInfoKHR where
        type FieldType "oldSwapchain" VkSwapchainCreateInfoKHR =
             VkSwapchainKHR
        type FieldOptional "oldSwapchain" VkSwapchainCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "oldSwapchain" VkSwapchainCreateInfoKHR =
             #{offset VkSwapchainCreateInfoKHR, oldSwapchain}
        type FieldIsArray "oldSwapchain" VkSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCreateInfoKHR, oldSwapchain}

instance {-# OVERLAPPING #-}
         CanReadField "oldSwapchain" VkSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, oldSwapchain})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, oldSwapchain}

instance {-# OVERLAPPING #-}
         CanWriteField "oldSwapchain" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, oldSwapchain}

instance Show VkSwapchainCreateInfoKHR where
        showsPrec d x
          = showString "VkSwapchainCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "surface = " .
                                  showsPrec d (getField @"surface" x) .
                                    showString ", " .
                                      showString "minImageCount = " .
                                        showsPrec d (getField @"minImageCount" x) .
                                          showString ", " .
                                            showString "imageFormat = " .
                                              showsPrec d (getField @"imageFormat" x) .
                                                showString ", " .
                                                  showString "imageColorSpace = " .
                                                    showsPrec d (getField @"imageColorSpace" x) .
                                                      showString ", " .
                                                        showString "imageExtent = " .
                                                          showsPrec d (getField @"imageExtent" x) .
                                                            showString ", " .
                                                              showString "imageArrayLayers = " .
                                                                showsPrec d
                                                                  (getField @"imageArrayLayers" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "imageUsage = " .
                                                                      showsPrec d
                                                                        (getField @"imageUsage" x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "imageSharingMode = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"imageSharingMode"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "queueFamilyIndexCount = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"queueFamilyIndexCount"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "pQueueFamilyIndices = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"pQueueFamilyIndices"
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "preTransform = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (getField
                                                                                                   @"preTransform"
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "compositeAlpha = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (getField
                                                                                                         @"compositeAlpha"
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "presentMode = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (getField
                                                                                                               @"presentMode"
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "clipped = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (getField
                                                                                                                     @"clipped"
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "oldSwapchain = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (getField
                                                                                                                           @"oldSwapchain"
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showChar
                                                                                                                          '}'
