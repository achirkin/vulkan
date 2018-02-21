#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR
       (VkSwapchainCreateInfoKHR(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                       (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkColorSpaceKHR            (VkColorSpaceKHR)
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR   (VkCompositeAlphaFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkFormat                   (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags          (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkPresentModeKHR           (VkPresentModeKHR)
import           Graphics.Vulkan.Types.Enum.VkSharingMode              (VkSharingMode)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR (VkSurfaceTransformFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR  (VkSwapchainCreateFlagsKHR)
import           Graphics.Vulkan.Types.Handles                         (VkSurfaceKHR,
                                                                        VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.VkExtent2D               (VkExtent2D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSwapchainCreateInfoKHR.html VkSwapchainCreateInfoKHR registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkSwapchainCreateInfoKHR
         where
        type VkSTypeMType VkSwapchainCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, sType}

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

instance CanReadField "sType" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSwapchainCreateInfoKHR
         where
        type VkPNextMType VkSwapchainCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, pNext}

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

instance CanReadField "pNext" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkSwapchainCreateInfoKHR
         where
        type VkFlagsMType VkSwapchainCreateInfoKHR =
             VkSwapchainCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, flags}

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

instance CanReadField "flags" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkSurface VkSwapchainCreateInfoKHR
         where
        type VkSurfaceMType VkSwapchainCreateInfoKHR = VkSurfaceKHR

        {-# NOINLINE vkSurface #-}
        vkSurface x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, surface})

        {-# INLINE vkSurfaceByteOffset #-}
        vkSurfaceByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, surface}

        {-# INLINE readVkSurface #-}
        readVkSurface p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, surface}

        {-# INLINE writeVkSurface #-}
        writeVkSurface p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, surface}

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

instance CanReadField "surface" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSurface

        {-# INLINE readField #-}
        readField = readVkSurface

instance CanWriteField "surface" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSurface

instance {-# OVERLAPPING #-}
         HasVkMinImageCount VkSwapchainCreateInfoKHR where
        type VkMinImageCountMType VkSwapchainCreateInfoKHR = Word32

        {-# NOINLINE vkMinImageCount #-}
        vkMinImageCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, minImageCount})

        {-# INLINE vkMinImageCountByteOffset #-}
        vkMinImageCountByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, minImageCount}

        {-# INLINE readVkMinImageCount #-}
        readVkMinImageCount p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, minImageCount}

        {-# INLINE writeVkMinImageCount #-}
        writeVkMinImageCount p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, minImageCount}

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

instance CanReadField "minImageCount" VkSwapchainCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkMinImageCount

        {-# INLINE readField #-}
        readField = readVkMinImageCount

instance CanWriteField "minImageCount" VkSwapchainCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinImageCount

instance {-# OVERLAPPING #-}
         HasVkImageFormat VkSwapchainCreateInfoKHR where
        type VkImageFormatMType VkSwapchainCreateInfoKHR = VkFormat

        {-# NOINLINE vkImageFormat #-}
        vkImageFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageFormat})

        {-# INLINE vkImageFormatByteOffset #-}
        vkImageFormatByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, imageFormat}

        {-# INLINE readVkImageFormat #-}
        readVkImageFormat p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageFormat}

        {-# INLINE writeVkImageFormat #-}
        writeVkImageFormat p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageFormat}

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

instance CanReadField "imageFormat" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkImageFormat

        {-# INLINE readField #-}
        readField = readVkImageFormat

instance CanWriteField "imageFormat" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkImageFormat

instance {-# OVERLAPPING #-}
         HasVkImageColorSpace VkSwapchainCreateInfoKHR where
        type VkImageColorSpaceMType VkSwapchainCreateInfoKHR =
             VkColorSpaceKHR

        {-# NOINLINE vkImageColorSpace #-}
        vkImageColorSpace x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageColorSpace})

        {-# INLINE vkImageColorSpaceByteOffset #-}
        vkImageColorSpaceByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, imageColorSpace}

        {-# INLINE readVkImageColorSpace #-}
        readVkImageColorSpace p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageColorSpace}

        {-# INLINE writeVkImageColorSpace #-}
        writeVkImageColorSpace p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageColorSpace}

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

instance CanReadField "imageColorSpace" VkSwapchainCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkImageColorSpace

        {-# INLINE readField #-}
        readField = readVkImageColorSpace

instance CanWriteField "imageColorSpace" VkSwapchainCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkImageColorSpace

instance {-# OVERLAPPING #-}
         HasVkImageExtent VkSwapchainCreateInfoKHR where
        type VkImageExtentMType VkSwapchainCreateInfoKHR = VkExtent2D

        {-# NOINLINE vkImageExtent #-}
        vkImageExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageExtent})

        {-# INLINE vkImageExtentByteOffset #-}
        vkImageExtentByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, imageExtent}

        {-# INLINE readVkImageExtent #-}
        readVkImageExtent p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageExtent}

        {-# INLINE writeVkImageExtent #-}
        writeVkImageExtent p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageExtent}

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

instance CanReadField "imageExtent" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkImageExtent

        {-# INLINE readField #-}
        readField = readVkImageExtent

instance CanWriteField "imageExtent" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkImageExtent

instance {-# OVERLAPPING #-}
         HasVkImageArrayLayers VkSwapchainCreateInfoKHR where
        type VkImageArrayLayersMType VkSwapchainCreateInfoKHR = Word32

        {-# NOINLINE vkImageArrayLayers #-}
        vkImageArrayLayers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageArrayLayers})

        {-# INLINE vkImageArrayLayersByteOffset #-}
        vkImageArrayLayersByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, imageArrayLayers}

        {-# INLINE readVkImageArrayLayers #-}
        readVkImageArrayLayers p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageArrayLayers}

        {-# INLINE writeVkImageArrayLayers #-}
        writeVkImageArrayLayers p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageArrayLayers}

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

instance CanReadField "imageArrayLayers" VkSwapchainCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkImageArrayLayers

        {-# INLINE readField #-}
        readField = readVkImageArrayLayers

instance CanWriteField "imageArrayLayers" VkSwapchainCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkImageArrayLayers

instance {-# OVERLAPPING #-}
         HasVkImageUsage VkSwapchainCreateInfoKHR where
        type VkImageUsageMType VkSwapchainCreateInfoKHR = VkImageUsageFlags

        {-# NOINLINE vkImageUsage #-}
        vkImageUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageUsage})

        {-# INLINE vkImageUsageByteOffset #-}
        vkImageUsageByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, imageUsage}

        {-# INLINE readVkImageUsage #-}
        readVkImageUsage p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageUsage}

        {-# INLINE writeVkImageUsage #-}
        writeVkImageUsage p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageUsage}

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

instance CanReadField "imageUsage" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkImageUsage

        {-# INLINE readField #-}
        readField = readVkImageUsage

instance CanWriteField "imageUsage" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkImageUsage

instance {-# OVERLAPPING #-}
         HasVkImageSharingMode VkSwapchainCreateInfoKHR where
        type VkImageSharingModeMType VkSwapchainCreateInfoKHR =
             VkSharingMode

        {-# NOINLINE vkImageSharingMode #-}
        vkImageSharingMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, imageSharingMode})

        {-# INLINE vkImageSharingModeByteOffset #-}
        vkImageSharingModeByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, imageSharingMode}

        {-# INLINE readVkImageSharingMode #-}
        readVkImageSharingMode p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, imageSharingMode}

        {-# INLINE writeVkImageSharingMode #-}
        writeVkImageSharingMode p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, imageSharingMode}

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

instance CanReadField "imageSharingMode" VkSwapchainCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkImageSharingMode

        {-# INLINE readField #-}
        readField = readVkImageSharingMode

instance CanWriteField "imageSharingMode" VkSwapchainCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkImageSharingMode

instance {-# OVERLAPPING #-}
         HasVkQueueFamilyIndexCount VkSwapchainCreateInfoKHR where
        type VkQueueFamilyIndexCountMType VkSwapchainCreateInfoKHR = Word32

        {-# NOINLINE vkQueueFamilyIndexCount #-}
        vkQueueFamilyIndexCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, queueFamilyIndexCount})

        {-# INLINE vkQueueFamilyIndexCountByteOffset #-}
        vkQueueFamilyIndexCountByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, queueFamilyIndexCount}

        {-# INLINE readVkQueueFamilyIndexCount #-}
        readVkQueueFamilyIndexCount p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, queueFamilyIndexCount}

        {-# INLINE writeVkQueueFamilyIndexCount #-}
        writeVkQueueFamilyIndexCount p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, queueFamilyIndexCount}

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

instance CanReadField "queueFamilyIndexCount"
           VkSwapchainCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkQueueFamilyIndexCount

        {-# INLINE readField #-}
        readField = readVkQueueFamilyIndexCount

instance CanWriteField "queueFamilyIndexCount"
           VkSwapchainCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkQueueFamilyIndexCount

instance {-# OVERLAPPING #-}
         HasVkPQueueFamilyIndices VkSwapchainCreateInfoKHR where
        type VkPQueueFamilyIndicesMType VkSwapchainCreateInfoKHR =
             Ptr Word32

        {-# NOINLINE vkPQueueFamilyIndices #-}
        vkPQueueFamilyIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, pQueueFamilyIndices})

        {-# INLINE vkPQueueFamilyIndicesByteOffset #-}
        vkPQueueFamilyIndicesByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, pQueueFamilyIndices}

        {-# INLINE readVkPQueueFamilyIndices #-}
        readVkPQueueFamilyIndices p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, pQueueFamilyIndices}

        {-# INLINE writeVkPQueueFamilyIndices #-}
        writeVkPQueueFamilyIndices p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, pQueueFamilyIndices}

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

instance CanReadField "pQueueFamilyIndices"
           VkSwapchainCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPQueueFamilyIndices

        {-# INLINE readField #-}
        readField = readVkPQueueFamilyIndices

instance CanWriteField "pQueueFamilyIndices"
           VkSwapchainCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPQueueFamilyIndices

instance {-# OVERLAPPING #-}
         HasVkPreTransform VkSwapchainCreateInfoKHR where
        type VkPreTransformMType VkSwapchainCreateInfoKHR =
             VkSurfaceTransformFlagBitsKHR

        {-# NOINLINE vkPreTransform #-}
        vkPreTransform x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, preTransform})

        {-# INLINE vkPreTransformByteOffset #-}
        vkPreTransformByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, preTransform}

        {-# INLINE readVkPreTransform #-}
        readVkPreTransform p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, preTransform}

        {-# INLINE writeVkPreTransform #-}
        writeVkPreTransform p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, preTransform}

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

instance CanReadField "preTransform" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPreTransform

        {-# INLINE readField #-}
        readField = readVkPreTransform

instance CanWriteField "preTransform" VkSwapchainCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPreTransform

instance {-# OVERLAPPING #-}
         HasVkCompositeAlpha VkSwapchainCreateInfoKHR where
        type VkCompositeAlphaMType VkSwapchainCreateInfoKHR =
             VkCompositeAlphaFlagBitsKHR

        {-# NOINLINE vkCompositeAlpha #-}
        vkCompositeAlpha x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, compositeAlpha})

        {-# INLINE vkCompositeAlphaByteOffset #-}
        vkCompositeAlphaByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, compositeAlpha}

        {-# INLINE readVkCompositeAlpha #-}
        readVkCompositeAlpha p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, compositeAlpha}

        {-# INLINE writeVkCompositeAlpha #-}
        writeVkCompositeAlpha p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, compositeAlpha}

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

instance CanReadField "compositeAlpha" VkSwapchainCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkCompositeAlpha

        {-# INLINE readField #-}
        readField = readVkCompositeAlpha

instance CanWriteField "compositeAlpha" VkSwapchainCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkCompositeAlpha

instance {-# OVERLAPPING #-}
         HasVkPresentMode VkSwapchainCreateInfoKHR where
        type VkPresentModeMType VkSwapchainCreateInfoKHR = VkPresentModeKHR

        {-# NOINLINE vkPresentMode #-}
        vkPresentMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, presentMode})

        {-# INLINE vkPresentModeByteOffset #-}
        vkPresentModeByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, presentMode}

        {-# INLINE readVkPresentMode #-}
        readVkPresentMode p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, presentMode}

        {-# INLINE writeVkPresentMode #-}
        writeVkPresentMode p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, presentMode}

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

instance CanReadField "presentMode" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPresentMode

        {-# INLINE readField #-}
        readField = readVkPresentMode

instance CanWriteField "presentMode" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPresentMode

instance {-# OVERLAPPING #-} HasVkClipped VkSwapchainCreateInfoKHR
         where
        type VkClippedMType VkSwapchainCreateInfoKHR = VkBool32

        {-# NOINLINE vkClipped #-}
        vkClipped x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, clipped})

        {-# INLINE vkClippedByteOffset #-}
        vkClippedByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, clipped}

        {-# INLINE readVkClipped #-}
        readVkClipped p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, clipped}

        {-# INLINE writeVkClipped #-}
        writeVkClipped p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, clipped}

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

instance CanReadField "clipped" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkClipped

        {-# INLINE readField #-}
        readField = readVkClipped

instance CanWriteField "clipped" VkSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkClipped

instance {-# OVERLAPPING #-}
         HasVkOldSwapchain VkSwapchainCreateInfoKHR where
        type VkOldSwapchainMType VkSwapchainCreateInfoKHR = VkSwapchainKHR

        {-# NOINLINE vkOldSwapchain #-}
        vkOldSwapchain x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCreateInfoKHR, oldSwapchain})

        {-# INLINE vkOldSwapchainByteOffset #-}
        vkOldSwapchainByteOffset ~_
          = #{offset VkSwapchainCreateInfoKHR, oldSwapchain}

        {-# INLINE readVkOldSwapchain #-}
        readVkOldSwapchain p
          = peekByteOff p #{offset VkSwapchainCreateInfoKHR, oldSwapchain}

        {-# INLINE writeVkOldSwapchain #-}
        writeVkOldSwapchain p
          = pokeByteOff p #{offset VkSwapchainCreateInfoKHR, oldSwapchain}

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

instance CanReadField "oldSwapchain" VkSwapchainCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkOldSwapchain

        {-# INLINE readField #-}
        readField = readVkOldSwapchain

instance CanWriteField "oldSwapchain" VkSwapchainCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkOldSwapchain

instance Show VkSwapchainCreateInfoKHR where
        showsPrec d x
          = showString "VkSwapchainCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkSurface = " .
                                  showsPrec d (vkSurface x) .
                                    showString ", " .
                                      showString "vkMinImageCount = " .
                                        showsPrec d (vkMinImageCount x) .
                                          showString ", " .
                                            showString "vkImageFormat = " .
                                              showsPrec d (vkImageFormat x) .
                                                showString ", " .
                                                  showString "vkImageColorSpace = " .
                                                    showsPrec d (vkImageColorSpace x) .
                                                      showString ", " .
                                                        showString "vkImageExtent = " .
                                                          showsPrec d (vkImageExtent x) .
                                                            showString ", " .
                                                              showString "vkImageArrayLayers = " .
                                                                showsPrec d (vkImageArrayLayers x) .
                                                                  showString ", " .
                                                                    showString "vkImageUsage = " .
                                                                      showsPrec d (vkImageUsage x) .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkImageSharingMode = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkImageSharingMode x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkQueueFamilyIndexCount = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkQueueFamilyIndexCount
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "vkPQueueFamilyIndices = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (vkPQueueFamilyIndices
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "vkPreTransform = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (vkPreTransform
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "vkCompositeAlpha = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (vkCompositeAlpha
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "vkPresentMode = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (vkPresentMode
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "vkClipped = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (vkClipped
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "vkOldSwapchain = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (vkOldSwapchain
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showChar
                                                                                                                          '}'
