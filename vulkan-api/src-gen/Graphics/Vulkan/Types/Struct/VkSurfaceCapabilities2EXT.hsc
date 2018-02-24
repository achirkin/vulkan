#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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

instance {-# OVERLAPPING #-} HasVkSType VkSurfaceCapabilities2EXT
         where
        type VkSTypeMType VkSurfaceCapabilities2EXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, sType}

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

instance CanReadField "sType" VkSurfaceCapabilities2EXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSurfaceCapabilities2EXT
         where
        type VkPNextMType VkSurfaceCapabilities2EXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, pNext}

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

instance CanReadField "pNext" VkSurfaceCapabilities2EXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSurfaceCapabilities2EXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMinImageCount VkSurfaceCapabilities2EXT where
        type VkMinImageCountMType VkSurfaceCapabilities2EXT = Word32

        {-# NOINLINE vkMinImageCount #-}
        vkMinImageCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, minImageCount})

        {-# INLINE vkMinImageCountByteOffset #-}
        vkMinImageCountByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, minImageCount}

        {-# INLINE readVkMinImageCount #-}
        readVkMinImageCount p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, minImageCount}

        {-# INLINE writeVkMinImageCount #-}
        writeVkMinImageCount p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, minImageCount}

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

instance CanReadField "minImageCount" VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkMinImageCount

        {-# INLINE readField #-}
        readField = readVkMinImageCount

instance CanWriteField "minImageCount" VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinImageCount

instance {-# OVERLAPPING #-}
         HasVkMaxImageCount VkSurfaceCapabilities2EXT where
        type VkMaxImageCountMType VkSurfaceCapabilities2EXT = Word32

        {-# NOINLINE vkMaxImageCount #-}
        vkMaxImageCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, maxImageCount})

        {-# INLINE vkMaxImageCountByteOffset #-}
        vkMaxImageCountByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, maxImageCount}

        {-# INLINE readVkMaxImageCount #-}
        readVkMaxImageCount p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageCount}

        {-# INLINE writeVkMaxImageCount #-}
        writeVkMaxImageCount p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageCount}

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

instance CanReadField "maxImageCount" VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkMaxImageCount

        {-# INLINE readField #-}
        readField = readVkMaxImageCount

instance CanWriteField "maxImageCount" VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxImageCount

instance {-# OVERLAPPING #-}
         HasVkCurrentExtent VkSurfaceCapabilities2EXT where
        type VkCurrentExtentMType VkSurfaceCapabilities2EXT = VkExtent2D

        {-# NOINLINE vkCurrentExtent #-}
        vkCurrentExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, currentExtent})

        {-# INLINE vkCurrentExtentByteOffset #-}
        vkCurrentExtentByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, currentExtent}

        {-# INLINE readVkCurrentExtent #-}
        readVkCurrentExtent p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, currentExtent}

        {-# INLINE writeVkCurrentExtent #-}
        writeVkCurrentExtent p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, currentExtent}

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

instance CanReadField "currentExtent" VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkCurrentExtent

        {-# INLINE readField #-}
        readField = readVkCurrentExtent

instance CanWriteField "currentExtent" VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkCurrentExtent

instance {-# OVERLAPPING #-}
         HasVkMinImageExtent VkSurfaceCapabilities2EXT where
        type VkMinImageExtentMType VkSurfaceCapabilities2EXT = VkExtent2D

        {-# NOINLINE vkMinImageExtent #-}
        vkMinImageExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, minImageExtent})

        {-# INLINE vkMinImageExtentByteOffset #-}
        vkMinImageExtentByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, minImageExtent}

        {-# INLINE readVkMinImageExtent #-}
        readVkMinImageExtent p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, minImageExtent}

        {-# INLINE writeVkMinImageExtent #-}
        writeVkMinImageExtent p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, minImageExtent}

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

instance CanReadField "minImageExtent" VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkMinImageExtent

        {-# INLINE readField #-}
        readField = readVkMinImageExtent

instance CanWriteField "minImageExtent" VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinImageExtent

instance {-# OVERLAPPING #-}
         HasVkMaxImageExtent VkSurfaceCapabilities2EXT where
        type VkMaxImageExtentMType VkSurfaceCapabilities2EXT = VkExtent2D

        {-# NOINLINE vkMaxImageExtent #-}
        vkMaxImageExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, maxImageExtent})

        {-# INLINE vkMaxImageExtentByteOffset #-}
        vkMaxImageExtentByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, maxImageExtent}

        {-# INLINE readVkMaxImageExtent #-}
        readVkMaxImageExtent p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageExtent}

        {-# INLINE writeVkMaxImageExtent #-}
        writeVkMaxImageExtent p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageExtent}

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

instance CanReadField "maxImageExtent" VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkMaxImageExtent

        {-# INLINE readField #-}
        readField = readVkMaxImageExtent

instance CanWriteField "maxImageExtent" VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxImageExtent

instance {-# OVERLAPPING #-}
         HasVkMaxImageArrayLayers VkSurfaceCapabilities2EXT where
        type VkMaxImageArrayLayersMType VkSurfaceCapabilities2EXT = Word32

        {-# NOINLINE vkMaxImageArrayLayers #-}
        vkMaxImageArrayLayers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers})

        {-# INLINE vkMaxImageArrayLayersByteOffset #-}
        vkMaxImageArrayLayersByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}

        {-# INLINE readVkMaxImageArrayLayers #-}
        readVkMaxImageArrayLayers p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}

        {-# INLINE writeVkMaxImageArrayLayers #-}
        writeVkMaxImageArrayLayers p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}

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

instance CanReadField "maxImageArrayLayers"
           VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkMaxImageArrayLayers

        {-# INLINE readField #-}
        readField = readVkMaxImageArrayLayers

instance CanWriteField "maxImageArrayLayers"
           VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxImageArrayLayers

instance {-# OVERLAPPING #-}
         HasVkSupportedTransforms VkSurfaceCapabilities2EXT where
        type VkSupportedTransformsMType VkSurfaceCapabilities2EXT =
             VkSurfaceTransformFlagsKHR

        {-# NOINLINE vkSupportedTransforms #-}
        vkSupportedTransforms x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedTransforms})

        {-# INLINE vkSupportedTransformsByteOffset #-}
        vkSupportedTransformsByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, supportedTransforms}

        {-# INLINE readVkSupportedTransforms #-}
        readVkSupportedTransforms p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedTransforms}

        {-# INLINE writeVkSupportedTransforms #-}
        writeVkSupportedTransforms p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedTransforms}

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

instance CanReadField "supportedTransforms"
           VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkSupportedTransforms

        {-# INLINE readField #-}
        readField = readVkSupportedTransforms

instance CanWriteField "supportedTransforms"
           VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSupportedTransforms

instance {-# OVERLAPPING #-}
         HasVkCurrentTransform VkSurfaceCapabilities2EXT where
        type VkCurrentTransformMType VkSurfaceCapabilities2EXT =
             VkSurfaceTransformFlagBitsKHR

        {-# NOINLINE vkCurrentTransform #-}
        vkCurrentTransform x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, currentTransform})

        {-# INLINE vkCurrentTransformByteOffset #-}
        vkCurrentTransformByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, currentTransform}

        {-# INLINE readVkCurrentTransform #-}
        readVkCurrentTransform p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, currentTransform}

        {-# INLINE writeVkCurrentTransform #-}
        writeVkCurrentTransform p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, currentTransform}

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

instance CanReadField "currentTransform" VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkCurrentTransform

        {-# INLINE readField #-}
        readField = readVkCurrentTransform

instance CanWriteField "currentTransform" VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkCurrentTransform

instance {-# OVERLAPPING #-}
         HasVkSupportedCompositeAlpha VkSurfaceCapabilities2EXT where
        type VkSupportedCompositeAlphaMType VkSurfaceCapabilities2EXT =
             VkCompositeAlphaFlagsKHR

        {-# NOINLINE vkSupportedCompositeAlpha #-}
        vkSupportedCompositeAlpha x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha})

        {-# INLINE vkSupportedCompositeAlphaByteOffset #-}
        vkSupportedCompositeAlphaByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}

        {-# INLINE readVkSupportedCompositeAlpha #-}
        readVkSupportedCompositeAlpha p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}

        {-# INLINE writeVkSupportedCompositeAlpha #-}
        writeVkSupportedCompositeAlpha p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}

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

instance CanReadField "supportedCompositeAlpha"
           VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkSupportedCompositeAlpha

        {-# INLINE readField #-}
        readField = readVkSupportedCompositeAlpha

instance CanWriteField "supportedCompositeAlpha"
           VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSupportedCompositeAlpha

instance {-# OVERLAPPING #-}
         HasVkSupportedUsageFlags VkSurfaceCapabilities2EXT where
        type VkSupportedUsageFlagsMType VkSurfaceCapabilities2EXT =
             VkImageUsageFlags

        {-# NOINLINE vkSupportedUsageFlags #-}
        vkSupportedUsageFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags})

        {-# INLINE vkSupportedUsageFlagsByteOffset #-}
        vkSupportedUsageFlagsByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}

        {-# INLINE readVkSupportedUsageFlags #-}
        readVkSupportedUsageFlags p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}

        {-# INLINE writeVkSupportedUsageFlags #-}
        writeVkSupportedUsageFlags p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}

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

instance CanReadField "supportedUsageFlags"
           VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkSupportedUsageFlags

        {-# INLINE readField #-}
        readField = readVkSupportedUsageFlags

instance CanWriteField "supportedUsageFlags"
           VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSupportedUsageFlags

instance {-# OVERLAPPING #-}
         HasVkSupportedSurfaceCounters VkSurfaceCapabilities2EXT where
        type VkSupportedSurfaceCountersMType VkSurfaceCapabilities2EXT =
             VkSurfaceCounterFlagsEXT

        {-# NOINLINE vkSupportedSurfaceCounters #-}
        vkSupportedSurfaceCounters x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters})

        {-# INLINE vkSupportedSurfaceCountersByteOffset #-}
        vkSupportedSurfaceCountersByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}

        {-# INLINE readVkSupportedSurfaceCounters #-}
        readVkSupportedSurfaceCounters p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}

        {-# INLINE writeVkSupportedSurfaceCounters #-}
        writeVkSupportedSurfaceCounters p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}

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

instance CanReadField "supportedSurfaceCounters"
           VkSurfaceCapabilities2EXT
         where
        {-# INLINE getField #-}
        getField = vkSupportedSurfaceCounters

        {-# INLINE readField #-}
        readField = readVkSupportedSurfaceCounters

instance CanWriteField "supportedSurfaceCounters"
           VkSurfaceCapabilities2EXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSupportedSurfaceCounters

instance Show VkSurfaceCapabilities2EXT where
        showsPrec d x
          = showString "VkSurfaceCapabilities2EXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMinImageCount = " .
                            showsPrec d (vkMinImageCount x) .
                              showString ", " .
                                showString "vkMaxImageCount = " .
                                  showsPrec d (vkMaxImageCount x) .
                                    showString ", " .
                                      showString "vkCurrentExtent = " .
                                        showsPrec d (vkCurrentExtent x) .
                                          showString ", " .
                                            showString "vkMinImageExtent = " .
                                              showsPrec d (vkMinImageExtent x) .
                                                showString ", " .
                                                  showString "vkMaxImageExtent = " .
                                                    showsPrec d (vkMaxImageExtent x) .
                                                      showString ", " .
                                                        showString "vkMaxImageArrayLayers = " .
                                                          showsPrec d (vkMaxImageArrayLayers x) .
                                                            showString ", " .
                                                              showString "vkSupportedTransforms = "
                                                                .
                                                                showsPrec d
                                                                  (vkSupportedTransforms x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkCurrentTransform = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkCurrentTransform x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkSupportedCompositeAlpha = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkSupportedCompositeAlpha
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkSupportedUsageFlags = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkSupportedUsageFlags
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "vkSupportedSurfaceCounters = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (vkSupportedSurfaceCounters
                                                                                             x)
                                                                                          .
                                                                                          showChar
                                                                                            '}'
