#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSurfaceCapabilitiesKHR
       (VkSurfaceCapabilitiesKHR(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR   (VkCompositeAlphaFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags          (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR (VkSurfaceTransformFlagBitsKHR,
                                                                        VkSurfaceTransformFlagsKHR)
import           Graphics.Vulkan.Types.Struct.VkExtent2D               (VkExtent2D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSurfaceCapabilitiesKHR.html VkSurfaceCapabilitiesKHR registry at www.khronos.org>
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
         HasVkMinImageCount VkSurfaceCapabilitiesKHR where
        type VkMinImageCountMType VkSurfaceCapabilitiesKHR = Word32

        {-# NOINLINE vkMinImageCount #-}
        vkMinImageCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, minImageCount})

        {-# INLINE vkMinImageCountByteOffset #-}
        vkMinImageCountByteOffset ~_
          = #{offset VkSurfaceCapabilitiesKHR, minImageCount}

        {-# INLINE readVkMinImageCount #-}
        readVkMinImageCount p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageCount}

        {-# INLINE writeVkMinImageCount #-}
        writeVkMinImageCount p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageCount}

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

instance CanReadField "minImageCount" VkSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMinImageCount

        {-# INLINE readField #-}
        readField = readVkMinImageCount

instance {-# OVERLAPPING #-}
         HasVkMaxImageCount VkSurfaceCapabilitiesKHR where
        type VkMaxImageCountMType VkSurfaceCapabilitiesKHR = Word32

        {-# NOINLINE vkMaxImageCount #-}
        vkMaxImageCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, maxImageCount})

        {-# INLINE vkMaxImageCountByteOffset #-}
        vkMaxImageCountByteOffset ~_
          = #{offset VkSurfaceCapabilitiesKHR, maxImageCount}

        {-# INLINE readVkMaxImageCount #-}
        readVkMaxImageCount p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageCount}

        {-# INLINE writeVkMaxImageCount #-}
        writeVkMaxImageCount p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageCount}

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

instance CanReadField "maxImageCount" VkSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxImageCount

        {-# INLINE readField #-}
        readField = readVkMaxImageCount

instance {-# OVERLAPPING #-}
         HasVkCurrentExtent VkSurfaceCapabilitiesKHR where
        type VkCurrentExtentMType VkSurfaceCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkCurrentExtent #-}
        vkCurrentExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, currentExtent})

        {-# INLINE vkCurrentExtentByteOffset #-}
        vkCurrentExtentByteOffset ~_
          = #{offset VkSurfaceCapabilitiesKHR, currentExtent}

        {-# INLINE readVkCurrentExtent #-}
        readVkCurrentExtent p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, currentExtent}

        {-# INLINE writeVkCurrentExtent #-}
        writeVkCurrentExtent p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, currentExtent}

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

instance CanReadField "currentExtent" VkSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCurrentExtent

        {-# INLINE readField #-}
        readField = readVkCurrentExtent

instance {-# OVERLAPPING #-}
         HasVkMinImageExtent VkSurfaceCapabilitiesKHR where
        type VkMinImageExtentMType VkSurfaceCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMinImageExtent #-}
        vkMinImageExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, minImageExtent})

        {-# INLINE vkMinImageExtentByteOffset #-}
        vkMinImageExtentByteOffset ~_
          = #{offset VkSurfaceCapabilitiesKHR, minImageExtent}

        {-# INLINE readVkMinImageExtent #-}
        readVkMinImageExtent p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageExtent}

        {-# INLINE writeVkMinImageExtent #-}
        writeVkMinImageExtent p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageExtent}

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

instance CanReadField "minImageExtent" VkSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMinImageExtent

        {-# INLINE readField #-}
        readField = readVkMinImageExtent

instance {-# OVERLAPPING #-}
         HasVkMaxImageExtent VkSurfaceCapabilitiesKHR where
        type VkMaxImageExtentMType VkSurfaceCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMaxImageExtent #-}
        vkMaxImageExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, maxImageExtent})

        {-# INLINE vkMaxImageExtentByteOffset #-}
        vkMaxImageExtentByteOffset ~_
          = #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}

        {-# INLINE readVkMaxImageExtent #-}
        readVkMaxImageExtent p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}

        {-# INLINE writeVkMaxImageExtent #-}
        writeVkMaxImageExtent p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}

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

instance CanReadField "maxImageExtent" VkSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxImageExtent

        {-# INLINE readField #-}
        readField = readVkMaxImageExtent

instance {-# OVERLAPPING #-}
         HasVkMaxImageArrayLayers VkSurfaceCapabilitiesKHR where
        type VkMaxImageArrayLayersMType VkSurfaceCapabilitiesKHR = Word32

        {-# NOINLINE vkMaxImageArrayLayers #-}
        vkMaxImageArrayLayers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers})

        {-# INLINE vkMaxImageArrayLayersByteOffset #-}
        vkMaxImageArrayLayersByteOffset ~_
          = #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}

        {-# INLINE readVkMaxImageArrayLayers #-}
        readVkMaxImageArrayLayers p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}

        {-# INLINE writeVkMaxImageArrayLayers #-}
        writeVkMaxImageArrayLayers p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}

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

instance CanReadField "maxImageArrayLayers"
           VkSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxImageArrayLayers

        {-# INLINE readField #-}
        readField = readVkMaxImageArrayLayers

instance {-# OVERLAPPING #-}
         HasVkSupportedTransforms VkSurfaceCapabilitiesKHR where
        type VkSupportedTransformsMType VkSurfaceCapabilitiesKHR =
             VkSurfaceTransformFlagsKHR

        {-# NOINLINE vkSupportedTransforms #-}
        vkSupportedTransforms x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, supportedTransforms})

        {-# INLINE vkSupportedTransformsByteOffset #-}
        vkSupportedTransformsByteOffset ~_
          = #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}

        {-# INLINE readVkSupportedTransforms #-}
        readVkSupportedTransforms p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}

        {-# INLINE writeVkSupportedTransforms #-}
        writeVkSupportedTransforms p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}

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

instance CanReadField "supportedTransforms"
           VkSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSupportedTransforms

        {-# INLINE readField #-}
        readField = readVkSupportedTransforms

instance {-# OVERLAPPING #-}
         HasVkCurrentTransform VkSurfaceCapabilitiesKHR where
        type VkCurrentTransformMType VkSurfaceCapabilitiesKHR =
             VkSurfaceTransformFlagBitsKHR

        {-# NOINLINE vkCurrentTransform #-}
        vkCurrentTransform x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, currentTransform})

        {-# INLINE vkCurrentTransformByteOffset #-}
        vkCurrentTransformByteOffset ~_
          = #{offset VkSurfaceCapabilitiesKHR, currentTransform}

        {-# INLINE readVkCurrentTransform #-}
        readVkCurrentTransform p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, currentTransform}

        {-# INLINE writeVkCurrentTransform #-}
        writeVkCurrentTransform p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, currentTransform}

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

instance CanReadField "currentTransform" VkSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCurrentTransform

        {-# INLINE readField #-}
        readField = readVkCurrentTransform

instance {-# OVERLAPPING #-}
         HasVkSupportedCompositeAlpha VkSurfaceCapabilitiesKHR where
        type VkSupportedCompositeAlphaMType VkSurfaceCapabilitiesKHR =
             VkCompositeAlphaFlagsKHR

        {-# NOINLINE vkSupportedCompositeAlpha #-}
        vkSupportedCompositeAlpha x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha})

        {-# INLINE vkSupportedCompositeAlphaByteOffset #-}
        vkSupportedCompositeAlphaByteOffset ~_
          = #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}

        {-# INLINE readVkSupportedCompositeAlpha #-}
        readVkSupportedCompositeAlpha p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}

        {-# INLINE writeVkSupportedCompositeAlpha #-}
        writeVkSupportedCompositeAlpha p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}

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

instance CanReadField "supportedCompositeAlpha"
           VkSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSupportedCompositeAlpha

        {-# INLINE readField #-}
        readField = readVkSupportedCompositeAlpha

instance {-# OVERLAPPING #-}
         HasVkSupportedUsageFlags VkSurfaceCapabilitiesKHR where
        type VkSupportedUsageFlagsMType VkSurfaceCapabilitiesKHR =
             VkImageUsageFlags

        {-# NOINLINE vkSupportedUsageFlags #-}
        vkSupportedUsageFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags})

        {-# INLINE vkSupportedUsageFlagsByteOffset #-}
        vkSupportedUsageFlagsByteOffset ~_
          = #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}

        {-# INLINE readVkSupportedUsageFlags #-}
        readVkSupportedUsageFlags p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}

        {-# INLINE writeVkSupportedUsageFlags #-}
        writeVkSupportedUsageFlags p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}

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

instance CanReadField "supportedUsageFlags"
           VkSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSupportedUsageFlags

        {-# INLINE readField #-}
        readField = readVkSupportedUsageFlags

instance Show VkSurfaceCapabilitiesKHR where
        showsPrec d x
          = showString "VkSurfaceCapabilitiesKHR {" .
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
                                                  showString "vkSupportedTransforms = " .
                                                    showsPrec d (vkSupportedTransforms x) .
                                                      showString ", " .
                                                        showString "vkCurrentTransform = " .
                                                          showsPrec d (vkCurrentTransform x) .
                                                            showString ", " .
                                                              showString
                                                                "vkSupportedCompositeAlpha = "
                                                                .
                                                                showsPrec d
                                                                  (vkSupportedCompositeAlpha x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkSupportedUsageFlags = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkSupportedUsageFlags x)
                                                                        . showChar '}'
