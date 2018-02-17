#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_KHR_maintenance2
       (-- * Vulkan extension: @VK_KHR_maintenance2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Michael Worcester @michaelworcester@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @118@
        VkPhysicalDevicePointClippingPropertiesKHR(..),
        VkRenderPassInputAttachmentAspectCreateInfoKHR(..),
        VkInputAttachmentAspectReferenceKHR(..),
        VkImageViewUsageCreateInfoKHR(..),
        VkPipelineTessellationDomainOriginStateCreateInfoKHR(..),
        VK_KHR_MAINTENANCE2_SPEC_VERSION,
        pattern VK_KHR_MAINTENANCE2_SPEC_VERSION,
        VK_KHR_MAINTENANCE2_EXTENSION_NAME,
        pattern VK_KHR_MAINTENANCE2_EXTENSION_NAME,
        pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR,
        pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR,
        pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR,
        pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR)
       where
import           Foreign.C.String                                           (CString)
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           GHC.Ptr                                                    (Ptr (..))
import           Graphics.Vulkan.Base                                       (VkImageViewCreateInfo,
                                                                             VkPipelineTessellationStateCreateInfo,
                                                                             VkRenderPassCreateInfo)
import           Graphics.Vulkan.Common                                     (VkImageAspectFlags,
                                                                             VkImageCreateFlagBits (..),
                                                                             VkImageLayout (..),
                                                                             VkImageUsageFlags,
                                                                             VkPointClippingBehaviorKHR,
                                                                             VkStructureType,
                                                                             VkStructureType (..),
                                                                             VkTessellationDomainOriginKHR,
                                                                             Word32)
import           Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2 (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDevicePointClippingPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPointClippingBehaviorKHR      pointClippingBehavior;
--   > } VkPhysicalDevicePointClippingPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDevicePointClippingPropertiesKHR.html VkPhysicalDevicePointClippingPropertiesKHR registry at www.khronos.org>
data VkPhysicalDevicePointClippingPropertiesKHR = VkPhysicalDevicePointClippingPropertiesKHR## Addr##
                                                                                              ByteArray##

instance Eq VkPhysicalDevicePointClippingPropertiesKHR where
        (VkPhysicalDevicePointClippingPropertiesKHR## a _) ==
          x@(VkPhysicalDevicePointClippingPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDevicePointClippingPropertiesKHR where
        (VkPhysicalDevicePointClippingPropertiesKHR## a _) `compare`
          x@(VkPhysicalDevicePointClippingPropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDevicePointClippingPropertiesKHR where
        sizeOf ~_
          = #{size VkPhysicalDevicePointClippingPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDevicePointClippingPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDevicePointClippingPropertiesKHR
         where
        unsafeAddr (VkPhysicalDevicePointClippingPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDevicePointClippingPropertiesKHR## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDevicePointClippingPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDevicePointClippingPropertiesKHR
         where
        type StructFields VkPhysicalDevicePointClippingPropertiesKHR =
             '["sType", "pNext", "pointClippingBehavior"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDevicePointClippingPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDevicePointClippingPropertiesKHR =
             'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDevicePointClippingPropertiesKHR =
             '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDevicePointClippingPropertiesKHR where
        type VkSTypeMType VkPhysicalDevicePointClippingPropertiesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDevicePointClippingPropertiesKHR where
        type FieldType "sType" VkPhysicalDevicePointClippingPropertiesKHR =
             VkStructureType
        type FieldOptional "sType"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDevicePointClippingPropertiesKHR
             =
             #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType}
        type FieldIsArray "sType"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType}

instance CanReadField "sType"
           VkPhysicalDevicePointClippingPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDevicePointClippingPropertiesKHR where
        type VkPNextMType VkPhysicalDevicePointClippingPropertiesKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDevicePointClippingPropertiesKHR where
        type FieldType "pNext" VkPhysicalDevicePointClippingPropertiesKHR =
             Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDevicePointClippingPropertiesKHR
             =
             #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext}

instance CanReadField "pNext"
           VkPhysicalDevicePointClippingPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkPointClippingBehavior
           VkPhysicalDevicePointClippingPropertiesKHR
         where
        type VkPointClippingBehaviorMType
               VkPhysicalDevicePointClippingPropertiesKHR
             = VkPointClippingBehaviorKHR

        {-# NOINLINE vkPointClippingBehavior #-}
        vkPointClippingBehavior x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior})

        {-# INLINE vkPointClippingBehaviorByteOffset #-}
        vkPointClippingBehaviorByteOffset ~_
          = #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior}

        {-# INLINE readVkPointClippingBehavior #-}
        readVkPointClippingBehavior p
          = peekByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior}

        {-# INLINE writeVkPointClippingBehavior #-}
        writeVkPointClippingBehavior p
          = pokeByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior}

instance {-# OVERLAPPING #-}
         HasField "pointClippingBehavior"
           VkPhysicalDevicePointClippingPropertiesKHR
         where
        type FieldType "pointClippingBehavior"
               VkPhysicalDevicePointClippingPropertiesKHR
             = VkPointClippingBehaviorKHR
        type FieldOptional "pointClippingBehavior"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pointClippingBehavior"
               VkPhysicalDevicePointClippingPropertiesKHR
             =
             #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior}
        type FieldIsArray "pointClippingBehavior"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior}

instance CanReadField "pointClippingBehavior"
           VkPhysicalDevicePointClippingPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPointClippingBehavior

        {-# INLINE readField #-}
        readField = readVkPointClippingBehavior

instance Show VkPhysicalDevicePointClippingPropertiesKHR where
        showsPrec d x
          = showString "VkPhysicalDevicePointClippingPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPointClippingBehavior = " .
                            showsPrec d (vkPointClippingBehavior x) . showChar '}'

-- | > typedef struct VkRenderPassInputAttachmentAspectCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                     pNext;
--   >     uint32_t                        aspectReferenceCount;
--   >     const VkInputAttachmentAspectReferenceKHR* pAspectReferences;
--   > } VkRenderPassInputAttachmentAspectCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkRenderPassInputAttachmentAspectCreateInfoKHR.html VkRenderPassInputAttachmentAspectCreateInfoKHR registry at www.khronos.org>
data VkRenderPassInputAttachmentAspectCreateInfoKHR = VkRenderPassInputAttachmentAspectCreateInfoKHR## Addr##
                                                                                                      ByteArray##

instance Eq VkRenderPassInputAttachmentAspectCreateInfoKHR where
        (VkRenderPassInputAttachmentAspectCreateInfoKHR## a _) ==
          x@(VkRenderPassInputAttachmentAspectCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassInputAttachmentAspectCreateInfoKHR where
        (VkRenderPassInputAttachmentAspectCreateInfoKHR## a _) `compare`
          x@(VkRenderPassInputAttachmentAspectCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        sizeOf ~_
          = #{size VkRenderPassInputAttachmentAspectCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassInputAttachmentAspectCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        unsafeAddr (VkRenderPassInputAttachmentAspectCreateInfoKHR## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkRenderPassInputAttachmentAspectCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassInputAttachmentAspectCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type StructFields VkRenderPassInputAttachmentAspectCreateInfoKHR =
             '["sType", "pNext", "aspectReferenceCount", "pAspectReferences"] -- ' closing tick for hsc2hs
        type CUnionType VkRenderPassInputAttachmentAspectCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassInputAttachmentAspectCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassInputAttachmentAspectCreateInfoKHR =
             '[VkRenderPassCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkRenderPassInputAttachmentAspectCreateInfoKHR where
        type VkSTypeMType VkRenderPassInputAttachmentAspectCreateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type FieldType "sType"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = VkStructureType
        type FieldOptional "sType"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}
        type FieldIsArray "sType"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}

instance CanReadField "sType"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkRenderPassInputAttachmentAspectCreateInfoKHR where
        type VkPNextMType VkRenderPassInputAttachmentAspectCreateInfoKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type FieldType "pNext"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = Ptr Void
        type FieldOptional "pNext"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}
        type FieldIsArray "pNext"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}

instance CanReadField "pNext"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkAspectReferenceCount
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type VkAspectReferenceCountMType
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = Word32

        {-# NOINLINE vkAspectReferenceCount #-}
        vkAspectReferenceCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount})

        {-# INLINE vkAspectReferenceCountByteOffset #-}
        vkAspectReferenceCountByteOffset ~_
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}

        {-# INLINE readVkAspectReferenceCount #-}
        readVkAspectReferenceCount p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}

        {-# INLINE writeVkAspectReferenceCount #-}
        writeVkAspectReferenceCount p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}

instance {-# OVERLAPPING #-}
         HasField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type FieldType "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = Word32
        type FieldOptional "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}
        type FieldIsArray "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}

instance CanReadField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkAspectReferenceCount

        {-# INLINE readField #-}
        readField = readVkAspectReferenceCount

instance CanWriteField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkAspectReferenceCount

instance {-# OVERLAPPING #-}
         HasVkPAspectReferences
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type VkPAspectReferencesMType
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = Ptr VkInputAttachmentAspectReferenceKHR

        {-# NOINLINE vkPAspectReferences #-}
        vkPAspectReferences x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences})

        {-# INLINE vkPAspectReferencesByteOffset #-}
        vkPAspectReferencesByteOffset ~_
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}

        {-# INLINE readVkPAspectReferences #-}
        readVkPAspectReferences p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}

        {-# INLINE writeVkPAspectReferences #-}
        writeVkPAspectReferences p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}

instance {-# OVERLAPPING #-}
         HasField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type FieldType "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = Ptr VkInputAttachmentAspectReferenceKHR
        type FieldOptional "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}
        type FieldIsArray "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}

instance CanReadField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPAspectReferences

        {-# INLINE readField #-}
        readField = readVkPAspectReferences

instance CanWriteField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPAspectReferences

instance Show VkRenderPassInputAttachmentAspectCreateInfoKHR where
        showsPrec d x
          = showString "VkRenderPassInputAttachmentAspectCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAspectReferenceCount = " .
                            showsPrec d (vkAspectReferenceCount x) .
                              showString ", " .
                                showString "vkPAspectReferences = " .
                                  showsPrec d (vkPAspectReferences x) . showChar '}'

-- | > typedef struct VkInputAttachmentAspectReferenceKHR {
--   >     uint32_t                        subpass;
--   >     uint32_t                        inputAttachmentIndex;
--   >     VkImageAspectFlags              aspectMask;
--   > } VkInputAttachmentAspectReferenceKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkInputAttachmentAspectReferenceKHR.html VkInputAttachmentAspectReferenceKHR registry at www.khronos.org>
data VkInputAttachmentAspectReferenceKHR = VkInputAttachmentAspectReferenceKHR## Addr##
                                                                                ByteArray##

instance Eq VkInputAttachmentAspectReferenceKHR where
        (VkInputAttachmentAspectReferenceKHR## a _) ==
          x@(VkInputAttachmentAspectReferenceKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkInputAttachmentAspectReferenceKHR where
        (VkInputAttachmentAspectReferenceKHR## a _) `compare`
          x@(VkInputAttachmentAspectReferenceKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkInputAttachmentAspectReferenceKHR where
        sizeOf ~_ = #{size VkInputAttachmentAspectReferenceKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkInputAttachmentAspectReferenceKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkInputAttachmentAspectReferenceKHR
         where
        unsafeAddr (VkInputAttachmentAspectReferenceKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkInputAttachmentAspectReferenceKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkInputAttachmentAspectReferenceKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkInputAttachmentAspectReferenceKHR where
        type StructFields VkInputAttachmentAspectReferenceKHR =
             '["subpass", "inputAttachmentIndex", "aspectMask"] -- ' closing tick for hsc2hs
        type CUnionType VkInputAttachmentAspectReferenceKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkInputAttachmentAspectReferenceKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkInputAttachmentAspectReferenceKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSubpass VkInputAttachmentAspectReferenceKHR where
        type VkSubpassMType VkInputAttachmentAspectReferenceKHR = Word32

        {-# NOINLINE vkSubpass #-}
        vkSubpass x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReferenceKHR, subpass})

        {-# INLINE vkSubpassByteOffset #-}
        vkSubpassByteOffset ~_
          = #{offset VkInputAttachmentAspectReferenceKHR, subpass}

        {-# INLINE readVkSubpass #-}
        readVkSubpass p
          = peekByteOff p #{offset VkInputAttachmentAspectReferenceKHR, subpass}

        {-# INLINE writeVkSubpass #-}
        writeVkSubpass p
          = pokeByteOff p #{offset VkInputAttachmentAspectReferenceKHR, subpass}

instance {-# OVERLAPPING #-}
         HasField "subpass" VkInputAttachmentAspectReferenceKHR where
        type FieldType "subpass" VkInputAttachmentAspectReferenceKHR =
             Word32
        type FieldOptional "subpass" VkInputAttachmentAspectReferenceKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subpass" VkInputAttachmentAspectReferenceKHR =
             #{offset VkInputAttachmentAspectReferenceKHR, subpass}
        type FieldIsArray "subpass" VkInputAttachmentAspectReferenceKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInputAttachmentAspectReferenceKHR, subpass}

instance CanReadField "subpass" VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE getField #-}
        getField = vkSubpass

        {-# INLINE readField #-}
        readField = readVkSubpass

instance CanWriteField "subpass"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubpass

instance {-# OVERLAPPING #-}
         HasVkInputAttachmentIndex VkInputAttachmentAspectReferenceKHR where
        type VkInputAttachmentIndexMType
               VkInputAttachmentAspectReferenceKHR
             = Word32

        {-# NOINLINE vkInputAttachmentIndex #-}
        vkInputAttachmentIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex})

        {-# INLINE vkInputAttachmentIndexByteOffset #-}
        vkInputAttachmentIndexByteOffset ~_
          = #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}

        {-# INLINE readVkInputAttachmentIndex #-}
        readVkInputAttachmentIndex p
          = peekByteOff p #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}

        {-# INLINE writeVkInputAttachmentIndex #-}
        writeVkInputAttachmentIndex p
          = pokeByteOff p #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}

instance {-# OVERLAPPING #-}
         HasField "inputAttachmentIndex" VkInputAttachmentAspectReferenceKHR
         where
        type FieldType "inputAttachmentIndex"
               VkInputAttachmentAspectReferenceKHR
             = Word32
        type FieldOptional "inputAttachmentIndex"
               VkInputAttachmentAspectReferenceKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "inputAttachmentIndex"
               VkInputAttachmentAspectReferenceKHR
             =
             #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}
        type FieldIsArray "inputAttachmentIndex"
               VkInputAttachmentAspectReferenceKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInputAttachmentAspectReferenceKHR, inputAttachmentIndex}

instance CanReadField "inputAttachmentIndex"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE getField #-}
        getField = vkInputAttachmentIndex

        {-# INLINE readField #-}
        readField = readVkInputAttachmentIndex

instance CanWriteField "inputAttachmentIndex"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkInputAttachmentIndex

instance {-# OVERLAPPING #-}
         HasVkAspectMask VkInputAttachmentAspectReferenceKHR where
        type VkAspectMaskMType VkInputAttachmentAspectReferenceKHR =
             VkImageAspectFlags

        {-# NOINLINE vkAspectMask #-}
        vkAspectMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInputAttachmentAspectReferenceKHR, aspectMask})

        {-# INLINE vkAspectMaskByteOffset #-}
        vkAspectMaskByteOffset ~_
          = #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}

        {-# INLINE readVkAspectMask #-}
        readVkAspectMask p
          = peekByteOff p #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}

        {-# INLINE writeVkAspectMask #-}
        writeVkAspectMask p
          = pokeByteOff p #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkInputAttachmentAspectReferenceKHR where
        type FieldType "aspectMask" VkInputAttachmentAspectReferenceKHR =
             VkImageAspectFlags
        type FieldOptional "aspectMask" VkInputAttachmentAspectReferenceKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkInputAttachmentAspectReferenceKHR =
             #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}
        type FieldIsArray "aspectMask" VkInputAttachmentAspectReferenceKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInputAttachmentAspectReferenceKHR, aspectMask}

instance CanReadField "aspectMask"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE getField #-}
        getField = vkAspectMask

        {-# INLINE readField #-}
        readField = readVkAspectMask

instance CanWriteField "aspectMask"
           VkInputAttachmentAspectReferenceKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkAspectMask

instance Show VkInputAttachmentAspectReferenceKHR where
        showsPrec d x
          = showString "VkInputAttachmentAspectReferenceKHR {" .
              showString "vkSubpass = " .
                showsPrec d (vkSubpass x) .
                  showString ", " .
                    showString "vkInputAttachmentIndex = " .
                      showsPrec d (vkInputAttachmentIndex x) .
                        showString ", " .
                          showString "vkAspectMask = " .
                            showsPrec d (vkAspectMask x) . showChar '}'

-- | > typedef struct VkImageViewUsageCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkImageUsageFlags usage;
--   > } VkImageViewUsageCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageViewUsageCreateInfoKHR.html VkImageViewUsageCreateInfoKHR registry at www.khronos.org>
data VkImageViewUsageCreateInfoKHR = VkImageViewUsageCreateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkImageViewUsageCreateInfoKHR where
        (VkImageViewUsageCreateInfoKHR## a _) ==
          x@(VkImageViewUsageCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageViewUsageCreateInfoKHR where
        (VkImageViewUsageCreateInfoKHR## a _) `compare`
          x@(VkImageViewUsageCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageViewUsageCreateInfoKHR where
        sizeOf ~_ = #{size VkImageViewUsageCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageViewUsageCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageViewUsageCreateInfoKHR where
        unsafeAddr (VkImageViewUsageCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageViewUsageCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageViewUsageCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageViewUsageCreateInfoKHR where
        type StructFields VkImageViewUsageCreateInfoKHR =
             '["sType", "pNext", "usage"] -- ' closing tick for hsc2hs
        type CUnionType VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageViewUsageCreateInfoKHR =
             '[VkImageViewCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkImageViewUsageCreateInfoKHR where
        type VkSTypeMType VkImageViewUsageCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageViewUsageCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageViewUsageCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageViewUsageCreateInfoKHR where
        type FieldType "sType" VkImageViewUsageCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageViewUsageCreateInfoKHR =
             #{offset VkImageViewUsageCreateInfoKHR, sType}
        type FieldIsArray "sType" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfoKHR, sType}

instance CanReadField "sType" VkImageViewUsageCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImageViewUsageCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkImageViewUsageCreateInfoKHR where
        type VkPNextMType VkImageViewUsageCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageViewUsageCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageViewUsageCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageViewUsageCreateInfoKHR where
        type FieldType "pNext" VkImageViewUsageCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageViewUsageCreateInfoKHR =
             #{offset VkImageViewUsageCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfoKHR, pNext}

instance CanReadField "pNext" VkImageViewUsageCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImageViewUsageCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkUsage VkImageViewUsageCreateInfoKHR where
        type VkUsageMType VkImageViewUsageCreateInfoKHR = VkImageUsageFlags

        {-# NOINLINE vkUsage #-}
        vkUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfoKHR, usage})

        {-# INLINE vkUsageByteOffset #-}
        vkUsageByteOffset ~_
          = #{offset VkImageViewUsageCreateInfoKHR, usage}

        {-# INLINE readVkUsage #-}
        readVkUsage p
          = peekByteOff p #{offset VkImageViewUsageCreateInfoKHR, usage}

        {-# INLINE writeVkUsage #-}
        writeVkUsage p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfoKHR, usage}

instance {-# OVERLAPPING #-}
         HasField "usage" VkImageViewUsageCreateInfoKHR where
        type FieldType "usage" VkImageViewUsageCreateInfoKHR =
             VkImageUsageFlags
        type FieldOptional "usage" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkImageViewUsageCreateInfoKHR =
             #{offset VkImageViewUsageCreateInfoKHR, usage}
        type FieldIsArray "usage" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfoKHR, usage}

instance CanReadField "usage" VkImageViewUsageCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkUsage

        {-# INLINE readField #-}
        readField = readVkUsage

instance CanWriteField "usage" VkImageViewUsageCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkUsage

instance Show VkImageViewUsageCreateInfoKHR where
        showsPrec d x
          = showString "VkImageViewUsageCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkUsage = " . showsPrec d (vkUsage x) . showChar '}'

-- | > typedef struct VkPipelineTessellationDomainOriginStateCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkTessellationDomainOriginKHR    domainOrigin;
--   > } VkPipelineTessellationDomainOriginStateCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineTessellationDomainOriginStateCreateInfoKHR.html VkPipelineTessellationDomainOriginStateCreateInfoKHR registry at www.khronos.org>
data VkPipelineTessellationDomainOriginStateCreateInfoKHR = VkPipelineTessellationDomainOriginStateCreateInfoKHR## Addr##
                                                                                                                  ByteArray##

instance Eq VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        (VkPipelineTessellationDomainOriginStateCreateInfoKHR## a _) ==
          x@(VkPipelineTessellationDomainOriginStateCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        (VkPipelineTessellationDomainOriginStateCreateInfoKHR## a _)
          `compare`
          x@(VkPipelineTessellationDomainOriginStateCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        sizeOf ~_
          = #{size VkPipelineTessellationDomainOriginStateCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineTessellationDomainOriginStateCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        unsafeAddr
          (VkPipelineTessellationDomainOriginStateCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineTessellationDomainOriginStateCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineTessellationDomainOriginStateCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type StructFields
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = '["sType", "pNext", "domainOrigin"] -- ' closing tick for hsc2hs
        type CUnionType
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = '[VkPipelineTessellationStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type VkSTypeMType
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type FieldType "sType"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType}
        type FieldIsArray "sType"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType}

instance CanReadField "sType"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type VkPNextMType
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type FieldType "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext}
        type FieldIsArray "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext}

instance CanReadField "pNext"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDomainOrigin
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type VkDomainOriginMType
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = VkTessellationDomainOriginKHR

        {-# NOINLINE vkDomainOrigin #-}
        vkDomainOrigin x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin})

        {-# INLINE vkDomainOriginByteOffset #-}
        vkDomainOriginByteOffset ~_
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin}

        {-# INLINE readVkDomainOrigin #-}
        readVkDomainOrigin p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin}

        {-# INLINE writeVkDomainOrigin #-}
        writeVkDomainOrigin p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin}

instance {-# OVERLAPPING #-}
         HasField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type FieldType "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = VkTessellationDomainOriginKHR
        type FieldOptional "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin}
        type FieldIsArray "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin}

instance CanReadField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkDomainOrigin

        {-# INLINE readField #-}
        readField = readVkDomainOrigin

instance CanWriteField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDomainOrigin

instance Show VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        showsPrec d x
          = showString
              "VkPipelineTessellationDomainOriginStateCreateInfoKHR {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDomainOrigin = " .
                            showsPrec d (vkDomainOrigin x) . showChar '}'

pattern VK_KHR_MAINTENANCE2_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_MAINTENANCE2_SPEC_VERSION = 1

type VK_KHR_MAINTENANCE2_SPEC_VERSION = 1

pattern VK_KHR_MAINTENANCE2_EXTENSION_NAME :: CString

pattern VK_KHR_MAINTENANCE2_EXTENSION_NAME <-
        (is_VK_KHR_MAINTENANCE2_EXTENSION_NAME -> True)
  where VK_KHR_MAINTENANCE2_EXTENSION_NAME
          = _VK_KHR_MAINTENANCE2_EXTENSION_NAME

{-# INLINE _VK_KHR_MAINTENANCE2_EXTENSION_NAME #-}

_VK_KHR_MAINTENANCE2_EXTENSION_NAME :: CString
_VK_KHR_MAINTENANCE2_EXTENSION_NAME
  = Ptr "VK_KHR_maintenance2\NUL"##

{-# INLINE is_VK_KHR_MAINTENANCE2_EXTENSION_NAME #-}

is_VK_KHR_MAINTENANCE2_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_MAINTENANCE2_EXTENSION_NAME
  = eqCStrings _VK_KHR_MAINTENANCE2_EXTENSION_NAME

type VK_KHR_MAINTENANCE2_EXTENSION_NAME = "VK_KHR_maintenance2"

-- | bitpos = @7@
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR =
        VkImageCreateFlagBits 128

-- | bitpos = @8@
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR =
        VkImageCreateFlagBits 256

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR
        = VkStructureType 1000117000

pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR
        = VkStructureType 1000117001

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR =
        VkStructureType 1000117002

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR
        = VkStructureType 1000117003

pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR
        :: VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR
        = VkImageLayout 1000117000

pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR
        :: VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR
        = VkImageLayout 1000117001
