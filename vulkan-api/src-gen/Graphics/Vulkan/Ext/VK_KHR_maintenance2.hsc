#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
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
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkImageAspectFlags,
                                                   VkImageCreateFlagBits (..),
                                                   VkImageLayout (..),
                                                   VkImageUsageFlags,
                                                   VkPointClippingBehaviorKHR,
                                                   VkStructureType,
                                                   VkStructureType (..),
                                                   VkTessellationDomainOriginKHR,
                                                   Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDevicePointClippingPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPointClippingBehaviorKHR      pointClippingBehavior;
--   > } VkPhysicalDevicePointClippingPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDevicePointClippingPropertiesKHR.html VkPhysicalDevicePointClippingPropertiesKHR registry at www.khronos.org>
data VkPhysicalDevicePointClippingPropertiesKHR = VkPhysicalDevicePointClippingPropertiesKHR## ByteArray##

instance Eq VkPhysicalDevicePointClippingPropertiesKHR where
        (VkPhysicalDevicePointClippingPropertiesKHR## a) ==
          (VkPhysicalDevicePointClippingPropertiesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDevicePointClippingPropertiesKHR where
        (VkPhysicalDevicePointClippingPropertiesKHR## a) `compare`
          (VkPhysicalDevicePointClippingPropertiesKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDevicePointClippingPropertiesKHR where
        sizeOf ~_
          = #{size VkPhysicalDevicePointClippingPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDevicePointClippingPropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDevicePointClippingPropertiesKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDevicePointClippingPropertiesKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDevicePointClippingPropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDevicePointClippingPropertiesKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDevicePointClippingPropertiesKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDevicePointClippingPropertiesKHR
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDevicePointClippingPropertiesKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDevicePointClippingPropertiesKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDevicePointClippingPropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDevicePointClippingPropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDevicePointClippingPropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDevicePointClippingPropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDevicePointClippingPropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDevicePointClippingPropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkRenderPassInputAttachmentAspectCreateInfoKHR = VkRenderPassInputAttachmentAspectCreateInfoKHR## ByteArray##

instance Eq VkRenderPassInputAttachmentAspectCreateInfoKHR where
        (VkRenderPassInputAttachmentAspectCreateInfoKHR## a) ==
          (VkRenderPassInputAttachmentAspectCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassInputAttachmentAspectCreateInfoKHR where
        (VkRenderPassInputAttachmentAspectCreateInfoKHR## a) `compare`
          (VkRenderPassInputAttachmentAspectCreateInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        sizeOf ~_
          = #{size VkRenderPassInputAttachmentAspectCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassInputAttachmentAspectCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkRenderPassInputAttachmentAspectCreateInfoKHR),
            I## a <- alignment
                      (undefined :: VkRenderPassInputAttachmentAspectCreateInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkRenderPassInputAttachmentAspectCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkRenderPassInputAttachmentAspectCreateInfoKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkRenderPassInputAttachmentAspectCreateInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkRenderPassInputAttachmentAspectCreateInfoKHR),
            I## a <- alignment
                      (undefined :: VkRenderPassInputAttachmentAspectCreateInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkRenderPassInputAttachmentAspectCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkRenderPassInputAttachmentAspectCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkRenderPassInputAttachmentAspectCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkRenderPassInputAttachmentAspectCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkRenderPassInputAttachmentAspectCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkRenderPassInputAttachmentAspectCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkInputAttachmentAspectReferenceKHR = VkInputAttachmentAspectReferenceKHR## ByteArray##

instance Eq VkInputAttachmentAspectReferenceKHR where
        (VkInputAttachmentAspectReferenceKHR## a) ==
          (VkInputAttachmentAspectReferenceKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkInputAttachmentAspectReferenceKHR where
        (VkInputAttachmentAspectReferenceKHR## a) `compare`
          (VkInputAttachmentAspectReferenceKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkInputAttachmentAspectReferenceKHR where
        sizeOf ~_ = #{size VkInputAttachmentAspectReferenceKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkInputAttachmentAspectReferenceKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkInputAttachmentAspectReferenceKHR),
            I## a <- alignment
                      (undefined :: VkInputAttachmentAspectReferenceKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkInputAttachmentAspectReferenceKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkInputAttachmentAspectReferenceKHR## ba)
          | I## n <- sizeOf (undefined :: VkInputAttachmentAspectReferenceKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkInputAttachmentAspectReferenceKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkInputAttachmentAspectReferenceKHR),
            I## a <- alignment
                      (undefined :: VkInputAttachmentAspectReferenceKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkInputAttachmentAspectReferenceKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkInputAttachmentAspectReferenceKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkInputAttachmentAspectReferenceKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkInputAttachmentAspectReferenceKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkInputAttachmentAspectReferenceKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkInputAttachmentAspectReferenceKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkImageViewUsageCreateInfoKHR = VkImageViewUsageCreateInfoKHR## ByteArray##

instance Eq VkImageViewUsageCreateInfoKHR where
        (VkImageViewUsageCreateInfoKHR## a) ==
          (VkImageViewUsageCreateInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImageViewUsageCreateInfoKHR where
        (VkImageViewUsageCreateInfoKHR## a) `compare`
          (VkImageViewUsageCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImageViewUsageCreateInfoKHR where
        sizeOf ~_ = #{size VkImageViewUsageCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageViewUsageCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImageViewUsageCreateInfoKHR),
            I## a <- alignment (undefined :: VkImageViewUsageCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImageViewUsageCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImageViewUsageCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkImageViewUsageCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImageViewUsageCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImageViewUsageCreateInfoKHR),
            I## a <- alignment (undefined :: VkImageViewUsageCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImageViewUsageCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImageViewUsageCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImageViewUsageCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImageViewUsageCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImageViewUsageCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImageViewUsageCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkPipelineTessellationDomainOriginStateCreateInfoKHR = VkPipelineTessellationDomainOriginStateCreateInfoKHR## ByteArray##

instance Eq VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        (VkPipelineTessellationDomainOriginStateCreateInfoKHR## a) ==
          (VkPipelineTessellationDomainOriginStateCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        (VkPipelineTessellationDomainOriginStateCreateInfoKHR## a) `compare`
          (VkPipelineTessellationDomainOriginStateCreateInfoKHR## b)
          = cmpImmutableContent a b

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
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined ::
                         VkPipelineTessellationDomainOriginStateCreateInfoKHR),
            I## a <- alignment
                      (undefined :: VkPipelineTessellationDomainOriginStateCreateInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPipelineTessellationDomainOriginStateCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkPipelineTessellationDomainOriginStateCreateInfoKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPipelineTessellationDomainOriginStateCreateInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined ::
                         VkPipelineTessellationDomainOriginStateCreateInfoKHR),
            I## a <- alignment
                      (undefined :: VkPipelineTessellationDomainOriginStateCreateInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPipelineTessellationDomainOriginStateCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr
          (VkPipelineTessellationDomainOriginStateCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr##
              VkPipelineTessellationDomainOriginStateCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr
          (VkPipelineTessellationDomainOriginStateCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPipelineTessellationDomainOriginStateCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData
          x@(VkPipelineTessellationDomainOriginStateCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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

_VK_KHR_MAINTENANCE2_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_MAINTENANCE2_EXTENSION_NAME #-}
_VK_KHR_MAINTENANCE2_EXTENSION_NAME
  = Ptr "VK_KHR_maintenance2\NUL"##

is_VK_KHR_MAINTENANCE2_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_MAINTENANCE2_EXTENSION_NAME #-}
is_VK_KHR_MAINTENANCE2_EXTENSION_NAME
  = (_VK_KHR_MAINTENANCE2_EXTENSION_NAME ==)

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
