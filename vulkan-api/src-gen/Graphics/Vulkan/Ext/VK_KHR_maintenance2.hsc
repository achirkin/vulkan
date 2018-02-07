#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedFFITypes     #-}
{-# LANGUAGE ViewPatterns         #-}
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
import           Data.Int
import           Data.Void                        (Void)
import           Data.Word
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Core
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

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
