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
module Graphics.Vulkan.Ext.VK_EXT_blend_operation_advanced
       (-- * Vulkan extension: @VK_EXT_blend_operation_advanced@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @149@
        VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT(..),
        VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT(..),
        VkPipelineColorBlendAdvancedStateCreateInfoEXT(..),
        VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION,
        pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION,
        VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME,
        pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT,
        pattern VK_BLEND_OP_ZERO_EXT, pattern VK_BLEND_OP_SRC_EXT,
        pattern VK_BLEND_OP_DST_EXT, pattern VK_BLEND_OP_SRC_OVER_EXT,
        pattern VK_BLEND_OP_DST_OVER_EXT, pattern VK_BLEND_OP_SRC_IN_EXT,
        pattern VK_BLEND_OP_DST_IN_EXT, pattern VK_BLEND_OP_SRC_OUT_EXT,
        pattern VK_BLEND_OP_DST_OUT_EXT, pattern VK_BLEND_OP_SRC_ATOP_EXT,
        pattern VK_BLEND_OP_DST_ATOP_EXT, pattern VK_BLEND_OP_XOR_EXT,
        pattern VK_BLEND_OP_MULTIPLY_EXT, pattern VK_BLEND_OP_SCREEN_EXT,
        pattern VK_BLEND_OP_OVERLAY_EXT, pattern VK_BLEND_OP_DARKEN_EXT,
        pattern VK_BLEND_OP_LIGHTEN_EXT,
        pattern VK_BLEND_OP_COLORDODGE_EXT,
        pattern VK_BLEND_OP_COLORBURN_EXT,
        pattern VK_BLEND_OP_HARDLIGHT_EXT,
        pattern VK_BLEND_OP_SOFTLIGHT_EXT,
        pattern VK_BLEND_OP_DIFFERENCE_EXT,
        pattern VK_BLEND_OP_EXCLUSION_EXT, pattern VK_BLEND_OP_INVERT_EXT,
        pattern VK_BLEND_OP_INVERT_RGB_EXT,
        pattern VK_BLEND_OP_LINEARDODGE_EXT,
        pattern VK_BLEND_OP_LINEARBURN_EXT,
        pattern VK_BLEND_OP_VIVIDLIGHT_EXT,
        pattern VK_BLEND_OP_LINEARLIGHT_EXT,
        pattern VK_BLEND_OP_PINLIGHT_EXT, pattern VK_BLEND_OP_HARDMIX_EXT,
        pattern VK_BLEND_OP_HSL_HUE_EXT,
        pattern VK_BLEND_OP_HSL_SATURATION_EXT,
        pattern VK_BLEND_OP_HSL_COLOR_EXT,
        pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT,
        pattern VK_BLEND_OP_PLUS_EXT, pattern VK_BLEND_OP_PLUS_CLAMPED_EXT,
        pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT,
        pattern VK_BLEND_OP_PLUS_DARKER_EXT, pattern VK_BLEND_OP_MINUS_EXT,
        pattern VK_BLEND_OP_MINUS_CLAMPED_EXT,
        pattern VK_BLEND_OP_CONTRAST_EXT,
        pattern VK_BLEND_OP_INVERT_OVG_EXT, pattern VK_BLEND_OP_RED_EXT,
        pattern VK_BLEND_OP_GREEN_EXT, pattern VK_BLEND_OP_BLUE_EXT,
        pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT)
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

data VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT = VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## ByteArray##

instance Eq VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
        (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## a) ==
          (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## a) `compare`
          (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr##
              VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr
          (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData
          x@(VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
        type VkSTypeMType VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
        type VkPNextMType VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkAdvancedBlendCoherentOperations
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        type VkAdvancedBlendCoherentOperationsMType
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = VkBool32

        {-# NOINLINE vkAdvancedBlendCoherentOperations #-}
        vkAdvancedBlendCoherentOperations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations})

        {-# INLINE vkAdvancedBlendCoherentOperationsByteOffset #-}
        vkAdvancedBlendCoherentOperationsByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

        {-# INLINE readVkAdvancedBlendCoherentOperations #-}
        readVkAdvancedBlendCoherentOperations p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

        {-# INLINE writeVkAdvancedBlendCoherentOperations #-}
        writeVkAdvancedBlendCoherentOperations p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

instance Show VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        showsPrec d x
          = showString "VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAdvancedBlendCoherentOperations = " .
                            showsPrec d (vkAdvancedBlendCoherentOperations x) . showChar '}'

data VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT = VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## ByteArray##

instance Eq VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## a) ==
          (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## a) `compare`
          (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr##
              VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr
          (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData
          x@(VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type VkSTypeMType
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type VkPNextMType
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkAdvancedBlendMaxColorAttachments
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type VkAdvancedBlendMaxColorAttachmentsMType
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = Word32

        {-# NOINLINE vkAdvancedBlendMaxColorAttachments #-}
        vkAdvancedBlendMaxColorAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments})

        {-# INLINE vkAdvancedBlendMaxColorAttachmentsByteOffset #-}
        vkAdvancedBlendMaxColorAttachmentsByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments}

        {-# INLINE readVkAdvancedBlendMaxColorAttachments #-}
        readVkAdvancedBlendMaxColorAttachments p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments}

        {-# INLINE writeVkAdvancedBlendMaxColorAttachments #-}
        writeVkAdvancedBlendMaxColorAttachments p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments}

instance {-# OVERLAPPING #-}
         HasVkAdvancedBlendIndependentBlend
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type VkAdvancedBlendIndependentBlendMType
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32

        {-# NOINLINE vkAdvancedBlendIndependentBlend #-}
        vkAdvancedBlendIndependentBlend x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend})

        {-# INLINE vkAdvancedBlendIndependentBlendByteOffset #-}
        vkAdvancedBlendIndependentBlendByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend}

        {-# INLINE readVkAdvancedBlendIndependentBlend #-}
        readVkAdvancedBlendIndependentBlend p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend}

        {-# INLINE writeVkAdvancedBlendIndependentBlend #-}
        writeVkAdvancedBlendIndependentBlend p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend}

instance {-# OVERLAPPING #-}
         HasVkAdvancedBlendNonPremultipliedSrcColor
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type VkAdvancedBlendNonPremultipliedSrcColorMType
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32

        {-# NOINLINE vkAdvancedBlendNonPremultipliedSrcColor #-}
        vkAdvancedBlendNonPremultipliedSrcColor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor})

        {-# INLINE vkAdvancedBlendNonPremultipliedSrcColorByteOffset #-}
        vkAdvancedBlendNonPremultipliedSrcColorByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor}

        {-# INLINE readVkAdvancedBlendNonPremultipliedSrcColor #-}
        readVkAdvancedBlendNonPremultipliedSrcColor p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor}

        {-# INLINE writeVkAdvancedBlendNonPremultipliedSrcColor #-}
        writeVkAdvancedBlendNonPremultipliedSrcColor p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor}

instance {-# OVERLAPPING #-}
         HasVkAdvancedBlendNonPremultipliedDstColor
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type VkAdvancedBlendNonPremultipliedDstColorMType
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32

        {-# NOINLINE vkAdvancedBlendNonPremultipliedDstColor #-}
        vkAdvancedBlendNonPremultipliedDstColor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor})

        {-# INLINE vkAdvancedBlendNonPremultipliedDstColorByteOffset #-}
        vkAdvancedBlendNonPremultipliedDstColorByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor}

        {-# INLINE readVkAdvancedBlendNonPremultipliedDstColor #-}
        readVkAdvancedBlendNonPremultipliedDstColor p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor}

        {-# INLINE writeVkAdvancedBlendNonPremultipliedDstColor #-}
        writeVkAdvancedBlendNonPremultipliedDstColor p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor}

instance {-# OVERLAPPING #-}
         HasVkAdvancedBlendCorrelatedOverlap
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type VkAdvancedBlendCorrelatedOverlapMType
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32

        {-# NOINLINE vkAdvancedBlendCorrelatedOverlap #-}
        vkAdvancedBlendCorrelatedOverlap x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap})

        {-# INLINE vkAdvancedBlendCorrelatedOverlapByteOffset #-}
        vkAdvancedBlendCorrelatedOverlapByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap}

        {-# INLINE readVkAdvancedBlendCorrelatedOverlap #-}
        readVkAdvancedBlendCorrelatedOverlap p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap}

        {-# INLINE writeVkAdvancedBlendCorrelatedOverlap #-}
        writeVkAdvancedBlendCorrelatedOverlap p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap}

instance {-# OVERLAPPING #-}
         HasVkAdvancedBlendAllOperations
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type VkAdvancedBlendAllOperationsMType
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32

        {-# NOINLINE vkAdvancedBlendAllOperations #-}
        vkAdvancedBlendAllOperations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations})

        {-# INLINE vkAdvancedBlendAllOperationsByteOffset #-}
        vkAdvancedBlendAllOperationsByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations}

        {-# INLINE readVkAdvancedBlendAllOperations #-}
        readVkAdvancedBlendAllOperations p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations}

        {-# INLINE writeVkAdvancedBlendAllOperations #-}
        writeVkAdvancedBlendAllOperations p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations}

instance Show VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        showsPrec d x
          = showString
              "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAdvancedBlendMaxColorAttachments = " .
                            showsPrec d (vkAdvancedBlendMaxColorAttachments x) .
                              showString ", " .
                                showString "vkAdvancedBlendIndependentBlend = " .
                                  showsPrec d (vkAdvancedBlendIndependentBlend x) .
                                    showString ", " .
                                      showString "vkAdvancedBlendNonPremultipliedSrcColor = " .
                                        showsPrec d (vkAdvancedBlendNonPremultipliedSrcColor x) .
                                          showString ", " .
                                            showString "vkAdvancedBlendNonPremultipliedDstColor = "
                                              .
                                              showsPrec d
                                                (vkAdvancedBlendNonPremultipliedDstColor x)
                                                .
                                                showString ", " .
                                                  showString "vkAdvancedBlendCorrelatedOverlap = " .
                                                    showsPrec d (vkAdvancedBlendCorrelatedOverlap x)
                                                      .
                                                      showString ", " .
                                                        showString "vkAdvancedBlendAllOperations = "
                                                          .
                                                          showsPrec d
                                                            (vkAdvancedBlendAllOperations x)
                                                            . showChar '}'

data VkPipelineColorBlendAdvancedStateCreateInfoEXT = VkPipelineColorBlendAdvancedStateCreateInfoEXT## ByteArray##

instance Eq VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a) ==
          (VkPipelineColorBlendAdvancedStateCreateInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a) `compare`
          (VkPipelineColorBlendAdvancedStateCreateInfoEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineColorBlendAdvancedStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineColorBlendAdvancedStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPipelineColorBlendAdvancedStateCreateInfoEXT),
            I## a <- alignment
                      (undefined :: VkPipelineColorBlendAdvancedStateCreateInfoEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPipelineColorBlendAdvancedStateCreateInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkPipelineColorBlendAdvancedStateCreateInfoEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkPipelineColorBlendAdvancedStateCreateInfoEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPipelineColorBlendAdvancedStateCreateInfoEXT),
            I## a <- alignment
                      (undefined :: VkPipelineColorBlendAdvancedStateCreateInfoEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPipelineColorBlendAdvancedStateCreateInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPipelineColorBlendAdvancedStateCreateInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPipelineColorBlendAdvancedStateCreateInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPipelineColorBlendAdvancedStateCreateInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPipelineColorBlendAdvancedStateCreateInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPipelineColorBlendAdvancedStateCreateInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        type VkSTypeMType VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        type VkPNextMType VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkSrcPremultiplied
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type VkSrcPremultipliedMType
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBool32

        {-# NOINLINE vkSrcPremultiplied #-}
        vkSrcPremultiplied x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied})

        {-# INLINE vkSrcPremultipliedByteOffset #-}
        vkSrcPremultipliedByteOffset ~_
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

        {-# INLINE readVkSrcPremultiplied #-}
        readVkSrcPremultiplied p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

        {-# INLINE writeVkSrcPremultiplied #-}
        writeVkSrcPremultiplied p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

instance {-# OVERLAPPING #-}
         HasVkDstPremultiplied
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type VkDstPremultipliedMType
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBool32

        {-# NOINLINE vkDstPremultiplied #-}
        vkDstPremultiplied x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied})

        {-# INLINE vkDstPremultipliedByteOffset #-}
        vkDstPremultipliedByteOffset ~_
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

        {-# INLINE readVkDstPremultiplied #-}
        readVkDstPremultiplied p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

        {-# INLINE writeVkDstPremultiplied #-}
        writeVkDstPremultiplied p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

instance {-# OVERLAPPING #-}
         HasVkBlendOverlap VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type VkBlendOverlapMType
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBlendOverlapEXT

        {-# NOINLINE vkBlendOverlap #-}
        vkBlendOverlap x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap})

        {-# INLINE vkBlendOverlapByteOffset #-}
        vkBlendOverlapByteOffset ~_
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

        {-# INLINE readVkBlendOverlap #-}
        readVkBlendOverlap p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

        {-# INLINE writeVkBlendOverlap #-}
        writeVkBlendOverlap p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

instance Show VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineColorBlendAdvancedStateCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSrcPremultiplied = " .
                            showsPrec d (vkSrcPremultiplied x) .
                              showString ", " .
                                showString "vkDstPremultiplied = " .
                                  showsPrec d (vkDstPremultiplied x) .
                                    showString ", " .
                                      showString "vkBlendOverlap = " .
                                        showsPrec d (vkBlendOverlap x) . showChar '}'

pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2

type VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2

pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: CString

pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME <-
        (is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME -> True)
  where VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
          = _VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME

_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME #-}
_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  = Ptr "VK_EXT_blend_operation_advanced\NUL"##

is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME ::
                                                  CString -> Bool

{-# INLINE is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME #-}
is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  = (_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME ==)

type VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME =
     "VK_EXT_blend_operation_advanced"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
        = VkStructureType 1000148000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
        = VkStructureType 1000148001

pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
        = VkStructureType 1000148002

pattern VK_BLEND_OP_ZERO_EXT :: VkBlendOp

pattern VK_BLEND_OP_ZERO_EXT = VkBlendOp 1000148000

pattern VK_BLEND_OP_SRC_EXT :: VkBlendOp

pattern VK_BLEND_OP_SRC_EXT = VkBlendOp 1000148001

pattern VK_BLEND_OP_DST_EXT :: VkBlendOp

pattern VK_BLEND_OP_DST_EXT = VkBlendOp 1000148002

pattern VK_BLEND_OP_SRC_OVER_EXT :: VkBlendOp

pattern VK_BLEND_OP_SRC_OVER_EXT = VkBlendOp 1000148003

pattern VK_BLEND_OP_DST_OVER_EXT :: VkBlendOp

pattern VK_BLEND_OP_DST_OVER_EXT = VkBlendOp 1000148004

pattern VK_BLEND_OP_SRC_IN_EXT :: VkBlendOp

pattern VK_BLEND_OP_SRC_IN_EXT = VkBlendOp 1000148005

pattern VK_BLEND_OP_DST_IN_EXT :: VkBlendOp

pattern VK_BLEND_OP_DST_IN_EXT = VkBlendOp 1000148006

pattern VK_BLEND_OP_SRC_OUT_EXT :: VkBlendOp

pattern VK_BLEND_OP_SRC_OUT_EXT = VkBlendOp 1000148007

pattern VK_BLEND_OP_DST_OUT_EXT :: VkBlendOp

pattern VK_BLEND_OP_DST_OUT_EXT = VkBlendOp 1000148008

pattern VK_BLEND_OP_SRC_ATOP_EXT :: VkBlendOp

pattern VK_BLEND_OP_SRC_ATOP_EXT = VkBlendOp 1000148009

pattern VK_BLEND_OP_DST_ATOP_EXT :: VkBlendOp

pattern VK_BLEND_OP_DST_ATOP_EXT = VkBlendOp 1000148010

pattern VK_BLEND_OP_XOR_EXT :: VkBlendOp

pattern VK_BLEND_OP_XOR_EXT = VkBlendOp 1000148011

pattern VK_BLEND_OP_MULTIPLY_EXT :: VkBlendOp

pattern VK_BLEND_OP_MULTIPLY_EXT = VkBlendOp 1000148012

pattern VK_BLEND_OP_SCREEN_EXT :: VkBlendOp

pattern VK_BLEND_OP_SCREEN_EXT = VkBlendOp 1000148013

pattern VK_BLEND_OP_OVERLAY_EXT :: VkBlendOp

pattern VK_BLEND_OP_OVERLAY_EXT = VkBlendOp 1000148014

pattern VK_BLEND_OP_DARKEN_EXT :: VkBlendOp

pattern VK_BLEND_OP_DARKEN_EXT = VkBlendOp 1000148015

pattern VK_BLEND_OP_LIGHTEN_EXT :: VkBlendOp

pattern VK_BLEND_OP_LIGHTEN_EXT = VkBlendOp 1000148016

pattern VK_BLEND_OP_COLORDODGE_EXT :: VkBlendOp

pattern VK_BLEND_OP_COLORDODGE_EXT = VkBlendOp 1000148017

pattern VK_BLEND_OP_COLORBURN_EXT :: VkBlendOp

pattern VK_BLEND_OP_COLORBURN_EXT = VkBlendOp 1000148018

pattern VK_BLEND_OP_HARDLIGHT_EXT :: VkBlendOp

pattern VK_BLEND_OP_HARDLIGHT_EXT = VkBlendOp 1000148019

pattern VK_BLEND_OP_SOFTLIGHT_EXT :: VkBlendOp

pattern VK_BLEND_OP_SOFTLIGHT_EXT = VkBlendOp 1000148020

pattern VK_BLEND_OP_DIFFERENCE_EXT :: VkBlendOp

pattern VK_BLEND_OP_DIFFERENCE_EXT = VkBlendOp 1000148021

pattern VK_BLEND_OP_EXCLUSION_EXT :: VkBlendOp

pattern VK_BLEND_OP_EXCLUSION_EXT = VkBlendOp 1000148022

pattern VK_BLEND_OP_INVERT_EXT :: VkBlendOp

pattern VK_BLEND_OP_INVERT_EXT = VkBlendOp 1000148023

pattern VK_BLEND_OP_INVERT_RGB_EXT :: VkBlendOp

pattern VK_BLEND_OP_INVERT_RGB_EXT = VkBlendOp 1000148024

pattern VK_BLEND_OP_LINEARDODGE_EXT :: VkBlendOp

pattern VK_BLEND_OP_LINEARDODGE_EXT = VkBlendOp 1000148025

pattern VK_BLEND_OP_LINEARBURN_EXT :: VkBlendOp

pattern VK_BLEND_OP_LINEARBURN_EXT = VkBlendOp 1000148026

pattern VK_BLEND_OP_VIVIDLIGHT_EXT :: VkBlendOp

pattern VK_BLEND_OP_VIVIDLIGHT_EXT = VkBlendOp 1000148027

pattern VK_BLEND_OP_LINEARLIGHT_EXT :: VkBlendOp

pattern VK_BLEND_OP_LINEARLIGHT_EXT = VkBlendOp 1000148028

pattern VK_BLEND_OP_PINLIGHT_EXT :: VkBlendOp

pattern VK_BLEND_OP_PINLIGHT_EXT = VkBlendOp 1000148029

pattern VK_BLEND_OP_HARDMIX_EXT :: VkBlendOp

pattern VK_BLEND_OP_HARDMIX_EXT = VkBlendOp 1000148030

pattern VK_BLEND_OP_HSL_HUE_EXT :: VkBlendOp

pattern VK_BLEND_OP_HSL_HUE_EXT = VkBlendOp 1000148031

pattern VK_BLEND_OP_HSL_SATURATION_EXT :: VkBlendOp

pattern VK_BLEND_OP_HSL_SATURATION_EXT = VkBlendOp 1000148032

pattern VK_BLEND_OP_HSL_COLOR_EXT :: VkBlendOp

pattern VK_BLEND_OP_HSL_COLOR_EXT = VkBlendOp 1000148033

pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT :: VkBlendOp

pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT = VkBlendOp 1000148034

pattern VK_BLEND_OP_PLUS_EXT :: VkBlendOp

pattern VK_BLEND_OP_PLUS_EXT = VkBlendOp 1000148035

pattern VK_BLEND_OP_PLUS_CLAMPED_EXT :: VkBlendOp

pattern VK_BLEND_OP_PLUS_CLAMPED_EXT = VkBlendOp 1000148036

pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT :: VkBlendOp

pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT = VkBlendOp 1000148037

pattern VK_BLEND_OP_PLUS_DARKER_EXT :: VkBlendOp

pattern VK_BLEND_OP_PLUS_DARKER_EXT = VkBlendOp 1000148038

pattern VK_BLEND_OP_MINUS_EXT :: VkBlendOp

pattern VK_BLEND_OP_MINUS_EXT = VkBlendOp 1000148039

pattern VK_BLEND_OP_MINUS_CLAMPED_EXT :: VkBlendOp

pattern VK_BLEND_OP_MINUS_CLAMPED_EXT = VkBlendOp 1000148040

pattern VK_BLEND_OP_CONTRAST_EXT :: VkBlendOp

pattern VK_BLEND_OP_CONTRAST_EXT = VkBlendOp 1000148041

pattern VK_BLEND_OP_INVERT_OVG_EXT :: VkBlendOp

pattern VK_BLEND_OP_INVERT_OVG_EXT = VkBlendOp 1000148042

pattern VK_BLEND_OP_RED_EXT :: VkBlendOp

pattern VK_BLEND_OP_RED_EXT = VkBlendOp 1000148043

pattern VK_BLEND_OP_GREEN_EXT :: VkBlendOp

pattern VK_BLEND_OP_GREEN_EXT = VkBlendOp 1000148044

pattern VK_BLEND_OP_BLUE_EXT :: VkBlendOp

pattern VK_BLEND_OP_BLUE_EXT = VkBlendOp 1000148045

-- | bitpos = @19@
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT ::
        VkAccessFlagBits

pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT =
        VkAccessFlagBits 524288
