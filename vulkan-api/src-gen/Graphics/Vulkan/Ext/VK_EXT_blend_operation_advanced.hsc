#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
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
import           Foreign.C.String                                           (CString)
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           GHC.Ptr                                                    (Ptr (..))
import           Graphics.Vulkan.Base                                       (VkPipelineColorBlendStateCreateInfo)
import           Graphics.Vulkan.Common                                     (VkAccessFlagBits (..),
                                                                             VkBlendOp (..),
                                                                             VkBlendOverlapEXT,
                                                                             VkBool32,
                                                                             VkStructureType,
                                                                             VkStructureType (..),
                                                                             Word32)
import           Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2 (VkPhysicalDeviceFeatures2KHR,
                                                                             VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         advancedBlendCoherentOperations;
--   > } VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT.html VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT registry at www.khronos.org>
data VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT = VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## Addr##
                                                                                                            ByteArray##

instance Eq VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
        (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## a _) ==
          x@(VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## a _) `compare`
          x@(VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        unsafeAddr (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        type StructFields VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = '["sType", "pNext", "advancedBlendCoherentOperations"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = '[VkPhysicalDeviceFeatures2KHR] -- ' closing tick for hsc2hs

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
         HasField "sType" VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

instance CanReadField "sType"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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

instance {-# OVERLAPPING #-}
         HasField "advancedBlendCoherentOperations"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        type FieldType "advancedBlendCoherentOperations"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = VkBool32
        type FieldOptional "advancedBlendCoherentOperations"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendCoherentOperations"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

instance CanReadField "advancedBlendCoherentOperations"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE getField #-}
        getField = vkAdvancedBlendCoherentOperations

        {-# INLINE readField #-}
        readField = readVkAdvancedBlendCoherentOperations

instance CanWriteField "advancedBlendCoherentOperations"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAdvancedBlendCoherentOperations

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

-- | > typedef struct VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         advancedBlendMaxColorAttachments;
--   >     VkBool32                         advancedBlendIndependentBlend;
--   >     VkBool32                         advancedBlendNonPremultipliedSrcColor;
--   >     VkBool32                         advancedBlendNonPremultipliedDstColor;
--   >     VkBool32                         advancedBlendCorrelatedOverlap;
--   >     VkBool32                         advancedBlendAllOperations;
--   > } VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT.html VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT = VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## Addr##
                                                                                                                ByteArray##

instance Eq VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## a _) ==
          x@(VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## a _)
          `compare`
          x@(VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

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
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        unsafeAddr
          (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type StructFields
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             '["sType", "pNext", "advancedBlendMaxColorAttachments", -- ' closing tick for hsc2hs
               "advancedBlendIndependentBlend",
               "advancedBlendNonPremultipliedSrcColor",
               "advancedBlendNonPremultipliedDstColor",
               "advancedBlendCorrelatedOverlap", "advancedBlendAllOperations"]
        type CUnionType VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'True -- ' closing tick for hsc2hs
        type StructExtends
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

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
         HasField "sType"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType}

instance CanReadField "sType"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

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
         HasField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

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
         HasField "advancedBlendMaxColorAttachments"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendMaxColorAttachments"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = Word32
        type FieldOptional "advancedBlendMaxColorAttachments"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendMaxColorAttachments"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments}

instance CanReadField "advancedBlendMaxColorAttachments"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkAdvancedBlendMaxColorAttachments

        {-# INLINE readField #-}
        readField = readVkAdvancedBlendMaxColorAttachments

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
         HasField "advancedBlendIndependentBlend"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendIndependentBlend"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32
        type FieldOptional "advancedBlendIndependentBlend"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendIndependentBlend"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend}

instance CanReadField "advancedBlendIndependentBlend"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkAdvancedBlendIndependentBlend

        {-# INLINE readField #-}
        readField = readVkAdvancedBlendIndependentBlend

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
         HasField "advancedBlendNonPremultipliedSrcColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendNonPremultipliedSrcColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32
        type FieldOptional "advancedBlendNonPremultipliedSrcColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendNonPremultipliedSrcColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor}

instance CanReadField "advancedBlendNonPremultipliedSrcColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkAdvancedBlendNonPremultipliedSrcColor

        {-# INLINE readField #-}
        readField = readVkAdvancedBlendNonPremultipliedSrcColor

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
         HasField "advancedBlendNonPremultipliedDstColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendNonPremultipliedDstColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32
        type FieldOptional "advancedBlendNonPremultipliedDstColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendNonPremultipliedDstColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor}

instance CanReadField "advancedBlendNonPremultipliedDstColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkAdvancedBlendNonPremultipliedDstColor

        {-# INLINE readField #-}
        readField = readVkAdvancedBlendNonPremultipliedDstColor

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
         HasField "advancedBlendCorrelatedOverlap"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendCorrelatedOverlap"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32
        type FieldOptional "advancedBlendCorrelatedOverlap"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendCorrelatedOverlap"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap}

instance CanReadField "advancedBlendCorrelatedOverlap"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkAdvancedBlendCorrelatedOverlap

        {-# INLINE readField #-}
        readField = readVkAdvancedBlendCorrelatedOverlap

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

instance {-# OVERLAPPING #-}
         HasField "advancedBlendAllOperations"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendAllOperations"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32
        type FieldOptional "advancedBlendAllOperations"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendAllOperations"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations}

instance CanReadField "advancedBlendAllOperations"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkAdvancedBlendAllOperations

        {-# INLINE readField #-}
        readField = readVkAdvancedBlendAllOperations

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

-- | > typedef struct VkPipelineColorBlendAdvancedStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBool32               srcPremultiplied;
--   >     VkBool32               dstPremultiplied;
--   >     VkBlendOverlapEXT      blendOverlap;
--   > } VkPipelineColorBlendAdvancedStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineColorBlendAdvancedStateCreateInfoEXT.html VkPipelineColorBlendAdvancedStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineColorBlendAdvancedStateCreateInfoEXT = VkPipelineColorBlendAdvancedStateCreateInfoEXT## Addr##
                                                                                                      ByteArray##

instance Eq VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a _) ==
          x@(VkPipelineColorBlendAdvancedStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a _) `compare`
          x@(VkPipelineColorBlendAdvancedStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineColorBlendAdvancedStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineColorBlendAdvancedStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        unsafeAddr (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineColorBlendAdvancedStateCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineColorBlendAdvancedStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type StructFields VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             '["sType", "pNext", "srcPremultiplied", "dstPremultiplied", -- ' closing tick for hsc2hs
               "blendOverlap"]
        type CUnionType VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             '[VkPipelineColorBlendStateCreateInfo] -- ' closing tick for hsc2hs

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
         HasField "sType" VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

instance CanReadField "sType"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

instance CanReadField "pNext"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
         HasField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBool32
        type FieldOptional "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

instance CanReadField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSrcPremultiplied

        {-# INLINE readField #-}
        readField = readVkSrcPremultiplied

instance CanWriteField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSrcPremultiplied

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
         HasField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBool32
        type FieldOptional "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

instance CanReadField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkDstPremultiplied

        {-# INLINE readField #-}
        readField = readVkDstPremultiplied

instance CanWriteField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkDstPremultiplied

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

instance {-# OVERLAPPING #-}
         HasField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBlendOverlapEXT
        type FieldOptional "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

instance CanReadField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkBlendOverlap

        {-# INLINE readField #-}
        readField = readVkBlendOverlap

instance CanWriteField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkBlendOverlap

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

{-# INLINE _VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME #-}

_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: CString
_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  = Ptr "VK_EXT_blend_operation_advanced\NUL"##

{-# INLINE is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME #-}

is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  = eqCStrings _VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME

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
