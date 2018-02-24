#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
       (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                             (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT.html VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT registry at www.khronos.org>
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
        type FieldIsArray "sType"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "sType"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
        type FieldIsArray "pNext"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
        type FieldIsArray "advancedBlendMaxColorAttachments"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "advancedBlendMaxColorAttachments"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAdvancedBlendMaxColorAttachments

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
        type FieldIsArray "advancedBlendIndependentBlend"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "advancedBlendIndependentBlend"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAdvancedBlendIndependentBlend

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
        type FieldIsArray "advancedBlendNonPremultipliedSrcColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "advancedBlendNonPremultipliedSrcColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAdvancedBlendNonPremultipliedSrcColor

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
        type FieldIsArray "advancedBlendNonPremultipliedDstColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "advancedBlendNonPremultipliedDstColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAdvancedBlendNonPremultipliedDstColor

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
        type FieldIsArray "advancedBlendCorrelatedOverlap"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "advancedBlendCorrelatedOverlap"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAdvancedBlendCorrelatedOverlap

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
        type FieldIsArray "advancedBlendAllOperations"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "advancedBlendAllOperations"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAdvancedBlendAllOperations

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
