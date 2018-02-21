#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateCreateInfoKHR
       (VkDescriptorUpdateTemplateCreateInfoKHR(..)) where
import           Foreign.Storable                                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                                  (VkDescriptorUpdateTemplateCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateTypeKHR    (VkDescriptorUpdateTemplateTypeKHR)
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint                  (VkPipelineBindPoint)
import           Graphics.Vulkan.Types.Enum.VkStructureType                      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                   (VkDescriptorSetLayout,
                                                                                  VkPipelineLayout)
import           Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntryKHR (VkDescriptorUpdateTemplateEntryKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                                (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorUpdateTemplateCreateInfoKHR {
--   >     VkStructureType sType;
--   >     void*                                   pNext;
--   >     VkDescriptorUpdateTemplateCreateFlagsKHR    flags;
--   >     uint32_t                 descriptorUpdateEntryCount;
--   >     const VkDescriptorUpdateTemplateEntryKHR* pDescriptorUpdateEntries;
--   >     VkDescriptorUpdateTemplateTypeKHR templateType;
--   >     VkDescriptorSetLayout descriptorSetLayout;
--   >     VkPipelineBindPoint pipelineBindPoint;
--   >     VkPipelineLayoutpipelineLayout;
--   >     uint32_t set;
--   > } VkDescriptorUpdateTemplateCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorUpdateTemplateCreateInfoKHR.html VkDescriptorUpdateTemplateCreateInfoKHR registry at www.khronos.org>
data VkDescriptorUpdateTemplateCreateInfoKHR = VkDescriptorUpdateTemplateCreateInfoKHR## Addr##
                                                                                        ByteArray##

instance Eq VkDescriptorUpdateTemplateCreateInfoKHR where
        (VkDescriptorUpdateTemplateCreateInfoKHR## a _) ==
          x@(VkDescriptorUpdateTemplateCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorUpdateTemplateCreateInfoKHR where
        (VkDescriptorUpdateTemplateCreateInfoKHR## a _) `compare`
          x@(VkDescriptorUpdateTemplateCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorUpdateTemplateCreateInfoKHR where
        sizeOf ~_
          = #{size VkDescriptorUpdateTemplateCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorUpdateTemplateCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorUpdateTemplateCreateInfoKHR
         where
        unsafeAddr (VkDescriptorUpdateTemplateCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorUpdateTemplateCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorUpdateTemplateCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type StructFields VkDescriptorUpdateTemplateCreateInfoKHR =
             '["sType", "pNext", "flags", "descriptorUpdateEntryCount", -- ' closing tick for hsc2hs
               "pDescriptorUpdateEntries", "templateType", "descriptorSetLayout",
               "pipelineBindPoint", "pipelineLayout", "set"]
        type CUnionType VkDescriptorUpdateTemplateCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorUpdateTemplateCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorUpdateTemplateCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkSTypeMType VkDescriptorUpdateTemplateCreateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "sType" VkDescriptorUpdateTemplateCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}
        type FieldIsArray "sType" VkDescriptorUpdateTemplateCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

instance CanReadField "sType"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkPNextMType VkDescriptorUpdateTemplateCreateInfoKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "pNext" VkDescriptorUpdateTemplateCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkDescriptorUpdateTemplateCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

instance CanReadField "pNext"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkFlagsMType VkDescriptorUpdateTemplateCreateInfoKHR =
             VkDescriptorUpdateTemplateCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "flags" VkDescriptorUpdateTemplateCreateInfoKHR =
             VkDescriptorUpdateTemplateCreateFlagsKHR
        type FieldOptional "flags" VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}
        type FieldIsArray "flags" VkDescriptorUpdateTemplateCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

instance CanReadField "flags"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkDescriptorUpdateEntryCount
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type VkDescriptorUpdateEntryCountMType
               VkDescriptorUpdateTemplateCreateInfoKHR
             = Word32

        {-# NOINLINE vkDescriptorUpdateEntryCount #-}
        vkDescriptorUpdateEntryCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount})

        {-# INLINE vkDescriptorUpdateEntryCountByteOffset #-}
        vkDescriptorUpdateEntryCountByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

        {-# INLINE readVkDescriptorUpdateEntryCount #-}
        readVkDescriptorUpdateEntryCount p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

        {-# INLINE writeVkDescriptorUpdateEntryCount #-}
        writeVkDescriptorUpdateEntryCount p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         HasField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = Word32
        type FieldOptional "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}
        type FieldIsArray "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

instance CanReadField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkDescriptorUpdateEntryCount

        {-# INLINE readField #-}
        readField = readVkDescriptorUpdateEntryCount

instance CanWriteField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorUpdateEntryCount

instance {-# OVERLAPPING #-}
         HasVkPDescriptorUpdateEntries
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type VkPDescriptorUpdateEntriesMType
               VkDescriptorUpdateTemplateCreateInfoKHR
             = Ptr VkDescriptorUpdateTemplateEntryKHR

        {-# NOINLINE vkPDescriptorUpdateEntries #-}
        vkPDescriptorUpdateEntries x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries})

        {-# INLINE vkPDescriptorUpdateEntriesByteOffset #-}
        vkPDescriptorUpdateEntriesByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

        {-# INLINE readVkPDescriptorUpdateEntries #-}
        readVkPDescriptorUpdateEntries p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

        {-# INLINE writeVkPDescriptorUpdateEntries #-}
        writeVkPDescriptorUpdateEntries p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         HasField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = Ptr VkDescriptorUpdateTemplateEntryKHR
        type FieldOptional "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}
        type FieldIsArray "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

instance CanReadField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPDescriptorUpdateEntries

        {-# INLINE readField #-}
        readField = readVkPDescriptorUpdateEntries

instance CanWriteField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDescriptorUpdateEntries

instance {-# OVERLAPPING #-}
         HasVkTemplateType VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkTemplateTypeMType VkDescriptorUpdateTemplateCreateInfoKHR =
             VkDescriptorUpdateTemplateTypeKHR

        {-# NOINLINE vkTemplateType #-}
        vkTemplateType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType})

        {-# INLINE vkTemplateTypeByteOffset #-}
        vkTemplateTypeByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

        {-# INLINE readVkTemplateType #-}
        readVkTemplateType p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

        {-# INLINE writeVkTemplateType #-}
        writeVkTemplateType p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

instance {-# OVERLAPPING #-}
         HasField "templateType" VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkDescriptorUpdateTemplateTypeKHR
        type FieldOptional "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}
        type FieldIsArray "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

instance CanReadField "templateType"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkTemplateType

        {-# INLINE readField #-}
        readField = readVkTemplateType

instance CanWriteField "templateType"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkTemplateType

instance {-# OVERLAPPING #-}
         HasVkDescriptorSetLayout VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type VkDescriptorSetLayoutMType
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkDescriptorSetLayout

        {-# NOINLINE vkDescriptorSetLayout #-}
        vkDescriptorSetLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout})

        {-# INLINE vkDescriptorSetLayoutByteOffset #-}
        vkDescriptorSetLayoutByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

        {-# INLINE readVkDescriptorSetLayout #-}
        readVkDescriptorSetLayout p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

        {-# INLINE writeVkDescriptorSetLayout #-}
        writeVkDescriptorSetLayout p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         HasField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkDescriptorSetLayout
        type FieldOptional "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}
        type FieldIsArray "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

instance CanReadField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkDescriptorSetLayout

        {-# INLINE readField #-}
        readField = readVkDescriptorSetLayout

instance CanWriteField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorSetLayout

instance {-# OVERLAPPING #-}
         HasVkPipelineBindPoint VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type VkPipelineBindPointMType
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkPipelineBindPoint

        {-# NOINLINE vkPipelineBindPoint #-}
        vkPipelineBindPoint x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint})

        {-# INLINE vkPipelineBindPointByteOffset #-}
        vkPipelineBindPointByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

        {-# INLINE readVkPipelineBindPoint #-}
        readVkPipelineBindPoint p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

        {-# INLINE writeVkPipelineBindPoint #-}
        writeVkPipelineBindPoint p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         HasField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkPipelineBindPoint
        type FieldOptional "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}
        type FieldIsArray "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

instance CanReadField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPipelineBindPoint

        {-# INLINE readField #-}
        readField = readVkPipelineBindPoint

instance CanWriteField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPipelineBindPoint

instance {-# OVERLAPPING #-}
         HasVkPipelineLayout VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkPipelineLayoutMType VkDescriptorUpdateTemplateCreateInfoKHR
             = VkPipelineLayout

        {-# NOINLINE vkPipelineLayout #-}
        vkPipelineLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout})

        {-# INLINE vkPipelineLayoutByteOffset #-}
        vkPipelineLayoutByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

        {-# INLINE readVkPipelineLayout #-}
        readVkPipelineLayout p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

        {-# INLINE writeVkPipelineLayout #-}
        writeVkPipelineLayout p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

instance {-# OVERLAPPING #-}
         HasField "pipelineLayout" VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkPipelineLayout
        type FieldOptional "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}
        type FieldIsArray "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

instance CanReadField "pipelineLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPipelineLayout

        {-# INLINE readField #-}
        readField = readVkPipelineLayout

instance CanWriteField "pipelineLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPipelineLayout

instance {-# OVERLAPPING #-}
         HasVkSet VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkSetMType VkDescriptorUpdateTemplateCreateInfoKHR = Word32

        {-# NOINLINE vkSet #-}
        vkSet x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set})

        {-# INLINE vkSetByteOffset #-}
        vkSetByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

        {-# INLINE readVkSet #-}
        readVkSet p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

        {-# INLINE writeVkSet #-}
        writeVkSet p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

instance {-# OVERLAPPING #-}
         HasField "set" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             Word32
        type FieldOptional "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}
        type FieldIsArray "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

instance CanReadField "set" VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSet

        {-# INLINE readField #-}
        readField = readVkSet

instance CanWriteField "set"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSet

instance Show VkDescriptorUpdateTemplateCreateInfoKHR where
        showsPrec d x
          = showString "VkDescriptorUpdateTemplateCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDescriptorUpdateEntryCount = " .
                                  showsPrec d (vkDescriptorUpdateEntryCount x) .
                                    showString ", " .
                                      showString "vkPDescriptorUpdateEntries = " .
                                        showsPrec d (vkPDescriptorUpdateEntries x) .
                                          showString ", " .
                                            showString "vkTemplateType = " .
                                              showsPrec d (vkTemplateType x) .
                                                showString ", " .
                                                  showString "vkDescriptorSetLayout = " .
                                                    showsPrec d (vkDescriptorSetLayout x) .
                                                      showString ", " .
                                                        showString "vkPipelineBindPoint = " .
                                                          showsPrec d (vkPipelineBindPoint x) .
                                                            showString ", " .
                                                              showString "vkPipelineLayout = " .
                                                                showsPrec d (vkPipelineLayout x) .
                                                                  showString ", " .
                                                                    showString "vkSet = " .
                                                                      showsPrec d (vkSet x) .
                                                                        showChar '}'
