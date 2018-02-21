#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntryKHR
       (VkDescriptorUpdateTemplateEntryKHR(..)) where
import           Foreign.Storable                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorType (VkDescriptorType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                            (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorUpdateTemplateEntryKHR {
--   >     uint32_t                         dstBinding;
--   >     uint32_t                         dstArrayElement;
--   >     uint32_t                         descriptorCount;
--   >     VkDescriptorType                 descriptorType;
--   >     size_t                           offset;
--   >     size_t                           stride;
--   > } VkDescriptorUpdateTemplateEntryKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorUpdateTemplateEntryKHR.html VkDescriptorUpdateTemplateEntryKHR registry at www.khronos.org>
data VkDescriptorUpdateTemplateEntryKHR = VkDescriptorUpdateTemplateEntryKHR## Addr##
                                                                              ByteArray##

instance Eq VkDescriptorUpdateTemplateEntryKHR where
        (VkDescriptorUpdateTemplateEntryKHR## a _) ==
          x@(VkDescriptorUpdateTemplateEntryKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorUpdateTemplateEntryKHR where
        (VkDescriptorUpdateTemplateEntryKHR## a _) `compare`
          x@(VkDescriptorUpdateTemplateEntryKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorUpdateTemplateEntryKHR where
        sizeOf ~_ = #{size VkDescriptorUpdateTemplateEntryKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorUpdateTemplateEntryKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorUpdateTemplateEntryKHR where
        unsafeAddr (VkDescriptorUpdateTemplateEntryKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorUpdateTemplateEntryKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorUpdateTemplateEntryKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorUpdateTemplateEntryKHR where
        type StructFields VkDescriptorUpdateTemplateEntryKHR =
             '["dstBinding", "dstArrayElement", "descriptorCount", -- ' closing tick for hsc2hs
               "descriptorType", "offset", "stride"]
        type CUnionType VkDescriptorUpdateTemplateEntryKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorUpdateTemplateEntryKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorUpdateTemplateEntryKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkDstBinding VkDescriptorUpdateTemplateEntryKHR where
        type VkDstBindingMType VkDescriptorUpdateTemplateEntryKHR = Word32

        {-# NOINLINE vkDstBinding #-}
        vkDstBinding x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding})

        {-# INLINE vkDstBindingByteOffset #-}
        vkDstBindingByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

        {-# INLINE readVkDstBinding #-}
        readVkDstBinding p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

        {-# INLINE writeVkDstBinding #-}
        writeVkDstBinding p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

instance {-# OVERLAPPING #-}
         HasField "dstBinding" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "dstBinding" VkDescriptorUpdateTemplateEntryKHR =
             Word32
        type FieldOptional "dstBinding" VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstBinding" VkDescriptorUpdateTemplateEntryKHR =
             #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}
        type FieldIsArray "dstBinding" VkDescriptorUpdateTemplateEntryKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

instance CanReadField "dstBinding"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkDstBinding

        {-# INLINE readField #-}
        readField = readVkDstBinding

instance CanWriteField "dstBinding"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDstBinding

instance {-# OVERLAPPING #-}
         HasVkDstArrayElement VkDescriptorUpdateTemplateEntryKHR where
        type VkDstArrayElementMType VkDescriptorUpdateTemplateEntryKHR =
             Word32

        {-# NOINLINE vkDstArrayElement #-}
        vkDstArrayElement x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement})

        {-# INLINE vkDstArrayElementByteOffset #-}
        vkDstArrayElementByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

        {-# INLINE readVkDstArrayElement #-}
        readVkDstArrayElement p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

        {-# INLINE writeVkDstArrayElement #-}
        writeVkDstArrayElement p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

instance {-# OVERLAPPING #-}
         HasField "dstArrayElement" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "dstArrayElement" VkDescriptorUpdateTemplateEntryKHR
             = Word32
        type FieldOptional "dstArrayElement"
               VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstArrayElement"
               VkDescriptorUpdateTemplateEntryKHR
             =
             #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}
        type FieldIsArray "dstArrayElement"
               VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

instance CanReadField "dstArrayElement"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkDstArrayElement

        {-# INLINE readField #-}
        readField = readVkDstArrayElement

instance CanWriteField "dstArrayElement"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDstArrayElement

instance {-# OVERLAPPING #-}
         HasVkDescriptorCount VkDescriptorUpdateTemplateEntryKHR where
        type VkDescriptorCountMType VkDescriptorUpdateTemplateEntryKHR =
             Word32

        {-# NOINLINE vkDescriptorCount #-}
        vkDescriptorCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount})

        {-# INLINE vkDescriptorCountByteOffset #-}
        vkDescriptorCountByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

        {-# INLINE readVkDescriptorCount #-}
        readVkDescriptorCount p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

        {-# INLINE writeVkDescriptorCount #-}
        writeVkDescriptorCount p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

instance {-# OVERLAPPING #-}
         HasField "descriptorCount" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "descriptorCount" VkDescriptorUpdateTemplateEntryKHR
             = Word32
        type FieldOptional "descriptorCount"
               VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorCount"
               VkDescriptorUpdateTemplateEntryKHR
             =
             #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}
        type FieldIsArray "descriptorCount"
               VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

instance CanReadField "descriptorCount"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkDescriptorCount

        {-# INLINE readField #-}
        readField = readVkDescriptorCount

instance CanWriteField "descriptorCount"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorCount

instance {-# OVERLAPPING #-}
         HasVkDescriptorType VkDescriptorUpdateTemplateEntryKHR where
        type VkDescriptorTypeMType VkDescriptorUpdateTemplateEntryKHR =
             VkDescriptorType

        {-# NOINLINE vkDescriptorType #-}
        vkDescriptorType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType})

        {-# INLINE vkDescriptorTypeByteOffset #-}
        vkDescriptorTypeByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

        {-# INLINE readVkDescriptorType #-}
        readVkDescriptorType p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

        {-# INLINE writeVkDescriptorType #-}
        writeVkDescriptorType p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

instance {-# OVERLAPPING #-}
         HasField "descriptorType" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "descriptorType" VkDescriptorUpdateTemplateEntryKHR
             = VkDescriptorType
        type FieldOptional "descriptorType"
               VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorType"
               VkDescriptorUpdateTemplateEntryKHR
             =
             #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}
        type FieldIsArray "descriptorType"
               VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

instance CanReadField "descriptorType"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkDescriptorType

        {-# INLINE readField #-}
        readField = readVkDescriptorType

instance CanWriteField "descriptorType"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorType

instance {-# OVERLAPPING #-}
         HasVkOffset VkDescriptorUpdateTemplateEntryKHR where
        type VkOffsetMType VkDescriptorUpdateTemplateEntryKHR = CSize

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

instance {-# OVERLAPPING #-}
         HasField "offset" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "offset" VkDescriptorUpdateTemplateEntryKHR = CSize
        type FieldOptional "offset" VkDescriptorUpdateTemplateEntryKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkDescriptorUpdateTemplateEntryKHR =
             #{offset VkDescriptorUpdateTemplateEntryKHR, offset}
        type FieldIsArray "offset" VkDescriptorUpdateTemplateEntryKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

instance CanReadField "offset" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-}
         HasVkStride VkDescriptorUpdateTemplateEntryKHR where
        type VkStrideMType VkDescriptorUpdateTemplateEntryKHR = CSize

        {-# NOINLINE vkStride #-}
        vkStride x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, stride})

        {-# INLINE vkStrideByteOffset #-}
        vkStrideByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

        {-# INLINE readVkStride #-}
        readVkStride p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

        {-# INLINE writeVkStride #-}
        writeVkStride p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

instance {-# OVERLAPPING #-}
         HasField "stride" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "stride" VkDescriptorUpdateTemplateEntryKHR = CSize
        type FieldOptional "stride" VkDescriptorUpdateTemplateEntryKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stride" VkDescriptorUpdateTemplateEntryKHR =
             #{offset VkDescriptorUpdateTemplateEntryKHR, stride}
        type FieldIsArray "stride" VkDescriptorUpdateTemplateEntryKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

instance CanReadField "stride" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkStride

        {-# INLINE readField #-}
        readField = readVkStride

instance CanWriteField "stride" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkStride

instance Show VkDescriptorUpdateTemplateEntryKHR where
        showsPrec d x
          = showString "VkDescriptorUpdateTemplateEntryKHR {" .
              showString "vkDstBinding = " .
                showsPrec d (vkDstBinding x) .
                  showString ", " .
                    showString "vkDstArrayElement = " .
                      showsPrec d (vkDstArrayElement x) .
                        showString ", " .
                          showString "vkDescriptorCount = " .
                            showsPrec d (vkDescriptorCount x) .
                              showString ", " .
                                showString "vkDescriptorType = " .
                                  showsPrec d (vkDescriptorType x) .
                                    showString ", " .
                                      showString "vkOffset = " .
                                        showsPrec d (vkOffset x) .
                                          showString ", " .
                                            showString "vkStride = " .
                                              showsPrec d (vkStride x) . showChar '}'
