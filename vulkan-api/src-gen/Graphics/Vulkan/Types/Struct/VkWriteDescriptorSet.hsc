#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkWriteDescriptorSet
       (VkWriteDescriptorSet(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorType         (VkDescriptorType)
import           Graphics.Vulkan.Types.Enum.VkStructureType          (VkStructureType)
import           Graphics.Vulkan.Types.Handles                       (VkBufferView,
                                                                      VkDescriptorSet)
import           Graphics.Vulkan.Types.Struct.VkDescriptorBufferInfo (VkDescriptorBufferInfo)
import           Graphics.Vulkan.Types.Struct.VkDescriptorImageInfo  (VkDescriptorImageInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkWriteDescriptorSet {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorSet        dstSet;
--   >     uint32_t               dstBinding;
--   >     uint32_t               dstArrayElement;
--   >     uint32_t               descriptorCount;
--   >     VkDescriptorType       descriptorType;
--   >     const VkDescriptorImageInfo* pImageInfo;
--   >     const VkDescriptorBufferInfo* pBufferInfo;
--   >     const VkBufferView*    pTexelBufferView;
--   > } VkWriteDescriptorSet;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkWriteDescriptorSet.html VkWriteDescriptorSet registry at www.khronos.org>
data VkWriteDescriptorSet = VkWriteDescriptorSet## Addr## ByteArray##

instance Eq VkWriteDescriptorSet where
        (VkWriteDescriptorSet## a _) == x@(VkWriteDescriptorSet## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkWriteDescriptorSet where
        (VkWriteDescriptorSet## a _) `compare` x@(VkWriteDescriptorSet## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkWriteDescriptorSet where
        sizeOf ~_ = #{size VkWriteDescriptorSet}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkWriteDescriptorSet}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkWriteDescriptorSet where
        unsafeAddr (VkWriteDescriptorSet## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkWriteDescriptorSet## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkWriteDescriptorSet## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkWriteDescriptorSet where
        type StructFields VkWriteDescriptorSet =
             '["sType", "pNext", "dstSet", "dstBinding", "dstArrayElement", -- ' closing tick for hsc2hs
               "descriptorCount", "descriptorType", "pImageInfo", "pBufferInfo",
               "pTexelBufferView"]
        type CUnionType VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type StructExtends VkWriteDescriptorSet = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkWriteDescriptorSet where
        type VkSTypeMType VkWriteDescriptorSet = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkWriteDescriptorSet, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkWriteDescriptorSet, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkWriteDescriptorSet, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkWriteDescriptorSet
         where
        type FieldType "sType" VkWriteDescriptorSet = VkStructureType
        type FieldOptional "sType" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWriteDescriptorSet =
             #{offset VkWriteDescriptorSet, sType}
        type FieldIsArray "sType" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkWriteDescriptorSet, sType}

instance CanReadField "sType" VkWriteDescriptorSet where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkWriteDescriptorSet where
        type VkPNextMType VkWriteDescriptorSet = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkWriteDescriptorSet, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkWriteDescriptorSet, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkWriteDescriptorSet, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkWriteDescriptorSet
         where
        type FieldType "pNext" VkWriteDescriptorSet = Ptr Void
        type FieldOptional "pNext" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWriteDescriptorSet =
             #{offset VkWriteDescriptorSet, pNext}
        type FieldIsArray "pNext" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkWriteDescriptorSet, pNext}

instance CanReadField "pNext" VkWriteDescriptorSet where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkDstSet VkWriteDescriptorSet where
        type VkDstSetMType VkWriteDescriptorSet = VkDescriptorSet

        {-# NOINLINE vkDstSet #-}
        vkDstSet x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, dstSet})

        {-# INLINE vkDstSetByteOffset #-}
        vkDstSetByteOffset ~_
          = #{offset VkWriteDescriptorSet, dstSet}

        {-# INLINE readVkDstSet #-}
        readVkDstSet p
          = peekByteOff p #{offset VkWriteDescriptorSet, dstSet}

        {-# INLINE writeVkDstSet #-}
        writeVkDstSet p
          = pokeByteOff p #{offset VkWriteDescriptorSet, dstSet}

instance {-# OVERLAPPING #-} HasField "dstSet" VkWriteDescriptorSet
         where
        type FieldType "dstSet" VkWriteDescriptorSet = VkDescriptorSet
        type FieldOptional "dstSet" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstSet" VkWriteDescriptorSet =
             #{offset VkWriteDescriptorSet, dstSet}
        type FieldIsArray "dstSet" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkWriteDescriptorSet, dstSet}

instance CanReadField "dstSet" VkWriteDescriptorSet where
        {-# INLINE getField #-}
        getField = vkDstSet

        {-# INLINE readField #-}
        readField = readVkDstSet

instance CanWriteField "dstSet" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkDstSet

instance {-# OVERLAPPING #-} HasVkDstBinding VkWriteDescriptorSet
         where
        type VkDstBindingMType VkWriteDescriptorSet = Word32

        {-# NOINLINE vkDstBinding #-}
        vkDstBinding x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, dstBinding})

        {-# INLINE vkDstBindingByteOffset #-}
        vkDstBindingByteOffset ~_
          = #{offset VkWriteDescriptorSet, dstBinding}

        {-# INLINE readVkDstBinding #-}
        readVkDstBinding p
          = peekByteOff p #{offset VkWriteDescriptorSet, dstBinding}

        {-# INLINE writeVkDstBinding #-}
        writeVkDstBinding p
          = pokeByteOff p #{offset VkWriteDescriptorSet, dstBinding}

instance {-# OVERLAPPING #-}
         HasField "dstBinding" VkWriteDescriptorSet where
        type FieldType "dstBinding" VkWriteDescriptorSet = Word32
        type FieldOptional "dstBinding" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstBinding" VkWriteDescriptorSet =
             #{offset VkWriteDescriptorSet, dstBinding}
        type FieldIsArray "dstBinding" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWriteDescriptorSet, dstBinding}

instance CanReadField "dstBinding" VkWriteDescriptorSet where
        {-# INLINE getField #-}
        getField = vkDstBinding

        {-# INLINE readField #-}
        readField = readVkDstBinding

instance CanWriteField "dstBinding" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkDstBinding

instance {-# OVERLAPPING #-}
         HasVkDstArrayElement VkWriteDescriptorSet where
        type VkDstArrayElementMType VkWriteDescriptorSet = Word32

        {-# NOINLINE vkDstArrayElement #-}
        vkDstArrayElement x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, dstArrayElement})

        {-# INLINE vkDstArrayElementByteOffset #-}
        vkDstArrayElementByteOffset ~_
          = #{offset VkWriteDescriptorSet, dstArrayElement}

        {-# INLINE readVkDstArrayElement #-}
        readVkDstArrayElement p
          = peekByteOff p #{offset VkWriteDescriptorSet, dstArrayElement}

        {-# INLINE writeVkDstArrayElement #-}
        writeVkDstArrayElement p
          = pokeByteOff p #{offset VkWriteDescriptorSet, dstArrayElement}

instance {-# OVERLAPPING #-}
         HasField "dstArrayElement" VkWriteDescriptorSet where
        type FieldType "dstArrayElement" VkWriteDescriptorSet = Word32
        type FieldOptional "dstArrayElement" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstArrayElement" VkWriteDescriptorSet =
             #{offset VkWriteDescriptorSet, dstArrayElement}
        type FieldIsArray "dstArrayElement" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWriteDescriptorSet, dstArrayElement}

instance CanReadField "dstArrayElement" VkWriteDescriptorSet where
        {-# INLINE getField #-}
        getField = vkDstArrayElement

        {-# INLINE readField #-}
        readField = readVkDstArrayElement

instance CanWriteField "dstArrayElement" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkDstArrayElement

instance {-# OVERLAPPING #-}
         HasVkDescriptorCount VkWriteDescriptorSet where
        type VkDescriptorCountMType VkWriteDescriptorSet = Word32

        {-# NOINLINE vkDescriptorCount #-}
        vkDescriptorCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, descriptorCount})

        {-# INLINE vkDescriptorCountByteOffset #-}
        vkDescriptorCountByteOffset ~_
          = #{offset VkWriteDescriptorSet, descriptorCount}

        {-# INLINE readVkDescriptorCount #-}
        readVkDescriptorCount p
          = peekByteOff p #{offset VkWriteDescriptorSet, descriptorCount}

        {-# INLINE writeVkDescriptorCount #-}
        writeVkDescriptorCount p
          = pokeByteOff p #{offset VkWriteDescriptorSet, descriptorCount}

instance {-# OVERLAPPING #-}
         HasField "descriptorCount" VkWriteDescriptorSet where
        type FieldType "descriptorCount" VkWriteDescriptorSet = Word32
        type FieldOptional "descriptorCount" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorCount" VkWriteDescriptorSet =
             #{offset VkWriteDescriptorSet, descriptorCount}
        type FieldIsArray "descriptorCount" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWriteDescriptorSet, descriptorCount}

instance CanReadField "descriptorCount" VkWriteDescriptorSet where
        {-# INLINE getField #-}
        getField = vkDescriptorCount

        {-# INLINE readField #-}
        readField = readVkDescriptorCount

instance CanWriteField "descriptorCount" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorCount

instance {-# OVERLAPPING #-}
         HasVkDescriptorType VkWriteDescriptorSet where
        type VkDescriptorTypeMType VkWriteDescriptorSet = VkDescriptorType

        {-# NOINLINE vkDescriptorType #-}
        vkDescriptorType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, descriptorType})

        {-# INLINE vkDescriptorTypeByteOffset #-}
        vkDescriptorTypeByteOffset ~_
          = #{offset VkWriteDescriptorSet, descriptorType}

        {-# INLINE readVkDescriptorType #-}
        readVkDescriptorType p
          = peekByteOff p #{offset VkWriteDescriptorSet, descriptorType}

        {-# INLINE writeVkDescriptorType #-}
        writeVkDescriptorType p
          = pokeByteOff p #{offset VkWriteDescriptorSet, descriptorType}

instance {-# OVERLAPPING #-}
         HasField "descriptorType" VkWriteDescriptorSet where
        type FieldType "descriptorType" VkWriteDescriptorSet =
             VkDescriptorType
        type FieldOptional "descriptorType" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorType" VkWriteDescriptorSet =
             #{offset VkWriteDescriptorSet, descriptorType}
        type FieldIsArray "descriptorType" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWriteDescriptorSet, descriptorType}

instance CanReadField "descriptorType" VkWriteDescriptorSet where
        {-# INLINE getField #-}
        getField = vkDescriptorType

        {-# INLINE readField #-}
        readField = readVkDescriptorType

instance CanWriteField "descriptorType" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorType

instance {-# OVERLAPPING #-} HasVkPImageInfo VkWriteDescriptorSet
         where
        type VkPImageInfoMType VkWriteDescriptorSet =
             Ptr VkDescriptorImageInfo

        {-# NOINLINE vkPImageInfo #-}
        vkPImageInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, pImageInfo})

        {-# INLINE vkPImageInfoByteOffset #-}
        vkPImageInfoByteOffset ~_
          = #{offset VkWriteDescriptorSet, pImageInfo}

        {-# INLINE readVkPImageInfo #-}
        readVkPImageInfo p
          = peekByteOff p #{offset VkWriteDescriptorSet, pImageInfo}

        {-# INLINE writeVkPImageInfo #-}
        writeVkPImageInfo p
          = pokeByteOff p #{offset VkWriteDescriptorSet, pImageInfo}

instance {-# OVERLAPPING #-}
         HasField "pImageInfo" VkWriteDescriptorSet where
        type FieldType "pImageInfo" VkWriteDescriptorSet =
             Ptr VkDescriptorImageInfo
        type FieldOptional "pImageInfo" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pImageInfo" VkWriteDescriptorSet =
             #{offset VkWriteDescriptorSet, pImageInfo}
        type FieldIsArray "pImageInfo" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWriteDescriptorSet, pImageInfo}

instance CanReadField "pImageInfo" VkWriteDescriptorSet where
        {-# INLINE getField #-}
        getField = vkPImageInfo

        {-# INLINE readField #-}
        readField = readVkPImageInfo

instance CanWriteField "pImageInfo" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkPImageInfo

instance {-# OVERLAPPING #-} HasVkPBufferInfo VkWriteDescriptorSet
         where
        type VkPBufferInfoMType VkWriteDescriptorSet =
             Ptr VkDescriptorBufferInfo

        {-# NOINLINE vkPBufferInfo #-}
        vkPBufferInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, pBufferInfo})

        {-# INLINE vkPBufferInfoByteOffset #-}
        vkPBufferInfoByteOffset ~_
          = #{offset VkWriteDescriptorSet, pBufferInfo}

        {-# INLINE readVkPBufferInfo #-}
        readVkPBufferInfo p
          = peekByteOff p #{offset VkWriteDescriptorSet, pBufferInfo}

        {-# INLINE writeVkPBufferInfo #-}
        writeVkPBufferInfo p
          = pokeByteOff p #{offset VkWriteDescriptorSet, pBufferInfo}

instance {-# OVERLAPPING #-}
         HasField "pBufferInfo" VkWriteDescriptorSet where
        type FieldType "pBufferInfo" VkWriteDescriptorSet =
             Ptr VkDescriptorBufferInfo
        type FieldOptional "pBufferInfo" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pBufferInfo" VkWriteDescriptorSet =
             #{offset VkWriteDescriptorSet, pBufferInfo}
        type FieldIsArray "pBufferInfo" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWriteDescriptorSet, pBufferInfo}

instance CanReadField "pBufferInfo" VkWriteDescriptorSet where
        {-# INLINE getField #-}
        getField = vkPBufferInfo

        {-# INLINE readField #-}
        readField = readVkPBufferInfo

instance CanWriteField "pBufferInfo" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkPBufferInfo

instance {-# OVERLAPPING #-}
         HasVkPTexelBufferView VkWriteDescriptorSet where
        type VkPTexelBufferViewMType VkWriteDescriptorSet =
             Ptr VkBufferView

        {-# NOINLINE vkPTexelBufferView #-}
        vkPTexelBufferView x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, pTexelBufferView})

        {-# INLINE vkPTexelBufferViewByteOffset #-}
        vkPTexelBufferViewByteOffset ~_
          = #{offset VkWriteDescriptorSet, pTexelBufferView}

        {-# INLINE readVkPTexelBufferView #-}
        readVkPTexelBufferView p
          = peekByteOff p #{offset VkWriteDescriptorSet, pTexelBufferView}

        {-# INLINE writeVkPTexelBufferView #-}
        writeVkPTexelBufferView p
          = pokeByteOff p #{offset VkWriteDescriptorSet, pTexelBufferView}

instance {-# OVERLAPPING #-}
         HasField "pTexelBufferView" VkWriteDescriptorSet where
        type FieldType "pTexelBufferView" VkWriteDescriptorSet =
             Ptr VkBufferView
        type FieldOptional "pTexelBufferView" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pTexelBufferView" VkWriteDescriptorSet =
             #{offset VkWriteDescriptorSet, pTexelBufferView}
        type FieldIsArray "pTexelBufferView" VkWriteDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWriteDescriptorSet, pTexelBufferView}

instance CanReadField "pTexelBufferView" VkWriteDescriptorSet where
        {-# INLINE getField #-}
        getField = vkPTexelBufferView

        {-# INLINE readField #-}
        readField = readVkPTexelBufferView

instance CanWriteField "pTexelBufferView" VkWriteDescriptorSet
         where
        {-# INLINE writeField #-}
        writeField = writeVkPTexelBufferView

instance Show VkWriteDescriptorSet where
        showsPrec d x
          = showString "VkWriteDescriptorSet {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDstSet = " .
                            showsPrec d (vkDstSet x) .
                              showString ", " .
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
                                                        showString "vkPImageInfo = " .
                                                          showsPrec d (vkPImageInfo x) .
                                                            showString ", " .
                                                              showString "vkPBufferInfo = " .
                                                                showsPrec d (vkPBufferInfo x) .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkPTexelBufferView = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkPTexelBufferView x)
                                                                        . showChar '}'
