#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCopyDescriptorSet
       (VkCopyDescriptorSet(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkDescriptorSet)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkCopyDescriptorSet {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorSet        srcSet;
--   >     uint32_t               srcBinding;
--   >     uint32_t               srcArrayElement;
--   >     VkDescriptorSet        dstSet;
--   >     uint32_t               dstBinding;
--   >     uint32_t               dstArrayElement;
--   >     uint32_t               descriptorCount;
--   > } VkCopyDescriptorSet;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkCopyDescriptorSet.html VkCopyDescriptorSet registry at www.khronos.org>
data VkCopyDescriptorSet = VkCopyDescriptorSet## Addr## ByteArray##

instance Eq VkCopyDescriptorSet where
        (VkCopyDescriptorSet## a _) == x@(VkCopyDescriptorSet## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCopyDescriptorSet where
        (VkCopyDescriptorSet## a _) `compare` x@(VkCopyDescriptorSet## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCopyDescriptorSet where
        sizeOf ~_ = #{size VkCopyDescriptorSet}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCopyDescriptorSet}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCopyDescriptorSet where
        unsafeAddr (VkCopyDescriptorSet## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCopyDescriptorSet## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCopyDescriptorSet## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCopyDescriptorSet where
        type StructFields VkCopyDescriptorSet =
             '["sType", "pNext", "srcSet", "srcBinding", "srcArrayElement", -- ' closing tick for hsc2hs
               "dstSet", "dstBinding", "dstArrayElement", "descriptorCount"]
        type CUnionType VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCopyDescriptorSet = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkCopyDescriptorSet where
        type VkSTypeMType VkCopyDescriptorSet = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkCopyDescriptorSet, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkCopyDescriptorSet, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkCopyDescriptorSet, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkCopyDescriptorSet
         where
        type FieldType "sType" VkCopyDescriptorSet = VkStructureType
        type FieldOptional "sType" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCopyDescriptorSet =
             #{offset VkCopyDescriptorSet, sType}
        type FieldIsArray "sType" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCopyDescriptorSet, sType}

instance CanReadField "sType" VkCopyDescriptorSet where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkCopyDescriptorSet where
        type VkPNextMType VkCopyDescriptorSet = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkCopyDescriptorSet, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkCopyDescriptorSet, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkCopyDescriptorSet, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkCopyDescriptorSet
         where
        type FieldType "pNext" VkCopyDescriptorSet = Ptr Void
        type FieldOptional "pNext" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCopyDescriptorSet =
             #{offset VkCopyDescriptorSet, pNext}
        type FieldIsArray "pNext" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCopyDescriptorSet, pNext}

instance CanReadField "pNext" VkCopyDescriptorSet where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkSrcSet VkCopyDescriptorSet where
        type VkSrcSetMType VkCopyDescriptorSet = VkDescriptorSet

        {-# NOINLINE vkSrcSet #-}
        vkSrcSet x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, srcSet})

        {-# INLINE vkSrcSetByteOffset #-}
        vkSrcSetByteOffset ~_
          = #{offset VkCopyDescriptorSet, srcSet}

        {-# INLINE readVkSrcSet #-}
        readVkSrcSet p
          = peekByteOff p #{offset VkCopyDescriptorSet, srcSet}

        {-# INLINE writeVkSrcSet #-}
        writeVkSrcSet p
          = pokeByteOff p #{offset VkCopyDescriptorSet, srcSet}

instance {-# OVERLAPPING #-} HasField "srcSet" VkCopyDescriptorSet
         where
        type FieldType "srcSet" VkCopyDescriptorSet = VkDescriptorSet
        type FieldOptional "srcSet" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcSet" VkCopyDescriptorSet =
             #{offset VkCopyDescriptorSet, srcSet}
        type FieldIsArray "srcSet" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCopyDescriptorSet, srcSet}

instance CanReadField "srcSet" VkCopyDescriptorSet where
        {-# INLINE getField #-}
        getField = vkSrcSet

        {-# INLINE readField #-}
        readField = readVkSrcSet

instance CanWriteField "srcSet" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkSrcSet

instance {-# OVERLAPPING #-} HasVkSrcBinding VkCopyDescriptorSet
         where
        type VkSrcBindingMType VkCopyDescriptorSet = Word32

        {-# NOINLINE vkSrcBinding #-}
        vkSrcBinding x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, srcBinding})

        {-# INLINE vkSrcBindingByteOffset #-}
        vkSrcBindingByteOffset ~_
          = #{offset VkCopyDescriptorSet, srcBinding}

        {-# INLINE readVkSrcBinding #-}
        readVkSrcBinding p
          = peekByteOff p #{offset VkCopyDescriptorSet, srcBinding}

        {-# INLINE writeVkSrcBinding #-}
        writeVkSrcBinding p
          = pokeByteOff p #{offset VkCopyDescriptorSet, srcBinding}

instance {-# OVERLAPPING #-}
         HasField "srcBinding" VkCopyDescriptorSet where
        type FieldType "srcBinding" VkCopyDescriptorSet = Word32
        type FieldOptional "srcBinding" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcBinding" VkCopyDescriptorSet =
             #{offset VkCopyDescriptorSet, srcBinding}
        type FieldIsArray "srcBinding" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCopyDescriptorSet, srcBinding}

instance CanReadField "srcBinding" VkCopyDescriptorSet where
        {-# INLINE getField #-}
        getField = vkSrcBinding

        {-# INLINE readField #-}
        readField = readVkSrcBinding

instance CanWriteField "srcBinding" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkSrcBinding

instance {-# OVERLAPPING #-}
         HasVkSrcArrayElement VkCopyDescriptorSet where
        type VkSrcArrayElementMType VkCopyDescriptorSet = Word32

        {-# NOINLINE vkSrcArrayElement #-}
        vkSrcArrayElement x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, srcArrayElement})

        {-# INLINE vkSrcArrayElementByteOffset #-}
        vkSrcArrayElementByteOffset ~_
          = #{offset VkCopyDescriptorSet, srcArrayElement}

        {-# INLINE readVkSrcArrayElement #-}
        readVkSrcArrayElement p
          = peekByteOff p #{offset VkCopyDescriptorSet, srcArrayElement}

        {-# INLINE writeVkSrcArrayElement #-}
        writeVkSrcArrayElement p
          = pokeByteOff p #{offset VkCopyDescriptorSet, srcArrayElement}

instance {-# OVERLAPPING #-}
         HasField "srcArrayElement" VkCopyDescriptorSet where
        type FieldType "srcArrayElement" VkCopyDescriptorSet = Word32
        type FieldOptional "srcArrayElement" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcArrayElement" VkCopyDescriptorSet =
             #{offset VkCopyDescriptorSet, srcArrayElement}
        type FieldIsArray "srcArrayElement" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCopyDescriptorSet, srcArrayElement}

instance CanReadField "srcArrayElement" VkCopyDescriptorSet where
        {-# INLINE getField #-}
        getField = vkSrcArrayElement

        {-# INLINE readField #-}
        readField = readVkSrcArrayElement

instance CanWriteField "srcArrayElement" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkSrcArrayElement

instance {-# OVERLAPPING #-} HasVkDstSet VkCopyDescriptorSet where
        type VkDstSetMType VkCopyDescriptorSet = VkDescriptorSet

        {-# NOINLINE vkDstSet #-}
        vkDstSet x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, dstSet})

        {-# INLINE vkDstSetByteOffset #-}
        vkDstSetByteOffset ~_
          = #{offset VkCopyDescriptorSet, dstSet}

        {-# INLINE readVkDstSet #-}
        readVkDstSet p
          = peekByteOff p #{offset VkCopyDescriptorSet, dstSet}

        {-# INLINE writeVkDstSet #-}
        writeVkDstSet p
          = pokeByteOff p #{offset VkCopyDescriptorSet, dstSet}

instance {-# OVERLAPPING #-} HasField "dstSet" VkCopyDescriptorSet
         where
        type FieldType "dstSet" VkCopyDescriptorSet = VkDescriptorSet
        type FieldOptional "dstSet" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstSet" VkCopyDescriptorSet =
             #{offset VkCopyDescriptorSet, dstSet}
        type FieldIsArray "dstSet" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCopyDescriptorSet, dstSet}

instance CanReadField "dstSet" VkCopyDescriptorSet where
        {-# INLINE getField #-}
        getField = vkDstSet

        {-# INLINE readField #-}
        readField = readVkDstSet

instance CanWriteField "dstSet" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkDstSet

instance {-# OVERLAPPING #-} HasVkDstBinding VkCopyDescriptorSet
         where
        type VkDstBindingMType VkCopyDescriptorSet = Word32

        {-# NOINLINE vkDstBinding #-}
        vkDstBinding x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, dstBinding})

        {-# INLINE vkDstBindingByteOffset #-}
        vkDstBindingByteOffset ~_
          = #{offset VkCopyDescriptorSet, dstBinding}

        {-# INLINE readVkDstBinding #-}
        readVkDstBinding p
          = peekByteOff p #{offset VkCopyDescriptorSet, dstBinding}

        {-# INLINE writeVkDstBinding #-}
        writeVkDstBinding p
          = pokeByteOff p #{offset VkCopyDescriptorSet, dstBinding}

instance {-# OVERLAPPING #-}
         HasField "dstBinding" VkCopyDescriptorSet where
        type FieldType "dstBinding" VkCopyDescriptorSet = Word32
        type FieldOptional "dstBinding" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstBinding" VkCopyDescriptorSet =
             #{offset VkCopyDescriptorSet, dstBinding}
        type FieldIsArray "dstBinding" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCopyDescriptorSet, dstBinding}

instance CanReadField "dstBinding" VkCopyDescriptorSet where
        {-# INLINE getField #-}
        getField = vkDstBinding

        {-# INLINE readField #-}
        readField = readVkDstBinding

instance CanWriteField "dstBinding" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkDstBinding

instance {-# OVERLAPPING #-}
         HasVkDstArrayElement VkCopyDescriptorSet where
        type VkDstArrayElementMType VkCopyDescriptorSet = Word32

        {-# NOINLINE vkDstArrayElement #-}
        vkDstArrayElement x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, dstArrayElement})

        {-# INLINE vkDstArrayElementByteOffset #-}
        vkDstArrayElementByteOffset ~_
          = #{offset VkCopyDescriptorSet, dstArrayElement}

        {-# INLINE readVkDstArrayElement #-}
        readVkDstArrayElement p
          = peekByteOff p #{offset VkCopyDescriptorSet, dstArrayElement}

        {-# INLINE writeVkDstArrayElement #-}
        writeVkDstArrayElement p
          = pokeByteOff p #{offset VkCopyDescriptorSet, dstArrayElement}

instance {-# OVERLAPPING #-}
         HasField "dstArrayElement" VkCopyDescriptorSet where
        type FieldType "dstArrayElement" VkCopyDescriptorSet = Word32
        type FieldOptional "dstArrayElement" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstArrayElement" VkCopyDescriptorSet =
             #{offset VkCopyDescriptorSet, dstArrayElement}
        type FieldIsArray "dstArrayElement" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCopyDescriptorSet, dstArrayElement}

instance CanReadField "dstArrayElement" VkCopyDescriptorSet where
        {-# INLINE getField #-}
        getField = vkDstArrayElement

        {-# INLINE readField #-}
        readField = readVkDstArrayElement

instance CanWriteField "dstArrayElement" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkDstArrayElement

instance {-# OVERLAPPING #-}
         HasVkDescriptorCount VkCopyDescriptorSet where
        type VkDescriptorCountMType VkCopyDescriptorSet = Word32

        {-# NOINLINE vkDescriptorCount #-}
        vkDescriptorCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, descriptorCount})

        {-# INLINE vkDescriptorCountByteOffset #-}
        vkDescriptorCountByteOffset ~_
          = #{offset VkCopyDescriptorSet, descriptorCount}

        {-# INLINE readVkDescriptorCount #-}
        readVkDescriptorCount p
          = peekByteOff p #{offset VkCopyDescriptorSet, descriptorCount}

        {-# INLINE writeVkDescriptorCount #-}
        writeVkDescriptorCount p
          = pokeByteOff p #{offset VkCopyDescriptorSet, descriptorCount}

instance {-# OVERLAPPING #-}
         HasField "descriptorCount" VkCopyDescriptorSet where
        type FieldType "descriptorCount" VkCopyDescriptorSet = Word32
        type FieldOptional "descriptorCount" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorCount" VkCopyDescriptorSet =
             #{offset VkCopyDescriptorSet, descriptorCount}
        type FieldIsArray "descriptorCount" VkCopyDescriptorSet = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCopyDescriptorSet, descriptorCount}

instance CanReadField "descriptorCount" VkCopyDescriptorSet where
        {-# INLINE getField #-}
        getField = vkDescriptorCount

        {-# INLINE readField #-}
        readField = readVkDescriptorCount

instance CanWriteField "descriptorCount" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorCount

instance Show VkCopyDescriptorSet where
        showsPrec d x
          = showString "VkCopyDescriptorSet {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSrcSet = " .
                            showsPrec d (vkSrcSet x) .
                              showString ", " .
                                showString "vkSrcBinding = " .
                                  showsPrec d (vkSrcBinding x) .
                                    showString ", " .
                                      showString "vkSrcArrayElement = " .
                                        showsPrec d (vkSrcArrayElement x) .
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
                                                                  showChar '}'
