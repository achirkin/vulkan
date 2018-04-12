#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.WriteDescriptorSet
       (VkWriteDescriptorSet(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Descriptor    (VkDescriptorType)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBufferView,
                                                           VkDescriptorSet)
import           Graphics.Vulkan.Types.Struct.Descriptor  (VkDescriptorBufferInfo,
                                                           VkDescriptorImageInfo)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkWriteDescriptorSet VkWriteDescriptorSet registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkWriteDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWriteDescriptorSet, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWriteDescriptorSet, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkWriteDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWriteDescriptorSet, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWriteDescriptorSet, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "dstSet" VkWriteDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, dstSet})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWriteDescriptorSet, dstSet}

instance {-# OVERLAPPING #-}
         CanWriteField "dstSet" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWriteDescriptorSet, dstSet}

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

instance {-# OVERLAPPING #-}
         CanReadField "dstBinding" VkWriteDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, dstBinding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWriteDescriptorSet, dstBinding}

instance {-# OVERLAPPING #-}
         CanWriteField "dstBinding" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWriteDescriptorSet, dstBinding}

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

instance {-# OVERLAPPING #-}
         CanReadField "dstArrayElement" VkWriteDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, dstArrayElement})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWriteDescriptorSet, dstArrayElement}

instance {-# OVERLAPPING #-}
         CanWriteField "dstArrayElement" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWriteDescriptorSet, dstArrayElement}

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

instance {-# OVERLAPPING #-}
         CanReadField "descriptorCount" VkWriteDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, descriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWriteDescriptorSet, descriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorCount" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWriteDescriptorSet, descriptorCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "descriptorType" VkWriteDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, descriptorType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWriteDescriptorSet, descriptorType}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorType" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWriteDescriptorSet, descriptorType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pImageInfo" VkWriteDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, pImageInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWriteDescriptorSet, pImageInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "pImageInfo" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWriteDescriptorSet, pImageInfo}

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

instance {-# OVERLAPPING #-}
         CanReadField "pBufferInfo" VkWriteDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, pBufferInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWriteDescriptorSet, pBufferInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "pBufferInfo" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWriteDescriptorSet, pBufferInfo}

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

instance {-# OVERLAPPING #-}
         CanReadField "pTexelBufferView" VkWriteDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWriteDescriptorSet, pTexelBufferView})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWriteDescriptorSet, pTexelBufferView}

instance {-# OVERLAPPING #-}
         CanWriteField "pTexelBufferView" VkWriteDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWriteDescriptorSet, pTexelBufferView}

instance Show VkWriteDescriptorSet where
        showsPrec d x
          = showString "VkWriteDescriptorSet {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "dstSet = " .
                            showsPrec d (getField @"dstSet" x) .
                              showString ", " .
                                showString "dstBinding = " .
                                  showsPrec d (getField @"dstBinding" x) .
                                    showString ", " .
                                      showString "dstArrayElement = " .
                                        showsPrec d (getField @"dstArrayElement" x) .
                                          showString ", " .
                                            showString "descriptorCount = " .
                                              showsPrec d (getField @"descriptorCount" x) .
                                                showString ", " .
                                                  showString "descriptorType = " .
                                                    showsPrec d (getField @"descriptorType" x) .
                                                      showString ", " .
                                                        showString "pImageInfo = " .
                                                          showsPrec d (getField @"pImageInfo" x) .
                                                            showString ", " .
                                                              showString "pBufferInfo = " .
                                                                showsPrec d
                                                                  (getField @"pBufferInfo" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "pTexelBufferView = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"pTexelBufferView"
                                                                           x)
                                                                        . showChar '}'
