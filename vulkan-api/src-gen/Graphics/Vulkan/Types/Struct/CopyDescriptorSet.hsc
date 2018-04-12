#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.CopyDescriptorSet
       (VkCopyDescriptorSet(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkDescriptorSet)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCopyDescriptorSet VkCopyDescriptorSet registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCopyDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCopyDescriptorSet, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCopyDescriptorSet, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCopyDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCopyDescriptorSet, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCopyDescriptorSet, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "srcSet" VkCopyDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, srcSet})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCopyDescriptorSet, srcSet}

instance {-# OVERLAPPING #-}
         CanWriteField "srcSet" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCopyDescriptorSet, srcSet}

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

instance {-# OVERLAPPING #-}
         CanReadField "srcBinding" VkCopyDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, srcBinding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCopyDescriptorSet, srcBinding}

instance {-# OVERLAPPING #-}
         CanWriteField "srcBinding" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCopyDescriptorSet, srcBinding}

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

instance {-# OVERLAPPING #-}
         CanReadField "srcArrayElement" VkCopyDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, srcArrayElement})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCopyDescriptorSet, srcArrayElement}

instance {-# OVERLAPPING #-}
         CanWriteField "srcArrayElement" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCopyDescriptorSet, srcArrayElement}

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

instance {-# OVERLAPPING #-}
         CanReadField "dstSet" VkCopyDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, dstSet})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCopyDescriptorSet, dstSet}

instance {-# OVERLAPPING #-}
         CanWriteField "dstSet" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCopyDescriptorSet, dstSet}

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

instance {-# OVERLAPPING #-}
         CanReadField "dstBinding" VkCopyDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, dstBinding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCopyDescriptorSet, dstBinding}

instance {-# OVERLAPPING #-}
         CanWriteField "dstBinding" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCopyDescriptorSet, dstBinding}

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

instance {-# OVERLAPPING #-}
         CanReadField "dstArrayElement" VkCopyDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, dstArrayElement})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCopyDescriptorSet, dstArrayElement}

instance {-# OVERLAPPING #-}
         CanWriteField "dstArrayElement" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCopyDescriptorSet, dstArrayElement}

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

instance {-# OVERLAPPING #-}
         CanReadField "descriptorCount" VkCopyDescriptorSet where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCopyDescriptorSet, descriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCopyDescriptorSet, descriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorCount" VkCopyDescriptorSet where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCopyDescriptorSet, descriptorCount}

instance Show VkCopyDescriptorSet where
        showsPrec d x
          = showString "VkCopyDescriptorSet {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "srcSet = " .
                            showsPrec d (getField @"srcSet" x) .
                              showString ", " .
                                showString "srcBinding = " .
                                  showsPrec d (getField @"srcBinding" x) .
                                    showString ", " .
                                      showString "srcArrayElement = " .
                                        showsPrec d (getField @"srcArrayElement" x) .
                                          showString ", " .
                                            showString "dstSet = " .
                                              showsPrec d (getField @"dstSet" x) .
                                                showString ", " .
                                                  showString "dstBinding = " .
                                                    showsPrec d (getField @"dstBinding" x) .
                                                      showString ", " .
                                                        showString "dstArrayElement = " .
                                                          showsPrec d
                                                            (getField @"dstArrayElement" x)
                                                            .
                                                            showString ", " .
                                                              showString "descriptorCount = " .
                                                                showsPrec d
                                                                  (getField @"descriptorCount" x)
                                                                  . showChar '}'
