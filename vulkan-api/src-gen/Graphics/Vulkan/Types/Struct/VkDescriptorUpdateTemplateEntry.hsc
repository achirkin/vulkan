#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntry
       (VkDescriptorUpdateTemplateEntry(..)) where
import           Foreign.Storable                            (Storable (..))
import           GHC.Base                                    (Addr##, ByteArray##,
                                                              byteArrayContents##,
                                                              plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorType (VkDescriptorType)
import           System.IO.Unsafe                            (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorUpdateTemplateEntry {
--   >     uint32_t                         dstBinding;
--   >     uint32_t                         dstArrayElement;
--   >     uint32_t                         descriptorCount;
--   >     VkDescriptorType                 descriptorType;
--   >     size_t                           offset;
--   >     size_t                           stride;
--   > } VkDescriptorUpdateTemplateEntry;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorUpdateTemplateEntry VkDescriptorUpdateTemplateEntry registry at www.khronos.org>
data VkDescriptorUpdateTemplateEntry = VkDescriptorUpdateTemplateEntry## Addr##
                                                                        ByteArray##

instance Eq VkDescriptorUpdateTemplateEntry where
        (VkDescriptorUpdateTemplateEntry## a _) ==
          x@(VkDescriptorUpdateTemplateEntry## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorUpdateTemplateEntry where
        (VkDescriptorUpdateTemplateEntry## a _) `compare`
          x@(VkDescriptorUpdateTemplateEntry## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorUpdateTemplateEntry where
        sizeOf ~_ = #{size VkDescriptorUpdateTemplateEntry}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorUpdateTemplateEntry}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorUpdateTemplateEntry where
        unsafeAddr (VkDescriptorUpdateTemplateEntry## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorUpdateTemplateEntry## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorUpdateTemplateEntry##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorUpdateTemplateEntry where
        type StructFields VkDescriptorUpdateTemplateEntry =
             '["dstBinding", "dstArrayElement", "descriptorCount", -- ' closing tick for hsc2hs
               "descriptorType", "offset", "stride"]
        type CUnionType VkDescriptorUpdateTemplateEntry = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorUpdateTemplateEntry = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorUpdateTemplateEntry = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "dstBinding" VkDescriptorUpdateTemplateEntry where
        type FieldType "dstBinding" VkDescriptorUpdateTemplateEntry =
             Word32
        type FieldOptional "dstBinding" VkDescriptorUpdateTemplateEntry =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dstBinding" VkDescriptorUpdateTemplateEntry =
             #{offset VkDescriptorUpdateTemplateEntry, dstBinding}
        type FieldIsArray "dstBinding" VkDescriptorUpdateTemplateEntry =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, dstBinding}

instance {-# OVERLAPPING #-}
         CanReadField "dstBinding" VkDescriptorUpdateTemplateEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, dstBinding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, dstBinding}

instance {-# OVERLAPPING #-}
         CanWriteField "dstBinding" VkDescriptorUpdateTemplateEntry where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, dstBinding}

instance {-# OVERLAPPING #-}
         HasField "dstArrayElement" VkDescriptorUpdateTemplateEntry where
        type FieldType "dstArrayElement" VkDescriptorUpdateTemplateEntry =
             Word32
        type FieldOptional "dstArrayElement"
               VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstArrayElement" VkDescriptorUpdateTemplateEntry
             =
             #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement}
        type FieldIsArray "dstArrayElement" VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement}

instance {-# OVERLAPPING #-}
         CanReadField "dstArrayElement" VkDescriptorUpdateTemplateEntry
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement}

instance {-# OVERLAPPING #-}
         CanWriteField "dstArrayElement" VkDescriptorUpdateTemplateEntry
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, dstArrayElement}

instance {-# OVERLAPPING #-}
         HasField "descriptorCount" VkDescriptorUpdateTemplateEntry where
        type FieldType "descriptorCount" VkDescriptorUpdateTemplateEntry =
             Word32
        type FieldOptional "descriptorCount"
               VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorCount" VkDescriptorUpdateTemplateEntry
             =
             #{offset VkDescriptorUpdateTemplateEntry, descriptorCount}
        type FieldIsArray "descriptorCount" VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, descriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorCount" VkDescriptorUpdateTemplateEntry
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, descriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, descriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorCount" VkDescriptorUpdateTemplateEntry
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, descriptorCount}

instance {-# OVERLAPPING #-}
         HasField "descriptorType" VkDescriptorUpdateTemplateEntry where
        type FieldType "descriptorType" VkDescriptorUpdateTemplateEntry =
             VkDescriptorType
        type FieldOptional "descriptorType" VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorType" VkDescriptorUpdateTemplateEntry =
             #{offset VkDescriptorUpdateTemplateEntry, descriptorType}
        type FieldIsArray "descriptorType" VkDescriptorUpdateTemplateEntry
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, descriptorType}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorType" VkDescriptorUpdateTemplateEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, descriptorType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, descriptorType}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorType" VkDescriptorUpdateTemplateEntry
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, descriptorType}

instance {-# OVERLAPPING #-}
         HasField "offset" VkDescriptorUpdateTemplateEntry where
        type FieldType "offset" VkDescriptorUpdateTemplateEntry = CSize
        type FieldOptional "offset" VkDescriptorUpdateTemplateEntry =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkDescriptorUpdateTemplateEntry =
             #{offset VkDescriptorUpdateTemplateEntry, offset}
        type FieldIsArray "offset" VkDescriptorUpdateTemplateEntry = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkDescriptorUpdateTemplateEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkDescriptorUpdateTemplateEntry where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, offset}

instance {-# OVERLAPPING #-}
         HasField "stride" VkDescriptorUpdateTemplateEntry where
        type FieldType "stride" VkDescriptorUpdateTemplateEntry = CSize
        type FieldOptional "stride" VkDescriptorUpdateTemplateEntry =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stride" VkDescriptorUpdateTemplateEntry =
             #{offset VkDescriptorUpdateTemplateEntry, stride}
        type FieldIsArray "stride" VkDescriptorUpdateTemplateEntry = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntry, stride}

instance {-# OVERLAPPING #-}
         CanReadField "stride" VkDescriptorUpdateTemplateEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntry, stride})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntry, stride}

instance {-# OVERLAPPING #-}
         CanWriteField "stride" VkDescriptorUpdateTemplateEntry where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntry, stride}

instance Show VkDescriptorUpdateTemplateEntry where
        showsPrec d x
          = showString "VkDescriptorUpdateTemplateEntry {" .
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
                                      showString "offset = " .
                                        showsPrec d (getField @"offset" x) .
                                          showString ", " .
                                            showString "stride = " .
                                              showsPrec d (getField @"stride" x) . showChar '}'
