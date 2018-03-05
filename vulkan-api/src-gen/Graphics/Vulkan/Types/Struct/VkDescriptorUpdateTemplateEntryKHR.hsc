#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntryKHR
       (VkDescriptorUpdateTemplateEntryKHR(..)) where
import           Foreign.Storable                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorType (VkDescriptorType)
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

instance {-# OVERLAPPING #-}
         CanReadField "dstBinding" VkDescriptorUpdateTemplateEntryKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

instance {-# OVERLAPPING #-}
         CanWriteField "dstBinding" VkDescriptorUpdateTemplateEntryKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

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

instance {-# OVERLAPPING #-}
         CanReadField "dstArrayElement" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

instance {-# OVERLAPPING #-}
         CanWriteField "dstArrayElement" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

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

instance {-# OVERLAPPING #-}
         CanReadField "descriptorCount" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorCount" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "descriptorType" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorType" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

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

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkDescriptorUpdateTemplateEntryKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkDescriptorUpdateTemplateEntryKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

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

instance {-# OVERLAPPING #-}
         CanReadField "stride" VkDescriptorUpdateTemplateEntryKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, stride})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

instance {-# OVERLAPPING #-}
         CanWriteField "stride" VkDescriptorUpdateTemplateEntryKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

instance Show VkDescriptorUpdateTemplateEntryKHR where
        showsPrec d x
          = showString "VkDescriptorUpdateTemplateEntryKHR {" .
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
