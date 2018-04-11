#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalImageFormatProperties
       (VkExternalImageFormatProperties(..)) where
import           Foreign.Storable                                        (Storable (..))
import           GHC.Base                                                (Addr##, ByteArray##,
                                                                          byteArrayContents##,
                                                                          plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType              (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryProperties (VkExternalMemoryProperties)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2   (VkImageFormatProperties2)
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

-- | > typedef struct VkExternalImageFormatProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryProperties externalMemoryProperties;
--   > } VkExternalImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalImageFormatProperties VkExternalImageFormatProperties registry at www.khronos.org>
data VkExternalImageFormatProperties = VkExternalImageFormatProperties## Addr##
                                                                        ByteArray##

instance Eq VkExternalImageFormatProperties where
        (VkExternalImageFormatProperties## a _) ==
          x@(VkExternalImageFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalImageFormatProperties where
        (VkExternalImageFormatProperties## a _) `compare`
          x@(VkExternalImageFormatProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalImageFormatProperties where
        sizeOf ~_ = #{size VkExternalImageFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalImageFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalImageFormatProperties where
        unsafeAddr (VkExternalImageFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalImageFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalImageFormatProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalImageFormatProperties where
        type StructFields VkExternalImageFormatProperties =
             '["sType", "pNext", "externalMemoryProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalImageFormatProperties =
             '[VkImageFormatProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalImageFormatProperties where
        type FieldType "sType" VkExternalImageFormatProperties =
             VkStructureType
        type FieldOptional "sType" VkExternalImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalImageFormatProperties =
             #{offset VkExternalImageFormatProperties, sType}
        type FieldIsArray "sType" VkExternalImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalImageFormatProperties where
        type FieldType "pNext" VkExternalImageFormatProperties = Ptr Void
        type FieldOptional "pNext" VkExternalImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalImageFormatProperties =
             #{offset VkExternalImageFormatProperties, pNext}
        type FieldIsArray "pNext" VkExternalImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryProperties" VkExternalImageFormatProperties
         where
        type FieldType "externalMemoryProperties"
               VkExternalImageFormatProperties
             = VkExternalMemoryProperties
        type FieldOptional "externalMemoryProperties"
               VkExternalImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "externalMemoryProperties"
               VkExternalImageFormatProperties
             =
             #{offset VkExternalImageFormatProperties, externalMemoryProperties}
        type FieldIsArray "externalMemoryProperties"
               VkExternalImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatProperties, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanReadField "externalMemoryProperties"
           VkExternalImageFormatProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatProperties, externalMemoryProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatProperties, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "externalMemoryProperties"
           VkExternalImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatProperties, externalMemoryProperties}

instance Show VkExternalImageFormatProperties where
        showsPrec d x
          = showString "VkExternalImageFormatProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "externalMemoryProperties = " .
                            showsPrec d (getField @"externalMemoryProperties" x) . showChar '}'
