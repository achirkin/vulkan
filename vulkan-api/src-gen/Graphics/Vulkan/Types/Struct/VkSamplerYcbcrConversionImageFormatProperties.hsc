#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionImageFormatProperties
       (VkSamplerYcbcrConversionImageFormatProperties(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2 (VkImageFormatProperties2)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkSamplerYcbcrConversionImageFormatProperties {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     uint32_t                         combinedImageSamplerDescriptorCount;
--   > } VkSamplerYcbcrConversionImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkSamplerYcbcrConversionImageFormatProperties VkSamplerYcbcrConversionImageFormatProperties registry at www.khronos.org>
data VkSamplerYcbcrConversionImageFormatProperties = VkSamplerYcbcrConversionImageFormatProperties## Addr##
                                                                                                    ByteArray##

instance Eq VkSamplerYcbcrConversionImageFormatProperties where
        (VkSamplerYcbcrConversionImageFormatProperties## a _) ==
          x@(VkSamplerYcbcrConversionImageFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionImageFormatProperties where
        (VkSamplerYcbcrConversionImageFormatProperties## a _) `compare`
          x@(VkSamplerYcbcrConversionImageFormatProperties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionImageFormatProperties
         where
        sizeOf ~_
          = #{size VkSamplerYcbcrConversionImageFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionImageFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkSamplerYcbcrConversionImageFormatProperties
         where
        unsafeAddr (VkSamplerYcbcrConversionImageFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkSamplerYcbcrConversionImageFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerYcbcrConversionImageFormatProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkSamplerYcbcrConversionImageFormatProperties
         where
        type StructFields VkSamplerYcbcrConversionImageFormatProperties =
             '["sType", "pNext", "combinedImageSamplerDescriptorCount"] -- ' closing tick for hsc2hs
        type CUnionType VkSamplerYcbcrConversionImageFormatProperties =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerYcbcrConversionImageFormatProperties =
             'True -- ' closing tick for hsc2hs
        type StructExtends VkSamplerYcbcrConversionImageFormatProperties =
             '[VkImageFormatProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerYcbcrConversionImageFormatProperties
         where
        type FieldType "sType"
               VkSamplerYcbcrConversionImageFormatProperties
             = VkStructureType
        type FieldOptional "sType"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkSamplerYcbcrConversionImageFormatProperties
             =
             #{offset VkSamplerYcbcrConversionImageFormatProperties, sType}
        type FieldIsArray "sType"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerYcbcrConversionImageFormatProperties
         where
        type FieldType "pNext"
               VkSamplerYcbcrConversionImageFormatProperties
             = Ptr Void
        type FieldOptional "pNext"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkSamplerYcbcrConversionImageFormatProperties
             =
             #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext}
        type FieldIsArray "pNext"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatProperties
         where
        type FieldType "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatProperties
             = Word32
        type FieldOptional "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatProperties
             =
             #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount}
        type FieldIsArray "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatProperties, combinedImageSamplerDescriptorCount}

instance Show VkSamplerYcbcrConversionImageFormatProperties where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionImageFormatProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "combinedImageSamplerDescriptorCount = " .
                            showsPrec d (getField @"combinedImageSamplerDescriptorCount" x) .
                              showChar '}'
