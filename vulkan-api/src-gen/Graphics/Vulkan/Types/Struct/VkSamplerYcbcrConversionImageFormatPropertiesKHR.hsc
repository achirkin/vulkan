#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionImageFormatPropertiesKHR
       (VkSamplerYcbcrConversionImageFormatPropertiesKHR(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR (VkImageFormatProperties2KHR)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkSamplerYcbcrConversionImageFormatPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     uint32_t                         combinedImageSamplerDescriptorCount;
--   > } VkSamplerYcbcrConversionImageFormatPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSamplerYcbcrConversionImageFormatPropertiesKHR.html VkSamplerYcbcrConversionImageFormatPropertiesKHR registry at www.khronos.org>
data VkSamplerYcbcrConversionImageFormatPropertiesKHR = VkSamplerYcbcrConversionImageFormatPropertiesKHR## Addr##
                                                                                                          ByteArray##

instance Eq VkSamplerYcbcrConversionImageFormatPropertiesKHR where
        (VkSamplerYcbcrConversionImageFormatPropertiesKHR## a _) ==
          x@(VkSamplerYcbcrConversionImageFormatPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionImageFormatPropertiesKHR where
        (VkSamplerYcbcrConversionImageFormatPropertiesKHR## a _) `compare`
          x@(VkSamplerYcbcrConversionImageFormatPropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        sizeOf ~_
          = #{size VkSamplerYcbcrConversionImageFormatPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionImageFormatPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        unsafeAddr (VkSamplerYcbcrConversionImageFormatPropertiesKHR## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkSamplerYcbcrConversionImageFormatPropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerYcbcrConversionImageFormatPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        type StructFields VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = '["sType", "pNext", "combinedImageSamplerDescriptorCount"] -- ' closing tick for hsc2hs
        type CUnionType VkSamplerYcbcrConversionImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = '[VkImageFormatProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        type FieldType "sType"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = VkStructureType
        type FieldOptional "sType"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             =
             #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType}
        type FieldIsArray "sType"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        type FieldType "pNext"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = Ptr Void
        type FieldOptional "pNext"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             =
             #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext}
        type FieldIsArray "pNext"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        type FieldType "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = Word32
        type FieldOptional "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             =
             #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount}
        type FieldIsArray "combinedImageSamplerDescriptorCount"
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount}

instance Show VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionImageFormatPropertiesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "combinedImageSamplerDescriptorCount = " .
                            showsPrec d (getField @"combinedImageSamplerDescriptorCount" x) .
                              showChar '}'
