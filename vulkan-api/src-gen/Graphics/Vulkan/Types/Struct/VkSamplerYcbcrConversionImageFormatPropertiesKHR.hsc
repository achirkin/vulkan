#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionImageFormatPropertiesKHR
       (VkSamplerYcbcrConversionImageFormatPropertiesKHR(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR (VkImageFormatProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkSamplerYcbcrConversionImageFormatPropertiesKHR where
        type VkSTypeMType VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, sType}

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

instance CanReadField "sType"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSamplerYcbcrConversionImageFormatPropertiesKHR where
        type VkPNextMType VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, pNext}

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

instance CanReadField "pNext"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkCombinedImageSamplerDescriptorCount
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        type VkCombinedImageSamplerDescriptorCountMType
               VkSamplerYcbcrConversionImageFormatPropertiesKHR
             = Word32

        {-# NOINLINE vkCombinedImageSamplerDescriptorCount #-}
        vkCombinedImageSamplerDescriptorCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount})

        {-# INLINE vkCombinedImageSamplerDescriptorCountByteOffset #-}
        vkCombinedImageSamplerDescriptorCountByteOffset ~_
          = #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount}

        {-# INLINE readVkCombinedImageSamplerDescriptorCount #-}
        readVkCombinedImageSamplerDescriptorCount p
          = peekByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount}

        {-# INLINE writeVkCombinedImageSamplerDescriptorCount #-}
        writeVkCombinedImageSamplerDescriptorCount p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionImageFormatPropertiesKHR, combinedImageSamplerDescriptorCount}

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

instance CanReadField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCombinedImageSamplerDescriptorCount

        {-# INLINE readField #-}
        readField = readVkCombinedImageSamplerDescriptorCount

instance CanWriteField "combinedImageSamplerDescriptorCount"
           VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkCombinedImageSamplerDescriptorCount

instance Show VkSamplerYcbcrConversionImageFormatPropertiesKHR
         where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionImageFormatPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkCombinedImageSamplerDescriptorCount = " .
                            showsPrec d (vkCombinedImageSamplerDescriptorCount x) .
                              showChar '}'
