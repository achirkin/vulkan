#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR
       (VkImageFormatProperties2KHR(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties (VkImageFormatProperties)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkImageFormatProperties2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkImageFormatProperties          imageFormatProperties;
--   > } VkImageFormatProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageFormatProperties2KHR.html VkImageFormatProperties2KHR registry at www.khronos.org>
data VkImageFormatProperties2KHR = VkImageFormatProperties2KHR## Addr##
                                                                ByteArray##

instance Eq VkImageFormatProperties2KHR where
        (VkImageFormatProperties2KHR## a _) ==
          x@(VkImageFormatProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageFormatProperties2KHR where
        (VkImageFormatProperties2KHR## a _) `compare`
          x@(VkImageFormatProperties2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageFormatProperties2KHR where
        sizeOf ~_ = #{size VkImageFormatProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageFormatProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageFormatProperties2KHR where
        unsafeAddr (VkImageFormatProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageFormatProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageFormatProperties2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageFormatProperties2KHR where
        type StructFields VkImageFormatProperties2KHR =
             '["sType", "pNext", "imageFormatProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkImageFormatProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageFormatProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkImageFormatProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkImageFormatProperties2KHR
         where
        type VkSTypeMType VkImageFormatProperties2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageFormatProperties2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageFormatProperties2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageFormatProperties2KHR where
        type FieldType "sType" VkImageFormatProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkImageFormatProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageFormatProperties2KHR =
             #{offset VkImageFormatProperties2KHR, sType}
        type FieldIsArray "sType" VkImageFormatProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties2KHR, sType}

instance CanReadField "sType" VkImageFormatProperties2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImageFormatProperties2KHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkImageFormatProperties2KHR
         where
        type VkPNextMType VkImageFormatProperties2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageFormatProperties2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageFormatProperties2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageFormatProperties2KHR where
        type FieldType "pNext" VkImageFormatProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkImageFormatProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageFormatProperties2KHR =
             #{offset VkImageFormatProperties2KHR, pNext}
        type FieldIsArray "pNext" VkImageFormatProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties2KHR, pNext}

instance CanReadField "pNext" VkImageFormatProperties2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImageFormatProperties2KHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkImageFormatProperties VkImageFormatProperties2KHR where
        type VkImageFormatPropertiesMType VkImageFormatProperties2KHR =
             VkImageFormatProperties

        {-# NOINLINE vkImageFormatProperties #-}
        vkImageFormatProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2KHR, imageFormatProperties})

        {-# INLINE vkImageFormatPropertiesByteOffset #-}
        vkImageFormatPropertiesByteOffset ~_
          = #{offset VkImageFormatProperties2KHR, imageFormatProperties}

        {-# INLINE readVkImageFormatProperties #-}
        readVkImageFormatProperties p
          = peekByteOff p #{offset VkImageFormatProperties2KHR, imageFormatProperties}

        {-# INLINE writeVkImageFormatProperties #-}
        writeVkImageFormatProperties p
          = pokeByteOff p #{offset VkImageFormatProperties2KHR, imageFormatProperties}

instance {-# OVERLAPPING #-}
         HasField "imageFormatProperties" VkImageFormatProperties2KHR where
        type FieldType "imageFormatProperties" VkImageFormatProperties2KHR
             = VkImageFormatProperties
        type FieldOptional "imageFormatProperties"
               VkImageFormatProperties2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageFormatProperties"
               VkImageFormatProperties2KHR
             =
             #{offset VkImageFormatProperties2KHR, imageFormatProperties}
        type FieldIsArray "imageFormatProperties"
               VkImageFormatProperties2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties2KHR, imageFormatProperties}

instance CanReadField "imageFormatProperties"
           VkImageFormatProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkImageFormatProperties

        {-# INLINE readField #-}
        readField = readVkImageFormatProperties

instance CanWriteField "imageFormatProperties"
           VkImageFormatProperties2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkImageFormatProperties

instance Show VkImageFormatProperties2KHR where
        showsPrec d x
          = showString "VkImageFormatProperties2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkImageFormatProperties = " .
                            showsPrec d (vkImageFormatProperties x) . showChar '}'
