#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalImageFormatPropertiesKHR
       (VkExternalImageFormatPropertiesKHR(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryPropertiesKHR (VkExternalMemoryPropertiesKHR)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR   (VkImageFormatProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkExternalImageFormatPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryPropertiesKHR externalMemoryProperties;
--   > } VkExternalImageFormatPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalImageFormatPropertiesKHR.html VkExternalImageFormatPropertiesKHR registry at www.khronos.org>
data VkExternalImageFormatPropertiesKHR = VkExternalImageFormatPropertiesKHR## Addr##
                                                                              ByteArray##

instance Eq VkExternalImageFormatPropertiesKHR where
        (VkExternalImageFormatPropertiesKHR## a _) ==
          x@(VkExternalImageFormatPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalImageFormatPropertiesKHR where
        (VkExternalImageFormatPropertiesKHR## a _) `compare`
          x@(VkExternalImageFormatPropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalImageFormatPropertiesKHR where
        sizeOf ~_ = #{size VkExternalImageFormatPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalImageFormatPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalImageFormatPropertiesKHR where
        unsafeAddr (VkExternalImageFormatPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalImageFormatPropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalImageFormatPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalImageFormatPropertiesKHR where
        type StructFields VkExternalImageFormatPropertiesKHR =
             '["sType", "pNext", "externalMemoryProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalImageFormatPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalImageFormatPropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalImageFormatPropertiesKHR =
             '[VkImageFormatProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalImageFormatPropertiesKHR where
        type VkSTypeMType VkExternalImageFormatPropertiesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalImageFormatPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalImageFormatPropertiesKHR where
        type FieldType "sType" VkExternalImageFormatPropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalImageFormatPropertiesKHR =
             #{offset VkExternalImageFormatPropertiesKHR, sType}
        type FieldIsArray "sType" VkExternalImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatPropertiesKHR, sType}

instance CanReadField "sType" VkExternalImageFormatPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalImageFormatPropertiesKHR where
        type VkPNextMType VkExternalImageFormatPropertiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalImageFormatPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalImageFormatPropertiesKHR where
        type FieldType "pNext" VkExternalImageFormatPropertiesKHR =
             Ptr Void
        type FieldOptional "pNext" VkExternalImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalImageFormatPropertiesKHR =
             #{offset VkExternalImageFormatPropertiesKHR, pNext}
        type FieldIsArray "pNext" VkExternalImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatPropertiesKHR, pNext}

instance CanReadField "pNext" VkExternalImageFormatPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkExternalMemoryProperties VkExternalImageFormatPropertiesKHR
         where
        type VkExternalMemoryPropertiesMType
               VkExternalImageFormatPropertiesKHR
             = VkExternalMemoryPropertiesKHR

        {-# NOINLINE vkExternalMemoryProperties #-}
        vkExternalMemoryProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties})

        {-# INLINE vkExternalMemoryPropertiesByteOffset #-}
        vkExternalMemoryPropertiesByteOffset ~_
          = #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}

        {-# INLINE readVkExternalMemoryProperties #-}
        readVkExternalMemoryProperties p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}

        {-# INLINE writeVkExternalMemoryProperties #-}
        writeVkExternalMemoryProperties p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryProperties"
           VkExternalImageFormatPropertiesKHR
         where
        type FieldType "externalMemoryProperties"
               VkExternalImageFormatPropertiesKHR
             = VkExternalMemoryPropertiesKHR
        type FieldOptional "externalMemoryProperties"
               VkExternalImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "externalMemoryProperties"
               VkExternalImageFormatPropertiesKHR
             =
             #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}
        type FieldIsArray "externalMemoryProperties"
               VkExternalImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}

instance CanReadField "externalMemoryProperties"
           VkExternalImageFormatPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkExternalMemoryProperties

        {-# INLINE readField #-}
        readField = readVkExternalMemoryProperties

instance Show VkExternalImageFormatPropertiesKHR where
        showsPrec d x
          = showString "VkExternalImageFormatPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkExternalMemoryProperties = " .
                            showsPrec d (vkExternalMemoryProperties x) . showChar '}'
