#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2KHR
       (VkSparseImageFormatProperties2KHR(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties (VkSparseImageFormatProperties)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageFormatProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkSparseImageFormatProperties    properties;
--   > } VkSparseImageFormatProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSparseImageFormatProperties2KHR.html VkSparseImageFormatProperties2KHR registry at www.khronos.org>
data VkSparseImageFormatProperties2KHR = VkSparseImageFormatProperties2KHR## Addr##
                                                                            ByteArray##

instance Eq VkSparseImageFormatProperties2KHR where
        (VkSparseImageFormatProperties2KHR## a _) ==
          x@(VkSparseImageFormatProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageFormatProperties2KHR where
        (VkSparseImageFormatProperties2KHR## a _) `compare`
          x@(VkSparseImageFormatProperties2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageFormatProperties2KHR where
        sizeOf ~_ = #{size VkSparseImageFormatProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageFormatProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageFormatProperties2KHR where
        unsafeAddr (VkSparseImageFormatProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageFormatProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageFormatProperties2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageFormatProperties2KHR where
        type StructFields VkSparseImageFormatProperties2KHR =
             '["sType", "pNext", "properties"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageFormatProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageFormatProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageFormatProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkSparseImageFormatProperties2KHR where
        type VkSTypeMType VkSparseImageFormatProperties2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSparseImageFormatProperties2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSparseImageFormatProperties2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSparseImageFormatProperties2KHR where
        type FieldType "sType" VkSparseImageFormatProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSparseImageFormatProperties2KHR =
             #{offset VkSparseImageFormatProperties2KHR, sType}
        type FieldIsArray "sType" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2KHR, sType}

instance CanReadField "sType" VkSparseImageFormatProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSparseImageFormatProperties2KHR where
        type VkPNextMType VkSparseImageFormatProperties2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSparseImageFormatProperties2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSparseImageFormatProperties2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSparseImageFormatProperties2KHR where
        type FieldType "pNext" VkSparseImageFormatProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSparseImageFormatProperties2KHR =
             #{offset VkSparseImageFormatProperties2KHR, pNext}
        type FieldIsArray "pNext" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2KHR, pNext}

instance CanReadField "pNext" VkSparseImageFormatProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkProperties VkSparseImageFormatProperties2KHR where
        type VkPropertiesMType VkSparseImageFormatProperties2KHR =
             VkSparseImageFormatProperties

        {-# NOINLINE vkProperties #-}
        vkProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2KHR, properties})

        {-# INLINE vkPropertiesByteOffset #-}
        vkPropertiesByteOffset ~_
          = #{offset VkSparseImageFormatProperties2KHR, properties}

        {-# INLINE readVkProperties #-}
        readVkProperties p
          = peekByteOff p #{offset VkSparseImageFormatProperties2KHR, properties}

        {-# INLINE writeVkProperties #-}
        writeVkProperties p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2KHR, properties}

instance {-# OVERLAPPING #-}
         HasField "properties" VkSparseImageFormatProperties2KHR where
        type FieldType "properties" VkSparseImageFormatProperties2KHR =
             VkSparseImageFormatProperties
        type FieldOptional "properties" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "properties" VkSparseImageFormatProperties2KHR =
             #{offset VkSparseImageFormatProperties2KHR, properties}
        type FieldIsArray "properties" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2KHR, properties}

instance CanReadField "properties"
           VkSparseImageFormatProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkProperties

        {-# INLINE readField #-}
        readField = readVkProperties

instance Show VkSparseImageFormatProperties2KHR where
        showsPrec d x
          = showString "VkSparseImageFormatProperties2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkProperties = " .
                            showsPrec d (vkProperties x) . showChar '}'
