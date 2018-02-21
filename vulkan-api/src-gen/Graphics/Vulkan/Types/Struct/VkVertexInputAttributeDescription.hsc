#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkVertexInputAttributeDescription
       (VkVertexInputAttributeDescription(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormat (VkFormat)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkVertexInputAttributeDescription {
--   >     uint32_t               location;
--   >     uint32_t               binding;
--   >     VkFormat               format;
--   >     uint32_t               offset;
--   > } VkVertexInputAttributeDescription;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkVertexInputAttributeDescription.html VkVertexInputAttributeDescription registry at www.khronos.org>
data VkVertexInputAttributeDescription = VkVertexInputAttributeDescription## Addr##
                                                                            ByteArray##

instance Eq VkVertexInputAttributeDescription where
        (VkVertexInputAttributeDescription## a _) ==
          x@(VkVertexInputAttributeDescription## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkVertexInputAttributeDescription where
        (VkVertexInputAttributeDescription## a _) `compare`
          x@(VkVertexInputAttributeDescription## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkVertexInputAttributeDescription where
        sizeOf ~_ = #{size VkVertexInputAttributeDescription}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkVertexInputAttributeDescription}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkVertexInputAttributeDescription where
        unsafeAddr (VkVertexInputAttributeDescription## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkVertexInputAttributeDescription## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkVertexInputAttributeDescription##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkVertexInputAttributeDescription where
        type StructFields VkVertexInputAttributeDescription =
             '["location", "binding", "format", "offset"] -- ' closing tick for hsc2hs
        type CUnionType VkVertexInputAttributeDescription = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkVertexInputAttributeDescription = 'False -- ' closing tick for hsc2hs
        type StructExtends VkVertexInputAttributeDescription = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkLocation VkVertexInputAttributeDescription where
        type VkLocationMType VkVertexInputAttributeDescription = Word32

        {-# NOINLINE vkLocation #-}
        vkLocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputAttributeDescription, location})

        {-# INLINE vkLocationByteOffset #-}
        vkLocationByteOffset ~_
          = #{offset VkVertexInputAttributeDescription, location}

        {-# INLINE readVkLocation #-}
        readVkLocation p
          = peekByteOff p #{offset VkVertexInputAttributeDescription, location}

        {-# INLINE writeVkLocation #-}
        writeVkLocation p
          = pokeByteOff p #{offset VkVertexInputAttributeDescription, location}

instance {-# OVERLAPPING #-}
         HasField "location" VkVertexInputAttributeDescription where
        type FieldType "location" VkVertexInputAttributeDescription =
             Word32
        type FieldOptional "location" VkVertexInputAttributeDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "location" VkVertexInputAttributeDescription =
             #{offset VkVertexInputAttributeDescription, location}
        type FieldIsArray "location" VkVertexInputAttributeDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputAttributeDescription, location}

instance CanReadField "location" VkVertexInputAttributeDescription
         where
        {-# INLINE getField #-}
        getField = vkLocation

        {-# INLINE readField #-}
        readField = readVkLocation

instance CanWriteField "location" VkVertexInputAttributeDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkLocation

instance {-# OVERLAPPING #-}
         HasVkBinding VkVertexInputAttributeDescription where
        type VkBindingMType VkVertexInputAttributeDescription = Word32

        {-# NOINLINE vkBinding #-}
        vkBinding x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputAttributeDescription, binding})

        {-# INLINE vkBindingByteOffset #-}
        vkBindingByteOffset ~_
          = #{offset VkVertexInputAttributeDescription, binding}

        {-# INLINE readVkBinding #-}
        readVkBinding p
          = peekByteOff p #{offset VkVertexInputAttributeDescription, binding}

        {-# INLINE writeVkBinding #-}
        writeVkBinding p
          = pokeByteOff p #{offset VkVertexInputAttributeDescription, binding}

instance {-# OVERLAPPING #-}
         HasField "binding" VkVertexInputAttributeDescription where
        type FieldType "binding" VkVertexInputAttributeDescription = Word32
        type FieldOptional "binding" VkVertexInputAttributeDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "binding" VkVertexInputAttributeDescription =
             #{offset VkVertexInputAttributeDescription, binding}
        type FieldIsArray "binding" VkVertexInputAttributeDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputAttributeDescription, binding}

instance CanReadField "binding" VkVertexInputAttributeDescription
         where
        {-# INLINE getField #-}
        getField = vkBinding

        {-# INLINE readField #-}
        readField = readVkBinding

instance CanWriteField "binding" VkVertexInputAttributeDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkBinding

instance {-# OVERLAPPING #-}
         HasVkFormat VkVertexInputAttributeDescription where
        type VkFormatMType VkVertexInputAttributeDescription = VkFormat

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputAttributeDescription, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkVertexInputAttributeDescription, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkVertexInputAttributeDescription, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkVertexInputAttributeDescription, format}

instance {-# OVERLAPPING #-}
         HasField "format" VkVertexInputAttributeDescription where
        type FieldType "format" VkVertexInputAttributeDescription =
             VkFormat
        type FieldOptional "format" VkVertexInputAttributeDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkVertexInputAttributeDescription =
             #{offset VkVertexInputAttributeDescription, format}
        type FieldIsArray "format" VkVertexInputAttributeDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputAttributeDescription, format}

instance CanReadField "format" VkVertexInputAttributeDescription
         where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format" VkVertexInputAttributeDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-}
         HasVkOffset VkVertexInputAttributeDescription where
        type VkOffsetMType VkVertexInputAttributeDescription = Word32

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputAttributeDescription, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkVertexInputAttributeDescription, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkVertexInputAttributeDescription, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkVertexInputAttributeDescription, offset}

instance {-# OVERLAPPING #-}
         HasField "offset" VkVertexInputAttributeDescription where
        type FieldType "offset" VkVertexInputAttributeDescription = Word32
        type FieldOptional "offset" VkVertexInputAttributeDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkVertexInputAttributeDescription =
             #{offset VkVertexInputAttributeDescription, offset}
        type FieldIsArray "offset" VkVertexInputAttributeDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputAttributeDescription, offset}

instance CanReadField "offset" VkVertexInputAttributeDescription
         where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkVertexInputAttributeDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance Show VkVertexInputAttributeDescription where
        showsPrec d x
          = showString "VkVertexInputAttributeDescription {" .
              showString "vkLocation = " .
                showsPrec d (vkLocation x) .
                  showString ", " .
                    showString "vkBinding = " .
                      showsPrec d (vkBinding x) .
                        showString ", " .
                          showString "vkFormat = " .
                            showsPrec d (vkFormat x) .
                              showString ", " .
                                showString "vkOffset = " . showsPrec d (vkOffset x) . showChar '}'
