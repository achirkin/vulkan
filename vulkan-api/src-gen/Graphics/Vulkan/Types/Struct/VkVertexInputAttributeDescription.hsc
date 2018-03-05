#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkVertexInputAttributeDescription
       (VkVertexInputAttributeDescription(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormat (VkFormat)
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

instance {-# OVERLAPPING #-}
         CanReadField "location" VkVertexInputAttributeDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputAttributeDescription, location})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputAttributeDescription, location}

instance {-# OVERLAPPING #-}
         CanWriteField "location" VkVertexInputAttributeDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputAttributeDescription, location}

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

instance {-# OVERLAPPING #-}
         CanReadField "binding" VkVertexInputAttributeDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputAttributeDescription, binding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputAttributeDescription, binding}

instance {-# OVERLAPPING #-}
         CanWriteField "binding" VkVertexInputAttributeDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputAttributeDescription, binding}

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

instance {-# OVERLAPPING #-}
         CanReadField "format" VkVertexInputAttributeDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputAttributeDescription, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputAttributeDescription, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkVertexInputAttributeDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputAttributeDescription, format}

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

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkVertexInputAttributeDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputAttributeDescription, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputAttributeDescription, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkVertexInputAttributeDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputAttributeDescription, offset}

instance Show VkVertexInputAttributeDescription where
        showsPrec d x
          = showString "VkVertexInputAttributeDescription {" .
              showString "location = " .
                showsPrec d (getField @"location" x) .
                  showString ", " .
                    showString "binding = " .
                      showsPrec d (getField @"binding" x) .
                        showString ", " .
                          showString "format = " .
                            showsPrec d (getField @"format" x) .
                              showString ", " .
                                showString "offset = " .
                                  showsPrec d (getField @"offset" x) . showChar '}'
