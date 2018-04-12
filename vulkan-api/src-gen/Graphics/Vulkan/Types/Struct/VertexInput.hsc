#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VertexInput
       (VkVertexInputAttributeDescription(..),
        VkVertexInputBindingDescription(..),
        VkVertexInputBindingDivisorDescriptionEXT(..))
       where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Format          (VkFormat)
import           Graphics.Vulkan.Types.Enum.VertexInputRate (VkVertexInputRate)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkVertexInputAttributeDescription {
--   >     uint32_t               location;
--   >     uint32_t               binding;
--   >     VkFormat               format;
--   >     uint32_t               offset;
--   > } VkVertexInputAttributeDescription;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkVertexInputAttributeDescription VkVertexInputAttributeDescription registry at www.khronos.org>
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

-- | > typedef struct VkVertexInputBindingDescription {
--   >     uint32_t               binding;
--   >     uint32_t               stride;
--   >     VkVertexInputRate      inputRate;
--   > } VkVertexInputBindingDescription;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkVertexInputBindingDescription VkVertexInputBindingDescription registry at www.khronos.org>
data VkVertexInputBindingDescription = VkVertexInputBindingDescription## Addr##
                                                                        ByteArray##

instance Eq VkVertexInputBindingDescription where
        (VkVertexInputBindingDescription## a _) ==
          x@(VkVertexInputBindingDescription## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkVertexInputBindingDescription where
        (VkVertexInputBindingDescription## a _) `compare`
          x@(VkVertexInputBindingDescription## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkVertexInputBindingDescription where
        sizeOf ~_ = #{size VkVertexInputBindingDescription}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkVertexInputBindingDescription}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkVertexInputBindingDescription where
        unsafeAddr (VkVertexInputBindingDescription## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkVertexInputBindingDescription## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkVertexInputBindingDescription##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkVertexInputBindingDescription where
        type StructFields VkVertexInputBindingDescription =
             '["binding", "stride", "inputRate"] -- ' closing tick for hsc2hs
        type CUnionType VkVertexInputBindingDescription = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkVertexInputBindingDescription = 'False -- ' closing tick for hsc2hs
        type StructExtends VkVertexInputBindingDescription = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "binding" VkVertexInputBindingDescription where
        type FieldType "binding" VkVertexInputBindingDescription = Word32
        type FieldOptional "binding" VkVertexInputBindingDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "binding" VkVertexInputBindingDescription =
             #{offset VkVertexInputBindingDescription, binding}
        type FieldIsArray "binding" VkVertexInputBindingDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputBindingDescription, binding}

instance {-# OVERLAPPING #-}
         CanReadField "binding" VkVertexInputBindingDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputBindingDescription, binding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputBindingDescription, binding}

instance {-# OVERLAPPING #-}
         CanWriteField "binding" VkVertexInputBindingDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputBindingDescription, binding}

instance {-# OVERLAPPING #-}
         HasField "stride" VkVertexInputBindingDescription where
        type FieldType "stride" VkVertexInputBindingDescription = Word32
        type FieldOptional "stride" VkVertexInputBindingDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stride" VkVertexInputBindingDescription =
             #{offset VkVertexInputBindingDescription, stride}
        type FieldIsArray "stride" VkVertexInputBindingDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputBindingDescription, stride}

instance {-# OVERLAPPING #-}
         CanReadField "stride" VkVertexInputBindingDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputBindingDescription, stride})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputBindingDescription, stride}

instance {-# OVERLAPPING #-}
         CanWriteField "stride" VkVertexInputBindingDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputBindingDescription, stride}

instance {-# OVERLAPPING #-}
         HasField "inputRate" VkVertexInputBindingDescription where
        type FieldType "inputRate" VkVertexInputBindingDescription =
             VkVertexInputRate
        type FieldOptional "inputRate" VkVertexInputBindingDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "inputRate" VkVertexInputBindingDescription =
             #{offset VkVertexInputBindingDescription, inputRate}
        type FieldIsArray "inputRate" VkVertexInputBindingDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputBindingDescription, inputRate}

instance {-# OVERLAPPING #-}
         CanReadField "inputRate" VkVertexInputBindingDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputBindingDescription, inputRate})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputBindingDescription, inputRate}

instance {-# OVERLAPPING #-}
         CanWriteField "inputRate" VkVertexInputBindingDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputBindingDescription, inputRate}

instance Show VkVertexInputBindingDescription where
        showsPrec d x
          = showString "VkVertexInputBindingDescription {" .
              showString "binding = " .
                showsPrec d (getField @"binding" x) .
                  showString ", " .
                    showString "stride = " .
                      showsPrec d (getField @"stride" x) .
                        showString ", " .
                          showString "inputRate = " .
                            showsPrec d (getField @"inputRate" x) . showChar '}'

-- | > typedef struct VkVertexInputBindingDivisorDescriptionEXT {
--   >     uint32_t          binding;
--   >     uint32_t          divisor;
--   > } VkVertexInputBindingDivisorDescriptionEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkVertexInputBindingDivisorDescriptionEXT VkVertexInputBindingDivisorDescriptionEXT registry at www.khronos.org>
data VkVertexInputBindingDivisorDescriptionEXT = VkVertexInputBindingDivisorDescriptionEXT## Addr##
                                                                                            ByteArray##

instance Eq VkVertexInputBindingDivisorDescriptionEXT where
        (VkVertexInputBindingDivisorDescriptionEXT## a _) ==
          x@(VkVertexInputBindingDivisorDescriptionEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkVertexInputBindingDivisorDescriptionEXT where
        (VkVertexInputBindingDivisorDescriptionEXT## a _) `compare`
          x@(VkVertexInputBindingDivisorDescriptionEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkVertexInputBindingDivisorDescriptionEXT where
        sizeOf ~_
          = #{size VkVertexInputBindingDivisorDescriptionEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkVertexInputBindingDivisorDescriptionEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkVertexInputBindingDivisorDescriptionEXT
         where
        unsafeAddr (VkVertexInputBindingDivisorDescriptionEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkVertexInputBindingDivisorDescriptionEXT## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkVertexInputBindingDivisorDescriptionEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkVertexInputBindingDivisorDescriptionEXT
         where
        type StructFields VkVertexInputBindingDivisorDescriptionEXT =
             '["binding", "divisor"] -- ' closing tick for hsc2hs
        type CUnionType VkVertexInputBindingDivisorDescriptionEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkVertexInputBindingDivisorDescriptionEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkVertexInputBindingDivisorDescriptionEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "binding" VkVertexInputBindingDivisorDescriptionEXT where
        type FieldType "binding" VkVertexInputBindingDivisorDescriptionEXT
             = Word32
        type FieldOptional "binding"
               VkVertexInputBindingDivisorDescriptionEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "binding"
               VkVertexInputBindingDivisorDescriptionEXT
             =
             #{offset VkVertexInputBindingDivisorDescriptionEXT, binding}
        type FieldIsArray "binding"
               VkVertexInputBindingDivisorDescriptionEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputBindingDivisorDescriptionEXT, binding}

instance {-# OVERLAPPING #-}
         CanReadField "binding" VkVertexInputBindingDivisorDescriptionEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputBindingDivisorDescriptionEXT, binding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputBindingDivisorDescriptionEXT, binding}

instance {-# OVERLAPPING #-}
         CanWriteField "binding" VkVertexInputBindingDivisorDescriptionEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputBindingDivisorDescriptionEXT, binding}

instance {-# OVERLAPPING #-}
         HasField "divisor" VkVertexInputBindingDivisorDescriptionEXT where
        type FieldType "divisor" VkVertexInputBindingDivisorDescriptionEXT
             = Word32
        type FieldOptional "divisor"
               VkVertexInputBindingDivisorDescriptionEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "divisor"
               VkVertexInputBindingDivisorDescriptionEXT
             =
             #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor}
        type FieldIsArray "divisor"
               VkVertexInputBindingDivisorDescriptionEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor}

instance {-# OVERLAPPING #-}
         CanReadField "divisor" VkVertexInputBindingDivisorDescriptionEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor}

instance {-# OVERLAPPING #-}
         CanWriteField "divisor" VkVertexInputBindingDivisorDescriptionEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor}

instance Show VkVertexInputBindingDivisorDescriptionEXT where
        showsPrec d x
          = showString "VkVertexInputBindingDivisorDescriptionEXT {" .
              showString "binding = " .
                showsPrec d (getField @"binding" x) .
                  showString ", " .
                    showString "divisor = " .
                      showsPrec d (getField @"divisor" x) . showChar '}'
