#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkVertexInputBindingDescription
       (VkVertexInputBindingDescription(..)) where
import           Foreign.Storable                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkVertexInputRate (VkVertexInputRate)
import           System.IO.Unsafe                             (unsafeDupablePerformIO)

-- | > typedef struct VkVertexInputBindingDescription {
--   >     uint32_t               binding;
--   >     uint32_t               stride;
--   >     VkVertexInputRate      inputRate;
--   > } VkVertexInputBindingDescription;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkVertexInputBindingDescription.html VkVertexInputBindingDescription registry at www.khronos.org>
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
