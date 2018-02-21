#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkVertexInputBindingDescription
       (VkVertexInputBindingDescription(..)) where
import           Foreign.Storable                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkVertexInputRate (VkVertexInputRate)
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkBinding VkVertexInputBindingDescription where
        type VkBindingMType VkVertexInputBindingDescription = Word32

        {-# NOINLINE vkBinding #-}
        vkBinding x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputBindingDescription, binding})

        {-# INLINE vkBindingByteOffset #-}
        vkBindingByteOffset ~_
          = #{offset VkVertexInputBindingDescription, binding}

        {-# INLINE readVkBinding #-}
        readVkBinding p
          = peekByteOff p #{offset VkVertexInputBindingDescription, binding}

        {-# INLINE writeVkBinding #-}
        writeVkBinding p
          = pokeByteOff p #{offset VkVertexInputBindingDescription, binding}

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

instance CanReadField "binding" VkVertexInputBindingDescription
         where
        {-# INLINE getField #-}
        getField = vkBinding

        {-# INLINE readField #-}
        readField = readVkBinding

instance CanWriteField "binding" VkVertexInputBindingDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkBinding

instance {-# OVERLAPPING #-}
         HasVkStride VkVertexInputBindingDescription where
        type VkStrideMType VkVertexInputBindingDescription = Word32

        {-# NOINLINE vkStride #-}
        vkStride x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputBindingDescription, stride})

        {-# INLINE vkStrideByteOffset #-}
        vkStrideByteOffset ~_
          = #{offset VkVertexInputBindingDescription, stride}

        {-# INLINE readVkStride #-}
        readVkStride p
          = peekByteOff p #{offset VkVertexInputBindingDescription, stride}

        {-# INLINE writeVkStride #-}
        writeVkStride p
          = pokeByteOff p #{offset VkVertexInputBindingDescription, stride}

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

instance CanReadField "stride" VkVertexInputBindingDescription
         where
        {-# INLINE getField #-}
        getField = vkStride

        {-# INLINE readField #-}
        readField = readVkStride

instance CanWriteField "stride" VkVertexInputBindingDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkStride

instance {-# OVERLAPPING #-}
         HasVkInputRate VkVertexInputBindingDescription where
        type VkInputRateMType VkVertexInputBindingDescription =
             VkVertexInputRate

        {-# NOINLINE vkInputRate #-}
        vkInputRate x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputBindingDescription, inputRate})

        {-# INLINE vkInputRateByteOffset #-}
        vkInputRateByteOffset ~_
          = #{offset VkVertexInputBindingDescription, inputRate}

        {-# INLINE readVkInputRate #-}
        readVkInputRate p
          = peekByteOff p #{offset VkVertexInputBindingDescription, inputRate}

        {-# INLINE writeVkInputRate #-}
        writeVkInputRate p
          = pokeByteOff p #{offset VkVertexInputBindingDescription, inputRate}

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

instance CanReadField "inputRate" VkVertexInputBindingDescription
         where
        {-# INLINE getField #-}
        getField = vkInputRate

        {-# INLINE readField #-}
        readField = readVkInputRate

instance CanWriteField "inputRate" VkVertexInputBindingDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkInputRate

instance Show VkVertexInputBindingDescription where
        showsPrec d x
          = showString "VkVertexInputBindingDescription {" .
              showString "vkBinding = " .
                showsPrec d (vkBinding x) .
                  showString ", " .
                    showString "vkStride = " .
                      showsPrec d (vkStride x) .
                        showString ", " .
                          showString "vkInputRate = " .
                            showsPrec d (vkInputRate x) . showChar '}'
