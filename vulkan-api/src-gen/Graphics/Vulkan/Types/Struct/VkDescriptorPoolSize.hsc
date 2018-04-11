#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorPoolSize
       (VkDescriptorPoolSize(..)) where
import           Foreign.Storable                            (Storable (..))
import           GHC.Base                                    (Addr##, ByteArray##,
                                                              byteArrayContents##,
                                                              plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorType (VkDescriptorType)
import           System.IO.Unsafe                            (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorPoolSize {
--   >     VkDescriptorType       type;
--   >     uint32_t               descriptorCount;
--   > } VkDescriptorPoolSize;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorPoolSize VkDescriptorPoolSize registry at www.khronos.org>
data VkDescriptorPoolSize = VkDescriptorPoolSize## Addr## ByteArray##

instance Eq VkDescriptorPoolSize where
        (VkDescriptorPoolSize## a _) == x@(VkDescriptorPoolSize## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorPoolSize where
        (VkDescriptorPoolSize## a _) `compare` x@(VkDescriptorPoolSize## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorPoolSize where
        sizeOf ~_ = #{size VkDescriptorPoolSize}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDescriptorPoolSize}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorPoolSize where
        unsafeAddr (VkDescriptorPoolSize## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorPoolSize## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorPoolSize## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorPoolSize where
        type StructFields VkDescriptorPoolSize =
             '["type", "descriptorCount"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorPoolSize = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "type" VkDescriptorPoolSize
         where
        type FieldType "type" VkDescriptorPoolSize = VkDescriptorType
        type FieldOptional "type" VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkDescriptorPoolSize =
             #{offset VkDescriptorPoolSize, type}
        type FieldIsArray "type" VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDescriptorPoolSize, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkDescriptorPoolSize where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolSize, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolSize, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkDescriptorPoolSize where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolSize, type}

instance {-# OVERLAPPING #-}
         HasField "descriptorCount" VkDescriptorPoolSize where
        type FieldType "descriptorCount" VkDescriptorPoolSize = Word32
        type FieldOptional "descriptorCount" VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorCount" VkDescriptorPoolSize =
             #{offset VkDescriptorPoolSize, descriptorCount}
        type FieldIsArray "descriptorCount" VkDescriptorPoolSize = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolSize, descriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorCount" VkDescriptorPoolSize where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolSize, descriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolSize, descriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorCount" VkDescriptorPoolSize where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolSize, descriptorCount}

instance Show VkDescriptorPoolSize where
        showsPrec d x
          = showString "VkDescriptorPoolSize {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "descriptorCount = " .
                      showsPrec d (getField @"descriptorCount" x) . showChar '}'
