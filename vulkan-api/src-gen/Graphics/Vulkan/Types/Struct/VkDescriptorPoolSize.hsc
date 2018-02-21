#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorPoolSize
       (VkDescriptorPoolSize(..)) where
import           Foreign.Storable                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorType (VkDescriptorType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                            (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorPoolSize {
--   >     VkDescriptorType       type;
--   >     uint32_t               descriptorCount;
--   > } VkDescriptorPoolSize;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorPoolSize.html VkDescriptorPoolSize registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkType VkDescriptorPoolSize where
        type VkTypeMType VkDescriptorPoolSize = VkDescriptorType

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolSize, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkDescriptorPoolSize, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkDescriptorPoolSize, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkDescriptorPoolSize, type}

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

instance CanReadField "type" VkDescriptorPoolSize where
        {-# INLINE getField #-}
        getField = vkType

        {-# INLINE readField #-}
        readField = readVkType

instance CanWriteField "type" VkDescriptorPoolSize where
        {-# INLINE writeField #-}
        writeField = writeVkType

instance {-# OVERLAPPING #-}
         HasVkDescriptorCount VkDescriptorPoolSize where
        type VkDescriptorCountMType VkDescriptorPoolSize = Word32

        {-# NOINLINE vkDescriptorCount #-}
        vkDescriptorCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolSize, descriptorCount})

        {-# INLINE vkDescriptorCountByteOffset #-}
        vkDescriptorCountByteOffset ~_
          = #{offset VkDescriptorPoolSize, descriptorCount}

        {-# INLINE readVkDescriptorCount #-}
        readVkDescriptorCount p
          = peekByteOff p #{offset VkDescriptorPoolSize, descriptorCount}

        {-# INLINE writeVkDescriptorCount #-}
        writeVkDescriptorCount p
          = pokeByteOff p #{offset VkDescriptorPoolSize, descriptorCount}

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

instance CanReadField "descriptorCount" VkDescriptorPoolSize where
        {-# INLINE getField #-}
        getField = vkDescriptorCount

        {-# INLINE readField #-}
        readField = readVkDescriptorCount

instance CanWriteField "descriptorCount" VkDescriptorPoolSize where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorCount

instance Show VkDescriptorPoolSize where
        showsPrec d x
          = showString "VkDescriptorPoolSize {" .
              showString "vkType = " .
                showsPrec d (vkType x) .
                  showString ", " .
                    showString "vkDescriptorCount = " .
                      showsPrec d (vkDescriptorCount x) . showChar '}'
