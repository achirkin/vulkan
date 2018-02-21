#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorBufferInfo
       (VkDescriptorBufferInfo(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes     (VkDeviceSize)
import           Graphics.Vulkan.Types.Handles       (VkBuffer)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorBufferInfo {
--   >     VkBuffer               buffer;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           range;
--   > } VkDescriptorBufferInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorBufferInfo.html VkDescriptorBufferInfo registry at www.khronos.org>
data VkDescriptorBufferInfo = VkDescriptorBufferInfo## Addr##
                                                      ByteArray##

instance Eq VkDescriptorBufferInfo where
        (VkDescriptorBufferInfo## a _) == x@(VkDescriptorBufferInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorBufferInfo where
        (VkDescriptorBufferInfo## a _) `compare`
          x@(VkDescriptorBufferInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorBufferInfo where
        sizeOf ~_ = #{size VkDescriptorBufferInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDescriptorBufferInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorBufferInfo where
        unsafeAddr (VkDescriptorBufferInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorBufferInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorBufferInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorBufferInfo where
        type StructFields VkDescriptorBufferInfo =
             '["buffer", "offset", "range"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorBufferInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkBuffer VkDescriptorBufferInfo
         where
        type VkBufferMType VkDescriptorBufferInfo = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorBufferInfo, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkDescriptorBufferInfo, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkDescriptorBufferInfo, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkDescriptorBufferInfo, buffer}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkDescriptorBufferInfo where
        type FieldType "buffer" VkDescriptorBufferInfo = VkBuffer
        type FieldOptional "buffer" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkDescriptorBufferInfo =
             #{offset VkDescriptorBufferInfo, buffer}
        type FieldIsArray "buffer" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDescriptorBufferInfo, buffer}

instance CanReadField "buffer" VkDescriptorBufferInfo where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer" VkDescriptorBufferInfo where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance {-# OVERLAPPING #-} HasVkOffset VkDescriptorBufferInfo
         where
        type VkOffsetMType VkDescriptorBufferInfo = VkDeviceSize

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorBufferInfo, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkDescriptorBufferInfo, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkDescriptorBufferInfo, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkDescriptorBufferInfo, offset}

instance {-# OVERLAPPING #-}
         HasField "offset" VkDescriptorBufferInfo where
        type FieldType "offset" VkDescriptorBufferInfo = VkDeviceSize
        type FieldOptional "offset" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkDescriptorBufferInfo =
             #{offset VkDescriptorBufferInfo, offset}
        type FieldIsArray "offset" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDescriptorBufferInfo, offset}

instance CanReadField "offset" VkDescriptorBufferInfo where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkDescriptorBufferInfo where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-} HasVkRange VkDescriptorBufferInfo
         where
        type VkRangeMType VkDescriptorBufferInfo = VkDeviceSize

        {-# NOINLINE vkRange #-}
        vkRange x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorBufferInfo, range})

        {-# INLINE vkRangeByteOffset #-}
        vkRangeByteOffset ~_
          = #{offset VkDescriptorBufferInfo, range}

        {-# INLINE readVkRange #-}
        readVkRange p
          = peekByteOff p #{offset VkDescriptorBufferInfo, range}

        {-# INLINE writeVkRange #-}
        writeVkRange p
          = pokeByteOff p #{offset VkDescriptorBufferInfo, range}

instance {-# OVERLAPPING #-}
         HasField "range" VkDescriptorBufferInfo where
        type FieldType "range" VkDescriptorBufferInfo = VkDeviceSize
        type FieldOptional "range" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "range" VkDescriptorBufferInfo =
             #{offset VkDescriptorBufferInfo, range}
        type FieldIsArray "range" VkDescriptorBufferInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDescriptorBufferInfo, range}

instance CanReadField "range" VkDescriptorBufferInfo where
        {-# INLINE getField #-}
        getField = vkRange

        {-# INLINE readField #-}
        readField = readVkRange

instance CanWriteField "range" VkDescriptorBufferInfo where
        {-# INLINE writeField #-}
        writeField = writeVkRange

instance Show VkDescriptorBufferInfo where
        showsPrec d x
          = showString "VkDescriptorBufferInfo {" .
              showString "vkBuffer = " .
                showsPrec d (vkBuffer x) .
                  showString ", " .
                    showString "vkOffset = " .
                      showsPrec d (vkOffset x) .
                        showString ", " .
                          showString "vkRange = " . showsPrec d (vkRange x) . showChar '}'
