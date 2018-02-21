#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferCopy (VkBufferCopy(..))
       where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes     (VkDeviceSize)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkBufferCopy {
--   >     VkDeviceSize           srcOffset;
--   >     VkDeviceSize           dstOffset;
--   >     VkDeviceSize           size;
--   > } VkBufferCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBufferCopy.html VkBufferCopy registry at www.khronos.org>
data VkBufferCopy = VkBufferCopy## Addr## ByteArray##

instance Eq VkBufferCopy where
        (VkBufferCopy## a _) == x@(VkBufferCopy## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferCopy where
        (VkBufferCopy## a _) `compare` x@(VkBufferCopy## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferCopy where
        sizeOf ~_ = #{size VkBufferCopy}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBufferCopy}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferCopy where
        unsafeAddr (VkBufferCopy## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferCopy## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferCopy## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferCopy where
        type StructFields VkBufferCopy =
             '["srcOffset", "dstOffset", "size"] -- ' closing tick for hsc2hs
        type CUnionType VkBufferCopy = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferCopy = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferCopy = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSrcOffset VkBufferCopy where
        type VkSrcOffsetMType VkBufferCopy = VkDeviceSize

        {-# NOINLINE vkSrcOffset #-}
        vkSrcOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCopy, srcOffset})

        {-# INLINE vkSrcOffsetByteOffset #-}
        vkSrcOffsetByteOffset ~_
          = #{offset VkBufferCopy, srcOffset}

        {-# INLINE readVkSrcOffset #-}
        readVkSrcOffset p
          = peekByteOff p #{offset VkBufferCopy, srcOffset}

        {-# INLINE writeVkSrcOffset #-}
        writeVkSrcOffset p
          = pokeByteOff p #{offset VkBufferCopy, srcOffset}

instance {-# OVERLAPPING #-} HasField "srcOffset" VkBufferCopy
         where
        type FieldType "srcOffset" VkBufferCopy = VkDeviceSize
        type FieldOptional "srcOffset" VkBufferCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcOffset" VkBufferCopy =
             #{offset VkBufferCopy, srcOffset}
        type FieldIsArray "srcOffset" VkBufferCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCopy, srcOffset}

instance CanReadField "srcOffset" VkBufferCopy where
        {-# INLINE getField #-}
        getField = vkSrcOffset

        {-# INLINE readField #-}
        readField = readVkSrcOffset

instance CanWriteField "srcOffset" VkBufferCopy where
        {-# INLINE writeField #-}
        writeField = writeVkSrcOffset

instance {-# OVERLAPPING #-} HasVkDstOffset VkBufferCopy where
        type VkDstOffsetMType VkBufferCopy = VkDeviceSize

        {-# NOINLINE vkDstOffset #-}
        vkDstOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCopy, dstOffset})

        {-# INLINE vkDstOffsetByteOffset #-}
        vkDstOffsetByteOffset ~_
          = #{offset VkBufferCopy, dstOffset}

        {-# INLINE readVkDstOffset #-}
        readVkDstOffset p
          = peekByteOff p #{offset VkBufferCopy, dstOffset}

        {-# INLINE writeVkDstOffset #-}
        writeVkDstOffset p
          = pokeByteOff p #{offset VkBufferCopy, dstOffset}

instance {-# OVERLAPPING #-} HasField "dstOffset" VkBufferCopy
         where
        type FieldType "dstOffset" VkBufferCopy = VkDeviceSize
        type FieldOptional "dstOffset" VkBufferCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstOffset" VkBufferCopy =
             #{offset VkBufferCopy, dstOffset}
        type FieldIsArray "dstOffset" VkBufferCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCopy, dstOffset}

instance CanReadField "dstOffset" VkBufferCopy where
        {-# INLINE getField #-}
        getField = vkDstOffset

        {-# INLINE readField #-}
        readField = readVkDstOffset

instance CanWriteField "dstOffset" VkBufferCopy where
        {-# INLINE writeField #-}
        writeField = writeVkDstOffset

instance {-# OVERLAPPING #-} HasVkSize VkBufferCopy where
        type VkSizeMType VkBufferCopy = VkDeviceSize

        {-# NOINLINE vkSize #-}
        vkSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCopy, size})

        {-# INLINE vkSizeByteOffset #-}
        vkSizeByteOffset ~_ = #{offset VkBufferCopy, size}

        {-# INLINE readVkSize #-}
        readVkSize p
          = peekByteOff p #{offset VkBufferCopy, size}

        {-# INLINE writeVkSize #-}
        writeVkSize p
          = pokeByteOff p #{offset VkBufferCopy, size}

instance {-# OVERLAPPING #-} HasField "size" VkBufferCopy where
        type FieldType "size" VkBufferCopy = VkDeviceSize
        type FieldOptional "size" VkBufferCopy = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkBufferCopy =
             #{offset VkBufferCopy, size}
        type FieldIsArray "size" VkBufferCopy = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCopy, size}

instance CanReadField "size" VkBufferCopy where
        {-# INLINE getField #-}
        getField = vkSize

        {-# INLINE readField #-}
        readField = readVkSize

instance CanWriteField "size" VkBufferCopy where
        {-# INLINE writeField #-}
        writeField = writeVkSize

instance Show VkBufferCopy where
        showsPrec d x
          = showString "VkBufferCopy {" .
              showString "vkSrcOffset = " .
                showsPrec d (vkSrcOffset x) .
                  showString ", " .
                    showString "vkDstOffset = " .
                      showsPrec d (vkDstOffset x) .
                        showString ", " .
                          showString "vkSize = " . showsPrec d (vkSize x) . showChar '}'
