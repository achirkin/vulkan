#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferCopy (VkBufferCopy(..))
       where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes  (VkDeviceSize)
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkBufferCopy {
--   >     VkDeviceSize           srcOffset;
--   >     VkDeviceSize           dstOffset;
--   >     VkDeviceSize           size;
--   > } VkBufferCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferCopy VkBufferCopy registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} CanReadField "srcOffset" VkBufferCopy
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCopy, srcOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCopy, srcOffset}

instance {-# OVERLAPPING #-} CanWriteField "srcOffset" VkBufferCopy
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCopy, srcOffset}

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

instance {-# OVERLAPPING #-} CanReadField "dstOffset" VkBufferCopy
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCopy, dstOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCopy, dstOffset}

instance {-# OVERLAPPING #-} CanWriteField "dstOffset" VkBufferCopy
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCopy, dstOffset}

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

instance {-# OVERLAPPING #-} CanReadField "size" VkBufferCopy where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCopy, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferCopy, size}

instance {-# OVERLAPPING #-} CanWriteField "size" VkBufferCopy
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferCopy, size}

instance Show VkBufferCopy where
        showsPrec d x
          = showString "VkBufferCopy {" .
              showString "srcOffset = " .
                showsPrec d (getField @"srcOffset" x) .
                  showString ", " .
                    showString "dstOffset = " .
                      showsPrec d (getField @"dstOffset" x) .
                        showString ", " .
                          showString "size = " .
                            showsPrec d (getField @"size" x) . showChar '}'
