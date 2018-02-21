#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkIndirectCommandsTokenNVX
       (VkIndirectCommandsTokenNVX(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                           (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkIndirectCommandsTokenTypeNVX (VkIndirectCommandsTokenTypeNVX)
import           Graphics.Vulkan.Types.Handles                             (VkBuffer)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkIndirectCommandsTokenNVX {
--   >     VkIndirectCommandsTokenTypeNVX      tokenType;
--   >     VkBuffer                         buffer;
--   >     VkDeviceSize                     offset;
--   > } VkIndirectCommandsTokenNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkIndirectCommandsTokenNVX.html VkIndirectCommandsTokenNVX registry at www.khronos.org>
data VkIndirectCommandsTokenNVX = VkIndirectCommandsTokenNVX## Addr##
                                                              ByteArray##

instance Eq VkIndirectCommandsTokenNVX where
        (VkIndirectCommandsTokenNVX## a _) ==
          x@(VkIndirectCommandsTokenNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkIndirectCommandsTokenNVX where
        (VkIndirectCommandsTokenNVX## a _) `compare`
          x@(VkIndirectCommandsTokenNVX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkIndirectCommandsTokenNVX where
        sizeOf ~_ = #{size VkIndirectCommandsTokenNVX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkIndirectCommandsTokenNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkIndirectCommandsTokenNVX where
        unsafeAddr (VkIndirectCommandsTokenNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkIndirectCommandsTokenNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkIndirectCommandsTokenNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkIndirectCommandsTokenNVX where
        type StructFields VkIndirectCommandsTokenNVX =
             '["tokenType", "buffer", "offset"] -- ' closing tick for hsc2hs
        type CUnionType VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkIndirectCommandsTokenNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkTokenType VkIndirectCommandsTokenNVX where
        type VkTokenTypeMType VkIndirectCommandsTokenNVX =
             VkIndirectCommandsTokenTypeNVX

        {-# NOINLINE vkTokenType #-}
        vkTokenType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, tokenType})

        {-# INLINE vkTokenTypeByteOffset #-}
        vkTokenTypeByteOffset ~_
          = #{offset VkIndirectCommandsTokenNVX, tokenType}

        {-# INLINE readVkTokenType #-}
        readVkTokenType p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, tokenType}

        {-# INLINE writeVkTokenType #-}
        writeVkTokenType p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, tokenType}

instance {-# OVERLAPPING #-}
         HasField "tokenType" VkIndirectCommandsTokenNVX where
        type FieldType "tokenType" VkIndirectCommandsTokenNVX =
             VkIndirectCommandsTokenTypeNVX
        type FieldOptional "tokenType" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tokenType" VkIndirectCommandsTokenNVX =
             #{offset VkIndirectCommandsTokenNVX, tokenType}
        type FieldIsArray "tokenType" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsTokenNVX, tokenType}

instance CanReadField "tokenType" VkIndirectCommandsTokenNVX where
        {-# INLINE getField #-}
        getField = vkTokenType

        {-# INLINE readField #-}
        readField = readVkTokenType

instance CanWriteField "tokenType" VkIndirectCommandsTokenNVX where
        {-# INLINE writeField #-}
        writeField = writeVkTokenType

instance {-# OVERLAPPING #-} HasVkBuffer VkIndirectCommandsTokenNVX
         where
        type VkBufferMType VkIndirectCommandsTokenNVX = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkIndirectCommandsTokenNVX, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, buffer}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkIndirectCommandsTokenNVX where
        type FieldType "buffer" VkIndirectCommandsTokenNVX = VkBuffer
        type FieldOptional "buffer" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkIndirectCommandsTokenNVX =
             #{offset VkIndirectCommandsTokenNVX, buffer}
        type FieldIsArray "buffer" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsTokenNVX, buffer}

instance CanReadField "buffer" VkIndirectCommandsTokenNVX where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer" VkIndirectCommandsTokenNVX where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance {-# OVERLAPPING #-} HasVkOffset VkIndirectCommandsTokenNVX
         where
        type VkOffsetMType VkIndirectCommandsTokenNVX = VkDeviceSize

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkIndirectCommandsTokenNVX, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, offset}

instance {-# OVERLAPPING #-}
         HasField "offset" VkIndirectCommandsTokenNVX where
        type FieldType "offset" VkIndirectCommandsTokenNVX = VkDeviceSize
        type FieldOptional "offset" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkIndirectCommandsTokenNVX =
             #{offset VkIndirectCommandsTokenNVX, offset}
        type FieldIsArray "offset" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsTokenNVX, offset}

instance CanReadField "offset" VkIndirectCommandsTokenNVX where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkIndirectCommandsTokenNVX where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance Show VkIndirectCommandsTokenNVX where
        showsPrec d x
          = showString "VkIndirectCommandsTokenNVX {" .
              showString "vkTokenType = " .
                showsPrec d (vkTokenType x) .
                  showString ", " .
                    showString "vkBuffer = " .
                      showsPrec d (vkBuffer x) .
                        showString ", " .
                          showString "vkOffset = " . showsPrec d (vkOffset x) . showChar '}'
