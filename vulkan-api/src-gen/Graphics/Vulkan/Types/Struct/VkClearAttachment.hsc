#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkClearAttachment
       (VkClearAttachment(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags (VkImageAspectFlags)
import           Graphics.Vulkan.Types.Struct.VkClearValue     (VkClearValue)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkClearAttachment {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               colorAttachment;
--   >     VkClearValue           clearValue;
--   > } VkClearAttachment;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkClearAttachment.html VkClearAttachment registry at www.khronos.org>
data VkClearAttachment = VkClearAttachment## Addr## ByteArray##

instance Eq VkClearAttachment where
        (VkClearAttachment## a _) == x@(VkClearAttachment## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkClearAttachment where
        (VkClearAttachment## a _) `compare` x@(VkClearAttachment## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkClearAttachment where
        sizeOf ~_ = #{size VkClearAttachment}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkClearAttachment}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkClearAttachment where
        unsafeAddr (VkClearAttachment## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkClearAttachment## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkClearAttachment## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkClearAttachment where
        type StructFields VkClearAttachment =
             '["aspectMask", "colorAttachment", "clearValue"] -- ' closing tick for hsc2hs
        type CUnionType VkClearAttachment = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkClearAttachment = 'False -- ' closing tick for hsc2hs
        type StructExtends VkClearAttachment = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkAspectMask VkClearAttachment
         where
        type VkAspectMaskMType VkClearAttachment = VkImageAspectFlags

        {-# NOINLINE vkAspectMask #-}
        vkAspectMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearAttachment, aspectMask})

        {-# INLINE vkAspectMaskByteOffset #-}
        vkAspectMaskByteOffset ~_
          = #{offset VkClearAttachment, aspectMask}

        {-# INLINE readVkAspectMask #-}
        readVkAspectMask p
          = peekByteOff p #{offset VkClearAttachment, aspectMask}

        {-# INLINE writeVkAspectMask #-}
        writeVkAspectMask p
          = pokeByteOff p #{offset VkClearAttachment, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkClearAttachment where
        type FieldType "aspectMask" VkClearAttachment = VkImageAspectFlags
        type FieldOptional "aspectMask" VkClearAttachment = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkClearAttachment =
             #{offset VkClearAttachment, aspectMask}
        type FieldIsArray "aspectMask" VkClearAttachment = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearAttachment, aspectMask}

instance CanReadField "aspectMask" VkClearAttachment where
        {-# INLINE getField #-}
        getField = vkAspectMask

        {-# INLINE readField #-}
        readField = readVkAspectMask

instance CanWriteField "aspectMask" VkClearAttachment where
        {-# INLINE writeField #-}
        writeField = writeVkAspectMask

instance {-# OVERLAPPING #-} HasVkColorAttachment VkClearAttachment
         where
        type VkColorAttachmentMType VkClearAttachment = Word32

        {-# NOINLINE vkColorAttachment #-}
        vkColorAttachment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearAttachment, colorAttachment})

        {-# INLINE vkColorAttachmentByteOffset #-}
        vkColorAttachmentByteOffset ~_
          = #{offset VkClearAttachment, colorAttachment}

        {-# INLINE readVkColorAttachment #-}
        readVkColorAttachment p
          = peekByteOff p #{offset VkClearAttachment, colorAttachment}

        {-# INLINE writeVkColorAttachment #-}
        writeVkColorAttachment p
          = pokeByteOff p #{offset VkClearAttachment, colorAttachment}

instance {-# OVERLAPPING #-}
         HasField "colorAttachment" VkClearAttachment where
        type FieldType "colorAttachment" VkClearAttachment = Word32
        type FieldOptional "colorAttachment" VkClearAttachment = 'False -- ' closing tick for hsc2hs
        type FieldOffset "colorAttachment" VkClearAttachment =
             #{offset VkClearAttachment, colorAttachment}
        type FieldIsArray "colorAttachment" VkClearAttachment = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkClearAttachment, colorAttachment}

instance CanReadField "colorAttachment" VkClearAttachment where
        {-# INLINE getField #-}
        getField = vkColorAttachment

        {-# INLINE readField #-}
        readField = readVkColorAttachment

instance CanWriteField "colorAttachment" VkClearAttachment where
        {-# INLINE writeField #-}
        writeField = writeVkColorAttachment

instance {-# OVERLAPPING #-} HasVkClearValue VkClearAttachment
         where
        type VkClearValueMType VkClearAttachment = VkClearValue

        {-# NOINLINE vkClearValue #-}
        vkClearValue x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearAttachment, clearValue})

        {-# INLINE vkClearValueByteOffset #-}
        vkClearValueByteOffset ~_
          = #{offset VkClearAttachment, clearValue}

        {-# INLINE readVkClearValue #-}
        readVkClearValue p
          = peekByteOff p #{offset VkClearAttachment, clearValue}

        {-# INLINE writeVkClearValue #-}
        writeVkClearValue p
          = pokeByteOff p #{offset VkClearAttachment, clearValue}

instance {-# OVERLAPPING #-}
         HasField "clearValue" VkClearAttachment where
        type FieldType "clearValue" VkClearAttachment = VkClearValue
        type FieldOptional "clearValue" VkClearAttachment = 'False -- ' closing tick for hsc2hs
        type FieldOffset "clearValue" VkClearAttachment =
             #{offset VkClearAttachment, clearValue}
        type FieldIsArray "clearValue" VkClearAttachment = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearAttachment, clearValue}

instance CanReadField "clearValue" VkClearAttachment where
        {-# INLINE getField #-}
        getField = vkClearValue

        {-# INLINE readField #-}
        readField = readVkClearValue

instance CanWriteField "clearValue" VkClearAttachment where
        {-# INLINE writeField #-}
        writeField = writeVkClearValue

instance Show VkClearAttachment where
        showsPrec d x
          = showString "VkClearAttachment {" .
              showString "vkAspectMask = " .
                showsPrec d (vkAspectMask x) .
                  showString ", " .
                    showString "vkColorAttachment = " .
                      showsPrec d (vkColorAttachment x) .
                        showString ", " .
                          showString "vkClearValue = " .
                            showsPrec d (vkClearValue x) . showChar '}'
