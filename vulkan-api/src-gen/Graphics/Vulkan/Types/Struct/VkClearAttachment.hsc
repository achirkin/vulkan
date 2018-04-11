#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkClearAttachment
       (VkClearAttachment(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags (VkImageAspectFlags)
import           Graphics.Vulkan.Types.Struct.VkClearValue     (VkClearValue)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkClearAttachment {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               colorAttachment;
--   >     VkClearValue           clearValue;
--   > } VkClearAttachment;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearAttachment VkClearAttachment registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkClearAttachment where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearAttachment, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearAttachment, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkClearAttachment where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearAttachment, aspectMask}

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

instance {-# OVERLAPPING #-}
         CanReadField "colorAttachment" VkClearAttachment where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearAttachment, colorAttachment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearAttachment, colorAttachment}

instance {-# OVERLAPPING #-}
         CanWriteField "colorAttachment" VkClearAttachment where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearAttachment, colorAttachment}

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

instance {-# OVERLAPPING #-}
         CanReadField "clearValue" VkClearAttachment where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearAttachment, clearValue})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearAttachment, clearValue}

instance {-# OVERLAPPING #-}
         CanWriteField "clearValue" VkClearAttachment where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearAttachment, clearValue}

instance Show VkClearAttachment where
        showsPrec d x
          = showString "VkClearAttachment {" .
              showString "aspectMask = " .
                showsPrec d (getField @"aspectMask" x) .
                  showString ", " .
                    showString "colorAttachment = " .
                      showsPrec d (getField @"colorAttachment" x) .
                        showString ", " .
                          showString "clearValue = " .
                            showsPrec d (getField @"clearValue" x) . showChar '}'
