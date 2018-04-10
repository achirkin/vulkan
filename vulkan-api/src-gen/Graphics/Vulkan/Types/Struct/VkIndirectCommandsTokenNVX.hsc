#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkIndirectCommandsTokenNVX
       (VkIndirectCommandsTokenNVX(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Base                                                  (Addr##,
                                                                            ByteArray##,
                                                                            byteArrayContents##,
                                                                            plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                           (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkIndirectCommandsTokenTypeNVX (VkIndirectCommandsTokenTypeNVX)
import           Graphics.Vulkan.Types.Handles                             (VkBuffer)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkIndirectCommandsTokenNVX {
--   >     VkIndirectCommandsTokenTypeNVX      tokenType;
--   >     VkBuffer                         buffer;
--   >     VkDeviceSize                     offset;
--   > } VkIndirectCommandsTokenNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkIndirectCommandsTokenNVX VkIndirectCommandsTokenNVX registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "tokenType" VkIndirectCommandsTokenNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, tokenType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, tokenType}

instance {-# OVERLAPPING #-}
         CanWriteField "tokenType" VkIndirectCommandsTokenNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, tokenType}

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

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkIndirectCommandsTokenNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkIndirectCommandsTokenNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, buffer}

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

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkIndirectCommandsTokenNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkIndirectCommandsTokenNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, offset}

instance Show VkIndirectCommandsTokenNVX where
        showsPrec d x
          = showString "VkIndirectCommandsTokenNVX {" .
              showString "tokenType = " .
                showsPrec d (getField @"tokenType" x) .
                  showString ", " .
                    showString "buffer = " .
                      showsPrec d (getField @"buffer" x) .
                        showString ", " .
                          showString "offset = " .
                            showsPrec d (getField @"offset" x) . showChar '}'
