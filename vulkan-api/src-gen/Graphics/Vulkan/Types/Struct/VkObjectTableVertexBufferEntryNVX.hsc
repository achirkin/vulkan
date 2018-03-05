#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkObjectTableVertexBufferEntryNVX
       (VkObjectTableVertexBufferEntryNVX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX       (VkObjectEntryTypeNVX)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX (VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.Handles                         (VkBuffer)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkObjectTableVertexBufferEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkBuffer                     buffer;
--   > } VkObjectTableVertexBufferEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkObjectTableVertexBufferEntryNVX.html VkObjectTableVertexBufferEntryNVX registry at www.khronos.org>
data VkObjectTableVertexBufferEntryNVX = VkObjectTableVertexBufferEntryNVX## Addr##
                                                                            ByteArray##

instance Eq VkObjectTableVertexBufferEntryNVX where
        (VkObjectTableVertexBufferEntryNVX## a _) ==
          x@(VkObjectTableVertexBufferEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableVertexBufferEntryNVX where
        (VkObjectTableVertexBufferEntryNVX## a _) `compare`
          x@(VkObjectTableVertexBufferEntryNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTableVertexBufferEntryNVX where
        sizeOf ~_ = #{size VkObjectTableVertexBufferEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTableVertexBufferEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTableVertexBufferEntryNVX where
        unsafeAddr (VkObjectTableVertexBufferEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTableVertexBufferEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTableVertexBufferEntryNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTableVertexBufferEntryNVX where
        type StructFields VkObjectTableVertexBufferEntryNVX =
             '["type", "flags", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTableVertexBufferEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTableVertexBufferEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTableVertexBufferEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "type" VkObjectTableVertexBufferEntryNVX where
        type FieldType "type" VkObjectTableVertexBufferEntryNVX =
             VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTableVertexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTableVertexBufferEntryNVX =
             #{offset VkObjectTableVertexBufferEntryNVX, type}
        type FieldIsArray "type" VkObjectTableVertexBufferEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableVertexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTableVertexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableVertexBufferEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableVertexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTableVertexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableVertexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasField "flags" VkObjectTableVertexBufferEntryNVX where
        type FieldType "flags" VkObjectTableVertexBufferEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTableVertexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTableVertexBufferEntryNVX =
             #{offset VkObjectTableVertexBufferEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTableVertexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableVertexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTableVertexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableVertexBufferEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableVertexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTableVertexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableVertexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkObjectTableVertexBufferEntryNVX where
        type FieldType "buffer" VkObjectTableVertexBufferEntryNVX =
             VkBuffer
        type FieldOptional "buffer" VkObjectTableVertexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkObjectTableVertexBufferEntryNVX =
             #{offset VkObjectTableVertexBufferEntryNVX, buffer}
        type FieldIsArray "buffer" VkObjectTableVertexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableVertexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkObjectTableVertexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableVertexBufferEntryNVX, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableVertexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkObjectTableVertexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableVertexBufferEntryNVX, buffer}

instance Show VkObjectTableVertexBufferEntryNVX where
        showsPrec d x
          = showString "VkObjectTableVertexBufferEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) . showChar '}'
