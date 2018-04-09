#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkObjectTableIndexBufferEntryNVX
       (VkObjectTableIndexBufferEntryNVX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkIndexType                (VkIndexType)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX       (VkObjectEntryTypeNVX)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX (VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.Handles                         (VkBuffer)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkObjectTableIndexBufferEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkBuffer                     buffer;
--   >     VkIndexType                  indexType;
--   > } VkObjectTableIndexBufferEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkObjectTableIndexBufferEntryNVXVkObjectTableIndexBufferEntryNVX registry at www.khronos.org>
data VkObjectTableIndexBufferEntryNVX = VkObjectTableIndexBufferEntryNVX## Addr##
                                                                          ByteArray##

instance Eq VkObjectTableIndexBufferEntryNVX where
        (VkObjectTableIndexBufferEntryNVX## a _) ==
          x@(VkObjectTableIndexBufferEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableIndexBufferEntryNVX where
        (VkObjectTableIndexBufferEntryNVX## a _) `compare`
          x@(VkObjectTableIndexBufferEntryNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTableIndexBufferEntryNVX where
        sizeOf ~_ = #{size VkObjectTableIndexBufferEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTableIndexBufferEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTableIndexBufferEntryNVX where
        unsafeAddr (VkObjectTableIndexBufferEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTableIndexBufferEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTableIndexBufferEntryNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTableIndexBufferEntryNVX where
        type StructFields VkObjectTableIndexBufferEntryNVX =
             '["type", "flags", "buffer", "indexType"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTableIndexBufferEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTableIndexBufferEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTableIndexBufferEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "type" VkObjectTableIndexBufferEntryNVX where
        type FieldType "type" VkObjectTableIndexBufferEntryNVX =
             VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTableIndexBufferEntryNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTableIndexBufferEntryNVX =
             #{offset VkObjectTableIndexBufferEntryNVX, type}
        type FieldIsArray "type" VkObjectTableIndexBufferEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableIndexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTableIndexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTableIndexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasField "flags" VkObjectTableIndexBufferEntryNVX where
        type FieldType "flags" VkObjectTableIndexBufferEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTableIndexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTableIndexBufferEntryNVX =
             #{offset VkObjectTableIndexBufferEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTableIndexBufferEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableIndexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTableIndexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTableIndexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkObjectTableIndexBufferEntryNVX where
        type FieldType "buffer" VkObjectTableIndexBufferEntryNVX = VkBuffer
        type FieldOptional "buffer" VkObjectTableIndexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkObjectTableIndexBufferEntryNVX =
             #{offset VkObjectTableIndexBufferEntryNVX, buffer}
        type FieldIsArray "buffer" VkObjectTableIndexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableIndexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkObjectTableIndexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkObjectTableIndexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         HasField "indexType" VkObjectTableIndexBufferEntryNVX where
        type FieldType "indexType" VkObjectTableIndexBufferEntryNVX =
             VkIndexType
        type FieldOptional "indexType" VkObjectTableIndexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "indexType" VkObjectTableIndexBufferEntryNVX =
             #{offset VkObjectTableIndexBufferEntryNVX, indexType}
        type FieldIsArray "indexType" VkObjectTableIndexBufferEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableIndexBufferEntryNVX, indexType}

instance {-# OVERLAPPING #-}
         CanReadField "indexType" VkObjectTableIndexBufferEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, indexType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, indexType}

instance {-# OVERLAPPING #-}
         CanWriteField "indexType" VkObjectTableIndexBufferEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, indexType}

instance Show VkObjectTableIndexBufferEntryNVX where
        showsPrec d x
          = showString "VkObjectTableIndexBufferEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) .
                              showString ", " .
                                showString "indexType = " .
                                  showsPrec d (getField @"indexType" x) . showChar '}'
