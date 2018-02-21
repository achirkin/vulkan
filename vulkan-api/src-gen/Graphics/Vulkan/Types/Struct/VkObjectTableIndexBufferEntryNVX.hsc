#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkObjectTableIndexBufferEntryNVX
       (VkObjectTableIndexBufferEntryNVX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkIndexType                (VkIndexType)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX       (VkObjectEntryTypeNVX)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX (VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.Handles                         (VkBuffer)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkObjectTableIndexBufferEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkBuffer                     buffer;
--   >     VkIndexType                  indexType;
--   > } VkObjectTableIndexBufferEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkObjectTableIndexBufferEntryNVX.html VkObjectTableIndexBufferEntryNVX registry at www.khronos.org>
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
         HasVkType VkObjectTableIndexBufferEntryNVX where
        type VkTypeMType VkObjectTableIndexBufferEntryNVX =
             VkObjectEntryTypeNVX

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkObjectTableIndexBufferEntryNVX, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, type}

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

instance CanReadField "type" VkObjectTableIndexBufferEntryNVX where
        {-# INLINE getField #-}
        getField = vkType

        {-# INLINE readField #-}
        readField = readVkType

instance CanWriteField "type" VkObjectTableIndexBufferEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkType

instance {-# OVERLAPPING #-}
         HasVkFlags VkObjectTableIndexBufferEntryNVX where
        type VkFlagsMType VkObjectTableIndexBufferEntryNVX =
             VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkObjectTableIndexBufferEntryNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, flags}

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

instance CanReadField "flags" VkObjectTableIndexBufferEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkObjectTableIndexBufferEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkBuffer VkObjectTableIndexBufferEntryNVX where
        type VkBufferMType VkObjectTableIndexBufferEntryNVX = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkObjectTableIndexBufferEntryNVX, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, buffer}

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

instance CanReadField "buffer" VkObjectTableIndexBufferEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer" VkObjectTableIndexBufferEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance {-# OVERLAPPING #-}
         HasVkIndexType VkObjectTableIndexBufferEntryNVX where
        type VkIndexTypeMType VkObjectTableIndexBufferEntryNVX =
             VkIndexType

        {-# NOINLINE vkIndexType #-}
        vkIndexType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, indexType})

        {-# INLINE vkIndexTypeByteOffset #-}
        vkIndexTypeByteOffset ~_
          = #{offset VkObjectTableIndexBufferEntryNVX, indexType}

        {-# INLINE readVkIndexType #-}
        readVkIndexType p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, indexType}

        {-# INLINE writeVkIndexType #-}
        writeVkIndexType p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, indexType}

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

instance CanReadField "indexType" VkObjectTableIndexBufferEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkIndexType

        {-# INLINE readField #-}
        readField = readVkIndexType

instance CanWriteField "indexType" VkObjectTableIndexBufferEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkIndexType

instance Show VkObjectTableIndexBufferEntryNVX where
        showsPrec d x
          = showString "VkObjectTableIndexBufferEntryNVX {" .
              showString "vkType = " .
                showsPrec d (vkType x) .
                  showString ", " .
                    showString "vkFlags = " .
                      showsPrec d (vkFlags x) .
                        showString ", " .
                          showString "vkBuffer = " .
                            showsPrec d (vkBuffer x) .
                              showString ", " .
                                showString "vkIndexType = " .
                                  showsPrec d (vkIndexType x) . showChar '}'
