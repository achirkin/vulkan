#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseBufferMemoryBindInfo
       (VkSparseBufferMemoryBindInfo(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Handles                   (VkBuffer)
import           Graphics.Vulkan.Types.Struct.VkSparseMemoryBind (VkSparseMemoryBind)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkSparseBufferMemoryBindInfo {
--   >     VkBuffer buffer;
--   >     uint32_t               bindCount;
--   >     const VkSparseMemoryBind* pBinds;
--   > } VkSparseBufferMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSparseBufferMemoryBindInfo.html VkSparseBufferMemoryBindInfo registry at www.khronos.org>
data VkSparseBufferMemoryBindInfo = VkSparseBufferMemoryBindInfo## Addr##
                                                                  ByteArray##

instance Eq VkSparseBufferMemoryBindInfo where
        (VkSparseBufferMemoryBindInfo## a _) ==
          x@(VkSparseBufferMemoryBindInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseBufferMemoryBindInfo where
        (VkSparseBufferMemoryBindInfo## a _) `compare`
          x@(VkSparseBufferMemoryBindInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseBufferMemoryBindInfo where
        sizeOf ~_ = #{size VkSparseBufferMemoryBindInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseBufferMemoryBindInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseBufferMemoryBindInfo where
        unsafeAddr (VkSparseBufferMemoryBindInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseBufferMemoryBindInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseBufferMemoryBindInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseBufferMemoryBindInfo where
        type StructFields VkSparseBufferMemoryBindInfo =
             '["buffer", "bindCount", "pBinds"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSparseBufferMemoryBindInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "buffer" VkSparseBufferMemoryBindInfo where
        type FieldType "buffer" VkSparseBufferMemoryBindInfo = VkBuffer
        type FieldOptional "buffer" VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkSparseBufferMemoryBindInfo =
             #{offset VkSparseBufferMemoryBindInfo, buffer}
        type FieldIsArray "buffer" VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseBufferMemoryBindInfo, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkSparseBufferMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseBufferMemoryBindInfo, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseBufferMemoryBindInfo, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkSparseBufferMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseBufferMemoryBindInfo, buffer}

instance {-# OVERLAPPING #-}
         HasField "bindCount" VkSparseBufferMemoryBindInfo where
        type FieldType "bindCount" VkSparseBufferMemoryBindInfo = Word32
        type FieldOptional "bindCount" VkSparseBufferMemoryBindInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "bindCount" VkSparseBufferMemoryBindInfo =
             #{offset VkSparseBufferMemoryBindInfo, bindCount}
        type FieldIsArray "bindCount" VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseBufferMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanReadField "bindCount" VkSparseBufferMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseBufferMemoryBindInfo, bindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseBufferMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bindCount" VkSparseBufferMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseBufferMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         HasField "pBinds" VkSparseBufferMemoryBindInfo where
        type FieldType "pBinds" VkSparseBufferMemoryBindInfo =
             Ptr VkSparseMemoryBind
        type FieldOptional "pBinds" VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pBinds" VkSparseBufferMemoryBindInfo =
             #{offset VkSparseBufferMemoryBindInfo, pBinds}
        type FieldIsArray "pBinds" VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseBufferMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanReadField "pBinds" VkSparseBufferMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseBufferMemoryBindInfo, pBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseBufferMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pBinds" VkSparseBufferMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseBufferMemoryBindInfo, pBinds}

instance Show VkSparseBufferMemoryBindInfo where
        showsPrec d x
          = showString "VkSparseBufferMemoryBindInfo {" .
              showString "buffer = " .
                showsPrec d (getField @"buffer" x) .
                  showString ", " .
                    showString "bindCount = " .
                      showsPrec d (getField @"bindCount" x) .
                        showString ", " .
                          showString "pBinds = " .
                            showsPrec d (getField @"pBinds" x) . showChar '}'
