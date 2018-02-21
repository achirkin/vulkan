#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkImageBlit (VkImageBlit(..))
       where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                                          (KnownNat,
                                                                        natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers (VkImageSubresourceLayers)
import           Graphics.Vulkan.Types.Struct.VkOffset3D               (VkOffset3D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkImageBlit {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffsets[2];
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffsets[2];
--   > } VkImageBlit;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageBlit.html VkImageBlit registry at www.khronos.org>
data VkImageBlit = VkImageBlit## Addr## ByteArray##

instance Eq VkImageBlit where
        (VkImageBlit## a _) == x@(VkImageBlit## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageBlit where
        (VkImageBlit## a _) `compare` x@(VkImageBlit## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageBlit where
        sizeOf ~_ = #{size VkImageBlit}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageBlit}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageBlit where
        unsafeAddr (VkImageBlit## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageBlit## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageBlit## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageBlit where
        type StructFields VkImageBlit =
             '["srcSubresource", "srcOffsets", "dstSubresource", "dstOffsets"] -- ' closing tick for hsc2hs
        type CUnionType VkImageBlit = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageBlit = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageBlit = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSrcSubresource VkImageBlit where
        type VkSrcSubresourceMType VkImageBlit = VkImageSubresourceLayers

        {-# NOINLINE vkSrcSubresource #-}
        vkSrcSubresource x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageBlit, srcSubresource})

        {-# INLINE vkSrcSubresourceByteOffset #-}
        vkSrcSubresourceByteOffset ~_
          = #{offset VkImageBlit, srcSubresource}

        {-# INLINE readVkSrcSubresource #-}
        readVkSrcSubresource p
          = peekByteOff p #{offset VkImageBlit, srcSubresource}

        {-# INLINE writeVkSrcSubresource #-}
        writeVkSrcSubresource p
          = pokeByteOff p #{offset VkImageBlit, srcSubresource}

instance {-# OVERLAPPING #-} HasField "srcSubresource" VkImageBlit
         where
        type FieldType "srcSubresource" VkImageBlit =
             VkImageSubresourceLayers
        type FieldOptional "srcSubresource" VkImageBlit = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcSubresource" VkImageBlit =
             #{offset VkImageBlit, srcSubresource}
        type FieldIsArray "srcSubresource" VkImageBlit = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageBlit, srcSubresource}

instance CanReadField "srcSubresource" VkImageBlit where
        {-# INLINE getField #-}
        getField = vkSrcSubresource

        {-# INLINE readField #-}
        readField = readVkSrcSubresource

instance CanWriteField "srcSubresource" VkImageBlit where
        {-# INLINE writeField #-}
        writeField = writeVkSrcSubresource

instance {-# OVERLAPPING #-} HasVkSrcOffsetsArray VkImageBlit where
        type VkSrcOffsetsArrayMType VkImageBlit = VkOffset3D

        {-# NOINLINE vkSrcOffsetsArray #-}
        vkSrcOffsetsArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: VkOffset3D) +
                    #{offset VkImageBlit, srcOffsets}))

        {-# INLINE vkSrcOffsetsArrayByteOffset #-}
        vkSrcOffsetsArrayByteOffset ~_
          = #{offset VkImageBlit, srcOffsets}

        {-# INLINE readVkSrcOffsetsArray #-}
        readVkSrcOffsetsArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: VkOffset3D) +
                 #{offset VkImageBlit, srcOffsets})

        {-# INLINE writeVkSrcOffsetsArray #-}
        writeVkSrcOffsetsArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: VkOffset3D) +
                 #{offset VkImageBlit, srcOffsets})

instance {-# OVERLAPPING #-} HasField "srcOffsets" VkImageBlit
         where
        type FieldType "srcOffsets" VkImageBlit = VkOffset3D
        type FieldOptional "srcOffsets" VkImageBlit = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcOffsets" VkImageBlit =
             #{offset VkImageBlit, srcOffsets}
        type FieldIsArray "srcOffsets" VkImageBlit = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageBlit, srcOffsets}

instance (KnownNat idx,
          IndexInBounds "srcOffsets" idx VkImageBlit) =>
         CanReadFieldArray "srcOffsets" idx VkImageBlit
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "srcOffsets" 0 VkImageBlit #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "srcOffsets" 1 VkImageBlit #-}
        type FieldArrayLength "srcOffsets" VkImageBlit = 2

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 2

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkSrcOffsetsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkSrcOffsetsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "srcOffsets" idx VkImageBlit) =>
         CanWriteFieldArray "srcOffsets" idx VkImageBlit
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "srcOffsets" 0 VkImageBlit #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "srcOffsets" 1 VkImageBlit #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkSrcOffsetsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkDstSubresource VkImageBlit where
        type VkDstSubresourceMType VkImageBlit = VkImageSubresourceLayers

        {-# NOINLINE vkDstSubresource #-}
        vkDstSubresource x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageBlit, dstSubresource})

        {-# INLINE vkDstSubresourceByteOffset #-}
        vkDstSubresourceByteOffset ~_
          = #{offset VkImageBlit, dstSubresource}

        {-# INLINE readVkDstSubresource #-}
        readVkDstSubresource p
          = peekByteOff p #{offset VkImageBlit, dstSubresource}

        {-# INLINE writeVkDstSubresource #-}
        writeVkDstSubresource p
          = pokeByteOff p #{offset VkImageBlit, dstSubresource}

instance {-# OVERLAPPING #-} HasField "dstSubresource" VkImageBlit
         where
        type FieldType "dstSubresource" VkImageBlit =
             VkImageSubresourceLayers
        type FieldOptional "dstSubresource" VkImageBlit = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstSubresource" VkImageBlit =
             #{offset VkImageBlit, dstSubresource}
        type FieldIsArray "dstSubresource" VkImageBlit = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageBlit, dstSubresource}

instance CanReadField "dstSubresource" VkImageBlit where
        {-# INLINE getField #-}
        getField = vkDstSubresource

        {-# INLINE readField #-}
        readField = readVkDstSubresource

instance CanWriteField "dstSubresource" VkImageBlit where
        {-# INLINE writeField #-}
        writeField = writeVkDstSubresource

instance {-# OVERLAPPING #-} HasVkDstOffsetsArray VkImageBlit where
        type VkDstOffsetsArrayMType VkImageBlit = VkOffset3D

        {-# NOINLINE vkDstOffsetsArray #-}
        vkDstOffsetsArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: VkOffset3D) +
                    #{offset VkImageBlit, dstOffsets}))

        {-# INLINE vkDstOffsetsArrayByteOffset #-}
        vkDstOffsetsArrayByteOffset ~_
          = #{offset VkImageBlit, dstOffsets}

        {-# INLINE readVkDstOffsetsArray #-}
        readVkDstOffsetsArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: VkOffset3D) +
                 #{offset VkImageBlit, dstOffsets})

        {-# INLINE writeVkDstOffsetsArray #-}
        writeVkDstOffsetsArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: VkOffset3D) +
                 #{offset VkImageBlit, dstOffsets})

instance {-# OVERLAPPING #-} HasField "dstOffsets" VkImageBlit
         where
        type FieldType "dstOffsets" VkImageBlit = VkOffset3D
        type FieldOptional "dstOffsets" VkImageBlit = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstOffsets" VkImageBlit =
             #{offset VkImageBlit, dstOffsets}
        type FieldIsArray "dstOffsets" VkImageBlit = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageBlit, dstOffsets}

instance (KnownNat idx,
          IndexInBounds "dstOffsets" idx VkImageBlit) =>
         CanReadFieldArray "dstOffsets" idx VkImageBlit
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "dstOffsets" 0 VkImageBlit #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "dstOffsets" 1 VkImageBlit #-}
        type FieldArrayLength "dstOffsets" VkImageBlit = 2

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 2

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkDstOffsetsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkDstOffsetsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "dstOffsets" idx VkImageBlit) =>
         CanWriteFieldArray "dstOffsets" idx VkImageBlit
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "dstOffsets" 0 VkImageBlit #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "dstOffsets" 1 VkImageBlit #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkDstOffsetsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance Show VkImageBlit where
        showsPrec d x
          = showString "VkImageBlit {" .
              showString "vkSrcSubresource = " .
                showsPrec d (vkSrcSubresource x) .
                  showString ", " .
                    showString "vkSrcOffsetsArray = [" .
                      showsPrec d (map (vkSrcOffsetsArray x) [1 .. 2]) .
                        showChar ']' .
                          showString ", " .
                            showString "vkDstSubresource = " .
                              showsPrec d (vkDstSubresource x) .
                                showString ", " .
                                  showString "vkDstOffsetsArray = [" .
                                    showsPrec d (map (vkDstOffsetsArray x) [1 .. 2]) .
                                      showChar ']' . showChar '}'
