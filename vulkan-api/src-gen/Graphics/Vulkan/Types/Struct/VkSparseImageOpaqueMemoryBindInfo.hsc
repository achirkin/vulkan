#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageOpaqueMemoryBindInfo
       (VkSparseImageOpaqueMemoryBindInfo(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Base                                        (Addr##,
                                                                  ByteArray##,
                                                                  byteArrayContents##,
                                                                  plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Handles                   (VkImage)
import           Graphics.Vulkan.Types.Struct.VkSparseMemoryBind (VkSparseMemoryBind)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageOpaqueMemoryBindInfo {
--   >     VkImage image;
--   >     uint32_t               bindCount;
--   >     const VkSparseMemoryBind* pBinds;
--   > } VkSparseImageOpaqueMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageOpaqueMemoryBindInfo VkSparseImageOpaqueMemoryBindInfo registry at www.khronos.org>
data VkSparseImageOpaqueMemoryBindInfo = VkSparseImageOpaqueMemoryBindInfo## Addr##
                                                                            ByteArray##

instance Eq VkSparseImageOpaqueMemoryBindInfo where
        (VkSparseImageOpaqueMemoryBindInfo## a _) ==
          x@(VkSparseImageOpaqueMemoryBindInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageOpaqueMemoryBindInfo where
        (VkSparseImageOpaqueMemoryBindInfo## a _) `compare`
          x@(VkSparseImageOpaqueMemoryBindInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageOpaqueMemoryBindInfo where
        sizeOf ~_ = #{size VkSparseImageOpaqueMemoryBindInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageOpaqueMemoryBindInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageOpaqueMemoryBindInfo where
        unsafeAddr (VkSparseImageOpaqueMemoryBindInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageOpaqueMemoryBindInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageOpaqueMemoryBindInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageOpaqueMemoryBindInfo where
        type StructFields VkSparseImageOpaqueMemoryBindInfo =
             '["image", "bindCount", "pBinds"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageOpaqueMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageOpaqueMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageOpaqueMemoryBindInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "image" VkSparseImageOpaqueMemoryBindInfo where
        type FieldType "image" VkSparseImageOpaqueMemoryBindInfo = VkImage
        type FieldOptional "image" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkSparseImageOpaqueMemoryBindInfo =
             #{offset VkSparseImageOpaqueMemoryBindInfo, image}
        type FieldIsArray "image" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageOpaqueMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkSparseImageOpaqueMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageOpaqueMemoryBindInfo, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkSparseImageOpaqueMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         HasField "bindCount" VkSparseImageOpaqueMemoryBindInfo where
        type FieldType "bindCount" VkSparseImageOpaqueMemoryBindInfo =
             Word32
        type FieldOptional "bindCount" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "bindCount" VkSparseImageOpaqueMemoryBindInfo =
             #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}
        type FieldIsArray "bindCount" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanReadField "bindCount" VkSparseImageOpaqueMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bindCount" VkSparseImageOpaqueMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         HasField "pBinds" VkSparseImageOpaqueMemoryBindInfo where
        type FieldType "pBinds" VkSparseImageOpaqueMemoryBindInfo =
             Ptr VkSparseMemoryBind
        type FieldOptional "pBinds" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pBinds" VkSparseImageOpaqueMemoryBindInfo =
             #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}
        type FieldIsArray "pBinds" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanReadField "pBinds" VkSparseImageOpaqueMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pBinds" VkSparseImageOpaqueMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}

instance Show VkSparseImageOpaqueMemoryBindInfo where
        showsPrec d x
          = showString "VkSparseImageOpaqueMemoryBindInfo {" .
              showString "image = " .
                showsPrec d (getField @"image" x) .
                  showString ", " .
                    showString "bindCount = " .
                      showsPrec d (getField @"bindCount" x) .
                        showString ", " .
                          showString "pBinds = " .
                            showsPrec d (getField @"pBinds" x) . showChar '}'
