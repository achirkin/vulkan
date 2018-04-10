#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBindInfo
       (VkSparseImageMemoryBindInfo(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Base                                             (Addr##, ByteArray##,
                                                                       byteArrayContents##,
                                                                       plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Handles                        (VkImage)
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBind (VkSparseImageMemoryBind)
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageMemoryBindInfo {
--   >     VkImage image;
--   >     uint32_t               bindCount;
--   >     const VkSparseImageMemoryBind* pBinds;
--   > } VkSparseImageMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkSparseImageMemoryBindInfo VkSparseImageMemoryBindInfo registry at www.khronos.org>
data VkSparseImageMemoryBindInfo = VkSparseImageMemoryBindInfo## Addr##
                                                                ByteArray##

instance Eq VkSparseImageMemoryBindInfo where
        (VkSparseImageMemoryBindInfo## a _) ==
          x@(VkSparseImageMemoryBindInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageMemoryBindInfo where
        (VkSparseImageMemoryBindInfo## a _) `compare`
          x@(VkSparseImageMemoryBindInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageMemoryBindInfo where
        sizeOf ~_ = #{size VkSparseImageMemoryBindInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSparseImageMemoryBindInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageMemoryBindInfo where
        unsafeAddr (VkSparseImageMemoryBindInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageMemoryBindInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageMemoryBindInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageMemoryBindInfo where
        type StructFields VkSparseImageMemoryBindInfo =
             '["image", "bindCount", "pBinds"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageMemoryBindInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "image" VkSparseImageMemoryBindInfo where
        type FieldType "image" VkSparseImageMemoryBindInfo = VkImage
        type FieldOptional "image" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkSparseImageMemoryBindInfo =
             #{offset VkSparseImageMemoryBindInfo, image}
        type FieldIsArray "image" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkSparseImageMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBindInfo, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkSparseImageMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         HasField "bindCount" VkSparseImageMemoryBindInfo where
        type FieldType "bindCount" VkSparseImageMemoryBindInfo = Word32
        type FieldOptional "bindCount" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "bindCount" VkSparseImageMemoryBindInfo =
             #{offset VkSparseImageMemoryBindInfo, bindCount}
        type FieldIsArray "bindCount" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanReadField "bindCount" VkSparseImageMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBindInfo, bindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bindCount" VkSparseImageMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         HasField "pBinds" VkSparseImageMemoryBindInfo where
        type FieldType "pBinds" VkSparseImageMemoryBindInfo =
             Ptr VkSparseImageMemoryBind
        type FieldOptional "pBinds" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pBinds" VkSparseImageMemoryBindInfo =
             #{offset VkSparseImageMemoryBindInfo, pBinds}
        type FieldIsArray "pBinds" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanReadField "pBinds" VkSparseImageMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBindInfo, pBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pBinds" VkSparseImageMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBindInfo, pBinds}

instance Show VkSparseImageMemoryBindInfo where
        showsPrec d x
          = showString "VkSparseImageMemoryBindInfo {" .
              showString "image = " .
                showsPrec d (getField @"image" x) .
                  showString ", " .
                    showString "bindCount = " .
                      showsPrec d (getField @"bindCount" x) .
                        showString ", " .
                          showString "pBinds = " .
                            showsPrec d (getField @"pBinds" x) . showChar '}'
