#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBindInfo
       (VkSparseImageMemoryBindInfo(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Handles                        (VkImage)
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBind (VkSparseImageMemoryBind)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageMemoryBindInfo {
--   >     VkImage image;
--   >     uint32_t               bindCount;
--   >     const VkSparseImageMemoryBind* pBinds;
--   > } VkSparseImageMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSparseImageMemoryBindInfo.html VkSparseImageMemoryBindInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkImage VkSparseImageMemoryBindInfo
         where
        type VkImageMType VkSparseImageMemoryBindInfo = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBindInfo, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkSparseImageMemoryBindInfo, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkSparseImageMemoryBindInfo, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkSparseImageMemoryBindInfo, image}

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

instance CanReadField "image" VkSparseImageMemoryBindInfo where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image" VkSparseImageMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance {-# OVERLAPPING #-}
         HasVkBindCount VkSparseImageMemoryBindInfo where
        type VkBindCountMType VkSparseImageMemoryBindInfo = Word32

        {-# NOINLINE vkBindCount #-}
        vkBindCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBindInfo, bindCount})

        {-# INLINE vkBindCountByteOffset #-}
        vkBindCountByteOffset ~_
          = #{offset VkSparseImageMemoryBindInfo, bindCount}

        {-# INLINE readVkBindCount #-}
        readVkBindCount p
          = peekByteOff p #{offset VkSparseImageMemoryBindInfo, bindCount}

        {-# INLINE writeVkBindCount #-}
        writeVkBindCount p
          = pokeByteOff p #{offset VkSparseImageMemoryBindInfo, bindCount}

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

instance CanReadField "bindCount" VkSparseImageMemoryBindInfo where
        {-# INLINE getField #-}
        getField = vkBindCount

        {-# INLINE readField #-}
        readField = readVkBindCount

instance CanWriteField "bindCount" VkSparseImageMemoryBindInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkBindCount

instance {-# OVERLAPPING #-}
         HasVkPBinds VkSparseImageMemoryBindInfo where
        type VkPBindsMType VkSparseImageMemoryBindInfo =
             Ptr VkSparseImageMemoryBind

        {-# NOINLINE vkPBinds #-}
        vkPBinds x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBindInfo, pBinds})

        {-# INLINE vkPBindsByteOffset #-}
        vkPBindsByteOffset ~_
          = #{offset VkSparseImageMemoryBindInfo, pBinds}

        {-# INLINE readVkPBinds #-}
        readVkPBinds p
          = peekByteOff p #{offset VkSparseImageMemoryBindInfo, pBinds}

        {-# INLINE writeVkPBinds #-}
        writeVkPBinds p
          = pokeByteOff p #{offset VkSparseImageMemoryBindInfo, pBinds}

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

instance CanReadField "pBinds" VkSparseImageMemoryBindInfo where
        {-# INLINE getField #-}
        getField = vkPBinds

        {-# INLINE readField #-}
        readField = readVkPBinds

instance CanWriteField "pBinds" VkSparseImageMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPBinds

instance Show VkSparseImageMemoryBindInfo where
        showsPrec d x
          = showString "VkSparseImageMemoryBindInfo {" .
              showString "vkImage = " .
                showsPrec d (vkImage x) .
                  showString ", " .
                    showString "vkBindCount = " .
                      showsPrec d (vkBindCount x) .
                        showString ", " .
                          showString "vkPBinds = " . showsPrec d (vkPBinds x) . showChar '}'
