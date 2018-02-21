#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageOpaqueMemoryBindInfo
       (VkSparseImageOpaqueMemoryBindInfo(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Handles                   (VkImage)
import           Graphics.Vulkan.Types.Struct.VkSparseMemoryBind (VkSparseMemoryBind)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageOpaqueMemoryBindInfo {
--   >     VkImage image;
--   >     uint32_t               bindCount;
--   >     const VkSparseMemoryBind* pBinds;
--   > } VkSparseImageOpaqueMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSparseImageOpaqueMemoryBindInfo.html VkSparseImageOpaqueMemoryBindInfo registry at www.khronos.org>
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
         HasVkImage VkSparseImageOpaqueMemoryBindInfo where
        type VkImageMType VkSparseImageOpaqueMemoryBindInfo = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageOpaqueMemoryBindInfo, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkSparseImageOpaqueMemoryBindInfo, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, image}

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

instance CanReadField "image" VkSparseImageOpaqueMemoryBindInfo
         where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image" VkSparseImageOpaqueMemoryBindInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance {-# OVERLAPPING #-}
         HasVkBindCount VkSparseImageOpaqueMemoryBindInfo where
        type VkBindCountMType VkSparseImageOpaqueMemoryBindInfo = Word32

        {-# NOINLINE vkBindCount #-}
        vkBindCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount})

        {-# INLINE vkBindCountByteOffset #-}
        vkBindCountByteOffset ~_
          = #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}

        {-# INLINE readVkBindCount #-}
        readVkBindCount p
          = peekByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}

        {-# INLINE writeVkBindCount #-}
        writeVkBindCount p
          = pokeByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}

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

instance CanReadField "bindCount" VkSparseImageOpaqueMemoryBindInfo
         where
        {-# INLINE getField #-}
        getField = vkBindCount

        {-# INLINE readField #-}
        readField = readVkBindCount

instance CanWriteField "bindCount"
           VkSparseImageOpaqueMemoryBindInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkBindCount

instance {-# OVERLAPPING #-}
         HasVkPBinds VkSparseImageOpaqueMemoryBindInfo where
        type VkPBindsMType VkSparseImageOpaqueMemoryBindInfo =
             Ptr VkSparseMemoryBind

        {-# NOINLINE vkPBinds #-}
        vkPBinds x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds})

        {-# INLINE vkPBindsByteOffset #-}
        vkPBindsByteOffset ~_
          = #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}

        {-# INLINE readVkPBinds #-}
        readVkPBinds p
          = peekByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}

        {-# INLINE writeVkPBinds #-}
        writeVkPBinds p
          = pokeByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}

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

instance CanReadField "pBinds" VkSparseImageOpaqueMemoryBindInfo
         where
        {-# INLINE getField #-}
        getField = vkPBinds

        {-# INLINE readField #-}
        readField = readVkPBinds

instance CanWriteField "pBinds" VkSparseImageOpaqueMemoryBindInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPBinds

instance Show VkSparseImageOpaqueMemoryBindInfo where
        showsPrec d x
          = showString "VkSparseImageOpaqueMemoryBindInfo {" .
              showString "vkImage = " .
                showsPrec d (vkImage x) .
                  showString ", " .
                    showString "vkBindCount = " .
                      showsPrec d (vkBindCount x) .
                        showString ", " .
                          showString "vkPBinds = " . showsPrec d (vkPBinds x) . showChar '}'
