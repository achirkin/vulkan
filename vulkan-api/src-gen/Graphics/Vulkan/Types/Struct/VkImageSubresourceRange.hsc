#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSubresourceRange
       (VkImageSubresourceRange(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags (VkImageAspectFlags)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkImageSubresourceRange {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               baseMipLevel;
--   >     uint32_t               levelCount;
--   >     uint32_t               baseArrayLayer;
--   >     uint32_t               layerCount;
--   > } VkImageSubresourceRange;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageSubresourceRange.html VkImageSubresourceRange registry at www.khronos.org>
data VkImageSubresourceRange = VkImageSubresourceRange## Addr##
                                                        ByteArray##

instance Eq VkImageSubresourceRange where
        (VkImageSubresourceRange## a _) == x@(VkImageSubresourceRange## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSubresourceRange where
        (VkImageSubresourceRange## a _) `compare`
          x@(VkImageSubresourceRange## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSubresourceRange where
        sizeOf ~_ = #{size VkImageSubresourceRange}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageSubresourceRange}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSubresourceRange where
        unsafeAddr (VkImageSubresourceRange## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSubresourceRange## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSubresourceRange## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSubresourceRange where
        type StructFields VkImageSubresourceRange =
             '["aspectMask", "baseMipLevel", "levelCount", "baseArrayLayer", -- ' closing tick for hsc2hs
               "layerCount"]
        type CUnionType VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSubresourceRange = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkAspectMask VkImageSubresourceRange where
        type VkAspectMaskMType VkImageSubresourceRange = VkImageAspectFlags

        {-# NOINLINE vkAspectMask #-}
        vkAspectMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, aspectMask})

        {-# INLINE vkAspectMaskByteOffset #-}
        vkAspectMaskByteOffset ~_
          = #{offset VkImageSubresourceRange, aspectMask}

        {-# INLINE readVkAspectMask #-}
        readVkAspectMask p
          = peekByteOff p #{offset VkImageSubresourceRange, aspectMask}

        {-# INLINE writeVkAspectMask #-}
        writeVkAspectMask p
          = pokeByteOff p #{offset VkImageSubresourceRange, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkImageSubresourceRange where
        type FieldType "aspectMask" VkImageSubresourceRange =
             VkImageAspectFlags
        type FieldOptional "aspectMask" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkImageSubresourceRange =
             #{offset VkImageSubresourceRange, aspectMask}
        type FieldIsArray "aspectMask" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceRange, aspectMask}

instance CanReadField "aspectMask" VkImageSubresourceRange where
        {-# INLINE getField #-}
        getField = vkAspectMask

        {-# INLINE readField #-}
        readField = readVkAspectMask

instance CanWriteField "aspectMask" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField = writeVkAspectMask

instance {-# OVERLAPPING #-}
         HasVkBaseMipLevel VkImageSubresourceRange where
        type VkBaseMipLevelMType VkImageSubresourceRange = Word32

        {-# NOINLINE vkBaseMipLevel #-}
        vkBaseMipLevel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, baseMipLevel})

        {-# INLINE vkBaseMipLevelByteOffset #-}
        vkBaseMipLevelByteOffset ~_
          = #{offset VkImageSubresourceRange, baseMipLevel}

        {-# INLINE readVkBaseMipLevel #-}
        readVkBaseMipLevel p
          = peekByteOff p #{offset VkImageSubresourceRange, baseMipLevel}

        {-# INLINE writeVkBaseMipLevel #-}
        writeVkBaseMipLevel p
          = pokeByteOff p #{offset VkImageSubresourceRange, baseMipLevel}

instance {-# OVERLAPPING #-}
         HasField "baseMipLevel" VkImageSubresourceRange where
        type FieldType "baseMipLevel" VkImageSubresourceRange = Word32
        type FieldOptional "baseMipLevel" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "baseMipLevel" VkImageSubresourceRange =
             #{offset VkImageSubresourceRange, baseMipLevel}
        type FieldIsArray "baseMipLevel" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceRange, baseMipLevel}

instance CanReadField "baseMipLevel" VkImageSubresourceRange where
        {-# INLINE getField #-}
        getField = vkBaseMipLevel

        {-# INLINE readField #-}
        readField = readVkBaseMipLevel

instance CanWriteField "baseMipLevel" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField = writeVkBaseMipLevel

instance {-# OVERLAPPING #-}
         HasVkLevelCount VkImageSubresourceRange where
        type VkLevelCountMType VkImageSubresourceRange = Word32

        {-# NOINLINE vkLevelCount #-}
        vkLevelCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, levelCount})

        {-# INLINE vkLevelCountByteOffset #-}
        vkLevelCountByteOffset ~_
          = #{offset VkImageSubresourceRange, levelCount}

        {-# INLINE readVkLevelCount #-}
        readVkLevelCount p
          = peekByteOff p #{offset VkImageSubresourceRange, levelCount}

        {-# INLINE writeVkLevelCount #-}
        writeVkLevelCount p
          = pokeByteOff p #{offset VkImageSubresourceRange, levelCount}

instance {-# OVERLAPPING #-}
         HasField "levelCount" VkImageSubresourceRange where
        type FieldType "levelCount" VkImageSubresourceRange = Word32
        type FieldOptional "levelCount" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "levelCount" VkImageSubresourceRange =
             #{offset VkImageSubresourceRange, levelCount}
        type FieldIsArray "levelCount" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceRange, levelCount}

instance CanReadField "levelCount" VkImageSubresourceRange where
        {-# INLINE getField #-}
        getField = vkLevelCount

        {-# INLINE readField #-}
        readField = readVkLevelCount

instance CanWriteField "levelCount" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField = writeVkLevelCount

instance {-# OVERLAPPING #-}
         HasVkBaseArrayLayer VkImageSubresourceRange where
        type VkBaseArrayLayerMType VkImageSubresourceRange = Word32

        {-# NOINLINE vkBaseArrayLayer #-}
        vkBaseArrayLayer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, baseArrayLayer})

        {-# INLINE vkBaseArrayLayerByteOffset #-}
        vkBaseArrayLayerByteOffset ~_
          = #{offset VkImageSubresourceRange, baseArrayLayer}

        {-# INLINE readVkBaseArrayLayer #-}
        readVkBaseArrayLayer p
          = peekByteOff p #{offset VkImageSubresourceRange, baseArrayLayer}

        {-# INLINE writeVkBaseArrayLayer #-}
        writeVkBaseArrayLayer p
          = pokeByteOff p #{offset VkImageSubresourceRange, baseArrayLayer}

instance {-# OVERLAPPING #-}
         HasField "baseArrayLayer" VkImageSubresourceRange where
        type FieldType "baseArrayLayer" VkImageSubresourceRange = Word32
        type FieldOptional "baseArrayLayer" VkImageSubresourceRange =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "baseArrayLayer" VkImageSubresourceRange =
             #{offset VkImageSubresourceRange, baseArrayLayer}
        type FieldIsArray "baseArrayLayer" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceRange, baseArrayLayer}

instance CanReadField "baseArrayLayer" VkImageSubresourceRange
         where
        {-# INLINE getField #-}
        getField = vkBaseArrayLayer

        {-# INLINE readField #-}
        readField = readVkBaseArrayLayer

instance CanWriteField "baseArrayLayer" VkImageSubresourceRange
         where
        {-# INLINE writeField #-}
        writeField = writeVkBaseArrayLayer

instance {-# OVERLAPPING #-}
         HasVkLayerCount VkImageSubresourceRange where
        type VkLayerCountMType VkImageSubresourceRange = Word32

        {-# NOINLINE vkLayerCount #-}
        vkLayerCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, layerCount})

        {-# INLINE vkLayerCountByteOffset #-}
        vkLayerCountByteOffset ~_
          = #{offset VkImageSubresourceRange, layerCount}

        {-# INLINE readVkLayerCount #-}
        readVkLayerCount p
          = peekByteOff p #{offset VkImageSubresourceRange, layerCount}

        {-# INLINE writeVkLayerCount #-}
        writeVkLayerCount p
          = pokeByteOff p #{offset VkImageSubresourceRange, layerCount}

instance {-# OVERLAPPING #-}
         HasField "layerCount" VkImageSubresourceRange where
        type FieldType "layerCount" VkImageSubresourceRange = Word32
        type FieldOptional "layerCount" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layerCount" VkImageSubresourceRange =
             #{offset VkImageSubresourceRange, layerCount}
        type FieldIsArray "layerCount" VkImageSubresourceRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSubresourceRange, layerCount}

instance CanReadField "layerCount" VkImageSubresourceRange where
        {-# INLINE getField #-}
        getField = vkLayerCount

        {-# INLINE readField #-}
        readField = readVkLayerCount

instance CanWriteField "layerCount" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField = writeVkLayerCount

instance Show VkImageSubresourceRange where
        showsPrec d x
          = showString "VkImageSubresourceRange {" .
              showString "vkAspectMask = " .
                showsPrec d (vkAspectMask x) .
                  showString ", " .
                    showString "vkBaseMipLevel = " .
                      showsPrec d (vkBaseMipLevel x) .
                        showString ", " .
                          showString "vkLevelCount = " .
                            showsPrec d (vkLevelCount x) .
                              showString ", " .
                                showString "vkBaseArrayLayer = " .
                                  showsPrec d (vkBaseArrayLayer x) .
                                    showString ", " .
                                      showString "vkLayerCount = " .
                                        showsPrec d (vkLayerCount x) . showChar '}'
