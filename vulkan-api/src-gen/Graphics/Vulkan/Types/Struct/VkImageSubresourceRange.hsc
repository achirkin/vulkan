#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSubresourceRange
       (VkImageSubresourceRange(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags (VkImageAspectFlags)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkImageSubresourceRange {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               baseMipLevel;
--   >     uint32_t               levelCount;
--   >     uint32_t               baseArrayLayer;
--   >     uint32_t               layerCount;
--   > } VkImageSubresourceRange;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageSubresourceRange VkImageSubresourceRange registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkImageSubresourceRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceRange, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceRange, aspectMask}

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

instance {-# OVERLAPPING #-}
         CanReadField "baseMipLevel" VkImageSubresourceRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, baseMipLevel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceRange, baseMipLevel}

instance {-# OVERLAPPING #-}
         CanWriteField "baseMipLevel" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceRange, baseMipLevel}

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

instance {-# OVERLAPPING #-}
         CanReadField "levelCount" VkImageSubresourceRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, levelCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceRange, levelCount}

instance {-# OVERLAPPING #-}
         CanWriteField "levelCount" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceRange, levelCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "baseArrayLayer" VkImageSubresourceRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, baseArrayLayer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceRange, baseArrayLayer}

instance {-# OVERLAPPING #-}
         CanWriteField "baseArrayLayer" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceRange, baseArrayLayer}

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

instance {-# OVERLAPPING #-}
         CanReadField "layerCount" VkImageSubresourceRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresourceRange, layerCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresourceRange, layerCount}

instance {-# OVERLAPPING #-}
         CanWriteField "layerCount" VkImageSubresourceRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresourceRange, layerCount}

instance Show VkImageSubresourceRange where
        showsPrec d x
          = showString "VkImageSubresourceRange {" .
              showString "aspectMask = " .
                showsPrec d (getField @"aspectMask" x) .
                  showString ", " .
                    showString "baseMipLevel = " .
                      showsPrec d (getField @"baseMipLevel" x) .
                        showString ", " .
                          showString "levelCount = " .
                            showsPrec d (getField @"levelCount" x) .
                              showString ", " .
                                showString "baseArrayLayer = " .
                                  showsPrec d (getField @"baseArrayLayer" x) .
                                    showString ", " .
                                      showString "layerCount = " .
                                        showsPrec d (getField @"layerCount" x) . showChar '}'
