#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageFormatProperties
       (VkImageFormatProperties(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags (VkSampleCountFlags)
import           Graphics.Vulkan.Types.Struct.VkExtent3D       (VkExtent3D)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkImageFormatProperties {
--   >     VkExtent3D             maxExtent;
--   >     uint32_t               maxMipLevels;
--   >     uint32_t               maxArrayLayers;
--   >     VkSampleCountFlags     sampleCounts;
--   >     VkDeviceSize           maxResourceSize;
--   > } VkImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageFormatProperties.html VkImageFormatProperties registry at www.khronos.org>
data VkImageFormatProperties = VkImageFormatProperties## Addr##
                                                        ByteArray##

instance Eq VkImageFormatProperties where
        (VkImageFormatProperties## a _) == x@(VkImageFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageFormatProperties where
        (VkImageFormatProperties## a _) `compare`
          x@(VkImageFormatProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageFormatProperties where
        sizeOf ~_ = #{size VkImageFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageFormatProperties where
        unsafeAddr (VkImageFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageFormatProperties## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageFormatProperties where
        type StructFields VkImageFormatProperties =
             '["maxExtent", "maxMipLevels", "maxArrayLayers", "sampleCounts", -- ' closing tick for hsc2hs
               "maxResourceSize"]
        type CUnionType VkImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkImageFormatProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "maxExtent" VkImageFormatProperties where
        type FieldType "maxExtent" VkImageFormatProperties = VkExtent3D
        type FieldOptional "maxExtent" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxExtent" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxExtent}
        type FieldIsArray "maxExtent" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxExtent}

instance {-# OVERLAPPING #-}
         CanReadField "maxExtent" VkImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties, maxExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "maxExtent" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties, maxExtent}

instance {-# OVERLAPPING #-}
         HasField "maxMipLevels" VkImageFormatProperties where
        type FieldType "maxMipLevels" VkImageFormatProperties = Word32
        type FieldOptional "maxMipLevels" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMipLevels" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxMipLevels}
        type FieldIsArray "maxMipLevels" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxMipLevels}

instance {-# OVERLAPPING #-}
         CanReadField "maxMipLevels" VkImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxMipLevels})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties, maxMipLevels}

instance {-# OVERLAPPING #-}
         CanWriteField "maxMipLevels" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties, maxMipLevels}

instance {-# OVERLAPPING #-}
         HasField "maxArrayLayers" VkImageFormatProperties where
        type FieldType "maxArrayLayers" VkImageFormatProperties = Word32
        type FieldOptional "maxArrayLayers" VkImageFormatProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxArrayLayers" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxArrayLayers}
        type FieldIsArray "maxArrayLayers" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxArrayLayers}

instance {-# OVERLAPPING #-}
         CanReadField "maxArrayLayers" VkImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxArrayLayers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties, maxArrayLayers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxArrayLayers" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties, maxArrayLayers}

instance {-# OVERLAPPING #-}
         HasField "sampleCounts" VkImageFormatProperties where
        type FieldType "sampleCounts" VkImageFormatProperties =
             VkSampleCountFlags
        type FieldOptional "sampleCounts" VkImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sampleCounts" VkImageFormatProperties =
             #{offset VkImageFormatProperties, sampleCounts}
        type FieldIsArray "sampleCounts" VkImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, sampleCounts}

instance {-# OVERLAPPING #-}
         CanReadField "sampleCounts" VkImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, sampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties, sampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleCounts" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties, sampleCounts}

instance {-# OVERLAPPING #-}
         HasField "maxResourceSize" VkImageFormatProperties where
        type FieldType "maxResourceSize" VkImageFormatProperties =
             VkDeviceSize
        type FieldOptional "maxResourceSize" VkImageFormatProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxResourceSize" VkImageFormatProperties =
             #{offset VkImageFormatProperties, maxResourceSize}
        type FieldIsArray "maxResourceSize" VkImageFormatProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties, maxResourceSize}

instance {-# OVERLAPPING #-}
         CanReadField "maxResourceSize" VkImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties, maxResourceSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties, maxResourceSize}

instance {-# OVERLAPPING #-}
         CanWriteField "maxResourceSize" VkImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties, maxResourceSize}

instance Show VkImageFormatProperties where
        showsPrec d x
          = showString "VkImageFormatProperties {" .
              showString "maxExtent = " .
                showsPrec d (getField @"maxExtent" x) .
                  showString ", " .
                    showString "maxMipLevels = " .
                      showsPrec d (getField @"maxMipLevels" x) .
                        showString ", " .
                          showString "maxArrayLayers = " .
                            showsPrec d (getField @"maxArrayLayers" x) .
                              showString ", " .
                                showString "sampleCounts = " .
                                  showsPrec d (getField @"sampleCounts" x) .
                                    showString ", " .
                                      showString "maxResourceSize = " .
                                        showsPrec d (getField @"maxResourceSize" x) . showChar '}'
