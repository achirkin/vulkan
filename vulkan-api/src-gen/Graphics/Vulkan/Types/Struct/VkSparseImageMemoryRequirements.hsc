#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements
       (VkSparseImageMemoryRequirements(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                            (VkDeviceSize)
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties (VkSparseImageFormatProperties)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageMemoryRequirements {
--   >     VkSparseImageFormatProperties formatProperties;
--   >     uint32_t               imageMipTailFirstLod;
--   >     VkDeviceSize           imageMipTailSize;
--   >     VkDeviceSize           imageMipTailOffset;
--   >     VkDeviceSize           imageMipTailStride;
--   > } VkSparseImageMemoryRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSparseImageMemoryRequirements.html VkSparseImageMemoryRequirements registry at www.khronos.org>
data VkSparseImageMemoryRequirements = VkSparseImageMemoryRequirements## Addr##
                                                                        ByteArray##

instance Eq VkSparseImageMemoryRequirements where
        (VkSparseImageMemoryRequirements## a _) ==
          x@(VkSparseImageMemoryRequirements## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageMemoryRequirements where
        (VkSparseImageMemoryRequirements## a _) `compare`
          x@(VkSparseImageMemoryRequirements## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageMemoryRequirements where
        sizeOf ~_ = #{size VkSparseImageMemoryRequirements}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageMemoryRequirements}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageMemoryRequirements where
        unsafeAddr (VkSparseImageMemoryRequirements## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageMemoryRequirements## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageMemoryRequirements##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageMemoryRequirements where
        type StructFields VkSparseImageMemoryRequirements =
             '["formatProperties", "imageMipTailFirstLod", "imageMipTailSize", -- ' closing tick for hsc2hs
               "imageMipTailOffset", "imageMipTailStride"]
        type CUnionType VkSparseImageMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageMemoryRequirements = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageMemoryRequirements = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkFormatProperties VkSparseImageMemoryRequirements where
        type VkFormatPropertiesMType VkSparseImageMemoryRequirements =
             VkSparseImageFormatProperties

        {-# NOINLINE vkFormatProperties #-}
        vkFormatProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, formatProperties})

        {-# INLINE vkFormatPropertiesByteOffset #-}
        vkFormatPropertiesByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements, formatProperties}

        {-# INLINE readVkFormatProperties #-}
        readVkFormatProperties p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, formatProperties}

        {-# INLINE writeVkFormatProperties #-}
        writeVkFormatProperties p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, formatProperties}

instance {-# OVERLAPPING #-}
         HasField "formatProperties" VkSparseImageMemoryRequirements where
        type FieldType "formatProperties" VkSparseImageMemoryRequirements =
             VkSparseImageFormatProperties
        type FieldOptional "formatProperties"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "formatProperties" VkSparseImageMemoryRequirements
             =
             #{offset VkSparseImageMemoryRequirements, formatProperties}
        type FieldIsArray "formatProperties"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements, formatProperties}

instance CanReadField "formatProperties"
           VkSparseImageMemoryRequirements
         where
        {-# INLINE getField #-}
        getField = vkFormatProperties

        {-# INLINE readField #-}
        readField = readVkFormatProperties

instance {-# OVERLAPPING #-}
         HasVkImageMipTailFirstLod VkSparseImageMemoryRequirements where
        type VkImageMipTailFirstLodMType VkSparseImageMemoryRequirements =
             Word32

        {-# NOINLINE vkImageMipTailFirstLod #-}
        vkImageMipTailFirstLod x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod})

        {-# INLINE vkImageMipTailFirstLodByteOffset #-}
        vkImageMipTailFirstLodByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}

        {-# INLINE readVkImageMipTailFirstLod #-}
        readVkImageMipTailFirstLod p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}

        {-# INLINE writeVkImageMipTailFirstLod #-}
        writeVkImageMipTailFirstLod p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}

instance {-# OVERLAPPING #-}
         HasField "imageMipTailFirstLod" VkSparseImageMemoryRequirements
         where
        type FieldType "imageMipTailFirstLod"
               VkSparseImageMemoryRequirements
             = Word32
        type FieldOptional "imageMipTailFirstLod"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageMipTailFirstLod"
               VkSparseImageMemoryRequirements
             =
             #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}
        type FieldIsArray "imageMipTailFirstLod"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}

instance CanReadField "imageMipTailFirstLod"
           VkSparseImageMemoryRequirements
         where
        {-# INLINE getField #-}
        getField = vkImageMipTailFirstLod

        {-# INLINE readField #-}
        readField = readVkImageMipTailFirstLod

instance {-# OVERLAPPING #-}
         HasVkImageMipTailSize VkSparseImageMemoryRequirements where
        type VkImageMipTailSizeMType VkSparseImageMemoryRequirements =
             VkDeviceSize

        {-# NOINLINE vkImageMipTailSize #-}
        vkImageMipTailSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailSize})

        {-# INLINE vkImageMipTailSizeByteOffset #-}
        vkImageMipTailSizeByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements, imageMipTailSize}

        {-# INLINE readVkImageMipTailSize #-}
        readVkImageMipTailSize p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailSize}

        {-# INLINE writeVkImageMipTailSize #-}
        writeVkImageMipTailSize p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailSize}

instance {-# OVERLAPPING #-}
         HasField "imageMipTailSize" VkSparseImageMemoryRequirements where
        type FieldType "imageMipTailSize" VkSparseImageMemoryRequirements =
             VkDeviceSize
        type FieldOptional "imageMipTailSize"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageMipTailSize" VkSparseImageMemoryRequirements
             =
             #{offset VkSparseImageMemoryRequirements, imageMipTailSize}
        type FieldIsArray "imageMipTailSize"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements, imageMipTailSize}

instance CanReadField "imageMipTailSize"
           VkSparseImageMemoryRequirements
         where
        {-# INLINE getField #-}
        getField = vkImageMipTailSize

        {-# INLINE readField #-}
        readField = readVkImageMipTailSize

instance {-# OVERLAPPING #-}
         HasVkImageMipTailOffset VkSparseImageMemoryRequirements where
        type VkImageMipTailOffsetMType VkSparseImageMemoryRequirements =
             VkDeviceSize

        {-# NOINLINE vkImageMipTailOffset #-}
        vkImageMipTailOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailOffset})

        {-# INLINE vkImageMipTailOffsetByteOffset #-}
        vkImageMipTailOffsetByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}

        {-# INLINE readVkImageMipTailOffset #-}
        readVkImageMipTailOffset p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}

        {-# INLINE writeVkImageMipTailOffset #-}
        writeVkImageMipTailOffset p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}

instance {-# OVERLAPPING #-}
         HasField "imageMipTailOffset" VkSparseImageMemoryRequirements where
        type FieldType "imageMipTailOffset" VkSparseImageMemoryRequirements
             = VkDeviceSize
        type FieldOptional "imageMipTailOffset"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageMipTailOffset"
               VkSparseImageMemoryRequirements
             =
             #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}
        type FieldIsArray "imageMipTailOffset"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}

instance CanReadField "imageMipTailOffset"
           VkSparseImageMemoryRequirements
         where
        {-# INLINE getField #-}
        getField = vkImageMipTailOffset

        {-# INLINE readField #-}
        readField = readVkImageMipTailOffset

instance {-# OVERLAPPING #-}
         HasVkImageMipTailStride VkSparseImageMemoryRequirements where
        type VkImageMipTailStrideMType VkSparseImageMemoryRequirements =
             VkDeviceSize

        {-# NOINLINE vkImageMipTailStride #-}
        vkImageMipTailStride x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailStride})

        {-# INLINE vkImageMipTailStrideByteOffset #-}
        vkImageMipTailStrideByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements, imageMipTailStride}

        {-# INLINE readVkImageMipTailStride #-}
        readVkImageMipTailStride p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailStride}

        {-# INLINE writeVkImageMipTailStride #-}
        writeVkImageMipTailStride p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailStride}

instance {-# OVERLAPPING #-}
         HasField "imageMipTailStride" VkSparseImageMemoryRequirements where
        type FieldType "imageMipTailStride" VkSparseImageMemoryRequirements
             = VkDeviceSize
        type FieldOptional "imageMipTailStride"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageMipTailStride"
               VkSparseImageMemoryRequirements
             =
             #{offset VkSparseImageMemoryRequirements, imageMipTailStride}
        type FieldIsArray "imageMipTailStride"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements, imageMipTailStride}

instance CanReadField "imageMipTailStride"
           VkSparseImageMemoryRequirements
         where
        {-# INLINE getField #-}
        getField = vkImageMipTailStride

        {-# INLINE readField #-}
        readField = readVkImageMipTailStride

instance Show VkSparseImageMemoryRequirements where
        showsPrec d x
          = showString "VkSparseImageMemoryRequirements {" .
              showString "vkFormatProperties = " .
                showsPrec d (vkFormatProperties x) .
                  showString ", " .
                    showString "vkImageMipTailFirstLod = " .
                      showsPrec d (vkImageMipTailFirstLod x) .
                        showString ", " .
                          showString "vkImageMipTailSize = " .
                            showsPrec d (vkImageMipTailSize x) .
                              showString ", " .
                                showString "vkImageMipTailOffset = " .
                                  showsPrec d (vkImageMipTailOffset x) .
                                    showString ", " .
                                      showString "vkImageMipTailStride = " .
                                        showsPrec d (vkImageMipTailStride x) . showChar '}'
