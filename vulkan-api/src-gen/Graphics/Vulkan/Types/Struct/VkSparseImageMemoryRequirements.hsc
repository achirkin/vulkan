#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements
       (VkSparseImageMemoryRequirements(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Base                                                   (Addr##,
                                                                             ByteArray##,
                                                                             byteArrayContents##,
                                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                            (VkDeviceSize)
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties (VkSparseImageFormatProperties)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageMemoryRequirements {
--   >     VkSparseImageFormatProperties formatProperties;
--   >     uint32_t               imageMipTailFirstLod;
--   >     VkDeviceSize           imageMipTailSize;
--   >     VkDeviceSize           imageMipTailOffset;
--   >     VkDeviceSize           imageMipTailStride;
--   > } VkSparseImageMemoryRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageMemoryRequirements VkSparseImageMemoryRequirements registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "formatProperties" VkSparseImageMemoryRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, formatProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, formatProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "formatProperties" VkSparseImageMemoryRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, formatProperties}

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

instance {-# OVERLAPPING #-}
         CanReadField "imageMipTailFirstLod" VkSparseImageMemoryRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}

instance {-# OVERLAPPING #-}
         CanWriteField "imageMipTailFirstLod"
           VkSparseImageMemoryRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}

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

instance {-# OVERLAPPING #-}
         CanReadField "imageMipTailSize" VkSparseImageMemoryRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailSize}

instance {-# OVERLAPPING #-}
         CanWriteField "imageMipTailSize" VkSparseImageMemoryRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailSize}

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

instance {-# OVERLAPPING #-}
         CanReadField "imageMipTailOffset" VkSparseImageMemoryRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "imageMipTailOffset" VkSparseImageMemoryRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "imageMipTailStride" VkSparseImageMemoryRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailStride})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailStride}

instance {-# OVERLAPPING #-}
         CanWriteField "imageMipTailStride" VkSparseImageMemoryRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailStride}

instance Show VkSparseImageMemoryRequirements where
        showsPrec d x
          = showString "VkSparseImageMemoryRequirements {" .
              showString "formatProperties = " .
                showsPrec d (getField @"formatProperties" x) .
                  showString ", " .
                    showString "imageMipTailFirstLod = " .
                      showsPrec d (getField @"imageMipTailFirstLod" x) .
                        showString ", " .
                          showString "imageMipTailSize = " .
                            showsPrec d (getField @"imageMipTailSize" x) .
                              showString ", " .
                                showString "imageMipTailOffset = " .
                                  showsPrec d (getField @"imageMipTailOffset" x) .
                                    showString ", " .
                                      showString "imageMipTailStride = " .
                                        showsPrec d (getField @"imageMipTailStride" x) .
                                          showChar '}'
