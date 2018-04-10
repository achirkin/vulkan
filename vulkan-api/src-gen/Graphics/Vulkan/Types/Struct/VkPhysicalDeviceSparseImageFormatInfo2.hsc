#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2
       (VkPhysicalDeviceSparseImageFormatInfo2(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormat           (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageTiling      (VkImageTiling)
import           Graphics.Vulkan.Types.Enum.VkImageType        (VkImageType)
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags  (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSparseImageFormatInfo2 {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkFormat                         format;
--   >     VkImageType                      type;
--   >     VkSampleCountFlagBits            samples;
--   >     VkImageUsageFlags                usage;
--   >     VkImageTiling                    tiling;
--   > } VkPhysicalDeviceSparseImageFormatInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDeviceSparseImageFormatInfo2 VkPhysicalDeviceSparseImageFormatInfo2 registry at www.khronos.org>
data VkPhysicalDeviceSparseImageFormatInfo2 = VkPhysicalDeviceSparseImageFormatInfo2## Addr##
                                                                                      ByteArray##

instance Eq VkPhysicalDeviceSparseImageFormatInfo2 where
        (VkPhysicalDeviceSparseImageFormatInfo2## a _) ==
          x@(VkPhysicalDeviceSparseImageFormatInfo2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSparseImageFormatInfo2 where
        (VkPhysicalDeviceSparseImageFormatInfo2## a _) `compare`
          x@(VkPhysicalDeviceSparseImageFormatInfo2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSparseImageFormatInfo2 where
        sizeOf ~_
          = #{size VkPhysicalDeviceSparseImageFormatInfo2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSparseImageFormatInfo2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceSparseImageFormatInfo2
         where
        unsafeAddr (VkPhysicalDeviceSparseImageFormatInfo2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceSparseImageFormatInfo2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceSparseImageFormatInfo2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceSparseImageFormatInfo2 where
        type StructFields VkPhysicalDeviceSparseImageFormatInfo2 =
             '["sType", "pNext", "format", "type", "samples", "usage", "tiling"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceSparseImageFormatInfo2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceSparseImageFormatInfo2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceSparseImageFormatInfo2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceSparseImageFormatInfo2 where
        type FieldType "sType" VkPhysicalDeviceSparseImageFormatInfo2 =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceSparseImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceSparseImageFormatInfo2 =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2, sType}
        type FieldIsArray "sType" VkPhysicalDeviceSparseImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceSparseImageFormatInfo2 where
        type FieldType "pNext" VkPhysicalDeviceSparseImageFormatInfo2 =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceSparseImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceSparseImageFormatInfo2 =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceSparseImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, pNext}

instance {-# OVERLAPPING #-}
         HasField "format" VkPhysicalDeviceSparseImageFormatInfo2 where
        type FieldType "format" VkPhysicalDeviceSparseImageFormatInfo2 =
             VkFormat
        type FieldOptional "format" VkPhysicalDeviceSparseImageFormatInfo2
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkPhysicalDeviceSparseImageFormatInfo2 =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2, format}
        type FieldIsArray "format" VkPhysicalDeviceSparseImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, format}

instance {-# OVERLAPPING #-}
         HasField "type" VkPhysicalDeviceSparseImageFormatInfo2 where
        type FieldType "type" VkPhysicalDeviceSparseImageFormatInfo2 =
             VkImageType
        type FieldOptional "type" VkPhysicalDeviceSparseImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkPhysicalDeviceSparseImageFormatInfo2 =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2, type}
        type FieldIsArray "type" VkPhysicalDeviceSparseImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, type}

instance {-# OVERLAPPING #-}
         HasField "samples" VkPhysicalDeviceSparseImageFormatInfo2 where
        type FieldType "samples" VkPhysicalDeviceSparseImageFormatInfo2 =
             VkSampleCountFlagBits
        type FieldOptional "samples" VkPhysicalDeviceSparseImageFormatInfo2
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "samples" VkPhysicalDeviceSparseImageFormatInfo2 =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2, samples}
        type FieldIsArray "samples" VkPhysicalDeviceSparseImageFormatInfo2
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2, samples}

instance {-# OVERLAPPING #-}
         CanReadField "samples" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2, samples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, samples}

instance {-# OVERLAPPING #-}
         CanWriteField "samples" VkPhysicalDeviceSparseImageFormatInfo2
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, samples}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceSparseImageFormatInfo2 where
        type FieldType "usage" VkPhysicalDeviceSparseImageFormatInfo2 =
             VkImageUsageFlags
        type FieldOptional "usage" VkPhysicalDeviceSparseImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkPhysicalDeviceSparseImageFormatInfo2 =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2, usage}
        type FieldIsArray "usage" VkPhysicalDeviceSparseImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2, usage}

instance {-# OVERLAPPING #-}
         CanReadField "usage" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, usage}

instance {-# OVERLAPPING #-}
         HasField "tiling" VkPhysicalDeviceSparseImageFormatInfo2 where
        type FieldType "tiling" VkPhysicalDeviceSparseImageFormatInfo2 =
             VkImageTiling
        type FieldOptional "tiling" VkPhysicalDeviceSparseImageFormatInfo2
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tiling" VkPhysicalDeviceSparseImageFormatInfo2 =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2, tiling}
        type FieldIsArray "tiling" VkPhysicalDeviceSparseImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2, tiling}

instance {-# OVERLAPPING #-}
         CanReadField "tiling" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2, tiling})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, tiling}

instance {-# OVERLAPPING #-}
         CanWriteField "tiling" VkPhysicalDeviceSparseImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2, tiling}

instance Show VkPhysicalDeviceSparseImageFormatInfo2 where
        showsPrec d x
          = showString "VkPhysicalDeviceSparseImageFormatInfo2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "format = " .
                            showsPrec d (getField @"format" x) .
                              showString ", " .
                                showString "type = " .
                                  showsPrec d (getField @"type" x) .
                                    showString ", " .
                                      showString "samples = " .
                                        showsPrec d (getField @"samples" x) .
                                          showString ", " .
                                            showString "usage = " .
                                              showsPrec d (getField @"usage" x) .
                                                showString ", " .
                                                  showString "tiling = " .
                                                    showsPrec d (getField @"tiling" x) .
                                                      showChar '}'
