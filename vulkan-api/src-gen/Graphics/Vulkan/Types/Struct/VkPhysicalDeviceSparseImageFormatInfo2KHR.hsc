#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2KHR
       (VkPhysicalDeviceSparseImageFormatInfo2KHR(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormat           (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageTiling      (VkImageTiling)
import           Graphics.Vulkan.Types.Enum.VkImageType        (VkImageType)
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags  (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSparseImageFormatInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkFormat                         format;
--   >     VkImageType                      type;
--   >     VkSampleCountFlagBits            samples;
--   >     VkImageUsageFlags                usage;
--   >     VkImageTiling                    tiling;
--   > } VkPhysicalDeviceSparseImageFormatInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceSparseImageFormatInfo2KHR.html VkPhysicalDeviceSparseImageFormatInfo2KHR registry at www.khronos.org>
data VkPhysicalDeviceSparseImageFormatInfo2KHR = VkPhysicalDeviceSparseImageFormatInfo2KHR## Addr##
                                                                                            ByteArray##

instance Eq VkPhysicalDeviceSparseImageFormatInfo2KHR where
        (VkPhysicalDeviceSparseImageFormatInfo2KHR## a _) ==
          x@(VkPhysicalDeviceSparseImageFormatInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSparseImageFormatInfo2KHR where
        (VkPhysicalDeviceSparseImageFormatInfo2KHR## a _) `compare`
          x@(VkPhysicalDeviceSparseImageFormatInfo2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSparseImageFormatInfo2KHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceSparseImageFormatInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSparseImageFormatInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        unsafeAddr (VkPhysicalDeviceSparseImageFormatInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceSparseImageFormatInfo2KHR## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceSparseImageFormatInfo2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        type StructFields VkPhysicalDeviceSparseImageFormatInfo2KHR =
             '["sType", "pNext", "format", "type", "samples", "usage", "tiling"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceSparseImageFormatInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceSparseImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceSparseImageFormatInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "format" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "format" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkFormat
        type FieldOptional "format"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}
        type FieldIsArray "format"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}

instance {-# OVERLAPPING #-}
         HasField "type" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "type" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageType
        type FieldOptional "type" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}
        type FieldIsArray "type" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}

instance {-# OVERLAPPING #-}
         HasField "samples" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "samples" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = VkSampleCountFlagBits
        type FieldOptional "samples"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "samples"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}
        type FieldIsArray "samples"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}

instance {-# OVERLAPPING #-}
         CanReadField "samples" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}

instance {-# OVERLAPPING #-}
         CanWriteField "samples" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageUsageFlags
        type FieldOptional "usage"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}
        type FieldIsArray "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}

instance {-# OVERLAPPING #-}
         CanReadField "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}

instance {-# OVERLAPPING #-}
         HasField "tiling" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "tiling" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageTiling
        type FieldOptional "tiling"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tiling" VkPhysicalDeviceSparseImageFormatInfo2KHR
             =
             #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}
        type FieldIsArray "tiling"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}

instance {-# OVERLAPPING #-}
         CanReadField "tiling" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}

instance {-# OVERLAPPING #-}
         CanWriteField "tiling" VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}

instance Show VkPhysicalDeviceSparseImageFormatInfo2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceSparseImageFormatInfo2KHR {" .
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
