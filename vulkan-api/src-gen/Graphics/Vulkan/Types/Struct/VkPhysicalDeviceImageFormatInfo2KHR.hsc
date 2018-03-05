#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2KHR
       (VkPhysicalDeviceImageFormatInfo2KHR(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormat           (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags (VkImageCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkImageTiling      (VkImageTiling)
import           Graphics.Vulkan.Types.Enum.VkImageType        (VkImageType)
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags  (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceImageFormatInfo2KHR {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkFormat                         format;
--   >     VkImageType                      type;
--   >     VkImageTiling                    tiling;
--   >     VkImageUsageFlags                usage;
--   >     VkImageCreateFlags flags;
--   > } VkPhysicalDeviceImageFormatInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceImageFormatInfo2KHR.html VkPhysicalDeviceImageFormatInfo2KHR registry at www.khronos.org>
data VkPhysicalDeviceImageFormatInfo2KHR = VkPhysicalDeviceImageFormatInfo2KHR## Addr##
                                                                                ByteArray##

instance Eq VkPhysicalDeviceImageFormatInfo2KHR where
        (VkPhysicalDeviceImageFormatInfo2KHR## a _) ==
          x@(VkPhysicalDeviceImageFormatInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceImageFormatInfo2KHR where
        (VkPhysicalDeviceImageFormatInfo2KHR## a _) `compare`
          x@(VkPhysicalDeviceImageFormatInfo2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceImageFormatInfo2KHR where
        sizeOf ~_ = #{size VkPhysicalDeviceImageFormatInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceImageFormatInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceImageFormatInfo2KHR
         where
        unsafeAddr (VkPhysicalDeviceImageFormatInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceImageFormatInfo2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceImageFormatInfo2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceImageFormatInfo2KHR where
        type StructFields VkPhysicalDeviceImageFormatInfo2KHR =
             '["sType", "pNext", "format", "type", "tiling", "usage", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceImageFormatInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceImageFormatInfo2KHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceImageFormatInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "sType" VkPhysicalDeviceImageFormatInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "pNext" VkPhysicalDeviceImageFormatInfo2KHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "format" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "format" VkPhysicalDeviceImageFormatInfo2KHR =
             VkFormat
        type FieldOptional "format" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}
        type FieldIsArray "format" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}

instance {-# OVERLAPPING #-}
         HasField "type" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "type" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageType
        type FieldOptional "type" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}
        type FieldIsArray "type" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}

instance {-# OVERLAPPING #-}
         HasField "tiling" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "tiling" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageTiling
        type FieldOptional "tiling" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "tiling" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}
        type FieldIsArray "tiling" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}

instance {-# OVERLAPPING #-}
         CanReadField "tiling" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}

instance {-# OVERLAPPING #-}
         CanWriteField "tiling" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "usage" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageUsageFlags
        type FieldOptional "usage" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}
        type FieldIsArray "usage" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}

instance {-# OVERLAPPING #-}
         CanReadField "usage" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "flags" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageCreateFlags
        type FieldOptional "flags" VkPhysicalDeviceImageFormatInfo2KHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}
        type FieldIsArray "flags" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPhysicalDeviceImageFormatInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}

instance Show VkPhysicalDeviceImageFormatInfo2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceImageFormatInfo2KHR {" .
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
                                      showString "tiling = " .
                                        showsPrec d (getField @"tiling" x) .
                                          showString ", " .
                                            showString "usage = " .
                                              showsPrec d (getField @"usage" x) .
                                                showString ", " .
                                                  showString "flags = " .
                                                    showsPrec d (getField @"flags" x) . showChar '}'
