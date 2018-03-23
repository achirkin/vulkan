#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2
       (VkPhysicalDeviceImageFormatInfo2(..)) where
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

-- | > typedef struct VkPhysicalDeviceImageFormatInfo2 {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkFormat                         format;
--   >     VkImageType                      type;
--   >     VkImageTiling                    tiling;
--   >     VkImageUsageFlags                usage;
--   >     VkImageCreateFlags flags;
--   > } VkPhysicalDeviceImageFormatInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceImageFormatInfo2.html VkPhysicalDeviceImageFormatInfo2 registry at www.khronos.org>
data VkPhysicalDeviceImageFormatInfo2 = VkPhysicalDeviceImageFormatInfo2## Addr##
                                                                          ByteArray##

instance Eq VkPhysicalDeviceImageFormatInfo2 where
        (VkPhysicalDeviceImageFormatInfo2## a _) ==
          x@(VkPhysicalDeviceImageFormatInfo2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceImageFormatInfo2 where
        (VkPhysicalDeviceImageFormatInfo2## a _) `compare`
          x@(VkPhysicalDeviceImageFormatInfo2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceImageFormatInfo2 where
        sizeOf ~_ = #{size VkPhysicalDeviceImageFormatInfo2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceImageFormatInfo2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceImageFormatInfo2 where
        unsafeAddr (VkPhysicalDeviceImageFormatInfo2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceImageFormatInfo2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceImageFormatInfo2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceImageFormatInfo2 where
        type StructFields VkPhysicalDeviceImageFormatInfo2 =
             '["sType", "pNext", "format", "type", "tiling", "usage", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceImageFormatInfo2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceImageFormatInfo2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceImageFormatInfo2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceImageFormatInfo2 where
        type FieldType "sType" VkPhysicalDeviceImageFormatInfo2 =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceImageFormatInfo2 =
             #{offset VkPhysicalDeviceImageFormatInfo2, sType}
        type FieldIsArray "sType" VkPhysicalDeviceImageFormatInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceImageFormatInfo2 where
        type FieldType "pNext" VkPhysicalDeviceImageFormatInfo2 = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceImageFormatInfo2 =
             #{offset VkPhysicalDeviceImageFormatInfo2, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceImageFormatInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, pNext}

instance {-# OVERLAPPING #-}
         HasField "format" VkPhysicalDeviceImageFormatInfo2 where
        type FieldType "format" VkPhysicalDeviceImageFormatInfo2 = VkFormat
        type FieldOptional "format" VkPhysicalDeviceImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkPhysicalDeviceImageFormatInfo2 =
             #{offset VkPhysicalDeviceImageFormatInfo2, format}
        type FieldIsArray "format" VkPhysicalDeviceImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkPhysicalDeviceImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkPhysicalDeviceImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, format}

instance {-# OVERLAPPING #-}
         HasField "type" VkPhysicalDeviceImageFormatInfo2 where
        type FieldType "type" VkPhysicalDeviceImageFormatInfo2 =
             VkImageType
        type FieldOptional "type" VkPhysicalDeviceImageFormatInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkPhysicalDeviceImageFormatInfo2 =
             #{offset VkPhysicalDeviceImageFormatInfo2, type}
        type FieldIsArray "type" VkPhysicalDeviceImageFormatInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkPhysicalDeviceImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkPhysicalDeviceImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, type}

instance {-# OVERLAPPING #-}
         HasField "tiling" VkPhysicalDeviceImageFormatInfo2 where
        type FieldType "tiling" VkPhysicalDeviceImageFormatInfo2 =
             VkImageTiling
        type FieldOptional "tiling" VkPhysicalDeviceImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "tiling" VkPhysicalDeviceImageFormatInfo2 =
             #{offset VkPhysicalDeviceImageFormatInfo2, tiling}
        type FieldIsArray "tiling" VkPhysicalDeviceImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2, tiling}

instance {-# OVERLAPPING #-}
         CanReadField "tiling" VkPhysicalDeviceImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2, tiling})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, tiling}

instance {-# OVERLAPPING #-}
         CanWriteField "tiling" VkPhysicalDeviceImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, tiling}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceImageFormatInfo2 where
        type FieldType "usage" VkPhysicalDeviceImageFormatInfo2 =
             VkImageUsageFlags
        type FieldOptional "usage" VkPhysicalDeviceImageFormatInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkPhysicalDeviceImageFormatInfo2 =
             #{offset VkPhysicalDeviceImageFormatInfo2, usage}
        type FieldIsArray "usage" VkPhysicalDeviceImageFormatInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2, usage}

instance {-# OVERLAPPING #-}
         CanReadField "usage" VkPhysicalDeviceImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkPhysicalDeviceImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, usage}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPhysicalDeviceImageFormatInfo2 where
        type FieldType "flags" VkPhysicalDeviceImageFormatInfo2 =
             VkImageCreateFlags
        type FieldOptional "flags" VkPhysicalDeviceImageFormatInfo2 = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPhysicalDeviceImageFormatInfo2 =
             #{offset VkPhysicalDeviceImageFormatInfo2, flags}
        type FieldIsArray "flags" VkPhysicalDeviceImageFormatInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPhysicalDeviceImageFormatInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPhysicalDeviceImageFormatInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2, flags}

instance Show VkPhysicalDeviceImageFormatInfo2 where
        showsPrec d x
          = showString "VkPhysicalDeviceImageFormatInfo2 {" .
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
