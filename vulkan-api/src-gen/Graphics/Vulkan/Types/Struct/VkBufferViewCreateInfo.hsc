#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferViewCreateInfo
       (VkBufferViewCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Bitmasks             (VkBufferViewCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkFormat        (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkBuffer)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBufferViewCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBufferViewCreateFlagsflags;
--   >     VkBuffer               buffer;
--   >     VkFormat               format;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           range;
--   > } VkBufferViewCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferViewCreateInfo VkBufferViewCreateInfo registry at www.khronos.org>
data VkBufferViewCreateInfo = VkBufferViewCreateInfo## Addr##
                                                      ByteArray##

instance Eq VkBufferViewCreateInfo where
        (VkBufferViewCreateInfo## a _) == x@(VkBufferViewCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferViewCreateInfo where
        (VkBufferViewCreateInfo## a _) `compare`
          x@(VkBufferViewCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferViewCreateInfo where
        sizeOf ~_ = #{size VkBufferViewCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBufferViewCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferViewCreateInfo where
        unsafeAddr (VkBufferViewCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferViewCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferViewCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferViewCreateInfo where
        type StructFields VkBufferViewCreateInfo =
             '["sType", "pNext", "flags", "buffer", "format", "offset", "range"] -- ' closing tick for hsc2hs
        type CUnionType VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferViewCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBufferViewCreateInfo where
        type FieldType "sType" VkBufferViewCreateInfo = VkStructureType
        type FieldOptional "sType" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, sType}
        type FieldIsArray "sType" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBufferViewCreateInfo where
        type FieldType "pNext" VkBufferViewCreateInfo = Ptr Void
        type FieldOptional "pNext" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, pNext}
        type FieldIsArray "pNext" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkBufferViewCreateInfo where
        type FieldType "flags" VkBufferViewCreateInfo =
             VkBufferViewCreateFlags
        type FieldOptional "flags" VkBufferViewCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, flags}
        type FieldIsArray "flags" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBufferViewCreateInfo where
        type FieldType "buffer" VkBufferViewCreateInfo = VkBuffer
        type FieldOptional "buffer" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, buffer}
        type FieldIsArray "buffer" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, buffer}

instance {-# OVERLAPPING #-}
         HasField "format" VkBufferViewCreateInfo where
        type FieldType "format" VkBufferViewCreateInfo = VkFormat
        type FieldOptional "format" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, format}
        type FieldIsArray "format" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         HasField "offset" VkBufferViewCreateInfo where
        type FieldType "offset" VkBufferViewCreateInfo = VkDeviceSize
        type FieldOptional "offset" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, offset}
        type FieldIsArray "offset" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, offset}

instance {-# OVERLAPPING #-}
         HasField "range" VkBufferViewCreateInfo where
        type FieldType "range" VkBufferViewCreateInfo = VkDeviceSize
        type FieldOptional "range" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "range" VkBufferViewCreateInfo =
             #{offset VkBufferViewCreateInfo, range}
        type FieldIsArray "range" VkBufferViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferViewCreateInfo, range}

instance {-# OVERLAPPING #-}
         CanReadField "range" VkBufferViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, range})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferViewCreateInfo, range}

instance {-# OVERLAPPING #-}
         CanWriteField "range" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, range}

instance Show VkBufferViewCreateInfo where
        showsPrec d x
          = showString "VkBufferViewCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "buffer = " .
                                  showsPrec d (getField @"buffer" x) .
                                    showString ", " .
                                      showString "format = " .
                                        showsPrec d (getField @"format" x) .
                                          showString ", " .
                                            showString "offset = " .
                                              showsPrec d (getField @"offset" x) .
                                                showString ", " .
                                                  showString "range = " .
                                                    showsPrec d (getField @"range" x) . showChar '}'
