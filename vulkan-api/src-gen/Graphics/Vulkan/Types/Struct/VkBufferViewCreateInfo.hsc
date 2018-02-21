#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferViewCreateInfo
       (VkBufferViewCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Bitmasks             (VkBufferViewCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkFormat        (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkBuffer)
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBufferViewCreateInfo.html VkBufferViewCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkBufferViewCreateInfo
         where
        type VkSTypeMType VkBufferViewCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBufferViewCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBufferViewCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, sType}

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

instance CanReadField "sType" VkBufferViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkBufferViewCreateInfo
         where
        type VkPNextMType VkBufferViewCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBufferViewCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBufferViewCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, pNext}

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

instance CanReadField "pNext" VkBufferViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkBufferViewCreateInfo
         where
        type VkFlagsMType VkBufferViewCreateInfo = VkBufferViewCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkBufferViewCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkBufferViewCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, flags}

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

instance CanReadField "flags" VkBufferViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkBuffer VkBufferViewCreateInfo
         where
        type VkBufferMType VkBufferViewCreateInfo = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkBufferViewCreateInfo, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkBufferViewCreateInfo, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, buffer}

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

instance CanReadField "buffer" VkBufferViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance {-# OVERLAPPING #-} HasVkFormat VkBufferViewCreateInfo
         where
        type VkFormatMType VkBufferViewCreateInfo = VkFormat

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkBufferViewCreateInfo, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkBufferViewCreateInfo, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, format}

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

instance CanReadField "format" VkBufferViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-} HasVkOffset VkBufferViewCreateInfo
         where
        type VkOffsetMType VkBufferViewCreateInfo = VkDeviceSize

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkBufferViewCreateInfo, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkBufferViewCreateInfo, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, offset}

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

instance CanReadField "offset" VkBufferViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-} HasVkRange VkBufferViewCreateInfo
         where
        type VkRangeMType VkBufferViewCreateInfo = VkDeviceSize

        {-# NOINLINE vkRange #-}
        vkRange x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferViewCreateInfo, range})

        {-# INLINE vkRangeByteOffset #-}
        vkRangeByteOffset ~_
          = #{offset VkBufferViewCreateInfo, range}

        {-# INLINE readVkRange #-}
        readVkRange p
          = peekByteOff p #{offset VkBufferViewCreateInfo, range}

        {-# INLINE writeVkRange #-}
        writeVkRange p
          = pokeByteOff p #{offset VkBufferViewCreateInfo, range}

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

instance CanReadField "range" VkBufferViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkRange

        {-# INLINE readField #-}
        readField = readVkRange

instance CanWriteField "range" VkBufferViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkRange

instance Show VkBufferViewCreateInfo where
        showsPrec d x
          = showString "VkBufferViewCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkBuffer = " .
                                  showsPrec d (vkBuffer x) .
                                    showString ", " .
                                      showString "vkFormat = " .
                                        showsPrec d (vkFormat x) .
                                          showString ", " .
                                            showString "vkOffset = " .
                                              showsPrec d (vkOffset x) .
                                                showString ", " .
                                                  showString "vkRange = " .
                                                    showsPrec d (vkRange x) . showChar '}'
