#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferCreateInfo
       (VkBufferCreateInfo(..)) where
import           Foreign.Storable                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkBufferCreateFlags (VkBufferCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkBufferUsageFlags  (VkBufferUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkSharingMode       (VkSharingMode)
import           Graphics.Vulkan.Types.Enum.VkStructureType     (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

-- | > typedef struct VkBufferCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBufferCreateFlags    flags;
--   >     VkDeviceSize           size;
--   >     VkBufferUsageFlags     usage;
--   >     VkSharingMode          sharingMode;
--   >     uint32_t               queueFamilyIndexCount;
--   >     const uint32_t*        pQueueFamilyIndices;
--   > } VkBufferCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBufferCreateInfo.html VkBufferCreateInfo registry at www.khronos.org>
data VkBufferCreateInfo = VkBufferCreateInfo## Addr## ByteArray##

instance Eq VkBufferCreateInfo where
        (VkBufferCreateInfo## a _) == x@(VkBufferCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferCreateInfo where
        (VkBufferCreateInfo## a _) `compare` x@(VkBufferCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferCreateInfo where
        sizeOf ~_ = #{size VkBufferCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBufferCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferCreateInfo where
        unsafeAddr (VkBufferCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferCreateInfo where
        type StructFields VkBufferCreateInfo =
             '["sType", "pNext", "flags", "size", "usage", "sharingMode", -- ' closing tick for hsc2hs
               "queueFamilyIndexCount", "pQueueFamilyIndices"]
        type CUnionType VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkBufferCreateInfo where
        type VkSTypeMType VkBufferCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBufferCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBufferCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBufferCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkBufferCreateInfo
         where
        type FieldType "sType" VkBufferCreateInfo = VkStructureType
        type FieldOptional "sType" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, sType}
        type FieldIsArray "sType" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, sType}

instance CanReadField "sType" VkBufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkBufferCreateInfo where
        type VkPNextMType VkBufferCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBufferCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBufferCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBufferCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkBufferCreateInfo
         where
        type FieldType "pNext" VkBufferCreateInfo = Ptr Void
        type FieldOptional "pNext" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, pNext}
        type FieldIsArray "pNext" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, pNext}

instance CanReadField "pNext" VkBufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkBufferCreateInfo where
        type VkFlagsMType VkBufferCreateInfo = VkBufferCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkBufferCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkBufferCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkBufferCreateInfo, flags}

instance {-# OVERLAPPING #-} HasField "flags" VkBufferCreateInfo
         where
        type FieldType "flags" VkBufferCreateInfo = VkBufferCreateFlags
        type FieldOptional "flags" VkBufferCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, flags}
        type FieldIsArray "flags" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, flags}

instance CanReadField "flags" VkBufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkSize VkBufferCreateInfo where
        type VkSizeMType VkBufferCreateInfo = VkDeviceSize

        {-# NOINLINE vkSize #-}
        vkSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, size})

        {-# INLINE vkSizeByteOffset #-}
        vkSizeByteOffset ~_
          = #{offset VkBufferCreateInfo, size}

        {-# INLINE readVkSize #-}
        readVkSize p
          = peekByteOff p #{offset VkBufferCreateInfo, size}

        {-# INLINE writeVkSize #-}
        writeVkSize p
          = pokeByteOff p #{offset VkBufferCreateInfo, size}

instance {-# OVERLAPPING #-} HasField "size" VkBufferCreateInfo
         where
        type FieldType "size" VkBufferCreateInfo = VkDeviceSize
        type FieldOptional "size" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, size}
        type FieldIsArray "size" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, size}

instance CanReadField "size" VkBufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkSize

        {-# INLINE readField #-}
        readField = readVkSize

instance CanWriteField "size" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSize

instance {-# OVERLAPPING #-} HasVkUsage VkBufferCreateInfo where
        type VkUsageMType VkBufferCreateInfo = VkBufferUsageFlags

        {-# NOINLINE vkUsage #-}
        vkUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, usage})

        {-# INLINE vkUsageByteOffset #-}
        vkUsageByteOffset ~_
          = #{offset VkBufferCreateInfo, usage}

        {-# INLINE readVkUsage #-}
        readVkUsage p
          = peekByteOff p #{offset VkBufferCreateInfo, usage}

        {-# INLINE writeVkUsage #-}
        writeVkUsage p
          = pokeByteOff p #{offset VkBufferCreateInfo, usage}

instance {-# OVERLAPPING #-} HasField "usage" VkBufferCreateInfo
         where
        type FieldType "usage" VkBufferCreateInfo = VkBufferUsageFlags
        type FieldOptional "usage" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, usage}
        type FieldIsArray "usage" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, usage}

instance CanReadField "usage" VkBufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkUsage

        {-# INLINE readField #-}
        readField = readVkUsage

instance CanWriteField "usage" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkUsage

instance {-# OVERLAPPING #-} HasVkSharingMode VkBufferCreateInfo
         where
        type VkSharingModeMType VkBufferCreateInfo = VkSharingMode

        {-# NOINLINE vkSharingMode #-}
        vkSharingMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, sharingMode})

        {-# INLINE vkSharingModeByteOffset #-}
        vkSharingModeByteOffset ~_
          = #{offset VkBufferCreateInfo, sharingMode}

        {-# INLINE readVkSharingMode #-}
        readVkSharingMode p
          = peekByteOff p #{offset VkBufferCreateInfo, sharingMode}

        {-# INLINE writeVkSharingMode #-}
        writeVkSharingMode p
          = pokeByteOff p #{offset VkBufferCreateInfo, sharingMode}

instance {-# OVERLAPPING #-}
         HasField "sharingMode" VkBufferCreateInfo where
        type FieldType "sharingMode" VkBufferCreateInfo = VkSharingMode
        type FieldOptional "sharingMode" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sharingMode" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, sharingMode}
        type FieldIsArray "sharingMode" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferCreateInfo, sharingMode}

instance CanReadField "sharingMode" VkBufferCreateInfo where
        {-# INLINE getField #-}
        getField = vkSharingMode

        {-# INLINE readField #-}
        readField = readVkSharingMode

instance CanWriteField "sharingMode" VkBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSharingMode

instance {-# OVERLAPPING #-}
         HasVkQueueFamilyIndexCount VkBufferCreateInfo where
        type VkQueueFamilyIndexCountMType VkBufferCreateInfo = Word32

        {-# NOINLINE vkQueueFamilyIndexCount #-}
        vkQueueFamilyIndexCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, queueFamilyIndexCount})

        {-# INLINE vkQueueFamilyIndexCountByteOffset #-}
        vkQueueFamilyIndexCountByteOffset ~_
          = #{offset VkBufferCreateInfo, queueFamilyIndexCount}

        {-# INLINE readVkQueueFamilyIndexCount #-}
        readVkQueueFamilyIndexCount p
          = peekByteOff p #{offset VkBufferCreateInfo, queueFamilyIndexCount}

        {-# INLINE writeVkQueueFamilyIndexCount #-}
        writeVkQueueFamilyIndexCount p
          = pokeByteOff p #{offset VkBufferCreateInfo, queueFamilyIndexCount}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndexCount" VkBufferCreateInfo where
        type FieldType "queueFamilyIndexCount" VkBufferCreateInfo = Word32
        type FieldOptional "queueFamilyIndexCount" VkBufferCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndexCount" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, queueFamilyIndexCount}
        type FieldIsArray "queueFamilyIndexCount" VkBufferCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferCreateInfo, queueFamilyIndexCount}

instance CanReadField "queueFamilyIndexCount" VkBufferCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkQueueFamilyIndexCount

        {-# INLINE readField #-}
        readField = readVkQueueFamilyIndexCount

instance CanWriteField "queueFamilyIndexCount" VkBufferCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkQueueFamilyIndexCount

instance {-# OVERLAPPING #-}
         HasVkPQueueFamilyIndices VkBufferCreateInfo where
        type VkPQueueFamilyIndicesMType VkBufferCreateInfo = Ptr Word32

        {-# NOINLINE vkPQueueFamilyIndices #-}
        vkPQueueFamilyIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferCreateInfo, pQueueFamilyIndices})

        {-# INLINE vkPQueueFamilyIndicesByteOffset #-}
        vkPQueueFamilyIndicesByteOffset ~_
          = #{offset VkBufferCreateInfo, pQueueFamilyIndices}

        {-# INLINE readVkPQueueFamilyIndices #-}
        readVkPQueueFamilyIndices p
          = peekByteOff p #{offset VkBufferCreateInfo, pQueueFamilyIndices}

        {-# INLINE writeVkPQueueFamilyIndices #-}
        writeVkPQueueFamilyIndices p
          = pokeByteOff p #{offset VkBufferCreateInfo, pQueueFamilyIndices}

instance {-# OVERLAPPING #-}
         HasField "pQueueFamilyIndices" VkBufferCreateInfo where
        type FieldType "pQueueFamilyIndices" VkBufferCreateInfo =
             Ptr Word32
        type FieldOptional "pQueueFamilyIndices" VkBufferCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pQueueFamilyIndices" VkBufferCreateInfo =
             #{offset VkBufferCreateInfo, pQueueFamilyIndices}
        type FieldIsArray "pQueueFamilyIndices" VkBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferCreateInfo, pQueueFamilyIndices}

instance CanReadField "pQueueFamilyIndices" VkBufferCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPQueueFamilyIndices

        {-# INLINE readField #-}
        readField = readVkPQueueFamilyIndices

instance CanWriteField "pQueueFamilyIndices" VkBufferCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPQueueFamilyIndices

instance Show VkBufferCreateInfo where
        showsPrec d x
          = showString "VkBufferCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkSize = " .
                                  showsPrec d (vkSize x) .
                                    showString ", " .
                                      showString "vkUsage = " .
                                        showsPrec d (vkUsage x) .
                                          showString ", " .
                                            showString "vkSharingMode = " .
                                              showsPrec d (vkSharingMode x) .
                                                showString ", " .
                                                  showString "vkQueueFamilyIndexCount = " .
                                                    showsPrec d (vkQueueFamilyIndexCount x) .
                                                      showString ", " .
                                                        showString "vkPQueueFamilyIndices = " .
                                                          showsPrec d (vkPQueueFamilyIndices x) .
                                                            showChar '}'
