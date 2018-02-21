#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo
       (VkMemoryAllocateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDeviceSize           allocationSize;
--   >     uint32_t               memoryTypeIndex;
--   > } VkMemoryAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryAllocateInfo.html VkMemoryAllocateInfo registry at www.khronos.org>
data VkMemoryAllocateInfo = VkMemoryAllocateInfo## Addr## ByteArray##

instance Eq VkMemoryAllocateInfo where
        (VkMemoryAllocateInfo## a _) == x@(VkMemoryAllocateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryAllocateInfo where
        (VkMemoryAllocateInfo## a _) `compare` x@(VkMemoryAllocateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryAllocateInfo where
        sizeOf ~_ = #{size VkMemoryAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryAllocateInfo where
        unsafeAddr (VkMemoryAllocateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryAllocateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryAllocateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryAllocateInfo where
        type StructFields VkMemoryAllocateInfo =
             '["sType", "pNext", "allocationSize", "memoryTypeIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryAllocateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkMemoryAllocateInfo where
        type VkSTypeMType VkMemoryAllocateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryAllocateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryAllocateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkMemoryAllocateInfo
         where
        type FieldType "sType" VkMemoryAllocateInfo = VkStructureType
        type FieldOptional "sType" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, sType}
        type FieldIsArray "sType" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryAllocateInfo, sType}

instance CanReadField "sType" VkMemoryAllocateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMemoryAllocateInfo where
        type VkPNextMType VkMemoryAllocateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryAllocateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryAllocateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkMemoryAllocateInfo
         where
        type FieldType "pNext" VkMemoryAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, pNext}
        type FieldIsArray "pNext" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryAllocateInfo, pNext}

instance CanReadField "pNext" VkMemoryAllocateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkAllocationSize VkMemoryAllocateInfo where
        type VkAllocationSizeMType VkMemoryAllocateInfo = VkDeviceSize

        {-# NOINLINE vkAllocationSize #-}
        vkAllocationSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, allocationSize})

        {-# INLINE vkAllocationSizeByteOffset #-}
        vkAllocationSizeByteOffset ~_
          = #{offset VkMemoryAllocateInfo, allocationSize}

        {-# INLINE readVkAllocationSize #-}
        readVkAllocationSize p
          = peekByteOff p #{offset VkMemoryAllocateInfo, allocationSize}

        {-# INLINE writeVkAllocationSize #-}
        writeVkAllocationSize p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, allocationSize}

instance {-# OVERLAPPING #-}
         HasField "allocationSize" VkMemoryAllocateInfo where
        type FieldType "allocationSize" VkMemoryAllocateInfo = VkDeviceSize
        type FieldOptional "allocationSize" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "allocationSize" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, allocationSize}
        type FieldIsArray "allocationSize" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateInfo, allocationSize}

instance CanReadField "allocationSize" VkMemoryAllocateInfo where
        {-# INLINE getField #-}
        getField = vkAllocationSize

        {-# INLINE readField #-}
        readField = readVkAllocationSize

instance CanWriteField "allocationSize" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkAllocationSize

instance {-# OVERLAPPING #-}
         HasVkMemoryTypeIndex VkMemoryAllocateInfo where
        type VkMemoryTypeIndexMType VkMemoryAllocateInfo = Word32

        {-# NOINLINE vkMemoryTypeIndex #-}
        vkMemoryTypeIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, memoryTypeIndex})

        {-# INLINE vkMemoryTypeIndexByteOffset #-}
        vkMemoryTypeIndexByteOffset ~_
          = #{offset VkMemoryAllocateInfo, memoryTypeIndex}

        {-# INLINE readVkMemoryTypeIndex #-}
        readVkMemoryTypeIndex p
          = peekByteOff p #{offset VkMemoryAllocateInfo, memoryTypeIndex}

        {-# INLINE writeVkMemoryTypeIndex #-}
        writeVkMemoryTypeIndex p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, memoryTypeIndex}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeIndex" VkMemoryAllocateInfo where
        type FieldType "memoryTypeIndex" VkMemoryAllocateInfo = Word32
        type FieldOptional "memoryTypeIndex" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeIndex" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, memoryTypeIndex}
        type FieldIsArray "memoryTypeIndex" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateInfo, memoryTypeIndex}

instance CanReadField "memoryTypeIndex" VkMemoryAllocateInfo where
        {-# INLINE getField #-}
        getField = vkMemoryTypeIndex

        {-# INLINE readField #-}
        readField = readVkMemoryTypeIndex

instance CanWriteField "memoryTypeIndex" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMemoryTypeIndex

instance Show VkMemoryAllocateInfo where
        showsPrec d x
          = showString "VkMemoryAllocateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAllocationSize = " .
                            showsPrec d (vkAllocationSize x) .
                              showString ", " .
                                showString "vkMemoryTypeIndex = " .
                                  showsPrec d (vkMemoryTypeIndex x) . showChar '}'
