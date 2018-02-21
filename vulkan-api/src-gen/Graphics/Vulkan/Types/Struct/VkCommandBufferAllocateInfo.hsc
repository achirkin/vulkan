#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCommandBufferAllocateInfo
       (VkCommandBufferAllocateInfo(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkCommandBufferLevel (VkCommandBufferLevel)
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                   (VkCommandPool)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkCommandBufferAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandPool          commandPool;
--   >     VkCommandBufferLevel   level;
--   >     uint32_t               commandBufferCount;
--   > } VkCommandBufferAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkCommandBufferAllocateInfo.html VkCommandBufferAllocateInfo registry at www.khronos.org>
data VkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo## Addr##
                                                                ByteArray##

instance Eq VkCommandBufferAllocateInfo where
        (VkCommandBufferAllocateInfo## a _) ==
          x@(VkCommandBufferAllocateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandBufferAllocateInfo where
        (VkCommandBufferAllocateInfo## a _) `compare`
          x@(VkCommandBufferAllocateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandBufferAllocateInfo where
        sizeOf ~_ = #{size VkCommandBufferAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCommandBufferAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandBufferAllocateInfo where
        unsafeAddr (VkCommandBufferAllocateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandBufferAllocateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandBufferAllocateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandBufferAllocateInfo where
        type StructFields VkCommandBufferAllocateInfo =
             '["sType", "pNext", "commandPool", "level", "commandBufferCount"] -- ' closing tick for hsc2hs
        type CUnionType VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandBufferAllocateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkCommandBufferAllocateInfo
         where
        type VkSTypeMType VkCommandBufferAllocateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkCommandBufferAllocateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandBufferAllocateInfo where
        type FieldType "sType" VkCommandBufferAllocateInfo =
             VkStructureType
        type FieldOptional "sType" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, sType}
        type FieldIsArray "sType" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, sType}

instance CanReadField "sType" VkCommandBufferAllocateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkCommandBufferAllocateInfo
         where
        type VkPNextMType VkCommandBufferAllocateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkCommandBufferAllocateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandBufferAllocateInfo where
        type FieldType "pNext" VkCommandBufferAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, pNext}
        type FieldIsArray "pNext" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, pNext}

instance CanReadField "pNext" VkCommandBufferAllocateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkCommandPool VkCommandBufferAllocateInfo where
        type VkCommandPoolMType VkCommandBufferAllocateInfo = VkCommandPool

        {-# NOINLINE vkCommandPool #-}
        vkCommandPool x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, commandPool})

        {-# INLINE vkCommandPoolByteOffset #-}
        vkCommandPoolByteOffset ~_
          = #{offset VkCommandBufferAllocateInfo, commandPool}

        {-# INLINE readVkCommandPool #-}
        readVkCommandPool p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, commandPool}

        {-# INLINE writeVkCommandPool #-}
        writeVkCommandPool p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, commandPool}

instance {-# OVERLAPPING #-}
         HasField "commandPool" VkCommandBufferAllocateInfo where
        type FieldType "commandPool" VkCommandBufferAllocateInfo =
             VkCommandPool
        type FieldOptional "commandPool" VkCommandBufferAllocateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "commandPool" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, commandPool}
        type FieldIsArray "commandPool" VkCommandBufferAllocateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, commandPool}

instance CanReadField "commandPool" VkCommandBufferAllocateInfo
         where
        {-# INLINE getField #-}
        getField = vkCommandPool

        {-# INLINE readField #-}
        readField = readVkCommandPool

instance CanWriteField "commandPool" VkCommandBufferAllocateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkCommandPool

instance {-# OVERLAPPING #-} HasVkLevel VkCommandBufferAllocateInfo
         where
        type VkLevelMType VkCommandBufferAllocateInfo =
             VkCommandBufferLevel

        {-# NOINLINE vkLevel #-}
        vkLevel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, level})

        {-# INLINE vkLevelByteOffset #-}
        vkLevelByteOffset ~_
          = #{offset VkCommandBufferAllocateInfo, level}

        {-# INLINE readVkLevel #-}
        readVkLevel p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, level}

        {-# INLINE writeVkLevel #-}
        writeVkLevel p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, level}

instance {-# OVERLAPPING #-}
         HasField "level" VkCommandBufferAllocateInfo where
        type FieldType "level" VkCommandBufferAllocateInfo =
             VkCommandBufferLevel
        type FieldOptional "level" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "level" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, level}
        type FieldIsArray "level" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, level}

instance CanReadField "level" VkCommandBufferAllocateInfo where
        {-# INLINE getField #-}
        getField = vkLevel

        {-# INLINE readField #-}
        readField = readVkLevel

instance CanWriteField "level" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkLevel

instance {-# OVERLAPPING #-}
         HasVkCommandBufferCount VkCommandBufferAllocateInfo where
        type VkCommandBufferCountMType VkCommandBufferAllocateInfo = Word32

        {-# NOINLINE vkCommandBufferCount #-}
        vkCommandBufferCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, commandBufferCount})

        {-# INLINE vkCommandBufferCountByteOffset #-}
        vkCommandBufferCountByteOffset ~_
          = #{offset VkCommandBufferAllocateInfo, commandBufferCount}

        {-# INLINE readVkCommandBufferCount #-}
        readVkCommandBufferCount p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, commandBufferCount}

        {-# INLINE writeVkCommandBufferCount #-}
        writeVkCommandBufferCount p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         HasField "commandBufferCount" VkCommandBufferAllocateInfo where
        type FieldType "commandBufferCount" VkCommandBufferAllocateInfo =
             Word32
        type FieldOptional "commandBufferCount" VkCommandBufferAllocateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "commandBufferCount" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, commandBufferCount}
        type FieldIsArray "commandBufferCount" VkCommandBufferAllocateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, commandBufferCount}

instance CanReadField "commandBufferCount"
           VkCommandBufferAllocateInfo
         where
        {-# INLINE getField #-}
        getField = vkCommandBufferCount

        {-# INLINE readField #-}
        readField = readVkCommandBufferCount

instance CanWriteField "commandBufferCount"
           VkCommandBufferAllocateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkCommandBufferCount

instance Show VkCommandBufferAllocateInfo where
        showsPrec d x
          = showString "VkCommandBufferAllocateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkCommandPool = " .
                            showsPrec d (vkCommandPool x) .
                              showString ", " .
                                showString "vkLevel = " .
                                  showsPrec d (vkLevel x) .
                                    showString ", " .
                                      showString "vkCommandBufferCount = " .
                                        showsPrec d (vkCommandBufferCount x) . showChar '}'
