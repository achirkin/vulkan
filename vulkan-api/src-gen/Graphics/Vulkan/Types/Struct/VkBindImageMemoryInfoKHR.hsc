#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR
       (VkBindImageMemoryInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkDeviceMemory,
                                                             VkImage)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBindImageMemoryInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage                          image;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindImageMemoryInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBindImageMemoryInfoKHR.html VkBindImageMemoryInfoKHR registry at www.khronos.org>
data VkBindImageMemoryInfoKHR = VkBindImageMemoryInfoKHR## Addr##
                                                          ByteArray##

instance Eq VkBindImageMemoryInfoKHR where
        (VkBindImageMemoryInfoKHR## a _) ==
          x@(VkBindImageMemoryInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemoryInfoKHR where
        (VkBindImageMemoryInfoKHR## a _) `compare`
          x@(VkBindImageMemoryInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemoryInfoKHR where
        sizeOf ~_ = #{size VkBindImageMemoryInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindImageMemoryInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImageMemoryInfoKHR where
        unsafeAddr (VkBindImageMemoryInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImageMemoryInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImageMemoryInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImageMemoryInfoKHR where
        type StructFields VkBindImageMemoryInfoKHR =
             '["sType", "pNext", "image", "memory", "memoryOffset"] -- ' closing tick for hsc2hs
        type CUnionType VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImageMemoryInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkBindImageMemoryInfoKHR
         where
        type VkSTypeMType VkBindImageMemoryInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindImageMemoryInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImageMemoryInfoKHR where
        type FieldType "sType" VkBindImageMemoryInfoKHR = VkStructureType
        type FieldOptional "sType" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImageMemoryInfoKHR =
             #{offset VkBindImageMemoryInfoKHR, sType}
        type FieldIsArray "sType" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfoKHR, sType}

instance CanReadField "sType" VkBindImageMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkBindImageMemoryInfoKHR
         where
        type VkPNextMType VkBindImageMemoryInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindImageMemoryInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImageMemoryInfoKHR where
        type FieldType "pNext" VkBindImageMemoryInfoKHR = Ptr Void
        type FieldOptional "pNext" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImageMemoryInfoKHR =
             #{offset VkBindImageMemoryInfoKHR, pNext}
        type FieldIsArray "pNext" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfoKHR, pNext}

instance CanReadField "pNext" VkBindImageMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkImage VkBindImageMemoryInfoKHR
         where
        type VkImageMType VkBindImageMemoryInfoKHR = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkBindImageMemoryInfoKHR, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, image}

instance {-# OVERLAPPING #-}
         HasField "image" VkBindImageMemoryInfoKHR where
        type FieldType "image" VkBindImageMemoryInfoKHR = VkImage
        type FieldOptional "image" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkBindImageMemoryInfoKHR =
             #{offset VkBindImageMemoryInfoKHR, image}
        type FieldIsArray "image" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfoKHR, image}

instance CanReadField "image" VkBindImageMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance {-# OVERLAPPING #-} HasVkMemory VkBindImageMemoryInfoKHR
         where
        type VkMemoryMType VkBindImageMemoryInfoKHR = VkDeviceMemory

        {-# NOINLINE vkMemory #-}
        vkMemory x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, memory})

        {-# INLINE vkMemoryByteOffset #-}
        vkMemoryByteOffset ~_
          = #{offset VkBindImageMemoryInfoKHR, memory}

        {-# INLINE readVkMemory #-}
        readVkMemory p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, memory}

        {-# INLINE writeVkMemory #-}
        writeVkMemory p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, memory}

instance {-# OVERLAPPING #-}
         HasField "memory" VkBindImageMemoryInfoKHR where
        type FieldType "memory" VkBindImageMemoryInfoKHR = VkDeviceMemory
        type FieldOptional "memory" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkBindImageMemoryInfoKHR =
             #{offset VkBindImageMemoryInfoKHR, memory}
        type FieldIsArray "memory" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryInfoKHR, memory}

instance CanReadField "memory" VkBindImageMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkMemory

        {-# INLINE readField #-}
        readField = readVkMemory

instance CanWriteField "memory" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkMemory

instance {-# OVERLAPPING #-}
         HasVkMemoryOffset VkBindImageMemoryInfoKHR where
        type VkMemoryOffsetMType VkBindImageMemoryInfoKHR = VkDeviceSize

        {-# NOINLINE vkMemoryOffset #-}
        vkMemoryOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, memoryOffset})

        {-# INLINE vkMemoryOffsetByteOffset #-}
        vkMemoryOffsetByteOffset ~_
          = #{offset VkBindImageMemoryInfoKHR, memoryOffset}

        {-# INLINE readVkMemoryOffset #-}
        readVkMemoryOffset p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, memoryOffset}

        {-# INLINE writeVkMemoryOffset #-}
        writeVkMemoryOffset p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, memoryOffset}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkBindImageMemoryInfoKHR where
        type FieldType "memoryOffset" VkBindImageMemoryInfoKHR =
             VkDeviceSize
        type FieldOptional "memoryOffset" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkBindImageMemoryInfoKHR =
             #{offset VkBindImageMemoryInfoKHR, memoryOffset}
        type FieldIsArray "memoryOffset" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryInfoKHR, memoryOffset}

instance CanReadField "memoryOffset" VkBindImageMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkMemoryOffset

        {-# INLINE readField #-}
        readField = readVkMemoryOffset

instance CanWriteField "memoryOffset" VkBindImageMemoryInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkMemoryOffset

instance Show VkBindImageMemoryInfoKHR where
        showsPrec d x
          = showString "VkBindImageMemoryInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkImage = " .
                            showsPrec d (vkImage x) .
                              showString ", " .
                                showString "vkMemory = " .
                                  showsPrec d (vkMemory x) .
                                    showString ", " .
                                      showString "vkMemoryOffset = " .
                                        showsPrec d (vkMemoryOffset x) . showChar '}'
