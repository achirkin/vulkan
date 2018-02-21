#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryDedicatedAllocateInfoKHR
       (VkMemoryDedicatedAllocateInfoKHR(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Handles                     (VkBuffer,
                                                                    VkImage)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryDedicatedAllocateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage          image;
--   >     VkBuffer         buffer;
--   > } VkMemoryDedicatedAllocateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryDedicatedAllocateInfoKHR.html VkMemoryDedicatedAllocateInfoKHR registry at www.khronos.org>
data VkMemoryDedicatedAllocateInfoKHR = VkMemoryDedicatedAllocateInfoKHR## Addr##
                                                                          ByteArray##

instance Eq VkMemoryDedicatedAllocateInfoKHR where
        (VkMemoryDedicatedAllocateInfoKHR## a _) ==
          x@(VkMemoryDedicatedAllocateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryDedicatedAllocateInfoKHR where
        (VkMemoryDedicatedAllocateInfoKHR## a _) `compare`
          x@(VkMemoryDedicatedAllocateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryDedicatedAllocateInfoKHR where
        sizeOf ~_ = #{size VkMemoryDedicatedAllocateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryDedicatedAllocateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryDedicatedAllocateInfoKHR where
        unsafeAddr (VkMemoryDedicatedAllocateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryDedicatedAllocateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryDedicatedAllocateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryDedicatedAllocateInfoKHR where
        type StructFields VkMemoryDedicatedAllocateInfoKHR =
             '["sType", "pNext", "image", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryDedicatedAllocateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryDedicatedAllocateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryDedicatedAllocateInfoKHR =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkMemoryDedicatedAllocateInfoKHR where
        type VkSTypeMType VkMemoryDedicatedAllocateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryDedicatedAllocateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryDedicatedAllocateInfoKHR where
        type FieldType "sType" VkMemoryDedicatedAllocateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryDedicatedAllocateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryDedicatedAllocateInfoKHR =
             #{offset VkMemoryDedicatedAllocateInfoKHR, sType}
        type FieldIsArray "sType" VkMemoryDedicatedAllocateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfoKHR, sType}

instance CanReadField "sType" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryDedicatedAllocateInfoKHR where
        type VkPNextMType VkMemoryDedicatedAllocateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryDedicatedAllocateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryDedicatedAllocateInfoKHR where
        type FieldType "pNext" VkMemoryDedicatedAllocateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryDedicatedAllocateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryDedicatedAllocateInfoKHR =
             #{offset VkMemoryDedicatedAllocateInfoKHR, pNext}
        type FieldIsArray "pNext" VkMemoryDedicatedAllocateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfoKHR, pNext}

instance CanReadField "pNext" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkImage VkMemoryDedicatedAllocateInfoKHR where
        type VkImageMType VkMemoryDedicatedAllocateInfoKHR = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfoKHR, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkMemoryDedicatedAllocateInfoKHR, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, image}

instance {-# OVERLAPPING #-}
         HasField "image" VkMemoryDedicatedAllocateInfoKHR where
        type FieldType "image" VkMemoryDedicatedAllocateInfoKHR = VkImage
        type FieldOptional "image" VkMemoryDedicatedAllocateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "image" VkMemoryDedicatedAllocateInfoKHR =
             #{offset VkMemoryDedicatedAllocateInfoKHR, image}
        type FieldIsArray "image" VkMemoryDedicatedAllocateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfoKHR, image}

instance CanReadField "image" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance {-# OVERLAPPING #-}
         HasVkBuffer VkMemoryDedicatedAllocateInfoKHR where
        type VkBufferMType VkMemoryDedicatedAllocateInfoKHR = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfoKHR, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkMemoryDedicatedAllocateInfoKHR, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, buffer}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkMemoryDedicatedAllocateInfoKHR where
        type FieldType "buffer" VkMemoryDedicatedAllocateInfoKHR = VkBuffer
        type FieldOptional "buffer" VkMemoryDedicatedAllocateInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkMemoryDedicatedAllocateInfoKHR =
             #{offset VkMemoryDedicatedAllocateInfoKHR, buffer}
        type FieldIsArray "buffer" VkMemoryDedicatedAllocateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfoKHR, buffer}

instance CanReadField "buffer" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance Show VkMemoryDedicatedAllocateInfoKHR where
        showsPrec d x
          = showString "VkMemoryDedicatedAllocateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkImage = " .
                            showsPrec d (vkImage x) .
                              showString ", " .
                                showString "vkBuffer = " . showsPrec d (vkBuffer x) . showChar '}'
