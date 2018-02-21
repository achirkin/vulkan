#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDedicatedAllocationMemoryAllocateInfoNV
       (VkDedicatedAllocationMemoryAllocateInfoNV(..)) where
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

-- | > typedef struct VkDedicatedAllocationMemoryAllocateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage          image;
--   >     VkBuffer         buffer;
--   > } VkDedicatedAllocationMemoryAllocateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDedicatedAllocationMemoryAllocateInfoNV.html VkDedicatedAllocationMemoryAllocateInfoNV registry at www.khronos.org>
data VkDedicatedAllocationMemoryAllocateInfoNV = VkDedicatedAllocationMemoryAllocateInfoNV## Addr##
                                                                                            ByteArray##

instance Eq VkDedicatedAllocationMemoryAllocateInfoNV where
        (VkDedicatedAllocationMemoryAllocateInfoNV## a _) ==
          x@(VkDedicatedAllocationMemoryAllocateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDedicatedAllocationMemoryAllocateInfoNV where
        (VkDedicatedAllocationMemoryAllocateInfoNV## a _) `compare`
          x@(VkDedicatedAllocationMemoryAllocateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDedicatedAllocationMemoryAllocateInfoNV where
        sizeOf ~_
          = #{size VkDedicatedAllocationMemoryAllocateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDedicatedAllocationMemoryAllocateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        unsafeAddr (VkDedicatedAllocationMemoryAllocateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDedicatedAllocationMemoryAllocateInfoNV## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDedicatedAllocationMemoryAllocateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDedicatedAllocationMemoryAllocateInfoNV
         where
        type StructFields VkDedicatedAllocationMemoryAllocateInfoNV =
             '["sType", "pNext", "image", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkDedicatedAllocationMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDedicatedAllocationMemoryAllocateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkDedicatedAllocationMemoryAllocateInfoNV =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDedicatedAllocationMemoryAllocateInfoNV where
        type VkSTypeMType VkDedicatedAllocationMemoryAllocateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "sType" VkDedicatedAllocationMemoryAllocateInfoNV =
             VkStructureType
        type FieldOptional "sType"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}
        type FieldIsArray "sType" VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

instance CanReadField "sType"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDedicatedAllocationMemoryAllocateInfoNV where
        type VkPNextMType VkDedicatedAllocationMemoryAllocateInfoNV =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "pNext" VkDedicatedAllocationMemoryAllocateInfoNV =
             Ptr Void
        type FieldOptional "pNext"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}
        type FieldIsArray "pNext" VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

instance CanReadField "pNext"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkImage VkDedicatedAllocationMemoryAllocateInfoNV where
        type VkImageMType VkDedicatedAllocationMemoryAllocateInfoNV =
             VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

instance {-# OVERLAPPING #-}
         HasField "image" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "image" VkDedicatedAllocationMemoryAllocateInfoNV =
             VkImage
        type FieldOptional "image"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "image" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}
        type FieldIsArray "image" VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

instance CanReadField "image"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance {-# OVERLAPPING #-}
         HasVkBuffer VkDedicatedAllocationMemoryAllocateInfoNV where
        type VkBufferMType VkDedicatedAllocationMemoryAllocateInfoNV =
             VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "buffer" VkDedicatedAllocationMemoryAllocateInfoNV =
             VkBuffer
        type FieldOptional "buffer"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}
        type FieldIsArray "buffer"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

instance CanReadField "buffer"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance Show VkDedicatedAllocationMemoryAllocateInfoNV where
        showsPrec d x
          = showString "VkDedicatedAllocationMemoryAllocateInfoNV {" .
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
