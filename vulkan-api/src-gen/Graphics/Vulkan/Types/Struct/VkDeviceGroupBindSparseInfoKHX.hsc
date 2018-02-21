#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupBindSparseInfoKHX
       (VkDeviceGroupBindSparseInfoKHX(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBindSparseInfo (VkBindSparseInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupBindSparseInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         resourceDeviceIndex;
--   >     uint32_t                         memoryDeviceIndex;
--   > } VkDeviceGroupBindSparseInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupBindSparseInfoKHX.html VkDeviceGroupBindSparseInfoKHX registry at www.khronos.org>
data VkDeviceGroupBindSparseInfoKHX = VkDeviceGroupBindSparseInfoKHX## Addr##
                                                                      ByteArray##

instance Eq VkDeviceGroupBindSparseInfoKHX where
        (VkDeviceGroupBindSparseInfoKHX## a _) ==
          x@(VkDeviceGroupBindSparseInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupBindSparseInfoKHX where
        (VkDeviceGroupBindSparseInfoKHX## a _) `compare`
          x@(VkDeviceGroupBindSparseInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupBindSparseInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupBindSparseInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupBindSparseInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupBindSparseInfoKHX where
        unsafeAddr (VkDeviceGroupBindSparseInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupBindSparseInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupBindSparseInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupBindSparseInfoKHX where
        type StructFields VkDeviceGroupBindSparseInfoKHX =
             '["sType", "pNext", "resourceDeviceIndex", "memoryDeviceIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupBindSparseInfoKHX =
             '[VkBindSparseInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupBindSparseInfoKHX where
        type VkSTypeMType VkDeviceGroupBindSparseInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupBindSparseInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "sType" VkDeviceGroupBindSparseInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupBindSparseInfoKHX =
             #{offset VkDeviceGroupBindSparseInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, sType}

instance CanReadField "sType" VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupBindSparseInfoKHX where
        type VkPNextMType VkDeviceGroupBindSparseInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "pNext" VkDeviceGroupBindSparseInfoKHX = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupBindSparseInfoKHX =
             #{offset VkDeviceGroupBindSparseInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkResourceDeviceIndex VkDeviceGroupBindSparseInfoKHX where
        type VkResourceDeviceIndexMType VkDeviceGroupBindSparseInfoKHX =
             Word32

        {-# NOINLINE vkResourceDeviceIndex #-}
        vkResourceDeviceIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex})

        {-# INLINE vkResourceDeviceIndexByteOffset #-}
        vkResourceDeviceIndexByteOffset ~_
          = #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

        {-# INLINE readVkResourceDeviceIndex #-}
        readVkResourceDeviceIndex p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

        {-# INLINE writeVkResourceDeviceIndex #-}
        writeVkResourceDeviceIndex p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         HasField "resourceDeviceIndex" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "resourceDeviceIndex" VkDeviceGroupBindSparseInfoKHX
             = Word32
        type FieldOptional "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             =
             #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}
        type FieldIsArray "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

instance CanReadField "resourceDeviceIndex"
           VkDeviceGroupBindSparseInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkResourceDeviceIndex

        {-# INLINE readField #-}
        readField = readVkResourceDeviceIndex

instance CanWriteField "resourceDeviceIndex"
           VkDeviceGroupBindSparseInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkResourceDeviceIndex

instance {-# OVERLAPPING #-}
         HasVkMemoryDeviceIndex VkDeviceGroupBindSparseInfoKHX where
        type VkMemoryDeviceIndexMType VkDeviceGroupBindSparseInfoKHX =
             Word32

        {-# NOINLINE vkMemoryDeviceIndex #-}
        vkMemoryDeviceIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex})

        {-# INLINE vkMemoryDeviceIndexByteOffset #-}
        vkMemoryDeviceIndexByteOffset ~_
          = #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

        {-# INLINE readVkMemoryDeviceIndex #-}
        readVkMemoryDeviceIndex p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

        {-# INLINE writeVkMemoryDeviceIndex #-}
        writeVkMemoryDeviceIndex p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

instance {-# OVERLAPPING #-}
         HasField "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX =
             Word32
        type FieldOptional "memoryDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX
             =
             #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}
        type FieldIsArray "memoryDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

instance CanReadField "memoryDeviceIndex"
           VkDeviceGroupBindSparseInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkMemoryDeviceIndex

        {-# INLINE readField #-}
        readField = readVkMemoryDeviceIndex

instance CanWriteField "memoryDeviceIndex"
           VkDeviceGroupBindSparseInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMemoryDeviceIndex

instance Show VkDeviceGroupBindSparseInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupBindSparseInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkResourceDeviceIndex = " .
                            showsPrec d (vkResourceDeviceIndex x) .
                              showString ", " .
                                showString "vkMemoryDeviceIndex = " .
                                  showsPrec d (vkMemoryDeviceIndex x) . showChar '}'
