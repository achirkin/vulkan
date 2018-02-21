#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryDeviceGroupInfoKHX
       (VkBindBufferMemoryDeviceGroupInfoKHX(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfoKHR (VkBindBufferMemoryInfoKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkBindBufferMemoryDeviceGroupInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   > } VkBindBufferMemoryDeviceGroupInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBindBufferMemoryDeviceGroupInfoKHX.html VkBindBufferMemoryDeviceGroupInfoKHX registry at www.khronos.org>
data VkBindBufferMemoryDeviceGroupInfoKHX = VkBindBufferMemoryDeviceGroupInfoKHX## Addr##
                                                                                  ByteArray##

instance Eq VkBindBufferMemoryDeviceGroupInfoKHX where
        (VkBindBufferMemoryDeviceGroupInfoKHX## a _) ==
          x@(VkBindBufferMemoryDeviceGroupInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindBufferMemoryDeviceGroupInfoKHX where
        (VkBindBufferMemoryDeviceGroupInfoKHX## a _) `compare`
          x@(VkBindBufferMemoryDeviceGroupInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindBufferMemoryDeviceGroupInfoKHX where
        sizeOf ~_
          = #{size VkBindBufferMemoryDeviceGroupInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindBufferMemoryDeviceGroupInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindBufferMemoryDeviceGroupInfoKHX
         where
        unsafeAddr (VkBindBufferMemoryDeviceGroupInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindBufferMemoryDeviceGroupInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindBufferMemoryDeviceGroupInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindBufferMemoryDeviceGroupInfoKHX where
        type StructFields VkBindBufferMemoryDeviceGroupInfoKHX =
             '["sType", "pNext", "deviceIndexCount", "pDeviceIndices"] -- ' closing tick for hsc2hs
        type CUnionType VkBindBufferMemoryDeviceGroupInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindBufferMemoryDeviceGroupInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindBufferMemoryDeviceGroupInfoKHX =
             '[VkBindBufferMemoryInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkBindBufferMemoryDeviceGroupInfoKHX where
        type VkSTypeMType VkBindBufferMemoryDeviceGroupInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindBufferMemoryDeviceGroupInfoKHX where
        type FieldType "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}
        type FieldIsArray "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

instance CanReadField "sType" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkBindBufferMemoryDeviceGroupInfoKHX where
        type VkPNextMType VkBindBufferMemoryDeviceGroupInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindBufferMemoryDeviceGroupInfoKHX where
        type FieldType "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}
        type FieldIsArray "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

instance CanReadField "pNext" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDeviceIndexCount VkBindBufferMemoryDeviceGroupInfoKHX where
        type VkDeviceIndexCountMType VkBindBufferMemoryDeviceGroupInfoKHX =
             Word32

        {-# NOINLINE vkDeviceIndexCount #-}
        vkDeviceIndexCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount})

        {-# INLINE vkDeviceIndexCountByteOffset #-}
        vkDeviceIndexCountByteOffset ~_
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

        {-# INLINE readVkDeviceIndexCount #-}
        readVkDeviceIndexCount p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

        {-# INLINE writeVkDeviceIndexCount #-}
        writeVkDeviceIndexCount p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance {-# OVERLAPPING #-}
         HasField "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        type FieldType "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = Word32
        type FieldOptional "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}
        type FieldIsArray "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance CanReadField "deviceIndexCount"
           VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceIndexCount

        {-# INLINE readField #-}
        readField = readVkDeviceIndexCount

instance CanWriteField "deviceIndexCount"
           VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceIndexCount

instance {-# OVERLAPPING #-}
         HasVkPDeviceIndices VkBindBufferMemoryDeviceGroupInfoKHX where
        type VkPDeviceIndicesMType VkBindBufferMemoryDeviceGroupInfoKHX =
             Ptr Word32

        {-# NOINLINE vkPDeviceIndices #-}
        vkPDeviceIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices})

        {-# INLINE vkPDeviceIndicesByteOffset #-}
        vkPDeviceIndicesByteOffset ~_
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

        {-# INLINE readVkPDeviceIndices #-}
        readVkPDeviceIndices p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

        {-# INLINE writeVkPDeviceIndices #-}
        writeVkPDeviceIndices p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        type FieldType "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = Ptr Word32
        type FieldOptional "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}
        type FieldIsArray "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance CanReadField "pDeviceIndices"
           VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPDeviceIndices

        {-# INLINE readField #-}
        readField = readVkPDeviceIndices

instance CanWriteField "pDeviceIndices"
           VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDeviceIndices

instance Show VkBindBufferMemoryDeviceGroupInfoKHX where
        showsPrec d x
          = showString "VkBindBufferMemoryDeviceGroupInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceIndexCount = " .
                            showsPrec d (vkDeviceIndexCount x) .
                              showString ", " .
                                showString "vkPDeviceIndices = " .
                                  showsPrec d (vkPDeviceIndices x) . showChar '}'
