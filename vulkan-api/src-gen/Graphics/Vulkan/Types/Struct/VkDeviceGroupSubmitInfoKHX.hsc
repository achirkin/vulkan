#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupSubmitInfoKHX
       (VkDeviceGroupSubmitInfoKHX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo  (VkSubmitInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupSubmitInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         waitSemaphoreCount;
--   >     const uint32_t*    pWaitSemaphoreDeviceIndices;
--   >     uint32_t         commandBufferCount;
--   >     const uint32_t*    pCommandBufferDeviceMasks;
--   >     uint32_t         signalSemaphoreCount;
--   >     const uint32_t*  pSignalSemaphoreDeviceIndices;
--   > } VkDeviceGroupSubmitInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupSubmitInfoKHX.html VkDeviceGroupSubmitInfoKHX registry at www.khronos.org>
data VkDeviceGroupSubmitInfoKHX = VkDeviceGroupSubmitInfoKHX## Addr##
                                                              ByteArray##

instance Eq VkDeviceGroupSubmitInfoKHX where
        (VkDeviceGroupSubmitInfoKHX## a _) ==
          x@(VkDeviceGroupSubmitInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupSubmitInfoKHX where
        (VkDeviceGroupSubmitInfoKHX## a _) `compare`
          x@(VkDeviceGroupSubmitInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupSubmitInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupSubmitInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupSubmitInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupSubmitInfoKHX where
        unsafeAddr (VkDeviceGroupSubmitInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupSubmitInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupSubmitInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupSubmitInfoKHX where
        type StructFields VkDeviceGroupSubmitInfoKHX =
             '["sType", "pNext", "waitSemaphoreCount", -- ' closing tick for hsc2hs
               "pWaitSemaphoreDeviceIndices", "commandBufferCount",
               "pCommandBufferDeviceMasks", "signalSemaphoreCount",
               "pSignalSemaphoreDeviceIndices"]
        type CUnionType VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupSubmitInfoKHX = '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkDeviceGroupSubmitInfoKHX
         where
        type VkSTypeMType VkDeviceGroupSubmitInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupSubmitInfoKHX where
        type FieldType "sType" VkDeviceGroupSubmitInfoKHX = VkStructureType
        type FieldOptional "sType" VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupSubmitInfoKHX =
             #{offset VkDeviceGroupSubmitInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, sType}

instance CanReadField "sType" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDeviceGroupSubmitInfoKHX
         where
        type VkPNextMType VkDeviceGroupSubmitInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupSubmitInfoKHX where
        type FieldType "pNext" VkDeviceGroupSubmitInfoKHX = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupSubmitInfoKHX =
             #{offset VkDeviceGroupSubmitInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkWaitSemaphoreCount VkDeviceGroupSubmitInfoKHX where
        type VkWaitSemaphoreCountMType VkDeviceGroupSubmitInfoKHX = Word32

        {-# NOINLINE vkWaitSemaphoreCount #-}
        vkWaitSemaphoreCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount})

        {-# INLINE vkWaitSemaphoreCountByteOffset #-}
        vkWaitSemaphoreCountByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}

        {-# INLINE readVkWaitSemaphoreCount #-}
        readVkWaitSemaphoreCount p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}

        {-# INLINE writeVkWaitSemaphoreCount #-}
        writeVkWaitSemaphoreCount p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX where
        type FieldType "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX =
             Word32
        type FieldOptional "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX =
             #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}
        type FieldIsArray "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}

instance CanReadField "waitSemaphoreCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkWaitSemaphoreCount

        {-# INLINE readField #-}
        readField = readVkWaitSemaphoreCount

instance CanWriteField "waitSemaphoreCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkWaitSemaphoreCount

instance {-# OVERLAPPING #-}
         HasVkPWaitSemaphoreDeviceIndices VkDeviceGroupSubmitInfoKHX where
        type VkPWaitSemaphoreDeviceIndicesMType VkDeviceGroupSubmitInfoKHX
             = Ptr Word32

        {-# NOINLINE vkPWaitSemaphoreDeviceIndices #-}
        vkPWaitSemaphoreDeviceIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices})

        {-# INLINE vkPWaitSemaphoreDeviceIndicesByteOffset #-}
        vkPWaitSemaphoreDeviceIndicesByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}

        {-# INLINE readVkPWaitSemaphoreDeviceIndices #-}
        readVkPWaitSemaphoreDeviceIndices p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}

        {-# INLINE writeVkPWaitSemaphoreDeviceIndices #-}
        writeVkPWaitSemaphoreDeviceIndices p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphoreDeviceIndices" VkDeviceGroupSubmitInfoKHX
         where
        type FieldType "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = Ptr Word32
        type FieldOptional "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             =
             #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}
        type FieldIsArray "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}

instance CanReadField "pWaitSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPWaitSemaphoreDeviceIndices

        {-# INLINE readField #-}
        readField = readVkPWaitSemaphoreDeviceIndices

instance CanWriteField "pWaitSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPWaitSemaphoreDeviceIndices

instance {-# OVERLAPPING #-}
         HasVkCommandBufferCount VkDeviceGroupSubmitInfoKHX where
        type VkCommandBufferCountMType VkDeviceGroupSubmitInfoKHX = Word32

        {-# NOINLINE vkCommandBufferCount #-}
        vkCommandBufferCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount})

        {-# INLINE vkCommandBufferCountByteOffset #-}
        vkCommandBufferCountByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}

        {-# INLINE readVkCommandBufferCount #-}
        readVkCommandBufferCount p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}

        {-# INLINE writeVkCommandBufferCount #-}
        writeVkCommandBufferCount p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}

instance {-# OVERLAPPING #-}
         HasField "commandBufferCount" VkDeviceGroupSubmitInfoKHX where
        type FieldType "commandBufferCount" VkDeviceGroupSubmitInfoKHX =
             Word32
        type FieldOptional "commandBufferCount" VkDeviceGroupSubmitInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "commandBufferCount" VkDeviceGroupSubmitInfoKHX =
             #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}
        type FieldIsArray "commandBufferCount" VkDeviceGroupSubmitInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}

instance CanReadField "commandBufferCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkCommandBufferCount

        {-# INLINE readField #-}
        readField = readVkCommandBufferCount

instance CanWriteField "commandBufferCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkCommandBufferCount

instance {-# OVERLAPPING #-}
         HasVkPCommandBufferDeviceMasks VkDeviceGroupSubmitInfoKHX where
        type VkPCommandBufferDeviceMasksMType VkDeviceGroupSubmitInfoKHX =
             Ptr Word32

        {-# NOINLINE vkPCommandBufferDeviceMasks #-}
        vkPCommandBufferDeviceMasks x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks})

        {-# INLINE vkPCommandBufferDeviceMasksByteOffset #-}
        vkPCommandBufferDeviceMasksByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}

        {-# INLINE readVkPCommandBufferDeviceMasks #-}
        readVkPCommandBufferDeviceMasks p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}

        {-# INLINE writeVkPCommandBufferDeviceMasks #-}
        writeVkPCommandBufferDeviceMasks p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}

instance {-# OVERLAPPING #-}
         HasField "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfoKHX
         where
        type FieldType "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfoKHX
             = Ptr Word32
        type FieldOptional "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfoKHX
             =
             #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}
        type FieldIsArray "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}

instance CanReadField "pCommandBufferDeviceMasks"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPCommandBufferDeviceMasks

        {-# INLINE readField #-}
        readField = readVkPCommandBufferDeviceMasks

instance CanWriteField "pCommandBufferDeviceMasks"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPCommandBufferDeviceMasks

instance {-# OVERLAPPING #-}
         HasVkSignalSemaphoreCount VkDeviceGroupSubmitInfoKHX where
        type VkSignalSemaphoreCountMType VkDeviceGroupSubmitInfoKHX =
             Word32

        {-# NOINLINE vkSignalSemaphoreCount #-}
        vkSignalSemaphoreCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount})

        {-# INLINE vkSignalSemaphoreCountByteOffset #-}
        vkSignalSemaphoreCountByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}

        {-# INLINE readVkSignalSemaphoreCount #-}
        readVkSignalSemaphoreCount p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}

        {-# INLINE writeVkSignalSemaphoreCount #-}
        writeVkSignalSemaphoreCount p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "signalSemaphoreCount" VkDeviceGroupSubmitInfoKHX where
        type FieldType "signalSemaphoreCount" VkDeviceGroupSubmitInfoKHX =
             Word32
        type FieldOptional "signalSemaphoreCount"
               VkDeviceGroupSubmitInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "signalSemaphoreCount" VkDeviceGroupSubmitInfoKHX
             =
             #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}
        type FieldIsArray "signalSemaphoreCount" VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}

instance CanReadField "signalSemaphoreCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSignalSemaphoreCount

        {-# INLINE readField #-}
        readField = readVkSignalSemaphoreCount

instance CanWriteField "signalSemaphoreCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSignalSemaphoreCount

instance {-# OVERLAPPING #-}
         HasVkPSignalSemaphoreDeviceIndices VkDeviceGroupSubmitInfoKHX where
        type VkPSignalSemaphoreDeviceIndicesMType
               VkDeviceGroupSubmitInfoKHX
             = Ptr Word32

        {-# NOINLINE vkPSignalSemaphoreDeviceIndices #-}
        vkPSignalSemaphoreDeviceIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices})

        {-# INLINE vkPSignalSemaphoreDeviceIndicesByteOffset #-}
        vkPSignalSemaphoreDeviceIndicesByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}

        {-# INLINE readVkPSignalSemaphoreDeviceIndices #-}
        readVkPSignalSemaphoreDeviceIndices p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}

        {-# INLINE writeVkPSignalSemaphoreDeviceIndices #-}
        writeVkPSignalSemaphoreDeviceIndices p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "pSignalSemaphoreDeviceIndices" VkDeviceGroupSubmitInfoKHX
         where
        type FieldType "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = Ptr Word32
        type FieldOptional "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             =
             #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}
        type FieldIsArray "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}

instance CanReadField "pSignalSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPSignalSemaphoreDeviceIndices

        {-# INLINE readField #-}
        readField = readVkPSignalSemaphoreDeviceIndices

instance CanWriteField "pSignalSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPSignalSemaphoreDeviceIndices

instance Show VkDeviceGroupSubmitInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupSubmitInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkWaitSemaphoreCount = " .
                            showsPrec d (vkWaitSemaphoreCount x) .
                              showString ", " .
                                showString "vkPWaitSemaphoreDeviceIndices = " .
                                  showsPrec d (vkPWaitSemaphoreDeviceIndices x) .
                                    showString ", " .
                                      showString "vkCommandBufferCount = " .
                                        showsPrec d (vkCommandBufferCount x) .
                                          showString ", " .
                                            showString "vkPCommandBufferDeviceMasks = " .
                                              showsPrec d (vkPCommandBufferDeviceMasks x) .
                                                showString ", " .
                                                  showString "vkSignalSemaphoreCount = " .
                                                    showsPrec d (vkSignalSemaphoreCount x) .
                                                      showString ", " .
                                                        showString
                                                          "vkPSignalSemaphoreDeviceIndices = "
                                                          .
                                                          showsPrec d
                                                            (vkPSignalSemaphoreDeviceIndices x)
                                                            . showChar '}'
