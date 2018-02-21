#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSubmitInfo (VkSubmitInfo(..))
       where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags (VkPipelineStageFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                   (VkCommandBuffer,
                                                                  VkSemaphore)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkSubmitInfo {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     uint32_t       waitSemaphoreCount;
--   >     const VkSemaphore*     pWaitSemaphores;
--   >     const VkPipelineStageFlags*           pWaitDstStageMask;
--   >     uint32_t       commandBufferCount;
--   >     const VkCommandBuffer*     pCommandBuffers;
--   >     uint32_t       signalSemaphoreCount;
--   >     const VkSemaphore*     pSignalSemaphores;
--   > } VkSubmitInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSubmitInfo.html VkSubmitInfo registry at www.khronos.org>
data VkSubmitInfo = VkSubmitInfo## Addr## ByteArray##

instance Eq VkSubmitInfo where
        (VkSubmitInfo## a _) == x@(VkSubmitInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSubmitInfo where
        (VkSubmitInfo## a _) `compare` x@(VkSubmitInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSubmitInfo where
        sizeOf ~_ = #{size VkSubmitInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSubmitInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSubmitInfo where
        unsafeAddr (VkSubmitInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSubmitInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSubmitInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSubmitInfo where
        type StructFields VkSubmitInfo =
             '["sType", "pNext", "waitSemaphoreCount", "pWaitSemaphores", -- ' closing tick for hsc2hs
               "pWaitDstStageMask", "commandBufferCount", "pCommandBuffers",
               "signalSemaphoreCount", "pSignalSemaphores"]
        type CUnionType VkSubmitInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSubmitInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSubmitInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkSubmitInfo where
        type VkSTypeMType VkSubmitInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_ = #{offset VkSubmitInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSubmitInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSubmitInfo, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkSubmitInfo where
        type FieldType "sType" VkSubmitInfo = VkStructureType
        type FieldOptional "sType" VkSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSubmitInfo =
             #{offset VkSubmitInfo, sType}
        type FieldIsArray "sType" VkSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubmitInfo, sType}

instance CanReadField "sType" VkSubmitInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSubmitInfo where
        type VkPNextMType VkSubmitInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_ = #{offset VkSubmitInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSubmitInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSubmitInfo, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkSubmitInfo where
        type FieldType "pNext" VkSubmitInfo = Ptr Void
        type FieldOptional "pNext" VkSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSubmitInfo =
             #{offset VkSubmitInfo, pNext}
        type FieldIsArray "pNext" VkSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubmitInfo, pNext}

instance CanReadField "pNext" VkSubmitInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkWaitSemaphoreCount VkSubmitInfo
         where
        type VkWaitSemaphoreCountMType VkSubmitInfo = Word32

        {-# NOINLINE vkWaitSemaphoreCount #-}
        vkWaitSemaphoreCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, waitSemaphoreCount})

        {-# INLINE vkWaitSemaphoreCountByteOffset #-}
        vkWaitSemaphoreCountByteOffset ~_
          = #{offset VkSubmitInfo, waitSemaphoreCount}

        {-# INLINE readVkWaitSemaphoreCount #-}
        readVkWaitSemaphoreCount p
          = peekByteOff p #{offset VkSubmitInfo, waitSemaphoreCount}

        {-# INLINE writeVkWaitSemaphoreCount #-}
        writeVkWaitSemaphoreCount p
          = pokeByteOff p #{offset VkSubmitInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreCount" VkSubmitInfo where
        type FieldType "waitSemaphoreCount" VkSubmitInfo = Word32
        type FieldOptional "waitSemaphoreCount" VkSubmitInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreCount" VkSubmitInfo =
             #{offset VkSubmitInfo, waitSemaphoreCount}
        type FieldIsArray "waitSemaphoreCount" VkSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubmitInfo, waitSemaphoreCount}

instance CanReadField "waitSemaphoreCount" VkSubmitInfo where
        {-# INLINE getField #-}
        getField = vkWaitSemaphoreCount

        {-# INLINE readField #-}
        readField = readVkWaitSemaphoreCount

instance CanWriteField "waitSemaphoreCount" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField = writeVkWaitSemaphoreCount

instance {-# OVERLAPPING #-} HasVkPWaitSemaphores VkSubmitInfo
         where
        type VkPWaitSemaphoresMType VkSubmitInfo = Ptr VkSemaphore

        {-# NOINLINE vkPWaitSemaphores #-}
        vkPWaitSemaphores x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, pWaitSemaphores})

        {-# INLINE vkPWaitSemaphoresByteOffset #-}
        vkPWaitSemaphoresByteOffset ~_
          = #{offset VkSubmitInfo, pWaitSemaphores}

        {-# INLINE readVkPWaitSemaphores #-}
        readVkPWaitSemaphores p
          = peekByteOff p #{offset VkSubmitInfo, pWaitSemaphores}

        {-# INLINE writeVkPWaitSemaphores #-}
        writeVkPWaitSemaphores p
          = pokeByteOff p #{offset VkSubmitInfo, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphores" VkSubmitInfo where
        type FieldType "pWaitSemaphores" VkSubmitInfo = Ptr VkSemaphore
        type FieldOptional "pWaitSemaphores" VkSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphores" VkSubmitInfo =
             #{offset VkSubmitInfo, pWaitSemaphores}
        type FieldIsArray "pWaitSemaphores" VkSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubmitInfo, pWaitSemaphores}

instance CanReadField "pWaitSemaphores" VkSubmitInfo where
        {-# INLINE getField #-}
        getField = vkPWaitSemaphores

        {-# INLINE readField #-}
        readField = readVkPWaitSemaphores

instance CanWriteField "pWaitSemaphores" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPWaitSemaphores

instance {-# OVERLAPPING #-} HasVkPWaitDstStageMask VkSubmitInfo
         where
        type VkPWaitDstStageMaskMType VkSubmitInfo =
             Ptr VkPipelineStageFlags

        {-# NOINLINE vkPWaitDstStageMask #-}
        vkPWaitDstStageMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, pWaitDstStageMask})

        {-# INLINE vkPWaitDstStageMaskByteOffset #-}
        vkPWaitDstStageMaskByteOffset ~_
          = #{offset VkSubmitInfo, pWaitDstStageMask}

        {-# INLINE readVkPWaitDstStageMask #-}
        readVkPWaitDstStageMask p
          = peekByteOff p #{offset VkSubmitInfo, pWaitDstStageMask}

        {-# INLINE writeVkPWaitDstStageMask #-}
        writeVkPWaitDstStageMask p
          = pokeByteOff p #{offset VkSubmitInfo, pWaitDstStageMask}

instance {-# OVERLAPPING #-}
         HasField "pWaitDstStageMask" VkSubmitInfo where
        type FieldType "pWaitDstStageMask" VkSubmitInfo =
             Ptr VkPipelineStageFlags
        type FieldOptional "pWaitDstStageMask" VkSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pWaitDstStageMask" VkSubmitInfo =
             #{offset VkSubmitInfo, pWaitDstStageMask}
        type FieldIsArray "pWaitDstStageMask" VkSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubmitInfo, pWaitDstStageMask}

instance CanReadField "pWaitDstStageMask" VkSubmitInfo where
        {-# INLINE getField #-}
        getField = vkPWaitDstStageMask

        {-# INLINE readField #-}
        readField = readVkPWaitDstStageMask

instance CanWriteField "pWaitDstStageMask" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPWaitDstStageMask

instance {-# OVERLAPPING #-} HasVkCommandBufferCount VkSubmitInfo
         where
        type VkCommandBufferCountMType VkSubmitInfo = Word32

        {-# NOINLINE vkCommandBufferCount #-}
        vkCommandBufferCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, commandBufferCount})

        {-# INLINE vkCommandBufferCountByteOffset #-}
        vkCommandBufferCountByteOffset ~_
          = #{offset VkSubmitInfo, commandBufferCount}

        {-# INLINE readVkCommandBufferCount #-}
        readVkCommandBufferCount p
          = peekByteOff p #{offset VkSubmitInfo, commandBufferCount}

        {-# INLINE writeVkCommandBufferCount #-}
        writeVkCommandBufferCount p
          = pokeByteOff p #{offset VkSubmitInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         HasField "commandBufferCount" VkSubmitInfo where
        type FieldType "commandBufferCount" VkSubmitInfo = Word32
        type FieldOptional "commandBufferCount" VkSubmitInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "commandBufferCount" VkSubmitInfo =
             #{offset VkSubmitInfo, commandBufferCount}
        type FieldIsArray "commandBufferCount" VkSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubmitInfo, commandBufferCount}

instance CanReadField "commandBufferCount" VkSubmitInfo where
        {-# INLINE getField #-}
        getField = vkCommandBufferCount

        {-# INLINE readField #-}
        readField = readVkCommandBufferCount

instance CanWriteField "commandBufferCount" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField = writeVkCommandBufferCount

instance {-# OVERLAPPING #-} HasVkPCommandBuffers VkSubmitInfo
         where
        type VkPCommandBuffersMType VkSubmitInfo = Ptr VkCommandBuffer

        {-# NOINLINE vkPCommandBuffers #-}
        vkPCommandBuffers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, pCommandBuffers})

        {-# INLINE vkPCommandBuffersByteOffset #-}
        vkPCommandBuffersByteOffset ~_
          = #{offset VkSubmitInfo, pCommandBuffers}

        {-# INLINE readVkPCommandBuffers #-}
        readVkPCommandBuffers p
          = peekByteOff p #{offset VkSubmitInfo, pCommandBuffers}

        {-# INLINE writeVkPCommandBuffers #-}
        writeVkPCommandBuffers p
          = pokeByteOff p #{offset VkSubmitInfo, pCommandBuffers}

instance {-# OVERLAPPING #-}
         HasField "pCommandBuffers" VkSubmitInfo where
        type FieldType "pCommandBuffers" VkSubmitInfo = Ptr VkCommandBuffer
        type FieldOptional "pCommandBuffers" VkSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pCommandBuffers" VkSubmitInfo =
             #{offset VkSubmitInfo, pCommandBuffers}
        type FieldIsArray "pCommandBuffers" VkSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubmitInfo, pCommandBuffers}

instance CanReadField "pCommandBuffers" VkSubmitInfo where
        {-# INLINE getField #-}
        getField = vkPCommandBuffers

        {-# INLINE readField #-}
        readField = readVkPCommandBuffers

instance CanWriteField "pCommandBuffers" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPCommandBuffers

instance {-# OVERLAPPING #-} HasVkSignalSemaphoreCount VkSubmitInfo
         where
        type VkSignalSemaphoreCountMType VkSubmitInfo = Word32

        {-# NOINLINE vkSignalSemaphoreCount #-}
        vkSignalSemaphoreCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, signalSemaphoreCount})

        {-# INLINE vkSignalSemaphoreCountByteOffset #-}
        vkSignalSemaphoreCountByteOffset ~_
          = #{offset VkSubmitInfo, signalSemaphoreCount}

        {-# INLINE readVkSignalSemaphoreCount #-}
        readVkSignalSemaphoreCount p
          = peekByteOff p #{offset VkSubmitInfo, signalSemaphoreCount}

        {-# INLINE writeVkSignalSemaphoreCount #-}
        writeVkSignalSemaphoreCount p
          = pokeByteOff p #{offset VkSubmitInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "signalSemaphoreCount" VkSubmitInfo where
        type FieldType "signalSemaphoreCount" VkSubmitInfo = Word32
        type FieldOptional "signalSemaphoreCount" VkSubmitInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "signalSemaphoreCount" VkSubmitInfo =
             #{offset VkSubmitInfo, signalSemaphoreCount}
        type FieldIsArray "signalSemaphoreCount" VkSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubmitInfo, signalSemaphoreCount}

instance CanReadField "signalSemaphoreCount" VkSubmitInfo where
        {-# INLINE getField #-}
        getField = vkSignalSemaphoreCount

        {-# INLINE readField #-}
        readField = readVkSignalSemaphoreCount

instance CanWriteField "signalSemaphoreCount" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSignalSemaphoreCount

instance {-# OVERLAPPING #-} HasVkPSignalSemaphores VkSubmitInfo
         where
        type VkPSignalSemaphoresMType VkSubmitInfo = Ptr VkSemaphore

        {-# NOINLINE vkPSignalSemaphores #-}
        vkPSignalSemaphores x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, pSignalSemaphores})

        {-# INLINE vkPSignalSemaphoresByteOffset #-}
        vkPSignalSemaphoresByteOffset ~_
          = #{offset VkSubmitInfo, pSignalSemaphores}

        {-# INLINE readVkPSignalSemaphores #-}
        readVkPSignalSemaphores p
          = peekByteOff p #{offset VkSubmitInfo, pSignalSemaphores}

        {-# INLINE writeVkPSignalSemaphores #-}
        writeVkPSignalSemaphores p
          = pokeByteOff p #{offset VkSubmitInfo, pSignalSemaphores}

instance {-# OVERLAPPING #-}
         HasField "pSignalSemaphores" VkSubmitInfo where
        type FieldType "pSignalSemaphores" VkSubmitInfo = Ptr VkSemaphore
        type FieldOptional "pSignalSemaphores" VkSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSignalSemaphores" VkSubmitInfo =
             #{offset VkSubmitInfo, pSignalSemaphores}
        type FieldIsArray "pSignalSemaphores" VkSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubmitInfo, pSignalSemaphores}

instance CanReadField "pSignalSemaphores" VkSubmitInfo where
        {-# INLINE getField #-}
        getField = vkPSignalSemaphores

        {-# INLINE readField #-}
        readField = readVkPSignalSemaphores

instance CanWriteField "pSignalSemaphores" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPSignalSemaphores

instance Show VkSubmitInfo where
        showsPrec d x
          = showString "VkSubmitInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkWaitSemaphoreCount = " .
                            showsPrec d (vkWaitSemaphoreCount x) .
                              showString ", " .
                                showString "vkPWaitSemaphores = " .
                                  showsPrec d (vkPWaitSemaphores x) .
                                    showString ", " .
                                      showString "vkPWaitDstStageMask = " .
                                        showsPrec d (vkPWaitDstStageMask x) .
                                          showString ", " .
                                            showString "vkCommandBufferCount = " .
                                              showsPrec d (vkCommandBufferCount x) .
                                                showString ", " .
                                                  showString "vkPCommandBuffers = " .
                                                    showsPrec d (vkPCommandBuffers x) .
                                                      showString ", " .
                                                        showString "vkSignalSemaphoreCount = " .
                                                          showsPrec d (vkSignalSemaphoreCount x) .
                                                            showString ", " .
                                                              showString "vkPSignalSemaphores = " .
                                                                showsPrec d (vkPSignalSemaphores x)
                                                                  . showChar '}'
