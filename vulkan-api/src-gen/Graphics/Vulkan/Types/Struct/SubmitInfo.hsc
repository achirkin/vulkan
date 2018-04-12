#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SubmitInfo (VkSubmitInfo(..))
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Pipeline      (VkPipelineStageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkCommandBuffer,
                                                           VkSemaphore)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubmitInfo VkSubmitInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} CanReadField "sType" VkSubmitInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubmitInfo, sType}

instance {-# OVERLAPPING #-} CanWriteField "sType" VkSubmitInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubmitInfo, sType}

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

instance {-# OVERLAPPING #-} CanReadField "pNext" VkSubmitInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubmitInfo, pNext}

instance {-# OVERLAPPING #-} CanWriteField "pNext" VkSubmitInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubmitInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "waitSemaphoreCount" VkSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, waitSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubmitInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "waitSemaphoreCount" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubmitInfo, waitSemaphoreCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pWaitSemaphores" VkSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, pWaitSemaphores})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubmitInfo, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         CanWriteField "pWaitSemaphores" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubmitInfo, pWaitSemaphores}

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

instance {-# OVERLAPPING #-}
         CanReadField "pWaitDstStageMask" VkSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, pWaitDstStageMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubmitInfo, pWaitDstStageMask}

instance {-# OVERLAPPING #-}
         CanWriteField "pWaitDstStageMask" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubmitInfo, pWaitDstStageMask}

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

instance {-# OVERLAPPING #-}
         CanReadField "commandBufferCount" VkSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, commandBufferCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubmitInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         CanWriteField "commandBufferCount" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubmitInfo, commandBufferCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pCommandBuffers" VkSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, pCommandBuffers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubmitInfo, pCommandBuffers}

instance {-# OVERLAPPING #-}
         CanWriteField "pCommandBuffers" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubmitInfo, pCommandBuffers}

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

instance {-# OVERLAPPING #-}
         CanReadField "signalSemaphoreCount" VkSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, signalSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubmitInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "signalSemaphoreCount" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubmitInfo, signalSemaphoreCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pSignalSemaphores" VkSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubmitInfo, pSignalSemaphores})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubmitInfo, pSignalSemaphores}

instance {-# OVERLAPPING #-}
         CanWriteField "pSignalSemaphores" VkSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubmitInfo, pSignalSemaphores}

instance Show VkSubmitInfo where
        showsPrec d x
          = showString "VkSubmitInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "waitSemaphoreCount = " .
                            showsPrec d (getField @"waitSemaphoreCount" x) .
                              showString ", " .
                                showString "pWaitSemaphores = " .
                                  showsPrec d (getField @"pWaitSemaphores" x) .
                                    showString ", " .
                                      showString "pWaitDstStageMask = " .
                                        showsPrec d (getField @"pWaitDstStageMask" x) .
                                          showString ", " .
                                            showString "commandBufferCount = " .
                                              showsPrec d (getField @"commandBufferCount" x) .
                                                showString ", " .
                                                  showString "pCommandBuffers = " .
                                                    showsPrec d (getField @"pCommandBuffers" x) .
                                                      showString ", " .
                                                        showString "signalSemaphoreCount = " .
                                                          showsPrec d
                                                            (getField @"signalSemaphoreCount" x)
                                                            .
                                                            showString ", " .
                                                              showString "pSignalSemaphores = " .
                                                                showsPrec d
                                                                  (getField @"pSignalSemaphores" x)
                                                                  . showChar '}'
