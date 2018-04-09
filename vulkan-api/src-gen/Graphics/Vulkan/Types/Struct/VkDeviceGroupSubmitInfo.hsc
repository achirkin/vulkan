#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupSubmitInfo
       (VkDeviceGroupSubmitInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo  (VkSubmitInfo)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupSubmitInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         waitSemaphoreCount;
--   >     const uint32_t*    pWaitSemaphoreDeviceIndices;
--   >     uint32_t         commandBufferCount;
--   >     const uint32_t*    pCommandBufferDeviceMasks;
--   >     uint32_t         signalSemaphoreCount;
--   >     const uint32_t*  pSignalSemaphoreDeviceIndices;
--   > } VkDeviceGroupSubmitInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDeviceGroupSubmitInfoVkDeviceGroupSubmitInfo registry at www.khronos.org>
data VkDeviceGroupSubmitInfo = VkDeviceGroupSubmitInfo## Addr##
                                                        ByteArray##

instance Eq VkDeviceGroupSubmitInfo where
        (VkDeviceGroupSubmitInfo## a _) == x@(VkDeviceGroupSubmitInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupSubmitInfo where
        (VkDeviceGroupSubmitInfo## a _) `compare`
          x@(VkDeviceGroupSubmitInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupSubmitInfo where
        sizeOf ~_ = #{size VkDeviceGroupSubmitInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupSubmitInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupSubmitInfo where
        unsafeAddr (VkDeviceGroupSubmitInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupSubmitInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupSubmitInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupSubmitInfo where
        type StructFields VkDeviceGroupSubmitInfo =
             '["sType", "pNext", "waitSemaphoreCount", -- ' closing tick for hsc2hs
               "pWaitSemaphoreDeviceIndices", "commandBufferCount",
               "pCommandBufferDeviceMasks", "signalSemaphoreCount",
               "pSignalSemaphoreDeviceIndices"]
        type CUnionType VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupSubmitInfo = '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupSubmitInfo where
        type FieldType "sType" VkDeviceGroupSubmitInfo = VkStructureType
        type FieldOptional "sType" VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupSubmitInfo =
             #{offset VkDeviceGroupSubmitInfo, sType}
        type FieldIsArray "sType" VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceGroupSubmitInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupSubmitInfo where
        type FieldType "pNext" VkDeviceGroupSubmitInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupSubmitInfo =
             #{offset VkDeviceGroupSubmitInfo, pNext}
        type FieldIsArray "pNext" VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceGroupSubmitInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreCount" VkDeviceGroupSubmitInfo where
        type FieldType "waitSemaphoreCount" VkDeviceGroupSubmitInfo =
             Word32
        type FieldOptional "waitSemaphoreCount" VkDeviceGroupSubmitInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreCount" VkDeviceGroupSubmitInfo =
             #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount}
        type FieldIsArray "waitSemaphoreCount" VkDeviceGroupSubmitInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanReadField "waitSemaphoreCount" VkDeviceGroupSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "waitSemaphoreCount" VkDeviceGroupSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphoreDeviceIndices" VkDeviceGroupSubmitInfo
         where
        type FieldType "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = Ptr Word32
        type FieldOptional "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             =
             #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices}
        type FieldIsArray "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pWaitSemaphoreDeviceIndices" VkDeviceGroupSubmitInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pWaitSemaphoreDeviceIndices" VkDeviceGroupSubmitInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "commandBufferCount" VkDeviceGroupSubmitInfo where
        type FieldType "commandBufferCount" VkDeviceGroupSubmitInfo =
             Word32
        type FieldOptional "commandBufferCount" VkDeviceGroupSubmitInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "commandBufferCount" VkDeviceGroupSubmitInfo =
             #{offset VkDeviceGroupSubmitInfo, commandBufferCount}
        type FieldIsArray "commandBufferCount" VkDeviceGroupSubmitInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         CanReadField "commandBufferCount" VkDeviceGroupSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, commandBufferCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         CanWriteField "commandBufferCount" VkDeviceGroupSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         HasField "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfo where
        type FieldType "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfo
             = Ptr Word32
        type FieldOptional "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfo
             =
             #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks}
        type FieldIsArray "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks}

instance {-# OVERLAPPING #-}
         HasField "signalSemaphoreCount" VkDeviceGroupSubmitInfo where
        type FieldType "signalSemaphoreCount" VkDeviceGroupSubmitInfo =
             Word32
        type FieldOptional "signalSemaphoreCount" VkDeviceGroupSubmitInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "signalSemaphoreCount" VkDeviceGroupSubmitInfo =
             #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount}
        type FieldIsArray "signalSemaphoreCount" VkDeviceGroupSubmitInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanReadField "signalSemaphoreCount" VkDeviceGroupSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "signalSemaphoreCount" VkDeviceGroupSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "pSignalSemaphoreDeviceIndices" VkDeviceGroupSubmitInfo
         where
        type FieldType "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = Ptr Word32
        type FieldOptional "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             =
             #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices}
        type FieldIsArray "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pSignalSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pSignalSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices}

instance Show VkDeviceGroupSubmitInfo where
        showsPrec d x
          = showString "VkDeviceGroupSubmitInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "waitSemaphoreCount = " .
                            showsPrec d (getField @"waitSemaphoreCount" x) .
                              showString ", " .
                                showString "pWaitSemaphoreDeviceIndices = " .
                                  showsPrec d (getField @"pWaitSemaphoreDeviceIndices" x) .
                                    showString ", " .
                                      showString "commandBufferCount = " .
                                        showsPrec d (getField @"commandBufferCount" x) .
                                          showString ", " .
                                            showString "pCommandBufferDeviceMasks = " .
                                              showsPrec d (getField @"pCommandBufferDeviceMasks" x)
                                                .
                                                showString ", " .
                                                  showString "signalSemaphoreCount = " .
                                                    showsPrec d (getField @"signalSemaphoreCount" x)
                                                      .
                                                      showString ", " .
                                                        showString
                                                          "pSignalSemaphoreDeviceIndices = "
                                                          .
                                                          showsPrec d
                                                            (getField
                                                               @"pSignalSemaphoreDeviceIndices"
                                                               x)
                                                            . showChar '}'
