#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupSubmitInfoKHX
       (VkDeviceGroupSubmitInfoKHX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo  (VkSubmitInfo)
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupSubmitInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupSubmitInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pWaitSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pWaitSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}

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

instance {-# OVERLAPPING #-}
         CanReadField "commandBufferCount" VkDeviceGroupSubmitInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}

instance {-# OVERLAPPING #-}
         CanWriteField "commandBufferCount" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pCommandBufferDeviceMasks"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}

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

instance {-# OVERLAPPING #-}
         CanReadField "signalSemaphoreCount" VkDeviceGroupSubmitInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "signalSemaphoreCount" VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pSignalSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pSignalSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}

instance Show VkDeviceGroupSubmitInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupSubmitInfoKHX {" .
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
