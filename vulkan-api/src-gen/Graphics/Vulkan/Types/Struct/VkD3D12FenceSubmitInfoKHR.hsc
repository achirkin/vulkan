#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkD3D12FenceSubmitInfoKHR
       (VkD3D12FenceSubmitInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo  (VkSubmitInfo)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkD3D12FenceSubmitInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         waitSemaphoreValuesCount;
--   >     const uint64_t* pWaitSemaphoreValues;
--   >     uint32_t         signalSemaphoreValuesCount;
--   >     const uint64_t* pSignalSemaphoreValues;
--   > } VkD3D12FenceSubmitInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkD3D12FenceSubmitInfoKHRVkD3D12FenceSubmitInfoKHR registry at www.khronos.org>
data VkD3D12FenceSubmitInfoKHR = VkD3D12FenceSubmitInfoKHR## Addr##
                                                            ByteArray##

instance Eq VkD3D12FenceSubmitInfoKHR where
        (VkD3D12FenceSubmitInfoKHR## a _) ==
          x@(VkD3D12FenceSubmitInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkD3D12FenceSubmitInfoKHR where
        (VkD3D12FenceSubmitInfoKHR## a _) `compare`
          x@(VkD3D12FenceSubmitInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkD3D12FenceSubmitInfoKHR where
        sizeOf ~_ = #{size VkD3D12FenceSubmitInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkD3D12FenceSubmitInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkD3D12FenceSubmitInfoKHR where
        unsafeAddr (VkD3D12FenceSubmitInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkD3D12FenceSubmitInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkD3D12FenceSubmitInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkD3D12FenceSubmitInfoKHR where
        type StructFields VkD3D12FenceSubmitInfoKHR =
             '["sType", "pNext", "waitSemaphoreValuesCount", -- ' closing tick for hsc2hs
               "pWaitSemaphoreValues", "signalSemaphoreValuesCount",
               "pSignalSemaphoreValues"]
        type CUnionType VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkD3D12FenceSubmitInfoKHR = '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkD3D12FenceSubmitInfoKHR where
        type FieldType "sType" VkD3D12FenceSubmitInfoKHR = VkStructureType
        type FieldOptional "sType" VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkD3D12FenceSubmitInfoKHR =
             #{offset VkD3D12FenceSubmitInfoKHR, sType}
        type FieldIsArray "sType" VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkD3D12FenceSubmitInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkD3D12FenceSubmitInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkD3D12FenceSubmitInfoKHR where
        type FieldType "pNext" VkD3D12FenceSubmitInfoKHR = Ptr Void
        type FieldOptional "pNext" VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkD3D12FenceSubmitInfoKHR =
             #{offset VkD3D12FenceSubmitInfoKHR, pNext}
        type FieldIsArray "pNext" VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkD3D12FenceSubmitInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkD3D12FenceSubmitInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreValuesCount" VkD3D12FenceSubmitInfoKHR where
        type FieldType "waitSemaphoreValuesCount" VkD3D12FenceSubmitInfoKHR
             = Word32
        type FieldOptional "waitSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             =
             #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}
        type FieldIsArray "waitSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}

instance {-# OVERLAPPING #-}
         CanReadField "waitSemaphoreValuesCount" VkD3D12FenceSubmitInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}

instance {-# OVERLAPPING #-}
         CanWriteField "waitSemaphoreValuesCount" VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR where
        type FieldType "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR =
             Ptr Word64
        type FieldOptional "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR =
             #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}
        type FieldIsArray "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}

instance {-# OVERLAPPING #-}
         CanReadField "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}

instance {-# OVERLAPPING #-}
         CanWriteField "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}

instance {-# OVERLAPPING #-}
         HasField "signalSemaphoreValuesCount" VkD3D12FenceSubmitInfoKHR
         where
        type FieldType "signalSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             = Word32
        type FieldOptional "signalSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "signalSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             =
             #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}
        type FieldIsArray "signalSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}

instance {-# OVERLAPPING #-}
         CanReadField "signalSemaphoreValuesCount" VkD3D12FenceSubmitInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}

instance {-# OVERLAPPING #-}
         CanWriteField "signalSemaphoreValuesCount"
           VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}

instance {-# OVERLAPPING #-}
         HasField "pSignalSemaphoreValues" VkD3D12FenceSubmitInfoKHR where
        type FieldType "pSignalSemaphoreValues" VkD3D12FenceSubmitInfoKHR =
             Ptr Word64
        type FieldOptional "pSignalSemaphoreValues"
               VkD3D12FenceSubmitInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pSignalSemaphoreValues" VkD3D12FenceSubmitInfoKHR
             =
             #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}
        type FieldIsArray "pSignalSemaphoreValues"
               VkD3D12FenceSubmitInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}

instance {-# OVERLAPPING #-}
         CanReadField "pSignalSemaphoreValues" VkD3D12FenceSubmitInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}

instance {-# OVERLAPPING #-}
         CanWriteField "pSignalSemaphoreValues" VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}

instance Show VkD3D12FenceSubmitInfoKHR where
        showsPrec d x
          = showString "VkD3D12FenceSubmitInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "waitSemaphoreValuesCount = " .
                            showsPrec d (getField @"waitSemaphoreValuesCount" x) .
                              showString ", " .
                                showString "pWaitSemaphoreValues = " .
                                  showsPrec d (getField @"pWaitSemaphoreValues" x) .
                                    showString ", " .
                                      showString "signalSemaphoreValuesCount = " .
                                        showsPrec d (getField @"signalSemaphoreValuesCount" x) .
                                          showString ", " .
                                            showString "pSignalSemaphoreValues = " .
                                              showsPrec d (getField @"pSignalSemaphoreValues" x) .
                                                showChar '}'
