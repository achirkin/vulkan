#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformWin32Khr
       (VkD3D12FenceSubmitInfoKHR(..),
        VkExportFenceWin32HandleInfoKHR(..),
        VkExportMemoryWin32HandleInfoKHR(..),
        VkExportMemoryWin32HandleInfoNV(..),
        VkExportSemaphoreWin32HandleInfoKHR(..),
        VkFenceGetWin32HandleInfoKHR(..),
        VkImportFenceWin32HandleInfoKHR(..),
        VkImportMemoryWin32HandleInfoKHR(..),
        VkImportMemoryWin32HandleInfoNV(..),
        VkImportSemaphoreWin32HandleInfoKHR(..),
        VkMemoryGetWin32HandleInfoKHR(..),
        VkMemoryWin32HandlePropertiesKHR(..),
        VkSemaphoreGetWin32HandleInfoKHR(..),
        VkWin32KeyedMutexAcquireReleaseInfoKHR(..),
        VkWin32KeyedMutexAcquireReleaseInfoNV(..),
        VkWin32SurfaceCreateInfoKHR(..))
       where
import           Foreign.Storable                               (Storable (..))
import           GHC.Base                                       (Addr##,
                                                                 ByteArray##,
                                                                 byteArrayContents##,
                                                                 plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                 (VkWin32SurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.External            (VkExternalFenceHandleTypeFlagBits,
                                                                 VkExternalMemoryHandleTypeFlagBits,
                                                                 VkExternalMemoryHandleTypeFlagsNV,
                                                                 VkExternalSemaphoreHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.Fence               (VkFenceImportFlags)
import           Graphics.Vulkan.Types.Enum.SemaphoreImportFlag (VkSemaphoreImportFlags)
import           Graphics.Vulkan.Types.Enum.StructureType       (VkStructureType)
import           Graphics.Vulkan.Types.Handles                  (VkDeviceMemory,
                                                                 VkFence,
                                                                 VkSemaphore)
import           Graphics.Vulkan.Types.Include                  (DWORD, HANDLE,
                                                                 HINSTANCE,
                                                                 HWND, LPCWSTR,
                                                                 SECURITY_ATTRIBUTES)
import           Graphics.Vulkan.Types.Struct.Fence             (VkFenceCreateInfo)
import           Graphics.Vulkan.Types.Struct.Memory            (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.Struct.Semaphore         (VkSemaphoreCreateInfo)
import           Graphics.Vulkan.Types.Struct.SubmitInfo        (VkSubmitInfo)
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

-- | > typedef struct VkD3D12FenceSubmitInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         waitSemaphoreValuesCount;
--   >     const uint64_t* pWaitSemaphoreValues;
--   >     uint32_t         signalSemaphoreValuesCount;
--   >     const uint64_t* pSignalSemaphoreValues;
--   > } VkD3D12FenceSubmitInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkD3D12FenceSubmitInfoKHR VkD3D12FenceSubmitInfoKHR registry at www.khronos.org>
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

-- | > typedef struct VkExportFenceWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                pNext;
--   >     const SECURITY_ATTRIBUTES* pAttributes;
--   >     DWORD                                      dwAccess;
--   >     LPCWSTR                                    name;
--   > } VkExportFenceWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportFenceWin32HandleInfoKHR VkExportFenceWin32HandleInfoKHR registry at www.khronos.org>
data VkExportFenceWin32HandleInfoKHR = VkExportFenceWin32HandleInfoKHR## Addr##
                                                                        ByteArray##

instance Eq VkExportFenceWin32HandleInfoKHR where
        (VkExportFenceWin32HandleInfoKHR## a _) ==
          x@(VkExportFenceWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportFenceWin32HandleInfoKHR where
        (VkExportFenceWin32HandleInfoKHR## a _) `compare`
          x@(VkExportFenceWin32HandleInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportFenceWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkExportFenceWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportFenceWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportFenceWin32HandleInfoKHR where
        unsafeAddr (VkExportFenceWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportFenceWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportFenceWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportFenceWin32HandleInfoKHR where
        type StructFields VkExportFenceWin32HandleInfoKHR =
             '["sType", "pNext", "pAttributes", "dwAccess", "name"] -- ' closing tick for hsc2hs
        type CUnionType VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportFenceWin32HandleInfoKHR =
             '[VkFenceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportFenceWin32HandleInfoKHR where
        type FieldType "sType" VkExportFenceWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportFenceWin32HandleInfoKHR =
             #{offset VkExportFenceWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportFenceWin32HandleInfoKHR where
        type FieldType "pNext" VkExportFenceWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportFenceWin32HandleInfoKHR =
             #{offset VkExportFenceWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pAttributes" VkExportFenceWin32HandleInfoKHR where
        type FieldType "pAttributes" VkExportFenceWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES
        type FieldOptional "pAttributes" VkExportFenceWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pAttributes" VkExportFenceWin32HandleInfoKHR =
             #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}
        type FieldIsArray "pAttributes" VkExportFenceWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanReadField "pAttributes" VkExportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, pAttributes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttributes" VkExportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         HasField "dwAccess" VkExportFenceWin32HandleInfoKHR where
        type FieldType "dwAccess" VkExportFenceWin32HandleInfoKHR = DWORD
        type FieldOptional "dwAccess" VkExportFenceWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dwAccess" VkExportFenceWin32HandleInfoKHR =
             #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}
        type FieldIsArray "dwAccess" VkExportFenceWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanReadField "dwAccess" VkExportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, dwAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "dwAccess" VkExportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         HasField "name" VkExportFenceWin32HandleInfoKHR where
        type FieldType "name" VkExportFenceWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "name" VkExportFenceWin32HandleInfoKHR =
             #{offset VkExportFenceWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkExportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkExportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, name}

instance Show VkExportFenceWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkExportFenceWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pAttributes = " .
                            showsPrec d (getField @"pAttributes" x) .
                              showString ", " .
                                showString "dwAccess = " .
                                  showsPrec d (getField @"dwAccess" x) .
                                    showString ", " .
                                      showString "name = " .
                                        showsPrec d (getField @"name" x) . showChar '}'

-- | > typedef struct VkExportMemoryWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES* pAttributes;
--   >     DWORD                            dwAccess;
--   >     LPCWSTR                          name;
--   > } VkExportMemoryWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportMemoryWin32HandleInfoKHR VkExportMemoryWin32HandleInfoKHR registry at www.khronos.org>
data VkExportMemoryWin32HandleInfoKHR = VkExportMemoryWin32HandleInfoKHR## Addr##
                                                                          ByteArray##

instance Eq VkExportMemoryWin32HandleInfoKHR where
        (VkExportMemoryWin32HandleInfoKHR## a _) ==
          x@(VkExportMemoryWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryWin32HandleInfoKHR where
        (VkExportMemoryWin32HandleInfoKHR## a _) `compare`
          x@(VkExportMemoryWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkExportMemoryWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryWin32HandleInfoKHR where
        unsafeAddr (VkExportMemoryWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryWin32HandleInfoKHR where
        type StructFields VkExportMemoryWin32HandleInfoKHR =
             '["sType", "pNext", "pAttributes", "dwAccess", "name"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryWin32HandleInfoKHR =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "sType" VkExportMemoryWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "pNext" VkExportMemoryWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pAttributes" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES
        type FieldOptional "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}
        type FieldIsArray "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanReadField "pAttributes" VkExportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttributes" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         HasField "dwAccess" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "dwAccess" VkExportMemoryWin32HandleInfoKHR = DWORD
        type FieldOptional "dwAccess" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dwAccess" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}
        type FieldIsArray "dwAccess" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanReadField "dwAccess" VkExportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "dwAccess" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         HasField "name" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "name" VkExportMemoryWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "name" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkExportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, name}

instance Show VkExportMemoryWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkExportMemoryWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pAttributes = " .
                            showsPrec d (getField @"pAttributes" x) .
                              showString ", " .
                                showString "dwAccess = " .
                                  showsPrec d (getField @"dwAccess" x) .
                                    showString ", " .
                                      showString "name = " .
                                        showsPrec d (getField @"name" x) . showChar '}'

-- | > typedef struct VkExportMemoryWin32HandleInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES*       pAttributes;
--   >     DWORD                            dwAccess;
--   > } VkExportMemoryWin32HandleInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportMemoryWin32HandleInfoNV VkExportMemoryWin32HandleInfoNV registry at www.khronos.org>
data VkExportMemoryWin32HandleInfoNV = VkExportMemoryWin32HandleInfoNV## Addr##
                                                                        ByteArray##

instance Eq VkExportMemoryWin32HandleInfoNV where
        (VkExportMemoryWin32HandleInfoNV## a _) ==
          x@(VkExportMemoryWin32HandleInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryWin32HandleInfoNV where
        (VkExportMemoryWin32HandleInfoNV## a _) `compare`
          x@(VkExportMemoryWin32HandleInfoNV## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryWin32HandleInfoNV where
        sizeOf ~_ = #{size VkExportMemoryWin32HandleInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryWin32HandleInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryWin32HandleInfoNV where
        unsafeAddr (VkExportMemoryWin32HandleInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryWin32HandleInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryWin32HandleInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryWin32HandleInfoNV where
        type StructFields VkExportMemoryWin32HandleInfoNV =
             '["sType", "pNext", "pAttributes", "dwAccess"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryWin32HandleInfoNV =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryWin32HandleInfoNV where
        type FieldType "sType" VkExportMemoryWin32HandleInfoNV =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryWin32HandleInfoNV =
             #{offset VkExportMemoryWin32HandleInfoNV, sType}
        type FieldIsArray "sType" VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryWin32HandleInfoNV where
        type FieldType "pNext" VkExportMemoryWin32HandleInfoNV = Ptr Void
        type FieldOptional "pNext" VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryWin32HandleInfoNV =
             #{offset VkExportMemoryWin32HandleInfoNV, pNext}
        type FieldIsArray "pNext" VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pAttributes" VkExportMemoryWin32HandleInfoNV where
        type FieldType "pAttributes" VkExportMemoryWin32HandleInfoNV =
             Ptr SECURITY_ATTRIBUTES
        type FieldOptional "pAttributes" VkExportMemoryWin32HandleInfoNV =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pAttributes" VkExportMemoryWin32HandleInfoNV =
             #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}
        type FieldIsArray "pAttributes" VkExportMemoryWin32HandleInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}

instance {-# OVERLAPPING #-}
         CanReadField "pAttributes" VkExportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, pAttributes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttributes" VkExportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}

instance {-# OVERLAPPING #-}
         HasField "dwAccess" VkExportMemoryWin32HandleInfoNV where
        type FieldType "dwAccess" VkExportMemoryWin32HandleInfoNV = DWORD
        type FieldOptional "dwAccess" VkExportMemoryWin32HandleInfoNV =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "dwAccess" VkExportMemoryWin32HandleInfoNV =
             #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}
        type FieldIsArray "dwAccess" VkExportMemoryWin32HandleInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}

instance {-# OVERLAPPING #-}
         CanReadField "dwAccess" VkExportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, dwAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "dwAccess" VkExportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}

instance Show VkExportMemoryWin32HandleInfoNV where
        showsPrec d x
          = showString "VkExportMemoryWin32HandleInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pAttributes = " .
                            showsPrec d (getField @"pAttributes" x) .
                              showString ", " .
                                showString "dwAccess = " .
                                  showsPrec d (getField @"dwAccess" x) . showChar '}'

-- | > typedef struct VkExportSemaphoreWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES*       pAttributes;
--   >     DWORD                            dwAccess;
--   >     LPCWSTR                          name;
--   > } VkExportSemaphoreWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportSemaphoreWin32HandleInfoKHR VkExportSemaphoreWin32HandleInfoKHR registry at www.khronos.org>
data VkExportSemaphoreWin32HandleInfoKHR = VkExportSemaphoreWin32HandleInfoKHR## Addr##
                                                                                ByteArray##

instance Eq VkExportSemaphoreWin32HandleInfoKHR where
        (VkExportSemaphoreWin32HandleInfoKHR## a _) ==
          x@(VkExportSemaphoreWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportSemaphoreWin32HandleInfoKHR where
        (VkExportSemaphoreWin32HandleInfoKHR## a _) `compare`
          x@(VkExportSemaphoreWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportSemaphoreWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkExportSemaphoreWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportSemaphoreWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportSemaphoreWin32HandleInfoKHR
         where
        unsafeAddr (VkExportSemaphoreWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportSemaphoreWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportSemaphoreWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportSemaphoreWin32HandleInfoKHR where
        type StructFields VkExportSemaphoreWin32HandleInfoKHR =
             '["sType", "pNext", "pAttributes", "dwAccess", "name"] -- ' closing tick for hsc2hs
        type CUnionType VkExportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportSemaphoreWin32HandleInfoKHR =
             '[VkSemaphoreCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "sType" VkExportSemaphoreWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pAttributes" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "pAttributes" VkExportSemaphoreWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES
        type FieldOptional "pAttributes"
               VkExportSemaphoreWin32HandleInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pAttributes" VkExportSemaphoreWin32HandleInfoKHR
             =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}
        type FieldIsArray "pAttributes" VkExportSemaphoreWin32HandleInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanReadField "pAttributes" VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttributes" VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         HasField "dwAccess" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             DWORD
        type FieldOptional "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}
        type FieldIsArray "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanReadField "dwAccess" VkExportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "dwAccess" VkExportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         HasField "name" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "name" VkExportSemaphoreWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "name" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkExportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkExportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

instance Show VkExportSemaphoreWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkExportSemaphoreWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pAttributes = " .
                            showsPrec d (getField @"pAttributes" x) .
                              showString ", " .
                                showString "dwAccess = " .
                                  showsPrec d (getField @"dwAccess" x) .
                                    showString ", " .
                                      showString "name = " .
                                        showsPrec d (getField @"name" x) . showChar '}'

-- | > typedef struct VkFenceGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence                                fence;
--   >     VkExternalFenceHandleTypeFlagBits   handleType;
--   > } VkFenceGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFenceGetWin32HandleInfoKHR VkFenceGetWin32HandleInfoKHR registry at www.khronos.org>
data VkFenceGetWin32HandleInfoKHR = VkFenceGetWin32HandleInfoKHR## Addr##
                                                                  ByteArray##

instance Eq VkFenceGetWin32HandleInfoKHR where
        (VkFenceGetWin32HandleInfoKHR## a _) ==
          x@(VkFenceGetWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFenceGetWin32HandleInfoKHR where
        (VkFenceGetWin32HandleInfoKHR## a _) `compare`
          x@(VkFenceGetWin32HandleInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFenceGetWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkFenceGetWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkFenceGetWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFenceGetWin32HandleInfoKHR where
        unsafeAddr (VkFenceGetWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFenceGetWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFenceGetWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFenceGetWin32HandleInfoKHR where
        type StructFields VkFenceGetWin32HandleInfoKHR =
             '["sType", "pNext", "fence", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkFenceGetWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkFenceGetWin32HandleInfoKHR where
        type FieldType "sType" VkFenceGetWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkFenceGetWin32HandleInfoKHR =
             #{offset VkFenceGetWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFenceGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkFenceGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkFenceGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkFenceGetWin32HandleInfoKHR where
        type FieldType "pNext" VkFenceGetWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkFenceGetWin32HandleInfoKHR =
             #{offset VkFenceGetWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFenceGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkFenceGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkFenceGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "fence" VkFenceGetWin32HandleInfoKHR where
        type FieldType "fence" VkFenceGetWin32HandleInfoKHR = VkFence
        type FieldOptional "fence" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fence" VkFenceGetWin32HandleInfoKHR =
             #{offset VkFenceGetWin32HandleInfoKHR, fence}
        type FieldIsArray "fence" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFenceGetWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanReadField "fence" VkFenceGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, fence})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanWriteField "fence" VkFenceGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkFenceGetWin32HandleInfoKHR where
        type FieldType "handleType" VkFenceGetWin32HandleInfoKHR =
             VkExternalFenceHandleTypeFlagBits
        type FieldOptional "handleType" VkFenceGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkFenceGetWin32HandleInfoKHR =
             #{offset VkFenceGetWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkFenceGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFenceGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkFenceGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkFenceGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, handleType}

instance Show VkFenceGetWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkFenceGetWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "fence = " .
                            showsPrec d (getField @"fence" x) .
                              showString ", " .
                                showString "handleType = " .
                                  showsPrec d (getField @"handleType" x) . showChar '}'

-- | > typedef struct VkImportFenceWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                        pNext;
--   >     VkFence                          fence;
--   >     VkFenceImportFlags              flags;
--   >     VkExternalFenceHandleTypeFlagBits  handleType;
--   >     HANDLE                             handle;
--   >     LPCWSTR                            name;
--   > } VkImportFenceWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportFenceWin32HandleInfoKHR VkImportFenceWin32HandleInfoKHR registry at www.khronos.org>
data VkImportFenceWin32HandleInfoKHR = VkImportFenceWin32HandleInfoKHR## Addr##
                                                                        ByteArray##

instance Eq VkImportFenceWin32HandleInfoKHR where
        (VkImportFenceWin32HandleInfoKHR## a _) ==
          x@(VkImportFenceWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportFenceWin32HandleInfoKHR where
        (VkImportFenceWin32HandleInfoKHR## a _) `compare`
          x@(VkImportFenceWin32HandleInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportFenceWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkImportFenceWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportFenceWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportFenceWin32HandleInfoKHR where
        unsafeAddr (VkImportFenceWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportFenceWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportFenceWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportFenceWin32HandleInfoKHR where
        type StructFields VkImportFenceWin32HandleInfoKHR =
             '["sType", "pNext", "fence", "flags", "handleType", "handle", -- ' closing tick for hsc2hs
               "name"]
        type CUnionType VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportFenceWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportFenceWin32HandleInfoKHR where
        type FieldType "sType" VkImportFenceWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportFenceWin32HandleInfoKHR where
        type FieldType "pNext" VkImportFenceWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "fence" VkImportFenceWin32HandleInfoKHR where
        type FieldType "fence" VkImportFenceWin32HandleInfoKHR = VkFence
        type FieldOptional "fence" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fence" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, fence}
        type FieldIsArray "fence" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanReadField "fence" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, fence})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanWriteField "fence" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasField "flags" VkImportFenceWin32HandleInfoKHR where
        type FieldType "flags" VkImportFenceWin32HandleInfoKHR =
             VkFenceImportFlags
        type FieldOptional "flags" VkImportFenceWin32HandleInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, flags}
        type FieldIsArray "flags" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportFenceWin32HandleInfoKHR where
        type FieldType "handleType" VkImportFenceWin32HandleInfoKHR =
             VkExternalFenceHandleTypeFlagBits
        type FieldOptional "handleType" VkImportFenceWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportFenceWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handle" VkImportFenceWin32HandleInfoKHR where
        type FieldType "handle" VkImportFenceWin32HandleInfoKHR = HANDLE
        type FieldOptional "handle" VkImportFenceWin32HandleInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, handle}
        type FieldIsArray "handle" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanReadField "handle" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, handle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanWriteField "handle" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         HasField "name" VkImportFenceWin32HandleInfoKHR where
        type FieldType "name" VkImportFenceWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkImportFenceWin32HandleInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "name" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, name}

instance Show VkImportFenceWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkImportFenceWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "fence = " .
                            showsPrec d (getField @"fence" x) .
                              showString ", " .
                                showString "flags = " .
                                  showsPrec d (getField @"flags" x) .
                                    showString ", " .
                                      showString "handleType = " .
                                        showsPrec d (getField @"handleType" x) .
                                          showString ", " .
                                            showString "handle = " .
                                              showsPrec d (getField @"handle" x) .
                                                showString ", " .
                                                  showString "name = " .
                                                    showsPrec d (getField @"name" x) . showChar '}'

-- | > typedef struct VkImportMemoryWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   >     HANDLE           handle;
--   >     LPCWSTR          name;
--   > } VkImportMemoryWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportMemoryWin32HandleInfoKHR VkImportMemoryWin32HandleInfoKHR registry at www.khronos.org>
data VkImportMemoryWin32HandleInfoKHR = VkImportMemoryWin32HandleInfoKHR## Addr##
                                                                          ByteArray##

instance Eq VkImportMemoryWin32HandleInfoKHR where
        (VkImportMemoryWin32HandleInfoKHR## a _) ==
          x@(VkImportMemoryWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryWin32HandleInfoKHR where
        (VkImportMemoryWin32HandleInfoKHR## a _) `compare`
          x@(VkImportMemoryWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkImportMemoryWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportMemoryWin32HandleInfoKHR where
        unsafeAddr (VkImportMemoryWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportMemoryWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportMemoryWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportMemoryWin32HandleInfoKHR where
        type StructFields VkImportMemoryWin32HandleInfoKHR =
             '["sType", "pNext", "handleType", "handle", "name"] -- ' closing tick for hsc2hs
        type CUnionType VkImportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportMemoryWin32HandleInfoKHR =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "sType" VkImportMemoryWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkImportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "pNext" VkImportMemoryWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "handleType" VkImportMemoryWin32HandleInfoKHR =
             VkExternalMemoryHandleTypeFlagBits
        type FieldOptional "handleType" VkImportMemoryWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handle" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "handle" VkImportMemoryWin32HandleInfoKHR = HANDLE
        type FieldOptional "handle" VkImportMemoryWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, handle}
        type FieldIsArray "handle" VkImportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanReadField "handle" VkImportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, handle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanWriteField "handle" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         HasField "name" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "name" VkImportMemoryWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkImportMemoryWin32HandleInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "name" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkImportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkImportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, name}

instance Show VkImportMemoryWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkImportMemoryWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) .
                              showString ", " .
                                showString "handle = " .
                                  showsPrec d (getField @"handle" x) .
                                    showString ", " .
                                      showString "name = " .
                                        showsPrec d (getField @"name" x) . showChar '}'

-- | > typedef struct VkImportMemoryWin32HandleInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleType;
--   >     HANDLE                           handle;
--   > } VkImportMemoryWin32HandleInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportMemoryWin32HandleInfoNV VkImportMemoryWin32HandleInfoNV registry at www.khronos.org>
data VkImportMemoryWin32HandleInfoNV = VkImportMemoryWin32HandleInfoNV## Addr##
                                                                        ByteArray##

instance Eq VkImportMemoryWin32HandleInfoNV where
        (VkImportMemoryWin32HandleInfoNV## a _) ==
          x@(VkImportMemoryWin32HandleInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryWin32HandleInfoNV where
        (VkImportMemoryWin32HandleInfoNV## a _) `compare`
          x@(VkImportMemoryWin32HandleInfoNV## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryWin32HandleInfoNV where
        sizeOf ~_ = #{size VkImportMemoryWin32HandleInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryWin32HandleInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportMemoryWin32HandleInfoNV where
        unsafeAddr (VkImportMemoryWin32HandleInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportMemoryWin32HandleInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportMemoryWin32HandleInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportMemoryWin32HandleInfoNV where
        type StructFields VkImportMemoryWin32HandleInfoNV =
             '["sType", "pNext", "handleType", "handle"] -- ' closing tick for hsc2hs
        type CUnionType VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportMemoryWin32HandleInfoNV =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryWin32HandleInfoNV where
        type FieldType "sType" VkImportMemoryWin32HandleInfoNV =
             VkStructureType
        type FieldOptional "sType" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, sType}
        type FieldIsArray "sType" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryWin32HandleInfoNV where
        type FieldType "pNext" VkImportMemoryWin32HandleInfoNV = Ptr Void
        type FieldOptional "pNext" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, pNext}
        type FieldIsArray "pNext" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryWin32HandleInfoNV where
        type FieldType "handleType" VkImportMemoryWin32HandleInfoNV =
             VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "handleType" VkImportMemoryWin32HandleInfoNV =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, handleType}
        type FieldIsArray "handleType" VkImportMemoryWin32HandleInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handleType}

instance {-# OVERLAPPING #-}
         HasField "handle" VkImportMemoryWin32HandleInfoNV where
        type FieldType "handle" VkImportMemoryWin32HandleInfoNV = HANDLE
        type FieldOptional "handle" VkImportMemoryWin32HandleInfoNV = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, handle}
        type FieldIsArray "handle" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, handle}

instance {-# OVERLAPPING #-}
         CanReadField "handle" VkImportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, handle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handle}

instance {-# OVERLAPPING #-}
         CanWriteField "handle" VkImportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handle}

instance Show VkImportMemoryWin32HandleInfoNV where
        showsPrec d x
          = showString "VkImportMemoryWin32HandleInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) .
                              showString ", " .
                                showString "handle = " .
                                  showsPrec d (getField @"handle" x) . showChar '}'

-- | > typedef struct VkImportSemaphoreWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore    semaphore;
--   >     VkSemaphoreImportFlags flags;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   >     HANDLE           handle;
--   >     LPCWSTR          name;
--   > } VkImportSemaphoreWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportSemaphoreWin32HandleInfoKHR VkImportSemaphoreWin32HandleInfoKHR registry at www.khronos.org>
data VkImportSemaphoreWin32HandleInfoKHR = VkImportSemaphoreWin32HandleInfoKHR## Addr##
                                                                                ByteArray##

instance Eq VkImportSemaphoreWin32HandleInfoKHR where
        (VkImportSemaphoreWin32HandleInfoKHR## a _) ==
          x@(VkImportSemaphoreWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportSemaphoreWin32HandleInfoKHR where
        (VkImportSemaphoreWin32HandleInfoKHR## a _) `compare`
          x@(VkImportSemaphoreWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportSemaphoreWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkImportSemaphoreWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportSemaphoreWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportSemaphoreWin32HandleInfoKHR
         where
        unsafeAddr (VkImportSemaphoreWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportSemaphoreWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportSemaphoreWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportSemaphoreWin32HandleInfoKHR where
        type StructFields VkImportSemaphoreWin32HandleInfoKHR =
             '["sType", "pNext", "semaphore", "flags", "handleType", "handle", -- ' closing tick for hsc2hs
               "name"]
        type CUnionType VkImportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportSemaphoreWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "sType" VkImportSemaphoreWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "semaphore" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "semaphore" VkImportSemaphoreWin32HandleInfoKHR =
             VkSemaphore
        type FieldOptional "semaphore" VkImportSemaphoreWin32HandleInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}
        type FieldIsArray "semaphore" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanReadField "semaphore" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanWriteField "semaphore" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         HasField "flags" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "flags" VkImportSemaphoreWin32HandleInfoKHR =
             VkSemaphoreImportFlags
        type FieldOptional "flags" VkImportSemaphoreWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}
        type FieldIsArray "flags" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "handleType" VkImportSemaphoreWin32HandleInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBits
        type FieldOptional "handleType" VkImportSemaphoreWin32HandleInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportSemaphoreWin32HandleInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handle" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "handle" VkImportSemaphoreWin32HandleInfoKHR =
             HANDLE
        type FieldOptional "handle" VkImportSemaphoreWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}
        type FieldIsArray "handle" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanReadField "handle" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, handle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanWriteField "handle" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         HasField "name" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "name" VkImportSemaphoreWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkImportSemaphoreWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "name" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, name}

instance Show VkImportSemaphoreWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkImportSemaphoreWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "semaphore = " .
                            showsPrec d (getField @"semaphore" x) .
                              showString ", " .
                                showString "flags = " .
                                  showsPrec d (getField @"flags" x) .
                                    showString ", " .
                                      showString "handleType = " .
                                        showsPrec d (getField @"handleType" x) .
                                          showString ", " .
                                            showString "handle = " .
                                              showsPrec d (getField @"handle" x) .
                                                showString ", " .
                                                  showString "name = " .
                                                    showsPrec d (getField @"name" x) . showChar '}'

-- | > typedef struct VkMemoryGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceMemory                   memory;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   > } VkMemoryGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryGetWin32HandleInfoKHR VkMemoryGetWin32HandleInfoKHR registry at www.khronos.org>
data VkMemoryGetWin32HandleInfoKHR = VkMemoryGetWin32HandleInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkMemoryGetWin32HandleInfoKHR where
        (VkMemoryGetWin32HandleInfoKHR## a _) ==
          x@(VkMemoryGetWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryGetWin32HandleInfoKHR where
        (VkMemoryGetWin32HandleInfoKHR## a _) `compare`
          x@(VkMemoryGetWin32HandleInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryGetWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkMemoryGetWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryGetWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryGetWin32HandleInfoKHR where
        unsafeAddr (VkMemoryGetWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryGetWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryGetWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryGetWin32HandleInfoKHR where
        type StructFields VkMemoryGetWin32HandleInfoKHR =
             '["sType", "pNext", "memory", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryGetWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "sType" VkMemoryGetWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "pNext" VkMemoryGetWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "memory" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "memory" VkMemoryGetWin32HandleInfoKHR =
             VkDeviceMemory
        type FieldOptional "memory" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, memory}
        type FieldIsArray "memory" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkMemoryGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, memory}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "handleType" VkMemoryGetWin32HandleInfoKHR =
             VkExternalMemoryHandleTypeFlagBits
        type FieldOptional "handleType" VkMemoryGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkMemoryGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkMemoryGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

instance Show VkMemoryGetWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkMemoryGetWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memory = " .
                            showsPrec d (getField @"memory" x) .
                              showString ", " .
                                showString "handleType = " .
                                  showsPrec d (getField @"handleType" x) . showChar '}'

-- | > typedef struct VkMemoryWin32HandlePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         memoryTypeBits;
--   > } VkMemoryWin32HandlePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryWin32HandlePropertiesKHR VkMemoryWin32HandlePropertiesKHR registry at www.khronos.org>
data VkMemoryWin32HandlePropertiesKHR = VkMemoryWin32HandlePropertiesKHR## Addr##
                                                                          ByteArray##

instance Eq VkMemoryWin32HandlePropertiesKHR where
        (VkMemoryWin32HandlePropertiesKHR## a _) ==
          x@(VkMemoryWin32HandlePropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryWin32HandlePropertiesKHR where
        (VkMemoryWin32HandlePropertiesKHR## a _) `compare`
          x@(VkMemoryWin32HandlePropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryWin32HandlePropertiesKHR where
        sizeOf ~_ = #{size VkMemoryWin32HandlePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryWin32HandlePropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryWin32HandlePropertiesKHR where
        unsafeAddr (VkMemoryWin32HandlePropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryWin32HandlePropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryWin32HandlePropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryWin32HandlePropertiesKHR where
        type StructFields VkMemoryWin32HandlePropertiesKHR =
             '["sType", "pNext", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryWin32HandlePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryWin32HandlePropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryWin32HandlePropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryWin32HandlePropertiesKHR where
        type FieldType "sType" VkMemoryWin32HandlePropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryWin32HandlePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryWin32HandlePropertiesKHR =
             #{offset VkMemoryWin32HandlePropertiesKHR, sType}
        type FieldIsArray "sType" VkMemoryWin32HandlePropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryWin32HandlePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryWin32HandlePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryWin32HandlePropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryWin32HandlePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryWin32HandlePropertiesKHR where
        type FieldType "pNext" VkMemoryWin32HandlePropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryWin32HandlePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryWin32HandlePropertiesKHR =
             #{offset VkMemoryWin32HandlePropertiesKHR, pNext}
        type FieldIsArray "pNext" VkMemoryWin32HandlePropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryWin32HandlePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryWin32HandlePropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryWin32HandlePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR where
        type FieldType "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR =
             Word32
        type FieldOptional "memoryTypeBits"
               VkMemoryWin32HandlePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR
             =
             #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}
        type FieldIsArray "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

instance Show VkMemoryWin32HandlePropertiesKHR where
        showsPrec d x
          = showString "VkMemoryWin32HandlePropertiesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryTypeBits = " .
                            showsPrec d (getField @"memoryTypeBits" x) . showChar '}'

-- | > typedef struct VkSemaphoreGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore                      semaphore;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   > } VkSemaphoreGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSemaphoreGetWin32HandleInfoKHR VkSemaphoreGetWin32HandleInfoKHR registry at www.khronos.org>
data VkSemaphoreGetWin32HandleInfoKHR = VkSemaphoreGetWin32HandleInfoKHR## Addr##
                                                                          ByteArray##

instance Eq VkSemaphoreGetWin32HandleInfoKHR where
        (VkSemaphoreGetWin32HandleInfoKHR## a _) ==
          x@(VkSemaphoreGetWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSemaphoreGetWin32HandleInfoKHR where
        (VkSemaphoreGetWin32HandleInfoKHR## a _) `compare`
          x@(VkSemaphoreGetWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSemaphoreGetWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkSemaphoreGetWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSemaphoreGetWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSemaphoreGetWin32HandleInfoKHR where
        unsafeAddr (VkSemaphoreGetWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSemaphoreGetWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSemaphoreGetWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSemaphoreGetWin32HandleInfoKHR where
        type StructFields VkSemaphoreGetWin32HandleInfoKHR =
             '["sType", "pNext", "semaphore", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkSemaphoreGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSemaphoreGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSemaphoreGetWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSemaphoreGetWin32HandleInfoKHR where
        type FieldType "sType" VkSemaphoreGetWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSemaphoreGetWin32HandleInfoKHR =
             #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkSemaphoreGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSemaphoreGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSemaphoreGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSemaphoreGetWin32HandleInfoKHR where
        type FieldType "pNext" VkSemaphoreGetWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSemaphoreGetWin32HandleInfoKHR =
             #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkSemaphoreGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSemaphoreGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSemaphoreGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "semaphore" VkSemaphoreGetWin32HandleInfoKHR where
        type FieldType "semaphore" VkSemaphoreGetWin32HandleInfoKHR =
             VkSemaphore
        type FieldOptional "semaphore" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkSemaphoreGetWin32HandleInfoKHR =
             #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}
        type FieldIsArray "semaphore" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanReadField "semaphore" VkSemaphoreGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanWriteField "semaphore" VkSemaphoreGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkSemaphoreGetWin32HandleInfoKHR where
        type FieldType "handleType" VkSemaphoreGetWin32HandleInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBits
        type FieldOptional "handleType" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkSemaphoreGetWin32HandleInfoKHR =
             #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkSemaphoreGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkSemaphoreGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}

instance Show VkSemaphoreGetWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkSemaphoreGetWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "semaphore = " .
                            showsPrec d (getField @"semaphore" x) .
                              showString ", " .
                                showString "handleType = " .
                                  showsPrec d (getField @"handleType" x) . showChar '}'

-- | > typedef struct VkWin32KeyedMutexAcquireReleaseInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         acquireCount;
--   >     const VkDeviceMemory* pAcquireSyncs;
--   >     const uint64_t* pAcquireKeys;
--   >     const uint32_t* pAcquireTimeouts;
--   >     uint32_t         releaseCount;
--   >     const VkDeviceMemory* pReleaseSyncs;
--   >     const uint64_t* pReleaseKeys;
--   > } VkWin32KeyedMutexAcquireReleaseInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkWin32KeyedMutexAcquireReleaseInfoKHR VkWin32KeyedMutexAcquireReleaseInfoKHR registry at www.khronos.org>
data VkWin32KeyedMutexAcquireReleaseInfoKHR = VkWin32KeyedMutexAcquireReleaseInfoKHR## Addr##
                                                                                      ByteArray##

instance Eq VkWin32KeyedMutexAcquireReleaseInfoKHR where
        (VkWin32KeyedMutexAcquireReleaseInfoKHR## a _) ==
          x@(VkWin32KeyedMutexAcquireReleaseInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkWin32KeyedMutexAcquireReleaseInfoKHR where
        (VkWin32KeyedMutexAcquireReleaseInfoKHR## a _) `compare`
          x@(VkWin32KeyedMutexAcquireReleaseInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkWin32KeyedMutexAcquireReleaseInfoKHR where
        sizeOf ~_
          = #{size VkWin32KeyedMutexAcquireReleaseInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkWin32KeyedMutexAcquireReleaseInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        unsafeAddr (VkWin32KeyedMutexAcquireReleaseInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkWin32KeyedMutexAcquireReleaseInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkWin32KeyedMutexAcquireReleaseInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type StructFields VkWin32KeyedMutexAcquireReleaseInfoKHR =
             '["sType", "pNext", "acquireCount", "pAcquireSyncs", -- ' closing tick for hsc2hs
               "pAcquireKeys", "pAcquireTimeouts", "releaseCount",
               "pReleaseSyncs", "pReleaseKeys"]
        type CUnionType VkWin32KeyedMutexAcquireReleaseInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkWin32KeyedMutexAcquireReleaseInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkWin32KeyedMutexAcquireReleaseInfoKHR =
             '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type FieldType "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}
        type FieldIsArray "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type FieldType "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}
        type FieldIsArray "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Word32
        type FieldOptional "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}
        type FieldIsArray "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}

instance {-# OVERLAPPING #-}
         CanReadField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}

instance {-# OVERLAPPING #-}
         CanWriteField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}

instance {-# OVERLAPPING #-}
         HasField "pAcquireSyncs" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr VkDeviceMemory
        type FieldOptional "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}
        type FieldIsArray "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireSyncs" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireSyncs"
           VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         HasField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr Word64
        type FieldOptional "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}
        type FieldIsArray "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}

instance {-# OVERLAPPING #-}
         HasField "pAcquireTimeouts" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "pAcquireTimeouts"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr Word32
        type FieldOptional "pAcquireTimeouts"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireTimeouts"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}
        type FieldIsArray "pAcquireTimeouts"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireTimeouts"
           VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireTimeouts"
           VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}

instance {-# OVERLAPPING #-}
         HasField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Word32
        type FieldOptional "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}
        type FieldIsArray "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}

instance {-# OVERLAPPING #-}
         CanReadField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}

instance {-# OVERLAPPING #-}
         CanWriteField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}

instance {-# OVERLAPPING #-}
         HasField "pReleaseSyncs" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr VkDeviceMemory
        type FieldOptional "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}
        type FieldIsArray "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         CanReadField "pReleaseSyncs" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         CanWriteField "pReleaseSyncs"
           VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         HasField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr Word64
        type FieldOptional "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}
        type FieldIsArray "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}

instance {-# OVERLAPPING #-}
         CanReadField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}

instance {-# OVERLAPPING #-}
         CanWriteField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}

instance Show VkWin32KeyedMutexAcquireReleaseInfoKHR where
        showsPrec d x
          = showString "VkWin32KeyedMutexAcquireReleaseInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "acquireCount = " .
                            showsPrec d (getField @"acquireCount" x) .
                              showString ", " .
                                showString "pAcquireSyncs = " .
                                  showsPrec d (getField @"pAcquireSyncs" x) .
                                    showString ", " .
                                      showString "pAcquireKeys = " .
                                        showsPrec d (getField @"pAcquireKeys" x) .
                                          showString ", " .
                                            showString "pAcquireTimeouts = " .
                                              showsPrec d (getField @"pAcquireTimeouts" x) .
                                                showString ", " .
                                                  showString "releaseCount = " .
                                                    showsPrec d (getField @"releaseCount" x) .
                                                      showString ", " .
                                                        showString "pReleaseSyncs = " .
                                                          showsPrec d (getField @"pReleaseSyncs" x)
                                                            .
                                                            showString ", " .
                                                              showString "pReleaseKeys = " .
                                                                showsPrec d
                                                                  (getField @"pReleaseKeys" x)
                                                                  . showChar '}'

-- | > typedef struct VkWin32KeyedMutexAcquireReleaseInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         acquireCount;
--   >     const VkDeviceMemory*            pAcquireSyncs;
--   >     const uint64_t*                  pAcquireKeys;
--   >     const uint32_t*                  pAcquireTimeoutMilliseconds;
--   >     uint32_t                         releaseCount;
--   >     const VkDeviceMemory*            pReleaseSyncs;
--   >     const uint64_t*                  pReleaseKeys;
--   > } VkWin32KeyedMutexAcquireReleaseInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkWin32KeyedMutexAcquireReleaseInfoNV VkWin32KeyedMutexAcquireReleaseInfoNV registry at www.khronos.org>
data VkWin32KeyedMutexAcquireReleaseInfoNV = VkWin32KeyedMutexAcquireReleaseInfoNV## Addr##
                                                                                    ByteArray##

instance Eq VkWin32KeyedMutexAcquireReleaseInfoNV where
        (VkWin32KeyedMutexAcquireReleaseInfoNV## a _) ==
          x@(VkWin32KeyedMutexAcquireReleaseInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkWin32KeyedMutexAcquireReleaseInfoNV where
        (VkWin32KeyedMutexAcquireReleaseInfoNV## a _) `compare`
          x@(VkWin32KeyedMutexAcquireReleaseInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkWin32KeyedMutexAcquireReleaseInfoNV where
        sizeOf ~_
          = #{size VkWin32KeyedMutexAcquireReleaseInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkWin32KeyedMutexAcquireReleaseInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        unsafeAddr (VkWin32KeyedMutexAcquireReleaseInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkWin32KeyedMutexAcquireReleaseInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkWin32KeyedMutexAcquireReleaseInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkWin32KeyedMutexAcquireReleaseInfoNV where
        type StructFields VkWin32KeyedMutexAcquireReleaseInfoNV =
             '["sType", "pNext", "acquireCount", "pAcquireSyncs", -- ' closing tick for hsc2hs
               "pAcquireKeys", "pAcquireTimeoutMilliseconds", "releaseCount",
               "pReleaseSyncs", "pReleaseKeys"]
        type CUnionType VkWin32KeyedMutexAcquireReleaseInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkWin32KeyedMutexAcquireReleaseInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkWin32KeyedMutexAcquireReleaseInfoNV =
             '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             VkStructureType
        type FieldOptional "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}
        type FieldIsArray "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkWin32KeyedMutexAcquireReleaseInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkWin32KeyedMutexAcquireReleaseInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr Void
        type FieldOptional "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}
        type FieldIsArray "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoNV
             = Word32
        type FieldOptional "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}
        type FieldIsArray "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

instance {-# OVERLAPPING #-}
         CanReadField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

instance {-# OVERLAPPING #-}
         CanWriteField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

instance {-# OVERLAPPING #-}
         HasField "pAcquireSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        type FieldType "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr VkDeviceMemory
        type FieldOptional "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}
        type FieldIsArray "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         HasField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr Word64
        type FieldOptional "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}
        type FieldIsArray "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

instance {-# OVERLAPPING #-}
         HasField "pAcquireTimeoutMilliseconds"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        type FieldType "pAcquireTimeoutMilliseconds"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr Word32
        type FieldOptional "pAcquireTimeoutMilliseconds"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireTimeoutMilliseconds"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}
        type FieldIsArray "pAcquireTimeoutMilliseconds"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireTimeoutMilliseconds"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireTimeoutMilliseconds"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

instance {-# OVERLAPPING #-}
         HasField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoNV
             = Word32
        type FieldOptional "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}
        type FieldIsArray "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

instance {-# OVERLAPPING #-}
         CanReadField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

instance {-# OVERLAPPING #-}
         CanWriteField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

instance {-# OVERLAPPING #-}
         HasField "pReleaseSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        type FieldType "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr VkDeviceMemory
        type FieldOptional "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}
        type FieldIsArray "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         CanReadField "pReleaseSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         CanWriteField "pReleaseSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         HasField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr Word64
        type FieldOptional "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}
        type FieldIsArray "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

instance {-# OVERLAPPING #-}
         CanReadField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

instance {-# OVERLAPPING #-}
         CanWriteField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

instance Show VkWin32KeyedMutexAcquireReleaseInfoNV where
        showsPrec d x
          = showString "VkWin32KeyedMutexAcquireReleaseInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "acquireCount = " .
                            showsPrec d (getField @"acquireCount" x) .
                              showString ", " .
                                showString "pAcquireSyncs = " .
                                  showsPrec d (getField @"pAcquireSyncs" x) .
                                    showString ", " .
                                      showString "pAcquireKeys = " .
                                        showsPrec d (getField @"pAcquireKeys" x) .
                                          showString ", " .
                                            showString "pAcquireTimeoutMilliseconds = " .
                                              showsPrec d
                                                (getField @"pAcquireTimeoutMilliseconds" x)
                                                .
                                                showString ", " .
                                                  showString "releaseCount = " .
                                                    showsPrec d (getField @"releaseCount" x) .
                                                      showString ", " .
                                                        showString "pReleaseSyncs = " .
                                                          showsPrec d (getField @"pReleaseSyncs" x)
                                                            .
                                                            showString ", " .
                                                              showString "pReleaseKeys = " .
                                                                showsPrec d
                                                                  (getField @"pReleaseKeys" x)
                                                                  . showChar '}'

-- | > typedef struct VkWin32SurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkWin32SurfaceCreateFlagsKHR   flags;
--   >     HINSTANCE                        hinstance;
--   >     HWND                             hwnd;
--   > } VkWin32SurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkWin32SurfaceCreateInfoKHR VkWin32SurfaceCreateInfoKHR registry at www.khronos.org>
data VkWin32SurfaceCreateInfoKHR = VkWin32SurfaceCreateInfoKHR## Addr##
                                                                ByteArray##

instance Eq VkWin32SurfaceCreateInfoKHR where
        (VkWin32SurfaceCreateInfoKHR## a _) ==
          x@(VkWin32SurfaceCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkWin32SurfaceCreateInfoKHR where
        (VkWin32SurfaceCreateInfoKHR## a _) `compare`
          x@(VkWin32SurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkWin32SurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkWin32SurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkWin32SurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkWin32SurfaceCreateInfoKHR where
        unsafeAddr (VkWin32SurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkWin32SurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkWin32SurfaceCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkWin32SurfaceCreateInfoKHR where
        type StructFields VkWin32SurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "hinstance", "hwnd"] -- ' closing tick for hsc2hs
        type CUnionType VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkWin32SurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkWin32SurfaceCreateInfoKHR where
        type FieldType "sType" VkWin32SurfaceCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkWin32SurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkWin32SurfaceCreateInfoKHR where
        type FieldType "pNext" VkWin32SurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkWin32SurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkWin32SurfaceCreateInfoKHR where
        type FieldType "flags" VkWin32SurfaceCreateInfoKHR =
             VkWin32SurfaceCreateFlagsKHR
        type FieldOptional "flags" VkWin32SurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkWin32SurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "hinstance" VkWin32SurfaceCreateInfoKHR where
        type FieldType "hinstance" VkWin32SurfaceCreateInfoKHR = HINSTANCE
        type FieldOptional "hinstance" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "hinstance" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, hinstance}
        type FieldIsArray "hinstance" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

instance {-# OVERLAPPING #-}
         CanReadField "hinstance" VkWin32SurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, hinstance})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

instance {-# OVERLAPPING #-}
         CanWriteField "hinstance" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

instance {-# OVERLAPPING #-}
         HasField "hwnd" VkWin32SurfaceCreateInfoKHR where
        type FieldType "hwnd" VkWin32SurfaceCreateInfoKHR = HWND
        type FieldOptional "hwnd" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "hwnd" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, hwnd}
        type FieldIsArray "hwnd" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

instance {-# OVERLAPPING #-}
         CanReadField "hwnd" VkWin32SurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, hwnd})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

instance {-# OVERLAPPING #-}
         CanWriteField "hwnd" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

instance Show VkWin32SurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkWin32SurfaceCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "hinstance = " .
                                  showsPrec d (getField @"hinstance" x) .
                                    showString ", " .
                                      showString "hwnd = " .
                                        showsPrec d (getField @"hwnd" x) . showChar '}'
