#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Bind
       (VkBindBufferMemoryDeviceGroupInfo(..),
        VkBindBufferMemoryDeviceGroupInfoKHR, VkBindBufferMemoryInfo(..),
        VkBindBufferMemoryInfoKHR, VkBindImageMemoryDeviceGroupInfo(..),
        VkBindImageMemoryDeviceGroupInfoKHR, VkBindImageMemoryInfo(..),
        VkBindImageMemoryInfoKHR, VkBindImageMemorySwapchainInfoKHR(..),
        VkBindImagePlaneMemoryInfo(..), VkBindImagePlaneMemoryInfoKHR,
        VkBindSparseInfo(..))
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.Image         (VkImageAspectFlagBits)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer,
                                                           VkDeviceMemory,
                                                           VkImage, VkSemaphore,
                                                           VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.Rect        (VkRect2D)
import           Graphics.Vulkan.Types.Struct.Sparse      (VkSparseBufferMemoryBindInfo,
                                                           VkSparseImageMemoryBindInfo,
                                                           VkSparseImageOpaqueMemoryBindInfo)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkBindBufferMemoryDeviceGroupInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   > } VkBindBufferMemoryDeviceGroupInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindBufferMemoryDeviceGroupInfo VkBindBufferMemoryDeviceGroupInfo registry at www.khronos.org>
data VkBindBufferMemoryDeviceGroupInfo = VkBindBufferMemoryDeviceGroupInfo## Addr##
                                                                            ByteArray##

instance Eq VkBindBufferMemoryDeviceGroupInfo where
        (VkBindBufferMemoryDeviceGroupInfo## a _) ==
          x@(VkBindBufferMemoryDeviceGroupInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindBufferMemoryDeviceGroupInfo where
        (VkBindBufferMemoryDeviceGroupInfo## a _) `compare`
          x@(VkBindBufferMemoryDeviceGroupInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindBufferMemoryDeviceGroupInfo where
        sizeOf ~_ = #{size VkBindBufferMemoryDeviceGroupInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindBufferMemoryDeviceGroupInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindBufferMemoryDeviceGroupInfo where
        unsafeAddr (VkBindBufferMemoryDeviceGroupInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindBufferMemoryDeviceGroupInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindBufferMemoryDeviceGroupInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindBufferMemoryDeviceGroupInfo where
        type StructFields VkBindBufferMemoryDeviceGroupInfo =
             '["sType", "pNext", "deviceIndexCount", "pDeviceIndices"] -- ' closing tick for hsc2hs
        type CUnionType VkBindBufferMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindBufferMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindBufferMemoryDeviceGroupInfo =
             '[VkBindBufferMemoryInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindBufferMemoryDeviceGroupInfo where
        type FieldType "sType" VkBindBufferMemoryDeviceGroupInfo =
             VkStructureType
        type FieldOptional "sType" VkBindBufferMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindBufferMemoryDeviceGroupInfo =
             #{offset VkBindBufferMemoryDeviceGroupInfo, sType}
        type FieldIsArray "sType" VkBindBufferMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindBufferMemoryDeviceGroupInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindBufferMemoryDeviceGroupInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindBufferMemoryDeviceGroupInfo where
        type FieldType "pNext" VkBindBufferMemoryDeviceGroupInfo = Ptr Void
        type FieldOptional "pNext" VkBindBufferMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindBufferMemoryDeviceGroupInfo =
             #{offset VkBindBufferMemoryDeviceGroupInfo, pNext}
        type FieldIsArray "pNext" VkBindBufferMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindBufferMemoryDeviceGroupInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindBufferMemoryDeviceGroupInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfo where
        type FieldType "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfo
             = Word32
        type FieldOptional "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfo
             =
             #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount}
        type FieldIsArray "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanReadField "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         HasField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfo where
        type FieldType "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfo =
             Ptr Word32
        type FieldOptional "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfo
             =
             #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices}
        type FieldIsArray "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices}

instance Show VkBindBufferMemoryDeviceGroupInfo where
        showsPrec d x
          = showString "VkBindBufferMemoryDeviceGroupInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceIndexCount = " .
                            showsPrec d (getField @"deviceIndexCount" x) .
                              showString ", " .
                                showString "pDeviceIndices = " .
                                  showsPrec d (getField @"pDeviceIndices" x) . showChar '}'

-- | Alias for `VkBindBufferMemoryDeviceGroupInfo`
type VkBindBufferMemoryDeviceGroupInfoKHR =
     VkBindBufferMemoryDeviceGroupInfo

-- | > typedef struct VkBindBufferMemoryInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBuffer                         buffer;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindBufferMemoryInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindBufferMemoryInfo VkBindBufferMemoryInfo registry at www.khronos.org>
data VkBindBufferMemoryInfo = VkBindBufferMemoryInfo## Addr##
                                                      ByteArray##

instance Eq VkBindBufferMemoryInfo where
        (VkBindBufferMemoryInfo## a _) == x@(VkBindBufferMemoryInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindBufferMemoryInfo where
        (VkBindBufferMemoryInfo## a _) `compare`
          x@(VkBindBufferMemoryInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindBufferMemoryInfo where
        sizeOf ~_ = #{size VkBindBufferMemoryInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindBufferMemoryInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindBufferMemoryInfo where
        unsafeAddr (VkBindBufferMemoryInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindBufferMemoryInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindBufferMemoryInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindBufferMemoryInfo where
        type StructFields VkBindBufferMemoryInfo =
             '["sType", "pNext", "buffer", "memory", "memoryOffset"] -- ' closing tick for hsc2hs
        type CUnionType VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindBufferMemoryInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindBufferMemoryInfo where
        type FieldType "sType" VkBindBufferMemoryInfo = VkStructureType
        type FieldOptional "sType" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindBufferMemoryInfo =
             #{offset VkBindBufferMemoryInfo, sType}
        type FieldIsArray "sType" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindBufferMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindBufferMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindBufferMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindBufferMemoryInfo where
        type FieldType "pNext" VkBindBufferMemoryInfo = Ptr Void
        type FieldOptional "pNext" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindBufferMemoryInfo =
             #{offset VkBindBufferMemoryInfo, pNext}
        type FieldIsArray "pNext" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindBufferMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindBufferMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindBufferMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBindBufferMemoryInfo where
        type FieldType "buffer" VkBindBufferMemoryInfo = VkBuffer
        type FieldOptional "buffer" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBindBufferMemoryInfo =
             #{offset VkBindBufferMemoryInfo, buffer}
        type FieldIsArray "buffer" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindBufferMemoryInfo, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkBindBufferMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfo, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfo, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkBindBufferMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfo, buffer}

instance {-# OVERLAPPING #-}
         HasField "memory" VkBindBufferMemoryInfo where
        type FieldType "memory" VkBindBufferMemoryInfo = VkDeviceMemory
        type FieldOptional "memory" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkBindBufferMemoryInfo =
             #{offset VkBindBufferMemoryInfo, memory}
        type FieldIsArray "memory" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindBufferMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkBindBufferMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfo, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkBindBufferMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkBindBufferMemoryInfo where
        type FieldType "memoryOffset" VkBindBufferMemoryInfo = VkDeviceSize
        type FieldOptional "memoryOffset" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkBindBufferMemoryInfo =
             #{offset VkBindBufferMemoryInfo, memoryOffset}
        type FieldIsArray "memoryOffset" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryInfo, memoryOffset}

instance {-# OVERLAPPING #-}
         CanReadField "memoryOffset" VkBindBufferMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfo, memoryOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfo, memoryOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryOffset" VkBindBufferMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfo, memoryOffset}

instance Show VkBindBufferMemoryInfo where
        showsPrec d x
          = showString "VkBindBufferMemoryInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) .
                              showString ", " .
                                showString "memory = " .
                                  showsPrec d (getField @"memory" x) .
                                    showString ", " .
                                      showString "memoryOffset = " .
                                        showsPrec d (getField @"memoryOffset" x) . showChar '}'

-- | Alias for `VkBindBufferMemoryInfo`
type VkBindBufferMemoryInfoKHR = VkBindBufferMemoryInfo

-- | > typedef struct VkBindImageMemoryDeviceGroupInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   >     uint32_t         splitInstanceBindRegionCount;
--   >     const VkRect2D*  pSplitInstanceBindRegions;
--   > } VkBindImageMemoryDeviceGroupInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindImageMemoryDeviceGroupInfo VkBindImageMemoryDeviceGroupInfo registry at www.khronos.org>
data VkBindImageMemoryDeviceGroupInfo = VkBindImageMemoryDeviceGroupInfo## Addr##
                                                                          ByteArray##

instance Eq VkBindImageMemoryDeviceGroupInfo where
        (VkBindImageMemoryDeviceGroupInfo## a _) ==
          x@(VkBindImageMemoryDeviceGroupInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemoryDeviceGroupInfo where
        (VkBindImageMemoryDeviceGroupInfo## a _) `compare`
          x@(VkBindImageMemoryDeviceGroupInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemoryDeviceGroupInfo where
        sizeOf ~_ = #{size VkBindImageMemoryDeviceGroupInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindImageMemoryDeviceGroupInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImageMemoryDeviceGroupInfo where
        unsafeAddr (VkBindImageMemoryDeviceGroupInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImageMemoryDeviceGroupInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImageMemoryDeviceGroupInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImageMemoryDeviceGroupInfo where
        type StructFields VkBindImageMemoryDeviceGroupInfo =
             '["sType", "pNext", "deviceIndexCount", "pDeviceIndices", -- ' closing tick for hsc2hs
               "splitInstanceBindRegionCount", "pSplitInstanceBindRegions"]
        type CUnionType VkBindImageMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImageMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImageMemoryDeviceGroupInfo =
             '[VkBindImageMemoryInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImageMemoryDeviceGroupInfo where
        type FieldType "sType" VkBindImageMemoryDeviceGroupInfo =
             VkStructureType
        type FieldOptional "sType" VkBindImageMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImageMemoryDeviceGroupInfo =
             #{offset VkBindImageMemoryDeviceGroupInfo, sType}
        type FieldIsArray "sType" VkBindImageMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImageMemoryDeviceGroupInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImageMemoryDeviceGroupInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImageMemoryDeviceGroupInfo where
        type FieldType "pNext" VkBindImageMemoryDeviceGroupInfo = Ptr Void
        type FieldOptional "pNext" VkBindImageMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImageMemoryDeviceGroupInfo =
             #{offset VkBindImageMemoryDeviceGroupInfo, pNext}
        type FieldIsArray "pNext" VkBindImageMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImageMemoryDeviceGroupInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImageMemoryDeviceGroupInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceIndexCount" VkBindImageMemoryDeviceGroupInfo where
        type FieldType "deviceIndexCount" VkBindImageMemoryDeviceGroupInfo
             = Word32
        type FieldOptional "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfo
             =
             #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount}
        type FieldIsArray "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanReadField "deviceIndexCount" VkBindImageMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceIndexCount" VkBindImageMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         HasField "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo where
        type FieldType "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo =
             Ptr Word32
        type FieldOptional "pDeviceIndices"
               VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo
             =
             #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices}
        type FieldIsArray "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "splitInstanceBindRegionCount"
           VkBindImageMemoryDeviceGroupInfo
         where
        type FieldType "splitInstanceBindRegionCount"
               VkBindImageMemoryDeviceGroupInfo
             = Word32
        type FieldOptional "splitInstanceBindRegionCount"
               VkBindImageMemoryDeviceGroupInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "splitInstanceBindRegionCount"
               VkBindImageMemoryDeviceGroupInfo
             =
             #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount}
        type FieldIsArray "splitInstanceBindRegionCount"
               VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount}

instance {-# OVERLAPPING #-}
         CanReadField "splitInstanceBindRegionCount"
           VkBindImageMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount}

instance {-# OVERLAPPING #-}
         CanWriteField "splitInstanceBindRegionCount"
           VkBindImageMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount}

instance {-# OVERLAPPING #-}
         HasField "pSplitInstanceBindRegions"
           VkBindImageMemoryDeviceGroupInfo
         where
        type FieldType "pSplitInstanceBindRegions"
               VkBindImageMemoryDeviceGroupInfo
             = Ptr VkRect2D
        type FieldOptional "pSplitInstanceBindRegions"
               VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSplitInstanceBindRegions"
               VkBindImageMemoryDeviceGroupInfo
             =
             #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions}
        type FieldIsArray "pSplitInstanceBindRegions"
               VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions}

instance {-# OVERLAPPING #-}
         CanReadField "pSplitInstanceBindRegions"
           VkBindImageMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions}

instance {-# OVERLAPPING #-}
         CanWriteField "pSplitInstanceBindRegions"
           VkBindImageMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions}

instance Show VkBindImageMemoryDeviceGroupInfo where
        showsPrec d x
          = showString "VkBindImageMemoryDeviceGroupInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceIndexCount = " .
                            showsPrec d (getField @"deviceIndexCount" x) .
                              showString ", " .
                                showString "pDeviceIndices = " .
                                  showsPrec d (getField @"pDeviceIndices" x) .
                                    showString ", " .
                                      showString "splitInstanceBindRegionCount = " .
                                        showsPrec d (getField @"splitInstanceBindRegionCount" x) .
                                          showString ", " .
                                            showString "pSplitInstanceBindRegions = " .
                                              showsPrec d (getField @"pSplitInstanceBindRegions" x)
                                                . showChar '}'

-- | Alias for `VkBindImageMemoryDeviceGroupInfo`
type VkBindImageMemoryDeviceGroupInfoKHR =
     VkBindImageMemoryDeviceGroupInfo

-- | > typedef struct VkBindImageMemoryInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage                          image;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindImageMemoryInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindImageMemoryInfo VkBindImageMemoryInfo registry at www.khronos.org>
data VkBindImageMemoryInfo = VkBindImageMemoryInfo## Addr##
                                                    ByteArray##

instance Eq VkBindImageMemoryInfo where
        (VkBindImageMemoryInfo## a _) == x@(VkBindImageMemoryInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemoryInfo where
        (VkBindImageMemoryInfo## a _) `compare`
          x@(VkBindImageMemoryInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemoryInfo where
        sizeOf ~_ = #{size VkBindImageMemoryInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindImageMemoryInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImageMemoryInfo where
        unsafeAddr (VkBindImageMemoryInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImageMemoryInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImageMemoryInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImageMemoryInfo where
        type StructFields VkBindImageMemoryInfo =
             '["sType", "pNext", "image", "memory", "memoryOffset"] -- ' closing tick for hsc2hs
        type CUnionType VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImageMemoryInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkBindImageMemoryInfo
         where
        type FieldType "sType" VkBindImageMemoryInfo = VkStructureType
        type FieldOptional "sType" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImageMemoryInfo =
             #{offset VkBindImageMemoryInfo, sType}
        type FieldIsArray "sType" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImageMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImageMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkBindImageMemoryInfo
         where
        type FieldType "pNext" VkBindImageMemoryInfo = Ptr Void
        type FieldOptional "pNext" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImageMemoryInfo =
             #{offset VkBindImageMemoryInfo, pNext}
        type FieldIsArray "pNext" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImageMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImageMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfo, pNext}

instance {-# OVERLAPPING #-} HasField "image" VkBindImageMemoryInfo
         where
        type FieldType "image" VkBindImageMemoryInfo = VkImage
        type FieldOptional "image" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkBindImageMemoryInfo =
             #{offset VkBindImageMemoryInfo, image}
        type FieldIsArray "image" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfo, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkBindImageMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfo, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfo, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkBindImageMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfo, image}

instance {-# OVERLAPPING #-}
         HasField "memory" VkBindImageMemoryInfo where
        type FieldType "memory" VkBindImageMemoryInfo = VkDeviceMemory
        type FieldOptional "memory" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkBindImageMemoryInfo =
             #{offset VkBindImageMemoryInfo, memory}
        type FieldIsArray "memory" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindImageMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkBindImageMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfo, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkBindImageMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkBindImageMemoryInfo where
        type FieldType "memoryOffset" VkBindImageMemoryInfo = VkDeviceSize
        type FieldOptional "memoryOffset" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkBindImageMemoryInfo =
             #{offset VkBindImageMemoryInfo, memoryOffset}
        type FieldIsArray "memoryOffset" VkBindImageMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryInfo, memoryOffset}

instance {-# OVERLAPPING #-}
         CanReadField "memoryOffset" VkBindImageMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfo, memoryOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryInfo, memoryOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryOffset" VkBindImageMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryInfo, memoryOffset}

instance Show VkBindImageMemoryInfo where
        showsPrec d x
          = showString "VkBindImageMemoryInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "image = " .
                            showsPrec d (getField @"image" x) .
                              showString ", " .
                                showString "memory = " .
                                  showsPrec d (getField @"memory" x) .
                                    showString ", " .
                                      showString "memoryOffset = " .
                                        showsPrec d (getField @"memoryOffset" x) . showChar '}'

-- | Alias for `VkBindImageMemoryInfo`
type VkBindImageMemoryInfoKHR = VkBindImageMemoryInfo

-- | > typedef struct VkBindImageMemorySwapchainInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR swapchain;
--   >     uint32_t                         imageIndex;
--   > } VkBindImageMemorySwapchainInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindImageMemorySwapchainInfoKHR VkBindImageMemorySwapchainInfoKHR registry at www.khronos.org>
data VkBindImageMemorySwapchainInfoKHR = VkBindImageMemorySwapchainInfoKHR## Addr##
                                                                            ByteArray##

instance Eq VkBindImageMemorySwapchainInfoKHR where
        (VkBindImageMemorySwapchainInfoKHR## a _) ==
          x@(VkBindImageMemorySwapchainInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemorySwapchainInfoKHR where
        (VkBindImageMemorySwapchainInfoKHR## a _) `compare`
          x@(VkBindImageMemorySwapchainInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemorySwapchainInfoKHR where
        sizeOf ~_ = #{size VkBindImageMemorySwapchainInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindImageMemorySwapchainInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImageMemorySwapchainInfoKHR where
        unsafeAddr (VkBindImageMemorySwapchainInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImageMemorySwapchainInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImageMemorySwapchainInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImageMemorySwapchainInfoKHR where
        type StructFields VkBindImageMemorySwapchainInfoKHR =
             '["sType", "pNext", "swapchain", "imageIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkBindImageMemorySwapchainInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImageMemorySwapchainInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImageMemorySwapchainInfoKHR =
             '[VkBindImageMemoryInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImageMemorySwapchainInfoKHR where
        type FieldType "sType" VkBindImageMemorySwapchainInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkBindImageMemorySwapchainInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImageMemorySwapchainInfoKHR =
             #{offset VkBindImageMemorySwapchainInfoKHR, sType}
        type FieldIsArray "sType" VkBindImageMemorySwapchainInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemorySwapchainInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImageMemorySwapchainInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImageMemorySwapchainInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImageMemorySwapchainInfoKHR where
        type FieldType "pNext" VkBindImageMemorySwapchainInfoKHR = Ptr Void
        type FieldOptional "pNext" VkBindImageMemorySwapchainInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImageMemorySwapchainInfoKHR =
             #{offset VkBindImageMemorySwapchainInfoKHR, pNext}
        type FieldIsArray "pNext" VkBindImageMemorySwapchainInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemorySwapchainInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImageMemorySwapchainInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImageMemorySwapchainInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchain" VkBindImageMemorySwapchainInfoKHR where
        type FieldType "swapchain" VkBindImageMemorySwapchainInfoKHR =
             VkSwapchainKHR
        type FieldOptional "swapchain" VkBindImageMemorySwapchainInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "swapchain" VkBindImageMemorySwapchainInfoKHR =
             #{offset VkBindImageMemorySwapchainInfoKHR, swapchain}
        type FieldIsArray "swapchain" VkBindImageMemorySwapchainInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemorySwapchainInfoKHR, swapchain}

instance {-# OVERLAPPING #-}
         CanReadField "swapchain" VkBindImageMemorySwapchainInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHR, swapchain})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHR, swapchain}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchain" VkBindImageMemorySwapchainInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHR, swapchain}

instance {-# OVERLAPPING #-}
         HasField "imageIndex" VkBindImageMemorySwapchainInfoKHR where
        type FieldType "imageIndex" VkBindImageMemorySwapchainInfoKHR =
             Word32
        type FieldOptional "imageIndex" VkBindImageMemorySwapchainInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "imageIndex" VkBindImageMemorySwapchainInfoKHR =
             #{offset VkBindImageMemorySwapchainInfoKHR, imageIndex}
        type FieldIsArray "imageIndex" VkBindImageMemorySwapchainInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemorySwapchainInfoKHR, imageIndex}

instance {-# OVERLAPPING #-}
         CanReadField "imageIndex" VkBindImageMemorySwapchainInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHR, imageIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHR, imageIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "imageIndex" VkBindImageMemorySwapchainInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHR, imageIndex}

instance Show VkBindImageMemorySwapchainInfoKHR where
        showsPrec d x
          = showString "VkBindImageMemorySwapchainInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "swapchain = " .
                            showsPrec d (getField @"swapchain" x) .
                              showString ", " .
                                showString "imageIndex = " .
                                  showsPrec d (getField @"imageIndex" x) . showChar '}'

-- | > typedef struct VkBindImagePlaneMemoryInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImageAspectFlagBits            planeAspect;
--   > } VkBindImagePlaneMemoryInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindImagePlaneMemoryInfo VkBindImagePlaneMemoryInfo registry at www.khronos.org>
data VkBindImagePlaneMemoryInfo = VkBindImagePlaneMemoryInfo## Addr##
                                                              ByteArray##

instance Eq VkBindImagePlaneMemoryInfo where
        (VkBindImagePlaneMemoryInfo## a _) ==
          x@(VkBindImagePlaneMemoryInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImagePlaneMemoryInfo where
        (VkBindImagePlaneMemoryInfo## a _) `compare`
          x@(VkBindImagePlaneMemoryInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImagePlaneMemoryInfo where
        sizeOf ~_ = #{size VkBindImagePlaneMemoryInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindImagePlaneMemoryInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImagePlaneMemoryInfo where
        unsafeAddr (VkBindImagePlaneMemoryInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImagePlaneMemoryInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImagePlaneMemoryInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImagePlaneMemoryInfo where
        type StructFields VkBindImagePlaneMemoryInfo =
             '["sType", "pNext", "planeAspect"] -- ' closing tick for hsc2hs
        type CUnionType VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImagePlaneMemoryInfo =
             '[VkBindImageMemoryInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImagePlaneMemoryInfo where
        type FieldType "sType" VkBindImagePlaneMemoryInfo = VkStructureType
        type FieldOptional "sType" VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImagePlaneMemoryInfo =
             #{offset VkBindImagePlaneMemoryInfo, sType}
        type FieldIsArray "sType" VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImagePlaneMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImagePlaneMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImagePlaneMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImagePlaneMemoryInfo where
        type FieldType "pNext" VkBindImagePlaneMemoryInfo = Ptr Void
        type FieldOptional "pNext" VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImagePlaneMemoryInfo =
             #{offset VkBindImagePlaneMemoryInfo, pNext}
        type FieldIsArray "pNext" VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImagePlaneMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImagePlaneMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImagePlaneMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "planeAspect" VkBindImagePlaneMemoryInfo where
        type FieldType "planeAspect" VkBindImagePlaneMemoryInfo =
             VkImageAspectFlagBits
        type FieldOptional "planeAspect" VkBindImagePlaneMemoryInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "planeAspect" VkBindImagePlaneMemoryInfo =
             #{offset VkBindImagePlaneMemoryInfo, planeAspect}
        type FieldIsArray "planeAspect" VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImagePlaneMemoryInfo, planeAspect}

instance {-# OVERLAPPING #-}
         CanReadField "planeAspect" VkBindImagePlaneMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfo, planeAspect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfo, planeAspect}

instance {-# OVERLAPPING #-}
         CanWriteField "planeAspect" VkBindImagePlaneMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfo, planeAspect}

instance Show VkBindImagePlaneMemoryInfo where
        showsPrec d x
          = showString "VkBindImagePlaneMemoryInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "planeAspect = " .
                            showsPrec d (getField @"planeAspect" x) . showChar '}'

-- | Alias for `VkBindImagePlaneMemoryInfo`
type VkBindImagePlaneMemoryInfoKHR = VkBindImagePlaneMemoryInfo

-- | > typedef struct VkBindSparseInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               waitSemaphoreCount;
--   >     const VkSemaphore*     pWaitSemaphores;
--   >     uint32_t               bufferBindCount;
--   >     const VkSparseBufferMemoryBindInfo* pBufferBinds;
--   >     uint32_t               imageOpaqueBindCount;
--   >     const VkSparseImageOpaqueMemoryBindInfo* pImageOpaqueBinds;
--   >     uint32_t               imageBindCount;
--   >     const VkSparseImageMemoryBindInfo* pImageBinds;
--   >     uint32_t               signalSemaphoreCount;
--   >     const VkSemaphore*     pSignalSemaphores;
--   > } VkBindSparseInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBindSparseInfo VkBindSparseInfo registry at www.khronos.org>
data VkBindSparseInfo = VkBindSparseInfo## Addr## ByteArray##

instance Eq VkBindSparseInfo where
        (VkBindSparseInfo## a _) == x@(VkBindSparseInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindSparseInfo where
        (VkBindSparseInfo## a _) `compare` x@(VkBindSparseInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindSparseInfo where
        sizeOf ~_ = #{size VkBindSparseInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindSparseInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindSparseInfo where
        unsafeAddr (VkBindSparseInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindSparseInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindSparseInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindSparseInfo where
        type StructFields VkBindSparseInfo =
             '["sType", "pNext", "waitSemaphoreCount", "pWaitSemaphores", -- ' closing tick for hsc2hs
               "bufferBindCount", "pBufferBinds", "imageOpaqueBindCount",
               "pImageOpaqueBinds", "imageBindCount", "pImageBinds",
               "signalSemaphoreCount", "pSignalSemaphores"]
        type CUnionType VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindSparseInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkBindSparseInfo
         where
        type FieldType "sType" VkBindSparseInfo = VkStructureType
        type FieldOptional "sType" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindSparseInfo =
             #{offset VkBindSparseInfo, sType}
        type FieldIsArray "sType" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindSparseInfo, sType}

instance {-# OVERLAPPING #-} CanReadField "sType" VkBindSparseInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, sType}

instance {-# OVERLAPPING #-} CanWriteField "sType" VkBindSparseInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkBindSparseInfo
         where
        type FieldType "pNext" VkBindSparseInfo = Ptr Void
        type FieldOptional "pNext" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pNext}
        type FieldIsArray "pNext" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindSparseInfo, pNext}

instance {-# OVERLAPPING #-} CanReadField "pNext" VkBindSparseInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pNext}

instance {-# OVERLAPPING #-} CanWriteField "pNext" VkBindSparseInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreCount" VkBindSparseInfo where
        type FieldType "waitSemaphoreCount" VkBindSparseInfo = Word32
        type FieldOptional "waitSemaphoreCount" VkBindSparseInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreCount" VkBindSparseInfo =
             #{offset VkBindSparseInfo, waitSemaphoreCount}
        type FieldIsArray "waitSemaphoreCount" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanReadField "waitSemaphoreCount" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, waitSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "waitSemaphoreCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphores" VkBindSparseInfo where
        type FieldType "pWaitSemaphores" VkBindSparseInfo = Ptr VkSemaphore
        type FieldOptional "pWaitSemaphores" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphores" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pWaitSemaphores}
        type FieldIsArray "pWaitSemaphores" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         CanReadField "pWaitSemaphores" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pWaitSemaphores})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         CanWriteField "pWaitSemaphores" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         HasField "bufferBindCount" VkBindSparseInfo where
        type FieldType "bufferBindCount" VkBindSparseInfo = Word32
        type FieldOptional "bufferBindCount" VkBindSparseInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "bufferBindCount" VkBindSparseInfo =
             #{offset VkBindSparseInfo, bufferBindCount}
        type FieldIsArray "bufferBindCount" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, bufferBindCount}

instance {-# OVERLAPPING #-}
         CanReadField "bufferBindCount" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, bufferBindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, bufferBindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bufferBindCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, bufferBindCount}

instance {-# OVERLAPPING #-}
         HasField "pBufferBinds" VkBindSparseInfo where
        type FieldType "pBufferBinds" VkBindSparseInfo =
             Ptr VkSparseBufferMemoryBindInfo
        type FieldOptional "pBufferBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pBufferBinds" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pBufferBinds}
        type FieldIsArray "pBufferBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindSparseInfo, pBufferBinds}

instance {-# OVERLAPPING #-}
         CanReadField "pBufferBinds" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pBufferBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pBufferBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pBufferBinds" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pBufferBinds}

instance {-# OVERLAPPING #-}
         HasField "imageOpaqueBindCount" VkBindSparseInfo where
        type FieldType "imageOpaqueBindCount" VkBindSparseInfo = Word32
        type FieldOptional "imageOpaqueBindCount" VkBindSparseInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "imageOpaqueBindCount" VkBindSparseInfo =
             #{offset VkBindSparseInfo, imageOpaqueBindCount}
        type FieldIsArray "imageOpaqueBindCount" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, imageOpaqueBindCount}

instance {-# OVERLAPPING #-}
         CanReadField "imageOpaqueBindCount" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, imageOpaqueBindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, imageOpaqueBindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "imageOpaqueBindCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, imageOpaqueBindCount}

instance {-# OVERLAPPING #-}
         HasField "pImageOpaqueBinds" VkBindSparseInfo where
        type FieldType "pImageOpaqueBinds" VkBindSparseInfo =
             Ptr VkSparseImageOpaqueMemoryBindInfo
        type FieldOptional "pImageOpaqueBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pImageOpaqueBinds" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pImageOpaqueBinds}
        type FieldIsArray "pImageOpaqueBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, pImageOpaqueBinds}

instance {-# OVERLAPPING #-}
         CanReadField "pImageOpaqueBinds" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pImageOpaqueBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pImageOpaqueBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pImageOpaqueBinds" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pImageOpaqueBinds}

instance {-# OVERLAPPING #-}
         HasField "imageBindCount" VkBindSparseInfo where
        type FieldType "imageBindCount" VkBindSparseInfo = Word32
        type FieldOptional "imageBindCount" VkBindSparseInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "imageBindCount" VkBindSparseInfo =
             #{offset VkBindSparseInfo, imageBindCount}
        type FieldIsArray "imageBindCount" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, imageBindCount}

instance {-# OVERLAPPING #-}
         CanReadField "imageBindCount" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, imageBindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, imageBindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "imageBindCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, imageBindCount}

instance {-# OVERLAPPING #-}
         HasField "pImageBinds" VkBindSparseInfo where
        type FieldType "pImageBinds" VkBindSparseInfo =
             Ptr VkSparseImageMemoryBindInfo
        type FieldOptional "pImageBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pImageBinds" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pImageBinds}
        type FieldIsArray "pImageBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindSparseInfo, pImageBinds}

instance {-# OVERLAPPING #-}
         CanReadField "pImageBinds" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pImageBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pImageBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pImageBinds" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pImageBinds}

instance {-# OVERLAPPING #-}
         HasField "signalSemaphoreCount" VkBindSparseInfo where
        type FieldType "signalSemaphoreCount" VkBindSparseInfo = Word32
        type FieldOptional "signalSemaphoreCount" VkBindSparseInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "signalSemaphoreCount" VkBindSparseInfo =
             #{offset VkBindSparseInfo, signalSemaphoreCount}
        type FieldIsArray "signalSemaphoreCount" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanReadField "signalSemaphoreCount" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, signalSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "signalSemaphoreCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "pSignalSemaphores" VkBindSparseInfo where
        type FieldType "pSignalSemaphores" VkBindSparseInfo =
             Ptr VkSemaphore
        type FieldOptional "pSignalSemaphores" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSignalSemaphores" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pSignalSemaphores}
        type FieldIsArray "pSignalSemaphores" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, pSignalSemaphores}

instance {-# OVERLAPPING #-}
         CanReadField "pSignalSemaphores" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pSignalSemaphores})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pSignalSemaphores}

instance {-# OVERLAPPING #-}
         CanWriteField "pSignalSemaphores" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pSignalSemaphores}

instance Show VkBindSparseInfo where
        showsPrec d x
          = showString "VkBindSparseInfo {" .
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
                                      showString "bufferBindCount = " .
                                        showsPrec d (getField @"bufferBindCount" x) .
                                          showString ", " .
                                            showString "pBufferBinds = " .
                                              showsPrec d (getField @"pBufferBinds" x) .
                                                showString ", " .
                                                  showString "imageOpaqueBindCount = " .
                                                    showsPrec d (getField @"imageOpaqueBindCount" x)
                                                      .
                                                      showString ", " .
                                                        showString "pImageOpaqueBinds = " .
                                                          showsPrec d
                                                            (getField @"pImageOpaqueBinds" x)
                                                            .
                                                            showString ", " .
                                                              showString "imageBindCount = " .
                                                                showsPrec d
                                                                  (getField @"imageBindCount" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "pImageBinds = " .
                                                                      showsPrec d
                                                                        (getField @"pImageBinds" x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "signalSemaphoreCount = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"signalSemaphoreCount"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "pSignalSemaphores = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"pSignalSemaphores"
                                                                                       x)
                                                                                    . showChar '}'
