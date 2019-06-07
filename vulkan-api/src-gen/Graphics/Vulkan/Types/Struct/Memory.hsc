#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Memory
       (VkMemoryAllocateFlagsInfo, VkMemoryAllocateFlagsInfo', -- ' closing tick for hsc2hs
        VkMemoryAllocateFlagsInfoKHR, VkMemoryAllocateInfo,
        VkMemoryAllocateInfo', VkMemoryBarrier, VkMemoryBarrier',
        VkMemoryDedicatedAllocateInfo, VkMemoryDedicatedAllocateInfo', -- ' closing tick for hsc2hs
        VkMemoryDedicatedAllocateInfoKHR, VkMemoryDedicatedRequirements,
        VkMemoryDedicatedRequirements', VkMemoryDedicatedRequirementsKHR, -- ' closing tick for hsc2hs
        VkMemoryFdPropertiesKHR, VkMemoryFdPropertiesKHR', -- ' closing tick for hsc2hs
        VkMemoryGetFdInfoKHR, VkMemoryGetFdInfoKHR', VkMemoryHeap, -- ' closing tick for hsc2hs
        VkMemoryHeap', VkMemoryHostPointerPropertiesEXT, -- ' closing tick for hsc2hs
        VkMemoryHostPointerPropertiesEXT', VkMemoryRequirements, -- ' closing tick for hsc2hs
        VkMemoryRequirements', VkMemoryRequirements2, -- ' closing tick for hsc2hs
        VkMemoryRequirements2', VkMemoryRequirements2KHR, VkMemoryType, -- ' closing tick for hsc2hs
        VkMemoryType') -- ' closing tick for hsc2hs
       where
import Foreign.Storable                         (Storable (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes          (VkBool32, VkDeviceSize)
import Graphics.Vulkan.Types.Enum.AccessFlags   (VkAccessFlags)
import Graphics.Vulkan.Types.Enum.External      (VkExternalMemoryHandleTypeFlagBits)
import Graphics.Vulkan.Types.Enum.Memory        (VkMemoryAllocateFlags,
                                                 VkMemoryHeapFlags,
                                                 VkMemoryPropertyFlags)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Handles            (VkBuffer, VkDeviceMemory,
                                                 VkImage)
import System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryAllocateFlagsInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMemoryAllocateFlags flags;
--   >     uint32_t                         deviceMask;
--   > } VkMemoryAllocateFlagsInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryAllocateFlagsInfo VkMemoryAllocateFlagsInfo registry at www.khronos.org>
type VkMemoryAllocateFlagsInfo =
     VulkanStruct VkMemoryAllocateFlagsInfo' -- ' closing tick for hsc2hs

data VkMemoryAllocateFlagsInfo' -- ' closing tick for hsc2hs

instance Eq VkMemoryAllocateFlagsInfo where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryAllocateFlagsInfo where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryAllocateFlagsInfo where
        sizeOf ~_ = #{size VkMemoryAllocateFlagsInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryAllocateFlagsInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryAllocateFlagsInfo where
        type StructFields VkMemoryAllocateFlagsInfo =
             '["sType", "pNext", "flags", "deviceMask"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryAllocateFlagsInfo =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryAllocateFlagsInfo where
        type FieldType "sType" VkMemoryAllocateFlagsInfo = VkStructureType
        type FieldOptional "sType" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryAllocateFlagsInfo =
             #{offset VkMemoryAllocateFlagsInfo, sType}
        type FieldIsArray "sType" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryAllocateFlagsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryAllocateFlagsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryAllocateFlagsInfo where
        type FieldType "pNext" VkMemoryAllocateFlagsInfo = Ptr Void
        type FieldOptional "pNext" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryAllocateFlagsInfo =
             #{offset VkMemoryAllocateFlagsInfo, pNext}
        type FieldIsArray "pNext" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryAllocateFlagsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryAllocateFlagsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkMemoryAllocateFlagsInfo where
        type FieldType "flags" VkMemoryAllocateFlagsInfo =
             VkMemoryAllocateFlags
        type FieldOptional "flags" VkMemoryAllocateFlagsInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMemoryAllocateFlagsInfo =
             #{offset VkMemoryAllocateFlagsInfo, flags}
        type FieldIsArray "flags" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkMemoryAllocateFlagsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkMemoryAllocateFlagsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkMemoryAllocateFlagsInfo where
        type FieldType "deviceMask" VkMemoryAllocateFlagsInfo = Word32
        type FieldOptional "deviceMask" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkMemoryAllocateFlagsInfo =
             #{offset VkMemoryAllocateFlagsInfo, deviceMask}
        type FieldIsArray "deviceMask" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkMemoryAllocateFlagsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfo, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkMemoryAllocateFlagsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfo, deviceMask}

instance Show VkMemoryAllocateFlagsInfo where
        showsPrec d x
          = showString "VkMemoryAllocateFlagsInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "deviceMask = " .
                                  showsPrec d (getField @"deviceMask" x) . showChar '}'

-- | Alias for `VkMemoryAllocateFlagsInfo`
type VkMemoryAllocateFlagsInfoKHR = VkMemoryAllocateFlagsInfo

-- | > typedef struct VkMemoryAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDeviceSize           allocationSize;
--   >     uint32_t               memoryTypeIndex;
--   > } VkMemoryAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryAllocateInfo VkMemoryAllocateInfo registry at www.khronos.org>
type VkMemoryAllocateInfo = VulkanStruct VkMemoryAllocateInfo' -- ' closing tick for hsc2hs

data VkMemoryAllocateInfo' -- ' closing tick for hsc2hs

instance Eq VkMemoryAllocateInfo where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryAllocateInfo where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryAllocateInfo where
        sizeOf ~_ = #{size VkMemoryAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryAllocateInfo where
        type StructFields VkMemoryAllocateInfo =
             '["sType", "pNext", "allocationSize", "memoryTypeIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryAllocateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkMemoryAllocateInfo
         where
        type FieldType "sType" VkMemoryAllocateInfo = VkStructureType
        type FieldOptional "sType" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, sType}
        type FieldIsArray "sType" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkMemoryAllocateInfo
         where
        type FieldType "pNext" VkMemoryAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, pNext}
        type FieldIsArray "pNext" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "allocationSize" VkMemoryAllocateInfo where
        type FieldType "allocationSize" VkMemoryAllocateInfo = VkDeviceSize
        type FieldOptional "allocationSize" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "allocationSize" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, allocationSize}
        type FieldIsArray "allocationSize" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateInfo, allocationSize}

instance {-# OVERLAPPING #-}
         CanReadField "allocationSize" VkMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, allocationSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateInfo, allocationSize}

instance {-# OVERLAPPING #-}
         CanWriteField "allocationSize" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, allocationSize}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeIndex" VkMemoryAllocateInfo where
        type FieldType "memoryTypeIndex" VkMemoryAllocateInfo = Word32
        type FieldOptional "memoryTypeIndex" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeIndex" VkMemoryAllocateInfo =
             #{offset VkMemoryAllocateInfo, memoryTypeIndex}
        type FieldIsArray "memoryTypeIndex" VkMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateInfo, memoryTypeIndex}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeIndex" VkMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateInfo, memoryTypeIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateInfo, memoryTypeIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeIndex" VkMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateInfo, memoryTypeIndex}

instance Show VkMemoryAllocateInfo where
        showsPrec d x
          = showString "VkMemoryAllocateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "allocationSize = " .
                            showsPrec d (getField @"allocationSize" x) .
                              showString ", " .
                                showString "memoryTypeIndex = " .
                                  showsPrec d (getField @"memoryTypeIndex" x) . showChar '}'

-- | > typedef struct VkMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   > } VkMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryBarrier VkMemoryBarrier registry at www.khronos.org>
type VkMemoryBarrier = VulkanStruct VkMemoryBarrier' -- ' closing tick for hsc2hs

data VkMemoryBarrier' -- ' closing tick for hsc2hs

instance Eq VkMemoryBarrier where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryBarrier where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryBarrier where
        sizeOf ~_ = #{size VkMemoryBarrier}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryBarrier}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryBarrier where
        type StructFields VkMemoryBarrier =
             '["sType", "pNext", "srcAccessMask", "dstAccessMask"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryBarrier = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkMemoryBarrier where
        type FieldType "sType" VkMemoryBarrier = VkStructureType
        type FieldOptional "sType" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryBarrier =
             #{offset VkMemoryBarrier, sType}
        type FieldIsArray "sType" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryBarrier, sType}

instance {-# OVERLAPPING #-} CanReadField "sType" VkMemoryBarrier
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryBarrier, sType}

instance {-# OVERLAPPING #-} CanWriteField "sType" VkMemoryBarrier
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryBarrier, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkMemoryBarrier where
        type FieldType "pNext" VkMemoryBarrier = Ptr Void
        type FieldOptional "pNext" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryBarrier =
             #{offset VkMemoryBarrier, pNext}
        type FieldIsArray "pNext" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryBarrier, pNext}

instance {-# OVERLAPPING #-} CanReadField "pNext" VkMemoryBarrier
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryBarrier, pNext}

instance {-# OVERLAPPING #-} CanWriteField "pNext" VkMemoryBarrier
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         HasField "srcAccessMask" VkMemoryBarrier where
        type FieldType "srcAccessMask" VkMemoryBarrier = VkAccessFlags
        type FieldOptional "srcAccessMask" VkMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "srcAccessMask" VkMemoryBarrier =
             #{offset VkMemoryBarrier, srcAccessMask}
        type FieldIsArray "srcAccessMask" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "srcAccessMask" VkMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, srcAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "srcAccessMask" VkMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dstAccessMask" VkMemoryBarrier where
        type FieldType "dstAccessMask" VkMemoryBarrier = VkAccessFlags
        type FieldOptional "dstAccessMask" VkMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dstAccessMask" VkMemoryBarrier =
             #{offset VkMemoryBarrier, dstAccessMask}
        type FieldIsArray "dstAccessMask" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "dstAccessMask" VkMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, dstAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "dstAccessMask" VkMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryBarrier, dstAccessMask}

instance Show VkMemoryBarrier where
        showsPrec d x
          = showString "VkMemoryBarrier {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "srcAccessMask = " .
                            showsPrec d (getField @"srcAccessMask" x) .
                              showString ", " .
                                showString "dstAccessMask = " .
                                  showsPrec d (getField @"dstAccessMask" x) . showChar '}'

-- | > typedef struct VkMemoryDedicatedAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage          image;
--   >     VkBuffer         buffer;
--   > } VkMemoryDedicatedAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryDedicatedAllocateInfo VkMemoryDedicatedAllocateInfo registry at www.khronos.org>
type VkMemoryDedicatedAllocateInfo =
     VulkanStruct VkMemoryDedicatedAllocateInfo' -- ' closing tick for hsc2hs

data VkMemoryDedicatedAllocateInfo' -- ' closing tick for hsc2hs

instance Eq VkMemoryDedicatedAllocateInfo where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryDedicatedAllocateInfo where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryDedicatedAllocateInfo where
        sizeOf ~_ = #{size VkMemoryDedicatedAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryDedicatedAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryDedicatedAllocateInfo where
        type StructFields VkMemoryDedicatedAllocateInfo =
             '["sType", "pNext", "image", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryDedicatedAllocateInfo =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryDedicatedAllocateInfo where
        type FieldType "sType" VkMemoryDedicatedAllocateInfo =
             VkStructureType
        type FieldOptional "sType" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryDedicatedAllocateInfo =
             #{offset VkMemoryDedicatedAllocateInfo, sType}
        type FieldIsArray "sType" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryDedicatedAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryDedicatedAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryDedicatedAllocateInfo where
        type FieldType "pNext" VkMemoryDedicatedAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryDedicatedAllocateInfo =
             #{offset VkMemoryDedicatedAllocateInfo, pNext}
        type FieldIsArray "pNext" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryDedicatedAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryDedicatedAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "image" VkMemoryDedicatedAllocateInfo where
        type FieldType "image" VkMemoryDedicatedAllocateInfo = VkImage
        type FieldOptional "image" VkMemoryDedicatedAllocateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "image" VkMemoryDedicatedAllocateInfo =
             #{offset VkMemoryDedicatedAllocateInfo, image}
        type FieldIsArray "image" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfo, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkMemoryDedicatedAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfo, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfo, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkMemoryDedicatedAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfo, image}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkMemoryDedicatedAllocateInfo where
        type FieldType "buffer" VkMemoryDedicatedAllocateInfo = VkBuffer
        type FieldOptional "buffer" VkMemoryDedicatedAllocateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkMemoryDedicatedAllocateInfo =
             #{offset VkMemoryDedicatedAllocateInfo, buffer}
        type FieldIsArray "buffer" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfo, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkMemoryDedicatedAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfo, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfo, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkMemoryDedicatedAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfo, buffer}

instance Show VkMemoryDedicatedAllocateInfo where
        showsPrec d x
          = showString "VkMemoryDedicatedAllocateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "image = " .
                            showsPrec d (getField @"image" x) .
                              showString ", " .
                                showString "buffer = " .
                                  showsPrec d (getField @"buffer" x) . showChar '}'

-- | Alias for `VkMemoryDedicatedAllocateInfo`
type VkMemoryDedicatedAllocateInfoKHR =
     VkMemoryDedicatedAllocateInfo

-- | > typedef struct VkMemoryDedicatedRequirements {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         prefersDedicatedAllocation;
--   >     VkBool32                         requiresDedicatedAllocation;
--   > } VkMemoryDedicatedRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryDedicatedRequirements VkMemoryDedicatedRequirements registry at www.khronos.org>
type VkMemoryDedicatedRequirements =
     VulkanStruct VkMemoryDedicatedRequirements' -- ' closing tick for hsc2hs

data VkMemoryDedicatedRequirements' -- ' closing tick for hsc2hs

instance Eq VkMemoryDedicatedRequirements where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryDedicatedRequirements where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryDedicatedRequirements where
        sizeOf ~_ = #{size VkMemoryDedicatedRequirements}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryDedicatedRequirements}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryDedicatedRequirements where
        type StructFields VkMemoryDedicatedRequirements =
             '["sType", "pNext", "prefersDedicatedAllocation", -- ' closing tick for hsc2hs
               "requiresDedicatedAllocation"]
        type CUnionType VkMemoryDedicatedRequirements = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryDedicatedRequirements = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryDedicatedRequirements =
             '[VkMemoryRequirements2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryDedicatedRequirements where
        type FieldType "sType" VkMemoryDedicatedRequirements =
             VkStructureType
        type FieldOptional "sType" VkMemoryDedicatedRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryDedicatedRequirements =
             #{offset VkMemoryDedicatedRequirements, sType}
        type FieldIsArray "sType" VkMemoryDedicatedRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirements, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryDedicatedRequirements where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirements, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedRequirements, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryDedicatedRequirements where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirements, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryDedicatedRequirements where
        type FieldType "pNext" VkMemoryDedicatedRequirements = Ptr Void
        type FieldOptional "pNext" VkMemoryDedicatedRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryDedicatedRequirements =
             #{offset VkMemoryDedicatedRequirements, pNext}
        type FieldIsArray "pNext" VkMemoryDedicatedRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirements, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryDedicatedRequirements where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirements, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedRequirements, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryDedicatedRequirements where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirements, pNext}

instance {-# OVERLAPPING #-}
         HasField "prefersDedicatedAllocation" VkMemoryDedicatedRequirements
         where
        type FieldType "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = VkBool32
        type FieldOptional "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirements
             =
             #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation}
        type FieldIsArray "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "prefersDedicatedAllocation"
           VkMemoryDedicatedRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "prefersDedicatedAllocation"
           VkMemoryDedicatedRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation}

instance {-# OVERLAPPING #-}
         HasField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirements
         where
        type FieldType "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = VkBool32
        type FieldOptional "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirements
             =
             #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation}
        type FieldIsArray "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation}

instance Show VkMemoryDedicatedRequirements where
        showsPrec d x
          = showString "VkMemoryDedicatedRequirements {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "prefersDedicatedAllocation = " .
                            showsPrec d (getField @"prefersDedicatedAllocation" x) .
                              showString ", " .
                                showString "requiresDedicatedAllocation = " .
                                  showsPrec d (getField @"requiresDedicatedAllocation" x) .
                                    showChar '}'

-- | Alias for `VkMemoryDedicatedRequirements`
type VkMemoryDedicatedRequirementsKHR =
     VkMemoryDedicatedRequirements

-- | > typedef struct VkMemoryFdPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         memoryTypeBits;
--   > } VkMemoryFdPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryFdPropertiesKHR VkMemoryFdPropertiesKHR registry at www.khronos.org>
type VkMemoryFdPropertiesKHR =
     VulkanStruct VkMemoryFdPropertiesKHR' -- ' closing tick for hsc2hs

data VkMemoryFdPropertiesKHR' -- ' closing tick for hsc2hs

instance Eq VkMemoryFdPropertiesKHR where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryFdPropertiesKHR where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryFdPropertiesKHR where
        sizeOf ~_ = #{size VkMemoryFdPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryFdPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryFdPropertiesKHR where
        type StructFields VkMemoryFdPropertiesKHR =
             '["sType", "pNext", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryFdPropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryFdPropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryFdPropertiesKHR where
        type FieldType "sType" VkMemoryFdPropertiesKHR = VkStructureType
        type FieldOptional "sType" VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryFdPropertiesKHR =
             #{offset VkMemoryFdPropertiesKHR, sType}
        type FieldIsArray "sType" VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryFdPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryFdPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryFdPropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryFdPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryFdPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryFdPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryFdPropertiesKHR where
        type FieldType "pNext" VkMemoryFdPropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryFdPropertiesKHR =
             #{offset VkMemoryFdPropertiesKHR, pNext}
        type FieldIsArray "pNext" VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryFdPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryFdPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryFdPropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryFdPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryFdPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryFdPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryFdPropertiesKHR where
        type FieldType "memoryTypeBits" VkMemoryFdPropertiesKHR = Word32
        type FieldOptional "memoryTypeBits" VkMemoryFdPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits" VkMemoryFdPropertiesKHR =
             #{offset VkMemoryFdPropertiesKHR, memoryTypeBits}
        type FieldIsArray "memoryTypeBits" VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryFdPropertiesKHR, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeBits" VkMemoryFdPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryFdPropertiesKHR, memoryTypeBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryFdPropertiesKHR, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeBits" VkMemoryFdPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryFdPropertiesKHR, memoryTypeBits}

instance Show VkMemoryFdPropertiesKHR where
        showsPrec d x
          = showString "VkMemoryFdPropertiesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryTypeBits = " .
                            showsPrec d (getField @"memoryTypeBits" x) . showChar '}'

-- | > typedef struct VkMemoryGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceMemory                   memory;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   > } VkMemoryGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryGetFdInfoKHR VkMemoryGetFdInfoKHR registry at www.khronos.org>
type VkMemoryGetFdInfoKHR = VulkanStruct VkMemoryGetFdInfoKHR' -- ' closing tick for hsc2hs

data VkMemoryGetFdInfoKHR' -- ' closing tick for hsc2hs

instance Eq VkMemoryGetFdInfoKHR where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryGetFdInfoKHR where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryGetFdInfoKHR where
        sizeOf ~_ = #{size VkMemoryGetFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryGetFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryGetFdInfoKHR where
        type StructFields VkMemoryGetFdInfoKHR =
             '["sType", "pNext", "memory", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryGetFdInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkMemoryGetFdInfoKHR
         where
        type FieldType "sType" VkMemoryGetFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, sType}
        type FieldIsArray "sType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkMemoryGetFdInfoKHR
         where
        type FieldType "pNext" VkMemoryGetFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasField "memory" VkMemoryGetFdInfoKHR
         where
        type FieldType "memory" VkMemoryGetFdInfoKHR = VkDeviceMemory
        type FieldOptional "memory" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, memory}
        type FieldIsArray "memory" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryGetFdInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkMemoryGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, memory}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkMemoryGetFdInfoKHR where
        type FieldType "handleType" VkMemoryGetFdInfoKHR =
             VkExternalMemoryHandleTypeFlagBits
        type FieldOptional "handleType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkMemoryGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, handleType}

instance Show VkMemoryGetFdInfoKHR where
        showsPrec d x
          = showString "VkMemoryGetFdInfoKHR {" .
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

-- | > typedef struct VkMemoryHeap {
--   >     VkDeviceSize           size;
--   >     VkMemoryHeapFlags      flags;
--   > } VkMemoryHeap;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryHeap VkMemoryHeap registry at www.khronos.org>
type VkMemoryHeap = VulkanStruct VkMemoryHeap' -- ' closing tick for hsc2hs

data VkMemoryHeap' -- ' closing tick for hsc2hs

instance Eq VkMemoryHeap where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryHeap where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryHeap where
        sizeOf ~_ = #{size VkMemoryHeap}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryHeap}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryHeap where
        type StructFields VkMemoryHeap = '["size", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryHeap = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryHeap = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryHeap = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "size" VkMemoryHeap where
        type FieldType "size" VkMemoryHeap = VkDeviceSize
        type FieldOptional "size" VkMemoryHeap = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkMemoryHeap =
             #{offset VkMemoryHeap, size}
        type FieldIsArray "size" VkMemoryHeap = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryHeap, size}

instance {-# OVERLAPPING #-} CanReadField "size" VkMemoryHeap where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHeap, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryHeap, size}

instance {-# OVERLAPPING #-} CanWriteField "size" VkMemoryHeap
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryHeap, size}

instance {-# OVERLAPPING #-} HasField "flags" VkMemoryHeap where
        type FieldType "flags" VkMemoryHeap = VkMemoryHeapFlags
        type FieldOptional "flags" VkMemoryHeap = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMemoryHeap =
             #{offset VkMemoryHeap, flags}
        type FieldIsArray "flags" VkMemoryHeap = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryHeap, flags}

instance {-# OVERLAPPING #-} CanReadField "flags" VkMemoryHeap
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHeap, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryHeap, flags}

instance {-# OVERLAPPING #-} CanWriteField "flags" VkMemoryHeap
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryHeap, flags}

instance Show VkMemoryHeap where
        showsPrec d x
          = showString "VkMemoryHeap {" .
              showString "size = " .
                showsPrec d (getField @"size" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) . showChar '}'

-- | > typedef struct VkMemoryHostPointerPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     uint32_t memoryTypeBits;
--   > } VkMemoryHostPointerPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryHostPointerPropertiesEXT VkMemoryHostPointerPropertiesEXT registry at www.khronos.org>
type VkMemoryHostPointerPropertiesEXT =
     VulkanStruct VkMemoryHostPointerPropertiesEXT' -- ' closing tick for hsc2hs

data VkMemoryHostPointerPropertiesEXT' -- ' closing tick for hsc2hs

instance Eq VkMemoryHostPointerPropertiesEXT where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryHostPointerPropertiesEXT where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryHostPointerPropertiesEXT where
        sizeOf ~_ = #{size VkMemoryHostPointerPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryHostPointerPropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryHostPointerPropertiesEXT where
        type StructFields VkMemoryHostPointerPropertiesEXT =
             '["sType", "pNext", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryHostPointerPropertiesEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryHostPointerPropertiesEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryHostPointerPropertiesEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryHostPointerPropertiesEXT where
        type FieldType "sType" VkMemoryHostPointerPropertiesEXT =
             VkStructureType
        type FieldOptional "sType" VkMemoryHostPointerPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryHostPointerPropertiesEXT =
             #{offset VkMemoryHostPointerPropertiesEXT, sType}
        type FieldIsArray "sType" VkMemoryHostPointerPropertiesEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryHostPointerPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryHostPointerPropertiesEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryHostPointerPropertiesEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryHostPointerPropertiesEXT where
        type FieldType "pNext" VkMemoryHostPointerPropertiesEXT = Ptr Void
        type FieldOptional "pNext" VkMemoryHostPointerPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryHostPointerPropertiesEXT =
             #{offset VkMemoryHostPointerPropertiesEXT, pNext}
        type FieldIsArray "pNext" VkMemoryHostPointerPropertiesEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryHostPointerPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryHostPointerPropertiesEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryHostPointerPropertiesEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryHostPointerPropertiesEXT where
        type FieldType "memoryTypeBits" VkMemoryHostPointerPropertiesEXT =
             Word32
        type FieldOptional "memoryTypeBits"
               VkMemoryHostPointerPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits" VkMemoryHostPointerPropertiesEXT
             =
             #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}
        type FieldIsArray "memoryTypeBits" VkMemoryHostPointerPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeBits" VkMemoryHostPointerPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeBits" VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

instance Show VkMemoryHostPointerPropertiesEXT where
        showsPrec d x
          = showString "VkMemoryHostPointerPropertiesEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryTypeBits = " .
                            showsPrec d (getField @"memoryTypeBits" x) . showChar '}'

-- | > typedef struct VkMemoryRequirements {
--   >     VkDeviceSize           size;
--   >     VkDeviceSize           alignment;
--   >     uint32_t               memoryTypeBits;
--   > } VkMemoryRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryRequirements VkMemoryRequirements registry at www.khronos.org>
type VkMemoryRequirements = VulkanStruct VkMemoryRequirements' -- ' closing tick for hsc2hs

data VkMemoryRequirements' -- ' closing tick for hsc2hs

instance Eq VkMemoryRequirements where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryRequirements where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryRequirements where
        sizeOf ~_ = #{size VkMemoryRequirements}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryRequirements}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryRequirements where
        type StructFields VkMemoryRequirements =
             '["size", "alignment", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryRequirements = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryRequirements = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "size" VkMemoryRequirements
         where
        type FieldType "size" VkMemoryRequirements = VkDeviceSize
        type FieldOptional "size" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkMemoryRequirements =
             #{offset VkMemoryRequirements, size}
        type FieldIsArray "size" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements, size}

instance {-# OVERLAPPING #-}
         CanReadField "size" VkMemoryRequirements where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkMemoryRequirements where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements, size}

instance {-# OVERLAPPING #-}
         HasField "alignment" VkMemoryRequirements where
        type FieldType "alignment" VkMemoryRequirements = VkDeviceSize
        type FieldOptional "alignment" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "alignment" VkMemoryRequirements =
             #{offset VkMemoryRequirements, alignment}
        type FieldIsArray "alignment" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements, alignment}

instance {-# OVERLAPPING #-}
         CanReadField "alignment" VkMemoryRequirements where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements, alignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements, alignment}

instance {-# OVERLAPPING #-}
         CanWriteField "alignment" VkMemoryRequirements where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements, alignment}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryRequirements where
        type FieldType "memoryTypeBits" VkMemoryRequirements = Word32
        type FieldOptional "memoryTypeBits" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits" VkMemoryRequirements =
             #{offset VkMemoryRequirements, memoryTypeBits}
        type FieldIsArray "memoryTypeBits" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryRequirements, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeBits" VkMemoryRequirements where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements, memoryTypeBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeBits" VkMemoryRequirements where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements, memoryTypeBits}

instance Show VkMemoryRequirements where
        showsPrec d x
          = showString "VkMemoryRequirements {" .
              showString "size = " .
                showsPrec d (getField @"size" x) .
                  showString ", " .
                    showString "alignment = " .
                      showsPrec d (getField @"alignment" x) .
                        showString ", " .
                          showString "memoryTypeBits = " .
                            showsPrec d (getField @"memoryTypeBits" x) . showChar '}'

-- | > typedef struct VkMemoryRequirements2 {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkMemoryRequirements                                                 memoryRequirements;
--   > } VkMemoryRequirements2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryRequirements2 VkMemoryRequirements2 registry at www.khronos.org>
type VkMemoryRequirements2 = VulkanStruct VkMemoryRequirements2' -- ' closing tick for hsc2hs

data VkMemoryRequirements2' -- ' closing tick for hsc2hs

instance Eq VkMemoryRequirements2 where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryRequirements2 where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryRequirements2 where
        sizeOf ~_ = #{size VkMemoryRequirements2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryRequirements2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryRequirements2 where
        type StructFields VkMemoryRequirements2 =
             '["sType", "pNext", "memoryRequirements"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryRequirements2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryRequirements2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryRequirements2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkMemoryRequirements2
         where
        type FieldType "sType" VkMemoryRequirements2 = VkStructureType
        type FieldOptional "sType" VkMemoryRequirements2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryRequirements2 =
             #{offset VkMemoryRequirements2, sType}
        type FieldIsArray "sType" VkMemoryRequirements2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryRequirements2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryRequirements2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements2, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkMemoryRequirements2
         where
        type FieldType "pNext" VkMemoryRequirements2 = Ptr Void
        type FieldOptional "pNext" VkMemoryRequirements2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryRequirements2 =
             #{offset VkMemoryRequirements2, pNext}
        type FieldIsArray "pNext" VkMemoryRequirements2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryRequirements2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryRequirements2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryRequirements" VkMemoryRequirements2 where
        type FieldType "memoryRequirements" VkMemoryRequirements2 =
             VkMemoryRequirements
        type FieldOptional "memoryRequirements" VkMemoryRequirements2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryRequirements" VkMemoryRequirements2 =
             #{offset VkMemoryRequirements2, memoryRequirements}
        type FieldIsArray "memoryRequirements" VkMemoryRequirements2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryRequirements2, memoryRequirements}

instance {-# OVERLAPPING #-}
         CanReadField "memoryRequirements" VkMemoryRequirements2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2, memoryRequirements})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements2, memoryRequirements}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryRequirements" VkMemoryRequirements2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements2, memoryRequirements}

instance Show VkMemoryRequirements2 where
        showsPrec d x
          = showString "VkMemoryRequirements2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryRequirements = " .
                            showsPrec d (getField @"memoryRequirements" x) . showChar '}'

-- | Alias for `VkMemoryRequirements2`
type VkMemoryRequirements2KHR = VkMemoryRequirements2

-- | > typedef struct VkMemoryType {
--   >     VkMemoryPropertyFlags  propertyFlags;
--   >     uint32_t               heapIndex;
--   > } VkMemoryType;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryType VkMemoryType registry at www.khronos.org>
type VkMemoryType = VulkanStruct VkMemoryType' -- ' closing tick for hsc2hs

data VkMemoryType' -- ' closing tick for hsc2hs

instance Eq VkMemoryType where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkMemoryType where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkMemoryType where
        sizeOf ~_ = #{size VkMemoryType}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryType}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryType where
        type StructFields VkMemoryType = '["propertyFlags", "heapIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryType = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryType = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryType = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "propertyFlags" VkMemoryType
         where
        type FieldType "propertyFlags" VkMemoryType = VkMemoryPropertyFlags
        type FieldOptional "propertyFlags" VkMemoryType = 'True -- ' closing tick for hsc2hs
        type FieldOffset "propertyFlags" VkMemoryType =
             #{offset VkMemoryType, propertyFlags}
        type FieldIsArray "propertyFlags" VkMemoryType = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryType, propertyFlags}

instance {-# OVERLAPPING #-}
         CanReadField "propertyFlags" VkMemoryType where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryType, propertyFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryType, propertyFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "propertyFlags" VkMemoryType where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryType, propertyFlags}

instance {-# OVERLAPPING #-} HasField "heapIndex" VkMemoryType
         where
        type FieldType "heapIndex" VkMemoryType = Word32
        type FieldOptional "heapIndex" VkMemoryType = 'False -- ' closing tick for hsc2hs
        type FieldOffset "heapIndex" VkMemoryType =
             #{offset VkMemoryType, heapIndex}
        type FieldIsArray "heapIndex" VkMemoryType = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryType, heapIndex}

instance {-# OVERLAPPING #-} CanReadField "heapIndex" VkMemoryType
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryType, heapIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryType, heapIndex}

instance {-# OVERLAPPING #-} CanWriteField "heapIndex" VkMemoryType
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryType, heapIndex}

instance Show VkMemoryType where
        showsPrec d x
          = showString "VkMemoryType {" .
              showString "propertyFlags = " .
                showsPrec d (getField @"propertyFlags" x) .
                  showString ", " .
                    showString "heapIndex = " .
                      showsPrec d (getField @"heapIndex" x) . showChar '}'
