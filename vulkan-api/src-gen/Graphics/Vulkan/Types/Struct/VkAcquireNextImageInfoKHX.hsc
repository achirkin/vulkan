#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAcquireNextImageInfoKHX
       (VkAcquireNextImageInfoKHX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkFence,
                                                             VkSemaphore,
                                                             VkSwapchainKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkAcquireNextImageInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR swapchain;
--   >     uint64_t                         timeout;
--   >     VkSemaphore semaphore;
--   >     VkFence fence;
--   >     uint32_t                         deviceMask;
--   > } VkAcquireNextImageInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkAcquireNextImageInfoKHX.html VkAcquireNextImageInfoKHX registry at www.khronos.org>
data VkAcquireNextImageInfoKHX = VkAcquireNextImageInfoKHX## Addr##
                                                            ByteArray##

instance Eq VkAcquireNextImageInfoKHX where
        (VkAcquireNextImageInfoKHX## a _) ==
          x@(VkAcquireNextImageInfoKHX## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAcquireNextImageInfoKHX where
        (VkAcquireNextImageInfoKHX## a _) `compare`
          x@(VkAcquireNextImageInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAcquireNextImageInfoKHX where
        sizeOf ~_ = #{size VkAcquireNextImageInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkAcquireNextImageInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAcquireNextImageInfoKHX where
        unsafeAddr (VkAcquireNextImageInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAcquireNextImageInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAcquireNextImageInfoKHX## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAcquireNextImageInfoKHX where
        type StructFields VkAcquireNextImageInfoKHX =
             '["sType", "pNext", "swapchain", "timeout", "semaphore", "fence", -- ' closing tick for hsc2hs
               "deviceMask"]
        type CUnionType VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAcquireNextImageInfoKHX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkAcquireNextImageInfoKHX
         where
        type VkSTypeMType VkAcquireNextImageInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkAcquireNextImageInfoKHX where
        type FieldType "sType" VkAcquireNextImageInfoKHX = VkStructureType
        type FieldOptional "sType" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkAcquireNextImageInfoKHX =
             #{offset VkAcquireNextImageInfoKHX, sType}
        type FieldIsArray "sType" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHX, sType}

instance CanReadField "sType" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkAcquireNextImageInfoKHX
         where
        type VkPNextMType VkAcquireNextImageInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkAcquireNextImageInfoKHX where
        type FieldType "pNext" VkAcquireNextImageInfoKHX = Ptr Void
        type FieldOptional "pNext" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkAcquireNextImageInfoKHX =
             #{offset VkAcquireNextImageInfoKHX, pNext}
        type FieldIsArray "pNext" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHX, pNext}

instance CanReadField "pNext" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSwapchain VkAcquireNextImageInfoKHX where
        type VkSwapchainMType VkAcquireNextImageInfoKHX = VkSwapchainKHR

        {-# NOINLINE vkSwapchain #-}
        vkSwapchain x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, swapchain})

        {-# INLINE vkSwapchainByteOffset #-}
        vkSwapchainByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, swapchain}

        {-# INLINE readVkSwapchain #-}
        readVkSwapchain p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, swapchain}

        {-# INLINE writeVkSwapchain #-}
        writeVkSwapchain p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, swapchain}

instance {-# OVERLAPPING #-}
         HasField "swapchain" VkAcquireNextImageInfoKHX where
        type FieldType "swapchain" VkAcquireNextImageInfoKHX =
             VkSwapchainKHR
        type FieldOptional "swapchain" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "swapchain" VkAcquireNextImageInfoKHX =
             #{offset VkAcquireNextImageInfoKHX, swapchain}
        type FieldIsArray "swapchain" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHX, swapchain}

instance CanReadField "swapchain" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkSwapchain

        {-# INLINE readField #-}
        readField = readVkSwapchain

instance CanWriteField "swapchain" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSwapchain

instance {-# OVERLAPPING #-} HasVkTimeout VkAcquireNextImageInfoKHX
         where
        type VkTimeoutMType VkAcquireNextImageInfoKHX = Word64

        {-# NOINLINE vkTimeout #-}
        vkTimeout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, timeout})

        {-# INLINE vkTimeoutByteOffset #-}
        vkTimeoutByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, timeout}

        {-# INLINE readVkTimeout #-}
        readVkTimeout p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, timeout}

        {-# INLINE writeVkTimeout #-}
        writeVkTimeout p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, timeout}

instance {-# OVERLAPPING #-}
         HasField "timeout" VkAcquireNextImageInfoKHX where
        type FieldType "timeout" VkAcquireNextImageInfoKHX = Word64
        type FieldOptional "timeout" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "timeout" VkAcquireNextImageInfoKHX =
             #{offset VkAcquireNextImageInfoKHX, timeout}
        type FieldIsArray "timeout" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHX, timeout}

instance CanReadField "timeout" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkTimeout

        {-# INLINE readField #-}
        readField = readVkTimeout

instance CanWriteField "timeout" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkTimeout

instance {-# OVERLAPPING #-}
         HasVkSemaphore VkAcquireNextImageInfoKHX where
        type VkSemaphoreMType VkAcquireNextImageInfoKHX = VkSemaphore

        {-# NOINLINE vkSemaphore #-}
        vkSemaphore x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, semaphore})

        {-# INLINE vkSemaphoreByteOffset #-}
        vkSemaphoreByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, semaphore}

        {-# INLINE readVkSemaphore #-}
        readVkSemaphore p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, semaphore}

        {-# INLINE writeVkSemaphore #-}
        writeVkSemaphore p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, semaphore}

instance {-# OVERLAPPING #-}
         HasField "semaphore" VkAcquireNextImageInfoKHX where
        type FieldType "semaphore" VkAcquireNextImageInfoKHX = VkSemaphore
        type FieldOptional "semaphore" VkAcquireNextImageInfoKHX = 'True -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkAcquireNextImageInfoKHX =
             #{offset VkAcquireNextImageInfoKHX, semaphore}
        type FieldIsArray "semaphore" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHX, semaphore}

instance CanReadField "semaphore" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkSemaphore

        {-# INLINE readField #-}
        readField = readVkSemaphore

instance CanWriteField "semaphore" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSemaphore

instance {-# OVERLAPPING #-} HasVkFence VkAcquireNextImageInfoKHX
         where
        type VkFenceMType VkAcquireNextImageInfoKHX = VkFence

        {-# NOINLINE vkFence #-}
        vkFence x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, fence})

        {-# INLINE vkFenceByteOffset #-}
        vkFenceByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, fence}

        {-# INLINE readVkFence #-}
        readVkFence p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, fence}

        {-# INLINE writeVkFence #-}
        writeVkFence p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, fence}

instance {-# OVERLAPPING #-}
         HasField "fence" VkAcquireNextImageInfoKHX where
        type FieldType "fence" VkAcquireNextImageInfoKHX = VkFence
        type FieldOptional "fence" VkAcquireNextImageInfoKHX = 'True -- ' closing tick for hsc2hs
        type FieldOffset "fence" VkAcquireNextImageInfoKHX =
             #{offset VkAcquireNextImageInfoKHX, fence}
        type FieldIsArray "fence" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHX, fence}

instance CanReadField "fence" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkFence

        {-# INLINE readField #-}
        readField = readVkFence

instance CanWriteField "fence" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkFence

instance {-# OVERLAPPING #-}
         HasVkDeviceMask VkAcquireNextImageInfoKHX where
        type VkDeviceMaskMType VkAcquireNextImageInfoKHX = Word32

        {-# NOINLINE vkDeviceMask #-}
        vkDeviceMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, deviceMask})

        {-# INLINE vkDeviceMaskByteOffset #-}
        vkDeviceMaskByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, deviceMask}

        {-# INLINE readVkDeviceMask #-}
        readVkDeviceMask p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, deviceMask}

        {-# INLINE writeVkDeviceMask #-}
        writeVkDeviceMask p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkAcquireNextImageInfoKHX where
        type FieldType "deviceMask" VkAcquireNextImageInfoKHX = Word32
        type FieldOptional "deviceMask" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkAcquireNextImageInfoKHX =
             #{offset VkAcquireNextImageInfoKHX, deviceMask}
        type FieldIsArray "deviceMask" VkAcquireNextImageInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHX, deviceMask}

instance CanReadField "deviceMask" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkDeviceMask

        {-# INLINE readField #-}
        readField = readVkDeviceMask

instance CanWriteField "deviceMask" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceMask

instance Show VkAcquireNextImageInfoKHX where
        showsPrec d x
          = showString "VkAcquireNextImageInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSwapchain = " .
                            showsPrec d (vkSwapchain x) .
                              showString ", " .
                                showString "vkTimeout = " .
                                  showsPrec d (vkTimeout x) .
                                    showString ", " .
                                      showString "vkSemaphore = " .
                                        showsPrec d (vkSemaphore x) .
                                          showString ", " .
                                            showString "vkFence = " .
                                              showsPrec d (vkFence x) .
                                                showString ", " .
                                                  showString "vkDeviceMask = " .
                                                    showsPrec d (vkDeviceMask x) . showChar '}'
