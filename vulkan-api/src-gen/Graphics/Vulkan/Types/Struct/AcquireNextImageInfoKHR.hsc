#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.AcquireNextImageInfoKHR
       (VkAcquireNextImageInfoKHR(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkFence, VkSemaphore,
                                                           VkSwapchainKHR)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkAcquireNextImageInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR swapchain;
--   >     uint64_t                         timeout;
--   >     VkSemaphore semaphore;
--   >     VkFence fence;
--   >     uint32_t                         deviceMask;
--   > } VkAcquireNextImageInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAcquireNextImageInfoKHR VkAcquireNextImageInfoKHR registry at www.khronos.org>
data VkAcquireNextImageInfoKHR = VkAcquireNextImageInfoKHR## Addr##
                                                            ByteArray##

instance Eq VkAcquireNextImageInfoKHR where
        (VkAcquireNextImageInfoKHR## a _) ==
          x@(VkAcquireNextImageInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAcquireNextImageInfoKHR where
        (VkAcquireNextImageInfoKHR## a _) `compare`
          x@(VkAcquireNextImageInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAcquireNextImageInfoKHR where
        sizeOf ~_ = #{size VkAcquireNextImageInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkAcquireNextImageInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAcquireNextImageInfoKHR where
        unsafeAddr (VkAcquireNextImageInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAcquireNextImageInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAcquireNextImageInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAcquireNextImageInfoKHR where
        type StructFields VkAcquireNextImageInfoKHR =
             '["sType", "pNext", "swapchain", "timeout", "semaphore", "fence", -- ' closing tick for hsc2hs
               "deviceMask"]
        type CUnionType VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAcquireNextImageInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkAcquireNextImageInfoKHR where
        type FieldType "sType" VkAcquireNextImageInfoKHR = VkStructureType
        type FieldOptional "sType" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkAcquireNextImageInfoKHR =
             #{offset VkAcquireNextImageInfoKHR, sType}
        type FieldIsArray "sType" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkAcquireNextImageInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkAcquireNextImageInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkAcquireNextImageInfoKHR where
        type FieldType "pNext" VkAcquireNextImageInfoKHR = Ptr Void
        type FieldOptional "pNext" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkAcquireNextImageInfoKHR =
             #{offset VkAcquireNextImageInfoKHR, pNext}
        type FieldIsArray "pNext" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkAcquireNextImageInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkAcquireNextImageInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchain" VkAcquireNextImageInfoKHR where
        type FieldType "swapchain" VkAcquireNextImageInfoKHR =
             VkSwapchainKHR
        type FieldOptional "swapchain" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "swapchain" VkAcquireNextImageInfoKHR =
             #{offset VkAcquireNextImageInfoKHR, swapchain}
        type FieldIsArray "swapchain" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHR, swapchain}

instance {-# OVERLAPPING #-}
         CanReadField "swapchain" VkAcquireNextImageInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHR, swapchain})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHR, swapchain}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchain" VkAcquireNextImageInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHR, swapchain}

instance {-# OVERLAPPING #-}
         HasField "timeout" VkAcquireNextImageInfoKHR where
        type FieldType "timeout" VkAcquireNextImageInfoKHR = Word64
        type FieldOptional "timeout" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "timeout" VkAcquireNextImageInfoKHR =
             #{offset VkAcquireNextImageInfoKHR, timeout}
        type FieldIsArray "timeout" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHR, timeout}

instance {-# OVERLAPPING #-}
         CanReadField "timeout" VkAcquireNextImageInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHR, timeout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHR, timeout}

instance {-# OVERLAPPING #-}
         CanWriteField "timeout" VkAcquireNextImageInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHR, timeout}

instance {-# OVERLAPPING #-}
         HasField "semaphore" VkAcquireNextImageInfoKHR where
        type FieldType "semaphore" VkAcquireNextImageInfoKHR = VkSemaphore
        type FieldOptional "semaphore" VkAcquireNextImageInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkAcquireNextImageInfoKHR =
             #{offset VkAcquireNextImageInfoKHR, semaphore}
        type FieldIsArray "semaphore" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanReadField "semaphore" VkAcquireNextImageInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHR, semaphore})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanWriteField "semaphore" VkAcquireNextImageInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         HasField "fence" VkAcquireNextImageInfoKHR where
        type FieldType "fence" VkAcquireNextImageInfoKHR = VkFence
        type FieldOptional "fence" VkAcquireNextImageInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "fence" VkAcquireNextImageInfoKHR =
             #{offset VkAcquireNextImageInfoKHR, fence}
        type FieldIsArray "fence" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanReadField "fence" VkAcquireNextImageInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHR, fence})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanWriteField "fence" VkAcquireNextImageInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkAcquireNextImageInfoKHR where
        type FieldType "deviceMask" VkAcquireNextImageInfoKHR = Word32
        type FieldOptional "deviceMask" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkAcquireNextImageInfoKHR =
             #{offset VkAcquireNextImageInfoKHR, deviceMask}
        type FieldIsArray "deviceMask" VkAcquireNextImageInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAcquireNextImageInfoKHR, deviceMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkAcquireNextImageInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHR, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHR, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkAcquireNextImageInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHR, deviceMask}

instance Show VkAcquireNextImageInfoKHR where
        showsPrec d x
          = showString "VkAcquireNextImageInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "swapchain = " .
                            showsPrec d (getField @"swapchain" x) .
                              showString ", " .
                                showString "timeout = " .
                                  showsPrec d (getField @"timeout" x) .
                                    showString ", " .
                                      showString "semaphore = " .
                                        showsPrec d (getField @"semaphore" x) .
                                          showString ", " .
                                            showString "fence = " .
                                              showsPrec d (getField @"fence" x) .
                                                showString ", " .
                                                  showString "deviceMask = " .
                                                    showsPrec d (getField @"deviceMask" x) .
                                                      showChar '}'
