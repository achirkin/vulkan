#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkAcquireNextImageInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkAcquireNextImageInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "swapchain" VkAcquireNextImageInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, swapchain})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, swapchain}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchain" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, swapchain}

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

instance {-# OVERLAPPING #-}
         CanReadField "timeout" VkAcquireNextImageInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, timeout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, timeout}

instance {-# OVERLAPPING #-}
         CanWriteField "timeout" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, timeout}

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

instance {-# OVERLAPPING #-}
         CanReadField "semaphore" VkAcquireNextImageInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, semaphore})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, semaphore}

instance {-# OVERLAPPING #-}
         CanWriteField "semaphore" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, semaphore}

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

instance {-# OVERLAPPING #-}
         CanReadField "fence" VkAcquireNextImageInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, fence})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, fence}

instance {-# OVERLAPPING #-}
         CanWriteField "fence" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, fence}

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

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkAcquireNextImageInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, deviceMask}

instance Show VkAcquireNextImageInfoKHX where
        showsPrec d x
          = showString "VkAcquireNextImageInfoKHX {" .
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
