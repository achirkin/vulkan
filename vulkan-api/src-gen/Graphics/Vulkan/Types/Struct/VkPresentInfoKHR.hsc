#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPresentInfoKHR
       (VkPresentInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkResult        (VkResult)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkSemaphore,
                                                             VkSwapchainKHR)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPresentInfoKHR {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     uint32_t         waitSemaphoreCount;
--   >     const VkSemaphore* pWaitSemaphores;
--   >     uint32_t                         swapchainCount;
--   >     const VkSwapchainKHR* pSwapchains;
--   >     const uint32_t* pImageIndices;
--   >     VkResult* pResults;
--   > } VkPresentInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentInfoKHR VkPresentInfoKHR registry at www.khronos.org>
data VkPresentInfoKHR = VkPresentInfoKHR## Addr## ByteArray##

instance Eq VkPresentInfoKHR where
        (VkPresentInfoKHR## a _) == x@(VkPresentInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPresentInfoKHR where
        (VkPresentInfoKHR## a _) `compare` x@(VkPresentInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPresentInfoKHR where
        sizeOf ~_ = #{size VkPresentInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPresentInfoKHR where
        unsafeAddr (VkPresentInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPresentInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPresentInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPresentInfoKHR where
        type StructFields VkPresentInfoKHR =
             '["sType", "pNext", "waitSemaphoreCount", "pWaitSemaphores", -- ' closing tick for hsc2hs
               "swapchainCount", "pSwapchains", "pImageIndices", "pResults"]
        type CUnionType VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPresentInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkPresentInfoKHR
         where
        type FieldType "sType" VkPresentInfoKHR = VkStructureType
        type FieldOptional "sType" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, sType}
        type FieldIsArray "sType" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentInfoKHR, sType}

instance {-# OVERLAPPING #-} CanReadField "sType" VkPresentInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentInfoKHR, sType}

instance {-# OVERLAPPING #-} CanWriteField "sType" VkPresentInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentInfoKHR, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkPresentInfoKHR
         where
        type FieldType "pNext" VkPresentInfoKHR = Ptr Void
        type FieldOptional "pNext" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, pNext}
        type FieldIsArray "pNext" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-} CanReadField "pNext" VkPresentInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-} CanWriteField "pNext" VkPresentInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreCount" VkPresentInfoKHR where
        type FieldType "waitSemaphoreCount" VkPresentInfoKHR = Word32
        type FieldOptional "waitSemaphoreCount" VkPresentInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreCount" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, waitSemaphoreCount}
        type FieldIsArray "waitSemaphoreCount" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentInfoKHR, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanReadField "waitSemaphoreCount" VkPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, waitSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentInfoKHR, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "waitSemaphoreCount" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentInfoKHR, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphores" VkPresentInfoKHR where
        type FieldType "pWaitSemaphores" VkPresentInfoKHR = Ptr VkSemaphore
        type FieldOptional "pWaitSemaphores" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphores" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, pWaitSemaphores}
        type FieldIsArray "pWaitSemaphores" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentInfoKHR, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         CanReadField "pWaitSemaphores" VkPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, pWaitSemaphores})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentInfoKHR, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         CanWriteField "pWaitSemaphores" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentInfoKHR, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkPresentInfoKHR where
        type FieldType "swapchainCount" VkPresentInfoKHR = Word32
        type FieldOptional "swapchainCount" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "swapchainCount" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, swapchainCount}
        type FieldIsArray "swapchainCount" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentInfoKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         CanReadField "swapchainCount" VkPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, swapchainCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentInfoKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchainCount" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentInfoKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "pSwapchains" VkPresentInfoKHR where
        type FieldType "pSwapchains" VkPresentInfoKHR = Ptr VkSwapchainKHR
        type FieldOptional "pSwapchains" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSwapchains" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, pSwapchains}
        type FieldIsArray "pSwapchains" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentInfoKHR, pSwapchains}

instance {-# OVERLAPPING #-}
         CanReadField "pSwapchains" VkPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, pSwapchains})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentInfoKHR, pSwapchains}

instance {-# OVERLAPPING #-}
         CanWriteField "pSwapchains" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentInfoKHR, pSwapchains}

instance {-# OVERLAPPING #-}
         HasField "pImageIndices" VkPresentInfoKHR where
        type FieldType "pImageIndices" VkPresentInfoKHR = Ptr Word32
        type FieldOptional "pImageIndices" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pImageIndices" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, pImageIndices}
        type FieldIsArray "pImageIndices" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentInfoKHR, pImageIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pImageIndices" VkPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, pImageIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentInfoKHR, pImageIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pImageIndices" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentInfoKHR, pImageIndices}

instance {-# OVERLAPPING #-} HasField "pResults" VkPresentInfoKHR
         where
        type FieldType "pResults" VkPresentInfoKHR = Ptr VkResult
        type FieldOptional "pResults" VkPresentInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pResults" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, pResults}
        type FieldIsArray "pResults" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentInfoKHR, pResults}

instance {-# OVERLAPPING #-}
         CanReadField "pResults" VkPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, pResults})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentInfoKHR, pResults}

instance {-# OVERLAPPING #-}
         CanWriteField "pResults" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentInfoKHR, pResults}

instance Show VkPresentInfoKHR where
        showsPrec d x
          = showString "VkPresentInfoKHR {" .
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
                                      showString "swapchainCount = " .
                                        showsPrec d (getField @"swapchainCount" x) .
                                          showString ", " .
                                            showString "pSwapchains = " .
                                              showsPrec d (getField @"pSwapchains" x) .
                                                showString ", " .
                                                  showString "pImageIndices = " .
                                                    showsPrec d (getField @"pImageIndices" x) .
                                                      showString ", " .
                                                        showString "pResults = " .
                                                          showsPrec d (getField @"pResults" x) .
                                                            showChar '}'
