#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Present
       (VkPresentInfoKHR(..), VkPresentRegionKHR(..),
        VkPresentRegionsKHR(..), VkPresentTimeGOOGLE(..),
        VkPresentTimesInfoGOOGLE(..))
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Result        (VkResult)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkSemaphore,
                                                           VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.Rect        (VkRectLayerKHR)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

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

-- | > typedef struct VkPresentRegionKHR {
--   >     uint32_t         rectangleCount;
--   >     const VkRectLayerKHR*   pRectangles;
--   > } VkPresentRegionKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentRegionKHR VkPresentRegionKHR registry at www.khronos.org>
data VkPresentRegionKHR = VkPresentRegionKHR## Addr## ByteArray##

instance Eq VkPresentRegionKHR where
        (VkPresentRegionKHR## a _) == x@(VkPresentRegionKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPresentRegionKHR where
        (VkPresentRegionKHR## a _) `compare` x@(VkPresentRegionKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPresentRegionKHR where
        sizeOf ~_ = #{size VkPresentRegionKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentRegionKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPresentRegionKHR where
        unsafeAddr (VkPresentRegionKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPresentRegionKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPresentRegionKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPresentRegionKHR where
        type StructFields VkPresentRegionKHR =
             '["rectangleCount", "pRectangles"] -- ' closing tick for hsc2hs
        type CUnionType VkPresentRegionKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPresentRegionKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPresentRegionKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "rectangleCount" VkPresentRegionKHR where
        type FieldType "rectangleCount" VkPresentRegionKHR = Word32
        type FieldOptional "rectangleCount" VkPresentRegionKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "rectangleCount" VkPresentRegionKHR =
             #{offset VkPresentRegionKHR, rectangleCount}
        type FieldIsArray "rectangleCount" VkPresentRegionKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentRegionKHR, rectangleCount}

instance {-# OVERLAPPING #-}
         CanReadField "rectangleCount" VkPresentRegionKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionKHR, rectangleCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentRegionKHR, rectangleCount}

instance {-# OVERLAPPING #-}
         CanWriteField "rectangleCount" VkPresentRegionKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentRegionKHR, rectangleCount}

instance {-# OVERLAPPING #-}
         HasField "pRectangles" VkPresentRegionKHR where
        type FieldType "pRectangles" VkPresentRegionKHR =
             Ptr VkRectLayerKHR
        type FieldOptional "pRectangles" VkPresentRegionKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pRectangles" VkPresentRegionKHR =
             #{offset VkPresentRegionKHR, pRectangles}
        type FieldIsArray "pRectangles" VkPresentRegionKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentRegionKHR, pRectangles}

instance {-# OVERLAPPING #-}
         CanReadField "pRectangles" VkPresentRegionKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionKHR, pRectangles})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentRegionKHR, pRectangles}

instance {-# OVERLAPPING #-}
         CanWriteField "pRectangles" VkPresentRegionKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentRegionKHR, pRectangles}

instance Show VkPresentRegionKHR where
        showsPrec d x
          = showString "VkPresentRegionKHR {" .
              showString "rectangleCount = " .
                showsPrec d (getField @"rectangleCount" x) .
                  showString ", " .
                    showString "pRectangles = " .
                      showsPrec d (getField @"pRectangles" x) . showChar '}'

-- | > typedef struct VkPresentRegionsKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         swapchainCount;
--   >     const VkPresentRegionKHR*   pRegions;
--   > } VkPresentRegionsKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentRegionsKHR VkPresentRegionsKHR registry at www.khronos.org>
data VkPresentRegionsKHR = VkPresentRegionsKHR## Addr## ByteArray##

instance Eq VkPresentRegionsKHR where
        (VkPresentRegionsKHR## a _) == x@(VkPresentRegionsKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPresentRegionsKHR where
        (VkPresentRegionsKHR## a _) `compare` x@(VkPresentRegionsKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPresentRegionsKHR where
        sizeOf ~_ = #{size VkPresentRegionsKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentRegionsKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPresentRegionsKHR where
        unsafeAddr (VkPresentRegionsKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPresentRegionsKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPresentRegionsKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPresentRegionsKHR where
        type StructFields VkPresentRegionsKHR =
             '["sType", "pNext", "swapchainCount", "pRegions"] -- ' closing tick for hsc2hs
        type CUnionType VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPresentRegionsKHR = '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkPresentRegionsKHR
         where
        type FieldType "sType" VkPresentRegionsKHR = VkStructureType
        type FieldOptional "sType" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPresentRegionsKHR =
             #{offset VkPresentRegionsKHR, sType}
        type FieldIsArray "sType" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentRegionsKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPresentRegionsKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentRegionsKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPresentRegionsKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentRegionsKHR, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkPresentRegionsKHR
         where
        type FieldType "pNext" VkPresentRegionsKHR = Ptr Void
        type FieldOptional "pNext" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPresentRegionsKHR =
             #{offset VkPresentRegionsKHR, pNext}
        type FieldIsArray "pNext" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentRegionsKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPresentRegionsKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentRegionsKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPresentRegionsKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentRegionsKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkPresentRegionsKHR where
        type FieldType "swapchainCount" VkPresentRegionsKHR = Word32
        type FieldOptional "swapchainCount" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "swapchainCount" VkPresentRegionsKHR =
             #{offset VkPresentRegionsKHR, swapchainCount}
        type FieldIsArray "swapchainCount" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentRegionsKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         CanReadField "swapchainCount" VkPresentRegionsKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, swapchainCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentRegionsKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchainCount" VkPresentRegionsKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentRegionsKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "pRegions" VkPresentRegionsKHR where
        type FieldType "pRegions" VkPresentRegionsKHR =
             Ptr VkPresentRegionKHR
        type FieldOptional "pRegions" VkPresentRegionsKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pRegions" VkPresentRegionsKHR =
             #{offset VkPresentRegionsKHR, pRegions}
        type FieldIsArray "pRegions" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentRegionsKHR, pRegions}

instance {-# OVERLAPPING #-}
         CanReadField "pRegions" VkPresentRegionsKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, pRegions})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentRegionsKHR, pRegions}

instance {-# OVERLAPPING #-}
         CanWriteField "pRegions" VkPresentRegionsKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentRegionsKHR, pRegions}

instance Show VkPresentRegionsKHR where
        showsPrec d x
          = showString "VkPresentRegionsKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "swapchainCount = " .
                            showsPrec d (getField @"swapchainCount" x) .
                              showString ", " .
                                showString "pRegions = " .
                                  showsPrec d (getField @"pRegions" x) . showChar '}'

-- | > typedef struct VkPresentTimeGOOGLE {
--   >     uint32_t                         presentID;
--   >     uint64_t                         desiredPresentTime;
--   > } VkPresentTimeGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentTimeGOOGLE VkPresentTimeGOOGLE registry at www.khronos.org>
data VkPresentTimeGOOGLE = VkPresentTimeGOOGLE## Addr## ByteArray##

instance Eq VkPresentTimeGOOGLE where
        (VkPresentTimeGOOGLE## a _) == x@(VkPresentTimeGOOGLE## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPresentTimeGOOGLE where
        (VkPresentTimeGOOGLE## a _) `compare` x@(VkPresentTimeGOOGLE## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPresentTimeGOOGLE where
        sizeOf ~_ = #{size VkPresentTimeGOOGLE}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentTimeGOOGLE}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPresentTimeGOOGLE where
        unsafeAddr (VkPresentTimeGOOGLE## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPresentTimeGOOGLE## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPresentTimeGOOGLE## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPresentTimeGOOGLE where
        type StructFields VkPresentTimeGOOGLE =
             '["presentID", "desiredPresentTime"] -- ' closing tick for hsc2hs
        type CUnionType VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPresentTimeGOOGLE = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "presentID" VkPresentTimeGOOGLE where
        type FieldType "presentID" VkPresentTimeGOOGLE = Word32
        type FieldOptional "presentID" VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs
        type FieldOffset "presentID" VkPresentTimeGOOGLE =
             #{offset VkPresentTimeGOOGLE, presentID}
        type FieldIsArray "presentID" VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentTimeGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         CanReadField "presentID" VkPresentTimeGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimeGOOGLE, presentID})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimeGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         CanWriteField "presentID" VkPresentTimeGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimeGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         HasField "desiredPresentTime" VkPresentTimeGOOGLE where
        type FieldType "desiredPresentTime" VkPresentTimeGOOGLE = Word64
        type FieldOptional "desiredPresentTime" VkPresentTimeGOOGLE =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "desiredPresentTime" VkPresentTimeGOOGLE =
             #{offset VkPresentTimeGOOGLE, desiredPresentTime}
        type FieldIsArray "desiredPresentTime" VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentTimeGOOGLE, desiredPresentTime}

instance {-# OVERLAPPING #-}
         CanReadField "desiredPresentTime" VkPresentTimeGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimeGOOGLE, desiredPresentTime})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimeGOOGLE, desiredPresentTime}

instance {-# OVERLAPPING #-}
         CanWriteField "desiredPresentTime" VkPresentTimeGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimeGOOGLE, desiredPresentTime}

instance Show VkPresentTimeGOOGLE where
        showsPrec d x
          = showString "VkPresentTimeGOOGLE {" .
              showString "presentID = " .
                showsPrec d (getField @"presentID" x) .
                  showString ", " .
                    showString "desiredPresentTime = " .
                      showsPrec d (getField @"desiredPresentTime" x) . showChar '}'

-- | > typedef struct VkPresentTimesInfoGOOGLE {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         swapchainCount;
--   >     const VkPresentTimeGOOGLE*   pTimes;
--   > } VkPresentTimesInfoGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentTimesInfoGOOGLE VkPresentTimesInfoGOOGLE registry at www.khronos.org>
data VkPresentTimesInfoGOOGLE = VkPresentTimesInfoGOOGLE## Addr##
                                                          ByteArray##

instance Eq VkPresentTimesInfoGOOGLE where
        (VkPresentTimesInfoGOOGLE## a _) ==
          x@(VkPresentTimesInfoGOOGLE## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPresentTimesInfoGOOGLE where
        (VkPresentTimesInfoGOOGLE## a _) `compare`
          x@(VkPresentTimesInfoGOOGLE## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPresentTimesInfoGOOGLE where
        sizeOf ~_ = #{size VkPresentTimesInfoGOOGLE}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentTimesInfoGOOGLE}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPresentTimesInfoGOOGLE where
        unsafeAddr (VkPresentTimesInfoGOOGLE## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPresentTimesInfoGOOGLE## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPresentTimesInfoGOOGLE## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPresentTimesInfoGOOGLE where
        type StructFields VkPresentTimesInfoGOOGLE =
             '["sType", "pNext", "swapchainCount", "pTimes"] -- ' closing tick for hsc2hs
        type CUnionType VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPresentTimesInfoGOOGLE = '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPresentTimesInfoGOOGLE where
        type FieldType "sType" VkPresentTimesInfoGOOGLE = VkStructureType
        type FieldOptional "sType" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPresentTimesInfoGOOGLE =
             #{offset VkPresentTimesInfoGOOGLE, sType}
        type FieldIsArray "sType" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentTimesInfoGOOGLE, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPresentTimesInfoGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPresentTimesInfoGOOGLE where
        type FieldType "pNext" VkPresentTimesInfoGOOGLE = Ptr Void
        type FieldOptional "pNext" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPresentTimesInfoGOOGLE =
             #{offset VkPresentTimesInfoGOOGLE, pNext}
        type FieldIsArray "pNext" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentTimesInfoGOOGLE, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPresentTimesInfoGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkPresentTimesInfoGOOGLE where
        type FieldType "swapchainCount" VkPresentTimesInfoGOOGLE = Word32
        type FieldOptional "swapchainCount" VkPresentTimesInfoGOOGLE =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "swapchainCount" VkPresentTimesInfoGOOGLE =
             #{offset VkPresentTimesInfoGOOGLE, swapchainCount}
        type FieldIsArray "swapchainCount" VkPresentTimesInfoGOOGLE =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

instance {-# OVERLAPPING #-}
         CanReadField "swapchainCount" VkPresentTimesInfoGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, swapchainCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchainCount" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "pTimes" VkPresentTimesInfoGOOGLE where
        type FieldType "pTimes" VkPresentTimesInfoGOOGLE =
             Ptr VkPresentTimeGOOGLE
        type FieldOptional "pTimes" VkPresentTimesInfoGOOGLE = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pTimes" VkPresentTimesInfoGOOGLE =
             #{offset VkPresentTimesInfoGOOGLE, pTimes}
        type FieldIsArray "pTimes" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentTimesInfoGOOGLE, pTimes}

instance {-# OVERLAPPING #-}
         CanReadField "pTimes" VkPresentTimesInfoGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, pTimes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, pTimes}

instance {-# OVERLAPPING #-}
         CanWriteField "pTimes" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, pTimes}

instance Show VkPresentTimesInfoGOOGLE where
        showsPrec d x
          = showString "VkPresentTimesInfoGOOGLE {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "swapchainCount = " .
                            showsPrec d (getField @"swapchainCount" x) .
                              showString ", " .
                                showString "pTimes = " .
                                  showsPrec d (getField @"pTimes" x) . showChar '}'
