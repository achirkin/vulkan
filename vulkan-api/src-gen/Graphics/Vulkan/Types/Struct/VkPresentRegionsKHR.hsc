#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPresentRegionsKHR
       (VkPresentRegionsKHR(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR   (VkPresentInfoKHR)
import           Graphics.Vulkan.Types.Struct.VkPresentRegionKHR (VkPresentRegionKHR)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkPresentRegionsKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         swapchainCount;
--   >     const VkPresentRegionKHR*   pRegions;
--   > } VkPresentRegionsKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPresentRegionsKHR.html VkPresentRegionsKHR registry at www.khronos.org>
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
