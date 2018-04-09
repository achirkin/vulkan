#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindImageMemorySwapchainInfoKHR
       (VkBindImageMemorySwapchainInfoKHR(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Base                                           (Addr##,
                                                                     ByteArray##,
                                                                     byteArrayContents##,
                                                                     plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Handles                      (VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo (VkBindImageMemoryInfo)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkBindImageMemorySwapchainInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR swapchain;
--   >     uint32_t                         imageIndex;
--   > } VkBindImageMemorySwapchainInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkBindImageMemorySwapchainInfoKHRVkBindImageMemorySwapchainInfoKHR registry at www.khronos.org>
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
