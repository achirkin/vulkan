#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindImageMemorySwapchainInfoKHX
       (VkBindImageMemorySwapchainInfoKHX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Handles                         (VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR (VkBindImageMemoryInfoKHR)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkBindImageMemorySwapchainInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR swapchain;
--   >     uint32_t                         imageIndex;
--   > } VkBindImageMemorySwapchainInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBindImageMemorySwapchainInfoKHX.html VkBindImageMemorySwapchainInfoKHX registry at www.khronos.org>
data VkBindImageMemorySwapchainInfoKHX = VkBindImageMemorySwapchainInfoKHX## Addr##
                                                                            ByteArray##

instance Eq VkBindImageMemorySwapchainInfoKHX where
        (VkBindImageMemorySwapchainInfoKHX## a _) ==
          x@(VkBindImageMemorySwapchainInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemorySwapchainInfoKHX where
        (VkBindImageMemorySwapchainInfoKHX## a _) `compare`
          x@(VkBindImageMemorySwapchainInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemorySwapchainInfoKHX where
        sizeOf ~_ = #{size VkBindImageMemorySwapchainInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindImageMemorySwapchainInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImageMemorySwapchainInfoKHX where
        unsafeAddr (VkBindImageMemorySwapchainInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImageMemorySwapchainInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImageMemorySwapchainInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImageMemorySwapchainInfoKHX where
        type StructFields VkBindImageMemorySwapchainInfoKHX =
             '["sType", "pNext", "swapchain", "imageIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkBindImageMemorySwapchainInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImageMemorySwapchainInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImageMemorySwapchainInfoKHX =
             '[VkBindImageMemoryInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImageMemorySwapchainInfoKHX where
        type FieldType "sType" VkBindImageMemorySwapchainInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkBindImageMemorySwapchainInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImageMemorySwapchainInfoKHX =
             #{offset VkBindImageMemorySwapchainInfoKHX, sType}
        type FieldIsArray "sType" VkBindImageMemorySwapchainInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemorySwapchainInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImageMemorySwapchainInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImageMemorySwapchainInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImageMemorySwapchainInfoKHX where
        type FieldType "pNext" VkBindImageMemorySwapchainInfoKHX = Ptr Void
        type FieldOptional "pNext" VkBindImageMemorySwapchainInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImageMemorySwapchainInfoKHX =
             #{offset VkBindImageMemorySwapchainInfoKHX, pNext}
        type FieldIsArray "pNext" VkBindImageMemorySwapchainInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemorySwapchainInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImageMemorySwapchainInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImageMemorySwapchainInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchain" VkBindImageMemorySwapchainInfoKHX where
        type FieldType "swapchain" VkBindImageMemorySwapchainInfoKHX =
             VkSwapchainKHR
        type FieldOptional "swapchain" VkBindImageMemorySwapchainInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "swapchain" VkBindImageMemorySwapchainInfoKHX =
             #{offset VkBindImageMemorySwapchainInfoKHX, swapchain}
        type FieldIsArray "swapchain" VkBindImageMemorySwapchainInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemorySwapchainInfoKHX, swapchain}

instance {-# OVERLAPPING #-}
         CanReadField "swapchain" VkBindImageMemorySwapchainInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHX, swapchain})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, swapchain}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchain" VkBindImageMemorySwapchainInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, swapchain}

instance {-# OVERLAPPING #-}
         HasField "imageIndex" VkBindImageMemorySwapchainInfoKHX where
        type FieldType "imageIndex" VkBindImageMemorySwapchainInfoKHX =
             Word32
        type FieldOptional "imageIndex" VkBindImageMemorySwapchainInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "imageIndex" VkBindImageMemorySwapchainInfoKHX =
             #{offset VkBindImageMemorySwapchainInfoKHX, imageIndex}
        type FieldIsArray "imageIndex" VkBindImageMemorySwapchainInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemorySwapchainInfoKHX, imageIndex}

instance {-# OVERLAPPING #-}
         CanReadField "imageIndex" VkBindImageMemorySwapchainInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHX, imageIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, imageIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "imageIndex" VkBindImageMemorySwapchainInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, imageIndex}

instance Show VkBindImageMemorySwapchainInfoKHX where
        showsPrec d x
          = showString "VkBindImageMemorySwapchainInfoKHX {" .
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
