#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSwapchainCreateInfoKHR
       (VkImageSwapchainCreateInfoKHR(..)) where
import           Foreign.Storable                               (Storable (..))
import           GHC.Base                                       (Addr##,
                                                                 ByteArray##,
                                                                 byteArrayContents##,
                                                                 plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType     (VkStructureType)
import           Graphics.Vulkan.Types.Handles                  (VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo (VkImageCreateInfo)
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

-- | > typedef struct VkImageSwapchainCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR   swapchain;
--   > } VkImageSwapchainCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageSwapchainCreateInfoKHR VkImageSwapchainCreateInfoKHR registry at www.khronos.org>
data VkImageSwapchainCreateInfoKHR = VkImageSwapchainCreateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkImageSwapchainCreateInfoKHR where
        (VkImageSwapchainCreateInfoKHR## a _) ==
          x@(VkImageSwapchainCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSwapchainCreateInfoKHR where
        (VkImageSwapchainCreateInfoKHR## a _) `compare`
          x@(VkImageSwapchainCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSwapchainCreateInfoKHR where
        sizeOf ~_ = #{size VkImageSwapchainCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageSwapchainCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSwapchainCreateInfoKHR where
        unsafeAddr (VkImageSwapchainCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSwapchainCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSwapchainCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSwapchainCreateInfoKHR where
        type StructFields VkImageSwapchainCreateInfoKHR =
             '["sType", "pNext", "swapchain"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSwapchainCreateInfoKHR =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageSwapchainCreateInfoKHR where
        type FieldType "sType" VkImageSwapchainCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageSwapchainCreateInfoKHR =
             #{offset VkImageSwapchainCreateInfoKHR, sType}
        type FieldIsArray "sType" VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageSwapchainCreateInfoKHR where
        type FieldType "pNext" VkImageSwapchainCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageSwapchainCreateInfoKHR =
             #{offset VkImageSwapchainCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkImageSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchain" VkImageSwapchainCreateInfoKHR where
        type FieldType "swapchain" VkImageSwapchainCreateInfoKHR =
             VkSwapchainKHR
        type FieldOptional "swapchain" VkImageSwapchainCreateInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "swapchain" VkImageSwapchainCreateInfoKHR =
             #{offset VkImageSwapchainCreateInfoKHR, swapchain}
        type FieldIsArray "swapchain" VkImageSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSwapchainCreateInfoKHR, swapchain}

instance {-# OVERLAPPING #-}
         CanReadField "swapchain" VkImageSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHR, swapchain})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHR, swapchain}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchain" VkImageSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHR, swapchain}

instance Show VkImageSwapchainCreateInfoKHR where
        showsPrec d x
          = showString "VkImageSwapchainCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "swapchain = " .
                            showsPrec d (getField @"swapchain" x) . showChar '}'
