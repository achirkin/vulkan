#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSwapchainCreateInfoKHX
       (VkImageSwapchainCreateInfoKHX(..)) where
import           Foreign.Storable                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType     (VkStructureType)
import           Graphics.Vulkan.Types.Handles                  (VkSwapchainKHR)
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo (VkImageCreateInfo)
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

-- | > typedef struct VkImageSwapchainCreateInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR   swapchain;
--   > } VkImageSwapchainCreateInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageSwapchainCreateInfoKHX.html VkImageSwapchainCreateInfoKHX registry at www.khronos.org>
data VkImageSwapchainCreateInfoKHX = VkImageSwapchainCreateInfoKHX## Addr##
                                                                    ByteArray##

instance Eq VkImageSwapchainCreateInfoKHX where
        (VkImageSwapchainCreateInfoKHX## a _) ==
          x@(VkImageSwapchainCreateInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSwapchainCreateInfoKHX where
        (VkImageSwapchainCreateInfoKHX## a _) `compare`
          x@(VkImageSwapchainCreateInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSwapchainCreateInfoKHX where
        sizeOf ~_ = #{size VkImageSwapchainCreateInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageSwapchainCreateInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSwapchainCreateInfoKHX where
        unsafeAddr (VkImageSwapchainCreateInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSwapchainCreateInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSwapchainCreateInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSwapchainCreateInfoKHX where
        type StructFields VkImageSwapchainCreateInfoKHX =
             '["sType", "pNext", "swapchain"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSwapchainCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSwapchainCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSwapchainCreateInfoKHX =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageSwapchainCreateInfoKHX where
        type FieldType "sType" VkImageSwapchainCreateInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkImageSwapchainCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageSwapchainCreateInfoKHX =
             #{offset VkImageSwapchainCreateInfoKHX, sType}
        type FieldIsArray "sType" VkImageSwapchainCreateInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSwapchainCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageSwapchainCreateInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageSwapchainCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageSwapchainCreateInfoKHX where
        type FieldType "pNext" VkImageSwapchainCreateInfoKHX = Ptr Void
        type FieldOptional "pNext" VkImageSwapchainCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageSwapchainCreateInfoKHX =
             #{offset VkImageSwapchainCreateInfoKHX, pNext}
        type FieldIsArray "pNext" VkImageSwapchainCreateInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSwapchainCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageSwapchainCreateInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageSwapchainCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchain" VkImageSwapchainCreateInfoKHX where
        type FieldType "swapchain" VkImageSwapchainCreateInfoKHX =
             VkSwapchainKHR
        type FieldOptional "swapchain" VkImageSwapchainCreateInfoKHX =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "swapchain" VkImageSwapchainCreateInfoKHX =
             #{offset VkImageSwapchainCreateInfoKHX, swapchain}
        type FieldIsArray "swapchain" VkImageSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSwapchainCreateInfoKHX, swapchain}

instance {-# OVERLAPPING #-}
         CanReadField "swapchain" VkImageSwapchainCreateInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHX, swapchain})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHX, swapchain}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchain" VkImageSwapchainCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHX, swapchain}

instance Show VkImageSwapchainCreateInfoKHX where
        showsPrec d x
          = showString "VkImageSwapchainCreateInfoKHX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "swapchain = " .
                            showsPrec d (getField @"swapchain" x) . showChar '}'
