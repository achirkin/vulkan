#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkBindImageMemorySwapchainInfoKHX where
        type VkSTypeMType VkBindImageMemorySwapchainInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindImageMemorySwapchainInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, sType}

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

instance CanReadField "sType" VkBindImageMemorySwapchainInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBindImageMemorySwapchainInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkBindImageMemorySwapchainInfoKHX where
        type VkPNextMType VkBindImageMemorySwapchainInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindImageMemorySwapchainInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, pNext}

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

instance CanReadField "pNext" VkBindImageMemorySwapchainInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBindImageMemorySwapchainInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSwapchain VkBindImageMemorySwapchainInfoKHX where
        type VkSwapchainMType VkBindImageMemorySwapchainInfoKHX =
             VkSwapchainKHR

        {-# NOINLINE vkSwapchain #-}
        vkSwapchain x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHX, swapchain})

        {-# INLINE vkSwapchainByteOffset #-}
        vkSwapchainByteOffset ~_
          = #{offset VkBindImageMemorySwapchainInfoKHX, swapchain}

        {-# INLINE readVkSwapchain #-}
        readVkSwapchain p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, swapchain}

        {-# INLINE writeVkSwapchain #-}
        writeVkSwapchain p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, swapchain}

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

instance CanReadField "swapchain" VkBindImageMemorySwapchainInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSwapchain

        {-# INLINE readField #-}
        readField = readVkSwapchain

instance CanWriteField "swapchain"
           VkBindImageMemorySwapchainInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSwapchain

instance {-# OVERLAPPING #-}
         HasVkImageIndex VkBindImageMemorySwapchainInfoKHX where
        type VkImageIndexMType VkBindImageMemorySwapchainInfoKHX = Word32

        {-# NOINLINE vkImageIndex #-}
        vkImageIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemorySwapchainInfoKHX, imageIndex})

        {-# INLINE vkImageIndexByteOffset #-}
        vkImageIndexByteOffset ~_
          = #{offset VkBindImageMemorySwapchainInfoKHX, imageIndex}

        {-# INLINE readVkImageIndex #-}
        readVkImageIndex p
          = peekByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, imageIndex}

        {-# INLINE writeVkImageIndex #-}
        writeVkImageIndex p
          = pokeByteOff p #{offset VkBindImageMemorySwapchainInfoKHX, imageIndex}

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

instance CanReadField "imageIndex"
           VkBindImageMemorySwapchainInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkImageIndex

        {-# INLINE readField #-}
        readField = readVkImageIndex

instance CanWriteField "imageIndex"
           VkBindImageMemorySwapchainInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkImageIndex

instance Show VkBindImageMemorySwapchainInfoKHX where
        showsPrec d x
          = showString "VkBindImageMemorySwapchainInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSwapchain = " .
                            showsPrec d (vkSwapchain x) .
                              showString ", " .
                                showString "vkImageIndex = " .
                                  showsPrec d (vkImageIndex x) . showChar '}'
