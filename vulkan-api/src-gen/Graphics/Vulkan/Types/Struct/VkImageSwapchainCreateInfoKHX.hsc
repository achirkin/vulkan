#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkImageSwapchainCreateInfoKHX where
        type VkSTypeMType VkImageSwapchainCreateInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageSwapchainCreateInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHX, sType}

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

instance CanReadField "sType" VkImageSwapchainCreateInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImageSwapchainCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkImageSwapchainCreateInfoKHX where
        type VkPNextMType VkImageSwapchainCreateInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageSwapchainCreateInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHX, pNext}

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

instance CanReadField "pNext" VkImageSwapchainCreateInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImageSwapchainCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSwapchain VkImageSwapchainCreateInfoKHX where
        type VkSwapchainMType VkImageSwapchainCreateInfoKHX =
             VkSwapchainKHR

        {-# NOINLINE vkSwapchain #-}
        vkSwapchain x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSwapchainCreateInfoKHX, swapchain})

        {-# INLINE vkSwapchainByteOffset #-}
        vkSwapchainByteOffset ~_
          = #{offset VkImageSwapchainCreateInfoKHX, swapchain}

        {-# INLINE readVkSwapchain #-}
        readVkSwapchain p
          = peekByteOff p #{offset VkImageSwapchainCreateInfoKHX, swapchain}

        {-# INLINE writeVkSwapchain #-}
        writeVkSwapchain p
          = pokeByteOff p #{offset VkImageSwapchainCreateInfoKHX, swapchain}

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

instance CanReadField "swapchain" VkImageSwapchainCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSwapchain

        {-# INLINE readField #-}
        readField = readVkSwapchain

instance CanWriteField "swapchain" VkImageSwapchainCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSwapchain

instance Show VkImageSwapchainCreateInfoKHX where
        showsPrec d x
          = showString "VkImageSwapchainCreateInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSwapchain = " .
                            showsPrec d (vkSwapchain x) . showChar '}'
