#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHX_device_group
       (-- * Vulkan extension: @VK_KHX_device_group@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @KHX@
        --
        -- type: @device@
        --
        -- Extension number: @61@
        --
        -- Required extensions: 'VK_KHX_device_group_creation'.
        --

        -- ** Required extensions: 'VK_KHX_device_group_creation'.
        VkMemoryAllocateFlagsInfoKHX(..),
        VkDeviceGroupRenderPassBeginInfoKHX(..),
        VkDeviceGroupCommandBufferBeginInfoKHX(..),
        VkDeviceGroupSubmitInfoKHX(..), VkDeviceGroupBindSparseInfoKHX(..),
        vkGetDeviceGroupPeerMemoryFeaturesKHX, vkCmdSetDeviceMaskKHX,
        vkCmdDispatchBaseKHX, VK_KHX_DEVICE_GROUP_SPEC_VERSION,
        pattern VK_KHX_DEVICE_GROUP_SPEC_VERSION,
        VK_KHX_DEVICE_GROUP_EXTENSION_NAME,
        pattern VK_KHX_DEVICE_GROUP_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX,
        pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX,
        pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHX,
        pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX,
        -- ** Required extensions: 'VK_KHR_bind_memory2', 'VK_KHX_device_group_creation'.
        VkBindBufferMemoryDeviceGroupInfoKHX(..),
        VkBindImageMemoryDeviceGroupInfoKHX(..),
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHX,
        pattern VK_IMAGE_CREATE_BIND_SFR_BIT_KHX,
        -- ** Required extensions: 'VK_KHR_surface', 'VK_KHX_device_group_creation'.
        VkDeviceGroupPresentCapabilitiesKHX(..),
        vkGetDeviceGroupPresentCapabilitiesKHX,
        vkGetDeviceGroupSurfacePresentModesKHX,
        vkGetPhysicalDevicePresentRectanglesKHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX,
        -- ** Required extensions: 'VK_KHR_swapchain', 'VK_KHX_device_group_creation'.
        VkImageSwapchainCreateInfoKHX(..),
        VkBindImageMemorySwapchainInfoKHX(..),
        VkAcquireNextImageInfoKHX(..), VkDeviceGroupPresentInfoKHX(..),
        VkDeviceGroupSwapchainCreateInfoKHX(..), vkAcquireNextImage2KHX,
        pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX,
        pattern VK_SWAPCHAIN_CREATE_BIND_SFR_BIT_KHX)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.TypeLits                     (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Base             (VkBindSparseInfo,
                                                   VkCommandBufferBeginInfo,
                                                   VkImageCreateInfo,
                                                   VkMemoryAllocateInfo,
                                                   VkPresentInfoKHR, VkRect2D,
                                                   VkRect2D (..),
                                                   VkRenderPassBeginInfo,
                                                   VkSubmitInfo,
                                                   VkSwapchainCreateInfoKHR)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

import Graphics.Vulkan.Ext.VK_KHR_bind_memory2

-- | > typedef struct VkMemoryAllocateFlagsInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMemoryAllocateFlagsKHX flags;
--   >     uint32_t                         deviceMask;
--   > } VkMemoryAllocateFlagsInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryAllocateFlagsInfoKHX.html VkMemoryAllocateFlagsInfoKHX registry at www.khronos.org>
data VkMemoryAllocateFlagsInfoKHX = VkMemoryAllocateFlagsInfoKHX## Addr##
                                                                  ByteArray##

instance Eq VkMemoryAllocateFlagsInfoKHX where
        (VkMemoryAllocateFlagsInfoKHX## a _) ==
          x@(VkMemoryAllocateFlagsInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryAllocateFlagsInfoKHX where
        (VkMemoryAllocateFlagsInfoKHX## a _) `compare`
          x@(VkMemoryAllocateFlagsInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryAllocateFlagsInfoKHX where
        sizeOf ~_ = #{size VkMemoryAllocateFlagsInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryAllocateFlagsInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryAllocateFlagsInfoKHX where
        unsafeAddr (VkMemoryAllocateFlagsInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryAllocateFlagsInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryAllocateFlagsInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryAllocateFlagsInfoKHX where
        type StructFields VkMemoryAllocateFlagsInfoKHX =
             '["sType", "pNext", "flags", "deviceMask"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryAllocateFlagsInfoKHX =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkMemoryAllocateFlagsInfoKHX where
        type VkSTypeMType VkMemoryAllocateFlagsInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryAllocateFlagsInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "sType" VkMemoryAllocateFlagsInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, sType}
        type FieldIsArray "sType" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, sType}

instance CanReadField "sType" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryAllocateFlagsInfoKHX where
        type VkPNextMType VkMemoryAllocateFlagsInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "pNext" VkMemoryAllocateFlagsInfoKHX = Ptr Void
        type FieldOptional "pNext" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, pNext}
        type FieldIsArray "pNext" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

instance CanReadField "pNext" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkMemoryAllocateFlagsInfoKHX where
        type VkFlagsMType VkMemoryAllocateFlagsInfoKHX =
             VkMemoryAllocateFlagsKHX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkMemoryAllocateFlagsInfoKHX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "flags" VkMemoryAllocateFlagsInfoKHX =
             VkMemoryAllocateFlagsKHX
        type FieldOptional "flags" VkMemoryAllocateFlagsInfoKHX = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, flags}
        type FieldIsArray "flags" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, flags}

instance CanReadField "flags" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkDeviceMask VkMemoryAllocateFlagsInfoKHX where
        type VkDeviceMaskMType VkMemoryAllocateFlagsInfoKHX = Word32

        {-# NOINLINE vkDeviceMask #-}
        vkDeviceMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask})

        {-# INLINE vkDeviceMaskByteOffset #-}
        vkDeviceMaskByteOffset ~_
          = #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

        {-# INLINE readVkDeviceMask #-}
        readVkDeviceMask p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

        {-# INLINE writeVkDeviceMask #-}
        writeVkDeviceMask p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "deviceMask" VkMemoryAllocateFlagsInfoKHX = Word32
        type FieldOptional "deviceMask" VkMemoryAllocateFlagsInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}
        type FieldIsArray "deviceMask" VkMemoryAllocateFlagsInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

instance CanReadField "deviceMask" VkMemoryAllocateFlagsInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceMask

        {-# INLINE readField #-}
        readField = readVkDeviceMask

instance CanWriteField "deviceMask" VkMemoryAllocateFlagsInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceMask

instance Show VkMemoryAllocateFlagsInfoKHX where
        showsPrec d x
          = showString "VkMemoryAllocateFlagsInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDeviceMask = " .
                                  showsPrec d (vkDeviceMask x) . showChar '}'

-- | > typedef struct VkDeviceGroupRenderPassBeginInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   >     uint32_t         deviceRenderAreaCount;
--   >     const VkRect2D*  pDeviceRenderAreas;
--   > } VkDeviceGroupRenderPassBeginInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGroupRenderPassBeginInfoKHX.html VkDeviceGroupRenderPassBeginInfoKHX registry at www.khronos.org>
data VkDeviceGroupRenderPassBeginInfoKHX = VkDeviceGroupRenderPassBeginInfoKHX## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupRenderPassBeginInfoKHX where
        (VkDeviceGroupRenderPassBeginInfoKHX## a _) ==
          x@(VkDeviceGroupRenderPassBeginInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupRenderPassBeginInfoKHX where
        (VkDeviceGroupRenderPassBeginInfoKHX## a _) `compare`
          x@(VkDeviceGroupRenderPassBeginInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupRenderPassBeginInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupRenderPassBeginInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupRenderPassBeginInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupRenderPassBeginInfoKHX
         where
        unsafeAddr (VkDeviceGroupRenderPassBeginInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupRenderPassBeginInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupRenderPassBeginInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupRenderPassBeginInfoKHX where
        type StructFields VkDeviceGroupRenderPassBeginInfoKHX =
             '["sType", "pNext", "deviceMask", "deviceRenderAreaCount", -- ' closing tick for hsc2hs
               "pDeviceRenderAreas"]
        type CUnionType VkDeviceGroupRenderPassBeginInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupRenderPassBeginInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupRenderPassBeginInfoKHX =
             '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupRenderPassBeginInfoKHX where
        type VkSTypeMType VkDeviceGroupRenderPassBeginInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupRenderPassBeginInfoKHX where
        type FieldType "sType" VkDeviceGroupRenderPassBeginInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupRenderPassBeginInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupRenderPassBeginInfoKHX =
             #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupRenderPassBeginInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}

instance CanReadField "sType" VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupRenderPassBeginInfoKHX where
        type VkPNextMType VkDeviceGroupRenderPassBeginInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupRenderPassBeginInfoKHX where
        type FieldType "pNext" VkDeviceGroupRenderPassBeginInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupRenderPassBeginInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupRenderPassBeginInfoKHX =
             #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupRenderPassBeginInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDeviceMask VkDeviceGroupRenderPassBeginInfoKHX where
        type VkDeviceMaskMType VkDeviceGroupRenderPassBeginInfoKHX = Word32

        {-# NOINLINE vkDeviceMask #-}
        vkDeviceMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask})

        {-# INLINE vkDeviceMaskByteOffset #-}
        vkDeviceMaskByteOffset ~_
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}

        {-# INLINE readVkDeviceMask #-}
        readVkDeviceMask p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}

        {-# INLINE writeVkDeviceMask #-}
        writeVkDeviceMask p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX where
        type FieldType "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX =
             Word32
        type FieldOptional "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX =
             #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}
        type FieldIsArray "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}

instance CanReadField "deviceMask"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceMask

        {-# INLINE readField #-}
        readField = readVkDeviceMask

instance CanWriteField "deviceMask"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceMask

instance {-# OVERLAPPING #-}
         HasVkDeviceRenderAreaCount VkDeviceGroupRenderPassBeginInfoKHX
         where
        type VkDeviceRenderAreaCountMType
               VkDeviceGroupRenderPassBeginInfoKHX
             = Word32

        {-# NOINLINE vkDeviceRenderAreaCount #-}
        vkDeviceRenderAreaCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount})

        {-# INLINE vkDeviceRenderAreaCountByteOffset #-}
        vkDeviceRenderAreaCountByteOffset ~_
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}

        {-# INLINE readVkDeviceRenderAreaCount #-}
        readVkDeviceRenderAreaCount p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}

        {-# INLINE writeVkDeviceRenderAreaCount #-}
        writeVkDeviceRenderAreaCount p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}

instance {-# OVERLAPPING #-}
         HasField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        type FieldType "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfoKHX
             = Word32
        type FieldOptional "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfoKHX
             =
             #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}
        type FieldIsArray "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}

instance CanReadField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceRenderAreaCount

        {-# INLINE readField #-}
        readField = readVkDeviceRenderAreaCount

instance CanWriteField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceRenderAreaCount

instance {-# OVERLAPPING #-}
         HasVkPDeviceRenderAreas VkDeviceGroupRenderPassBeginInfoKHX where
        type VkPDeviceRenderAreasMType VkDeviceGroupRenderPassBeginInfoKHX
             = Ptr VkRect2D

        {-# NOINLINE vkPDeviceRenderAreas #-}
        vkPDeviceRenderAreas x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas})

        {-# INLINE vkPDeviceRenderAreasByteOffset #-}
        vkPDeviceRenderAreasByteOffset ~_
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}

        {-# INLINE readVkPDeviceRenderAreas #-}
        readVkPDeviceRenderAreas p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}

        {-# INLINE writeVkPDeviceRenderAreas #-}
        writeVkPDeviceRenderAreas p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}

instance {-# OVERLAPPING #-}
         HasField "pDeviceRenderAreas" VkDeviceGroupRenderPassBeginInfoKHX
         where
        type FieldType "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfoKHX
             = Ptr VkRect2D
        type FieldOptional "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfoKHX
             =
             #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}
        type FieldIsArray "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}

instance CanReadField "pDeviceRenderAreas"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPDeviceRenderAreas

        {-# INLINE readField #-}
        readField = readVkPDeviceRenderAreas

instance CanWriteField "pDeviceRenderAreas"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDeviceRenderAreas

instance Show VkDeviceGroupRenderPassBeginInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupRenderPassBeginInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceMask = " .
                            showsPrec d (vkDeviceMask x) .
                              showString ", " .
                                showString "vkDeviceRenderAreaCount = " .
                                  showsPrec d (vkDeviceRenderAreaCount x) .
                                    showString ", " .
                                      showString "vkPDeviceRenderAreas = " .
                                        showsPrec d (vkPDeviceRenderAreas x) . showChar '}'

-- | > typedef struct VkDeviceGroupCommandBufferBeginInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   > } VkDeviceGroupCommandBufferBeginInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGroupCommandBufferBeginInfoKHX.html VkDeviceGroupCommandBufferBeginInfoKHX registry at www.khronos.org>
data VkDeviceGroupCommandBufferBeginInfoKHX = VkDeviceGroupCommandBufferBeginInfoKHX## Addr##
                                                                                      ByteArray##

instance Eq VkDeviceGroupCommandBufferBeginInfoKHX where
        (VkDeviceGroupCommandBufferBeginInfoKHX## a _) ==
          x@(VkDeviceGroupCommandBufferBeginInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupCommandBufferBeginInfoKHX where
        (VkDeviceGroupCommandBufferBeginInfoKHX## a _) `compare`
          x@(VkDeviceGroupCommandBufferBeginInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupCommandBufferBeginInfoKHX where
        sizeOf ~_
          = #{size VkDeviceGroupCommandBufferBeginInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupCommandBufferBeginInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupCommandBufferBeginInfoKHX
         where
        unsafeAddr (VkDeviceGroupCommandBufferBeginInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupCommandBufferBeginInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupCommandBufferBeginInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupCommandBufferBeginInfoKHX where
        type StructFields VkDeviceGroupCommandBufferBeginInfoKHX =
             '["sType", "pNext", "deviceMask"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupCommandBufferBeginInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupCommandBufferBeginInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupCommandBufferBeginInfoKHX =
             '[VkCommandBufferBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupCommandBufferBeginInfoKHX where
        type VkSTypeMType VkDeviceGroupCommandBufferBeginInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupCommandBufferBeginInfoKHX where
        type FieldType "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

instance CanReadField "sType"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupCommandBufferBeginInfoKHX where
        type VkPNextMType VkDeviceGroupCommandBufferBeginInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupCommandBufferBeginInfoKHX where
        type FieldType "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

instance CanReadField "pNext"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDeviceMask VkDeviceGroupCommandBufferBeginInfoKHX where
        type VkDeviceMaskMType VkDeviceGroupCommandBufferBeginInfoKHX =
             Word32

        {-# NOINLINE vkDeviceMask #-}
        vkDeviceMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask})

        {-# INLINE vkDeviceMaskByteOffset #-}
        vkDeviceMaskByteOffset ~_
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

        {-# INLINE readVkDeviceMask #-}
        readVkDeviceMask p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

        {-# INLINE writeVkDeviceMask #-}
        writeVkDeviceMask p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkDeviceGroupCommandBufferBeginInfoKHX where
        type FieldType "deviceMask" VkDeviceGroupCommandBufferBeginInfoKHX
             = Word32
        type FieldOptional "deviceMask"
               VkDeviceGroupCommandBufferBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask"
               VkDeviceGroupCommandBufferBeginInfoKHX
             =
             #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}
        type FieldIsArray "deviceMask"
               VkDeviceGroupCommandBufferBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

instance CanReadField "deviceMask"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceMask

        {-# INLINE readField #-}
        readField = readVkDeviceMask

instance CanWriteField "deviceMask"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceMask

instance Show VkDeviceGroupCommandBufferBeginInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupCommandBufferBeginInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceMask = " .
                            showsPrec d (vkDeviceMask x) . showChar '}'

-- | > typedef struct VkDeviceGroupSubmitInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         waitSemaphoreCount;
--   >     const uint32_t*    pWaitSemaphoreDeviceIndices;
--   >     uint32_t         commandBufferCount;
--   >     const uint32_t*    pCommandBufferDeviceMasks;
--   >     uint32_t         signalSemaphoreCount;
--   >     const uint32_t*  pSignalSemaphoreDeviceIndices;
--   > } VkDeviceGroupSubmitInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGroupSubmitInfoKHX.html VkDeviceGroupSubmitInfoKHX registry at www.khronos.org>
data VkDeviceGroupSubmitInfoKHX = VkDeviceGroupSubmitInfoKHX## Addr##
                                                              ByteArray##

instance Eq VkDeviceGroupSubmitInfoKHX where
        (VkDeviceGroupSubmitInfoKHX## a _) ==
          x@(VkDeviceGroupSubmitInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupSubmitInfoKHX where
        (VkDeviceGroupSubmitInfoKHX## a _) `compare`
          x@(VkDeviceGroupSubmitInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupSubmitInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupSubmitInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupSubmitInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupSubmitInfoKHX where
        unsafeAddr (VkDeviceGroupSubmitInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupSubmitInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupSubmitInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupSubmitInfoKHX where
        type StructFields VkDeviceGroupSubmitInfoKHX =
             '["sType", "pNext", "waitSemaphoreCount", -- ' closing tick for hsc2hs
               "pWaitSemaphoreDeviceIndices", "commandBufferCount",
               "pCommandBufferDeviceMasks", "signalSemaphoreCount",
               "pSignalSemaphoreDeviceIndices"]
        type CUnionType VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupSubmitInfoKHX = '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkDeviceGroupSubmitInfoKHX
         where
        type VkSTypeMType VkDeviceGroupSubmitInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupSubmitInfoKHX where
        type FieldType "sType" VkDeviceGroupSubmitInfoKHX = VkStructureType
        type FieldOptional "sType" VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupSubmitInfoKHX =
             #{offset VkDeviceGroupSubmitInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, sType}

instance CanReadField "sType" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDeviceGroupSubmitInfoKHX
         where
        type VkPNextMType VkDeviceGroupSubmitInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupSubmitInfoKHX where
        type FieldType "pNext" VkDeviceGroupSubmitInfoKHX = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupSubmitInfoKHX =
             #{offset VkDeviceGroupSubmitInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupSubmitInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupSubmitInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkWaitSemaphoreCount VkDeviceGroupSubmitInfoKHX where
        type VkWaitSemaphoreCountMType VkDeviceGroupSubmitInfoKHX = Word32

        {-# NOINLINE vkWaitSemaphoreCount #-}
        vkWaitSemaphoreCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount})

        {-# INLINE vkWaitSemaphoreCountByteOffset #-}
        vkWaitSemaphoreCountByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}

        {-# INLINE readVkWaitSemaphoreCount #-}
        readVkWaitSemaphoreCount p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}

        {-# INLINE writeVkWaitSemaphoreCount #-}
        writeVkWaitSemaphoreCount p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX where
        type FieldType "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX =
             Word32
        type FieldOptional "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX =
             #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}
        type FieldIsArray "waitSemaphoreCount" VkDeviceGroupSubmitInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, waitSemaphoreCount}

instance CanReadField "waitSemaphoreCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkWaitSemaphoreCount

        {-# INLINE readField #-}
        readField = readVkWaitSemaphoreCount

instance CanWriteField "waitSemaphoreCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkWaitSemaphoreCount

instance {-# OVERLAPPING #-}
         HasVkPWaitSemaphoreDeviceIndices VkDeviceGroupSubmitInfoKHX where
        type VkPWaitSemaphoreDeviceIndicesMType VkDeviceGroupSubmitInfoKHX
             = Ptr Word32

        {-# NOINLINE vkPWaitSemaphoreDeviceIndices #-}
        vkPWaitSemaphoreDeviceIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices})

        {-# INLINE vkPWaitSemaphoreDeviceIndicesByteOffset #-}
        vkPWaitSemaphoreDeviceIndicesByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}

        {-# INLINE readVkPWaitSemaphoreDeviceIndices #-}
        readVkPWaitSemaphoreDeviceIndices p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}

        {-# INLINE writeVkPWaitSemaphoreDeviceIndices #-}
        writeVkPWaitSemaphoreDeviceIndices p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphoreDeviceIndices" VkDeviceGroupSubmitInfoKHX
         where
        type FieldType "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = Ptr Word32
        type FieldOptional "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             =
             #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}
        type FieldIsArray "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, pWaitSemaphoreDeviceIndices}

instance CanReadField "pWaitSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPWaitSemaphoreDeviceIndices

        {-# INLINE readField #-}
        readField = readVkPWaitSemaphoreDeviceIndices

instance CanWriteField "pWaitSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPWaitSemaphoreDeviceIndices

instance {-# OVERLAPPING #-}
         HasVkCommandBufferCount VkDeviceGroupSubmitInfoKHX where
        type VkCommandBufferCountMType VkDeviceGroupSubmitInfoKHX = Word32

        {-# NOINLINE vkCommandBufferCount #-}
        vkCommandBufferCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount})

        {-# INLINE vkCommandBufferCountByteOffset #-}
        vkCommandBufferCountByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}

        {-# INLINE readVkCommandBufferCount #-}
        readVkCommandBufferCount p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}

        {-# INLINE writeVkCommandBufferCount #-}
        writeVkCommandBufferCount p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}

instance {-# OVERLAPPING #-}
         HasField "commandBufferCount" VkDeviceGroupSubmitInfoKHX where
        type FieldType "commandBufferCount" VkDeviceGroupSubmitInfoKHX =
             Word32
        type FieldOptional "commandBufferCount" VkDeviceGroupSubmitInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "commandBufferCount" VkDeviceGroupSubmitInfoKHX =
             #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}
        type FieldIsArray "commandBufferCount" VkDeviceGroupSubmitInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, commandBufferCount}

instance CanReadField "commandBufferCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkCommandBufferCount

        {-# INLINE readField #-}
        readField = readVkCommandBufferCount

instance CanWriteField "commandBufferCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkCommandBufferCount

instance {-# OVERLAPPING #-}
         HasVkPCommandBufferDeviceMasks VkDeviceGroupSubmitInfoKHX where
        type VkPCommandBufferDeviceMasksMType VkDeviceGroupSubmitInfoKHX =
             Ptr Word32

        {-# NOINLINE vkPCommandBufferDeviceMasks #-}
        vkPCommandBufferDeviceMasks x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks})

        {-# INLINE vkPCommandBufferDeviceMasksByteOffset #-}
        vkPCommandBufferDeviceMasksByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}

        {-# INLINE readVkPCommandBufferDeviceMasks #-}
        readVkPCommandBufferDeviceMasks p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}

        {-# INLINE writeVkPCommandBufferDeviceMasks #-}
        writeVkPCommandBufferDeviceMasks p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}

instance {-# OVERLAPPING #-}
         HasField "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfoKHX
         where
        type FieldType "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfoKHX
             = Ptr Word32
        type FieldOptional "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfoKHX
             =
             #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}
        type FieldIsArray "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, pCommandBufferDeviceMasks}

instance CanReadField "pCommandBufferDeviceMasks"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPCommandBufferDeviceMasks

        {-# INLINE readField #-}
        readField = readVkPCommandBufferDeviceMasks

instance CanWriteField "pCommandBufferDeviceMasks"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPCommandBufferDeviceMasks

instance {-# OVERLAPPING #-}
         HasVkSignalSemaphoreCount VkDeviceGroupSubmitInfoKHX where
        type VkSignalSemaphoreCountMType VkDeviceGroupSubmitInfoKHX =
             Word32

        {-# NOINLINE vkSignalSemaphoreCount #-}
        vkSignalSemaphoreCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount})

        {-# INLINE vkSignalSemaphoreCountByteOffset #-}
        vkSignalSemaphoreCountByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}

        {-# INLINE readVkSignalSemaphoreCount #-}
        readVkSignalSemaphoreCount p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}

        {-# INLINE writeVkSignalSemaphoreCount #-}
        writeVkSignalSemaphoreCount p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "signalSemaphoreCount" VkDeviceGroupSubmitInfoKHX where
        type FieldType "signalSemaphoreCount" VkDeviceGroupSubmitInfoKHX =
             Word32
        type FieldOptional "signalSemaphoreCount"
               VkDeviceGroupSubmitInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "signalSemaphoreCount" VkDeviceGroupSubmitInfoKHX
             =
             #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}
        type FieldIsArray "signalSemaphoreCount" VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, signalSemaphoreCount}

instance CanReadField "signalSemaphoreCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSignalSemaphoreCount

        {-# INLINE readField #-}
        readField = readVkSignalSemaphoreCount

instance CanWriteField "signalSemaphoreCount"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSignalSemaphoreCount

instance {-# OVERLAPPING #-}
         HasVkPSignalSemaphoreDeviceIndices VkDeviceGroupSubmitInfoKHX where
        type VkPSignalSemaphoreDeviceIndicesMType
               VkDeviceGroupSubmitInfoKHX
             = Ptr Word32

        {-# NOINLINE vkPSignalSemaphoreDeviceIndices #-}
        vkPSignalSemaphoreDeviceIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices})

        {-# INLINE vkPSignalSemaphoreDeviceIndicesByteOffset #-}
        vkPSignalSemaphoreDeviceIndicesByteOffset ~_
          = #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}

        {-# INLINE readVkPSignalSemaphoreDeviceIndices #-}
        readVkPSignalSemaphoreDeviceIndices p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}

        {-# INLINE writeVkPSignalSemaphoreDeviceIndices #-}
        writeVkPSignalSemaphoreDeviceIndices p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "pSignalSemaphoreDeviceIndices" VkDeviceGroupSubmitInfoKHX
         where
        type FieldType "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = Ptr Word32
        type FieldOptional "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             =
             #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}
        type FieldIsArray "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfoKHX, pSignalSemaphoreDeviceIndices}

instance CanReadField "pSignalSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPSignalSemaphoreDeviceIndices

        {-# INLINE readField #-}
        readField = readVkPSignalSemaphoreDeviceIndices

instance CanWriteField "pSignalSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPSignalSemaphoreDeviceIndices

instance Show VkDeviceGroupSubmitInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupSubmitInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkWaitSemaphoreCount = " .
                            showsPrec d (vkWaitSemaphoreCount x) .
                              showString ", " .
                                showString "vkPWaitSemaphoreDeviceIndices = " .
                                  showsPrec d (vkPWaitSemaphoreDeviceIndices x) .
                                    showString ", " .
                                      showString "vkCommandBufferCount = " .
                                        showsPrec d (vkCommandBufferCount x) .
                                          showString ", " .
                                            showString "vkPCommandBufferDeviceMasks = " .
                                              showsPrec d (vkPCommandBufferDeviceMasks x) .
                                                showString ", " .
                                                  showString "vkSignalSemaphoreCount = " .
                                                    showsPrec d (vkSignalSemaphoreCount x) .
                                                      showString ", " .
                                                        showString
                                                          "vkPSignalSemaphoreDeviceIndices = "
                                                          .
                                                          showsPrec d
                                                            (vkPSignalSemaphoreDeviceIndices x)
                                                            . showChar '}'

-- | > typedef struct VkDeviceGroupBindSparseInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         resourceDeviceIndex;
--   >     uint32_t                         memoryDeviceIndex;
--   > } VkDeviceGroupBindSparseInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGroupBindSparseInfoKHX.html VkDeviceGroupBindSparseInfoKHX registry at www.khronos.org>
data VkDeviceGroupBindSparseInfoKHX = VkDeviceGroupBindSparseInfoKHX## Addr##
                                                                      ByteArray##

instance Eq VkDeviceGroupBindSparseInfoKHX where
        (VkDeviceGroupBindSparseInfoKHX## a _) ==
          x@(VkDeviceGroupBindSparseInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupBindSparseInfoKHX where
        (VkDeviceGroupBindSparseInfoKHX## a _) `compare`
          x@(VkDeviceGroupBindSparseInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupBindSparseInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupBindSparseInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupBindSparseInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupBindSparseInfoKHX where
        unsafeAddr (VkDeviceGroupBindSparseInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupBindSparseInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupBindSparseInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupBindSparseInfoKHX where
        type StructFields VkDeviceGroupBindSparseInfoKHX =
             '["sType", "pNext", "resourceDeviceIndex", "memoryDeviceIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupBindSparseInfoKHX =
             '[VkBindSparseInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupBindSparseInfoKHX where
        type VkSTypeMType VkDeviceGroupBindSparseInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupBindSparseInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "sType" VkDeviceGroupBindSparseInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupBindSparseInfoKHX =
             #{offset VkDeviceGroupBindSparseInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, sType}

instance CanReadField "sType" VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupBindSparseInfoKHX where
        type VkPNextMType VkDeviceGroupBindSparseInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "pNext" VkDeviceGroupBindSparseInfoKHX = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupBindSparseInfoKHX =
             #{offset VkDeviceGroupBindSparseInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkResourceDeviceIndex VkDeviceGroupBindSparseInfoKHX where
        type VkResourceDeviceIndexMType VkDeviceGroupBindSparseInfoKHX =
             Word32

        {-# NOINLINE vkResourceDeviceIndex #-}
        vkResourceDeviceIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex})

        {-# INLINE vkResourceDeviceIndexByteOffset #-}
        vkResourceDeviceIndexByteOffset ~_
          = #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

        {-# INLINE readVkResourceDeviceIndex #-}
        readVkResourceDeviceIndex p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

        {-# INLINE writeVkResourceDeviceIndex #-}
        writeVkResourceDeviceIndex p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         HasField "resourceDeviceIndex" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "resourceDeviceIndex" VkDeviceGroupBindSparseInfoKHX
             = Word32
        type FieldOptional "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             =
             #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}
        type FieldIsArray "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

instance CanReadField "resourceDeviceIndex"
           VkDeviceGroupBindSparseInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkResourceDeviceIndex

        {-# INLINE readField #-}
        readField = readVkResourceDeviceIndex

instance CanWriteField "resourceDeviceIndex"
           VkDeviceGroupBindSparseInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkResourceDeviceIndex

instance {-# OVERLAPPING #-}
         HasVkMemoryDeviceIndex VkDeviceGroupBindSparseInfoKHX where
        type VkMemoryDeviceIndexMType VkDeviceGroupBindSparseInfoKHX =
             Word32

        {-# NOINLINE vkMemoryDeviceIndex #-}
        vkMemoryDeviceIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex})

        {-# INLINE vkMemoryDeviceIndexByteOffset #-}
        vkMemoryDeviceIndexByteOffset ~_
          = #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

        {-# INLINE readVkMemoryDeviceIndex #-}
        readVkMemoryDeviceIndex p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

        {-# INLINE writeVkMemoryDeviceIndex #-}
        writeVkMemoryDeviceIndex p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

instance {-# OVERLAPPING #-}
         HasField "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX =
             Word32
        type FieldOptional "memoryDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX
             =
             #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}
        type FieldIsArray "memoryDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

instance CanReadField "memoryDeviceIndex"
           VkDeviceGroupBindSparseInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkMemoryDeviceIndex

        {-# INLINE readField #-}
        readField = readVkMemoryDeviceIndex

instance CanWriteField "memoryDeviceIndex"
           VkDeviceGroupBindSparseInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMemoryDeviceIndex

instance Show VkDeviceGroupBindSparseInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupBindSparseInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkResourceDeviceIndex = " .
                            showsPrec d (vkResourceDeviceIndex x) .
                              showString ", " .
                                showString "vkMemoryDeviceIndex = " .
                                  showsPrec d (vkMemoryDeviceIndex x) . showChar '}'

-- | > void vkGetDeviceGroupPeerMemoryFeaturesKHX
--   >     ( VkDevice device
--   >     , uint32_t heapIndex
--   >     , uint32_t localDeviceIndex
--   >     , uint32_t remoteDeviceIndex
--   >     , VkPeerMemoryFeatureFlagsKHX* pPeerMemoryFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceGroupPeerMemoryFeaturesKHX.html vkGetDeviceGroupPeerMemoryFeaturesKHX registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceGroupPeerMemoryFeaturesKHX"
               vkGetDeviceGroupPeerMemoryFeaturesKHX ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ heapIndex
                        ->
                   Word32 -- ^ localDeviceIndex
                          -> Word32 -- ^ remoteDeviceIndex
                                    -> Ptr VkPeerMemoryFeatureFlagsKHX -- ^ pPeerMemoryFeatures
                                                                       -> IO ()

-- | queues: @graphics,compute,transfer@
--
--   renderpass: @both@
--
--   > void vkCmdSetDeviceMaskKHX
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t deviceMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetDeviceMaskKHX.html vkCmdSetDeviceMaskKHX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDeviceMaskKHX"
               vkCmdSetDeviceMaskKHX :: VkCommandBuffer -- ^ commandBuffer
                                                        -> Word32 -- ^ deviceMask
                                                                  -> IO ()

-- | queues: @compute@
--
--   renderpass: @outside@
--
--   > void vkCmdDispatchBaseKHX
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t baseGroupX
--   >     , uint32_t baseGroupY
--   >     , uint32_t baseGroupZ
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDispatchBaseKHX.html vkCmdDispatchBaseKHX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDispatchBaseKHX"
               vkCmdDispatchBaseKHX ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ baseGroupX
                        -> Word32 -- ^ baseGroupY
                                  -> Word32 -- ^ baseGroupZ
                                            -> Word32 -- ^ groupCountX
                                                      -> Word32 -- ^ groupCountY
                                                                -> Word32 -- ^ groupCountZ
                                                                          -> IO ()

pattern VK_KHX_DEVICE_GROUP_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHX_DEVICE_GROUP_SPEC_VERSION = 2

type VK_KHX_DEVICE_GROUP_SPEC_VERSION = 2

pattern VK_KHX_DEVICE_GROUP_EXTENSION_NAME :: CString

pattern VK_KHX_DEVICE_GROUP_EXTENSION_NAME <-
        (is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME -> True)
  where VK_KHX_DEVICE_GROUP_EXTENSION_NAME
          = _VK_KHX_DEVICE_GROUP_EXTENSION_NAME

{-# INLINE _VK_KHX_DEVICE_GROUP_EXTENSION_NAME #-}

_VK_KHX_DEVICE_GROUP_EXTENSION_NAME :: CString
_VK_KHX_DEVICE_GROUP_EXTENSION_NAME
  = Ptr "VK_KHX_device_group\NUL"##

{-# INLINE is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME #-}

is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME :: CString -> Bool
is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME
  = eqCStrings _VK_KHX_DEVICE_GROUP_EXTENSION_NAME

type VK_KHX_DEVICE_GROUP_EXTENSION_NAME = "VK_KHX_device_group"

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX =
        VkStructureType 1000060000

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX =
        VkStructureType 1000060003

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX
        = VkStructureType 1000060004

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX =
        VkStructureType 1000060005

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX =
        VkStructureType 1000060006

pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX =
        VkStructureType 1000060010

-- | bitpos = @3@
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX =
        VkPipelineCreateFlagBits 8

-- | bitpos = @4@
pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHX ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHX =
        VkPipelineCreateFlagBits 16

-- | Dependency is across devices
--
--   bitpos = @2@
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX :: VkDependencyFlagBits

pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX = VkDependencyFlagBits 4

-- | > typedef struct VkBindBufferMemoryDeviceGroupInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   > } VkBindBufferMemoryDeviceGroupInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBindBufferMemoryDeviceGroupInfoKHX.html VkBindBufferMemoryDeviceGroupInfoKHX registry at www.khronos.org>
data VkBindBufferMemoryDeviceGroupInfoKHX = VkBindBufferMemoryDeviceGroupInfoKHX## Addr##
                                                                                  ByteArray##

instance Eq VkBindBufferMemoryDeviceGroupInfoKHX where
        (VkBindBufferMemoryDeviceGroupInfoKHX## a _) ==
          x@(VkBindBufferMemoryDeviceGroupInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindBufferMemoryDeviceGroupInfoKHX where
        (VkBindBufferMemoryDeviceGroupInfoKHX## a _) `compare`
          x@(VkBindBufferMemoryDeviceGroupInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindBufferMemoryDeviceGroupInfoKHX where
        sizeOf ~_
          = #{size VkBindBufferMemoryDeviceGroupInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindBufferMemoryDeviceGroupInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindBufferMemoryDeviceGroupInfoKHX
         where
        unsafeAddr (VkBindBufferMemoryDeviceGroupInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindBufferMemoryDeviceGroupInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindBufferMemoryDeviceGroupInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindBufferMemoryDeviceGroupInfoKHX where
        type StructFields VkBindBufferMemoryDeviceGroupInfoKHX =
             '["sType", "pNext", "deviceIndexCount", "pDeviceIndices"] -- ' closing tick for hsc2hs
        type CUnionType VkBindBufferMemoryDeviceGroupInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindBufferMemoryDeviceGroupInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindBufferMemoryDeviceGroupInfoKHX =
             '[VkBindBufferMemoryInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkBindBufferMemoryDeviceGroupInfoKHX where
        type VkSTypeMType VkBindBufferMemoryDeviceGroupInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindBufferMemoryDeviceGroupInfoKHX where
        type FieldType "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}
        type FieldIsArray "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

instance CanReadField "sType" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkBindBufferMemoryDeviceGroupInfoKHX where
        type VkPNextMType VkBindBufferMemoryDeviceGroupInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindBufferMemoryDeviceGroupInfoKHX where
        type FieldType "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}
        type FieldIsArray "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

instance CanReadField "pNext" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDeviceIndexCount VkBindBufferMemoryDeviceGroupInfoKHX where
        type VkDeviceIndexCountMType VkBindBufferMemoryDeviceGroupInfoKHX =
             Word32

        {-# NOINLINE vkDeviceIndexCount #-}
        vkDeviceIndexCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount})

        {-# INLINE vkDeviceIndexCountByteOffset #-}
        vkDeviceIndexCountByteOffset ~_
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

        {-# INLINE readVkDeviceIndexCount #-}
        readVkDeviceIndexCount p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

        {-# INLINE writeVkDeviceIndexCount #-}
        writeVkDeviceIndexCount p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance {-# OVERLAPPING #-}
         HasField "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        type FieldType "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = Word32
        type FieldOptional "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}
        type FieldIsArray "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance CanReadField "deviceIndexCount"
           VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceIndexCount

        {-# INLINE readField #-}
        readField = readVkDeviceIndexCount

instance CanWriteField "deviceIndexCount"
           VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceIndexCount

instance {-# OVERLAPPING #-}
         HasVkPDeviceIndices VkBindBufferMemoryDeviceGroupInfoKHX where
        type VkPDeviceIndicesMType VkBindBufferMemoryDeviceGroupInfoKHX =
             Ptr Word32

        {-# NOINLINE vkPDeviceIndices #-}
        vkPDeviceIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices})

        {-# INLINE vkPDeviceIndicesByteOffset #-}
        vkPDeviceIndicesByteOffset ~_
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

        {-# INLINE readVkPDeviceIndices #-}
        readVkPDeviceIndices p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

        {-# INLINE writeVkPDeviceIndices #-}
        writeVkPDeviceIndices p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        type FieldType "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = Ptr Word32
        type FieldOptional "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}
        type FieldIsArray "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance CanReadField "pDeviceIndices"
           VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPDeviceIndices

        {-# INLINE readField #-}
        readField = readVkPDeviceIndices

instance CanWriteField "pDeviceIndices"
           VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDeviceIndices

instance Show VkBindBufferMemoryDeviceGroupInfoKHX where
        showsPrec d x
          = showString "VkBindBufferMemoryDeviceGroupInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceIndexCount = " .
                            showsPrec d (vkDeviceIndexCount x) .
                              showString ", " .
                                showString "vkPDeviceIndices = " .
                                  showsPrec d (vkPDeviceIndices x) . showChar '}'

-- | > typedef struct VkBindImageMemoryDeviceGroupInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   >     uint32_t         SFRRectCount;
--   >     const VkRect2D*  pSFRRects;
--   > } VkBindImageMemoryDeviceGroupInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBindImageMemoryDeviceGroupInfoKHX.html VkBindImageMemoryDeviceGroupInfoKHX registry at www.khronos.org>
data VkBindImageMemoryDeviceGroupInfoKHX = VkBindImageMemoryDeviceGroupInfoKHX## Addr##
                                                                                ByteArray##

instance Eq VkBindImageMemoryDeviceGroupInfoKHX where
        (VkBindImageMemoryDeviceGroupInfoKHX## a _) ==
          x@(VkBindImageMemoryDeviceGroupInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemoryDeviceGroupInfoKHX where
        (VkBindImageMemoryDeviceGroupInfoKHX## a _) `compare`
          x@(VkBindImageMemoryDeviceGroupInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemoryDeviceGroupInfoKHX where
        sizeOf ~_ = #{size VkBindImageMemoryDeviceGroupInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindImageMemoryDeviceGroupInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImageMemoryDeviceGroupInfoKHX
         where
        unsafeAddr (VkBindImageMemoryDeviceGroupInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImageMemoryDeviceGroupInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImageMemoryDeviceGroupInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImageMemoryDeviceGroupInfoKHX where
        type StructFields VkBindImageMemoryDeviceGroupInfoKHX =
             '["sType", "pNext", "deviceIndexCount", "pDeviceIndices", -- ' closing tick for hsc2hs
               "SFRRectCount", "pSFRRects"]
        type CUnionType VkBindImageMemoryDeviceGroupInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImageMemoryDeviceGroupInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImageMemoryDeviceGroupInfoKHX =
             '[VkBindImageMemoryInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkBindImageMemoryDeviceGroupInfoKHX where
        type VkSTypeMType VkBindImageMemoryDeviceGroupInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImageMemoryDeviceGroupInfoKHX where
        type FieldType "sType" VkBindImageMemoryDeviceGroupInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkBindImageMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImageMemoryDeviceGroupInfoKHX =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}
        type FieldIsArray "sType" VkBindImageMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}

instance CanReadField "sType" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkBindImageMemoryDeviceGroupInfoKHX where
        type VkPNextMType VkBindImageMemoryDeviceGroupInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImageMemoryDeviceGroupInfoKHX where
        type FieldType "pNext" VkBindImageMemoryDeviceGroupInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkBindImageMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImageMemoryDeviceGroupInfoKHX =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}
        type FieldIsArray "pNext" VkBindImageMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}

instance CanReadField "pNext" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDeviceIndexCount VkBindImageMemoryDeviceGroupInfoKHX where
        type VkDeviceIndexCountMType VkBindImageMemoryDeviceGroupInfoKHX =
             Word32

        {-# NOINLINE vkDeviceIndexCount #-}
        vkDeviceIndexCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount})

        {-# INLINE vkDeviceIndexCountByteOffset #-}
        vkDeviceIndexCountByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}

        {-# INLINE readVkDeviceIndexCount #-}
        readVkDeviceIndexCount p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}

        {-# INLINE writeVkDeviceIndexCount #-}
        writeVkDeviceIndexCount p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance {-# OVERLAPPING #-}
         HasField "deviceIndexCount" VkBindImageMemoryDeviceGroupInfoKHX
         where
        type FieldType "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             = Word32
        type FieldOptional "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}
        type FieldIsArray "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance CanReadField "deviceIndexCount"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceIndexCount

        {-# INLINE readField #-}
        readField = readVkDeviceIndexCount

instance CanWriteField "deviceIndexCount"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceIndexCount

instance {-# OVERLAPPING #-}
         HasVkPDeviceIndices VkBindImageMemoryDeviceGroupInfoKHX where
        type VkPDeviceIndicesMType VkBindImageMemoryDeviceGroupInfoKHX =
             Ptr Word32

        {-# NOINLINE vkPDeviceIndices #-}
        vkPDeviceIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices})

        {-# INLINE vkPDeviceIndicesByteOffset #-}
        vkPDeviceIndicesByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}

        {-# INLINE readVkPDeviceIndices #-}
        readVkPDeviceIndices p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}

        {-# INLINE writeVkPDeviceIndices #-}
        writeVkPDeviceIndices p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "pDeviceIndices" VkBindImageMemoryDeviceGroupInfoKHX where
        type FieldType "pDeviceIndices" VkBindImageMemoryDeviceGroupInfoKHX
             = Ptr Word32
        type FieldOptional "pDeviceIndices"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceIndices"
               VkBindImageMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}
        type FieldIsArray "pDeviceIndices"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance CanReadField "pDeviceIndices"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPDeviceIndices

        {-# INLINE readField #-}
        readField = readVkPDeviceIndices

instance CanWriteField "pDeviceIndices"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDeviceIndices

instance {-# OVERLAPPING #-}
         HasVkSFRRectCount VkBindImageMemoryDeviceGroupInfoKHX where
        type VkSFRRectCountMType VkBindImageMemoryDeviceGroupInfoKHX =
             Word32

        {-# NOINLINE vkSFRRectCount #-}
        vkSFRRectCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount})

        {-# INLINE vkSFRRectCountByteOffset #-}
        vkSFRRectCountByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}

        {-# INLINE readVkSFRRectCount #-}
        readVkSFRRectCount p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}

        {-# INLINE writeVkSFRRectCount #-}
        writeVkSFRRectCount p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}

instance {-# OVERLAPPING #-}
         HasField "SFRRectCount" VkBindImageMemoryDeviceGroupInfoKHX where
        type FieldType "SFRRectCount" VkBindImageMemoryDeviceGroupInfoKHX =
             Word32
        type FieldOptional "SFRRectCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "SFRRectCount" VkBindImageMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}
        type FieldIsArray "SFRRectCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}

instance CanReadField "SFRRectCount"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSFRRectCount

        {-# INLINE readField #-}
        readField = readVkSFRRectCount

instance CanWriteField "SFRRectCount"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSFRRectCount

instance {-# OVERLAPPING #-}
         HasVkPSFRRects VkBindImageMemoryDeviceGroupInfoKHX where
        type VkPSFRRectsMType VkBindImageMemoryDeviceGroupInfoKHX =
             Ptr VkRect2D

        {-# NOINLINE vkPSFRRects #-}
        vkPSFRRects x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects})

        {-# INLINE vkPSFRRectsByteOffset #-}
        vkPSFRRectsByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}

        {-# INLINE readVkPSFRRects #-}
        readVkPSFRRects p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}

        {-# INLINE writeVkPSFRRects #-}
        writeVkPSFRRects p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}

instance {-# OVERLAPPING #-}
         HasField "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX where
        type FieldType "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX =
             Ptr VkRect2D
        type FieldOptional "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}
        type FieldIsArray "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}

instance CanReadField "pSFRRects"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPSFRRects

        {-# INLINE readField #-}
        readField = readVkPSFRRects

instance CanWriteField "pSFRRects"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPSFRRects

instance Show VkBindImageMemoryDeviceGroupInfoKHX where
        showsPrec d x
          = showString "VkBindImageMemoryDeviceGroupInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceIndexCount = " .
                            showsPrec d (vkDeviceIndexCount x) .
                              showString ", " .
                                showString "vkPDeviceIndices = " .
                                  showsPrec d (vkPDeviceIndices x) .
                                    showString ", " .
                                      showString "vkSFRRectCount = " .
                                        showsPrec d (vkSFRRectCount x) .
                                          showString ", " .
                                            showString "vkPSFRRects = " .
                                              showsPrec d (vkPSFRRects x) . showChar '}'

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHX
        = VkStructureType 1000060013

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHX =
        VkStructureType 1000060014

-- | Allows using VkBindImageMemoryDeviceGroupInfoKHX::pSFRRects when binding memory to the image
--
--   bitpos = @6@
pattern VK_IMAGE_CREATE_BIND_SFR_BIT_KHX :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_BIND_SFR_BIT_KHX = VkImageCreateFlagBits 64

-- | > typedef struct VkDeviceGroupPresentCapabilitiesKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         presentMask[VK_MAX_DEVICE_GROUP_SIZE_KHX];
--   >     VkDeviceGroupPresentModeFlagsKHX modes;
--   > } VkDeviceGroupPresentCapabilitiesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGroupPresentCapabilitiesKHX.html VkDeviceGroupPresentCapabilitiesKHX registry at www.khronos.org>
data VkDeviceGroupPresentCapabilitiesKHX = VkDeviceGroupPresentCapabilitiesKHX## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupPresentCapabilitiesKHX where
        (VkDeviceGroupPresentCapabilitiesKHX## a _) ==
          x@(VkDeviceGroupPresentCapabilitiesKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentCapabilitiesKHX where
        (VkDeviceGroupPresentCapabilitiesKHX## a _) `compare`
          x@(VkDeviceGroupPresentCapabilitiesKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentCapabilitiesKHX where
        sizeOf ~_ = #{size VkDeviceGroupPresentCapabilitiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupPresentCapabilitiesKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupPresentCapabilitiesKHX
         where
        unsafeAddr (VkDeviceGroupPresentCapabilitiesKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupPresentCapabilitiesKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupPresentCapabilitiesKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupPresentCapabilitiesKHX where
        type StructFields VkDeviceGroupPresentCapabilitiesKHX =
             '["sType", "pNext", "presentMask", "modes"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupPresentCapabilitiesKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupPresentCapabilitiesKHX = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupPresentCapabilitiesKHX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupPresentCapabilitiesKHX where
        type VkSTypeMType VkDeviceGroupPresentCapabilitiesKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "sType" VkDeviceGroupPresentCapabilitiesKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupPresentCapabilitiesKHX =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

instance CanReadField "sType" VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupPresentCapabilitiesKHX where
        type VkPNextMType VkDeviceGroupPresentCapabilitiesKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkPresentMaskArray VkDeviceGroupPresentCapabilitiesKHX where
        type VkPresentMaskArrayMType VkDeviceGroupPresentCapabilitiesKHX =
             Word32

        {-# NOINLINE vkPresentMaskArray #-}
        vkPresentMaskArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word32) +
                    #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}))

        {-# INLINE vkPresentMaskArrayByteOffset #-}
        vkPresentMaskArrayByteOffset ~_
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}

        {-# INLINE readVkPresentMaskArray #-}
        readVkPresentMaskArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask})

        {-# INLINE writeVkPresentMaskArray #-}
        writeVkPresentMaskArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask})

instance {-# OVERLAPPING #-}
         HasField "presentMask" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "presentMask" VkDeviceGroupPresentCapabilitiesKHX =
             Word32
        type FieldOptional "presentMask"
               VkDeviceGroupPresentCapabilitiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "presentMask" VkDeviceGroupPresentCapabilitiesKHX
             =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}
        type FieldIsArray "presentMask" VkDeviceGroupPresentCapabilitiesKHX
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}

instance (KnownNat idx,
          IndexInBounds "presentMask" idx
            VkDeviceGroupPresentCapabilitiesKHX) =>
         CanReadFieldArray "presentMask" idx
           VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 0
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 1
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 2
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 3
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}
        type FieldArrayLength "presentMask"
               VkDeviceGroupPresentCapabilitiesKHX
             = VK_MAX_DEVICE_GROUP_SIZE_KHX

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_DEVICE_GROUP_SIZE_KHX

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkPresentMaskArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkPresentMaskArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkModes VkDeviceGroupPresentCapabilitiesKHX where
        type VkModesMType VkDeviceGroupPresentCapabilitiesKHX =
             VkDeviceGroupPresentModeFlagsKHX

        {-# NOINLINE vkModes #-}
        vkModes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHX, modes})

        {-# INLINE vkModesByteOffset #-}
        vkModesByteOffset ~_
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

        {-# INLINE readVkModes #-}
        readVkModes p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

        {-# INLINE writeVkModes #-}
        writeVkModes p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

instance {-# OVERLAPPING #-}
         HasField "modes" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "modes" VkDeviceGroupPresentCapabilitiesKHX =
             VkDeviceGroupPresentModeFlagsKHX
        type FieldOptional "modes" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "modes" VkDeviceGroupPresentCapabilitiesKHX =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}
        type FieldIsArray "modes" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

instance CanReadField "modes" VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# INLINE getField #-}
        getField = vkModes

        {-# INLINE readField #-}
        readField = readVkModes

instance Show VkDeviceGroupPresentCapabilitiesKHX where
        showsPrec d x
          = showString "VkDeviceGroupPresentCapabilitiesKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPresentMaskArray = [" .
                            showsPrec d
                              (map (vkPresentMaskArray x) [1 .. VK_MAX_DEVICE_GROUP_SIZE_KHX])
                              .
                              showChar ']' .
                                showString ", " .
                                  showString "vkModes = " . showsPrec d (vkModes x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDeviceGroupPresentCapabilitiesKHX
--   >     ( VkDevice device
--   >     , VkDeviceGroupPresentCapabilitiesKHX* pDeviceGroupPresentCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceGroupPresentCapabilitiesKHX.html vkGetDeviceGroupPresentCapabilitiesKHX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetDeviceGroupPresentCapabilitiesKHX"
               vkGetDeviceGroupPresentCapabilitiesKHX ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceGroupPresentCapabilitiesKHX -- ^ pDeviceGroupPresentCapabilities
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetDeviceGroupSurfacePresentModesKHX
--   >     ( VkDevice device
--   >     , VkSurfaceKHR surface
--   >     , VkDeviceGroupPresentModeFlagsKHX* pModes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceGroupSurfacePresentModesKHX.html vkGetDeviceGroupSurfacePresentModesKHX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetDeviceGroupSurfacePresentModesKHX"
               vkGetDeviceGroupSurfacePresentModesKHX ::
               VkDevice -- ^ device
                        ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkDeviceGroupPresentModeFlagsKHX -- ^ pModes
                                                                      -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDevicePresentRectanglesKHX
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , uint32_t* pRectCount
--   >     , VkRect2D* pRects
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDevicePresentRectanglesKHX.html vkGetPhysicalDevicePresentRectanglesKHX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDevicePresentRectanglesKHX"
               vkGetPhysicalDevicePresentRectanglesKHX ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pRectCount
                                            -> Ptr VkRect2D -- ^ pRects
                                                            -> IO VkResult

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX =
        VkStructureType 1000060007

-- | > typedef struct VkImageSwapchainCreateInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR   swapchain;
--   > } VkImageSwapchainCreateInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageSwapchainCreateInfoKHX.html VkImageSwapchainCreateInfoKHX registry at www.khronos.org>
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

-- | > typedef struct VkBindImageMemorySwapchainInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR swapchain;
--   >     uint32_t                         imageIndex;
--   > } VkBindImageMemorySwapchainInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBindImageMemorySwapchainInfoKHX.html VkBindImageMemorySwapchainInfoKHX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAcquireNextImageInfoKHX.html VkAcquireNextImageInfoKHX registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkAcquireNextImageInfoKHX
         where
        type VkSTypeMType VkAcquireNextImageInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, sType}

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

instance CanReadField "sType" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkAcquireNextImageInfoKHX
         where
        type VkPNextMType VkAcquireNextImageInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, pNext}

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

instance CanReadField "pNext" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSwapchain VkAcquireNextImageInfoKHX where
        type VkSwapchainMType VkAcquireNextImageInfoKHX = VkSwapchainKHR

        {-# NOINLINE vkSwapchain #-}
        vkSwapchain x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, swapchain})

        {-# INLINE vkSwapchainByteOffset #-}
        vkSwapchainByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, swapchain}

        {-# INLINE readVkSwapchain #-}
        readVkSwapchain p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, swapchain}

        {-# INLINE writeVkSwapchain #-}
        writeVkSwapchain p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, swapchain}

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

instance CanReadField "swapchain" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkSwapchain

        {-# INLINE readField #-}
        readField = readVkSwapchain

instance CanWriteField "swapchain" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSwapchain

instance {-# OVERLAPPING #-} HasVkTimeout VkAcquireNextImageInfoKHX
         where
        type VkTimeoutMType VkAcquireNextImageInfoKHX = Word64

        {-# NOINLINE vkTimeout #-}
        vkTimeout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, timeout})

        {-# INLINE vkTimeoutByteOffset #-}
        vkTimeoutByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, timeout}

        {-# INLINE readVkTimeout #-}
        readVkTimeout p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, timeout}

        {-# INLINE writeVkTimeout #-}
        writeVkTimeout p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, timeout}

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

instance CanReadField "timeout" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkTimeout

        {-# INLINE readField #-}
        readField = readVkTimeout

instance CanWriteField "timeout" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkTimeout

instance {-# OVERLAPPING #-}
         HasVkSemaphore VkAcquireNextImageInfoKHX where
        type VkSemaphoreMType VkAcquireNextImageInfoKHX = VkSemaphore

        {-# NOINLINE vkSemaphore #-}
        vkSemaphore x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, semaphore})

        {-# INLINE vkSemaphoreByteOffset #-}
        vkSemaphoreByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, semaphore}

        {-# INLINE readVkSemaphore #-}
        readVkSemaphore p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, semaphore}

        {-# INLINE writeVkSemaphore #-}
        writeVkSemaphore p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, semaphore}

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

instance CanReadField "semaphore" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkSemaphore

        {-# INLINE readField #-}
        readField = readVkSemaphore

instance CanWriteField "semaphore" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSemaphore

instance {-# OVERLAPPING #-} HasVkFence VkAcquireNextImageInfoKHX
         where
        type VkFenceMType VkAcquireNextImageInfoKHX = VkFence

        {-# NOINLINE vkFence #-}
        vkFence x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, fence})

        {-# INLINE vkFenceByteOffset #-}
        vkFenceByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, fence}

        {-# INLINE readVkFence #-}
        readVkFence p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, fence}

        {-# INLINE writeVkFence #-}
        writeVkFence p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, fence}

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

instance CanReadField "fence" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkFence

        {-# INLINE readField #-}
        readField = readVkFence

instance CanWriteField "fence" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkFence

instance {-# OVERLAPPING #-}
         HasVkDeviceMask VkAcquireNextImageInfoKHX where
        type VkDeviceMaskMType VkAcquireNextImageInfoKHX = Word32

        {-# NOINLINE vkDeviceMask #-}
        vkDeviceMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAcquireNextImageInfoKHX, deviceMask})

        {-# INLINE vkDeviceMaskByteOffset #-}
        vkDeviceMaskByteOffset ~_
          = #{offset VkAcquireNextImageInfoKHX, deviceMask}

        {-# INLINE readVkDeviceMask #-}
        readVkDeviceMask p
          = peekByteOff p #{offset VkAcquireNextImageInfoKHX, deviceMask}

        {-# INLINE writeVkDeviceMask #-}
        writeVkDeviceMask p
          = pokeByteOff p #{offset VkAcquireNextImageInfoKHX, deviceMask}

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

instance CanReadField "deviceMask" VkAcquireNextImageInfoKHX where
        {-# INLINE getField #-}
        getField = vkDeviceMask

        {-# INLINE readField #-}
        readField = readVkDeviceMask

instance CanWriteField "deviceMask" VkAcquireNextImageInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceMask

instance Show VkAcquireNextImageInfoKHX where
        showsPrec d x
          = showString "VkAcquireNextImageInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSwapchain = " .
                            showsPrec d (vkSwapchain x) .
                              showString ", " .
                                showString "vkTimeout = " .
                                  showsPrec d (vkTimeout x) .
                                    showString ", " .
                                      showString "vkSemaphore = " .
                                        showsPrec d (vkSemaphore x) .
                                          showString ", " .
                                            showString "vkFence = " .
                                              showsPrec d (vkFence x) .
                                                showString ", " .
                                                  showString "vkDeviceMask = " .
                                                    showsPrec d (vkDeviceMask x) . showChar '}'

-- | > typedef struct VkDeviceGroupPresentInfoKHX {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     uint32_t         swapchainCount;
--   >     const uint32_t* pDeviceMasks;
--   >     VkDeviceGroupPresentModeFlagBitsKHX mode;
--   > } VkDeviceGroupPresentInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGroupPresentInfoKHX.html VkDeviceGroupPresentInfoKHX registry at www.khronos.org>
data VkDeviceGroupPresentInfoKHX = VkDeviceGroupPresentInfoKHX## Addr##
                                                                ByteArray##

instance Eq VkDeviceGroupPresentInfoKHX where
        (VkDeviceGroupPresentInfoKHX## a _) ==
          x@(VkDeviceGroupPresentInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentInfoKHX where
        (VkDeviceGroupPresentInfoKHX## a _) `compare`
          x@(VkDeviceGroupPresentInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupPresentInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupPresentInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupPresentInfoKHX where
        unsafeAddr (VkDeviceGroupPresentInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupPresentInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupPresentInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupPresentInfoKHX where
        type StructFields VkDeviceGroupPresentInfoKHX =
             '["sType", "pNext", "swapchainCount", "pDeviceMasks", "mode"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupPresentInfoKHX =
             '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkDeviceGroupPresentInfoKHX
         where
        type VkSTypeMType VkDeviceGroupPresentInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupPresentInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupPresentInfoKHX where
        type FieldType "sType" VkDeviceGroupPresentInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupPresentInfoKHX =
             #{offset VkDeviceGroupPresentInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHX, sType}

instance CanReadField "sType" VkDeviceGroupPresentInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDeviceGroupPresentInfoKHX
         where
        type VkPNextMType VkDeviceGroupPresentInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupPresentInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupPresentInfoKHX where
        type FieldType "pNext" VkDeviceGroupPresentInfoKHX = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupPresentInfoKHX =
             #{offset VkDeviceGroupPresentInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupPresentInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSwapchainCount VkDeviceGroupPresentInfoKHX where
        type VkSwapchainCountMType VkDeviceGroupPresentInfoKHX = Word32

        {-# NOINLINE vkSwapchainCount #-}
        vkSwapchainCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, swapchainCount})

        {-# INLINE vkSwapchainCountByteOffset #-}
        vkSwapchainCountByteOffset ~_
          = #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}

        {-# INLINE readVkSwapchainCount #-}
        readVkSwapchainCount p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}

        {-# INLINE writeVkSwapchainCount #-}
        writeVkSwapchainCount p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkDeviceGroupPresentInfoKHX where
        type FieldType "swapchainCount" VkDeviceGroupPresentInfoKHX =
             Word32
        type FieldOptional "swapchainCount" VkDeviceGroupPresentInfoKHX =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "swapchainCount" VkDeviceGroupPresentInfoKHX =
             #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}
        type FieldIsArray "swapchainCount" VkDeviceGroupPresentInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}

instance CanReadField "swapchainCount" VkDeviceGroupPresentInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSwapchainCount

        {-# INLINE readField #-}
        readField = readVkSwapchainCount

instance CanWriteField "swapchainCount" VkDeviceGroupPresentInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSwapchainCount

instance {-# OVERLAPPING #-}
         HasVkPDeviceMasks VkDeviceGroupPresentInfoKHX where
        type VkPDeviceMasksMType VkDeviceGroupPresentInfoKHX = Ptr Word32

        {-# NOINLINE vkPDeviceMasks #-}
        vkPDeviceMasks x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks})

        {-# INLINE vkPDeviceMasksByteOffset #-}
        vkPDeviceMasksByteOffset ~_
          = #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}

        {-# INLINE readVkPDeviceMasks #-}
        readVkPDeviceMasks p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}

        {-# INLINE writeVkPDeviceMasks #-}
        writeVkPDeviceMasks p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}

instance {-# OVERLAPPING #-}
         HasField "pDeviceMasks" VkDeviceGroupPresentInfoKHX where
        type FieldType "pDeviceMasks" VkDeviceGroupPresentInfoKHX =
             Ptr Word32
        type FieldOptional "pDeviceMasks" VkDeviceGroupPresentInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceMasks" VkDeviceGroupPresentInfoKHX =
             #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}
        type FieldIsArray "pDeviceMasks" VkDeviceGroupPresentInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}

instance CanReadField "pDeviceMasks" VkDeviceGroupPresentInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPDeviceMasks

        {-# INLINE readField #-}
        readField = readVkPDeviceMasks

instance CanWriteField "pDeviceMasks" VkDeviceGroupPresentInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDeviceMasks

instance {-# OVERLAPPING #-} HasVkMode VkDeviceGroupPresentInfoKHX
         where
        type VkModeMType VkDeviceGroupPresentInfoKHX =
             VkDeviceGroupPresentModeFlagBitsKHX

        {-# NOINLINE vkMode #-}
        vkMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, mode})

        {-# INLINE vkModeByteOffset #-}
        vkModeByteOffset ~_
          = #{offset VkDeviceGroupPresentInfoKHX, mode}

        {-# INLINE readVkMode #-}
        readVkMode p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, mode}

        {-# INLINE writeVkMode #-}
        writeVkMode p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, mode}

instance {-# OVERLAPPING #-}
         HasField "mode" VkDeviceGroupPresentInfoKHX where
        type FieldType "mode" VkDeviceGroupPresentInfoKHX =
             VkDeviceGroupPresentModeFlagBitsKHX
        type FieldOptional "mode" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mode" VkDeviceGroupPresentInfoKHX =
             #{offset VkDeviceGroupPresentInfoKHX, mode}
        type FieldIsArray "mode" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHX, mode}

instance CanReadField "mode" VkDeviceGroupPresentInfoKHX where
        {-# INLINE getField #-}
        getField = vkMode

        {-# INLINE readField #-}
        readField = readVkMode

instance CanWriteField "mode" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkMode

instance Show VkDeviceGroupPresentInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupPresentInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSwapchainCount = " .
                            showsPrec d (vkSwapchainCount x) .
                              showString ", " .
                                showString "vkPDeviceMasks = " .
                                  showsPrec d (vkPDeviceMasks x) .
                                    showString ", " .
                                      showString "vkMode = " . showsPrec d (vkMode x) . showChar '}'

-- | > typedef struct VkDeviceGroupSwapchainCreateInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceGroupPresentModeFlagsKHX                         modes;
--   > } VkDeviceGroupSwapchainCreateInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGroupSwapchainCreateInfoKHX.html VkDeviceGroupSwapchainCreateInfoKHX registry at www.khronos.org>
data VkDeviceGroupSwapchainCreateInfoKHX = VkDeviceGroupSwapchainCreateInfoKHX## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupSwapchainCreateInfoKHX where
        (VkDeviceGroupSwapchainCreateInfoKHX## a _) ==
          x@(VkDeviceGroupSwapchainCreateInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupSwapchainCreateInfoKHX where
        (VkDeviceGroupSwapchainCreateInfoKHX## a _) `compare`
          x@(VkDeviceGroupSwapchainCreateInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupSwapchainCreateInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupSwapchainCreateInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupSwapchainCreateInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupSwapchainCreateInfoKHX
         where
        unsafeAddr (VkDeviceGroupSwapchainCreateInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupSwapchainCreateInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupSwapchainCreateInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupSwapchainCreateInfoKHX where
        type StructFields VkDeviceGroupSwapchainCreateInfoKHX =
             '["sType", "pNext", "modes"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupSwapchainCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupSwapchainCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupSwapchainCreateInfoKHX =
             '[VkSwapchainCreateInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupSwapchainCreateInfoKHX where
        type VkSTypeMType VkDeviceGroupSwapchainCreateInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupSwapchainCreateInfoKHX where
        type FieldType "sType" VkDeviceGroupSwapchainCreateInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupSwapchainCreateInfoKHX =
             #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType}

instance CanReadField "sType" VkDeviceGroupSwapchainCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupSwapchainCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupSwapchainCreateInfoKHX where
        type VkPNextMType VkDeviceGroupSwapchainCreateInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupSwapchainCreateInfoKHX where
        type FieldType "pNext" VkDeviceGroupSwapchainCreateInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupSwapchainCreateInfoKHX =
             #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupSwapchainCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupSwapchainCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkModes VkDeviceGroupSwapchainCreateInfoKHX where
        type VkModesMType VkDeviceGroupSwapchainCreateInfoKHX =
             VkDeviceGroupPresentModeFlagsKHX

        {-# NOINLINE vkModes #-}
        vkModes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes})

        {-# INLINE vkModesByteOffset #-}
        vkModesByteOffset ~_
          = #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes}

        {-# INLINE readVkModes #-}
        readVkModes p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes}

        {-# INLINE writeVkModes #-}
        writeVkModes p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes}

instance {-# OVERLAPPING #-}
         HasField "modes" VkDeviceGroupSwapchainCreateInfoKHX where
        type FieldType "modes" VkDeviceGroupSwapchainCreateInfoKHX =
             VkDeviceGroupPresentModeFlagsKHX
        type FieldOptional "modes" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "modes" VkDeviceGroupSwapchainCreateInfoKHX =
             #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes}
        type FieldIsArray "modes" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes}

instance CanReadField "modes" VkDeviceGroupSwapchainCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkModes

        {-# INLINE readField #-}
        readField = readVkModes

instance CanWriteField "modes" VkDeviceGroupSwapchainCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkModes

instance Show VkDeviceGroupSwapchainCreateInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupSwapchainCreateInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkModes = " . showsPrec d (vkModes x) . showChar '}'

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkAcquireNextImage2KHX
--   >     ( VkDevice device
--   >     , const VkAcquireNextImageInfoKHX* pAcquireInfo
--   >     , uint32_t* pImageIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAcquireNextImage2KHX.html vkAcquireNextImage2KHX registry at www.khronos.org>
foreign import ccall unsafe "vkAcquireNextImage2KHX"
               vkAcquireNextImage2KHX ::
               VkDevice -- ^ device
                        ->
                 Ptr VkAcquireNextImageInfoKHX -- ^ pAcquireInfo
                                               -> Ptr Word32 -- ^ pImageIndex
                                                             -> IO VkResult

pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX =
        VkStructureType 1000060008

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX =
        VkStructureType 1000060009

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX =
        VkStructureType 1000060011

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX =
        VkStructureType 1000060012

-- | Allow images with VK_IMAGE_CREATE_BIND_SFR_BIT_KHX
--
--   bitpos = @0@
pattern VK_SWAPCHAIN_CREATE_BIND_SFR_BIT_KHX ::
        VkSwapchainCreateFlagBitsKHR

pattern VK_SWAPCHAIN_CREATE_BIND_SFR_BIT_KHX =
        VkSwapchainCreateFlagBitsKHR 1
