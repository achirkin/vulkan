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
{-# LANGUAGE UnboxedTuples            #-}
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
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.TypeLits                     (KnownNat, natVal') -- ' closing tick for hsc2hs
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkRect2D (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryAllocateFlagsInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMemoryAllocateFlagsKHX flags;
--   >     uint32_t                         deviceMask;
--   > } VkMemoryAllocateFlagsInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryAllocateFlagsInfoKHX.html VkMemoryAllocateFlagsInfoKHX registry at www.khronos.org>
data VkMemoryAllocateFlagsInfoKHX = VkMemoryAllocateFlagsInfoKHX## ByteArray##

instance Eq VkMemoryAllocateFlagsInfoKHX where
        (VkMemoryAllocateFlagsInfoKHX## a) ==
          (VkMemoryAllocateFlagsInfoKHX## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkMemoryAllocateFlagsInfoKHX where
        (VkMemoryAllocateFlagsInfoKHX## a) `compare`
          (VkMemoryAllocateFlagsInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkMemoryAllocateFlagsInfoKHX where
        sizeOf ~_ = #{size VkMemoryAllocateFlagsInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryAllocateFlagsInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkMemoryAllocateFlagsInfoKHX),
            I## a <- alignment (undefined :: VkMemoryAllocateFlagsInfoKHX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkMemoryAllocateFlagsInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkMemoryAllocateFlagsInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkMemoryAllocateFlagsInfoKHX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkMemoryAllocateFlagsInfoKHX),
            I## a <- alignment (undefined :: VkMemoryAllocateFlagsInfoKHX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkMemoryAllocateFlagsInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkMemoryAllocateFlagsInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkMemoryAllocateFlagsInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkMemoryAllocateFlagsInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkMemoryAllocateFlagsInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkMemoryAllocateFlagsInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkDeviceGroupRenderPassBeginInfoKHX = VkDeviceGroupRenderPassBeginInfoKHX## ByteArray##

instance Eq VkDeviceGroupRenderPassBeginInfoKHX where
        (VkDeviceGroupRenderPassBeginInfoKHX## a) ==
          (VkDeviceGroupRenderPassBeginInfoKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupRenderPassBeginInfoKHX where
        (VkDeviceGroupRenderPassBeginInfoKHX## a) `compare`
          (VkDeviceGroupRenderPassBeginInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupRenderPassBeginInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupRenderPassBeginInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupRenderPassBeginInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkDeviceGroupRenderPassBeginInfoKHX),
            I## a <- alignment
                      (undefined :: VkDeviceGroupRenderPassBeginInfoKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceGroupRenderPassBeginInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceGroupRenderPassBeginInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkDeviceGroupRenderPassBeginInfoKHX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceGroupRenderPassBeginInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkDeviceGroupRenderPassBeginInfoKHX),
            I## a <- alignment
                      (undefined :: VkDeviceGroupRenderPassBeginInfoKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceGroupRenderPassBeginInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceGroupRenderPassBeginInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDeviceGroupRenderPassBeginInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceGroupRenderPassBeginInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceGroupRenderPassBeginInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceGroupRenderPassBeginInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkDeviceGroupCommandBufferBeginInfoKHX = VkDeviceGroupCommandBufferBeginInfoKHX## ByteArray##

instance Eq VkDeviceGroupCommandBufferBeginInfoKHX where
        (VkDeviceGroupCommandBufferBeginInfoKHX## a) ==
          (VkDeviceGroupCommandBufferBeginInfoKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupCommandBufferBeginInfoKHX where
        (VkDeviceGroupCommandBufferBeginInfoKHX## a) `compare`
          (VkDeviceGroupCommandBufferBeginInfoKHX## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupCommandBufferBeginInfoKHX where
        sizeOf ~_
          = #{size VkDeviceGroupCommandBufferBeginInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupCommandBufferBeginInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkDeviceGroupCommandBufferBeginInfoKHX),
            I## a <- alignment
                      (undefined :: VkDeviceGroupCommandBufferBeginInfoKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceGroupCommandBufferBeginInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceGroupCommandBufferBeginInfoKHX## ba)
          | I## n <- sizeOf
                      (undefined :: VkDeviceGroupCommandBufferBeginInfoKHX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceGroupCommandBufferBeginInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkDeviceGroupCommandBufferBeginInfoKHX),
            I## a <- alignment
                      (undefined :: VkDeviceGroupCommandBufferBeginInfoKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceGroupCommandBufferBeginInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceGroupCommandBufferBeginInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDeviceGroupCommandBufferBeginInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceGroupCommandBufferBeginInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceGroupCommandBufferBeginInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceGroupCommandBufferBeginInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkDeviceGroupSubmitInfoKHX = VkDeviceGroupSubmitInfoKHX## ByteArray##

instance Eq VkDeviceGroupSubmitInfoKHX where
        (VkDeviceGroupSubmitInfoKHX## a) == (VkDeviceGroupSubmitInfoKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupSubmitInfoKHX where
        (VkDeviceGroupSubmitInfoKHX## a) `compare`
          (VkDeviceGroupSubmitInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupSubmitInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupSubmitInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupSubmitInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDeviceGroupSubmitInfoKHX),
            I## a <- alignment (undefined :: VkDeviceGroupSubmitInfoKHX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceGroupSubmitInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceGroupSubmitInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkDeviceGroupSubmitInfoKHX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceGroupSubmitInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDeviceGroupSubmitInfoKHX),
            I## a <- alignment (undefined :: VkDeviceGroupSubmitInfoKHX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceGroupSubmitInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceGroupSubmitInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDeviceGroupSubmitInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceGroupSubmitInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceGroupSubmitInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceGroupSubmitInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkDeviceGroupBindSparseInfoKHX = VkDeviceGroupBindSparseInfoKHX## ByteArray##

instance Eq VkDeviceGroupBindSparseInfoKHX where
        (VkDeviceGroupBindSparseInfoKHX## a) ==
          (VkDeviceGroupBindSparseInfoKHX## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupBindSparseInfoKHX where
        (VkDeviceGroupBindSparseInfoKHX## a) `compare`
          (VkDeviceGroupBindSparseInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupBindSparseInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupBindSparseInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupBindSparseInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDeviceGroupBindSparseInfoKHX),
            I## a <- alignment (undefined :: VkDeviceGroupBindSparseInfoKHX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceGroupBindSparseInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceGroupBindSparseInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkDeviceGroupBindSparseInfoKHX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDeviceGroupBindSparseInfoKHX),
            I## a <- alignment (undefined :: VkDeviceGroupBindSparseInfoKHX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceGroupBindSparseInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceGroupBindSparseInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDeviceGroupBindSparseInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceGroupBindSparseInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceGroupBindSparseInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceGroupBindSparseInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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

_VK_KHX_DEVICE_GROUP_EXTENSION_NAME :: CString

{-# INLINE _VK_KHX_DEVICE_GROUP_EXTENSION_NAME #-}
_VK_KHX_DEVICE_GROUP_EXTENSION_NAME
  = Ptr "VK_KHX_device_group\NUL"##

is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME #-}
is_VK_KHX_DEVICE_GROUP_EXTENSION_NAME
  = (_VK_KHX_DEVICE_GROUP_EXTENSION_NAME ==)

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
data VkBindBufferMemoryDeviceGroupInfoKHX = VkBindBufferMemoryDeviceGroupInfoKHX## ByteArray##

instance Eq VkBindBufferMemoryDeviceGroupInfoKHX where
        (VkBindBufferMemoryDeviceGroupInfoKHX## a) ==
          (VkBindBufferMemoryDeviceGroupInfoKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkBindBufferMemoryDeviceGroupInfoKHX where
        (VkBindBufferMemoryDeviceGroupInfoKHX## a) `compare`
          (VkBindBufferMemoryDeviceGroupInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkBindBufferMemoryDeviceGroupInfoKHX where
        sizeOf ~_
          = #{size VkBindBufferMemoryDeviceGroupInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindBufferMemoryDeviceGroupInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkBindBufferMemoryDeviceGroupInfoKHX),
            I## a <- alignment
                      (undefined :: VkBindBufferMemoryDeviceGroupInfoKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkBindBufferMemoryDeviceGroupInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkBindBufferMemoryDeviceGroupInfoKHX## ba)
          | I## n <- sizeOf
                      (undefined :: VkBindBufferMemoryDeviceGroupInfoKHX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkBindBufferMemoryDeviceGroupInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkBindBufferMemoryDeviceGroupInfoKHX),
            I## a <- alignment
                      (undefined :: VkBindBufferMemoryDeviceGroupInfoKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkBindBufferMemoryDeviceGroupInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkBindBufferMemoryDeviceGroupInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkBindBufferMemoryDeviceGroupInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkBindBufferMemoryDeviceGroupInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkBindBufferMemoryDeviceGroupInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkBindBufferMemoryDeviceGroupInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkBindImageMemoryDeviceGroupInfoKHX = VkBindImageMemoryDeviceGroupInfoKHX## ByteArray##

instance Eq VkBindImageMemoryDeviceGroupInfoKHX where
        (VkBindImageMemoryDeviceGroupInfoKHX## a) ==
          (VkBindImageMemoryDeviceGroupInfoKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemoryDeviceGroupInfoKHX where
        (VkBindImageMemoryDeviceGroupInfoKHX## a) `compare`
          (VkBindImageMemoryDeviceGroupInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemoryDeviceGroupInfoKHX where
        sizeOf ~_ = #{size VkBindImageMemoryDeviceGroupInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindImageMemoryDeviceGroupInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkBindImageMemoryDeviceGroupInfoKHX),
            I## a <- alignment
                      (undefined :: VkBindImageMemoryDeviceGroupInfoKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkBindImageMemoryDeviceGroupInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkBindImageMemoryDeviceGroupInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkBindImageMemoryDeviceGroupInfoKHX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkBindImageMemoryDeviceGroupInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkBindImageMemoryDeviceGroupInfoKHX),
            I## a <- alignment
                      (undefined :: VkBindImageMemoryDeviceGroupInfoKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkBindImageMemoryDeviceGroupInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkBindImageMemoryDeviceGroupInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkBindImageMemoryDeviceGroupInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkBindImageMemoryDeviceGroupInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkBindImageMemoryDeviceGroupInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkBindImageMemoryDeviceGroupInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkDeviceGroupPresentCapabilitiesKHX = VkDeviceGroupPresentCapabilitiesKHX## ByteArray##

instance Eq VkDeviceGroupPresentCapabilitiesKHX where
        (VkDeviceGroupPresentCapabilitiesKHX## a) ==
          (VkDeviceGroupPresentCapabilitiesKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentCapabilitiesKHX where
        (VkDeviceGroupPresentCapabilitiesKHX## a) `compare`
          (VkDeviceGroupPresentCapabilitiesKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentCapabilitiesKHX where
        sizeOf ~_ = #{size VkDeviceGroupPresentCapabilitiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupPresentCapabilitiesKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkDeviceGroupPresentCapabilitiesKHX),
            I## a <- alignment
                      (undefined :: VkDeviceGroupPresentCapabilitiesKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceGroupPresentCapabilitiesKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceGroupPresentCapabilitiesKHX## ba)
          | I## n <- sizeOf (undefined :: VkDeviceGroupPresentCapabilitiesKHX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceGroupPresentCapabilitiesKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkDeviceGroupPresentCapabilitiesKHX),
            I## a <- alignment
                      (undefined :: VkDeviceGroupPresentCapabilitiesKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceGroupPresentCapabilitiesKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceGroupPresentCapabilitiesKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDeviceGroupPresentCapabilitiesKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceGroupPresentCapabilitiesKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceGroupPresentCapabilitiesKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceGroupPresentCapabilitiesKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkImageSwapchainCreateInfoKHX = VkImageSwapchainCreateInfoKHX## ByteArray##

instance Eq VkImageSwapchainCreateInfoKHX where
        (VkImageSwapchainCreateInfoKHX## a) ==
          (VkImageSwapchainCreateInfoKHX## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImageSwapchainCreateInfoKHX where
        (VkImageSwapchainCreateInfoKHX## a) `compare`
          (VkImageSwapchainCreateInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImageSwapchainCreateInfoKHX where
        sizeOf ~_ = #{size VkImageSwapchainCreateInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageSwapchainCreateInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImageSwapchainCreateInfoKHX),
            I## a <- alignment (undefined :: VkImageSwapchainCreateInfoKHX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImageSwapchainCreateInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImageSwapchainCreateInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkImageSwapchainCreateInfoKHX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImageSwapchainCreateInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImageSwapchainCreateInfoKHX),
            I## a <- alignment (undefined :: VkImageSwapchainCreateInfoKHX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImageSwapchainCreateInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImageSwapchainCreateInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImageSwapchainCreateInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImageSwapchainCreateInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImageSwapchainCreateInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImageSwapchainCreateInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkBindImageMemorySwapchainInfoKHX = VkBindImageMemorySwapchainInfoKHX## ByteArray##

instance Eq VkBindImageMemorySwapchainInfoKHX where
        (VkBindImageMemorySwapchainInfoKHX## a) ==
          (VkBindImageMemorySwapchainInfoKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemorySwapchainInfoKHX where
        (VkBindImageMemorySwapchainInfoKHX## a) `compare`
          (VkBindImageMemorySwapchainInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemorySwapchainInfoKHX where
        sizeOf ~_ = #{size VkBindImageMemorySwapchainInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindImageMemorySwapchainInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkBindImageMemorySwapchainInfoKHX),
            I## a <- alignment (undefined :: VkBindImageMemorySwapchainInfoKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkBindImageMemorySwapchainInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkBindImageMemorySwapchainInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkBindImageMemorySwapchainInfoKHX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkBindImageMemorySwapchainInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkBindImageMemorySwapchainInfoKHX),
            I## a <- alignment (undefined :: VkBindImageMemorySwapchainInfoKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkBindImageMemorySwapchainInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkBindImageMemorySwapchainInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkBindImageMemorySwapchainInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkBindImageMemorySwapchainInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkBindImageMemorySwapchainInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkBindImageMemorySwapchainInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkAcquireNextImageInfoKHX = VkAcquireNextImageInfoKHX## ByteArray##

instance Eq VkAcquireNextImageInfoKHX where
        (VkAcquireNextImageInfoKHX## a) == (VkAcquireNextImageInfoKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkAcquireNextImageInfoKHX where
        (VkAcquireNextImageInfoKHX## a) `compare`
          (VkAcquireNextImageInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkAcquireNextImageInfoKHX where
        sizeOf ~_ = #{size VkAcquireNextImageInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkAcquireNextImageInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkAcquireNextImageInfoKHX),
            I## a <- alignment (undefined :: VkAcquireNextImageInfoKHX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkAcquireNextImageInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkAcquireNextImageInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkAcquireNextImageInfoKHX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkAcquireNextImageInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkAcquireNextImageInfoKHX),
            I## a <- alignment (undefined :: VkAcquireNextImageInfoKHX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkAcquireNextImageInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkAcquireNextImageInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkAcquireNextImageInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkAcquireNextImageInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkAcquireNextImageInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkAcquireNextImageInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkDeviceGroupPresentInfoKHX = VkDeviceGroupPresentInfoKHX## ByteArray##

instance Eq VkDeviceGroupPresentInfoKHX where
        (VkDeviceGroupPresentInfoKHX## a) ==
          (VkDeviceGroupPresentInfoKHX## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentInfoKHX where
        (VkDeviceGroupPresentInfoKHX## a) `compare`
          (VkDeviceGroupPresentInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupPresentInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupPresentInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDeviceGroupPresentInfoKHX),
            I## a <- alignment (undefined :: VkDeviceGroupPresentInfoKHX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceGroupPresentInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceGroupPresentInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkDeviceGroupPresentInfoKHX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceGroupPresentInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDeviceGroupPresentInfoKHX),
            I## a <- alignment (undefined :: VkDeviceGroupPresentInfoKHX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceGroupPresentInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceGroupPresentInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDeviceGroupPresentInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceGroupPresentInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceGroupPresentInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceGroupPresentInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkDeviceGroupSwapchainCreateInfoKHX = VkDeviceGroupSwapchainCreateInfoKHX## ByteArray##

instance Eq VkDeviceGroupSwapchainCreateInfoKHX where
        (VkDeviceGroupSwapchainCreateInfoKHX## a) ==
          (VkDeviceGroupSwapchainCreateInfoKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupSwapchainCreateInfoKHX where
        (VkDeviceGroupSwapchainCreateInfoKHX## a) `compare`
          (VkDeviceGroupSwapchainCreateInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupSwapchainCreateInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupSwapchainCreateInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupSwapchainCreateInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkDeviceGroupSwapchainCreateInfoKHX),
            I## a <- alignment
                      (undefined :: VkDeviceGroupSwapchainCreateInfoKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceGroupSwapchainCreateInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceGroupSwapchainCreateInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkDeviceGroupSwapchainCreateInfoKHX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceGroupSwapchainCreateInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkDeviceGroupSwapchainCreateInfoKHX),
            I## a <- alignment
                      (undefined :: VkDeviceGroupSwapchainCreateInfoKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceGroupSwapchainCreateInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceGroupSwapchainCreateInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDeviceGroupSwapchainCreateInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceGroupSwapchainCreateInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceGroupSwapchainCreateInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceGroupSwapchainCreateInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
