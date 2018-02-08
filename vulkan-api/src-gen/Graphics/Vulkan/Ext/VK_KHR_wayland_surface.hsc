#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_wayland_surface
       (-- * Vulkan extension: @VK_KHR_wayland_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall,Ian Elliott ianelliott@google.com@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @7@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WAYLAND_KHR@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkWaylandSurfaceCreateInfoKHR(..), vkCreateWaylandSurfaceKHR,
        vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        VK_KHR_WAYLAND_SURFACE_SPEC_VERSION,
        pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION,
        VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..))
import           Graphics.Vulkan.Common           (VkBool32 (..), VkInstance,
                                                   VkPhysicalDevice,
                                                   VkResult (..),
                                                   VkStructureType,
                                                   VkStructureType (..),
                                                   VkSurfaceKHR,
                                                   VkWaylandSurfaceCreateFlagsKHR,
                                                   WlDisplay, Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkWaylandSurfaceCreateInfoKHR = VkWaylandSurfaceCreateInfoKHR## ByteArray##

instance Eq VkWaylandSurfaceCreateInfoKHR where
        (VkWaylandSurfaceCreateInfoKHR## a) ==
          (VkWaylandSurfaceCreateInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkWaylandSurfaceCreateInfoKHR where
        (VkWaylandSurfaceCreateInfoKHR## a) `compare`
          (VkWaylandSurfaceCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkWaylandSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkWaylandSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkWaylandSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkWaylandSurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkWaylandSurfaceCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkWaylandSurfaceCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkWaylandSurfaceCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkWaylandSurfaceCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkWaylandSurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkWaylandSurfaceCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkWaylandSurfaceCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkWaylandSurfaceCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkWaylandSurfaceCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkWaylandSurfaceCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkWaylandSurfaceCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkWaylandSurfaceCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkWaylandSurfaceCreateInfoKHR where
        type VkSTypeMType VkWaylandSurfaceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWaylandSurfaceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkWaylandSurfaceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkWaylandSurfaceCreateInfoKHR where
        type VkPNextMType VkWaylandSurfaceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWaylandSurfaceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkWaylandSurfaceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkFlags VkWaylandSurfaceCreateInfoKHR where
        type VkFlagsMType VkWaylandSurfaceCreateInfoKHR =
             VkWaylandSurfaceCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWaylandSurfaceCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkWaylandSurfaceCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasVkDisplay VkWaylandSurfaceCreateInfoKHR where
        type VkDisplayMType VkWaylandSurfaceCreateInfoKHR = Ptr WlDisplay

        {-# NOINLINE vkDisplay #-}
        vkDisplay x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWaylandSurfaceCreateInfoKHR, display})

        {-# INLINE vkDisplayByteOffset #-}
        vkDisplayByteOffset ~_
          = #{offset VkWaylandSurfaceCreateInfoKHR, display}

        {-# INLINE readVkDisplay #-}
        readVkDisplay p
          = peekByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, display}

        {-# INLINE writeVkDisplay #-}
        writeVkDisplay p
          = pokeByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, display}

instance {-# OVERLAPPING #-}
         HasVkSurface VkWaylandSurfaceCreateInfoKHR where
        type VkSurfaceMType VkWaylandSurfaceCreateInfoKHR = Ptr WlSurface

        {-# NOINLINE vkSurface #-}
        vkSurface x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWaylandSurfaceCreateInfoKHR, surface})

        {-# INLINE vkSurfaceByteOffset #-}
        vkSurfaceByteOffset ~_
          = #{offset VkWaylandSurfaceCreateInfoKHR, surface}

        {-# INLINE readVkSurface #-}
        readVkSurface p
          = peekByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, surface}

        {-# INLINE writeVkSurface #-}
        writeVkSurface p
          = pokeByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, surface}

instance Show VkWaylandSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkWaylandSurfaceCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDisplay = " .
                                  showsPrec d (vkDisplay x) .
                                    showString ", " .
                                      showString "vkSurface = " .
                                        showsPrec d (vkSurface x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateWaylandSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkWaylandSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateWaylandSurfaceKHR.html vkCreateWaylandSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateWaylandSurfaceKHR"
               vkCreateWaylandSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , struct wl_display* display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceWaylandPresentationSupportKHR.html vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
               vkGetPhysicalDeviceWaylandPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr WlDisplay -- ^ display
                                                           -> IO VkBool32

pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION = 6

type VK_KHR_WAYLAND_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
          = _VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME

_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME #-}
_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_wayland_surface\NUL"##

is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME #-}
is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  = (_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME ==)

type VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME =
     "VK_KHR_wayland_surface"

pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000006000
