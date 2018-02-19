#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
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
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkWaylandSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkWaylandSurfaceCreateFlagsKHR   flags;
--   >     struct wl_display*               display;
--   >     struct wl_surface*               surface;
--   > } VkWaylandSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkWaylandSurfaceCreateInfoKHR.html VkWaylandSurfaceCreateInfoKHR registry at www.khronos.org>
data VkWaylandSurfaceCreateInfoKHR = VkWaylandSurfaceCreateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkWaylandSurfaceCreateInfoKHR where
        (VkWaylandSurfaceCreateInfoKHR## a _) ==
          x@(VkWaylandSurfaceCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkWaylandSurfaceCreateInfoKHR where
        (VkWaylandSurfaceCreateInfoKHR## a _) `compare`
          x@(VkWaylandSurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkWaylandSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkWaylandSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkWaylandSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkWaylandSurfaceCreateInfoKHR where
        unsafeAddr (VkWaylandSurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkWaylandSurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkWaylandSurfaceCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkWaylandSurfaceCreateInfoKHR where
        type StructFields VkWaylandSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "display", "surface"] -- ' closing tick for hsc2hs
        type CUnionType VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkWaylandSurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

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
         HasField "sType" VkWaylandSurfaceCreateInfoKHR where
        type FieldType "sType" VkWaylandSurfaceCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWaylandSurfaceCreateInfoKHR =
             #{offset VkWaylandSurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWaylandSurfaceCreateInfoKHR, sType}

instance CanReadField "sType" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkWaylandSurfaceCreateInfoKHR where
        type FieldType "pNext" VkWaylandSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWaylandSurfaceCreateInfoKHR =
             #{offset VkWaylandSurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWaylandSurfaceCreateInfoKHR, pNext}

instance CanReadField "pNext" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
         HasField "flags" VkWaylandSurfaceCreateInfoKHR where
        type FieldType "flags" VkWaylandSurfaceCreateInfoKHR =
             VkWaylandSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkWaylandSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkWaylandSurfaceCreateInfoKHR =
             #{offset VkWaylandSurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWaylandSurfaceCreateInfoKHR, flags}

instance CanReadField "flags" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

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
         HasField "display" VkWaylandSurfaceCreateInfoKHR where
        type FieldType "display" VkWaylandSurfaceCreateInfoKHR =
             Ptr WlDisplay
        type FieldOptional "display" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "display" VkWaylandSurfaceCreateInfoKHR =
             #{offset VkWaylandSurfaceCreateInfoKHR, display}
        type FieldIsArray "display" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWaylandSurfaceCreateInfoKHR, display}

instance CanReadField "display" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkDisplay

        {-# INLINE readField #-}
        readField = readVkDisplay

instance CanWriteField "display" VkWaylandSurfaceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDisplay

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

instance {-# OVERLAPPING #-}
         HasField "surface" VkWaylandSurfaceCreateInfoKHR where
        type FieldType "surface" VkWaylandSurfaceCreateInfoKHR =
             Ptr WlSurface
        type FieldOptional "surface" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "surface" VkWaylandSurfaceCreateInfoKHR =
             #{offset VkWaylandSurfaceCreateInfoKHR, surface}
        type FieldIsArray "surface" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWaylandSurfaceCreateInfoKHR, surface}

instance CanReadField "surface" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSurface

        {-# INLINE readField #-}
        readField = readVkSurface

instance CanWriteField "surface" VkWaylandSurfaceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSurface

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateWaylandSurfaceKHR.html vkCreateWaylandSurfaceKHR registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceWaylandPresentationSupportKHR.html vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
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

{-# INLINE _VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME #-}

_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_wayland_surface\NUL"##

{-# INLINE is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  = eqCStrings _VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME

type VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME =
     "VK_KHR_wayland_surface"

pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000006000
