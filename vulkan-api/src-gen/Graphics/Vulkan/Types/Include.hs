{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Strict         #-}
module Graphics.Vulkan.Types.Include
       (-- > #include "vk_platform.h"
        DWORD, Display, GgpFrameToken, GgpStreamDescriptor, HANDLE,
        HINSTANCE, HMONITOR, HWND, IDirectFB, IDirectFBSurface, LPCWSTR,
        RROutput, SECURITY_ATTRIBUTES, VisualID, Window, WlDisplay,
        WlSurface, XcbConnectionT, XcbVisualidT, XcbWindowT, Zx_handle_t)
       where
import Graphics.Vulkan.Marshal (CULong (..), CWchar (..), Ptr, Word32)

-- | Requires @windows.h@
type DWORD = Word32

-- | Requires @X11/Xlib.h@
data Display

-- | Requires @ggp_c/vulkan_types.h@
data GgpFrameToken

-- | Requires @ggp_c/vulkan_types.h@
data GgpStreamDescriptor

-- | Requires @windows.h@
type HANDLE = Ptr ()

-- | Requires @windows.h@
type HINSTANCE = Ptr ()

-- | Requires @windows.h@
data HMONITOR

-- | Requires @windows.h@
type HWND = Ptr ()

-- | Requires @directfb.h@
data IDirectFB

-- | Requires @directfb.h@
data IDirectFBSurface

-- | Requires @windows.h@
type LPCWSTR = Ptr CWchar

-- | Requires @X11/extensions/Xrandr.h@
type RROutput = CULong

-- | Requires @windows.h@
data SECURITY_ATTRIBUTES

-- | Requires @X11/Xlib.h@
type VisualID = CULong

-- | Requires @X11/Xlib.h@
type Window = CULong

-- | Requires @wayland-client.h@
data WlDisplay

-- | Requires @wayland-client.h@
data WlSurface

-- | Requires @xcb/xcb.h@
data XcbConnectionT

-- | Requires @xcb/xcb.h@
type XcbVisualidT = CULong

-- | Requires @xcb/xcb.h@
type XcbWindowT = CULong

-- | Requires @zircon/types.h@
data Zx_handle_t
