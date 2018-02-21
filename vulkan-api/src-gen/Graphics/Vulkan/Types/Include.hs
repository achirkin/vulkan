{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Strict         #-}
module Graphics.Vulkan.Types.Include
       (-- > #include "vk_platform.h"
        --
        -- > #include <X11/Xlib.h>
        --
        -- > #include <X11/extensions/Xrandr.h>
        --
        -- > #include <android/native_window.h>
        --
        -- > #include <mir_toolkit/client_types.h>
        --
        -- > #include "vulkan.h"
        --
        -- > #include <wayland-client.h>
        --
        -- > #include <windows.h>
        --
        -- > #include <xcb/xcb.h>
        ANativeWindow, DWORD, Display, HANDLE, HINSTANCE, HWND, LPCWSTR,
        MirConnection, MirSurface, RROutput, SECURITY_ATTRIBUTES, VisualID,
        Window, WlDisplay, WlSurface, XcbConnectionT, XcbVisualidT,
        XcbWindowT)
       where
import           Graphics.Vulkan.Marshal (CULong (..), CWchar (..), Ptr, Word32)

-- | Requires @android/native_window.h@
data ANativeWindow

-- | Requires @windows.h@
type DWORD = Word32

-- | Requires @X11/Xlib.h@
data Display

-- | Requires @windows.h@
type HANDLE = Ptr ()

-- | Requires @windows.h@
type HINSTANCE = Ptr ()

-- | Requires @windows.h@
type HWND = Ptr ()

-- | Requires @windows.h@
type LPCWSTR = Ptr CWchar

-- | Requires @mir_toolkit/client_types.h@
data MirConnection

-- | Requires @mir_toolkit/client_types.h@
data MirSurface

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
