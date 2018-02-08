#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_xlib_surface
       (-- * Vulkan extension: @VK_KHR_xlib_surface@
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
        -- Extension number: @5@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_XLIB_KHR@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkXlibSurfaceCreateInfoKHR(..), vkCreateXlibSurfaceKHR,
        vkGetPhysicalDeviceXlibPresentationSupportKHR,
        VK_KHR_XLIB_SURFACE_SPEC_VERSION,
        pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION,
        VK_KHR_XLIB_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR)
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
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkXlibSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkXlibSurfaceCreateFlagsKHR   flags;
--   >     Display*                         dpy;
--   >     Window                           window;
--   > } VkXlibSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkXlibSurfaceCreateInfoKHR.html VkXlibSurfaceCreateInfoKHR registry at www.khronos.org>
data VkXlibSurfaceCreateInfoKHR = VkXlibSurfaceCreateInfoKHR## ByteArray##

instance Eq VkXlibSurfaceCreateInfoKHR where
        (VkXlibSurfaceCreateInfoKHR## a) == (VkXlibSurfaceCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkXlibSurfaceCreateInfoKHR where
        (VkXlibSurfaceCreateInfoKHR## a) `compare`
          (VkXlibSurfaceCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkXlibSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkXlibSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkXlibSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkXlibSurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkXlibSurfaceCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkXlibSurfaceCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkXlibSurfaceCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkXlibSurfaceCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkXlibSurfaceCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkXlibSurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkXlibSurfaceCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkXlibSurfaceCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkXlibSurfaceCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkXlibSurfaceCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkXlibSurfaceCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkXlibSurfaceCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkXlibSurfaceCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkXlibSurfaceCreateInfoKHR
         where
        type VkSTypeMType VkXlibSurfaceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXlibSurfaceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkXlibSurfaceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkXlibSurfaceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkXlibSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkXlibSurfaceCreateInfoKHR where
        type FieldType "sType" VkXlibSurfaceCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkXlibSurfaceCreateInfoKHR
         where
        type VkPNextMType VkXlibSurfaceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXlibSurfaceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkXlibSurfaceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkXlibSurfaceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkXlibSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkXlibSurfaceCreateInfoKHR where
        type FieldType "pNext" VkXlibSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkXlibSurfaceCreateInfoKHR
         where
        type VkFlagsMType VkXlibSurfaceCreateInfoKHR =
             VkXlibSurfaceCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXlibSurfaceCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkXlibSurfaceCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkXlibSurfaceCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkXlibSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkXlibSurfaceCreateInfoKHR where
        type FieldType "flags" VkXlibSurfaceCreateInfoKHR =
             VkXlibSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkXlibSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs

instance CanReadField "flags" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkDpy VkXlibSurfaceCreateInfoKHR
         where
        type VkDpyMType VkXlibSurfaceCreateInfoKHR = Ptr Display

        {-# NOINLINE vkDpy #-}
        vkDpy x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXlibSurfaceCreateInfoKHR, dpy})

        {-# INLINE vkDpyByteOffset #-}
        vkDpyByteOffset ~_
          = #{offset VkXlibSurfaceCreateInfoKHR, dpy}

        {-# INLINE readVkDpy #-}
        readVkDpy p
          = peekByteOff p #{offset VkXlibSurfaceCreateInfoKHR, dpy}

        {-# INLINE writeVkDpy #-}
        writeVkDpy p
          = pokeByteOff p #{offset VkXlibSurfaceCreateInfoKHR, dpy}

instance {-# OVERLAPPING #-}
         HasField "dpy" VkXlibSurfaceCreateInfoKHR where
        type FieldType "dpy" VkXlibSurfaceCreateInfoKHR = Ptr Display
        type FieldOptional "dpy" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "dpy" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkDpy

        {-# INLINE readField #-}
        readField = readVkDpy

instance CanWriteField "dpy" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkDpy

instance {-# OVERLAPPING #-} HasVkWindow VkXlibSurfaceCreateInfoKHR
         where
        type VkWindowMType VkXlibSurfaceCreateInfoKHR = Window

        {-# NOINLINE vkWindow #-}
        vkWindow x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXlibSurfaceCreateInfoKHR, window})

        {-# INLINE vkWindowByteOffset #-}
        vkWindowByteOffset ~_
          = #{offset VkXlibSurfaceCreateInfoKHR, window}

        {-# INLINE readVkWindow #-}
        readVkWindow p
          = peekByteOff p #{offset VkXlibSurfaceCreateInfoKHR, window}

        {-# INLINE writeVkWindow #-}
        writeVkWindow p
          = pokeByteOff p #{offset VkXlibSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         HasField "window" VkXlibSurfaceCreateInfoKHR where
        type FieldType "window" VkXlibSurfaceCreateInfoKHR = Window
        type FieldOptional "window" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "window" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkWindow

        {-# INLINE readField #-}
        readField = readVkWindow

instance CanWriteField "window" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkWindow

instance Show VkXlibSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkXlibSurfaceCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDpy = " .
                                  showsPrec d (vkDpy x) .
                                    showString ", " .
                                      showString "vkWindow = " .
                                        showsPrec d (vkWindow x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateXlibSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkXlibSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateXlibSurfaceKHR.html vkCreateXlibSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateXlibSurfaceKHR"
               vkCreateXlibSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , Display* dpy
--   >     , VisualID visualID
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceXlibPresentationSupportKHR.html vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceXlibPresentationSupportKHR"
               vkGetPhysicalDeviceXlibPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr Display -- ^ dpy
                                       -> VisualID -- ^ visualID
                                                   -> IO VkBool32

pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION = 6

type VK_KHR_XLIB_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_XLIB_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_XLIB_SURFACE_EXTENSION_NAME
          = _VK_KHR_XLIB_SURFACE_EXTENSION_NAME

_VK_KHR_XLIB_SURFACE_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_XLIB_SURFACE_EXTENSION_NAME #-}
_VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_xlib_surface\NUL"##

is_VK_KHR_XLIB_SURFACE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_XLIB_SURFACE_EXTENSION_NAME #-}
is_VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  = (_VK_KHR_XLIB_SURFACE_EXTENSION_NAME ==)

type VK_KHR_XLIB_SURFACE_EXTENSION_NAME = "VK_KHR_xlib_surface"

pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000004000
