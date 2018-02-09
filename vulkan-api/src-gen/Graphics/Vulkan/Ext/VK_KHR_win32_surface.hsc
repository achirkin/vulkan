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
module Graphics.Vulkan.Ext.VK_KHR_win32_surface
       (-- * Vulkan extension: @VK_KHR_win32_surface@
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
        -- Extension number: @10@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkWin32SurfaceCreateInfoKHR(..), vkCreateWin32SurfaceKHR,
        vkGetPhysicalDeviceWin32PresentationSupportKHR,
        VK_KHR_WIN32_SURFACE_SPEC_VERSION,
        pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION,
        VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR)
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

-- | > typedef struct VkWin32SurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkWin32SurfaceCreateFlagsKHR   flags;
--   >     HINSTANCE                        hinstance;
--   >     HWND                             hwnd;
--   > } VkWin32SurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkWin32SurfaceCreateInfoKHR.html VkWin32SurfaceCreateInfoKHR registry at www.khronos.org>
data VkWin32SurfaceCreateInfoKHR = VkWin32SurfaceCreateInfoKHR## ByteArray##

instance Eq VkWin32SurfaceCreateInfoKHR where
        (VkWin32SurfaceCreateInfoKHR## a) ==
          (VkWin32SurfaceCreateInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkWin32SurfaceCreateInfoKHR where
        (VkWin32SurfaceCreateInfoKHR## a) `compare`
          (VkWin32SurfaceCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkWin32SurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkWin32SurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkWin32SurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkWin32SurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkWin32SurfaceCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkWin32SurfaceCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkWin32SurfaceCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkWin32SurfaceCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkWin32SurfaceCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkWin32SurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkWin32SurfaceCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkWin32SurfaceCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkWin32SurfaceCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkWin32SurfaceCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkWin32SurfaceCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkWin32SurfaceCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkWin32SurfaceCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkWin32SurfaceCreateInfoKHR
         where
        type VkSTypeMType VkWin32SurfaceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkWin32SurfaceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkWin32SurfaceCreateInfoKHR where
        type FieldType "sType" VkWin32SurfaceCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, sType}

instance CanReadField "sType" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkWin32SurfaceCreateInfoKHR
         where
        type VkPNextMType VkWin32SurfaceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkWin32SurfaceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkWin32SurfaceCreateInfoKHR where
        type FieldType "pNext" VkWin32SurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, pNext}

instance CanReadField "pNext" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkWin32SurfaceCreateInfoKHR
         where
        type VkFlagsMType VkWin32SurfaceCreateInfoKHR =
             VkWin32SurfaceCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkWin32SurfaceCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkWin32SurfaceCreateInfoKHR where
        type FieldType "flags" VkWin32SurfaceCreateInfoKHR =
             VkWin32SurfaceCreateFlagsKHR
        type FieldOptional "flags" VkWin32SurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, flags}

instance CanReadField "flags" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkHinstance VkWin32SurfaceCreateInfoKHR where
        type VkHinstanceMType VkWin32SurfaceCreateInfoKHR = HINSTANCE

        {-# NOINLINE vkHinstance #-}
        vkHinstance x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, hinstance})

        {-# INLINE vkHinstanceByteOffset #-}
        vkHinstanceByteOffset ~_
          = #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

        {-# INLINE readVkHinstance #-}
        readVkHinstance p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

        {-# INLINE writeVkHinstance #-}
        writeVkHinstance p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

instance {-# OVERLAPPING #-}
         HasField "hinstance" VkWin32SurfaceCreateInfoKHR where
        type FieldType "hinstance" VkWin32SurfaceCreateInfoKHR = HINSTANCE
        type FieldOptional "hinstance" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "hinstance" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

instance CanReadField "hinstance" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkHinstance

        {-# INLINE readField #-}
        readField = readVkHinstance

instance CanWriteField "hinstance" VkWin32SurfaceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHinstance

instance {-# OVERLAPPING #-} HasVkHwnd VkWin32SurfaceCreateInfoKHR
         where
        type VkHwndMType VkWin32SurfaceCreateInfoKHR = HWND

        {-# NOINLINE vkHwnd #-}
        vkHwnd x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, hwnd})

        {-# INLINE vkHwndByteOffset #-}
        vkHwndByteOffset ~_
          = #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

        {-# INLINE readVkHwnd #-}
        readVkHwnd p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

        {-# INLINE writeVkHwnd #-}
        writeVkHwnd p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

instance {-# OVERLAPPING #-}
         HasField "hwnd" VkWin32SurfaceCreateInfoKHR where
        type FieldType "hwnd" VkWin32SurfaceCreateInfoKHR = HWND
        type FieldOptional "hwnd" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "hwnd" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

instance CanReadField "hwnd" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkHwnd

        {-# INLINE readField #-}
        readField = readVkHwnd

instance CanWriteField "hwnd" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkHwnd

instance Show VkWin32SurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkWin32SurfaceCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkHinstance = " .
                                  showsPrec d (vkHinstance x) .
                                    showString ", " .
                                      showString "vkHwnd = " . showsPrec d (vkHwnd x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateWin32SurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkWin32SurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateWin32SurfaceKHR.html vkCreateWin32SurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateWin32SurfaceKHR"
               vkCreateWin32SurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                                 ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceWin32PresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceWin32PresentationSupportKHR.html vkGetPhysicalDeviceWin32PresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceWin32PresentationSupportKHR"
               vkGetPhysicalDeviceWin32PresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> IO VkBool32

pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION = 6

type VK_KHR_WIN32_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_WIN32_SURFACE_EXTENSION_NAME
          = _VK_KHR_WIN32_SURFACE_EXTENSION_NAME

_VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_WIN32_SURFACE_EXTENSION_NAME #-}
_VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_win32_surface\NUL"##

is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME #-}
is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  = (_VK_KHR_WIN32_SURFACE_EXTENSION_NAME ==)

type VK_KHR_WIN32_SURFACE_EXTENSION_NAME = "VK_KHR_win32_surface"

pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000009000
