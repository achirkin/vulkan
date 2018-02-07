#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_xcb_surface
       (-- * Vulkan extension: @VK_KHR_xcb_surface@
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
        -- Extension number: @6@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_XCB_KHR@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkXcbSurfaceCreateInfoKHR(..), vkCreateXcbSurfaceKHR,
        vkGetPhysicalDeviceXcbPresentationSupportKHR,
        VK_KHR_XCB_SURFACE_SPEC_VERSION,
        pattern VK_KHR_XCB_SURFACE_SPEC_VERSION,
        VK_KHR_XCB_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks)
import           Graphics.Vulkan.Common           (VkBool32, VkInstance,
                                                   VkPhysicalDevice, VkResult,
                                                   VkStructureType,
                                                   VkStructureType (..),
                                                   VkSurfaceKHR,
                                                   VkXcbSurfaceCreateFlagsKHR,
                                                   Word32, XcbConnectionT,
                                                   XcbVisualidT, XcbWindowT)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkXcbSurfaceCreateInfoKHR = VkXcbSurfaceCreateInfoKHR## ByteArray##

instance Eq VkXcbSurfaceCreateInfoKHR where
        (VkXcbSurfaceCreateInfoKHR## a) == (VkXcbSurfaceCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkXcbSurfaceCreateInfoKHR where
        (VkXcbSurfaceCreateInfoKHR## a) `compare`
          (VkXcbSurfaceCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkXcbSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkXcbSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkXcbSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkXcbSurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkXcbSurfaceCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkXcbSurfaceCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkXcbSurfaceCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkXcbSurfaceCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkXcbSurfaceCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkXcbSurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkXcbSurfaceCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkXcbSurfaceCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkXcbSurfaceCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkXcbSurfaceCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkXcbSurfaceCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkXcbSurfaceCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkXcbSurfaceCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkXcbSurfaceCreateInfoKHR
         where
        type VkSTypeMType VkXcbSurfaceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkXcbSurfaceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkXcbSurfaceCreateInfoKHR
         where
        type VkPNextMType VkXcbSurfaceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkXcbSurfaceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasVkFlags VkXcbSurfaceCreateInfoKHR
         where
        type VkFlagsMType VkXcbSurfaceCreateInfoKHR =
             VkXcbSurfaceCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkXcbSurfaceCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasVkConnection VkXcbSurfaceCreateInfoKHR where
        type VkConnectionMType VkXcbSurfaceCreateInfoKHR =
             Ptr XcbConnectionT

        {-# NOINLINE vkConnection #-}
        vkConnection x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, connection})

        {-# INLINE vkConnectionByteOffset #-}
        vkConnectionByteOffset ~_
          = #{offset VkXcbSurfaceCreateInfoKHR, connection}

        {-# INLINE readVkConnection #-}
        readVkConnection p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, connection}

        {-# INLINE writeVkConnection #-}
        writeVkConnection p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, connection}

instance {-# OVERLAPPING #-} HasVkWindow VkXcbSurfaceCreateInfoKHR
         where
        type VkWindowMType VkXcbSurfaceCreateInfoKHR = XcbWindowT

        {-# NOINLINE vkWindow #-}
        vkWindow x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, window})

        {-# INLINE vkWindowByteOffset #-}
        vkWindowByteOffset ~_
          = #{offset VkXcbSurfaceCreateInfoKHR, window}

        {-# INLINE readVkWindow #-}
        readVkWindow p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, window}

        {-# INLINE writeVkWindow #-}
        writeVkWindow p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, window}

instance Show VkXcbSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkXcbSurfaceCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkConnection = " .
                                  showsPrec d (vkConnection x) .
                                    showString ", " .
                                      showString "vkWindow = " .
                                        showsPrec d (vkWindow x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateXcbSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkXcbSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateXcbSurfaceKHR.html vkCreateXcbSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateXcbSurfaceKHR"
               vkCreateXcbSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXcbSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceXcbPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , xcb_connection_t* connection
--   >     , xcb_visualid_t visual_id
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceXcbPresentationSupportKHR.html vkGetPhysicalDeviceXcbPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceXcbPresentationSupportKHR"
               vkGetPhysicalDeviceXcbPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr XcbConnectionT -- ^ connection
                                              -> XcbVisualidT -- ^ visual_id
                                                              -> IO VkBool32

pattern VK_KHR_XCB_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_XCB_SURFACE_SPEC_VERSION = 6

type VK_KHR_XCB_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_XCB_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_XCB_SURFACE_EXTENSION_NAME
          = _VK_KHR_XCB_SURFACE_EXTENSION_NAME

_VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_XCB_SURFACE_EXTENSION_NAME #-}
_VK_KHR_XCB_SURFACE_EXTENSION_NAME = Ptr "VK_KHR_xcb_surface\NUL"##

is_VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_XCB_SURFACE_EXTENSION_NAME #-}
is_VK_KHR_XCB_SURFACE_EXTENSION_NAME
  = (_VK_KHR_XCB_SURFACE_EXTENSION_NAME ==)

type VK_KHR_XCB_SURFACE_EXTENSION_NAME = "VK_KHR_xcb_surface"

pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000005000
