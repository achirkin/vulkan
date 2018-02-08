#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_MVK_macos_surface
       (-- * Vulkan extension: @VK_MVK_macos_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Bill Hollings @billhollings@
        --
        -- author: @MVK@
        --
        -- type: @instance@
        --
        -- Extension number: @124@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_MACOS_MVK@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkMacOSSurfaceCreateInfoMVK(..), vkCreateMacOSSurfaceMVK,
        VK_MVK_MACOS_SURFACE_SPEC_VERSION,
        pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION,
        VK_MVK_MACOS_SURFACE_EXTENSION_NAME,
        pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK)
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
import           Graphics.Vulkan.Common           (VkInstance,
                                                   VkMacOSSurfaceCreateFlagsMVK,
                                                   VkResult (..),
                                                   VkStructureType,
                                                   VkStructureType (..),
                                                   VkSurfaceKHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkMacOSSurfaceCreateInfoMVK = VkMacOSSurfaceCreateInfoMVK## ByteArray##

instance Eq VkMacOSSurfaceCreateInfoMVK where
        (VkMacOSSurfaceCreateInfoMVK## a) ==
          (VkMacOSSurfaceCreateInfoMVK## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkMacOSSurfaceCreateInfoMVK where
        (VkMacOSSurfaceCreateInfoMVK## a) `compare`
          (VkMacOSSurfaceCreateInfoMVK## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkMacOSSurfaceCreateInfoMVK where
        sizeOf ~_ = #{size VkMacOSSurfaceCreateInfoMVK}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMacOSSurfaceCreateInfoMVK}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkMacOSSurfaceCreateInfoMVK),
            I## a <- alignment (undefined :: VkMacOSSurfaceCreateInfoMVK) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkMacOSSurfaceCreateInfoMVK##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkMacOSSurfaceCreateInfoMVK## ba)
          | I## n <- sizeOf (undefined :: VkMacOSSurfaceCreateInfoMVK) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkMacOSSurfaceCreateInfoMVK),
            I## a <- alignment (undefined :: VkMacOSSurfaceCreateInfoMVK) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkMacOSSurfaceCreateInfoMVK##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkMacOSSurfaceCreateInfoMVK## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkMacOSSurfaceCreateInfoMVK##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkMacOSSurfaceCreateInfoMVK## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkMacOSSurfaceCreateInfoMVK## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkMacOSSurfaceCreateInfoMVK## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkMacOSSurfaceCreateInfoMVK
         where
        type VkSTypeMType VkMacOSSurfaceCreateInfoMVK = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMacOSSurfaceCreateInfoMVK, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkMacOSSurfaceCreateInfoMVK
         where
        type VkPNextMType VkMacOSSurfaceCreateInfoMVK = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMacOSSurfaceCreateInfoMVK, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pNext}

instance {-# OVERLAPPING #-} HasVkFlags VkMacOSSurfaceCreateInfoMVK
         where
        type VkFlagsMType VkMacOSSurfaceCreateInfoMVK =
             VkMacOSSurfaceCreateFlagsMVK

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkMacOSSurfaceCreateInfoMVK, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, flags}

instance {-# OVERLAPPING #-} HasVkPView VkMacOSSurfaceCreateInfoMVK
         where
        type VkPViewMType VkMacOSSurfaceCreateInfoMVK = Ptr Void

        {-# NOINLINE vkPView #-}
        vkPView x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, pView})

        {-# INLINE vkPViewByteOffset #-}
        vkPViewByteOffset ~_
          = #{offset VkMacOSSurfaceCreateInfoMVK, pView}

        {-# INLINE readVkPView #-}
        readVkPView p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pView}

        {-# INLINE writeVkPView #-}
        writeVkPView p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pView}

instance Show VkMacOSSurfaceCreateInfoMVK where
        showsPrec d x
          = showString "VkMacOSSurfaceCreateInfoMVK {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkPView = " . showsPrec d (vkPView x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateMacOSSurfaceMVK
--   >     ( VkInstance instance
--   >     , const VkMacOSSurfaceCreateInfoMVK* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateMacOSSurfaceMVK.html vkCreateMacOSSurfaceMVK registry at www.khronos.org>
foreign import ccall unsafe "vkCreateMacOSSurfaceMVK"
               vkCreateMacOSSurfaceMVK ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkMacOSSurfaceCreateInfoMVK -- ^ pCreateInfo
                                                 ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION = 2

type VK_MVK_MACOS_SURFACE_SPEC_VERSION = 2

pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString

pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME <-
        (is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME -> True)
  where VK_MVK_MACOS_SURFACE_EXTENSION_NAME
          = _VK_MVK_MACOS_SURFACE_EXTENSION_NAME

_VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString

{-# INLINE _VK_MVK_MACOS_SURFACE_EXTENSION_NAME #-}
_VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  = Ptr "VK_MVK_macos_surface\NUL"##

is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME #-}
is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  = (_VK_MVK_MACOS_SURFACE_EXTENSION_NAME ==)

type VK_MVK_MACOS_SURFACE_EXTENSION_NAME = "VK_MVK_macos_surface"

pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK =
        VkStructureType 1000123000
