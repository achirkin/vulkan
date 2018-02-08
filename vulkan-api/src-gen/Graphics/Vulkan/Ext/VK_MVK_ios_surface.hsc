#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_MVK_ios_surface
       (-- * Vulkan extension: @VK_MVK_ios_surface@
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
        -- Extension number: @123@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_IOS_MVK@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkIOSSurfaceCreateInfoMVK(..), vkCreateIOSSurfaceMVK,
        VK_MVK_IOS_SURFACE_SPEC_VERSION,
        pattern VK_MVK_IOS_SURFACE_SPEC_VERSION,
        VK_MVK_IOS_SURFACE_EXTENSION_NAME,
        pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK)
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
import           Graphics.Vulkan.Common           (VkIOSSurfaceCreateFlagsMVK,
                                                   VkInstance, VkResult (..),
                                                   VkStructureType,
                                                   VkStructureType (..),
                                                   VkSurfaceKHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkIOSSurfaceCreateInfoMVK = VkIOSSurfaceCreateInfoMVK## ByteArray##

instance Eq VkIOSSurfaceCreateInfoMVK where
        (VkIOSSurfaceCreateInfoMVK## a) == (VkIOSSurfaceCreateInfoMVK## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkIOSSurfaceCreateInfoMVK where
        (VkIOSSurfaceCreateInfoMVK## a) `compare`
          (VkIOSSurfaceCreateInfoMVK## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkIOSSurfaceCreateInfoMVK where
        sizeOf ~_ = #{size VkIOSSurfaceCreateInfoMVK}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkIOSSurfaceCreateInfoMVK}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkIOSSurfaceCreateInfoMVK),
            I## a <- alignment (undefined :: VkIOSSurfaceCreateInfoMVK) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkIOSSurfaceCreateInfoMVK##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkIOSSurfaceCreateInfoMVK## ba)
          | I## n <- sizeOf (undefined :: VkIOSSurfaceCreateInfoMVK) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkIOSSurfaceCreateInfoMVK where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkIOSSurfaceCreateInfoMVK),
            I## a <- alignment (undefined :: VkIOSSurfaceCreateInfoMVK) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkIOSSurfaceCreateInfoMVK##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkIOSSurfaceCreateInfoMVK## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkIOSSurfaceCreateInfoMVK##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkIOSSurfaceCreateInfoMVK## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkIOSSurfaceCreateInfoMVK## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkIOSSurfaceCreateInfoMVK## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkIOSSurfaceCreateInfoMVK
         where
        type VkSTypeMType VkIOSSurfaceCreateInfoMVK = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkIOSSurfaceCreateInfoMVK, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkIOSSurfaceCreateInfoMVK
         where
        type VkPNextMType VkIOSSurfaceCreateInfoMVK = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkIOSSurfaceCreateInfoMVK, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pNext}

instance {-# OVERLAPPING #-} HasVkFlags VkIOSSurfaceCreateInfoMVK
         where
        type VkFlagsMType VkIOSSurfaceCreateInfoMVK =
             VkIOSSurfaceCreateFlagsMVK

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkIOSSurfaceCreateInfoMVK, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, flags}

instance {-# OVERLAPPING #-} HasVkPView VkIOSSurfaceCreateInfoMVK
         where
        type VkPViewMType VkIOSSurfaceCreateInfoMVK = Ptr Void

        {-# NOINLINE vkPView #-}
        vkPView x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, pView})

        {-# INLINE vkPViewByteOffset #-}
        vkPViewByteOffset ~_
          = #{offset VkIOSSurfaceCreateInfoMVK, pView}

        {-# INLINE readVkPView #-}
        readVkPView p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pView}

        {-# INLINE writeVkPView #-}
        writeVkPView p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pView}

instance Show VkIOSSurfaceCreateInfoMVK where
        showsPrec d x
          = showString "VkIOSSurfaceCreateInfoMVK {" .
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
--   > VkResult vkCreateIOSSurfaceMVK
--   >     ( VkInstance instance
--   >     , const VkIOSSurfaceCreateInfoMVK* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateIOSSurfaceMVK.html vkCreateIOSSurfaceMVK registry at www.khronos.org>
foreign import ccall unsafe "vkCreateIOSSurfaceMVK"
               vkCreateIOSSurfaceMVK ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkIOSSurfaceCreateInfoMVK -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

pattern VK_MVK_IOS_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_MVK_IOS_SURFACE_SPEC_VERSION = 2

type VK_MVK_IOS_SURFACE_SPEC_VERSION = 2

pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME :: CString

pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME <-
        (is_VK_MVK_IOS_SURFACE_EXTENSION_NAME -> True)
  where VK_MVK_IOS_SURFACE_EXTENSION_NAME
          = _VK_MVK_IOS_SURFACE_EXTENSION_NAME

_VK_MVK_IOS_SURFACE_EXTENSION_NAME :: CString

{-# INLINE _VK_MVK_IOS_SURFACE_EXTENSION_NAME #-}
_VK_MVK_IOS_SURFACE_EXTENSION_NAME = Ptr "VK_MVK_ios_surface\NUL"##

is_VK_MVK_IOS_SURFACE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_MVK_IOS_SURFACE_EXTENSION_NAME #-}
is_VK_MVK_IOS_SURFACE_EXTENSION_NAME
  = (_VK_MVK_IOS_SURFACE_EXTENSION_NAME ==)

type VK_MVK_IOS_SURFACE_EXTENSION_NAME = "VK_MVK_ios_surface"

pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK =
        VkStructureType 1000122000
