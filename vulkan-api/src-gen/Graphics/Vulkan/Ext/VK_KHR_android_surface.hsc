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
module Graphics.Vulkan.Ext.VK_KHR_android_surface
       (-- * Vulkan extension: @VK_KHR_android_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @9@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_ANDROID_KHR@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkAndroidSurfaceCreateInfoKHR(..), vkCreateAndroidSurfaceKHR,
        VK_KHR_ANDROID_SURFACE_SPEC_VERSION,
        pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION,
        VK_KHR_ANDROID_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR)
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

-- | > typedef struct VkAndroidSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkAndroidSurfaceCreateFlagsKHR   flags;
--   >     ANativeWindow*                   window;
--   > } VkAndroidSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAndroidSurfaceCreateInfoKHR.html VkAndroidSurfaceCreateInfoKHR registry at www.khronos.org>
data VkAndroidSurfaceCreateInfoKHR = VkAndroidSurfaceCreateInfoKHR## ByteArray##

instance Eq VkAndroidSurfaceCreateInfoKHR where
        (VkAndroidSurfaceCreateInfoKHR## a) ==
          (VkAndroidSurfaceCreateInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkAndroidSurfaceCreateInfoKHR where
        (VkAndroidSurfaceCreateInfoKHR## a) `compare`
          (VkAndroidSurfaceCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkAndroidSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkAndroidSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAndroidSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkAndroidSurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkAndroidSurfaceCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkAndroidSurfaceCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkAndroidSurfaceCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkAndroidSurfaceCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkAndroidSurfaceCreateInfoKHR where
        type StructFields VkAndroidSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "window"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkAndroidSurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkAndroidSurfaceCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkAndroidSurfaceCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkAndroidSurfaceCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkAndroidSurfaceCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkAndroidSurfaceCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkAndroidSurfaceCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkAndroidSurfaceCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkAndroidSurfaceCreateInfoKHR where
        type VkSTypeMType VkAndroidSurfaceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkAndroidSurfaceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "sType" VkAndroidSurfaceCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, sType}

instance CanReadField "sType" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkAndroidSurfaceCreateInfoKHR where
        type VkPNextMType VkAndroidSurfaceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "pNext" VkAndroidSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

instance CanReadField "pNext" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkAndroidSurfaceCreateInfoKHR where
        type VkFlagsMType VkAndroidSurfaceCreateInfoKHR =
             VkAndroidSurfaceCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkAndroidSurfaceCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "flags" VkAndroidSurfaceCreateInfoKHR =
             VkAndroidSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkAndroidSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, flags}

instance CanReadField "flags" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkWindow VkAndroidSurfaceCreateInfoKHR where
        type VkWindowMType VkAndroidSurfaceCreateInfoKHR =
             Ptr ANativeWindow

        {-# NOINLINE vkWindow #-}
        vkWindow x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, window})

        {-# INLINE vkWindowByteOffset #-}
        vkWindowByteOffset ~_
          = #{offset VkAndroidSurfaceCreateInfoKHR, window}

        {-# INLINE readVkWindow #-}
        readVkWindow p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, window}

        {-# INLINE writeVkWindow #-}
        writeVkWindow p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         HasField "window" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "window" VkAndroidSurfaceCreateInfoKHR =
             Ptr ANativeWindow
        type FieldOptional "window" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "window" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, window}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, window}

instance CanReadField "window" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkWindow

        {-# INLINE readField #-}
        readField = readVkWindow

instance CanWriteField "window" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkWindow

instance Show VkAndroidSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkAndroidSurfaceCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkWindow = " . showsPrec d (vkWindow x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateAndroidSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateAndroidSurfaceKHR.html vkCreateAndroidSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateAndroidSurfaceKHR"
               vkCreateAndroidSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION = 6

type VK_KHR_ANDROID_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
          = _VK_KHR_ANDROID_SURFACE_EXTENSION_NAME

_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_ANDROID_SURFACE_EXTENSION_NAME #-}
_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_android_surface\NUL"##

is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME #-}
is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  = (_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME ==)

type VK_KHR_ANDROID_SURFACE_EXTENSION_NAME =
     "VK_KHR_android_surface"

pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000008000
