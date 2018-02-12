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
module Graphics.Vulkan.Ext.VK_NN_vi_surface
       (-- * Vulkan extension: @VK_NN_vi_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Mathias Heyer @mheyer@
        --
        -- author: @NN@
        --
        -- type: @instance@
        --
        -- Extension number: @63@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_VI_NN@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkViSurfaceCreateInfoNN(..), vkCreateViSurfaceNN,
        VK_NN_VI_SURFACE_SPEC_VERSION,
        pattern VK_NN_VI_SURFACE_SPEC_VERSION,
        VK_NN_VI_SURFACE_EXTENSION_NAME,
        pattern VK_NN_VI_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN)
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

-- | > typedef struct VkViSurfaceCreateInfoNN {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkViSurfaceCreateFlagsNN   flags;
--   >     void*                            window;
--   > } VkViSurfaceCreateInfoNN;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkViSurfaceCreateInfoNN.html VkViSurfaceCreateInfoNN registry at www.khronos.org>
data VkViSurfaceCreateInfoNN = VkViSurfaceCreateInfoNN## ByteArray##

instance Eq VkViSurfaceCreateInfoNN where
        (VkViSurfaceCreateInfoNN## a) == (VkViSurfaceCreateInfoNN## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkViSurfaceCreateInfoNN where
        (VkViSurfaceCreateInfoNN## a) `compare` (VkViSurfaceCreateInfoNN## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkViSurfaceCreateInfoNN where
        sizeOf ~_ = #{size VkViSurfaceCreateInfoNN}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViSurfaceCreateInfoNN}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkViSurfaceCreateInfoNN),
            I## a <- alignment (undefined :: VkViSurfaceCreateInfoNN) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkViSurfaceCreateInfoNN##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkViSurfaceCreateInfoNN## ba)
          | I## n <- sizeOf (undefined :: VkViSurfaceCreateInfoNN) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkViSurfaceCreateInfoNN where
        type StructFields VkViSurfaceCreateInfoNN =
             '["sType", "pNext", "flags", "window"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkViSurfaceCreateInfoNN),
            I## a <- alignment (undefined :: VkViSurfaceCreateInfoNN) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkViSurfaceCreateInfoNN##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkViSurfaceCreateInfoNN## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkViSurfaceCreateInfoNN##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkViSurfaceCreateInfoNN## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkViSurfaceCreateInfoNN## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkViSurfaceCreateInfoNN## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkViSurfaceCreateInfoNN
         where
        type VkSTypeMType VkViSurfaceCreateInfoNN = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkViSurfaceCreateInfoNN, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkViSurfaceCreateInfoNN where
        type FieldType "sType" VkViSurfaceCreateInfoNN = VkStructureType
        type FieldOptional "sType" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, sType}

instance CanReadField "sType" VkViSurfaceCreateInfoNN where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkViSurfaceCreateInfoNN
         where
        type VkPNextMType VkViSurfaceCreateInfoNN = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkViSurfaceCreateInfoNN, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkViSurfaceCreateInfoNN where
        type FieldType "pNext" VkViSurfaceCreateInfoNN = Ptr Void
        type FieldOptional "pNext" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, pNext}

instance CanReadField "pNext" VkViSurfaceCreateInfoNN where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkViSurfaceCreateInfoNN
         where
        type VkFlagsMType VkViSurfaceCreateInfoNN =
             VkViSurfaceCreateFlagsNN

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkViSurfaceCreateInfoNN, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkViSurfaceCreateInfoNN where
        type FieldType "flags" VkViSurfaceCreateInfoNN =
             VkViSurfaceCreateFlagsNN
        type FieldOptional "flags" VkViSurfaceCreateInfoNN = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, flags}

instance CanReadField "flags" VkViSurfaceCreateInfoNN where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkWindow VkViSurfaceCreateInfoNN
         where
        type VkWindowMType VkViSurfaceCreateInfoNN = Ptr Void

        {-# NOINLINE vkWindow #-}
        vkWindow x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, window})

        {-# INLINE vkWindowByteOffset #-}
        vkWindowByteOffset ~_
          = #{offset VkViSurfaceCreateInfoNN, window}

        {-# INLINE readVkWindow #-}
        readVkWindow p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, window}

        {-# INLINE writeVkWindow #-}
        writeVkWindow p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, window}

instance {-# OVERLAPPING #-}
         HasField "window" VkViSurfaceCreateInfoNN where
        type FieldType "window" VkViSurfaceCreateInfoNN = Ptr Void
        type FieldOptional "window" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type FieldOffset "window" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, window}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, window}

instance CanReadField "window" VkViSurfaceCreateInfoNN where
        {-# INLINE getField #-}
        getField = vkWindow

        {-# INLINE readField #-}
        readField = readVkWindow

instance CanWriteField "window" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField = writeVkWindow

instance Show VkViSurfaceCreateInfoNN where
        showsPrec d x
          = showString "VkViSurfaceCreateInfoNN {" .
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
--   > VkResult vkCreateViSurfaceNN
--   >     ( VkInstance instance
--   >     , const VkViSurfaceCreateInfoNN* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateViSurfaceNN.html vkCreateViSurfaceNN registry at www.khronos.org>
foreign import ccall unsafe "vkCreateViSurfaceNN"
               vkCreateViSurfaceNN ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkViSurfaceCreateInfoNN -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

pattern VK_NN_VI_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NN_VI_SURFACE_SPEC_VERSION = 1

type VK_NN_VI_SURFACE_SPEC_VERSION = 1

pattern VK_NN_VI_SURFACE_EXTENSION_NAME :: CString

pattern VK_NN_VI_SURFACE_EXTENSION_NAME <-
        (is_VK_NN_VI_SURFACE_EXTENSION_NAME -> True)
  where VK_NN_VI_SURFACE_EXTENSION_NAME
          = _VK_NN_VI_SURFACE_EXTENSION_NAME

{-# INLINE _VK_NN_VI_SURFACE_EXTENSION_NAME #-}

_VK_NN_VI_SURFACE_EXTENSION_NAME :: CString
_VK_NN_VI_SURFACE_EXTENSION_NAME = Ptr "VK_NN_vi_surface\NUL"##

{-# INLINE is_VK_NN_VI_SURFACE_EXTENSION_NAME #-}

is_VK_NN_VI_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_NN_VI_SURFACE_EXTENSION_NAME
  = eqCStrings _VK_NN_VI_SURFACE_EXTENSION_NAME

type VK_NN_VI_SURFACE_EXTENSION_NAME = "VK_NN_vi_surface"

pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN =
        VkStructureType 1000062000
