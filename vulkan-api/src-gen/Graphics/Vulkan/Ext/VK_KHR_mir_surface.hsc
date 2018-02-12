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
module Graphics.Vulkan.Ext.VK_KHR_mir_surface
       (-- * Vulkan extension: @VK_KHR_mir_surface@
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
        -- Extension number: @8@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_MIR_KHR@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkMirSurfaceCreateInfoKHR(..), vkCreateMirSurfaceKHR,
        vkGetPhysicalDeviceMirPresentationSupportKHR,
        VK_KHR_MIR_SURFACE_SPEC_VERSION,
        pattern VK_KHR_MIR_SURFACE_SPEC_VERSION,
        VK_KHR_MIR_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR)
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

-- | > typedef struct VkMirSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMirSurfaceCreateFlagsKHR   flags;
--   >     MirConnection*                   connection;
--   >     MirSurface*                      mirSurface;
--   > } VkMirSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMirSurfaceCreateInfoKHR.html VkMirSurfaceCreateInfoKHR registry at www.khronos.org>
data VkMirSurfaceCreateInfoKHR = VkMirSurfaceCreateInfoKHR## ByteArray##

instance Eq VkMirSurfaceCreateInfoKHR where
        (VkMirSurfaceCreateInfoKHR## a) == (VkMirSurfaceCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkMirSurfaceCreateInfoKHR where
        (VkMirSurfaceCreateInfoKHR## a) `compare`
          (VkMirSurfaceCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkMirSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkMirSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMirSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkMirSurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkMirSurfaceCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkMirSurfaceCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkMirSurfaceCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkMirSurfaceCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkMirSurfaceCreateInfoKHR where
        type StructFields VkMirSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "connection", "mirSurface"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkMirSurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkMirSurfaceCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkMirSurfaceCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkMirSurfaceCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkMirSurfaceCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkMirSurfaceCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkMirSurfaceCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkMirSurfaceCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkMirSurfaceCreateInfoKHR
         where
        type VkSTypeMType VkMirSurfaceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMirSurfaceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMirSurfaceCreateInfoKHR where
        type FieldType "sType" VkMirSurfaceCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, sType}

instance CanReadField "sType" VkMirSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMirSurfaceCreateInfoKHR
         where
        type VkPNextMType VkMirSurfaceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMirSurfaceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMirSurfaceCreateInfoKHR where
        type FieldType "pNext" VkMirSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, pNext}

instance CanReadField "pNext" VkMirSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkMirSurfaceCreateInfoKHR
         where
        type VkFlagsMType VkMirSurfaceCreateInfoKHR =
             VkMirSurfaceCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkMirSurfaceCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkMirSurfaceCreateInfoKHR where
        type FieldType "flags" VkMirSurfaceCreateInfoKHR =
             VkMirSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkMirSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, flags}

instance CanReadField "flags" VkMirSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkConnection VkMirSurfaceCreateInfoKHR where
        type VkConnectionMType VkMirSurfaceCreateInfoKHR =
             Ptr MirConnection

        {-# NOINLINE vkConnection #-}
        vkConnection x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, connection})

        {-# INLINE vkConnectionByteOffset #-}
        vkConnectionByteOffset ~_
          = #{offset VkMirSurfaceCreateInfoKHR, connection}

        {-# INLINE readVkConnection #-}
        readVkConnection p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, connection}

        {-# INLINE writeVkConnection #-}
        writeVkConnection p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, connection}

instance {-# OVERLAPPING #-}
         HasField "connection" VkMirSurfaceCreateInfoKHR where
        type FieldType "connection" VkMirSurfaceCreateInfoKHR =
             Ptr MirConnection
        type FieldOptional "connection" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "connection" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, connection}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, connection}

instance CanReadField "connection" VkMirSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkConnection

        {-# INLINE readField #-}
        readField = readVkConnection

instance CanWriteField "connection" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkConnection

instance {-# OVERLAPPING #-}
         HasVkMirSurface VkMirSurfaceCreateInfoKHR where
        type VkMirSurfaceMType VkMirSurfaceCreateInfoKHR = Ptr MirSurface

        {-# NOINLINE vkMirSurface #-}
        vkMirSurface x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, mirSurface})

        {-# INLINE vkMirSurfaceByteOffset #-}
        vkMirSurfaceByteOffset ~_
          = #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

        {-# INLINE readVkMirSurface #-}
        readVkMirSurface p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

        {-# INLINE writeVkMirSurface #-}
        writeVkMirSurface p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

instance {-# OVERLAPPING #-}
         HasField "mirSurface" VkMirSurfaceCreateInfoKHR where
        type FieldType "mirSurface" VkMirSurfaceCreateInfoKHR =
             Ptr MirSurface
        type FieldOptional "mirSurface" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mirSurface" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

instance CanReadField "mirSurface" VkMirSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkMirSurface

        {-# INLINE readField #-}
        readField = readVkMirSurface

instance CanWriteField "mirSurface" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkMirSurface

instance Show VkMirSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkMirSurfaceCreateInfoKHR {" .
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
                                      showString "vkMirSurface = " .
                                        showsPrec d (vkMirSurface x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateMirSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkMirSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateMirSurfaceKHR.html vkCreateMirSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateMirSurfaceKHR"
               vkCreateMirSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , MirConnection* connection
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceMirPresentationSupportKHR.html vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceMirPresentationSupportKHR"
               vkGetPhysicalDeviceMirPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr MirConnection -- ^ connection
                                                               -> IO VkBool32

pattern VK_KHR_MIR_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_MIR_SURFACE_SPEC_VERSION = 4

type VK_KHR_MIR_SURFACE_SPEC_VERSION = 4

pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_MIR_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_MIR_SURFACE_EXTENSION_NAME
          = _VK_KHR_MIR_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_MIR_SURFACE_EXTENSION_NAME #-}

_VK_KHR_MIR_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_MIR_SURFACE_EXTENSION_NAME = Ptr "VK_KHR_mir_surface\NUL"##

{-# INLINE is_VK_KHR_MIR_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_MIR_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_MIR_SURFACE_EXTENSION_NAME
  = eqCStrings _VK_KHR_MIR_SURFACE_EXTENSION_NAME

type VK_KHR_MIR_SURFACE_EXTENSION_NAME = "VK_KHR_mir_surface"

pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000007000
