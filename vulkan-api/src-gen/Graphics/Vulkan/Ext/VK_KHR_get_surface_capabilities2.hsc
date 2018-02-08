#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_get_surface_capabilities2
       (-- * Vulkan extension: @VK_KHR_get_surface_capabilities2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @120@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkPhysicalDeviceSurfaceInfo2KHR(..),
        VkSurfaceCapabilities2KHR(..), VkSurfaceFormat2KHR(..),
        vkGetPhysicalDeviceSurfaceCapabilities2KHR,
        vkGetPhysicalDeviceSurfaceFormats2KHR,
        VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION,
        pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION,
        VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME,
        pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkSurfaceCapabilitiesKHR,
                                                   VkSurfaceFormatKHR)
import           Graphics.Vulkan.Common           (VkPhysicalDevice,
                                                   VkResult (..),
                                                   VkStructureType,
                                                   VkStructureType (..),
                                                   VkSurfaceKHR, Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkPhysicalDeviceSurfaceInfo2KHR = VkPhysicalDeviceSurfaceInfo2KHR## ByteArray##

instance Eq VkPhysicalDeviceSurfaceInfo2KHR where
        (VkPhysicalDeviceSurfaceInfo2KHR## a) ==
          (VkPhysicalDeviceSurfaceInfo2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSurfaceInfo2KHR where
        (VkPhysicalDeviceSurfaceInfo2KHR## a) `compare`
          (VkPhysicalDeviceSurfaceInfo2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSurfaceInfo2KHR where
        sizeOf ~_ = #{size VkPhysicalDeviceSurfaceInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSurfaceInfo2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceSurfaceInfo2KHR),
            I## a <- alignment (undefined :: VkPhysicalDeviceSurfaceInfo2KHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceSurfaceInfo2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceSurfaceInfo2KHR## ba)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceSurfaceInfo2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceSurfaceInfo2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceSurfaceInfo2KHR),
            I## a <- alignment (undefined :: VkPhysicalDeviceSurfaceInfo2KHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceSurfaceInfo2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceSurfaceInfo2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkPhysicalDeviceSurfaceInfo2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceSurfaceInfo2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceSurfaceInfo2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceSurfaceInfo2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceSurfaceInfo2KHR where
        type VkSTypeMType VkPhysicalDeviceSurfaceInfo2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceSurfaceInfo2KHR where
        type VkPNextMType VkPhysicalDeviceSurfaceInfo2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkSurface VkPhysicalDeviceSurfaceInfo2KHR where
        type VkSurfaceMType VkPhysicalDeviceSurfaceInfo2KHR = VkSurfaceKHR

        {-# NOINLINE vkSurface #-}
        vkSurface x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface})

        {-# INLINE vkSurfaceByteOffset #-}
        vkSurfaceByteOffset ~_
          = #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface}

        {-# INLINE readVkSurface #-}
        readVkSurface p
          = peekByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface}

        {-# INLINE writeVkSurface #-}
        writeVkSurface p
          = pokeByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface}

instance Show VkPhysicalDeviceSurfaceInfo2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceSurfaceInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSurface = " .
                            showsPrec d (vkSurface x) . showChar '}'

data VkSurfaceCapabilities2KHR = VkSurfaceCapabilities2KHR## ByteArray##

instance Eq VkSurfaceCapabilities2KHR where
        (VkSurfaceCapabilities2KHR## a) == (VkSurfaceCapabilities2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceCapabilities2KHR where
        (VkSurfaceCapabilities2KHR## a) `compare`
          (VkSurfaceCapabilities2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSurfaceCapabilities2KHR where
        sizeOf ~_ = #{size VkSurfaceCapabilities2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceCapabilities2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkSurfaceCapabilities2KHR),
            I## a <- alignment (undefined :: VkSurfaceCapabilities2KHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSurfaceCapabilities2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSurfaceCapabilities2KHR## ba)
          | I## n <- sizeOf (undefined :: VkSurfaceCapabilities2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSurfaceCapabilities2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkSurfaceCapabilities2KHR),
            I## a <- alignment (undefined :: VkSurfaceCapabilities2KHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSurfaceCapabilities2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSurfaceCapabilities2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkSurfaceCapabilities2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSurfaceCapabilities2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSurfaceCapabilities2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSurfaceCapabilities2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkSurfaceCapabilities2KHR
         where
        type VkSTypeMType VkSurfaceCapabilities2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSurfaceCapabilities2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkSurfaceCapabilities2KHR
         where
        type VkPNextMType VkSurfaceCapabilities2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSurfaceCapabilities2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkSurfaceCapabilities VkSurfaceCapabilities2KHR where
        type VkSurfaceCapabilitiesMType VkSurfaceCapabilities2KHR =
             VkSurfaceCapabilitiesKHR

        {-# NOINLINE vkSurfaceCapabilities #-}
        vkSurfaceCapabilities x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities})

        {-# INLINE vkSurfaceCapabilitiesByteOffset #-}
        vkSurfaceCapabilitiesByteOffset ~_
          = #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

        {-# INLINE readVkSurfaceCapabilities #-}
        readVkSurfaceCapabilities p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

        {-# INLINE writeVkSurfaceCapabilities #-}
        writeVkSurfaceCapabilities p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

instance Show VkSurfaceCapabilities2KHR where
        showsPrec d x
          = showString "VkSurfaceCapabilities2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSurfaceCapabilities = " .
                            showsPrec d (vkSurfaceCapabilities x) . showChar '}'

data VkSurfaceFormat2KHR = VkSurfaceFormat2KHR## ByteArray##

instance Eq VkSurfaceFormat2KHR where
        (VkSurfaceFormat2KHR## a) == (VkSurfaceFormat2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceFormat2KHR where
        (VkSurfaceFormat2KHR## a) `compare` (VkSurfaceFormat2KHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSurfaceFormat2KHR where
        sizeOf ~_ = #{size VkSurfaceFormat2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceFormat2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkSurfaceFormat2KHR),
            I## a <- alignment (undefined :: VkSurfaceFormat2KHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSurfaceFormat2KHR## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSurfaceFormat2KHR## ba)
          | I## n <- sizeOf (undefined :: VkSurfaceFormat2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSurfaceFormat2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkSurfaceFormat2KHR),
            I## a <- alignment (undefined :: VkSurfaceFormat2KHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSurfaceFormat2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSurfaceFormat2KHR## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkSurfaceFormat2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSurfaceFormat2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSurfaceFormat2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSurfaceFormat2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkSurfaceFormat2KHR where
        type VkSTypeMType VkSurfaceFormat2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSurfaceFormat2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkSurfaceFormat2KHR where
        type VkPNextMType VkSurfaceFormat2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSurfaceFormat2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, pNext}

instance {-# OVERLAPPING #-} HasVkSurfaceFormat VkSurfaceFormat2KHR
         where
        type VkSurfaceFormatMType VkSurfaceFormat2KHR = VkSurfaceFormatKHR

        {-# NOINLINE vkSurfaceFormat #-}
        vkSurfaceFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, surfaceFormat})

        {-# INLINE vkSurfaceFormatByteOffset #-}
        vkSurfaceFormatByteOffset ~_
          = #{offset VkSurfaceFormat2KHR, surfaceFormat}

        {-# INLINE readVkSurfaceFormat #-}
        readVkSurfaceFormat p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, surfaceFormat}

        {-# INLINE writeVkSurfaceFormat #-}
        writeVkSurfaceFormat p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, surfaceFormat}

instance Show VkSurfaceFormat2KHR where
        showsPrec d x
          = showString "VkSurfaceFormat2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSurfaceFormat = " .
                            showsPrec d (vkSurfaceFormat x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilities2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , VkSurfaceCapabilities2KHR* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceCapabilities2KHR.html vkGetPhysicalDeviceSurfaceCapabilities2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSurfaceCapabilities2KHR"
               vkGetPhysicalDeviceSurfaceCapabilities2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                                     ->
                   Ptr VkSurfaceCapabilities2KHR -- ^ pSurfaceCapabilities
                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceFormats2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , uint32_t* pSurfaceFormatCount
--   >     , VkSurfaceFormat2KHR* pSurfaceFormats
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceFormats2KHR.html vkGetPhysicalDeviceSurfaceFormats2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceSurfaceFormats2KHR"
               vkGetPhysicalDeviceSurfaceFormats2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                                     ->
                   Ptr Word32 -- ^ pSurfaceFormatCount
                              -> Ptr VkSurfaceFormat2KHR -- ^ pSurfaceFormats
                                                         -> IO VkResult

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1

type VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: CString

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME <-
        (is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME -> True)
  where VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
          = _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME

_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME #-}
_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  = Ptr "VK_KHR_get_surface_capabilities2\NUL"##

is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME ::
                                                    CString -> Bool

{-# INLINE is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME #-}
is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  = (_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME ==)

type VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME =
     "VK_KHR_get_surface_capabilities2"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR =
        VkStructureType 1000119000

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR =
        VkStructureType 1000119001

pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR =
        VkStructureType 1000119002
