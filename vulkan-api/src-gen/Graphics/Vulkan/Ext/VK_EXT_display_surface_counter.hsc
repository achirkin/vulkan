#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_display_surface_counter
       (-- * Vulkan extension: @VK_EXT_display_surface_counter@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @instance@
        --
        -- Extension number: @91@
        --
        -- Required extensions: 'VK_KHR_display'.
        --

        -- ** Required extensions: 'VK_KHR_display'.
        VkSurfaceCapabilities2EXT(..),
        vkGetPhysicalDeviceSurfaceCapabilities2EXT,
        VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION,
        pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION,
        VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME,
        pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT)
       where
import           Data.Int
import           Data.Void                        (Void)
import           Data.Word
import           Foreign.C.String                 (CString)
import           Foreign.C.Types                  (CChar (..), CFloat (..),
                                                   CInt (..), CSize (..),
                                                   CULong (..))
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Core
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkSurfaceCapabilities2EXT = VkSurfaceCapabilities2EXT## ByteArray##

instance Eq VkSurfaceCapabilities2EXT where
        (VkSurfaceCapabilities2EXT## a) == (VkSurfaceCapabilities2EXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceCapabilities2EXT where
        (VkSurfaceCapabilities2EXT## a) `compare`
          (VkSurfaceCapabilities2EXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSurfaceCapabilities2EXT where
        sizeOf ~_ = #{size VkSurfaceCapabilities2EXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceCapabilities2EXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkSurfaceCapabilities2EXT),
            I## a <- alignment (undefined :: VkSurfaceCapabilities2EXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSurfaceCapabilities2EXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSurfaceCapabilities2EXT## ba)
          | I## n <- sizeOf (undefined :: VkSurfaceCapabilities2EXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSurfaceCapabilities2EXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkSurfaceCapabilities2EXT),
            I## a <- alignment (undefined :: VkSurfaceCapabilities2EXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSurfaceCapabilities2EXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSurfaceCapabilities2EXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkSurfaceCapabilities2EXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSurfaceCapabilities2EXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSurfaceCapabilities2EXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSurfaceCapabilities2EXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkSurfaceCapabilities2EXT
         where
        type VkSTypeMType VkSurfaceCapabilities2EXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkSurfaceCapabilities2EXT
         where
        type VkPNextMType VkSurfaceCapabilities2EXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkMinImageCount VkSurfaceCapabilities2EXT where
        type VkMinImageCountMType VkSurfaceCapabilities2EXT = Word32

        {-# NOINLINE vkMinImageCount #-}
        vkMinImageCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, minImageCount})

        {-# INLINE vkMinImageCountByteOffset #-}
        vkMinImageCountByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, minImageCount}

        {-# INLINE readVkMinImageCount #-}
        readVkMinImageCount p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, minImageCount}

        {-# INLINE writeVkMinImageCount #-}
        writeVkMinImageCount p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, minImageCount}

instance {-# OVERLAPPING #-}
         HasVkMaxImageCount VkSurfaceCapabilities2EXT where
        type VkMaxImageCountMType VkSurfaceCapabilities2EXT = Word32

        {-# NOINLINE vkMaxImageCount #-}
        vkMaxImageCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, maxImageCount})

        {-# INLINE vkMaxImageCountByteOffset #-}
        vkMaxImageCountByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, maxImageCount}

        {-# INLINE readVkMaxImageCount #-}
        readVkMaxImageCount p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageCount}

        {-# INLINE writeVkMaxImageCount #-}
        writeVkMaxImageCount p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageCount}

instance {-# OVERLAPPING #-}
         HasVkCurrentExtent VkSurfaceCapabilities2EXT where
        type VkCurrentExtentMType VkSurfaceCapabilities2EXT = VkExtent2D

        {-# NOINLINE vkCurrentExtent #-}
        vkCurrentExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, currentExtent})

        {-# INLINE vkCurrentExtentByteOffset #-}
        vkCurrentExtentByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, currentExtent}

        {-# INLINE readVkCurrentExtent #-}
        readVkCurrentExtent p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, currentExtent}

        {-# INLINE writeVkCurrentExtent #-}
        writeVkCurrentExtent p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, currentExtent}

instance {-# OVERLAPPING #-}
         HasVkMinImageExtent VkSurfaceCapabilities2EXT where
        type VkMinImageExtentMType VkSurfaceCapabilities2EXT = VkExtent2D

        {-# NOINLINE vkMinImageExtent #-}
        vkMinImageExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, minImageExtent})

        {-# INLINE vkMinImageExtentByteOffset #-}
        vkMinImageExtentByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, minImageExtent}

        {-# INLINE readVkMinImageExtent #-}
        readVkMinImageExtent p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, minImageExtent}

        {-# INLINE writeVkMinImageExtent #-}
        writeVkMinImageExtent p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, minImageExtent}

instance {-# OVERLAPPING #-}
         HasVkMaxImageExtent VkSurfaceCapabilities2EXT where
        type VkMaxImageExtentMType VkSurfaceCapabilities2EXT = VkExtent2D

        {-# NOINLINE vkMaxImageExtent #-}
        vkMaxImageExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, maxImageExtent})

        {-# INLINE vkMaxImageExtentByteOffset #-}
        vkMaxImageExtentByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, maxImageExtent}

        {-# INLINE readVkMaxImageExtent #-}
        readVkMaxImageExtent p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageExtent}

        {-# INLINE writeVkMaxImageExtent #-}
        writeVkMaxImageExtent p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageExtent}

instance {-# OVERLAPPING #-}
         HasVkMaxImageArrayLayers VkSurfaceCapabilities2EXT where
        type VkMaxImageArrayLayersMType VkSurfaceCapabilities2EXT = Word32

        {-# NOINLINE vkMaxImageArrayLayers #-}
        vkMaxImageArrayLayers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers})

        {-# INLINE vkMaxImageArrayLayersByteOffset #-}
        vkMaxImageArrayLayersByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}

        {-# INLINE readVkMaxImageArrayLayers #-}
        readVkMaxImageArrayLayers p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}

        {-# INLINE writeVkMaxImageArrayLayers #-}
        writeVkMaxImageArrayLayers p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         HasVkSupportedTransforms VkSurfaceCapabilities2EXT where
        type VkSupportedTransformsMType VkSurfaceCapabilities2EXT =
             VkSurfaceTransformFlagsKHR

        {-# NOINLINE vkSupportedTransforms #-}
        vkSupportedTransforms x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedTransforms})

        {-# INLINE vkSupportedTransformsByteOffset #-}
        vkSupportedTransformsByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, supportedTransforms}

        {-# INLINE readVkSupportedTransforms #-}
        readVkSupportedTransforms p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedTransforms}

        {-# INLINE writeVkSupportedTransforms #-}
        writeVkSupportedTransforms p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedTransforms}

instance {-# OVERLAPPING #-}
         HasVkCurrentTransform VkSurfaceCapabilities2EXT where
        type VkCurrentTransformMType VkSurfaceCapabilities2EXT =
             VkSurfaceTransformFlagBitsKHR

        {-# NOINLINE vkCurrentTransform #-}
        vkCurrentTransform x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, currentTransform})

        {-# INLINE vkCurrentTransformByteOffset #-}
        vkCurrentTransformByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, currentTransform}

        {-# INLINE readVkCurrentTransform #-}
        readVkCurrentTransform p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, currentTransform}

        {-# INLINE writeVkCurrentTransform #-}
        writeVkCurrentTransform p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, currentTransform}

instance {-# OVERLAPPING #-}
         HasVkSupportedCompositeAlpha VkSurfaceCapabilities2EXT where
        type VkSupportedCompositeAlphaMType VkSurfaceCapabilities2EXT =
             VkCompositeAlphaFlagsKHR

        {-# NOINLINE vkSupportedCompositeAlpha #-}
        vkSupportedCompositeAlpha x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha})

        {-# INLINE vkSupportedCompositeAlphaByteOffset #-}
        vkSupportedCompositeAlphaByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}

        {-# INLINE readVkSupportedCompositeAlpha #-}
        readVkSupportedCompositeAlpha p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}

        {-# INLINE writeVkSupportedCompositeAlpha #-}
        writeVkSupportedCompositeAlpha p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedCompositeAlpha}

instance {-# OVERLAPPING #-}
         HasVkSupportedUsageFlags VkSurfaceCapabilities2EXT where
        type VkSupportedUsageFlagsMType VkSurfaceCapabilities2EXT =
             VkImageUsageFlags

        {-# NOINLINE vkSupportedUsageFlags #-}
        vkSupportedUsageFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags})

        {-# INLINE vkSupportedUsageFlagsByteOffset #-}
        vkSupportedUsageFlagsByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}

        {-# INLINE readVkSupportedUsageFlags #-}
        readVkSupportedUsageFlags p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}

        {-# INLINE writeVkSupportedUsageFlags #-}
        writeVkSupportedUsageFlags p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedUsageFlags}

instance {-# OVERLAPPING #-}
         HasVkSupportedSurfaceCounters VkSurfaceCapabilities2EXT where
        type VkSupportedSurfaceCountersMType VkSurfaceCapabilities2EXT =
             VkSurfaceCounterFlagsEXT

        {-# NOINLINE vkSupportedSurfaceCounters #-}
        vkSupportedSurfaceCounters x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters})

        {-# INLINE vkSupportedSurfaceCountersByteOffset #-}
        vkSupportedSurfaceCountersByteOffset ~_
          = #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}

        {-# INLINE readVkSupportedSurfaceCounters #-}
        readVkSupportedSurfaceCounters p
          = peekByteOff p #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}

        {-# INLINE writeVkSupportedSurfaceCounters #-}
        writeVkSupportedSurfaceCounters p
          = pokeByteOff p #{offset VkSurfaceCapabilities2EXT, supportedSurfaceCounters}

instance Show VkSurfaceCapabilities2EXT where
        showsPrec d x
          = showString "VkSurfaceCapabilities2EXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMinImageCount = " .
                            showsPrec d (vkMinImageCount x) .
                              showString ", " .
                                showString "vkMaxImageCount = " .
                                  showsPrec d (vkMaxImageCount x) .
                                    showString ", " .
                                      showString "vkCurrentExtent = " .
                                        showsPrec d (vkCurrentExtent x) .
                                          showString ", " .
                                            showString "vkMinImageExtent = " .
                                              showsPrec d (vkMinImageExtent x) .
                                                showString ", " .
                                                  showString "vkMaxImageExtent = " .
                                                    showsPrec d (vkMaxImageExtent x) .
                                                      showString ", " .
                                                        showString "vkMaxImageArrayLayers = " .
                                                          showsPrec d (vkMaxImageArrayLayers x) .
                                                            showString ", " .
                                                              showString "vkSupportedTransforms = "
                                                                .
                                                                showsPrec d
                                                                  (vkSupportedTransforms x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkCurrentTransform = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkCurrentTransform x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkSupportedCompositeAlpha = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkSupportedCompositeAlpha
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkSupportedUsageFlags = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkSupportedUsageFlags
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "vkSupportedSurfaceCounters = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (vkSupportedSurfaceCounters
                                                                                             x)
                                                                                          .
                                                                                          showChar
                                                                                            '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilities2EXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , VkSurfaceCapabilities2EXT* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceCapabilities2EXT.html vkGetPhysicalDeviceSurfaceCapabilities2EXT registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSurfaceCapabilities2EXT"
               vkGetPhysicalDeviceSurfaceCapabilities2EXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkSurfaceCapabilities2EXT -- ^ pSurfaceCapabilities
                                                               -> IO VkResult

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1

type VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: CString

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME <-
        (is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME -> True)
  where VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
          = _VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME

_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME #-}
_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  = Ptr "VK_EXT_display_surface_counter\NUL"##

is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME #-}
is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  = (_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME ==)

type VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME =
     "VK_EXT_display_surface_counter"

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT =
        VkStructureType 1000090000

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT =
        VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
