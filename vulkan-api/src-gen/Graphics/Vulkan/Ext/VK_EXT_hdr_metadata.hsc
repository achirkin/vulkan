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
module Graphics.Vulkan.Ext.VK_EXT_hdr_metadata
       (-- * Vulkan extension: @VK_EXT_hdr_metadata@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Courtney Goeltzenleuchter @courtneygo@
        --
        -- author: @GOOGLE@
        --
        -- type: @device@
        --
        -- Extension number: @106@
        --
        -- Required extensions: 'VK_KHR_swapchain'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain'.
        VkHdrMetadataEXT(..), VkXYColorEXT(..), vkSetHdrMetadataEXT,
        VK_EXT_HDR_METADATA_SPEC_VERSION,
        pattern VK_EXT_HDR_METADATA_SPEC_VERSION,
        VK_EXT_HDR_METADATA_EXTENSION_NAME,
        pattern VK_EXT_HDR_METADATA_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT)
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

data VkHdrMetadataEXT = VkHdrMetadataEXT## ByteArray##

instance Eq VkHdrMetadataEXT where
        (VkHdrMetadataEXT## a) == (VkHdrMetadataEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkHdrMetadataEXT where
        (VkHdrMetadataEXT## a) `compare` (VkHdrMetadataEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkHdrMetadataEXT where
        sizeOf ~_ = #{size VkHdrMetadataEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkHdrMetadataEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkHdrMetadataEXT),
            I## a <- alignment (undefined :: VkHdrMetadataEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3, VkHdrMetadataEXT## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkHdrMetadataEXT## ba)
          | I## n <- sizeOf (undefined :: VkHdrMetadataEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkHdrMetadataEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkHdrMetadataEXT),
            I## a <- alignment (undefined :: VkHdrMetadataEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkHdrMetadataEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkHdrMetadataEXT## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkHdrMetadataEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkHdrMetadataEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkHdrMetadataEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkHdrMetadataEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkHdrMetadataEXT where
        type VkSTypeMType VkHdrMetadataEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkHdrMetadataEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkHdrMetadataEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkHdrMetadataEXT, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkHdrMetadataEXT where
        type VkPNextMType VkHdrMetadataEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkHdrMetadataEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkHdrMetadataEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkHdrMetadataEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkDisplayPrimaryRed VkHdrMetadataEXT where
        type VkDisplayPrimaryRedMType VkHdrMetadataEXT = VkXYColorEXT

        {-# NOINLINE vkDisplayPrimaryRed #-}
        vkDisplayPrimaryRed x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, displayPrimaryRed})

        {-# INLINE vkDisplayPrimaryRedByteOffset #-}
        vkDisplayPrimaryRedByteOffset ~_
          = #{offset VkHdrMetadataEXT, displayPrimaryRed}

        {-# INLINE readVkDisplayPrimaryRed #-}
        readVkDisplayPrimaryRed p
          = peekByteOff p #{offset VkHdrMetadataEXT, displayPrimaryRed}

        {-# INLINE writeVkDisplayPrimaryRed #-}
        writeVkDisplayPrimaryRed p
          = pokeByteOff p #{offset VkHdrMetadataEXT, displayPrimaryRed}

instance {-# OVERLAPPING #-}
         HasVkDisplayPrimaryGreen VkHdrMetadataEXT where
        type VkDisplayPrimaryGreenMType VkHdrMetadataEXT = VkXYColorEXT

        {-# NOINLINE vkDisplayPrimaryGreen #-}
        vkDisplayPrimaryGreen x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, displayPrimaryGreen})

        {-# INLINE vkDisplayPrimaryGreenByteOffset #-}
        vkDisplayPrimaryGreenByteOffset ~_
          = #{offset VkHdrMetadataEXT, displayPrimaryGreen}

        {-# INLINE readVkDisplayPrimaryGreen #-}
        readVkDisplayPrimaryGreen p
          = peekByteOff p #{offset VkHdrMetadataEXT, displayPrimaryGreen}

        {-# INLINE writeVkDisplayPrimaryGreen #-}
        writeVkDisplayPrimaryGreen p
          = pokeByteOff p #{offset VkHdrMetadataEXT, displayPrimaryGreen}

instance {-# OVERLAPPING #-}
         HasVkDisplayPrimaryBlue VkHdrMetadataEXT where
        type VkDisplayPrimaryBlueMType VkHdrMetadataEXT = VkXYColorEXT

        {-# NOINLINE vkDisplayPrimaryBlue #-}
        vkDisplayPrimaryBlue x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, displayPrimaryBlue})

        {-# INLINE vkDisplayPrimaryBlueByteOffset #-}
        vkDisplayPrimaryBlueByteOffset ~_
          = #{offset VkHdrMetadataEXT, displayPrimaryBlue}

        {-# INLINE readVkDisplayPrimaryBlue #-}
        readVkDisplayPrimaryBlue p
          = peekByteOff p #{offset VkHdrMetadataEXT, displayPrimaryBlue}

        {-# INLINE writeVkDisplayPrimaryBlue #-}
        writeVkDisplayPrimaryBlue p
          = pokeByteOff p #{offset VkHdrMetadataEXT, displayPrimaryBlue}

instance {-# OVERLAPPING #-} HasVkWhitePoint VkHdrMetadataEXT where
        type VkWhitePointMType VkHdrMetadataEXT = VkXYColorEXT

        {-# NOINLINE vkWhitePoint #-}
        vkWhitePoint x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, whitePoint})

        {-# INLINE vkWhitePointByteOffset #-}
        vkWhitePointByteOffset ~_
          = #{offset VkHdrMetadataEXT, whitePoint}

        {-# INLINE readVkWhitePoint #-}
        readVkWhitePoint p
          = peekByteOff p #{offset VkHdrMetadataEXT, whitePoint}

        {-# INLINE writeVkWhitePoint #-}
        writeVkWhitePoint p
          = pokeByteOff p #{offset VkHdrMetadataEXT, whitePoint}

instance {-# OVERLAPPING #-} HasVkMaxLuminance VkHdrMetadataEXT
         where
        type VkMaxLuminanceMType VkHdrMetadataEXT =
             #{type float}

        {-# NOINLINE vkMaxLuminance #-}
        vkMaxLuminance x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, maxLuminance})

        {-# INLINE vkMaxLuminanceByteOffset #-}
        vkMaxLuminanceByteOffset ~_
          = #{offset VkHdrMetadataEXT, maxLuminance}

        {-# INLINE readVkMaxLuminance #-}
        readVkMaxLuminance p
          = peekByteOff p #{offset VkHdrMetadataEXT, maxLuminance}

        {-# INLINE writeVkMaxLuminance #-}
        writeVkMaxLuminance p
          = pokeByteOff p #{offset VkHdrMetadataEXT, maxLuminance}

instance {-# OVERLAPPING #-} HasVkMinLuminance VkHdrMetadataEXT
         where
        type VkMinLuminanceMType VkHdrMetadataEXT =
             #{type float}

        {-# NOINLINE vkMinLuminance #-}
        vkMinLuminance x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, minLuminance})

        {-# INLINE vkMinLuminanceByteOffset #-}
        vkMinLuminanceByteOffset ~_
          = #{offset VkHdrMetadataEXT, minLuminance}

        {-# INLINE readVkMinLuminance #-}
        readVkMinLuminance p
          = peekByteOff p #{offset VkHdrMetadataEXT, minLuminance}

        {-# INLINE writeVkMinLuminance #-}
        writeVkMinLuminance p
          = pokeByteOff p #{offset VkHdrMetadataEXT, minLuminance}

instance {-# OVERLAPPING #-}
         HasVkMaxContentLightLevel VkHdrMetadataEXT where
        type VkMaxContentLightLevelMType VkHdrMetadataEXT =
             #{type float}

        {-# NOINLINE vkMaxContentLightLevel #-}
        vkMaxContentLightLevel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, maxContentLightLevel})

        {-# INLINE vkMaxContentLightLevelByteOffset #-}
        vkMaxContentLightLevelByteOffset ~_
          = #{offset VkHdrMetadataEXT, maxContentLightLevel}

        {-# INLINE readVkMaxContentLightLevel #-}
        readVkMaxContentLightLevel p
          = peekByteOff p #{offset VkHdrMetadataEXT, maxContentLightLevel}

        {-# INLINE writeVkMaxContentLightLevel #-}
        writeVkMaxContentLightLevel p
          = pokeByteOff p #{offset VkHdrMetadataEXT, maxContentLightLevel}

instance {-# OVERLAPPING #-}
         HasVkMaxFrameAverageLightLevel VkHdrMetadataEXT where
        type VkMaxFrameAverageLightLevelMType VkHdrMetadataEXT =
             #{type float}

        {-# NOINLINE vkMaxFrameAverageLightLevel #-}
        vkMaxFrameAverageLightLevel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel})

        {-# INLINE vkMaxFrameAverageLightLevelByteOffset #-}
        vkMaxFrameAverageLightLevelByteOffset ~_
          = #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}

        {-# INLINE readVkMaxFrameAverageLightLevel #-}
        readVkMaxFrameAverageLightLevel p
          = peekByteOff p #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}

        {-# INLINE writeVkMaxFrameAverageLightLevel #-}
        writeVkMaxFrameAverageLightLevel p
          = pokeByteOff p #{offset VkHdrMetadataEXT, maxFrameAverageLightLevel}

instance Show VkHdrMetadataEXT where
        showsPrec d x
          = showString "VkHdrMetadataEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDisplayPrimaryRed = " .
                            showsPrec d (vkDisplayPrimaryRed x) .
                              showString ", " .
                                showString "vkDisplayPrimaryGreen = " .
                                  showsPrec d (vkDisplayPrimaryGreen x) .
                                    showString ", " .
                                      showString "vkDisplayPrimaryBlue = " .
                                        showsPrec d (vkDisplayPrimaryBlue x) .
                                          showString ", " .
                                            showString "vkWhitePoint = " .
                                              showsPrec d (vkWhitePoint x) .
                                                showString ", " .
                                                  showString "vkMaxLuminance = " .
                                                    showsPrec d (vkMaxLuminance x) .
                                                      showString ", " .
                                                        showString "vkMinLuminance = " .
                                                          showsPrec d (vkMinLuminance x) .
                                                            showString ", " .
                                                              showString "vkMaxContentLightLevel = "
                                                                .
                                                                showsPrec d
                                                                  (vkMaxContentLightLevel x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkMaxFrameAverageLightLevel = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkMaxFrameAverageLightLevel
                                                                           x)
                                                                        . showChar '}'

-- | Chromaticity coordinate
--
data VkXYColorEXT = VkXYColorEXT## ByteArray##

instance Eq VkXYColorEXT where
        (VkXYColorEXT## a) == (VkXYColorEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkXYColorEXT where
        (VkXYColorEXT## a) `compare` (VkXYColorEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkXYColorEXT where
        sizeOf ~_ = #{size VkXYColorEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkXYColorEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkXYColorEXT),
            I## a <- alignment (undefined :: VkXYColorEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3, VkXYColorEXT## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkXYColorEXT## ba)
          | I## n <- sizeOf (undefined :: VkXYColorEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkXYColorEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkXYColorEXT),
            I## a <- alignment (undefined :: VkXYColorEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkXYColorEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkXYColorEXT## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkXYColorEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkXYColorEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkXYColorEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkXYColorEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkX VkXYColorEXT where
        type VkXMType VkXYColorEXT = #{type float}

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXYColorEXT, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkXYColorEXT, x}

        {-# INLINE readVkX #-}
        readVkX p = peekByteOff p #{offset VkXYColorEXT, x}

        {-# INLINE writeVkX #-}
        writeVkX p = pokeByteOff p #{offset VkXYColorEXT, x}

instance {-# OVERLAPPING #-} HasVkY VkXYColorEXT where
        type VkYMType VkXYColorEXT = #{type float}

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXYColorEXT, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkXYColorEXT, y}

        {-# INLINE readVkY #-}
        readVkY p = peekByteOff p #{offset VkXYColorEXT, y}

        {-# INLINE writeVkY #-}
        writeVkY p = pokeByteOff p #{offset VkXYColorEXT, y}

instance Show VkXYColorEXT where
        showsPrec d x
          = showString "VkXYColorEXT {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " . showsPrec d (vkY x) . showChar '}'

-- | > void vkSetHdrMetadataEXT
--   >     ( VkDevice device
--   >     , uint32_t swapchainCount
--   >     , const VkSwapchainKHR* pSwapchains
--   >     , const VkHdrMetadataEXT* pMetadata
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkSetHdrMetadataEXT.html vkSetHdrMetadataEXT registry at www.khronos.org>
foreign import ccall unsafe "vkSetHdrMetadataEXT"
               vkSetHdrMetadataEXT ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ swapchainCount
                                  ->
                   Ptr VkSwapchainKHR -- ^ pSwapchains
                                      -> Ptr VkHdrMetadataEXT -- ^ pMetadata
                                                              -> IO ()

pattern VK_EXT_HDR_METADATA_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_HDR_METADATA_SPEC_VERSION = 1

type VK_EXT_HDR_METADATA_SPEC_VERSION = 1

pattern VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString

pattern VK_EXT_HDR_METADATA_EXTENSION_NAME <-
        (is_VK_EXT_HDR_METADATA_EXTENSION_NAME -> True)
  where VK_EXT_HDR_METADATA_EXTENSION_NAME
          = _VK_EXT_HDR_METADATA_EXTENSION_NAME

_VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_HDR_METADATA_EXTENSION_NAME #-}
_VK_EXT_HDR_METADATA_EXTENSION_NAME
  = Ptr "VK_EXT_hdr_metadata\NUL"##

is_VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_HDR_METADATA_EXTENSION_NAME #-}
is_VK_EXT_HDR_METADATA_EXTENSION_NAME
  = (_VK_EXT_HDR_METADATA_EXTENSION_NAME ==)

type VK_EXT_HDR_METADATA_EXTENSION_NAME = "VK_EXT_hdr_metadata"

pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT =
        VkStructureType 1000105000
