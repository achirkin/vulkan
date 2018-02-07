#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedFFITypes     #-}
{-# LANGUAGE ViewPatterns         #-}
module Graphics.Vulkan.Ext.VK_KHR_incremental_present
       (-- * Vulkan extension: @VK_KHR_incremental_present@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Ian Elliott ianelliott@google.com@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @85@
        --
        -- Required extensions: 'VK_KHR_swapchain'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain'.
        VkPresentRegionsKHR(..), VkPresentRegionKHR(..),
        VkRectLayerKHR(..), VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION,
        pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION,
        VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME,
        pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR)
       where
import           Data.Int
import           Data.Void                        (Void)
import           Data.Word
import           Foreign.C.String                 (CString)
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

data VkPresentRegionsKHR = VkPresentRegionsKHR## ByteArray##

instance Eq VkPresentRegionsKHR where
        (VkPresentRegionsKHR## a) == (VkPresentRegionsKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPresentRegionsKHR where
        (VkPresentRegionsKHR## a) `compare` (VkPresentRegionsKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPresentRegionsKHR where
        sizeOf ~_ = #{size VkPresentRegionsKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentRegionsKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkPresentRegionsKHR),
            I## a <- alignment (undefined :: VkPresentRegionsKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPresentRegionsKHR## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPresentRegionsKHR## ba)
          | I## n <- sizeOf (undefined :: VkPresentRegionsKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPresentRegionsKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkPresentRegionsKHR),
            I## a <- alignment (undefined :: VkPresentRegionsKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPresentRegionsKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPresentRegionsKHR## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkPresentRegionsKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPresentRegionsKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPresentRegionsKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPresentRegionsKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkPresentRegionsKHR where
        type VkSTypeMType VkPresentRegionsKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPresentRegionsKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPresentRegionsKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPresentRegionsKHR, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkPresentRegionsKHR where
        type VkPNextMType VkPresentRegionsKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPresentRegionsKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPresentRegionsKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPresentRegionsKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkSwapchainCount VkPresentRegionsKHR where
        type VkSwapchainCountMType VkPresentRegionsKHR = Word32

        {-# NOINLINE vkSwapchainCount #-}
        vkSwapchainCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, swapchainCount})

        {-# INLINE vkSwapchainCountByteOffset #-}
        vkSwapchainCountByteOffset ~_
          = #{offset VkPresentRegionsKHR, swapchainCount}

        {-# INLINE readVkSwapchainCount #-}
        readVkSwapchainCount p
          = peekByteOff p #{offset VkPresentRegionsKHR, swapchainCount}

        {-# INLINE writeVkSwapchainCount #-}
        writeVkSwapchainCount p
          = pokeByteOff p #{offset VkPresentRegionsKHR, swapchainCount}

instance {-# OVERLAPPING #-} HasVkPRegions VkPresentRegionsKHR
         where
        type VkPRegionsMType VkPresentRegionsKHR = Ptr VkPresentRegionKHR

        {-# NOINLINE vkPRegions #-}
        vkPRegions x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, pRegions})

        {-# INLINE vkPRegionsByteOffset #-}
        vkPRegionsByteOffset ~_
          = #{offset VkPresentRegionsKHR, pRegions}

        {-# INLINE readVkPRegions #-}
        readVkPRegions p
          = peekByteOff p #{offset VkPresentRegionsKHR, pRegions}

        {-# INLINE writeVkPRegions #-}
        writeVkPRegions p
          = pokeByteOff p #{offset VkPresentRegionsKHR, pRegions}

instance Show VkPresentRegionsKHR where
        showsPrec d x
          = showString "VkPresentRegionsKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSwapchainCount = " .
                            showsPrec d (vkSwapchainCount x) .
                              showString ", " .
                                showString "vkPRegions = " .
                                  showsPrec d (vkPRegions x) . showChar '}'

data VkPresentRegionKHR = VkPresentRegionKHR## ByteArray##

instance Eq VkPresentRegionKHR where
        (VkPresentRegionKHR## a) == (VkPresentRegionKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPresentRegionKHR where
        (VkPresentRegionKHR## a) `compare` (VkPresentRegionKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPresentRegionKHR where
        sizeOf ~_ = #{size VkPresentRegionKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentRegionKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkPresentRegionKHR),
            I## a <- alignment (undefined :: VkPresentRegionKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPresentRegionKHR## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPresentRegionKHR## ba)
          | I## n <- sizeOf (undefined :: VkPresentRegionKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPresentRegionKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkPresentRegionKHR),
            I## a <- alignment (undefined :: VkPresentRegionKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPresentRegionKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPresentRegionKHR## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkPresentRegionKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPresentRegionKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPresentRegionKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPresentRegionKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkRectangleCount VkPresentRegionKHR
         where
        type VkRectangleCountMType VkPresentRegionKHR = Word32

        {-# NOINLINE vkRectangleCount #-}
        vkRectangleCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionKHR, rectangleCount})

        {-# INLINE vkRectangleCountByteOffset #-}
        vkRectangleCountByteOffset ~_
          = #{offset VkPresentRegionKHR, rectangleCount}

        {-# INLINE readVkRectangleCount #-}
        readVkRectangleCount p
          = peekByteOff p #{offset VkPresentRegionKHR, rectangleCount}

        {-# INLINE writeVkRectangleCount #-}
        writeVkRectangleCount p
          = pokeByteOff p #{offset VkPresentRegionKHR, rectangleCount}

instance {-# OVERLAPPING #-} HasVkPRectangles VkPresentRegionKHR
         where
        type VkPRectanglesMType VkPresentRegionKHR = Ptr VkRectLayerKHR

        {-# NOINLINE vkPRectangles #-}
        vkPRectangles x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionKHR, pRectangles})

        {-# INLINE vkPRectanglesByteOffset #-}
        vkPRectanglesByteOffset ~_
          = #{offset VkPresentRegionKHR, pRectangles}

        {-# INLINE readVkPRectangles #-}
        readVkPRectangles p
          = peekByteOff p #{offset VkPresentRegionKHR, pRectangles}

        {-# INLINE writeVkPRectangles #-}
        writeVkPRectangles p
          = pokeByteOff p #{offset VkPresentRegionKHR, pRectangles}

instance Show VkPresentRegionKHR where
        showsPrec d x
          = showString "VkPresentRegionKHR {" .
              showString "vkRectangleCount = " .
                showsPrec d (vkRectangleCount x) .
                  showString ", " .
                    showString "vkPRectangles = " .
                      showsPrec d (vkPRectangles x) . showChar '}'

data VkRectLayerKHR = VkRectLayerKHR## ByteArray##

instance Eq VkRectLayerKHR where
        (VkRectLayerKHR## a) == (VkRectLayerKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkRectLayerKHR where
        (VkRectLayerKHR## a) `compare` (VkRectLayerKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkRectLayerKHR where
        sizeOf ~_ = #{size VkRectLayerKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkRectLayerKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkRectLayerKHR),
            I## a <- alignment (undefined :: VkRectLayerKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3, VkRectLayerKHR## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkRectLayerKHR## ba)
          | I## n <- sizeOf (undefined :: VkRectLayerKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkRectLayerKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkRectLayerKHR),
            I## a <- alignment (undefined :: VkRectLayerKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkRectLayerKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkRectLayerKHR## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkRectLayerKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkRectLayerKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkRectLayerKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkRectLayerKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkOffset VkRectLayerKHR where
        type VkOffsetMType VkRectLayerKHR = VkOffset2D

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRectLayerKHR, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkRectLayerKHR, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkRectLayerKHR, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkRectLayerKHR, offset}

instance {-# OVERLAPPING #-} HasVkExtent VkRectLayerKHR where
        type VkExtentMType VkRectLayerKHR = VkExtent2D

        {-# NOINLINE vkExtent #-}
        vkExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRectLayerKHR, extent})

        {-# INLINE vkExtentByteOffset #-}
        vkExtentByteOffset ~_
          = #{offset VkRectLayerKHR, extent}

        {-# INLINE readVkExtent #-}
        readVkExtent p
          = peekByteOff p #{offset VkRectLayerKHR, extent}

        {-# INLINE writeVkExtent #-}
        writeVkExtent p
          = pokeByteOff p #{offset VkRectLayerKHR, extent}

instance {-# OVERLAPPING #-} HasVkLayer VkRectLayerKHR where
        type VkLayerMType VkRectLayerKHR = Word32

        {-# NOINLINE vkLayer #-}
        vkLayer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRectLayerKHR, layer})

        {-# INLINE vkLayerByteOffset #-}
        vkLayerByteOffset ~_ = #{offset VkRectLayerKHR, layer}

        {-# INLINE readVkLayer #-}
        readVkLayer p
          = peekByteOff p #{offset VkRectLayerKHR, layer}

        {-# INLINE writeVkLayer #-}
        writeVkLayer p
          = pokeByteOff p #{offset VkRectLayerKHR, layer}

instance Show VkRectLayerKHR where
        showsPrec d x
          = showString "VkRectLayerKHR {" .
              showString "vkOffset = " .
                showsPrec d (vkOffset x) .
                  showString ", " .
                    showString "vkExtent = " .
                      showsPrec d (vkExtent x) .
                        showString ", " .
                          showString "vkLayer = " . showsPrec d (vkLayer x) . showChar '}'

pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1

type VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1

pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: CString

pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME <-
        (is_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME -> True)
  where VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
          = _VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME

_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME #-}
_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  = Ptr "VK_KHR_incremental_present\NUL"##

is_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME #-}
is_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  = (_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME ==)

type VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME =
     "VK_KHR_incremental_present"

pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR =
        VkStructureType 1000084000
