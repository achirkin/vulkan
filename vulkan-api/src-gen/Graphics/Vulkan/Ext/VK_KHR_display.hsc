#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_display
       (-- * Vulkan extension: @VK_KHR_display@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo,Norbert Nopper @FslNopper@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @3@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkDisplayPropertiesKHR(..), VkDisplayModeParametersKHR(..),
        VkDisplayModePropertiesKHR(..), VkDisplayModeCreateInfoKHR(..),
        VkDisplayPlaneCapabilitiesKHR(..), VkDisplayPlanePropertiesKHR(..),
        VkDisplaySurfaceCreateInfoKHR(..),
        vkGetPhysicalDeviceDisplayPropertiesKHR,
        vkGetPhysicalDeviceDisplayPlanePropertiesKHR,
        vkGetDisplayPlaneSupportedDisplaysKHR,
        vkGetDisplayModePropertiesKHR, vkCreateDisplayModeKHR,
        vkGetDisplayPlaneCapabilitiesKHR, vkCreateDisplayPlaneSurfaceKHR,
        VK_KHR_DISPLAY_SPEC_VERSION, pattern VK_KHR_DISPLAY_SPEC_VERSION,
        VK_KHR_DISPLAY_EXTENSION_NAME,
        pattern VK_KHR_DISPLAY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR,
        pattern VK_OBJECT_TYPE_DISPLAY_KHR,
        pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..),
                                                   VkExtent2D, VkOffset2D)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkDisplayPropertiesKHR = VkDisplayPropertiesKHR## ByteArray##

instance Eq VkDisplayPropertiesKHR where
        (VkDisplayPropertiesKHR## a) == (VkDisplayPropertiesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPropertiesKHR where
        (VkDisplayPropertiesKHR## a) `compare` (VkDisplayPropertiesKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDisplayPropertiesKHR where
        sizeOf ~_ = #{size VkDisplayPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDisplayPropertiesKHR),
            I## a <- alignment (undefined :: VkDisplayPropertiesKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDisplayPropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDisplayPropertiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkDisplayPropertiesKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDisplayPropertiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDisplayPropertiesKHR),
            I## a <- alignment (undefined :: VkDisplayPropertiesKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDisplayPropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDisplayPropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDisplayPropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDisplayPropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDisplayPropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDisplayPropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkDisplay VkDisplayPropertiesKHR
         where
        type VkDisplayMType VkDisplayPropertiesKHR = VkDisplayKHR

        {-# NOINLINE vkDisplay #-}
        vkDisplay x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, display})

        {-# INLINE vkDisplayByteOffset #-}
        vkDisplayByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, display}

        {-# INLINE readVkDisplay #-}
        readVkDisplay p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, display}

        {-# INLINE writeVkDisplay #-}
        writeVkDisplay p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, display}

instance {-# OVERLAPPING #-}
         HasVkDisplayName VkDisplayPropertiesKHR where
        type VkDisplayNameMType VkDisplayPropertiesKHR = CString

        {-# NOINLINE vkDisplayName #-}
        vkDisplayName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, displayName})

        {-# INLINE vkDisplayNameByteOffset #-}
        vkDisplayNameByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, displayName}

        {-# INLINE readVkDisplayName #-}
        readVkDisplayName p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, displayName}

        {-# INLINE writeVkDisplayName #-}
        writeVkDisplayName p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, displayName}

instance {-# OVERLAPPING #-}
         HasVkPhysicalDimensions VkDisplayPropertiesKHR where
        type VkPhysicalDimensionsMType VkDisplayPropertiesKHR = VkExtent2D

        {-# NOINLINE vkPhysicalDimensions #-}
        vkPhysicalDimensions x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, physicalDimensions})

        {-# INLINE vkPhysicalDimensionsByteOffset #-}
        vkPhysicalDimensionsByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, physicalDimensions}

        {-# INLINE readVkPhysicalDimensions #-}
        readVkPhysicalDimensions p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, physicalDimensions}

        {-# INLINE writeVkPhysicalDimensions #-}
        writeVkPhysicalDimensions p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, physicalDimensions}

instance {-# OVERLAPPING #-}
         HasVkPhysicalResolution VkDisplayPropertiesKHR where
        type VkPhysicalResolutionMType VkDisplayPropertiesKHR = VkExtent2D

        {-# NOINLINE vkPhysicalResolution #-}
        vkPhysicalResolution x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, physicalResolution})

        {-# INLINE vkPhysicalResolutionByteOffset #-}
        vkPhysicalResolutionByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, physicalResolution}

        {-# INLINE readVkPhysicalResolution #-}
        readVkPhysicalResolution p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, physicalResolution}

        {-# INLINE writeVkPhysicalResolution #-}
        writeVkPhysicalResolution p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, physicalResolution}

instance {-# OVERLAPPING #-}
         HasVkSupportedTransforms VkDisplayPropertiesKHR where
        type VkSupportedTransformsMType VkDisplayPropertiesKHR =
             VkSurfaceTransformFlagsKHR

        {-# NOINLINE vkSupportedTransforms #-}
        vkSupportedTransforms x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, supportedTransforms})

        {-# INLINE vkSupportedTransformsByteOffset #-}
        vkSupportedTransformsByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, supportedTransforms}

        {-# INLINE readVkSupportedTransforms #-}
        readVkSupportedTransforms p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, supportedTransforms}

        {-# INLINE writeVkSupportedTransforms #-}
        writeVkSupportedTransforms p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         HasVkPlaneReorderPossible VkDisplayPropertiesKHR where
        type VkPlaneReorderPossibleMType VkDisplayPropertiesKHR = VkBool32

        {-# NOINLINE vkPlaneReorderPossible #-}
        vkPlaneReorderPossible x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, planeReorderPossible})

        {-# INLINE vkPlaneReorderPossibleByteOffset #-}
        vkPlaneReorderPossibleByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, planeReorderPossible}

        {-# INLINE readVkPlaneReorderPossible #-}
        readVkPlaneReorderPossible p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, planeReorderPossible}

        {-# INLINE writeVkPlaneReorderPossible #-}
        writeVkPlaneReorderPossible p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, planeReorderPossible}

instance {-# OVERLAPPING #-}
         HasVkPersistentContent VkDisplayPropertiesKHR where
        type VkPersistentContentMType VkDisplayPropertiesKHR = VkBool32

        {-# NOINLINE vkPersistentContent #-}
        vkPersistentContent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, persistentContent})

        {-# INLINE vkPersistentContentByteOffset #-}
        vkPersistentContentByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, persistentContent}

        {-# INLINE readVkPersistentContent #-}
        readVkPersistentContent p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, persistentContent}

        {-# INLINE writeVkPersistentContent #-}
        writeVkPersistentContent p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, persistentContent}

instance Show VkDisplayPropertiesKHR where
        showsPrec d x
          = showString "VkDisplayPropertiesKHR {" .
              showString "vkDisplay = " .
                showsPrec d (vkDisplay x) .
                  showString ", " .
                    showString "vkDisplayName = " .
                      showsPrec d (vkDisplayName x) .
                        showString ", " .
                          showString "vkPhysicalDimensions = " .
                            showsPrec d (vkPhysicalDimensions x) .
                              showString ", " .
                                showString "vkPhysicalResolution = " .
                                  showsPrec d (vkPhysicalResolution x) .
                                    showString ", " .
                                      showString "vkSupportedTransforms = " .
                                        showsPrec d (vkSupportedTransforms x) .
                                          showString ", " .
                                            showString "vkPlaneReorderPossible = " .
                                              showsPrec d (vkPlaneReorderPossible x) .
                                                showString ", " .
                                                  showString "vkPersistentContent = " .
                                                    showsPrec d (vkPersistentContent x) .
                                                      showChar '}'

data VkDisplayModeParametersKHR = VkDisplayModeParametersKHR## ByteArray##

instance Eq VkDisplayModeParametersKHR where
        (VkDisplayModeParametersKHR## a) == (VkDisplayModeParametersKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDisplayModeParametersKHR where
        (VkDisplayModeParametersKHR## a) `compare`
          (VkDisplayModeParametersKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDisplayModeParametersKHR where
        sizeOf ~_ = #{size VkDisplayModeParametersKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayModeParametersKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDisplayModeParametersKHR),
            I## a <- alignment (undefined :: VkDisplayModeParametersKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDisplayModeParametersKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDisplayModeParametersKHR## ba)
          | I## n <- sizeOf (undefined :: VkDisplayModeParametersKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDisplayModeParametersKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDisplayModeParametersKHR),
            I## a <- alignment (undefined :: VkDisplayModeParametersKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDisplayModeParametersKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDisplayModeParametersKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDisplayModeParametersKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDisplayModeParametersKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDisplayModeParametersKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDisplayModeParametersKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkVisibleRegion VkDisplayModeParametersKHR where
        type VkVisibleRegionMType VkDisplayModeParametersKHR = VkExtent2D

        {-# NOINLINE vkVisibleRegion #-}
        vkVisibleRegion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeParametersKHR, visibleRegion})

        {-# INLINE vkVisibleRegionByteOffset #-}
        vkVisibleRegionByteOffset ~_
          = #{offset VkDisplayModeParametersKHR, visibleRegion}

        {-# INLINE readVkVisibleRegion #-}
        readVkVisibleRegion p
          = peekByteOff p #{offset VkDisplayModeParametersKHR, visibleRegion}

        {-# INLINE writeVkVisibleRegion #-}
        writeVkVisibleRegion p
          = pokeByteOff p #{offset VkDisplayModeParametersKHR, visibleRegion}

instance {-# OVERLAPPING #-}
         HasVkRefreshRate VkDisplayModeParametersKHR where
        type VkRefreshRateMType VkDisplayModeParametersKHR = Word32

        {-# NOINLINE vkRefreshRate #-}
        vkRefreshRate x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeParametersKHR, refreshRate})

        {-# INLINE vkRefreshRateByteOffset #-}
        vkRefreshRateByteOffset ~_
          = #{offset VkDisplayModeParametersKHR, refreshRate}

        {-# INLINE readVkRefreshRate #-}
        readVkRefreshRate p
          = peekByteOff p #{offset VkDisplayModeParametersKHR, refreshRate}

        {-# INLINE writeVkRefreshRate #-}
        writeVkRefreshRate p
          = pokeByteOff p #{offset VkDisplayModeParametersKHR, refreshRate}

instance Show VkDisplayModeParametersKHR where
        showsPrec d x
          = showString "VkDisplayModeParametersKHR {" .
              showString "vkVisibleRegion = " .
                showsPrec d (vkVisibleRegion x) .
                  showString ", " .
                    showString "vkRefreshRate = " .
                      showsPrec d (vkRefreshRate x) . showChar '}'

data VkDisplayModePropertiesKHR = VkDisplayModePropertiesKHR## ByteArray##

instance Eq VkDisplayModePropertiesKHR where
        (VkDisplayModePropertiesKHR## a) == (VkDisplayModePropertiesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDisplayModePropertiesKHR where
        (VkDisplayModePropertiesKHR## a) `compare`
          (VkDisplayModePropertiesKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDisplayModePropertiesKHR where
        sizeOf ~_ = #{size VkDisplayModePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayModePropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDisplayModePropertiesKHR),
            I## a <- alignment (undefined :: VkDisplayModePropertiesKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDisplayModePropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDisplayModePropertiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkDisplayModePropertiesKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDisplayModePropertiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDisplayModePropertiesKHR),
            I## a <- alignment (undefined :: VkDisplayModePropertiesKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDisplayModePropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDisplayModePropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDisplayModePropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDisplayModePropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDisplayModePropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDisplayModePropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkDisplayMode VkDisplayModePropertiesKHR where
        type VkDisplayModeMType VkDisplayModePropertiesKHR =
             VkDisplayModeKHR

        {-# NOINLINE vkDisplayMode #-}
        vkDisplayMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModePropertiesKHR, displayMode})

        {-# INLINE vkDisplayModeByteOffset #-}
        vkDisplayModeByteOffset ~_
          = #{offset VkDisplayModePropertiesKHR, displayMode}

        {-# INLINE readVkDisplayMode #-}
        readVkDisplayMode p
          = peekByteOff p #{offset VkDisplayModePropertiesKHR, displayMode}

        {-# INLINE writeVkDisplayMode #-}
        writeVkDisplayMode p
          = pokeByteOff p #{offset VkDisplayModePropertiesKHR, displayMode}

instance {-# OVERLAPPING #-}
         HasVkParameters VkDisplayModePropertiesKHR where
        type VkParametersMType VkDisplayModePropertiesKHR =
             VkDisplayModeParametersKHR

        {-# NOINLINE vkParameters #-}
        vkParameters x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModePropertiesKHR, parameters})

        {-# INLINE vkParametersByteOffset #-}
        vkParametersByteOffset ~_
          = #{offset VkDisplayModePropertiesKHR, parameters}

        {-# INLINE readVkParameters #-}
        readVkParameters p
          = peekByteOff p #{offset VkDisplayModePropertiesKHR, parameters}

        {-# INLINE writeVkParameters #-}
        writeVkParameters p
          = pokeByteOff p #{offset VkDisplayModePropertiesKHR, parameters}

instance Show VkDisplayModePropertiesKHR where
        showsPrec d x
          = showString "VkDisplayModePropertiesKHR {" .
              showString "vkDisplayMode = " .
                showsPrec d (vkDisplayMode x) .
                  showString ", " .
                    showString "vkParameters = " .
                      showsPrec d (vkParameters x) . showChar '}'

data VkDisplayModeCreateInfoKHR = VkDisplayModeCreateInfoKHR## ByteArray##

instance Eq VkDisplayModeCreateInfoKHR where
        (VkDisplayModeCreateInfoKHR## a) == (VkDisplayModeCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDisplayModeCreateInfoKHR where
        (VkDisplayModeCreateInfoKHR## a) `compare`
          (VkDisplayModeCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDisplayModeCreateInfoKHR where
        sizeOf ~_ = #{size VkDisplayModeCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayModeCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDisplayModeCreateInfoKHR),
            I## a <- alignment (undefined :: VkDisplayModeCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDisplayModeCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDisplayModeCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkDisplayModeCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDisplayModeCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDisplayModeCreateInfoKHR),
            I## a <- alignment (undefined :: VkDisplayModeCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDisplayModeCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDisplayModeCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDisplayModeCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDisplayModeCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDisplayModeCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDisplayModeCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkDisplayModeCreateInfoKHR
         where
        type VkSTypeMType VkDisplayModeCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkDisplayModeCreateInfoKHR
         where
        type VkPNextMType VkDisplayModeCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasVkFlags VkDisplayModeCreateInfoKHR
         where
        type VkFlagsMType VkDisplayModeCreateInfoKHR =
             VkDisplayModeCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasVkParameters VkDisplayModeCreateInfoKHR where
        type VkParametersMType VkDisplayModeCreateInfoKHR =
             VkDisplayModeParametersKHR

        {-# NOINLINE vkParameters #-}
        vkParameters x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, parameters})

        {-# INLINE vkParametersByteOffset #-}
        vkParametersByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, parameters}

        {-# INLINE readVkParameters #-}
        readVkParameters p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, parameters}

        {-# INLINE writeVkParameters #-}
        writeVkParameters p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, parameters}

instance Show VkDisplayModeCreateInfoKHR where
        showsPrec d x
          = showString "VkDisplayModeCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkParameters = " .
                                  showsPrec d (vkParameters x) . showChar '}'

data VkDisplayPlaneCapabilitiesKHR = VkDisplayPlaneCapabilitiesKHR## ByteArray##

instance Eq VkDisplayPlaneCapabilitiesKHR where
        (VkDisplayPlaneCapabilitiesKHR## a) ==
          (VkDisplayPlaneCapabilitiesKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPlaneCapabilitiesKHR where
        (VkDisplayPlaneCapabilitiesKHR## a) `compare`
          (VkDisplayPlaneCapabilitiesKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDisplayPlaneCapabilitiesKHR where
        sizeOf ~_ = #{size VkDisplayPlaneCapabilitiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDisplayPlaneCapabilitiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDisplayPlaneCapabilitiesKHR),
            I## a <- alignment (undefined :: VkDisplayPlaneCapabilitiesKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDisplayPlaneCapabilitiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDisplayPlaneCapabilitiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkDisplayPlaneCapabilitiesKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDisplayPlaneCapabilitiesKHR),
            I## a <- alignment (undefined :: VkDisplayPlaneCapabilitiesKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDisplayPlaneCapabilitiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDisplayPlaneCapabilitiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDisplayPlaneCapabilitiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDisplayPlaneCapabilitiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDisplayPlaneCapabilitiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDisplayPlaneCapabilitiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSupportedAlpha VkDisplayPlaneCapabilitiesKHR where
        type VkSupportedAlphaMType VkDisplayPlaneCapabilitiesKHR =
             VkDisplayPlaneAlphaFlagsKHR

        {-# NOINLINE vkSupportedAlpha #-}
        vkSupportedAlpha x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha})

        {-# INLINE vkSupportedAlphaByteOffset #-}
        vkSupportedAlphaByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

        {-# INLINE readVkSupportedAlpha #-}
        readVkSupportedAlpha p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

        {-# INLINE writeVkSupportedAlpha #-}
        writeVkSupportedAlpha p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

instance {-# OVERLAPPING #-}
         HasVkMinSrcPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMinSrcPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMinSrcPosition #-}
        vkMinSrcPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition})

        {-# INLINE vkMinSrcPositionByteOffset #-}
        vkMinSrcPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

        {-# INLINE readVkMinSrcPosition #-}
        readVkMinSrcPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

        {-# INLINE writeVkMinSrcPosition #-}
        writeVkMinSrcPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

instance {-# OVERLAPPING #-}
         HasVkMaxSrcPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMaxSrcPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMaxSrcPosition #-}
        vkMaxSrcPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition})

        {-# INLINE vkMaxSrcPositionByteOffset #-}
        vkMaxSrcPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

        {-# INLINE readVkMaxSrcPosition #-}
        readVkMaxSrcPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

        {-# INLINE writeVkMaxSrcPosition #-}
        writeVkMaxSrcPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

instance {-# OVERLAPPING #-}
         HasVkMinSrcExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMinSrcExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMinSrcExtent #-}
        vkMinSrcExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent})

        {-# INLINE vkMinSrcExtentByteOffset #-}
        vkMinSrcExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

        {-# INLINE readVkMinSrcExtent #-}
        readVkMinSrcExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

        {-# INLINE writeVkMinSrcExtent #-}
        writeVkMinSrcExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

instance {-# OVERLAPPING #-}
         HasVkMaxSrcExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMaxSrcExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMaxSrcExtent #-}
        vkMaxSrcExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent})

        {-# INLINE vkMaxSrcExtentByteOffset #-}
        vkMaxSrcExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

        {-# INLINE readVkMaxSrcExtent #-}
        readVkMaxSrcExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

        {-# INLINE writeVkMaxSrcExtent #-}
        writeVkMaxSrcExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

instance {-# OVERLAPPING #-}
         HasVkMinDstPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMinDstPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMinDstPosition #-}
        vkMinDstPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition})

        {-# INLINE vkMinDstPositionByteOffset #-}
        vkMinDstPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

        {-# INLINE readVkMinDstPosition #-}
        readVkMinDstPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

        {-# INLINE writeVkMinDstPosition #-}
        writeVkMinDstPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

instance {-# OVERLAPPING #-}
         HasVkMaxDstPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMaxDstPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMaxDstPosition #-}
        vkMaxDstPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition})

        {-# INLINE vkMaxDstPositionByteOffset #-}
        vkMaxDstPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

        {-# INLINE readVkMaxDstPosition #-}
        readVkMaxDstPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

        {-# INLINE writeVkMaxDstPosition #-}
        writeVkMaxDstPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

instance {-# OVERLAPPING #-}
         HasVkMinDstExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMinDstExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMinDstExtent #-}
        vkMinDstExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent})

        {-# INLINE vkMinDstExtentByteOffset #-}
        vkMinDstExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

        {-# INLINE readVkMinDstExtent #-}
        readVkMinDstExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

        {-# INLINE writeVkMinDstExtent #-}
        writeVkMinDstExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

instance {-# OVERLAPPING #-}
         HasVkMaxDstExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMaxDstExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMaxDstExtent #-}
        vkMaxDstExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent})

        {-# INLINE vkMaxDstExtentByteOffset #-}
        vkMaxDstExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

        {-# INLINE readVkMaxDstExtent #-}
        readVkMaxDstExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

        {-# INLINE writeVkMaxDstExtent #-}
        writeVkMaxDstExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

instance Show VkDisplayPlaneCapabilitiesKHR where
        showsPrec d x
          = showString "VkDisplayPlaneCapabilitiesKHR {" .
              showString "vkSupportedAlpha = " .
                showsPrec d (vkSupportedAlpha x) .
                  showString ", " .
                    showString "vkMinSrcPosition = " .
                      showsPrec d (vkMinSrcPosition x) .
                        showString ", " .
                          showString "vkMaxSrcPosition = " .
                            showsPrec d (vkMaxSrcPosition x) .
                              showString ", " .
                                showString "vkMinSrcExtent = " .
                                  showsPrec d (vkMinSrcExtent x) .
                                    showString ", " .
                                      showString "vkMaxSrcExtent = " .
                                        showsPrec d (vkMaxSrcExtent x) .
                                          showString ", " .
                                            showString "vkMinDstPosition = " .
                                              showsPrec d (vkMinDstPosition x) .
                                                showString ", " .
                                                  showString "vkMaxDstPosition = " .
                                                    showsPrec d (vkMaxDstPosition x) .
                                                      showString ", " .
                                                        showString "vkMinDstExtent = " .
                                                          showsPrec d (vkMinDstExtent x) .
                                                            showString ", " .
                                                              showString "vkMaxDstExtent = " .
                                                                showsPrec d (vkMaxDstExtent x) .
                                                                  showChar '}'

data VkDisplayPlanePropertiesKHR = VkDisplayPlanePropertiesKHR## ByteArray##

instance Eq VkDisplayPlanePropertiesKHR where
        (VkDisplayPlanePropertiesKHR## a) ==
          (VkDisplayPlanePropertiesKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPlanePropertiesKHR where
        (VkDisplayPlanePropertiesKHR## a) `compare`
          (VkDisplayPlanePropertiesKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDisplayPlanePropertiesKHR where
        sizeOf ~_ = #{size VkDisplayPlanePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPlanePropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDisplayPlanePropertiesKHR),
            I## a <- alignment (undefined :: VkDisplayPlanePropertiesKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDisplayPlanePropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDisplayPlanePropertiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkDisplayPlanePropertiesKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDisplayPlanePropertiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDisplayPlanePropertiesKHR),
            I## a <- alignment (undefined :: VkDisplayPlanePropertiesKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDisplayPlanePropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDisplayPlanePropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDisplayPlanePropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDisplayPlanePropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDisplayPlanePropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDisplayPlanePropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkCurrentDisplay VkDisplayPlanePropertiesKHR where
        type VkCurrentDisplayMType VkDisplayPlanePropertiesKHR =
             VkDisplayKHR

        {-# NOINLINE vkCurrentDisplay #-}
        vkCurrentDisplay x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlanePropertiesKHR, currentDisplay})

        {-# INLINE vkCurrentDisplayByteOffset #-}
        vkCurrentDisplayByteOffset ~_
          = #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

        {-# INLINE readVkCurrentDisplay #-}
        readVkCurrentDisplay p
          = peekByteOff p #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

        {-# INLINE writeVkCurrentDisplay #-}
        writeVkCurrentDisplay p
          = pokeByteOff p #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

instance {-# OVERLAPPING #-}
         HasVkCurrentStackIndex VkDisplayPlanePropertiesKHR where
        type VkCurrentStackIndexMType VkDisplayPlanePropertiesKHR = Word32

        {-# NOINLINE vkCurrentStackIndex #-}
        vkCurrentStackIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlanePropertiesKHR, currentStackIndex})

        {-# INLINE vkCurrentStackIndexByteOffset #-}
        vkCurrentStackIndexByteOffset ~_
          = #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

        {-# INLINE readVkCurrentStackIndex #-}
        readVkCurrentStackIndex p
          = peekByteOff p #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

        {-# INLINE writeVkCurrentStackIndex #-}
        writeVkCurrentStackIndex p
          = pokeByteOff p #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

instance Show VkDisplayPlanePropertiesKHR where
        showsPrec d x
          = showString "VkDisplayPlanePropertiesKHR {" .
              showString "vkCurrentDisplay = " .
                showsPrec d (vkCurrentDisplay x) .
                  showString ", " .
                    showString "vkCurrentStackIndex = " .
                      showsPrec d (vkCurrentStackIndex x) . showChar '}'

data VkDisplaySurfaceCreateInfoKHR = VkDisplaySurfaceCreateInfoKHR## ByteArray##

instance Eq VkDisplaySurfaceCreateInfoKHR where
        (VkDisplaySurfaceCreateInfoKHR## a) ==
          (VkDisplaySurfaceCreateInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDisplaySurfaceCreateInfoKHR where
        (VkDisplaySurfaceCreateInfoKHR## a) `compare`
          (VkDisplaySurfaceCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDisplaySurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkDisplaySurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDisplaySurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDisplaySurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkDisplaySurfaceCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDisplaySurfaceCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDisplaySurfaceCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkDisplaySurfaceCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDisplaySurfaceCreateInfoKHR),
            I## a <- alignment (undefined :: VkDisplaySurfaceCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDisplaySurfaceCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDisplaySurfaceCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDisplaySurfaceCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDisplaySurfaceCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDisplaySurfaceCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDisplaySurfaceCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDisplaySurfaceCreateInfoKHR where
        type VkSTypeMType VkDisplaySurfaceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkDisplaySurfaceCreateInfoKHR where
        type VkPNextMType VkDisplaySurfaceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkFlags VkDisplaySurfaceCreateInfoKHR where
        type VkFlagsMType VkDisplaySurfaceCreateInfoKHR =
             VkDisplaySurfaceCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasVkDisplayMode VkDisplaySurfaceCreateInfoKHR where
        type VkDisplayModeMType VkDisplaySurfaceCreateInfoKHR =
             VkDisplayModeKHR

        {-# NOINLINE vkDisplayMode #-}
        vkDisplayMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, displayMode})

        {-# INLINE vkDisplayModeByteOffset #-}
        vkDisplayModeByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

        {-# INLINE readVkDisplayMode #-}
        readVkDisplayMode p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

        {-# INLINE writeVkDisplayMode #-}
        writeVkDisplayMode p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

instance {-# OVERLAPPING #-}
         HasVkPlaneIndex VkDisplaySurfaceCreateInfoKHR where
        type VkPlaneIndexMType VkDisplaySurfaceCreateInfoKHR = Word32

        {-# NOINLINE vkPlaneIndex #-}
        vkPlaneIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex})

        {-# INLINE vkPlaneIndexByteOffset #-}
        vkPlaneIndexByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

        {-# INLINE readVkPlaneIndex #-}
        readVkPlaneIndex p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

        {-# INLINE writeVkPlaneIndex #-}
        writeVkPlaneIndex p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

instance {-# OVERLAPPING #-}
         HasVkPlaneStackIndex VkDisplaySurfaceCreateInfoKHR where
        type VkPlaneStackIndexMType VkDisplaySurfaceCreateInfoKHR = Word32

        {-# NOINLINE vkPlaneStackIndex #-}
        vkPlaneStackIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex})

        {-# INLINE vkPlaneStackIndexByteOffset #-}
        vkPlaneStackIndexByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

        {-# INLINE readVkPlaneStackIndex #-}
        readVkPlaneStackIndex p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

        {-# INLINE writeVkPlaneStackIndex #-}
        writeVkPlaneStackIndex p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

instance {-# OVERLAPPING #-}
         HasVkTransform VkDisplaySurfaceCreateInfoKHR where
        type VkTransformMType VkDisplaySurfaceCreateInfoKHR =
             VkSurfaceTransformFlagBitsKHR

        {-# NOINLINE vkTransform #-}
        vkTransform x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, transform})

        {-# INLINE vkTransformByteOffset #-}
        vkTransformByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, transform}

        {-# INLINE readVkTransform #-}
        readVkTransform p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, transform}

        {-# INLINE writeVkTransform #-}
        writeVkTransform p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, transform}

instance {-# OVERLAPPING #-}
         HasVkGlobalAlpha VkDisplaySurfaceCreateInfoKHR where
        type VkGlobalAlphaMType VkDisplaySurfaceCreateInfoKHR =
             #{type float}

        {-# NOINLINE vkGlobalAlpha #-}
        vkGlobalAlpha x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha})

        {-# INLINE vkGlobalAlphaByteOffset #-}
        vkGlobalAlphaByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

        {-# INLINE readVkGlobalAlpha #-}
        readVkGlobalAlpha p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

        {-# INLINE writeVkGlobalAlpha #-}
        writeVkGlobalAlpha p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

instance {-# OVERLAPPING #-}
         HasVkAlphaMode VkDisplaySurfaceCreateInfoKHR where
        type VkAlphaModeMType VkDisplaySurfaceCreateInfoKHR =
             VkDisplayPlaneAlphaFlagBitsKHR

        {-# NOINLINE vkAlphaMode #-}
        vkAlphaMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode})

        {-# INLINE vkAlphaModeByteOffset #-}
        vkAlphaModeByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

        {-# INLINE readVkAlphaMode #-}
        readVkAlphaMode p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

        {-# INLINE writeVkAlphaMode #-}
        writeVkAlphaMode p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

instance {-# OVERLAPPING #-}
         HasVkImageExtent VkDisplaySurfaceCreateInfoKHR where
        type VkImageExtentMType VkDisplaySurfaceCreateInfoKHR = VkExtent2D

        {-# NOINLINE vkImageExtent #-}
        vkImageExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent})

        {-# INLINE vkImageExtentByteOffset #-}
        vkImageExtentByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

        {-# INLINE readVkImageExtent #-}
        readVkImageExtent p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

        {-# INLINE writeVkImageExtent #-}
        writeVkImageExtent p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

instance Show VkDisplaySurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkDisplaySurfaceCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDisplayMode = " .
                                  showsPrec d (vkDisplayMode x) .
                                    showString ", " .
                                      showString "vkPlaneIndex = " .
                                        showsPrec d (vkPlaneIndex x) .
                                          showString ", " .
                                            showString "vkPlaneStackIndex = " .
                                              showsPrec d (vkPlaneStackIndex x) .
                                                showString ", " .
                                                  showString "vkTransform = " .
                                                    showsPrec d (vkTransform x) .
                                                      showString ", " .
                                                        showString "vkGlobalAlpha = " .
                                                          showsPrec d (vkGlobalAlpha x) .
                                                            showString ", " .
                                                              showString "vkAlphaMode = " .
                                                                showsPrec d (vkAlphaMode x) .
                                                                  showString ", " .
                                                                    showString "vkImageExtent = " .
                                                                      showsPrec d (vkImageExtent x)
                                                                        . showChar '}'

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceDisplayPropertiesKHR.html vkGetPhysicalDeviceDisplayPropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceDisplayPropertiesKHR"
               vkGetPhysicalDeviceDisplayPropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkDisplayPropertiesKHR -- ^ pProperties
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPlanePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPlanePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceDisplayPlanePropertiesKHR.html vkGetPhysicalDeviceDisplayPlanePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
               vkGetPhysicalDeviceDisplayPlanePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkDisplayPlanePropertiesKHR -- ^ pProperties
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneSupportedDisplaysKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t planeIndex
--   >     , uint32_t* pDisplayCount
--   >     , VkDisplayKHR* pDisplays
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDisplayPlaneSupportedDisplaysKHR.html vkGetDisplayPlaneSupportedDisplaysKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayPlaneSupportedDisplaysKHR"
               vkGetDisplayPlaneSupportedDisplaysKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ planeIndex
                        -> Ptr Word32 -- ^ pDisplayCount
                                      -> Ptr VkDisplayKHR -- ^ pDisplays
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayModePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayModePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDisplayModePropertiesKHR.html vkGetDisplayModePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayModePropertiesKHR"
               vkGetDisplayModePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkDisplayModePropertiesKHR -- ^ pProperties
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkCreateDisplayModeKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , const VkDisplayModeCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDisplayModeKHR* pMode
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDisplayModeKHR.html vkCreateDisplayModeKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDisplayModeKHR"
               vkCreateDisplayModeKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr VkDisplayModeCreateInfoKHR -- ^ pCreateInfo
                                                  ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkDisplayModeKHR -- ^ pMode
                                                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneCapabilitiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayModeKHR mode
--   >     , uint32_t planeIndex
--   >     , VkDisplayPlaneCapabilitiesKHR* pCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDisplayPlaneCapabilitiesKHR.html vkGetDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayPlaneCapabilitiesKHR"
               vkGetDisplayPlaneCapabilitiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayModeKHR -- ^ mode
                                  ->
                   Word32 -- ^ planeIndex
                          -> Ptr VkDisplayPlaneCapabilitiesKHR -- ^ pCapabilities
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDisplayPlaneSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkDisplaySurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDisplayPlaneSurfaceKHR.html vkCreateDisplayPlaneSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDisplayPlaneSurfaceKHR"
               vkCreateDisplayPlaneSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkDisplaySurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

pattern VK_KHR_DISPLAY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_DISPLAY_SPEC_VERSION = 21

type VK_KHR_DISPLAY_SPEC_VERSION = 21

pattern VK_KHR_DISPLAY_EXTENSION_NAME :: CString

pattern VK_KHR_DISPLAY_EXTENSION_NAME <-
        (is_VK_KHR_DISPLAY_EXTENSION_NAME -> True)
  where VK_KHR_DISPLAY_EXTENSION_NAME
          = _VK_KHR_DISPLAY_EXTENSION_NAME

_VK_KHR_DISPLAY_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_DISPLAY_EXTENSION_NAME #-}
_VK_KHR_DISPLAY_EXTENSION_NAME = Ptr "VK_KHR_display\NUL"##

is_VK_KHR_DISPLAY_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_DISPLAY_EXTENSION_NAME #-}
is_VK_KHR_DISPLAY_EXTENSION_NAME
  = (_VK_KHR_DISPLAY_EXTENSION_NAME ==)

type VK_KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"

pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR =
        VkStructureType 1000002000

pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000002001

-- | VkDisplayKHR
pattern VK_OBJECT_TYPE_DISPLAY_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_DISPLAY_KHR = VkObjectType 1000002000

-- | VkDisplayModeKHR
pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR = VkObjectType 1000002001
