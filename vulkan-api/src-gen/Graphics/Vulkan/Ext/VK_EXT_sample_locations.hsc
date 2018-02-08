#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_sample_locations
       (-- * Vulkan extension: @VK_EXT_sample_locations@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @aqnuep@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @144@
        VkSampleLocationEXT(..), VkSampleLocationsInfoEXT(..),
        VkAttachmentSampleLocationsEXT(..),
        VkSubpassSampleLocationsEXT(..),
        VkRenderPassSampleLocationsBeginInfoEXT(..),
        VkPipelineSampleLocationsStateCreateInfoEXT(..),
        VkPhysicalDeviceSampleLocationsPropertiesEXT(..),
        VkMultisamplePropertiesEXT(..), vkCmdSetSampleLocationsEXT,
        vkGetPhysicalDeviceMultisamplePropertiesEXT,
        VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION,
        pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION,
        VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME,
        pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME,
        pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT,
        pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT,
        pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkExtent2D)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkSampleLocationEXT {
--   >     float                            x;
--   >     float                            y;
--   > } VkSampleLocationEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSampleLocationEXT.html VkSampleLocationEXT registry at www.khronos.org>
data VkSampleLocationEXT = VkSampleLocationEXT## ByteArray##

instance Eq VkSampleLocationEXT where
        (VkSampleLocationEXT## a) == (VkSampleLocationEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSampleLocationEXT where
        (VkSampleLocationEXT## a) `compare` (VkSampleLocationEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSampleLocationEXT where
        sizeOf ~_ = #{size VkSampleLocationEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSampleLocationEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkSampleLocationEXT),
            I## a <- alignment (undefined :: VkSampleLocationEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSampleLocationEXT## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSampleLocationEXT## ba)
          | I## n <- sizeOf (undefined :: VkSampleLocationEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSampleLocationEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkSampleLocationEXT),
            I## a <- alignment (undefined :: VkSampleLocationEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSampleLocationEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSampleLocationEXT## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkSampleLocationEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSampleLocationEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSampleLocationEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSampleLocationEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkX VkSampleLocationEXT where
        type VkXMType VkSampleLocationEXT = #{type float}

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationEXT, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkSampleLocationEXT, x}

        {-# INLINE readVkX #-}
        readVkX p
          = peekByteOff p #{offset VkSampleLocationEXT, x}

        {-# INLINE writeVkX #-}
        writeVkX p
          = pokeByteOff p #{offset VkSampleLocationEXT, x}

instance {-# OVERLAPPING #-} HasVkY VkSampleLocationEXT where
        type VkYMType VkSampleLocationEXT = #{type float}

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationEXT, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkSampleLocationEXT, y}

        {-# INLINE readVkY #-}
        readVkY p
          = peekByteOff p #{offset VkSampleLocationEXT, y}

        {-# INLINE writeVkY #-}
        writeVkY p
          = pokeByteOff p #{offset VkSampleLocationEXT, y}

instance Show VkSampleLocationEXT where
        showsPrec d x
          = showString "VkSampleLocationEXT {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " . showsPrec d (vkY x) . showChar '}'

-- | > typedef struct VkSampleLocationsInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSampleCountFlagBits            sampleLocationsPerPixel;
--   >     VkExtent2D                       sampleLocationGridSize;
--   >     uint32_t                         sampleLocationsCount;
--   >     const VkSampleLocationEXT* pSampleLocations;
--   > } VkSampleLocationsInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSampleLocationsInfoEXT.html VkSampleLocationsInfoEXT registry at www.khronos.org>
data VkSampleLocationsInfoEXT = VkSampleLocationsInfoEXT## ByteArray##

instance Eq VkSampleLocationsInfoEXT where
        (VkSampleLocationsInfoEXT## a) == (VkSampleLocationsInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSampleLocationsInfoEXT where
        (VkSampleLocationsInfoEXT## a) `compare`
          (VkSampleLocationsInfoEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSampleLocationsInfoEXT where
        sizeOf ~_ = #{size VkSampleLocationsInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSampleLocationsInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkSampleLocationsInfoEXT),
            I## a <- alignment (undefined :: VkSampleLocationsInfoEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSampleLocationsInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSampleLocationsInfoEXT## ba)
          | I## n <- sizeOf (undefined :: VkSampleLocationsInfoEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSampleLocationsInfoEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkSampleLocationsInfoEXT),
            I## a <- alignment (undefined :: VkSampleLocationsInfoEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSampleLocationsInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSampleLocationsInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkSampleLocationsInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSampleLocationsInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSampleLocationsInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSampleLocationsInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkSampleLocationsInfoEXT
         where
        type VkSTypeMType VkSampleLocationsInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkSampleLocationsInfoEXT
         where
        type VkPNextMType VkSampleLocationsInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsPerPixel VkSampleLocationsInfoEXT where
        type VkSampleLocationsPerPixelMType VkSampleLocationsInfoEXT =
             VkSampleCountFlagBits

        {-# NOINLINE vkSampleLocationsPerPixel #-}
        vkSampleLocationsPerPixel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel})

        {-# INLINE vkSampleLocationsPerPixelByteOffset #-}
        vkSampleLocationsPerPixelByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}

        {-# INLINE readVkSampleLocationsPerPixel #-}
        readVkSampleLocationsPerPixel p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}

        {-# INLINE writeVkSampleLocationsPerPixel #-}
        writeVkSampleLocationsPerPixel p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsPerPixel}

instance {-# OVERLAPPING #-}
         HasVkSampleLocationGridSize VkSampleLocationsInfoEXT where
        type VkSampleLocationGridSizeMType VkSampleLocationsInfoEXT =
             VkExtent2D

        {-# NOINLINE vkSampleLocationGridSize #-}
        vkSampleLocationGridSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize})

        {-# INLINE vkSampleLocationGridSizeByteOffset #-}
        vkSampleLocationGridSizeByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}

        {-# INLINE readVkSampleLocationGridSize #-}
        readVkSampleLocationGridSize p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}

        {-# INLINE writeVkSampleLocationGridSize #-}
        writeVkSampleLocationGridSize p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationGridSize}

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsCount VkSampleLocationsInfoEXT where
        type VkSampleLocationsCountMType VkSampleLocationsInfoEXT = Word32

        {-# NOINLINE vkSampleLocationsCount #-}
        vkSampleLocationsCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, sampleLocationsCount})

        {-# INLINE vkSampleLocationsCountByteOffset #-}
        vkSampleLocationsCountByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}

        {-# INLINE readVkSampleLocationsCount #-}
        readVkSampleLocationsCount p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}

        {-# INLINE writeVkSampleLocationsCount #-}
        writeVkSampleLocationsCount p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, sampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasVkPSampleLocations VkSampleLocationsInfoEXT where
        type VkPSampleLocationsMType VkSampleLocationsInfoEXT =
             Ptr VkSampleLocationEXT

        {-# NOINLINE vkPSampleLocations #-}
        vkPSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSampleLocationsInfoEXT, pSampleLocations})

        {-# INLINE vkPSampleLocationsByteOffset #-}
        vkPSampleLocationsByteOffset ~_
          = #{offset VkSampleLocationsInfoEXT, pSampleLocations}

        {-# INLINE readVkPSampleLocations #-}
        readVkPSampleLocations p
          = peekByteOff p #{offset VkSampleLocationsInfoEXT, pSampleLocations}

        {-# INLINE writeVkPSampleLocations #-}
        writeVkPSampleLocations p
          = pokeByteOff p #{offset VkSampleLocationsInfoEXT, pSampleLocations}

instance Show VkSampleLocationsInfoEXT where
        showsPrec d x
          = showString "VkSampleLocationsInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSampleLocationsPerPixel = " .
                            showsPrec d (vkSampleLocationsPerPixel x) .
                              showString ", " .
                                showString "vkSampleLocationGridSize = " .
                                  showsPrec d (vkSampleLocationGridSize x) .
                                    showString ", " .
                                      showString "vkSampleLocationsCount = " .
                                        showsPrec d (vkSampleLocationsCount x) .
                                          showString ", " .
                                            showString "vkPSampleLocations = " .
                                              showsPrec d (vkPSampleLocations x) . showChar '}'

-- | > typedef struct VkAttachmentSampleLocationsEXT {
--   >     uint32_t                         attachmentIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkAttachmentSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentSampleLocationsEXT.html VkAttachmentSampleLocationsEXT registry at www.khronos.org>
data VkAttachmentSampleLocationsEXT = VkAttachmentSampleLocationsEXT## ByteArray##

instance Eq VkAttachmentSampleLocationsEXT where
        (VkAttachmentSampleLocationsEXT## a) ==
          (VkAttachmentSampleLocationsEXT## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkAttachmentSampleLocationsEXT where
        (VkAttachmentSampleLocationsEXT## a) `compare`
          (VkAttachmentSampleLocationsEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkAttachmentSampleLocationsEXT where
        sizeOf ~_ = #{size VkAttachmentSampleLocationsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAttachmentSampleLocationsEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkAttachmentSampleLocationsEXT),
            I## a <- alignment (undefined :: VkAttachmentSampleLocationsEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkAttachmentSampleLocationsEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkAttachmentSampleLocationsEXT## ba)
          | I## n <- sizeOf (undefined :: VkAttachmentSampleLocationsEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkAttachmentSampleLocationsEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkAttachmentSampleLocationsEXT),
            I## a <- alignment (undefined :: VkAttachmentSampleLocationsEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkAttachmentSampleLocationsEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkAttachmentSampleLocationsEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkAttachmentSampleLocationsEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkAttachmentSampleLocationsEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkAttachmentSampleLocationsEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkAttachmentSampleLocationsEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkAttachmentIndex VkAttachmentSampleLocationsEXT where
        type VkAttachmentIndexMType VkAttachmentSampleLocationsEXT = Word32

        {-# NOINLINE vkAttachmentIndex #-}
        vkAttachmentIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentSampleLocationsEXT, attachmentIndex})

        {-# INLINE vkAttachmentIndexByteOffset #-}
        vkAttachmentIndexByteOffset ~_
          = #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

        {-# INLINE readVkAttachmentIndex #-}
        readVkAttachmentIndex p
          = peekByteOff p #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

        {-# INLINE writeVkAttachmentIndex #-}
        writeVkAttachmentIndex p
          = pokeByteOff p #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsInfo VkAttachmentSampleLocationsEXT where
        type VkSampleLocationsInfoMType VkAttachmentSampleLocationsEXT =
             VkSampleLocationsInfoEXT

        {-# NOINLINE vkSampleLocationsInfo #-}
        vkSampleLocationsInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo})

        {-# INLINE vkSampleLocationsInfoByteOffset #-}
        vkSampleLocationsInfoByteOffset ~_
          = #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE readVkSampleLocationsInfo #-}
        readVkSampleLocationsInfo p
          = peekByteOff p #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE writeVkSampleLocationsInfo #-}
        writeVkSampleLocationsInfo p
          = pokeByteOff p #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

instance Show VkAttachmentSampleLocationsEXT where
        showsPrec d x
          = showString "VkAttachmentSampleLocationsEXT {" .
              showString "vkAttachmentIndex = " .
                showsPrec d (vkAttachmentIndex x) .
                  showString ", " .
                    showString "vkSampleLocationsInfo = " .
                      showsPrec d (vkSampleLocationsInfo x) . showChar '}'

-- | > typedef struct VkSubpassSampleLocationsEXT {
--   >     uint32_t                         subpassIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkSubpassSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubpassSampleLocationsEXT.html VkSubpassSampleLocationsEXT registry at www.khronos.org>
data VkSubpassSampleLocationsEXT = VkSubpassSampleLocationsEXT## ByteArray##

instance Eq VkSubpassSampleLocationsEXT where
        (VkSubpassSampleLocationsEXT## a) ==
          (VkSubpassSampleLocationsEXT## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSubpassSampleLocationsEXT where
        (VkSubpassSampleLocationsEXT## a) `compare`
          (VkSubpassSampleLocationsEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSubpassSampleLocationsEXT where
        sizeOf ~_ = #{size VkSubpassSampleLocationsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSubpassSampleLocationsEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkSubpassSampleLocationsEXT),
            I## a <- alignment (undefined :: VkSubpassSampleLocationsEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSubpassSampleLocationsEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSubpassSampleLocationsEXT## ba)
          | I## n <- sizeOf (undefined :: VkSubpassSampleLocationsEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSubpassSampleLocationsEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkSubpassSampleLocationsEXT),
            I## a <- alignment (undefined :: VkSubpassSampleLocationsEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSubpassSampleLocationsEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSubpassSampleLocationsEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkSubpassSampleLocationsEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSubpassSampleLocationsEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSubpassSampleLocationsEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSubpassSampleLocationsEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSubpassIndex VkSubpassSampleLocationsEXT where
        type VkSubpassIndexMType VkSubpassSampleLocationsEXT = Word32

        {-# NOINLINE vkSubpassIndex #-}
        vkSubpassIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassSampleLocationsEXT, subpassIndex})

        {-# INLINE vkSubpassIndexByteOffset #-}
        vkSubpassIndexByteOffset ~_
          = #{offset VkSubpassSampleLocationsEXT, subpassIndex}

        {-# INLINE readVkSubpassIndex #-}
        readVkSubpassIndex p
          = peekByteOff p #{offset VkSubpassSampleLocationsEXT, subpassIndex}

        {-# INLINE writeVkSubpassIndex #-}
        writeVkSubpassIndex p
          = pokeByteOff p #{offset VkSubpassSampleLocationsEXT, subpassIndex}

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsInfo VkSubpassSampleLocationsEXT where
        type VkSampleLocationsInfoMType VkSubpassSampleLocationsEXT =
             VkSampleLocationsInfoEXT

        {-# NOINLINE vkSampleLocationsInfo #-}
        vkSampleLocationsInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo})

        {-# INLINE vkSampleLocationsInfoByteOffset #-}
        vkSampleLocationsInfoByteOffset ~_
          = #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE readVkSampleLocationsInfo #-}
        readVkSampleLocationsInfo p
          = peekByteOff p #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE writeVkSampleLocationsInfo #-}
        writeVkSampleLocationsInfo p
          = pokeByteOff p #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

instance Show VkSubpassSampleLocationsEXT where
        showsPrec d x
          = showString "VkSubpassSampleLocationsEXT {" .
              showString "vkSubpassIndex = " .
                showsPrec d (vkSubpassIndex x) .
                  showString ", " .
                    showString "vkSampleLocationsInfo = " .
                      showsPrec d (vkSampleLocationsInfo x) . showChar '}'

-- | > typedef struct VkRenderPassSampleLocationsBeginInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         attachmentInitialSampleLocationsCount;
--   >     const VkAttachmentSampleLocationsEXT* pAttachmentInitialSampleLocations;
--   >     uint32_t         postSubpassSampleLocationsCount;
--   >     const VkSubpassSampleLocationsEXT* pPostSubpassSampleLocations;
--   > } VkRenderPassSampleLocationsBeginInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkRenderPassSampleLocationsBeginInfoEXT.html VkRenderPassSampleLocationsBeginInfoEXT registry at www.khronos.org>
data VkRenderPassSampleLocationsBeginInfoEXT = VkRenderPassSampleLocationsBeginInfoEXT## ByteArray##

instance Eq VkRenderPassSampleLocationsBeginInfoEXT where
        (VkRenderPassSampleLocationsBeginInfoEXT## a) ==
          (VkRenderPassSampleLocationsBeginInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassSampleLocationsBeginInfoEXT where
        (VkRenderPassSampleLocationsBeginInfoEXT## a) `compare`
          (VkRenderPassSampleLocationsBeginInfoEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkRenderPassSampleLocationsBeginInfoEXT where
        sizeOf ~_
          = #{size VkRenderPassSampleLocationsBeginInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassSampleLocationsBeginInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkRenderPassSampleLocationsBeginInfoEXT),
            I## a <- alignment
                      (undefined :: VkRenderPassSampleLocationsBeginInfoEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkRenderPassSampleLocationsBeginInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkRenderPassSampleLocationsBeginInfoEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkRenderPassSampleLocationsBeginInfoEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkRenderPassSampleLocationsBeginInfoEXT),
            I## a <- alignment
                      (undefined :: VkRenderPassSampleLocationsBeginInfoEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkRenderPassSampleLocationsBeginInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkRenderPassSampleLocationsBeginInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkRenderPassSampleLocationsBeginInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkRenderPassSampleLocationsBeginInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkRenderPassSampleLocationsBeginInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkRenderPassSampleLocationsBeginInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkRenderPassSampleLocationsBeginInfoEXT where
        type VkSTypeMType VkRenderPassSampleLocationsBeginInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkRenderPassSampleLocationsBeginInfoEXT where
        type VkPNextMType VkRenderPassSampleLocationsBeginInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkAttachmentInitialSampleLocationsCount
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkAttachmentInitialSampleLocationsCountMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32

        {-# NOINLINE vkAttachmentInitialSampleLocationsCount #-}
        vkAttachmentInitialSampleLocationsCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount})

        {-# INLINE vkAttachmentInitialSampleLocationsCountByteOffset #-}
        vkAttachmentInitialSampleLocationsCountByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

        {-# INLINE readVkAttachmentInitialSampleLocationsCount #-}
        readVkAttachmentInitialSampleLocationsCount p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

        {-# INLINE writeVkAttachmentInitialSampleLocationsCount #-}
        writeVkAttachmentInitialSampleLocationsCount p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasVkPAttachmentInitialSampleLocations
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkPAttachmentInitialSampleLocationsMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkAttachmentSampleLocationsEXT

        {-# NOINLINE vkPAttachmentInitialSampleLocations #-}
        vkPAttachmentInitialSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations})

        {-# INLINE vkPAttachmentInitialSampleLocationsByteOffset #-}
        vkPAttachmentInitialSampleLocationsByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

        {-# INLINE readVkPAttachmentInitialSampleLocations #-}
        readVkPAttachmentInitialSampleLocations p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

        {-# INLINE writeVkPAttachmentInitialSampleLocations #-}
        writeVkPAttachmentInitialSampleLocations p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance {-# OVERLAPPING #-}
         HasVkPostSubpassSampleLocationsCount
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkPostSubpassSampleLocationsCountMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32

        {-# NOINLINE vkPostSubpassSampleLocationsCount #-}
        vkPostSubpassSampleLocationsCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount})

        {-# INLINE vkPostSubpassSampleLocationsCountByteOffset #-}
        vkPostSubpassSampleLocationsCountByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

        {-# INLINE readVkPostSubpassSampleLocationsCount #-}
        readVkPostSubpassSampleLocationsCount p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

        {-# INLINE writeVkPostSubpassSampleLocationsCount #-}
        writeVkPostSubpassSampleLocationsCount p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasVkPPostSubpassSampleLocations
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkPPostSubpassSampleLocationsMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkSubpassSampleLocationsEXT

        {-# NOINLINE vkPPostSubpassSampleLocations #-}
        vkPPostSubpassSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations})

        {-# INLINE vkPPostSubpassSampleLocationsByteOffset #-}
        vkPPostSubpassSampleLocationsByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

        {-# INLINE readVkPPostSubpassSampleLocations #-}
        readVkPPostSubpassSampleLocations p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

        {-# INLINE writeVkPPostSubpassSampleLocations #-}
        writeVkPPostSubpassSampleLocations p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance Show VkRenderPassSampleLocationsBeginInfoEXT where
        showsPrec d x
          = showString "VkRenderPassSampleLocationsBeginInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAttachmentInitialSampleLocationsCount = " .
                            showsPrec d (vkAttachmentInitialSampleLocationsCount x) .
                              showString ", " .
                                showString "vkPAttachmentInitialSampleLocations = " .
                                  showsPrec d (vkPAttachmentInitialSampleLocations x) .
                                    showString ", " .
                                      showString "vkPostSubpassSampleLocationsCount = " .
                                        showsPrec d (vkPostSubpassSampleLocationsCount x) .
                                          showString ", " .
                                            showString "vkPPostSubpassSampleLocations = " .
                                              showsPrec d (vkPPostSubpassSampleLocations x) .
                                                showChar '}'

-- | > typedef struct VkPipelineSampleLocationsStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         sampleLocationsEnable;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkPipelineSampleLocationsStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineSampleLocationsStateCreateInfoEXT.html VkPipelineSampleLocationsStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineSampleLocationsStateCreateInfoEXT = VkPipelineSampleLocationsStateCreateInfoEXT## ByteArray##

instance Eq VkPipelineSampleLocationsStateCreateInfoEXT where
        (VkPipelineSampleLocationsStateCreateInfoEXT## a) ==
          (VkPipelineSampleLocationsStateCreateInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPipelineSampleLocationsStateCreateInfoEXT where
        (VkPipelineSampleLocationsStateCreateInfoEXT## a) `compare`
          (VkPipelineSampleLocationsStateCreateInfoEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPipelineSampleLocationsStateCreateInfoEXT where
        sizeOf ~_
          = #{size VkPipelineSampleLocationsStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineSampleLocationsStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPipelineSampleLocationsStateCreateInfoEXT),
            I## a <- alignment
                      (undefined :: VkPipelineSampleLocationsStateCreateInfoEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPipelineSampleLocationsStateCreateInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPipelineSampleLocationsStateCreateInfoEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkPipelineSampleLocationsStateCreateInfoEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPipelineSampleLocationsStateCreateInfoEXT),
            I## a <- alignment
                      (undefined :: VkPipelineSampleLocationsStateCreateInfoEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPipelineSampleLocationsStateCreateInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPipelineSampleLocationsStateCreateInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPipelineSampleLocationsStateCreateInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPipelineSampleLocationsStateCreateInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPipelineSampleLocationsStateCreateInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPipelineSampleLocationsStateCreateInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineSampleLocationsStateCreateInfoEXT where
        type VkSTypeMType VkPipelineSampleLocationsStateCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineSampleLocationsStateCreateInfoEXT where
        type VkPNextMType VkPipelineSampleLocationsStateCreateInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsEnable
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type VkSampleLocationsEnableMType
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkBool32

        {-# NOINLINE vkSampleLocationsEnable #-}
        vkSampleLocationsEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable})

        {-# INLINE vkSampleLocationsEnableByteOffset #-}
        vkSampleLocationsEnableByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

        {-# INLINE readVkSampleLocationsEnable #-}
        readVkSampleLocationsEnable p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

        {-# INLINE writeVkSampleLocationsEnable #-}
        writeVkSampleLocationsEnable p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsInfo
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type VkSampleLocationsInfoMType
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkSampleLocationsInfoEXT

        {-# NOINLINE vkSampleLocationsInfo #-}
        vkSampleLocationsInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo})

        {-# INLINE vkSampleLocationsInfoByteOffset #-}
        vkSampleLocationsInfoByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

        {-# INLINE readVkSampleLocationsInfo #-}
        readVkSampleLocationsInfo p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

        {-# INLINE writeVkSampleLocationsInfo #-}
        writeVkSampleLocationsInfo p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

instance Show VkPipelineSampleLocationsStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineSampleLocationsStateCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSampleLocationsEnable = " .
                            showsPrec d (vkSampleLocationsEnable x) .
                              showString ", " .
                                showString "vkSampleLocationsInfo = " .
                                  showsPrec d (vkSampleLocationsInfo x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceSampleLocationsPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkSampleCountFlags               sampleLocationSampleCounts;
--   >     VkExtent2D                       maxSampleLocationGridSize;
--   >     float                            sampleLocationCoordinateRange[2];
--   >     uint32_t                         sampleLocationSubPixelBits;
--   >     VkBool32                         variableSampleLocations;
--   > } VkPhysicalDeviceSampleLocationsPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceSampleLocationsPropertiesEXT.html VkPhysicalDeviceSampleLocationsPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceSampleLocationsPropertiesEXT = VkPhysicalDeviceSampleLocationsPropertiesEXT## ByteArray##

instance Eq VkPhysicalDeviceSampleLocationsPropertiesEXT where
        (VkPhysicalDeviceSampleLocationsPropertiesEXT## a) ==
          (VkPhysicalDeviceSampleLocationsPropertiesEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSampleLocationsPropertiesEXT where
        (VkPhysicalDeviceSampleLocationsPropertiesEXT## a) `compare`
          (VkPhysicalDeviceSampleLocationsPropertiesEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceSampleLocationsPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSampleLocationsPropertiesEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceSampleLocationsPropertiesEXT),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceSampleLocationsPropertiesEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceSampleLocationsPropertiesEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceSampleLocationsPropertiesEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceSampleLocationsPropertiesEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceSampleLocationsPropertiesEXT),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceSampleLocationsPropertiesEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceSampleLocationsPropertiesEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceSampleLocationsPropertiesEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceSampleLocationsPropertiesEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceSampleLocationsPropertiesEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPhysicalDeviceSampleLocationsPropertiesEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceSampleLocationsPropertiesEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceSampleLocationsPropertiesEXT where
        type VkSTypeMType VkPhysicalDeviceSampleLocationsPropertiesEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceSampleLocationsPropertiesEXT where
        type VkPNextMType VkPhysicalDeviceSampleLocationsPropertiesEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkSampleLocationSampleCounts
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type VkSampleLocationSampleCountsMType
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = VkSampleCountFlags

        {-# NOINLINE vkSampleLocationSampleCounts #-}
        vkSampleLocationSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts})

        {-# INLINE vkSampleLocationSampleCountsByteOffset #-}
        vkSampleLocationSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}

        {-# INLINE readVkSampleLocationSampleCounts #-}
        readVkSampleLocationSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}

        {-# INLINE writeVkSampleLocationSampleCounts #-}
        writeVkSampleLocationSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}

instance {-# OVERLAPPING #-}
         HasVkMaxSampleLocationGridSize
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type VkMaxSampleLocationGridSizeMType
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = VkExtent2D

        {-# NOINLINE vkMaxSampleLocationGridSize #-}
        vkMaxSampleLocationGridSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize})

        {-# INLINE vkMaxSampleLocationGridSizeByteOffset #-}
        vkMaxSampleLocationGridSizeByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}

        {-# INLINE readVkMaxSampleLocationGridSize #-}
        readVkMaxSampleLocationGridSize p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}

        {-# INLINE writeVkMaxSampleLocationGridSize #-}
        writeVkMaxSampleLocationGridSize p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}

instance {-# OVERLAPPING #-}
         HasVkSampleLocationCoordinateRangeArray
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type VkSampleLocationCoordinateRangeArrayMType
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = #{type float}

        {-# NOINLINE vkSampleLocationCoordinateRangeArray #-}
        vkSampleLocationCoordinateRangeArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: #{type float}) +
                    #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange}))

        {-# INLINE vkSampleLocationCoordinateRangeArrayByteOffset #-}
        vkSampleLocationCoordinateRangeArrayByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange}

        {-# INLINE readVkSampleLocationCoordinateRangeArray #-}
        readVkSampleLocationCoordinateRangeArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange})

        {-# INLINE writeVkSampleLocationCoordinateRangeArray #-}
        writeVkSampleLocationCoordinateRangeArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange})

instance {-# OVERLAPPING #-}
         HasVkSampleLocationSubPixelBits
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type VkSampleLocationSubPixelBitsMType
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = Word32

        {-# NOINLINE vkSampleLocationSubPixelBits #-}
        vkSampleLocationSubPixelBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits})

        {-# INLINE vkSampleLocationSubPixelBitsByteOffset #-}
        vkSampleLocationSubPixelBitsByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}

        {-# INLINE readVkSampleLocationSubPixelBits #-}
        readVkSampleLocationSubPixelBits p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}

        {-# INLINE writeVkSampleLocationSubPixelBits #-}
        writeVkSampleLocationSubPixelBits p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}

instance {-# OVERLAPPING #-}
         HasVkVariableSampleLocations
           VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
        type VkVariableSampleLocationsMType
               VkPhysicalDeviceSampleLocationsPropertiesEXT
             = VkBool32

        {-# NOINLINE vkVariableSampleLocations #-}
        vkVariableSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations})

        {-# INLINE vkVariableSampleLocationsByteOffset #-}
        vkVariableSampleLocationsByteOffset ~_
          = #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}

        {-# INLINE readVkVariableSampleLocations #-}
        readVkVariableSampleLocations p
          = peekByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}

        {-# INLINE writeVkVariableSampleLocations #-}
        writeVkVariableSampleLocations p
          = pokeByteOff p #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}

instance Show VkPhysicalDeviceSampleLocationsPropertiesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceSampleLocationsPropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSampleLocationSampleCounts = " .
                            showsPrec d (vkSampleLocationSampleCounts x) .
                              showString ", " .
                                showString "vkMaxSampleLocationGridSize = " .
                                  showsPrec d (vkMaxSampleLocationGridSize x) .
                                    showString ", " .
                                      showString "vkSampleLocationCoordinateRangeArray = [" .
                                        showsPrec d
                                          (map (vkSampleLocationCoordinateRangeArray x) [1 .. 2])
                                          .
                                          showChar ']' .
                                            showString ", " .
                                              showString "vkSampleLocationSubPixelBits = " .
                                                showsPrec d (vkSampleLocationSubPixelBits x) .
                                                  showString ", " .
                                                    showString "vkVariableSampleLocations = " .
                                                      showsPrec d (vkVariableSampleLocations x) .
                                                        showChar '}'

-- | > typedef struct VkMultisamplePropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExtent2D                       maxSampleLocationGridSize;
--   > } VkMultisamplePropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMultisamplePropertiesEXT.html VkMultisamplePropertiesEXT registry at www.khronos.org>
data VkMultisamplePropertiesEXT = VkMultisamplePropertiesEXT## ByteArray##

instance Eq VkMultisamplePropertiesEXT where
        (VkMultisamplePropertiesEXT## a) == (VkMultisamplePropertiesEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkMultisamplePropertiesEXT where
        (VkMultisamplePropertiesEXT## a) `compare`
          (VkMultisamplePropertiesEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkMultisamplePropertiesEXT where
        sizeOf ~_ = #{size VkMultisamplePropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMultisamplePropertiesEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkMultisamplePropertiesEXT),
            I## a <- alignment (undefined :: VkMultisamplePropertiesEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkMultisamplePropertiesEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkMultisamplePropertiesEXT## ba)
          | I## n <- sizeOf (undefined :: VkMultisamplePropertiesEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkMultisamplePropertiesEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkMultisamplePropertiesEXT),
            I## a <- alignment (undefined :: VkMultisamplePropertiesEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkMultisamplePropertiesEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkMultisamplePropertiesEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkMultisamplePropertiesEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkMultisamplePropertiesEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkMultisamplePropertiesEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkMultisamplePropertiesEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkMultisamplePropertiesEXT
         where
        type VkSTypeMType VkMultisamplePropertiesEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMultisamplePropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkMultisamplePropertiesEXT
         where
        type VkPNextMType VkMultisamplePropertiesEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMultisamplePropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkMaxSampleLocationGridSize VkMultisamplePropertiesEXT where
        type VkMaxSampleLocationGridSizeMType VkMultisamplePropertiesEXT =
             VkExtent2D

        {-# NOINLINE vkMaxSampleLocationGridSize #-}
        vkMaxSampleLocationGridSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize})

        {-# INLINE vkMaxSampleLocationGridSizeByteOffset #-}
        vkMaxSampleLocationGridSizeByteOffset ~_
          = #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

        {-# INLINE readVkMaxSampleLocationGridSize #-}
        readVkMaxSampleLocationGridSize p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

        {-# INLINE writeVkMaxSampleLocationGridSize #-}
        writeVkMaxSampleLocationGridSize p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

instance Show VkMultisamplePropertiesEXT where
        showsPrec d x
          = showString "VkMultisamplePropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMaxSampleLocationGridSize = " .
                            showsPrec d (vkMaxSampleLocationGridSize x) . showChar '}'

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetSampleLocationsEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkSampleLocationsInfoEXT* pSampleLocationsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetSampleLocationsEXT.html vkCmdSetSampleLocationsEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetSampleLocationsEXT"
               vkCmdSetSampleLocationsEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkSampleLocationsInfoEXT -- ^ pSampleLocationsInfo
                                                               -> IO ()

-- | > void vkGetPhysicalDeviceMultisamplePropertiesEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSampleCountFlagBits samples
--   >     , VkMultisamplePropertiesEXT* pMultisampleProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceMultisamplePropertiesEXT.html vkGetPhysicalDeviceMultisamplePropertiesEXT registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceMultisamplePropertiesEXT"
               vkGetPhysicalDeviceMultisamplePropertiesEXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSampleCountFlagBits -- ^ samples
                                       -> Ptr VkMultisamplePropertiesEXT -- ^ pMultisampleProperties
                                                                         -> IO ()

pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1

type VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1

pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString

pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME <-
        (is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME -> True)
  where VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
          = _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME

_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME #-}
_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  = Ptr "VK_EXT_sample_locations\NUL"##

is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME #-}
is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  = (_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME ==)

type VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME =
     "VK_EXT_sample_locations"

-- | bitpos = @12@
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
        :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT =
        VkImageCreateFlagBits 4096

pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT =
        VkStructureType 1000143000

pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
        = VkStructureType 1000143001

pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
        = VkStructureType 1000143002

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
        = VkStructureType 1000143003

pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT =
        VkStructureType 1000143004

pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT =
        VkDynamicState 1000143000
