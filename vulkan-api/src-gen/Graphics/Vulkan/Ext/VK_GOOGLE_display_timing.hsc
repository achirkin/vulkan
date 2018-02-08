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
module Graphics.Vulkan.Ext.VK_GOOGLE_display_timing
       (-- * Vulkan extension: @VK_GOOGLE_display_timing@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Ian Elliott ianelliott@google.com@
        --
        -- author: @GOOGLE@
        --
        -- type: @device@
        --
        -- Extension number: @93@
        --
        -- Required extensions: 'VK_KHR_swapchain'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain'.
        VkRefreshCycleDurationGOOGLE(..),
        VkPastPresentationTimingGOOGLE(..), VkPresentTimesInfoGOOGLE(..),
        VkPresentTimeGOOGLE(..), vkGetRefreshCycleDurationGOOGLE,
        vkGetPastPresentationTimingGOOGLE,
        VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION,
        pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION,
        VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME,
        pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkRefreshCycleDurationGOOGLE {
--   >     uint64_t                         refreshDuration;
--   > } VkRefreshCycleDurationGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkRefreshCycleDurationGOOGLE.html VkRefreshCycleDurationGOOGLE registry at www.khronos.org>
data VkRefreshCycleDurationGOOGLE = VkRefreshCycleDurationGOOGLE## ByteArray##

instance Eq VkRefreshCycleDurationGOOGLE where
        (VkRefreshCycleDurationGOOGLE## a) ==
          (VkRefreshCycleDurationGOOGLE## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkRefreshCycleDurationGOOGLE where
        (VkRefreshCycleDurationGOOGLE## a) `compare`
          (VkRefreshCycleDurationGOOGLE## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkRefreshCycleDurationGOOGLE where
        sizeOf ~_ = #{size VkRefreshCycleDurationGOOGLE}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRefreshCycleDurationGOOGLE}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkRefreshCycleDurationGOOGLE),
            I## a <- alignment (undefined :: VkRefreshCycleDurationGOOGLE) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkRefreshCycleDurationGOOGLE##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkRefreshCycleDurationGOOGLE## ba)
          | I## n <- sizeOf (undefined :: VkRefreshCycleDurationGOOGLE) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkRefreshCycleDurationGOOGLE where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkRefreshCycleDurationGOOGLE),
            I## a <- alignment (undefined :: VkRefreshCycleDurationGOOGLE) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkRefreshCycleDurationGOOGLE##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkRefreshCycleDurationGOOGLE## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkRefreshCycleDurationGOOGLE##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkRefreshCycleDurationGOOGLE## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkRefreshCycleDurationGOOGLE## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkRefreshCycleDurationGOOGLE## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkRefreshDuration VkRefreshCycleDurationGOOGLE where
        type VkRefreshDurationMType VkRefreshCycleDurationGOOGLE = Word64

        {-# NOINLINE vkRefreshDuration #-}
        vkRefreshDuration x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRefreshCycleDurationGOOGLE, refreshDuration})

        {-# INLINE vkRefreshDurationByteOffset #-}
        vkRefreshDurationByteOffset ~_
          = #{offset VkRefreshCycleDurationGOOGLE, refreshDuration}

        {-# INLINE readVkRefreshDuration #-}
        readVkRefreshDuration p
          = peekByteOff p #{offset VkRefreshCycleDurationGOOGLE, refreshDuration}

        {-# INLINE writeVkRefreshDuration #-}
        writeVkRefreshDuration p
          = pokeByteOff p #{offset VkRefreshCycleDurationGOOGLE, refreshDuration}

instance {-# OVERLAPPING #-}
         HasField "refreshDuration" VkRefreshCycleDurationGOOGLE where
        type FieldType "refreshDuration" VkRefreshCycleDurationGOOGLE =
             Word64
        type FieldOptional "refreshDuration" VkRefreshCycleDurationGOOGLE =
             'False -- ' closing tick for hsc2hs

instance CanReadField "refreshDuration"
           VkRefreshCycleDurationGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkRefreshDuration

        {-# INLINE readField #-}
        readField = readVkRefreshDuration

instance CanWriteField "refreshDuration"
           VkRefreshCycleDurationGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkRefreshDuration

instance Show VkRefreshCycleDurationGOOGLE where
        showsPrec d x
          = showString "VkRefreshCycleDurationGOOGLE {" .
              showString "vkRefreshDuration = " .
                showsPrec d (vkRefreshDuration x) . showChar '}'

-- | > typedef struct VkPastPresentationTimingGOOGLE {
--   >     uint32_t                         presentID;
--   >     uint64_t                         desiredPresentTime;
--   >     uint64_t                         actualPresentTime;
--   >     uint64_t                         earliestPresentTime;
--   >     uint64_t                         presentMargin;
--   > } VkPastPresentationTimingGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPastPresentationTimingGOOGLE.html VkPastPresentationTimingGOOGLE registry at www.khronos.org>
data VkPastPresentationTimingGOOGLE = VkPastPresentationTimingGOOGLE## ByteArray##

instance Eq VkPastPresentationTimingGOOGLE where
        (VkPastPresentationTimingGOOGLE## a) ==
          (VkPastPresentationTimingGOOGLE## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPastPresentationTimingGOOGLE where
        (VkPastPresentationTimingGOOGLE## a) `compare`
          (VkPastPresentationTimingGOOGLE## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPastPresentationTimingGOOGLE where
        sizeOf ~_ = #{size VkPastPresentationTimingGOOGLE}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPastPresentationTimingGOOGLE}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkPastPresentationTimingGOOGLE),
            I## a <- alignment (undefined :: VkPastPresentationTimingGOOGLE) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPastPresentationTimingGOOGLE##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPastPresentationTimingGOOGLE## ba)
          | I## n <- sizeOf (undefined :: VkPastPresentationTimingGOOGLE) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPastPresentationTimingGOOGLE where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkPastPresentationTimingGOOGLE),
            I## a <- alignment (undefined :: VkPastPresentationTimingGOOGLE) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPastPresentationTimingGOOGLE##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPastPresentationTimingGOOGLE## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkPastPresentationTimingGOOGLE##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPastPresentationTimingGOOGLE## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPastPresentationTimingGOOGLE## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPastPresentationTimingGOOGLE## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkPresentID VkPastPresentationTimingGOOGLE where
        type VkPresentIDMType VkPastPresentationTimingGOOGLE = Word32

        {-# NOINLINE vkPresentID #-}
        vkPresentID x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, presentID})

        {-# INLINE vkPresentIDByteOffset #-}
        vkPresentIDByteOffset ~_
          = #{offset VkPastPresentationTimingGOOGLE, presentID}

        {-# INLINE readVkPresentID #-}
        readVkPresentID p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, presentID}

        {-# INLINE writeVkPresentID #-}
        writeVkPresentID p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         HasField "presentID" VkPastPresentationTimingGOOGLE where
        type FieldType "presentID" VkPastPresentationTimingGOOGLE = Word32
        type FieldOptional "presentID" VkPastPresentationTimingGOOGLE =
             'False -- ' closing tick for hsc2hs

instance CanReadField "presentID" VkPastPresentationTimingGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkPresentID

        {-# INLINE readField #-}
        readField = readVkPresentID

instance CanWriteField "presentID" VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkPresentID

instance {-# OVERLAPPING #-}
         HasVkDesiredPresentTime VkPastPresentationTimingGOOGLE where
        type VkDesiredPresentTimeMType VkPastPresentationTimingGOOGLE =
             Word64

        {-# NOINLINE vkDesiredPresentTime #-}
        vkDesiredPresentTime x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime})

        {-# INLINE vkDesiredPresentTimeByteOffset #-}
        vkDesiredPresentTimeByteOffset ~_
          = #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}

        {-# INLINE readVkDesiredPresentTime #-}
        readVkDesiredPresentTime p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}

        {-# INLINE writeVkDesiredPresentTime #-}
        writeVkDesiredPresentTime p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}

instance {-# OVERLAPPING #-}
         HasField "desiredPresentTime" VkPastPresentationTimingGOOGLE where
        type FieldType "desiredPresentTime" VkPastPresentationTimingGOOGLE
             = Word64
        type FieldOptional "desiredPresentTime"
               VkPastPresentationTimingGOOGLE
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "desiredPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkDesiredPresentTime

        {-# INLINE readField #-}
        readField = readVkDesiredPresentTime

instance CanWriteField "desiredPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkDesiredPresentTime

instance {-# OVERLAPPING #-}
         HasVkActualPresentTime VkPastPresentationTimingGOOGLE where
        type VkActualPresentTimeMType VkPastPresentationTimingGOOGLE =
             Word64

        {-# NOINLINE vkActualPresentTime #-}
        vkActualPresentTime x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, actualPresentTime})

        {-# INLINE vkActualPresentTimeByteOffset #-}
        vkActualPresentTimeByteOffset ~_
          = #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}

        {-# INLINE readVkActualPresentTime #-}
        readVkActualPresentTime p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}

        {-# INLINE writeVkActualPresentTime #-}
        writeVkActualPresentTime p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}

instance {-# OVERLAPPING #-}
         HasField "actualPresentTime" VkPastPresentationTimingGOOGLE where
        type FieldType "actualPresentTime" VkPastPresentationTimingGOOGLE =
             Word64
        type FieldOptional "actualPresentTime"
               VkPastPresentationTimingGOOGLE
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "actualPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkActualPresentTime

        {-# INLINE readField #-}
        readField = readVkActualPresentTime

instance CanWriteField "actualPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkActualPresentTime

instance {-# OVERLAPPING #-}
         HasVkEarliestPresentTime VkPastPresentationTimingGOOGLE where
        type VkEarliestPresentTimeMType VkPastPresentationTimingGOOGLE =
             Word64

        {-# NOINLINE vkEarliestPresentTime #-}
        vkEarliestPresentTime x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime})

        {-# INLINE vkEarliestPresentTimeByteOffset #-}
        vkEarliestPresentTimeByteOffset ~_
          = #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}

        {-# INLINE readVkEarliestPresentTime #-}
        readVkEarliestPresentTime p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}

        {-# INLINE writeVkEarliestPresentTime #-}
        writeVkEarliestPresentTime p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}

instance {-# OVERLAPPING #-}
         HasField "earliestPresentTime" VkPastPresentationTimingGOOGLE where
        type FieldType "earliestPresentTime" VkPastPresentationTimingGOOGLE
             = Word64
        type FieldOptional "earliestPresentTime"
               VkPastPresentationTimingGOOGLE
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "earliestPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkEarliestPresentTime

        {-# INLINE readField #-}
        readField = readVkEarliestPresentTime

instance CanWriteField "earliestPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkEarliestPresentTime

instance {-# OVERLAPPING #-}
         HasVkPresentMargin VkPastPresentationTimingGOOGLE where
        type VkPresentMarginMType VkPastPresentationTimingGOOGLE = Word64

        {-# NOINLINE vkPresentMargin #-}
        vkPresentMargin x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, presentMargin})

        {-# INLINE vkPresentMarginByteOffset #-}
        vkPresentMarginByteOffset ~_
          = #{offset VkPastPresentationTimingGOOGLE, presentMargin}

        {-# INLINE readVkPresentMargin #-}
        readVkPresentMargin p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, presentMargin}

        {-# INLINE writeVkPresentMargin #-}
        writeVkPresentMargin p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, presentMargin}

instance {-# OVERLAPPING #-}
         HasField "presentMargin" VkPastPresentationTimingGOOGLE where
        type FieldType "presentMargin" VkPastPresentationTimingGOOGLE =
             Word64
        type FieldOptional "presentMargin" VkPastPresentationTimingGOOGLE =
             'False -- ' closing tick for hsc2hs

instance CanReadField "presentMargin"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkPresentMargin

        {-# INLINE readField #-}
        readField = readVkPresentMargin

instance CanWriteField "presentMargin"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkPresentMargin

instance Show VkPastPresentationTimingGOOGLE where
        showsPrec d x
          = showString "VkPastPresentationTimingGOOGLE {" .
              showString "vkPresentID = " .
                showsPrec d (vkPresentID x) .
                  showString ", " .
                    showString "vkDesiredPresentTime = " .
                      showsPrec d (vkDesiredPresentTime x) .
                        showString ", " .
                          showString "vkActualPresentTime = " .
                            showsPrec d (vkActualPresentTime x) .
                              showString ", " .
                                showString "vkEarliestPresentTime = " .
                                  showsPrec d (vkEarliestPresentTime x) .
                                    showString ", " .
                                      showString "vkPresentMargin = " .
                                        showsPrec d (vkPresentMargin x) . showChar '}'

-- | > typedef struct VkPresentTimesInfoGOOGLE {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     uint32_t                         swapchainCount;
--   >     const VkPresentTimeGOOGLE*   pTimes;
--   > } VkPresentTimesInfoGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPresentTimesInfoGOOGLE.html VkPresentTimesInfoGOOGLE registry at www.khronos.org>
data VkPresentTimesInfoGOOGLE = VkPresentTimesInfoGOOGLE## ByteArray##

instance Eq VkPresentTimesInfoGOOGLE where
        (VkPresentTimesInfoGOOGLE## a) == (VkPresentTimesInfoGOOGLE## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPresentTimesInfoGOOGLE where
        (VkPresentTimesInfoGOOGLE## a) `compare`
          (VkPresentTimesInfoGOOGLE## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPresentTimesInfoGOOGLE where
        sizeOf ~_ = #{size VkPresentTimesInfoGOOGLE}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentTimesInfoGOOGLE}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkPresentTimesInfoGOOGLE),
            I## a <- alignment (undefined :: VkPresentTimesInfoGOOGLE) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPresentTimesInfoGOOGLE##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPresentTimesInfoGOOGLE## ba)
          | I## n <- sizeOf (undefined :: VkPresentTimesInfoGOOGLE) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPresentTimesInfoGOOGLE where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkPresentTimesInfoGOOGLE),
            I## a <- alignment (undefined :: VkPresentTimesInfoGOOGLE) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPresentTimesInfoGOOGLE##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPresentTimesInfoGOOGLE## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkPresentTimesInfoGOOGLE##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPresentTimesInfoGOOGLE## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPresentTimesInfoGOOGLE## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPresentTimesInfoGOOGLE## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkPresentTimesInfoGOOGLE
         where
        type VkSTypeMType VkPresentTimesInfoGOOGLE = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPresentTimesInfoGOOGLE, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPresentTimesInfoGOOGLE where
        type FieldType "sType" VkPresentTimesInfoGOOGLE = VkStructureType
        type FieldOptional "sType" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkPresentTimesInfoGOOGLE where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkPresentTimesInfoGOOGLE
         where
        type VkPNextMType VkPresentTimesInfoGOOGLE = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPresentTimesInfoGOOGLE, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPresentTimesInfoGOOGLE where
        type FieldType "pNext" VkPresentTimesInfoGOOGLE = Ptr Void
        type FieldOptional "pNext" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkPresentTimesInfoGOOGLE where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSwapchainCount VkPresentTimesInfoGOOGLE where
        type VkSwapchainCountMType VkPresentTimesInfoGOOGLE = Word32

        {-# NOINLINE vkSwapchainCount #-}
        vkSwapchainCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, swapchainCount})

        {-# INLINE vkSwapchainCountByteOffset #-}
        vkSwapchainCountByteOffset ~_
          = #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

        {-# INLINE readVkSwapchainCount #-}
        readVkSwapchainCount p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

        {-# INLINE writeVkSwapchainCount #-}
        writeVkSwapchainCount p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkPresentTimesInfoGOOGLE where
        type FieldType "swapchainCount" VkPresentTimesInfoGOOGLE = Word32
        type FieldOptional "swapchainCount" VkPresentTimesInfoGOOGLE =
             'False -- ' closing tick for hsc2hs

instance CanReadField "swapchainCount" VkPresentTimesInfoGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkSwapchainCount

        {-# INLINE readField #-}
        readField = readVkSwapchainCount

instance CanWriteField "swapchainCount" VkPresentTimesInfoGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkSwapchainCount

instance {-# OVERLAPPING #-} HasVkPTimes VkPresentTimesInfoGOOGLE
         where
        type VkPTimesMType VkPresentTimesInfoGOOGLE =
             Ptr VkPresentTimeGOOGLE

        {-# NOINLINE vkPTimes #-}
        vkPTimes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, pTimes})

        {-# INLINE vkPTimesByteOffset #-}
        vkPTimesByteOffset ~_
          = #{offset VkPresentTimesInfoGOOGLE, pTimes}

        {-# INLINE readVkPTimes #-}
        readVkPTimes p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, pTimes}

        {-# INLINE writeVkPTimes #-}
        writeVkPTimes p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, pTimes}

instance {-# OVERLAPPING #-}
         HasField "pTimes" VkPresentTimesInfoGOOGLE where
        type FieldType "pTimes" VkPresentTimesInfoGOOGLE =
             Ptr VkPresentTimeGOOGLE
        type FieldOptional "pTimes" VkPresentTimesInfoGOOGLE = 'True -- ' closing tick for hsc2hs

instance CanReadField "pTimes" VkPresentTimesInfoGOOGLE where
        {-# INLINE getField #-}
        getField = vkPTimes

        {-# INLINE readField #-}
        readField = readVkPTimes

instance CanWriteField "pTimes" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField = writeVkPTimes

instance Show VkPresentTimesInfoGOOGLE where
        showsPrec d x
          = showString "VkPresentTimesInfoGOOGLE {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSwapchainCount = " .
                            showsPrec d (vkSwapchainCount x) .
                              showString ", " .
                                showString "vkPTimes = " . showsPrec d (vkPTimes x) . showChar '}'

-- | > typedef struct VkPresentTimeGOOGLE {
--   >     uint32_t                         presentID;
--   >     uint64_t                         desiredPresentTime;
--   > } VkPresentTimeGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPresentTimeGOOGLE.html VkPresentTimeGOOGLE registry at www.khronos.org>
data VkPresentTimeGOOGLE = VkPresentTimeGOOGLE## ByteArray##

instance Eq VkPresentTimeGOOGLE where
        (VkPresentTimeGOOGLE## a) == (VkPresentTimeGOOGLE## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPresentTimeGOOGLE where
        (VkPresentTimeGOOGLE## a) `compare` (VkPresentTimeGOOGLE## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPresentTimeGOOGLE where
        sizeOf ~_ = #{size VkPresentTimeGOOGLE}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentTimeGOOGLE}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkPresentTimeGOOGLE),
            I## a <- alignment (undefined :: VkPresentTimeGOOGLE) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPresentTimeGOOGLE## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPresentTimeGOOGLE## ba)
          | I## n <- sizeOf (undefined :: VkPresentTimeGOOGLE) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPresentTimeGOOGLE where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkPresentTimeGOOGLE),
            I## a <- alignment (undefined :: VkPresentTimeGOOGLE) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPresentTimeGOOGLE##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPresentTimeGOOGLE## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkPresentTimeGOOGLE##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPresentTimeGOOGLE## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPresentTimeGOOGLE## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPresentTimeGOOGLE## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkPresentID VkPresentTimeGOOGLE
         where
        type VkPresentIDMType VkPresentTimeGOOGLE = Word32

        {-# NOINLINE vkPresentID #-}
        vkPresentID x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimeGOOGLE, presentID})

        {-# INLINE vkPresentIDByteOffset #-}
        vkPresentIDByteOffset ~_
          = #{offset VkPresentTimeGOOGLE, presentID}

        {-# INLINE readVkPresentID #-}
        readVkPresentID p
          = peekByteOff p #{offset VkPresentTimeGOOGLE, presentID}

        {-# INLINE writeVkPresentID #-}
        writeVkPresentID p
          = pokeByteOff p #{offset VkPresentTimeGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         HasField "presentID" VkPresentTimeGOOGLE where
        type FieldType "presentID" VkPresentTimeGOOGLE = Word32
        type FieldOptional "presentID" VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs

instance CanReadField "presentID" VkPresentTimeGOOGLE where
        {-# INLINE getField #-}
        getField = vkPresentID

        {-# INLINE readField #-}
        readField = readVkPresentID

instance CanWriteField "presentID" VkPresentTimeGOOGLE where
        {-# INLINE writeField #-}
        writeField = writeVkPresentID

instance {-# OVERLAPPING #-}
         HasVkDesiredPresentTime VkPresentTimeGOOGLE where
        type VkDesiredPresentTimeMType VkPresentTimeGOOGLE = Word64

        {-# NOINLINE vkDesiredPresentTime #-}
        vkDesiredPresentTime x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimeGOOGLE, desiredPresentTime})

        {-# INLINE vkDesiredPresentTimeByteOffset #-}
        vkDesiredPresentTimeByteOffset ~_
          = #{offset VkPresentTimeGOOGLE, desiredPresentTime}

        {-# INLINE readVkDesiredPresentTime #-}
        readVkDesiredPresentTime p
          = peekByteOff p #{offset VkPresentTimeGOOGLE, desiredPresentTime}

        {-# INLINE writeVkDesiredPresentTime #-}
        writeVkDesiredPresentTime p
          = pokeByteOff p #{offset VkPresentTimeGOOGLE, desiredPresentTime}

instance {-# OVERLAPPING #-}
         HasField "desiredPresentTime" VkPresentTimeGOOGLE where
        type FieldType "desiredPresentTime" VkPresentTimeGOOGLE = Word64
        type FieldOptional "desiredPresentTime" VkPresentTimeGOOGLE =
             'False -- ' closing tick for hsc2hs

instance CanReadField "desiredPresentTime" VkPresentTimeGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkDesiredPresentTime

        {-# INLINE readField #-}
        readField = readVkDesiredPresentTime

instance CanWriteField "desiredPresentTime" VkPresentTimeGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkDesiredPresentTime

instance Show VkPresentTimeGOOGLE where
        showsPrec d x
          = showString "VkPresentTimeGOOGLE {" .
              showString "vkPresentID = " .
                showsPrec d (vkPresentID x) .
                  showString ", " .
                    showString "vkDesiredPresentTime = " .
                      showsPrec d (vkDesiredPresentTime x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetRefreshCycleDurationGOOGLE
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , VkRefreshCycleDurationGOOGLE* pDisplayTimingProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetRefreshCycleDurationGOOGLE.html vkGetRefreshCycleDurationGOOGLE registry at www.khronos.org>
foreign import ccall unsafe "vkGetRefreshCycleDurationGOOGLE"
               vkGetRefreshCycleDurationGOOGLE ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                -> Ptr VkRefreshCycleDurationGOOGLE -- ^ pDisplayTimingProperties
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPastPresentationTimingGOOGLE
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , uint32_t* pPresentationTimingCount
--   >     , VkPastPresentationTimingGOOGLE* pPresentationTimings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPastPresentationTimingGOOGLE.html vkGetPastPresentationTimingGOOGLE registry at www.khronos.org>
foreign import ccall unsafe "vkGetPastPresentationTimingGOOGLE"
               vkGetPastPresentationTimingGOOGLE ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   Ptr Word32 -- ^ pPresentationTimingCount
                              -> Ptr VkPastPresentationTimingGOOGLE -- ^ pPresentationTimings
                                                                    -> IO VkResult

pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1

type VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1

pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: CString

pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME <-
        (is_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME -> True)
  where VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
          = _VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME

_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: CString

{-# INLINE _VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME #-}
_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  = Ptr "VK_GOOGLE_display_timing\NUL"##

is_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME #-}
is_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
  = (_VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME ==)

type VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME =
     "VK_GOOGLE_display_timing"

pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE =
        VkStructureType 1000092000
