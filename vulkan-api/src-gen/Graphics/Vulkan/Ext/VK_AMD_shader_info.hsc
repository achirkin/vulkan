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
module Graphics.Vulkan.Ext.VK_AMD_shader_info
       (-- * Vulkan extension: @VK_AMD_shader_info@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jaakko Konttinen @jaakko@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @43@
        VkShaderResourceUsageAMD(..), VkShaderStatisticsInfoAMD(..),
        vkGetShaderInfoAMD, VK_AMD_SHADER_INFO_SPEC_VERSION,
        pattern VK_AMD_SHADER_INFO_SPEC_VERSION,
        VK_AMD_SHADER_INFO_EXTENSION_NAME,
        pattern VK_AMD_SHADER_INFO_EXTENSION_NAME)
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

data VkShaderResourceUsageAMD = VkShaderResourceUsageAMD## ByteArray##

instance Eq VkShaderResourceUsageAMD where
        (VkShaderResourceUsageAMD## a) == (VkShaderResourceUsageAMD## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkShaderResourceUsageAMD where
        (VkShaderResourceUsageAMD## a) `compare`
          (VkShaderResourceUsageAMD## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkShaderResourceUsageAMD where
        sizeOf ~_ = #{size VkShaderResourceUsageAMD}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkShaderResourceUsageAMD}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkShaderResourceUsageAMD),
            I## a <- alignment (undefined :: VkShaderResourceUsageAMD) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkShaderResourceUsageAMD##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkShaderResourceUsageAMD## ba)
          | I## n <- sizeOf (undefined :: VkShaderResourceUsageAMD) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkShaderResourceUsageAMD where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkShaderResourceUsageAMD),
            I## a <- alignment (undefined :: VkShaderResourceUsageAMD) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkShaderResourceUsageAMD##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkShaderResourceUsageAMD## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkShaderResourceUsageAMD##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkShaderResourceUsageAMD## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkShaderResourceUsageAMD## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkShaderResourceUsageAMD## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkNumUsedVgprs VkShaderResourceUsageAMD where
        type VkNumUsedVgprsMType VkShaderResourceUsageAMD = Word32

        {-# NOINLINE vkNumUsedVgprs #-}
        vkNumUsedVgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, numUsedVgprs})

        {-# INLINE vkNumUsedVgprsByteOffset #-}
        vkNumUsedVgprsByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, numUsedVgprs}

        {-# INLINE readVkNumUsedVgprs #-}
        readVkNumUsedVgprs p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, numUsedVgprs}

        {-# INLINE writeVkNumUsedVgprs #-}
        writeVkNumUsedVgprs p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, numUsedVgprs}

instance {-# OVERLAPPING #-}
         HasVkNumUsedSgprs VkShaderResourceUsageAMD where
        type VkNumUsedSgprsMType VkShaderResourceUsageAMD = Word32

        {-# NOINLINE vkNumUsedSgprs #-}
        vkNumUsedSgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, numUsedSgprs})

        {-# INLINE vkNumUsedSgprsByteOffset #-}
        vkNumUsedSgprsByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, numUsedSgprs}

        {-# INLINE readVkNumUsedSgprs #-}
        readVkNumUsedSgprs p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, numUsedSgprs}

        {-# INLINE writeVkNumUsedSgprs #-}
        writeVkNumUsedSgprs p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, numUsedSgprs}

instance {-# OVERLAPPING #-}
         HasVkLdsSizePerLocalWorkGroup VkShaderResourceUsageAMD where
        type VkLdsSizePerLocalWorkGroupMType VkShaderResourceUsageAMD =
             Word32

        {-# NOINLINE vkLdsSizePerLocalWorkGroup #-}
        vkLdsSizePerLocalWorkGroup x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup})

        {-# INLINE vkLdsSizePerLocalWorkGroupByteOffset #-}
        vkLdsSizePerLocalWorkGroupByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

        {-# INLINE readVkLdsSizePerLocalWorkGroup #-}
        readVkLdsSizePerLocalWorkGroup p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

        {-# INLINE writeVkLdsSizePerLocalWorkGroup #-}
        writeVkLdsSizePerLocalWorkGroup p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

instance {-# OVERLAPPING #-}
         HasVkLdsUsageSizeInBytes VkShaderResourceUsageAMD where
        type VkLdsUsageSizeInBytesMType VkShaderResourceUsageAMD =
             #{type size_t}

        {-# NOINLINE vkLdsUsageSizeInBytes #-}
        vkLdsUsageSizeInBytes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes})

        {-# INLINE vkLdsUsageSizeInBytesByteOffset #-}
        vkLdsUsageSizeInBytesByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

        {-# INLINE readVkLdsUsageSizeInBytes #-}
        readVkLdsUsageSizeInBytes p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

        {-# INLINE writeVkLdsUsageSizeInBytes #-}
        writeVkLdsUsageSizeInBytes p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

instance {-# OVERLAPPING #-}
         HasVkScratchMemUsageInBytes VkShaderResourceUsageAMD where
        type VkScratchMemUsageInBytesMType VkShaderResourceUsageAMD =
             #{type size_t}

        {-# NOINLINE vkScratchMemUsageInBytes #-}
        vkScratchMemUsageInBytes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes})

        {-# INLINE vkScratchMemUsageInBytesByteOffset #-}
        vkScratchMemUsageInBytesByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

        {-# INLINE readVkScratchMemUsageInBytes #-}
        readVkScratchMemUsageInBytes p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

        {-# INLINE writeVkScratchMemUsageInBytes #-}
        writeVkScratchMemUsageInBytes p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

instance Show VkShaderResourceUsageAMD where
        showsPrec d x
          = showString "VkShaderResourceUsageAMD {" .
              showString "vkNumUsedVgprs = " .
                showsPrec d (vkNumUsedVgprs x) .
                  showString ", " .
                    showString "vkNumUsedSgprs = " .
                      showsPrec d (vkNumUsedSgprs x) .
                        showString ", " .
                          showString "vkLdsSizePerLocalWorkGroup = " .
                            showsPrec d (vkLdsSizePerLocalWorkGroup x) .
                              showString ", " .
                                showString "vkLdsUsageSizeInBytes = " .
                                  showsPrec d (vkLdsUsageSizeInBytes x) .
                                    showString ", " .
                                      showString "vkScratchMemUsageInBytes = " .
                                        showsPrec d (vkScratchMemUsageInBytes x) . showChar '}'

data VkShaderStatisticsInfoAMD = VkShaderStatisticsInfoAMD## ByteArray##

instance Eq VkShaderStatisticsInfoAMD where
        (VkShaderStatisticsInfoAMD## a) == (VkShaderStatisticsInfoAMD## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkShaderStatisticsInfoAMD where
        (VkShaderStatisticsInfoAMD## a) `compare`
          (VkShaderStatisticsInfoAMD## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkShaderStatisticsInfoAMD where
        sizeOf ~_ = #{size VkShaderStatisticsInfoAMD}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkShaderStatisticsInfoAMD}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkShaderStatisticsInfoAMD),
            I## a <- alignment (undefined :: VkShaderStatisticsInfoAMD) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkShaderStatisticsInfoAMD##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkShaderStatisticsInfoAMD## ba)
          | I## n <- sizeOf (undefined :: VkShaderStatisticsInfoAMD) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkShaderStatisticsInfoAMD where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkShaderStatisticsInfoAMD),
            I## a <- alignment (undefined :: VkShaderStatisticsInfoAMD) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkShaderStatisticsInfoAMD##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkShaderStatisticsInfoAMD## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkShaderStatisticsInfoAMD##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkShaderStatisticsInfoAMD## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkShaderStatisticsInfoAMD## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkShaderStatisticsInfoAMD## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkShaderStageMask VkShaderStatisticsInfoAMD where
        type VkShaderStageMaskMType VkShaderStatisticsInfoAMD =
             VkShaderStageFlags

        {-# NOINLINE vkShaderStageMask #-}
        vkShaderStageMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, shaderStageMask})

        {-# INLINE vkShaderStageMaskByteOffset #-}
        vkShaderStageMaskByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

        {-# INLINE readVkShaderStageMask #-}
        readVkShaderStageMask p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

        {-# INLINE writeVkShaderStageMask #-}
        writeVkShaderStageMask p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

instance {-# OVERLAPPING #-}
         HasVkResourceUsage VkShaderStatisticsInfoAMD where
        type VkResourceUsageMType VkShaderStatisticsInfoAMD =
             VkShaderResourceUsageAMD

        {-# NOINLINE vkResourceUsage #-}
        vkResourceUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, resourceUsage})

        {-# INLINE vkResourceUsageByteOffset #-}
        vkResourceUsageByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, resourceUsage}

        {-# INLINE readVkResourceUsage #-}
        readVkResourceUsage p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, resourceUsage}

        {-# INLINE writeVkResourceUsage #-}
        writeVkResourceUsage p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, resourceUsage}

instance {-# OVERLAPPING #-}
         HasVkNumPhysicalVgprs VkShaderStatisticsInfoAMD where
        type VkNumPhysicalVgprsMType VkShaderStatisticsInfoAMD = Word32

        {-# NOINLINE vkNumPhysicalVgprs #-}
        vkNumPhysicalVgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs})

        {-# INLINE vkNumPhysicalVgprsByteOffset #-}
        vkNumPhysicalVgprsByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

        {-# INLINE readVkNumPhysicalVgprs #-}
        readVkNumPhysicalVgprs p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

        {-# INLINE writeVkNumPhysicalVgprs #-}
        writeVkNumPhysicalVgprs p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

instance {-# OVERLAPPING #-}
         HasVkNumPhysicalSgprs VkShaderStatisticsInfoAMD where
        type VkNumPhysicalSgprsMType VkShaderStatisticsInfoAMD = Word32

        {-# NOINLINE vkNumPhysicalSgprs #-}
        vkNumPhysicalSgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs})

        {-# INLINE vkNumPhysicalSgprsByteOffset #-}
        vkNumPhysicalSgprsByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

        {-# INLINE readVkNumPhysicalSgprs #-}
        readVkNumPhysicalSgprs p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

        {-# INLINE writeVkNumPhysicalSgprs #-}
        writeVkNumPhysicalSgprs p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

instance {-# OVERLAPPING #-}
         HasVkNumAvailableVgprs VkShaderStatisticsInfoAMD where
        type VkNumAvailableVgprsMType VkShaderStatisticsInfoAMD = Word32

        {-# NOINLINE vkNumAvailableVgprs #-}
        vkNumAvailableVgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs})

        {-# INLINE vkNumAvailableVgprsByteOffset #-}
        vkNumAvailableVgprsByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

        {-# INLINE readVkNumAvailableVgprs #-}
        readVkNumAvailableVgprs p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

        {-# INLINE writeVkNumAvailableVgprs #-}
        writeVkNumAvailableVgprs p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

instance {-# OVERLAPPING #-}
         HasVkNumAvailableSgprs VkShaderStatisticsInfoAMD where
        type VkNumAvailableSgprsMType VkShaderStatisticsInfoAMD = Word32

        {-# NOINLINE vkNumAvailableSgprs #-}
        vkNumAvailableSgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs})

        {-# INLINE vkNumAvailableSgprsByteOffset #-}
        vkNumAvailableSgprsByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

        {-# INLINE readVkNumAvailableSgprs #-}
        readVkNumAvailableSgprs p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

        {-# INLINE writeVkNumAvailableSgprs #-}
        writeVkNumAvailableSgprs p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

instance {-# OVERLAPPING #-}
         HasVkComputeWorkGroupSizeArray VkShaderStatisticsInfoAMD where
        type VkComputeWorkGroupSizeArrayMType VkShaderStatisticsInfoAMD =
             Word32

        {-# NOINLINE vkComputeWorkGroupSizeArray #-}
        vkComputeWorkGroupSizeArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word32) +
                    #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}))

        {-# INLINE vkComputeWorkGroupSizeArrayByteOffset #-}
        vkComputeWorkGroupSizeArrayByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}

        {-# INLINE readVkComputeWorkGroupSizeArray #-}
        readVkComputeWorkGroupSizeArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize})

        {-# INLINE writeVkComputeWorkGroupSizeArray #-}
        writeVkComputeWorkGroupSizeArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize})

instance Show VkShaderStatisticsInfoAMD where
        showsPrec d x
          = showString "VkShaderStatisticsInfoAMD {" .
              showString "vkShaderStageMask = " .
                showsPrec d (vkShaderStageMask x) .
                  showString ", " .
                    showString "vkResourceUsage = " .
                      showsPrec d (vkResourceUsage x) .
                        showString ", " .
                          showString "vkNumPhysicalVgprs = " .
                            showsPrec d (vkNumPhysicalVgprs x) .
                              showString ", " .
                                showString "vkNumPhysicalSgprs = " .
                                  showsPrec d (vkNumPhysicalSgprs x) .
                                    showString ", " .
                                      showString "vkNumAvailableVgprs = " .
                                        showsPrec d (vkNumAvailableVgprs x) .
                                          showString ", " .
                                            showString "vkNumAvailableSgprs = " .
                                              showsPrec d (vkNumAvailableSgprs x) .
                                                showString ", " .
                                                  showString "vkComputeWorkGroupSizeArray = [" .
                                                    showsPrec d
                                                      (map (vkComputeWorkGroupSizeArray x) [1 .. 3])
                                                      . showChar ']' . showChar '}'

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_FEATURE_NOT_PRESENT', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetShaderInfoAMD
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , VkShaderStageFlagBits shaderStage
--   >     , VkShaderInfoTypeAMD infoType
--   >     , size_t* pInfoSize
--   >     , void* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetShaderInfoAMD.html vkGetShaderInfoAMD registry at www.khronos.org>
foreign import ccall unsafe "vkGetShaderInfoAMD" vkGetShaderInfoAMD
               ::
               VkDevice -- ^ device
                        ->
                 VkPipeline -- ^ pipeline
                            ->
                   VkShaderStageFlagBits -- ^ shaderStage
                                         ->
                     VkShaderInfoTypeAMD -- ^ infoType
                                         ->
                       Ptr #{type size_t} -- ^ pInfoSize
                                                      -> Ptr Void -- ^ pInfo
                                                                  -> IO VkResult

pattern VK_AMD_SHADER_INFO_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_AMD_SHADER_INFO_SPEC_VERSION = 1

type VK_AMD_SHADER_INFO_SPEC_VERSION = 1

pattern VK_AMD_SHADER_INFO_EXTENSION_NAME :: CString

pattern VK_AMD_SHADER_INFO_EXTENSION_NAME <-
        (is_VK_AMD_SHADER_INFO_EXTENSION_NAME -> True)
  where VK_AMD_SHADER_INFO_EXTENSION_NAME
          = _VK_AMD_SHADER_INFO_EXTENSION_NAME

_VK_AMD_SHADER_INFO_EXTENSION_NAME :: CString

{-# INLINE _VK_AMD_SHADER_INFO_EXTENSION_NAME #-}
_VK_AMD_SHADER_INFO_EXTENSION_NAME = Ptr "VK_AMD_shader_info\NUL"##

is_VK_AMD_SHADER_INFO_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_AMD_SHADER_INFO_EXTENSION_NAME #-}
is_VK_AMD_SHADER_INFO_EXTENSION_NAME
  = (_VK_AMD_SHADER_INFO_EXTENSION_NAME ==)

type VK_AMD_SHADER_INFO_EXTENSION_NAME = "VK_AMD_shader_info"
