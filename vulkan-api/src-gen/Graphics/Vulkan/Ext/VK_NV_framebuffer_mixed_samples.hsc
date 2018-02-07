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
module Graphics.Vulkan.Ext.VK_NV_framebuffer_mixed_samples
       (-- * Vulkan extension: @VK_NV_framebuffer_mixed_samples@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @153@
        VkPipelineCoverageModulationStateCreateInfoNV(..),
        VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION,
        pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION,
        VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME,
        pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV)
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

data VkPipelineCoverageModulationStateCreateInfoNV = VkPipelineCoverageModulationStateCreateInfoNV## ByteArray##

instance Eq VkPipelineCoverageModulationStateCreateInfoNV where
        (VkPipelineCoverageModulationStateCreateInfoNV## a) ==
          (VkPipelineCoverageModulationStateCreateInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPipelineCoverageModulationStateCreateInfoNV where
        (VkPipelineCoverageModulationStateCreateInfoNV## a) `compare`
          (VkPipelineCoverageModulationStateCreateInfoNV## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPipelineCoverageModulationStateCreateInfoNV
         where
        sizeOf ~_
          = #{size VkPipelineCoverageModulationStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineCoverageModulationStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPipelineCoverageModulationStateCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkPipelineCoverageModulationStateCreateInfoNV)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPipelineCoverageModulationStateCreateInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPipelineCoverageModulationStateCreateInfoNV## ba)
          | I## n <- sizeOf
                      (undefined :: VkPipelineCoverageModulationStateCreateInfoNV)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPipelineCoverageModulationStateCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkPipelineCoverageModulationStateCreateInfoNV)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPipelineCoverageModulationStateCreateInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPipelineCoverageModulationStateCreateInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPipelineCoverageModulationStateCreateInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPipelineCoverageModulationStateCreateInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPipelineCoverageModulationStateCreateInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPipelineCoverageModulationStateCreateInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineCoverageModulationStateCreateInfoNV where
        type VkSTypeMType VkPipelineCoverageModulationStateCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineCoverageModulationStateCreateInfoNV where
        type VkPNextMType VkPipelineCoverageModulationStateCreateInfoNV =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineCoverageModulationStateCreateInfoNV where
        type VkFlagsMType VkPipelineCoverageModulationStateCreateInfoNV =
             VkPipelineCoverageModulationStateCreateFlagsNV

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         HasVkCoverageModulationMode
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type VkCoverageModulationModeMType
               VkPipelineCoverageModulationStateCreateInfoNV
             = VkCoverageModulationModeNV

        {-# NOINLINE vkCoverageModulationMode #-}
        vkCoverageModulationMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode})

        {-# INLINE vkCoverageModulationModeByteOffset #-}
        vkCoverageModulationModeByteOffset ~_
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}

        {-# INLINE readVkCoverageModulationMode #-}
        readVkCoverageModulationMode p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}

        {-# INLINE writeVkCoverageModulationMode #-}
        writeVkCoverageModulationMode p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationMode}

instance {-# OVERLAPPING #-}
         HasVkCoverageModulationTableEnable
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type VkCoverageModulationTableEnableMType
               VkPipelineCoverageModulationStateCreateInfoNV
             = VkBool32

        {-# NOINLINE vkCoverageModulationTableEnable #-}
        vkCoverageModulationTableEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable})

        {-# INLINE vkCoverageModulationTableEnableByteOffset #-}
        vkCoverageModulationTableEnableByteOffset ~_
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}

        {-# INLINE readVkCoverageModulationTableEnable #-}
        readVkCoverageModulationTableEnable p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}

        {-# INLINE writeVkCoverageModulationTableEnable #-}
        writeVkCoverageModulationTableEnable p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableEnable}

instance {-# OVERLAPPING #-}
         HasVkCoverageModulationTableCount
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type VkCoverageModulationTableCountMType
               VkPipelineCoverageModulationStateCreateInfoNV
             = Word32

        {-# NOINLINE vkCoverageModulationTableCount #-}
        vkCoverageModulationTableCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount})

        {-# INLINE vkCoverageModulationTableCountByteOffset #-}
        vkCoverageModulationTableCountByteOffset ~_
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}

        {-# INLINE readVkCoverageModulationTableCount #-}
        readVkCoverageModulationTableCount p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}

        {-# INLINE writeVkCoverageModulationTableCount #-}
        writeVkCoverageModulationTableCount p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, coverageModulationTableCount}

instance {-# OVERLAPPING #-}
         HasVkPCoverageModulationTable
           VkPipelineCoverageModulationStateCreateInfoNV
         where
        type VkPCoverageModulationTableMType
               VkPipelineCoverageModulationStateCreateInfoNV
             = Ptr #{type float}

        {-# NOINLINE vkPCoverageModulationTable #-}
        vkPCoverageModulationTable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable})

        {-# INLINE vkPCoverageModulationTableByteOffset #-}
        vkPCoverageModulationTableByteOffset ~_
          = #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}

        {-# INLINE readVkPCoverageModulationTable #-}
        readVkPCoverageModulationTable p
          = peekByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}

        {-# INLINE writeVkPCoverageModulationTable #-}
        writeVkPCoverageModulationTable p
          = pokeByteOff p #{offset VkPipelineCoverageModulationStateCreateInfoNV, pCoverageModulationTable}

instance Show VkPipelineCoverageModulationStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineCoverageModulationStateCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkCoverageModulationMode = " .
                                  showsPrec d (vkCoverageModulationMode x) .
                                    showString ", " .
                                      showString "vkCoverageModulationTableEnable = " .
                                        showsPrec d (vkCoverageModulationTableEnable x) .
                                          showString ", " .
                                            showString "vkCoverageModulationTableCount = " .
                                              showsPrec d (vkCoverageModulationTableCount x) .
                                                showString ", " .
                                                  showString "vkPCoverageModulationTable = " .
                                                    showsPrec d (vkPCoverageModulationTable x) .
                                                      showChar '}'

pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1

type VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1

pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME :: CString

pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME <-
        (is_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME -> True)
  where VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
          = _VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME

_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME :: CString

{-# INLINE _VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME #-}
_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
  = Ptr "VK_NV_framebuffer_mixed_samples\NUL"##

is_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME ::
                                                  CString -> Bool

{-# INLINE is_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME #-}
is_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
  = (_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME ==)

type VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME =
     "VK_NV_framebuffer_mixed_samples"

pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
        = VkStructureType 1000152000
