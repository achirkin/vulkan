#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_NV_fragment_coverage_to_color
       (-- * Vulkan extension: @VK_NV_fragment_coverage_to_color@
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
        -- Extension number: @150@
        VkPipelineCoverageToColorStateCreateInfoNV(..),
        VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION,
        pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION,
        VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME,
        pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkBool32, VkPipelineCoverageToColorStateCreateFlagsNV,
                                                   VkStructureType (..), Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineCoverageToColorStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineCoverageToColorStateCreateFlagsNV                    flags;
--   >     VkBool32                         coverageToColorEnable;
--   >     uint32_t         coverageToColorLocation;
--   > } VkPipelineCoverageToColorStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineCoverageToColorStateCreateInfoNV.html VkPipelineCoverageToColorStateCreateInfoNV registry at www.khronos.org>
data VkPipelineCoverageToColorStateCreateInfoNV = VkPipelineCoverageToColorStateCreateInfoNV## ByteArray##

instance Eq VkPipelineCoverageToColorStateCreateInfoNV where
        (VkPipelineCoverageToColorStateCreateInfoNV## a) ==
          (VkPipelineCoverageToColorStateCreateInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPipelineCoverageToColorStateCreateInfoNV where
        (VkPipelineCoverageToColorStateCreateInfoNV## a) `compare`
          (VkPipelineCoverageToColorStateCreateInfoNV## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPipelineCoverageToColorStateCreateInfoNV where
        sizeOf ~_
          = #{size VkPipelineCoverageToColorStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineCoverageToColorStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPipelineCoverageToColorStateCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkPipelineCoverageToColorStateCreateInfoNV)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPipelineCoverageToColorStateCreateInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPipelineCoverageToColorStateCreateInfoNV## ba)
          | I## n <- sizeOf
                      (undefined :: VkPipelineCoverageToColorStateCreateInfoNV)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPipelineCoverageToColorStateCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkPipelineCoverageToColorStateCreateInfoNV)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPipelineCoverageToColorStateCreateInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPipelineCoverageToColorStateCreateInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPipelineCoverageToColorStateCreateInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPipelineCoverageToColorStateCreateInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPipelineCoverageToColorStateCreateInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPipelineCoverageToColorStateCreateInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineCoverageToColorStateCreateInfoNV where
        type VkSTypeMType VkPipelineCoverageToColorStateCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineCoverageToColorStateCreateInfoNV where
        type FieldType "sType" VkPipelineCoverageToColorStateCreateInfoNV =
             VkStructureType
        type FieldOptional "sType"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineCoverageToColorStateCreateInfoNV where
        type VkPNextMType VkPipelineCoverageToColorStateCreateInfoNV =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineCoverageToColorStateCreateInfoNV where
        type FieldType "pNext" VkPipelineCoverageToColorStateCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineCoverageToColorStateCreateInfoNV where
        type VkFlagsMType VkPipelineCoverageToColorStateCreateInfoNV =
             VkPipelineCoverageToColorStateCreateFlagsNV

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineCoverageToColorStateCreateInfoNV where
        type FieldType "flags" VkPipelineCoverageToColorStateCreateInfoNV =
             VkPipelineCoverageToColorStateCreateFlagsNV
        type FieldOptional "flags"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs

instance CanReadField "flags"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkCoverageToColorEnable
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        type VkCoverageToColorEnableMType
               VkPipelineCoverageToColorStateCreateInfoNV
             = VkBool32

        {-# NOINLINE vkCoverageToColorEnable #-}
        vkCoverageToColorEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable})

        {-# INLINE vkCoverageToColorEnableByteOffset #-}
        vkCoverageToColorEnableByteOffset ~_
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}

        {-# INLINE readVkCoverageToColorEnable #-}
        readVkCoverageToColorEnable p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}

        {-# INLINE writeVkCoverageToColorEnable #-}
        writeVkCoverageToColorEnable p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}

instance {-# OVERLAPPING #-}
         HasField "coverageToColorEnable"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        type FieldType "coverageToColorEnable"
               VkPipelineCoverageToColorStateCreateInfoNV
             = VkBool32
        type FieldOptional "coverageToColorEnable"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "coverageToColorEnable"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkCoverageToColorEnable

        {-# INLINE readField #-}
        readField = readVkCoverageToColorEnable

instance CanWriteField "coverageToColorEnable"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkCoverageToColorEnable

instance {-# OVERLAPPING #-}
         HasVkCoverageToColorLocation
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        type VkCoverageToColorLocationMType
               VkPipelineCoverageToColorStateCreateInfoNV
             = Word32

        {-# NOINLINE vkCoverageToColorLocation #-}
        vkCoverageToColorLocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation})

        {-# INLINE vkCoverageToColorLocationByteOffset #-}
        vkCoverageToColorLocationByteOffset ~_
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}

        {-# INLINE readVkCoverageToColorLocation #-}
        readVkCoverageToColorLocation p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}

        {-# INLINE writeVkCoverageToColorLocation #-}
        writeVkCoverageToColorLocation p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}

instance {-# OVERLAPPING #-}
         HasField "coverageToColorLocation"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        type FieldType "coverageToColorLocation"
               VkPipelineCoverageToColorStateCreateInfoNV
             = Word32
        type FieldOptional "coverageToColorLocation"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs

instance CanReadField "coverageToColorLocation"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkCoverageToColorLocation

        {-# INLINE readField #-}
        readField = readVkCoverageToColorLocation

instance CanWriteField "coverageToColorLocation"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkCoverageToColorLocation

instance Show VkPipelineCoverageToColorStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineCoverageToColorStateCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkCoverageToColorEnable = " .
                                  showsPrec d (vkCoverageToColorEnable x) .
                                    showString ", " .
                                      showString "vkCoverageToColorLocation = " .
                                        showsPrec d (vkCoverageToColorLocation x) . showChar '}'

pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1

type VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1

pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME :: CString

pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME <-
        (is_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME -> True)
  where VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
          = _VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME

_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME :: CString

{-# INLINE _VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME #-}
_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  = Ptr "VK_NV_fragment_coverage_to_color\NUL"##

is_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME ::
                                                   CString -> Bool

{-# INLINE is_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME #-}
is_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  = (_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME ==)

type VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME =
     "VK_NV_fragment_coverage_to_color"

pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
        = VkStructureType 1000149000
