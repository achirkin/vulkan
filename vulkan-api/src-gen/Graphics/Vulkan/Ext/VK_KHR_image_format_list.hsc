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
module Graphics.Vulkan.Ext.VK_KHR_image_format_list
       (-- * Vulkan extension: @VK_KHR_image_format_list@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jason Ekstrand @jekstrand@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @148@
        VkImageFormatListCreateInfoKHR(..),
        VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION,
        pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION,
        VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME,
        pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkFormat,
                                                   VkStructureType (..), Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkImageFormatListCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               viewFormatCount;
--   >     const VkFormat*      pViewFormats;
--   > } VkImageFormatListCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageFormatListCreateInfoKHR.html VkImageFormatListCreateInfoKHR registry at www.khronos.org>
data VkImageFormatListCreateInfoKHR = VkImageFormatListCreateInfoKHR## ByteArray##

instance Eq VkImageFormatListCreateInfoKHR where
        (VkImageFormatListCreateInfoKHR## a) ==
          (VkImageFormatListCreateInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImageFormatListCreateInfoKHR where
        (VkImageFormatListCreateInfoKHR## a) `compare`
          (VkImageFormatListCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImageFormatListCreateInfoKHR where
        sizeOf ~_ = #{size VkImageFormatListCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageFormatListCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImageFormatListCreateInfoKHR),
            I## a <- alignment (undefined :: VkImageFormatListCreateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImageFormatListCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImageFormatListCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkImageFormatListCreateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImageFormatListCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImageFormatListCreateInfoKHR),
            I## a <- alignment (undefined :: VkImageFormatListCreateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImageFormatListCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImageFormatListCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImageFormatListCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImageFormatListCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImageFormatListCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImageFormatListCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkImageFormatListCreateInfoKHR where
        type VkSTypeMType VkImageFormatListCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageFormatListCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageFormatListCreateInfoKHR where
        type FieldType "sType" VkImageFormatListCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, sType}

instance CanReadField "sType" VkImageFormatListCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImageFormatListCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkImageFormatListCreateInfoKHR where
        type VkPNextMType VkImageFormatListCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageFormatListCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageFormatListCreateInfoKHR where
        type FieldType "pNext" VkImageFormatListCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, pNext}

instance CanReadField "pNext" VkImageFormatListCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImageFormatListCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkViewFormatCount VkImageFormatListCreateInfoKHR where
        type VkViewFormatCountMType VkImageFormatListCreateInfoKHR = Word32

        {-# NOINLINE vkViewFormatCount #-}
        vkViewFormatCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, viewFormatCount})

        {-# INLINE vkViewFormatCountByteOffset #-}
        vkViewFormatCountByteOffset ~_
          = #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

        {-# INLINE readVkViewFormatCount #-}
        readVkViewFormatCount p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

        {-# INLINE writeVkViewFormatCount #-}
        writeVkViewFormatCount p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

instance {-# OVERLAPPING #-}
         HasField "viewFormatCount" VkImageFormatListCreateInfoKHR where
        type FieldType "viewFormatCount" VkImageFormatListCreateInfoKHR =
             Word32
        type FieldOptional "viewFormatCount" VkImageFormatListCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "viewFormatCount" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

instance CanReadField "viewFormatCount"
           VkImageFormatListCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkViewFormatCount

        {-# INLINE readField #-}
        readField = readVkViewFormatCount

instance CanWriteField "viewFormatCount"
           VkImageFormatListCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkViewFormatCount

instance {-# OVERLAPPING #-}
         HasVkPViewFormats VkImageFormatListCreateInfoKHR where
        type VkPViewFormatsMType VkImageFormatListCreateInfoKHR =
             Ptr VkFormat

        {-# NOINLINE vkPViewFormats #-}
        vkPViewFormats x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, pViewFormats})

        {-# INLINE vkPViewFormatsByteOffset #-}
        vkPViewFormatsByteOffset ~_
          = #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

        {-# INLINE readVkPViewFormats #-}
        readVkPViewFormats p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

        {-# INLINE writeVkPViewFormats #-}
        writeVkPViewFormats p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

instance {-# OVERLAPPING #-}
         HasField "pViewFormats" VkImageFormatListCreateInfoKHR where
        type FieldType "pViewFormats" VkImageFormatListCreateInfoKHR =
             Ptr VkFormat
        type FieldOptional "pViewFormats" VkImageFormatListCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewFormats" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

instance CanReadField "pViewFormats" VkImageFormatListCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPViewFormats

        {-# INLINE readField #-}
        readField = readVkPViewFormats

instance CanWriteField "pViewFormats"
           VkImageFormatListCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPViewFormats

instance Show VkImageFormatListCreateInfoKHR where
        showsPrec d x
          = showString "VkImageFormatListCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkViewFormatCount = " .
                            showsPrec d (vkViewFormatCount x) .
                              showString ", " .
                                showString "vkPViewFormats = " .
                                  showsPrec d (vkPViewFormats x) . showChar '}'

pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1

type VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1

pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: CString

pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME <-
        (is_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME -> True)
  where VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
          = _VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME

_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME #-}
_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  = Ptr "VK_KHR_image_format_list\NUL"##

is_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME #-}
is_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  = (_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME ==)

type VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME =
     "VK_KHR_image_format_list"

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR =
        VkStructureType 1000147000
