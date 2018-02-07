#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_debug_marker
       (-- * Vulkan extension: @VK_EXT_debug_marker@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @baldurk@baldurk.org@
        --
        -- author: @Baldur Karlsson@
        --
        -- type: @device@
        --
        -- Extension number: @23@
        --
        -- Required extensions: 'VK_EXT_debug_report'.
        --

        -- ** Required extensions: 'VK_EXT_debug_report'.
        VkDebugMarkerObjectNameInfoEXT(..),
        VkDebugMarkerObjectTagInfoEXT(..), VkDebugMarkerMarkerInfoEXT(..),
        vkDebugMarkerSetObjectTagEXT, vkDebugMarkerSetObjectNameEXT,
        vkCmdDebugMarkerBeginEXT, vkCmdDebugMarkerEndEXT,
        vkCmdDebugMarkerInsertEXT, VK_EXT_DEBUG_MARKER_SPEC_VERSION,
        pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION,
        VK_EXT_DEBUG_MARKER_EXTENSION_NAME,
        pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkCommandBuffer,
                                                   VkDebugReportObjectTypeEXT,
                                                   VkDevice, VkResult,
                                                   VkStructureType (..), Word64)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkDebugMarkerObjectNameInfoEXT = VkDebugMarkerObjectNameInfoEXT## ByteArray##

instance Eq VkDebugMarkerObjectNameInfoEXT where
        (VkDebugMarkerObjectNameInfoEXT## a) ==
          (VkDebugMarkerObjectNameInfoEXT## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDebugMarkerObjectNameInfoEXT where
        (VkDebugMarkerObjectNameInfoEXT## a) `compare`
          (VkDebugMarkerObjectNameInfoEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDebugMarkerObjectNameInfoEXT where
        sizeOf ~_ = #{size VkDebugMarkerObjectNameInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugMarkerObjectNameInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDebugMarkerObjectNameInfoEXT),
            I## a <- alignment (undefined :: VkDebugMarkerObjectNameInfoEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDebugMarkerObjectNameInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDebugMarkerObjectNameInfoEXT## ba)
          | I## n <- sizeOf (undefined :: VkDebugMarkerObjectNameInfoEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDebugMarkerObjectNameInfoEXT),
            I## a <- alignment (undefined :: VkDebugMarkerObjectNameInfoEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDebugMarkerObjectNameInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDebugMarkerObjectNameInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDebugMarkerObjectNameInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDebugMarkerObjectNameInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDebugMarkerObjectNameInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDebugMarkerObjectNameInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDebugMarkerObjectNameInfoEXT where
        type VkSTypeMType VkDebugMarkerObjectNameInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDebugMarkerObjectNameInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkDebugMarkerObjectNameInfoEXT where
        type VkPNextMType VkDebugMarkerObjectNameInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkObjectType VkDebugMarkerObjectNameInfoEXT where
        type VkObjectTypeMType VkDebugMarkerObjectNameInfoEXT =
             VkDebugReportObjectTypeEXT

        {-# NOINLINE vkObjectType #-}
        vkObjectType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, objectType})

        {-# INLINE vkObjectTypeByteOffset #-}
        vkObjectTypeByteOffset ~_
          = #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

        {-# INLINE readVkObjectType #-}
        readVkObjectType p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

        {-# INLINE writeVkObjectType #-}
        writeVkObjectType p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasVkObject VkDebugMarkerObjectNameInfoEXT where
        type VkObjectMType VkDebugMarkerObjectNameInfoEXT = Word64

        {-# NOINLINE vkObject #-}
        vkObject x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, object})

        {-# INLINE vkObjectByteOffset #-}
        vkObjectByteOffset ~_
          = #{offset VkDebugMarkerObjectNameInfoEXT, object}

        {-# INLINE readVkObject #-}
        readVkObject p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, object}

        {-# INLINE writeVkObject #-}
        writeVkObject p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, object}

instance {-# OVERLAPPING #-}
         HasVkPObjectName VkDebugMarkerObjectNameInfoEXT where
        type VkPObjectNameMType VkDebugMarkerObjectNameInfoEXT = CString

        {-# NOINLINE vkPObjectName #-}
        vkPObjectName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName})

        {-# INLINE vkPObjectNameByteOffset #-}
        vkPObjectNameByteOffset ~_
          = #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

        {-# INLINE readVkPObjectName #-}
        readVkPObjectName p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

        {-# INLINE writeVkPObjectName #-}
        writeVkPObjectName p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

instance Show VkDebugMarkerObjectNameInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerObjectNameInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkObjectType = " .
                            showsPrec d (vkObjectType x) .
                              showString ", " .
                                showString "vkObject = " .
                                  showsPrec d (vkObject x) .
                                    showString ", " .
                                      showString "vkPObjectName = " .
                                        showsPrec d (vkPObjectName x) . showChar '}'

data VkDebugMarkerObjectTagInfoEXT = VkDebugMarkerObjectTagInfoEXT## ByteArray##

instance Eq VkDebugMarkerObjectTagInfoEXT where
        (VkDebugMarkerObjectTagInfoEXT## a) ==
          (VkDebugMarkerObjectTagInfoEXT## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDebugMarkerObjectTagInfoEXT where
        (VkDebugMarkerObjectTagInfoEXT## a) `compare`
          (VkDebugMarkerObjectTagInfoEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDebugMarkerObjectTagInfoEXT where
        sizeOf ~_ = #{size VkDebugMarkerObjectTagInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugMarkerObjectTagInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDebugMarkerObjectTagInfoEXT),
            I## a <- alignment (undefined :: VkDebugMarkerObjectTagInfoEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDebugMarkerObjectTagInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDebugMarkerObjectTagInfoEXT## ba)
          | I## n <- sizeOf (undefined :: VkDebugMarkerObjectTagInfoEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDebugMarkerObjectTagInfoEXT),
            I## a <- alignment (undefined :: VkDebugMarkerObjectTagInfoEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDebugMarkerObjectTagInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDebugMarkerObjectTagInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDebugMarkerObjectTagInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDebugMarkerObjectTagInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDebugMarkerObjectTagInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDebugMarkerObjectTagInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDebugMarkerObjectTagInfoEXT where
        type VkSTypeMType VkDebugMarkerObjectTagInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkDebugMarkerObjectTagInfoEXT where
        type VkPNextMType VkDebugMarkerObjectTagInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkObjectType VkDebugMarkerObjectTagInfoEXT where
        type VkObjectTypeMType VkDebugMarkerObjectTagInfoEXT =
             VkDebugReportObjectTypeEXT

        {-# NOINLINE vkObjectType #-}
        vkObjectType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, objectType})

        {-# INLINE vkObjectTypeByteOffset #-}
        vkObjectTypeByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

        {-# INLINE readVkObjectType #-}
        readVkObjectType p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

        {-# INLINE writeVkObjectType #-}
        writeVkObjectType p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasVkObject VkDebugMarkerObjectTagInfoEXT where
        type VkObjectMType VkDebugMarkerObjectTagInfoEXT = Word64

        {-# NOINLINE vkObject #-}
        vkObject x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, object})

        {-# INLINE vkObjectByteOffset #-}
        vkObjectByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, object}

        {-# INLINE readVkObject #-}
        readVkObject p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, object}

        {-# INLINE writeVkObject #-}
        writeVkObject p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, object}

instance {-# OVERLAPPING #-}
         HasVkTagName VkDebugMarkerObjectTagInfoEXT where
        type VkTagNameMType VkDebugMarkerObjectTagInfoEXT = Word64

        {-# NOINLINE vkTagName #-}
        vkTagName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, tagName})

        {-# INLINE vkTagNameByteOffset #-}
        vkTagNameByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

        {-# INLINE readVkTagName #-}
        readVkTagName p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

        {-# INLINE writeVkTagName #-}
        writeVkTagName p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         HasVkTagSize VkDebugMarkerObjectTagInfoEXT where
        type VkTagSizeMType VkDebugMarkerObjectTagInfoEXT =
             #{type size_t}

        {-# NOINLINE vkTagSize #-}
        vkTagSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, tagSize})

        {-# INLINE vkTagSizeByteOffset #-}
        vkTagSizeByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

        {-# INLINE readVkTagSize #-}
        readVkTagSize p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

        {-# INLINE writeVkTagSize #-}
        writeVkTagSize p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         HasVkPTag VkDebugMarkerObjectTagInfoEXT where
        type VkPTagMType VkDebugMarkerObjectTagInfoEXT = Ptr Void

        {-# NOINLINE vkPTag #-}
        vkPTag x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, pTag})

        {-# INLINE vkPTagByteOffset #-}
        vkPTagByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

        {-# INLINE readVkPTag #-}
        readVkPTag p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

        {-# INLINE writeVkPTag #-}
        writeVkPTag p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

instance Show VkDebugMarkerObjectTagInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerObjectTagInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkObjectType = " .
                            showsPrec d (vkObjectType x) .
                              showString ", " .
                                showString "vkObject = " .
                                  showsPrec d (vkObject x) .
                                    showString ", " .
                                      showString "vkTagName = " .
                                        showsPrec d (vkTagName x) .
                                          showString ", " .
                                            showString "vkTagSize = " .
                                              showsPrec d (vkTagSize x) .
                                                showString ", " .
                                                  showString "vkPTag = " .
                                                    showsPrec d (vkPTag x) . showChar '}'

data VkDebugMarkerMarkerInfoEXT = VkDebugMarkerMarkerInfoEXT## ByteArray##

instance Eq VkDebugMarkerMarkerInfoEXT where
        (VkDebugMarkerMarkerInfoEXT## a) == (VkDebugMarkerMarkerInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDebugMarkerMarkerInfoEXT where
        (VkDebugMarkerMarkerInfoEXT## a) `compare`
          (VkDebugMarkerMarkerInfoEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDebugMarkerMarkerInfoEXT where
        sizeOf ~_ = #{size VkDebugMarkerMarkerInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDebugMarkerMarkerInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDebugMarkerMarkerInfoEXT),
            I## a <- alignment (undefined :: VkDebugMarkerMarkerInfoEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDebugMarkerMarkerInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDebugMarkerMarkerInfoEXT## ba)
          | I## n <- sizeOf (undefined :: VkDebugMarkerMarkerInfoEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDebugMarkerMarkerInfoEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDebugMarkerMarkerInfoEXT),
            I## a <- alignment (undefined :: VkDebugMarkerMarkerInfoEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDebugMarkerMarkerInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDebugMarkerMarkerInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDebugMarkerMarkerInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDebugMarkerMarkerInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDebugMarkerMarkerInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDebugMarkerMarkerInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkDebugMarkerMarkerInfoEXT
         where
        type VkSTypeMType VkDebugMarkerMarkerInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDebugMarkerMarkerInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkDebugMarkerMarkerInfoEXT
         where
        type VkPNextMType VkDebugMarkerMarkerInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDebugMarkerMarkerInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkPMarkerName VkDebugMarkerMarkerInfoEXT where
        type VkPMarkerNameMType VkDebugMarkerMarkerInfoEXT = CString

        {-# NOINLINE vkPMarkerName #-}
        vkPMarkerName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName})

        {-# INLINE vkPMarkerNameByteOffset #-}
        vkPMarkerNameByteOffset ~_
          = #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

        {-# INLINE readVkPMarkerName #-}
        readVkPMarkerName p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

        {-# INLINE writeVkPMarkerName #-}
        writeVkPMarkerName p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

instance {-# OVERLAPPING #-}
         HasVkColorArray VkDebugMarkerMarkerInfoEXT where
        type VkColorArrayMType VkDebugMarkerMarkerInfoEXT =
             #{type float}

        {-# NOINLINE vkColorArray #-}
        vkColorArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: #{type float}) +
                    #{offset VkDebugMarkerMarkerInfoEXT, color}))

        {-# INLINE vkColorArrayByteOffset #-}
        vkColorArrayByteOffset ~_
          = #{offset VkDebugMarkerMarkerInfoEXT, color}

        {-# INLINE readVkColorArray #-}
        readVkColorArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkDebugMarkerMarkerInfoEXT, color})

        {-# INLINE writeVkColorArray #-}
        writeVkColorArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkDebugMarkerMarkerInfoEXT, color})

instance Show VkDebugMarkerMarkerInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerMarkerInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPMarkerName = " .
                            showsPrec d (vkPMarkerName x) .
                              showString ", " .
                                showString "vkColorArray = [" .
                                  showsPrec d (map (vkColorArray x) [1 .. 4]) .
                                    showChar ']' . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDebugMarkerSetObjectTagEXT
--   >     ( VkDevice device
--   >     , const VkDebugMarkerObjectTagInfoEXT* pTagInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDebugMarkerSetObjectTagEXT.html vkDebugMarkerSetObjectTagEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugMarkerSetObjectTagEXT"
               vkDebugMarkerSetObjectTagEXT ::
               VkDevice -- ^ device
                        -> Ptr VkDebugMarkerObjectTagInfoEXT -- ^ pTagInfo
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDebugMarkerSetObjectNameEXT
--   >     ( VkDevice device
--   >     , const VkDebugMarkerObjectNameInfoEXT* pNameInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDebugMarkerSetObjectNameEXT.html vkDebugMarkerSetObjectNameEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugMarkerSetObjectNameEXT"
               vkDebugMarkerSetObjectNameEXT ::
               VkDevice -- ^ device
                        -> Ptr VkDebugMarkerObjectNameInfoEXT -- ^ pNameInfo
                                                              -> IO VkResult

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdDebugMarkerBeginEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugMarkerMarkerInfoEXT* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDebugMarkerBeginEXT.html vkCmdDebugMarkerBeginEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerBeginEXT"
               vkCmdDebugMarkerBeginEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkDebugMarkerMarkerInfoEXT -- ^ pMarkerInfo
                                                                 -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdDebugMarkerEndEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDebugMarkerEndEXT.html vkCmdDebugMarkerEndEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerEndEXT"
               vkCmdDebugMarkerEndEXT :: VkCommandBuffer -- ^ commandBuffer
                                                         -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdDebugMarkerInsertEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugMarkerMarkerInfoEXT* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDebugMarkerInsertEXT.html vkCmdDebugMarkerInsertEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerInsertEXT"
               vkCmdDebugMarkerInsertEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkDebugMarkerMarkerInfoEXT -- ^ pMarkerInfo
                                                                 -> IO ()

pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4

type VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4

pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString

pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME <-
        (is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME -> True)
  where VK_EXT_DEBUG_MARKER_EXTENSION_NAME
          = _VK_EXT_DEBUG_MARKER_EXTENSION_NAME

_VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_DEBUG_MARKER_EXTENSION_NAME #-}
_VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  = Ptr "VK_EXT_debug_marker\NUL"##

is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME #-}
is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  = (_VK_EXT_DEBUG_MARKER_EXTENSION_NAME ==)

type VK_EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT =
        VkStructureType 1000022000

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT =
        VkStructureType 1000022001

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT =
        VkStructureType 1000022002
