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
module Graphics.Vulkan.Ext.VK_EXT_discard_rectangles
       (-- * Vulkan extension: @VK_EXT_discard_rectangles@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @100@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceDiscardRectanglePropertiesEXT(..),
        VkPipelineDiscardRectangleStateCreateInfoEXT(..),
        vkCmdSetDiscardRectangleEXT,
        VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION,
        pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION,
        VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME,
        pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT,
        pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkRect2D (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceDiscardRectanglePropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               maxDiscardRectangles;
--   > } VkPhysicalDeviceDiscardRectanglePropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceDiscardRectanglePropertiesEXT.html VkPhysicalDeviceDiscardRectanglePropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceDiscardRectanglePropertiesEXT = VkPhysicalDeviceDiscardRectanglePropertiesEXT## ByteArray##

instance Eq VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        (VkPhysicalDeviceDiscardRectanglePropertiesEXT## a) ==
          (VkPhysicalDeviceDiscardRectanglePropertiesEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        (VkPhysicalDeviceDiscardRectanglePropertiesEXT## a) `compare`
          (VkPhysicalDeviceDiscardRectanglePropertiesEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceDiscardRectanglePropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceDiscardRectanglePropertiesEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceDiscardRectanglePropertiesEXT),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceDiscardRectanglePropertiesEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceDiscardRectanglePropertiesEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceDiscardRectanglePropertiesEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceDiscardRectanglePropertiesEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceDiscardRectanglePropertiesEXT),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceDiscardRectanglePropertiesEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceDiscardRectanglePropertiesEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceDiscardRectanglePropertiesEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceDiscardRectanglePropertiesEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceDiscardRectanglePropertiesEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPhysicalDeviceDiscardRectanglePropertiesEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceDiscardRectanglePropertiesEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        type VkSTypeMType VkPhysicalDeviceDiscardRectanglePropertiesEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             =
             #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

instance CanReadField "sType"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        type VkPNextMType VkPhysicalDeviceDiscardRectanglePropertiesEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             =
             #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMaxDiscardRectangles
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        type VkMaxDiscardRectanglesMType
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = Word32

        {-# NOINLINE vkMaxDiscardRectangles #-}
        vkMaxDiscardRectangles x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles})

        {-# INLINE vkMaxDiscardRectanglesByteOffset #-}
        vkMaxDiscardRectanglesByteOffset ~_
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

        {-# INLINE readVkMaxDiscardRectangles #-}
        readVkMaxDiscardRectangles p
          = peekByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

        {-# INLINE writeVkMaxDiscardRectangles #-}
        writeVkMaxDiscardRectangles p
          = pokeByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

instance {-# OVERLAPPING #-}
         HasField "maxDiscardRectangles"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        type FieldType "maxDiscardRectangles"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = Word32
        type FieldOptional "maxDiscardRectangles"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDiscardRectangles"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             =
             #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

instance CanReadField "maxDiscardRectangles"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkMaxDiscardRectangles

        {-# INLINE readField #-}
        readField = readVkMaxDiscardRectangles

instance CanWriteField "maxDiscardRectangles"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDiscardRectangles

instance Show VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceDiscardRectanglePropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMaxDiscardRectangles = " .
                            showsPrec d (vkMaxDiscardRectangles x) . showChar '}'

-- | > typedef struct VkPipelineDiscardRectangleStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineDiscardRectangleStateCreateFlagsEXT                    flags;
--   >     VkDiscardRectangleModeEXT                                                        discardRectangleMode;
--   >     uint32_t                                                         discardRectangleCount;
--   >     const VkRect2D* pDiscardRectangles;
--   > } VkPipelineDiscardRectangleStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineDiscardRectangleStateCreateInfoEXT.html VkPipelineDiscardRectangleStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineDiscardRectangleStateCreateInfoEXT = VkPipelineDiscardRectangleStateCreateInfoEXT## ByteArray##

instance Eq VkPipelineDiscardRectangleStateCreateInfoEXT where
        (VkPipelineDiscardRectangleStateCreateInfoEXT## a) ==
          (VkPipelineDiscardRectangleStateCreateInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPipelineDiscardRectangleStateCreateInfoEXT where
        (VkPipelineDiscardRectangleStateCreateInfoEXT## a) `compare`
          (VkPipelineDiscardRectangleStateCreateInfoEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineDiscardRectangleStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineDiscardRectangleStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPipelineDiscardRectangleStateCreateInfoEXT),
            I## a <- alignment
                      (undefined :: VkPipelineDiscardRectangleStateCreateInfoEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPipelineDiscardRectangleStateCreateInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPipelineDiscardRectangleStateCreateInfoEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkPipelineDiscardRectangleStateCreateInfoEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPipelineDiscardRectangleStateCreateInfoEXT),
            I## a <- alignment
                      (undefined :: VkPipelineDiscardRectangleStateCreateInfoEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPipelineDiscardRectangleStateCreateInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPipelineDiscardRectangleStateCreateInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPipelineDiscardRectangleStateCreateInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPipelineDiscardRectangleStateCreateInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPipelineDiscardRectangleStateCreateInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPipelineDiscardRectangleStateCreateInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineDiscardRectangleStateCreateInfoEXT where
        type VkSTypeMType VkPipelineDiscardRectangleStateCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineDiscardRectangleStateCreateInfoEXT where
        type FieldType "sType" VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

instance CanReadField "sType"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineDiscardRectangleStateCreateInfoEXT where
        type VkPNextMType VkPipelineDiscardRectangleStateCreateInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineDiscardRectangleStateCreateInfoEXT where
        type FieldType "pNext" VkPipelineDiscardRectangleStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

instance CanReadField "pNext"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineDiscardRectangleStateCreateInfoEXT where
        type VkFlagsMType VkPipelineDiscardRectangleStateCreateInfoEXT =
             VkPipelineDiscardRectangleStateCreateFlagsEXT

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineDiscardRectangleStateCreateInfoEXT where
        type FieldType "flags" VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkPipelineDiscardRectangleStateCreateFlagsEXT
        type FieldOptional "flags"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

instance CanReadField "flags"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkDiscardRectangleMode
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type VkDiscardRectangleModeMType
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkDiscardRectangleModeEXT

        {-# NOINLINE vkDiscardRectangleMode #-}
        vkDiscardRectangleMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode})

        {-# INLINE vkDiscardRectangleModeByteOffset #-}
        vkDiscardRectangleModeByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

        {-# INLINE readVkDiscardRectangleMode #-}
        readVkDiscardRectangleMode p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

        {-# INLINE writeVkDiscardRectangleMode #-}
        writeVkDiscardRectangleMode p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

instance {-# OVERLAPPING #-}
         HasField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type FieldType "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkDiscardRectangleModeEXT
        type FieldOptional "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

instance CanReadField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkDiscardRectangleMode

        {-# INLINE readField #-}
        readField = readVkDiscardRectangleMode

instance CanWriteField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkDiscardRectangleMode

instance {-# OVERLAPPING #-}
         HasVkDiscardRectangleCount
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type VkDiscardRectangleCountMType
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = Word32

        {-# NOINLINE vkDiscardRectangleCount #-}
        vkDiscardRectangleCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount})

        {-# INLINE vkDiscardRectangleCountByteOffset #-}
        vkDiscardRectangleCountByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

        {-# INLINE readVkDiscardRectangleCount #-}
        readVkDiscardRectangleCount p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

        {-# INLINE writeVkDiscardRectangleCount #-}
        writeVkDiscardRectangleCount p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

instance {-# OVERLAPPING #-}
         HasField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type FieldType "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = Word32
        type FieldOptional "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

instance CanReadField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkDiscardRectangleCount

        {-# INLINE readField #-}
        readField = readVkDiscardRectangleCount

instance CanWriteField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkDiscardRectangleCount

instance {-# OVERLAPPING #-}
         HasVkPDiscardRectangles
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type VkPDiscardRectanglesMType
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = Ptr VkRect2D

        {-# NOINLINE vkPDiscardRectangles #-}
        vkPDiscardRectangles x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles})

        {-# INLINE vkPDiscardRectanglesByteOffset #-}
        vkPDiscardRectanglesByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

        {-# INLINE readVkPDiscardRectangles #-}
        readVkPDiscardRectangles p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

        {-# INLINE writeVkPDiscardRectangles #-}
        writeVkPDiscardRectangles p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

instance {-# OVERLAPPING #-}
         HasField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type FieldType "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = Ptr VkRect2D
        type FieldOptional "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

instance CanReadField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPDiscardRectangles

        {-# INLINE readField #-}
        readField = readVkPDiscardRectangles

instance CanWriteField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDiscardRectangles

instance Show VkPipelineDiscardRectangleStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineDiscardRectangleStateCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDiscardRectangleMode = " .
                                  showsPrec d (vkDiscardRectangleMode x) .
                                    showString ", " .
                                      showString "vkDiscardRectangleCount = " .
                                        showsPrec d (vkDiscardRectangleCount x) .
                                          showString ", " .
                                            showString "vkPDiscardRectangles = " .
                                              showsPrec d (vkPDiscardRectangles x) . showChar '}'

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetDiscardRectangleEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstDiscardRectangle
--   >     , uint32_t discardRectangleCount
--   >     , const VkRect2D* pDiscardRectangles
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetDiscardRectangleEXT.html vkCmdSetDiscardRectangleEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDiscardRectangleEXT"
               vkCmdSetDiscardRectangleEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ firstDiscardRectangle
                                         -> Word32 -- ^ discardRectangleCount
                                                   -> Ptr VkRect2D -- ^ pDiscardRectangles
                                                                   -> IO ()

pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION = 1

type VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION = 1

pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME :: CString

pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME <-
        (is_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME -> True)
  where VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
          = _VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME

_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME #-}
_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  = Ptr "VK_EXT_discard_rectangles\NUL"##

is_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME #-}
is_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  = (_VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME ==)

type VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME =
     "VK_EXT_discard_rectangles"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
        = VkStructureType 1000099000

pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
        = VkStructureType 1000099001

pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT =
        VkDynamicState 1000099000
