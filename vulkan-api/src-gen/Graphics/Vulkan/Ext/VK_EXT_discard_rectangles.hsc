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
                               ->
                 Data.Word.Word32 -- ^ firstDiscardRectangle
                                  -> Data.Word.Word32 -- ^ discardRectangleCount
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
