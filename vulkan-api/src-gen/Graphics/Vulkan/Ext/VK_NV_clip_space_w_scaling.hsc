#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NV_clip_space_w_scaling
       (-- * Vulkan extension: @VK_NV_clip_space_w_scaling@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Eric Werness @ewerness@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @88@
        VkViewportWScalingNV(..),
        VkPipelineViewportWScalingStateCreateInfoNV(..),
        vkCmdSetViewportWScalingNV,
        VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION,
        pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION,
        VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME,
        pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV,
        pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkBool32, VkCommandBuffer,
                                                   VkDynamicState (..),
                                                   VkStructureType,
                                                   VkStructureType (..), Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkViewportWScalingNV = VkViewportWScalingNV## ByteArray##

instance Eq VkViewportWScalingNV where
        (VkViewportWScalingNV## a) == (VkViewportWScalingNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkViewportWScalingNV where
        (VkViewportWScalingNV## a) `compare` (VkViewportWScalingNV## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkViewportWScalingNV where
        sizeOf ~_ = #{size VkViewportWScalingNV}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViewportWScalingNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkViewportWScalingNV),
            I## a <- alignment (undefined :: VkViewportWScalingNV) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkViewportWScalingNV## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkViewportWScalingNV## ba)
          | I## n <- sizeOf (undefined :: VkViewportWScalingNV) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkViewportWScalingNV where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkViewportWScalingNV),
            I## a <- alignment (undefined :: VkViewportWScalingNV) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkViewportWScalingNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkViewportWScalingNV## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkViewportWScalingNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkViewportWScalingNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkViewportWScalingNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkViewportWScalingNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkXcoeff VkViewportWScalingNV where
        type VkXcoeffMType VkViewportWScalingNV = #{type float}

        {-# NOINLINE vkXcoeff #-}
        vkXcoeff x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportWScalingNV, xcoeff})

        {-# INLINE vkXcoeffByteOffset #-}
        vkXcoeffByteOffset ~_
          = #{offset VkViewportWScalingNV, xcoeff}

        {-# INLINE readVkXcoeff #-}
        readVkXcoeff p
          = peekByteOff p #{offset VkViewportWScalingNV, xcoeff}

        {-# INLINE writeVkXcoeff #-}
        writeVkXcoeff p
          = pokeByteOff p #{offset VkViewportWScalingNV, xcoeff}

instance {-# OVERLAPPING #-} HasVkYcoeff VkViewportWScalingNV where
        type VkYcoeffMType VkViewportWScalingNV = #{type float}

        {-# NOINLINE vkYcoeff #-}
        vkYcoeff x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportWScalingNV, ycoeff})

        {-# INLINE vkYcoeffByteOffset #-}
        vkYcoeffByteOffset ~_
          = #{offset VkViewportWScalingNV, ycoeff}

        {-# INLINE readVkYcoeff #-}
        readVkYcoeff p
          = peekByteOff p #{offset VkViewportWScalingNV, ycoeff}

        {-# INLINE writeVkYcoeff #-}
        writeVkYcoeff p
          = pokeByteOff p #{offset VkViewportWScalingNV, ycoeff}

instance Show VkViewportWScalingNV where
        showsPrec d x
          = showString "VkViewportWScalingNV {" .
              showString "vkXcoeff = " .
                showsPrec d (vkXcoeff x) .
                  showString ", " .
                    showString "vkYcoeff = " . showsPrec d (vkYcoeff x) . showChar '}'

data VkPipelineViewportWScalingStateCreateInfoNV = VkPipelineViewportWScalingStateCreateInfoNV## ByteArray##

instance Eq VkPipelineViewportWScalingStateCreateInfoNV where
        (VkPipelineViewportWScalingStateCreateInfoNV## a) ==
          (VkPipelineViewportWScalingStateCreateInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPipelineViewportWScalingStateCreateInfoNV where
        (VkPipelineViewportWScalingStateCreateInfoNV## a) `compare`
          (VkPipelineViewportWScalingStateCreateInfoNV## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPipelineViewportWScalingStateCreateInfoNV where
        sizeOf ~_
          = #{size VkPipelineViewportWScalingStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineViewportWScalingStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPipelineViewportWScalingStateCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkPipelineViewportWScalingStateCreateInfoNV)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPipelineViewportWScalingStateCreateInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPipelineViewportWScalingStateCreateInfoNV## ba)
          | I## n <- sizeOf
                      (undefined :: VkPipelineViewportWScalingStateCreateInfoNV)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPipelineViewportWScalingStateCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkPipelineViewportWScalingStateCreateInfoNV)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPipelineViewportWScalingStateCreateInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPipelineViewportWScalingStateCreateInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPipelineViewportWScalingStateCreateInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPipelineViewportWScalingStateCreateInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPipelineViewportWScalingStateCreateInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPipelineViewportWScalingStateCreateInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineViewportWScalingStateCreateInfoNV where
        type VkSTypeMType VkPipelineViewportWScalingStateCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineViewportWScalingStateCreateInfoNV where
        type VkPNextMType VkPipelineViewportWScalingStateCreateInfoNV =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasVkViewportWScalingEnable
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type VkViewportWScalingEnableMType
               VkPipelineViewportWScalingStateCreateInfoNV
             = VkBool32

        {-# NOINLINE vkViewportWScalingEnable #-}
        vkViewportWScalingEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable})

        {-# INLINE vkViewportWScalingEnableByteOffset #-}
        vkViewportWScalingEnableByteOffset ~_
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

        {-# INLINE readVkViewportWScalingEnable #-}
        readVkViewportWScalingEnable p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

        {-# INLINE writeVkViewportWScalingEnable #-}
        writeVkViewportWScalingEnable p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

instance {-# OVERLAPPING #-}
         HasVkViewportCount VkPipelineViewportWScalingStateCreateInfoNV
         where
        type VkViewportCountMType
               VkPipelineViewportWScalingStateCreateInfoNV
             = Word32

        {-# NOINLINE vkViewportCount #-}
        vkViewportCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount})

        {-# INLINE vkViewportCountByteOffset #-}
        vkViewportCountByteOffset ~_
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

        {-# INLINE readVkViewportCount #-}
        readVkViewportCount p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

        {-# INLINE writeVkViewportCount #-}
        writeVkViewportCount p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         HasVkPViewportWScalings VkPipelineViewportWScalingStateCreateInfoNV
         where
        type VkPViewportWScalingsMType
               VkPipelineViewportWScalingStateCreateInfoNV
             = Ptr VkViewportWScalingNV

        {-# NOINLINE vkPViewportWScalings #-}
        vkPViewportWScalings x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings})

        {-# INLINE vkPViewportWScalingsByteOffset #-}
        vkPViewportWScalingsByteOffset ~_
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

        {-# INLINE readVkPViewportWScalings #-}
        readVkPViewportWScalings p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

        {-# INLINE writeVkPViewportWScalings #-}
        writeVkPViewportWScalings p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

instance Show VkPipelineViewportWScalingStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineViewportWScalingStateCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkViewportWScalingEnable = " .
                            showsPrec d (vkViewportWScalingEnable x) .
                              showString ", " .
                                showString "vkViewportCount = " .
                                  showsPrec d (vkViewportCount x) .
                                    showString ", " .
                                      showString "vkPViewportWScalings = " .
                                        showsPrec d (vkPViewportWScalings x) . showChar '}'

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetViewportWScalingNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewportWScalingNV* pViewportWScalings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetViewportWScalingNV.html vkCmdSetViewportWScalingNV registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetViewportWScalingNV"
               vkCmdSetViewportWScalingNV ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ firstViewport
                        -> Word32 -- ^ viewportCount
                                  -> Ptr VkViewportWScalingNV -- ^ pViewportWScalings
                                                              -> IO ()

pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1

type VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1

pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString

pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME <-
        (is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME -> True)
  where VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
          = _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME

_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString

{-# INLINE _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME #-}
_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  = Ptr "VK_NV_clip_space_w_scaling\NUL"##

is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME #-}
is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  = (_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME ==)

type VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME =
     "VK_NV_clip_space_w_scaling"

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
        = VkStructureType 1000087000

pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: VkDynamicState

pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV =
        VkDynamicState 1000087000
